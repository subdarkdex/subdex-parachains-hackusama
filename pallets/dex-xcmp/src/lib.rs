// Copyright 2020 Parity Technologies (UK) Ltd.
// This file is part of Cumulus.

// Cumulus is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Cumulus is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Cumulus.  If not, see <http://www.gnu.org/licenses/>.

//! Example Pallet that shows how to send upward messages and how to receive
//! downward messages.
#![cfg_attr(not(feature = "std"), no_std)]

use frame_support::{
	decl_error, decl_event, decl_module, 
	decl_storage, ensure, dispatch::DispatchResult, traits::{Currency, ExistenceRequirement, Get, WithdrawReason},
};
use frame_system::ensure_signed;

use codec::{Codec, Decode, Encode};
use cumulus_primitives::{
	relay_chain::DownwardMessage,
	xcmp::{XCMPMessageHandler, XCMPMessageSender},
	DownwardMessageHandler, ParaId, UpwardMessageOrigin, UpwardMessageSender,
};
use cumulus_upward_message::BalancesMessage;
use generic_asset::{AssetOptions, Owner, PermissionsV1};
use polkadot_parachain::primitives::AccountIdConversion;

#[derive(Encode, Decode)]
pub enum XCMPMessage<XAccountId, XBalance> {
	/// Transfer tokens to the given account from the Parachain account.
	TransferToken(XAccountId, XBalance),
}

type BalanceOf<T> = <T as generic_asset::Trait>::Balance;

pub type AssetIdOf<T> = <T as generic_asset::Trait>::AssetId;

/// Configuration trait of this pallet.
pub trait Trait: frame_system::Trait + generic_asset::Trait {
	/// Event type used by the runtime.
	type Event: From<Event<Self>> + Into<<Self as frame_system::Trait>::Event>;

	/// The sender of upward messages.
	type UpwardMessageSender: UpwardMessageSender<Self::UpwardMessage>;

	/// The upward message type used by the Parachain runtime.
	type UpwardMessage: Codec + BalancesMessage<Self::AccountId, BalanceOf<Self>>;

	// /// Currency of the runtime.
	// type Currency: Currency<Self::AccountId>;

	/// The sender of XCMP messages.
	type XCMPMessageSender: XCMPMessageSender<XCMPMessage<Self::AccountId, BalanceOf<Self>>>;
}

// This pallet's storage items.
decl_storage! {
	trait Store for Module<T: Trait> as ParachainUpgrade {
		pub DEXAccountId get(fn dex_account_id) config(): T::AccountId;
		pub CurrencyByParaId get(fn currency_by_para_id): map hasher(blake2_128_concat) ParaId => AssetIdOf<T>;
	}
}

decl_event! {
	pub enum Event<T> where
		AccountId = <T as frame_system::Trait>::AccountId,
		Balance = BalanceOf<T>
	{
		/// Transferred tokens to the account on the relay chain.
		TransferredTokensToRelayChain(AccountId, Balance),
		/// Transferred tokens to the account on request from the relay chain.
		TransferredTokensFromRelayChain(AccountId, Balance),
		/// Transferred tokens to the account from the given parachain account.
		TransferredTokensViaXCMP(ParaId, AccountId, Balance, DispatchResult),
	}
}

decl_module! {
	pub struct Module<T: Trait> for enum Call where origin: T::Origin, system = frame_system {
		/// Transfer `amount` of tokens on the relay chain from the Parachain account to
		/// the given `dest` account.
		#[weight = 10]
		fn transfer_tokens_to_relay_chain(origin, dest: T::AccountId, amount: BalanceOf<T>) {
			let sender = ensure_signed(origin)?;

			<generic_asset::Module<T>>::burn_free(
				&<generic_asset::Module<T>>::spending_asset_id(), &Self::dex_account_id(), &dest, &amount
			)?;


			let msg = <T as Trait>::UpwardMessage::transfer(dest.clone(), amount.clone());
			<T as Trait>::UpwardMessageSender::send_upward_message(&msg, UpwardMessageOrigin::Signed)
				.expect("Should not fail; qed");

			Self::deposit_event(Event::<T>::TransferredTokensToRelayChain(dest, amount));
		}

		/// Transfer `amount` of tokens to another parachain.
		#[weight = 10]
		fn transfer_tokens_to_parachain_chain(
			origin,
			para_id: u32,
			dest: T::AccountId,
			amount: BalanceOf<T>,
		) {
			//TODO we don't make sure that the parachain has some tokens on the other parachain.
			let who = ensure_signed(origin)?;

			let para_id: ParaId = para_id.into();

			let asset_id = Self::ensure_parachain_currency_exists(&para_id)?;

			<generic_asset::Module<T>>::burn_free(
				&asset_id, &Self::dex_account_id(), &dest, &amount
			)?;

			T::XCMPMessageSender::send_xcmp_message(
				para_id,
				&XCMPMessage::TransferToken(dest, amount),
			).expect("Should not fail; qed");
		}

		fn deposit_event() = default;
	}
}

/// This is a hack to convert from one generic type to another where we are sure that both are the
/// same type/use the same encoding.
fn convert_hack<O: Decode>(input: &impl Encode) -> O {
	input.using_encoded(|e| Decode::decode(&mut &e[..]).expect("Must be compatible; qed"))
}

impl<T: Trait> Module<T> {
	fn ensure_parachain_currency_exists(para_id: &ParaId) -> Result<AssetIdOf<T>, Error<T>> {
		ensure!(
			<CurrencyByParaId<T>>::contains_key(para_id),
			Error::<T>::CurrencyDoesNotExist
		);
		Ok(Self::currency_by_para_id(para_id))
	}

	fn create_default_dex_asset_options(
		asset_id: Option<AssetIdOf<T>>,
	) -> AssetOptions<BalanceOf<T>, T::AccountId> {
		let owner = Owner::Address(Self::dex_account_id());

		let asset_id = if let Some(asset_id) = asset_id {
			asset_id
		} else {
			<generic_asset::Module<T>>::next_asset_id()
		};

		let permissions = PermissionsV1 {
			update: owner.clone(),
			mint: owner.clone(),
			burn: owner.clone(),
		};

		AssetOptions {
			// Set default asset initial issuance right after creation equal to zero,
			// will be updated after minting performed.
			initial_issuance: BalanceOf::<T>::default(),
			// Which accounts are allowed to possess this asset.
			permissions,
		}
	}
}

impl<T: Trait> DownwardMessageHandler for Module<T> {
	fn handle_downward_message(msg: &DownwardMessage) {
		match msg {
			DownwardMessage::TransferInto(dest, amount, _) => {
				let dest = convert_hack(&dest);
				let amount: BalanceOf<T> = convert_hack(amount);

				let relay_chain_currency_id = <generic_asset::Module<T>>::spending_asset_id();
				let dex_account_id = Self::dex_account_id();

				// TODO figure out a better way to ensure relay chain asset was not created

				if !<CurrencyByParaId<T>>::contains_key(ParaId::default()) {
					let dex_asset_options =
						Self::create_default_dex_asset_options(Some(relay_chain_currency_id));

					<generic_asset::Module<T>>::create_asset(
						Some(relay_chain_currency_id),
						Some(dex_account_id.clone()),
						dex_asset_options,
					)
					.expect("Should not fail!");

					<CurrencyByParaId<T>>::insert(ParaId::default(), AssetIdOf::<T>::default());
				}

				<generic_asset::Module<T>>::mint_free(
					&relay_chain_currency_id,
					&dex_account_id,
					&dest,
					&amount,
				)
				.expect("Should not fail!");

				Self::deposit_event(Event::<T>::TransferredTokensFromRelayChain(dest, amount));
			}
			_ => {}
		}
	}
}

impl<T: Trait> XCMPMessageHandler<XCMPMessage<T::AccountId, BalanceOf<T>>> for Module<T> {
	fn handle_xcmp_message(src: ParaId, msg: &XCMPMessage<T::AccountId, BalanceOf<T>>) {
		match msg {
			XCMPMessage::TransferToken(dest, amount) => {
				let dex_account_id = Self::dex_account_id();

				if !<CurrencyByParaId<T>>::contains_key(&src) {
					let asset_id = <generic_asset::Module<T>>::next_asset_id();

					let dex_asset_options = Self::create_default_dex_asset_options(None);

					<generic_asset::Module<T>>::create_asset(
						None,
						Some(dex_account_id.clone()),
						dex_asset_options,
					)
					.expect("Should not fail!");

					<CurrencyByParaId<T>>::insert(src.clone(), asset_id);
				}

				let res = <generic_asset::Module<T>>::mint_free(
					&Self::currency_by_para_id(src.clone()),
					&dex_account_id,
					&dest,
					&amount,
				);

				Self::deposit_event(Event::<T>::TransferredTokensViaXCMP(
					src,
					dest.clone(),
					*amount,
					res,
				));
			}
		}
	}
}

decl_error! {
	pub enum Error for Module<T: Trait> {
		CurrencyDoesNotExist
	}
}
