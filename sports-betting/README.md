
# Sports Betting Smart Contract

## About
This smart contract implements a decentralized sports betting platform on the Stacks blockchain. It allows users to create betting events, place stakes, and claim winnings based on the outcome of various sporting events.

## Features

- Create betting events with multiple options
- Place stakes on betting events
- Close betting events
- Settle bets and declare winners
- Claim winnings for successful bets

## Contract Overview

The contract consists of several main components:

1. **Data Structures**:
   - `bets`: Stores information about each betting event
   - `stakes`: Tracks individual user stakes for each betting event

2. **Constants**:
   - `contract-owner`: The address that deployed the contract
   - Error codes for various scenarios

3. **Public Functions**:
   - `create-bet`: Create a new betting event
   - `place-stake`: Place a stake on a betting option
   - `close-bet`: Close a betting event to prevent further stakes
   - `settle-bet`: Declare the winning option for a closed bet
   - `claim-winnings`: Allow winners to claim their winnings

4. **Read-only Functions**:
   - `get-bet`: Retrieve information about a specific betting event
   - `get-stake`: Get stake information for a specific user and betting event

## Usage

### Creating a Bet

To create a new betting event, call the `create-bet` function with the following parameters:

- `event-description`: A description of the betting event (max 256 ASCII characters)
- `betting-options`: A list of betting options (max 10 options, each max 64 ASCII characters)
- `betting-close-height`: The block height at which betting will close

Example:
```clarity
(contract-call? .sports-betting create-bet "World Cup Final" (list "France" "Argentina") u100000)
```

### Placing a Stake

To place a stake on a betting event, use the `place-stake` function:

- `betting-event-id`: The ID of the betting event
- `chosen-option`: The index of the chosen betting option
- `stake-amount`: The amount of STX to stake

Example:
```clarity
(contract-call? .sports-betting place-stake u0 u1 u1000000)
```

### Closing a Bet

Only the event creator can close a bet using the `close-bet` function:

- `betting-event-id`: The ID of the betting event to close

Example:
```clarity
(contract-call? .sports-betting close-bet u0)
```

### Settling a Bet

Only the contract owner can settle a bet by calling the `settle-bet` function:

- `betting-event-id`: The ID of the betting event to settle
- `winning-option-id`: The index of the winning option

Example:
```clarity
(contract-call? .sports-betting settle-bet u0 u1)
```

### Claiming Winnings

Winners can claim their winnings using the `claim-winnings` function:

- `betting-event-id`: The ID of the settled betting event

Example:
```clarity
(contract-call? .sports-betting claim-winnings u0)
```

## Security Considerations

- Only the contract owner can settle bets
- Bets can only be closed by their creators
- Users can only claim winnings if they chose the correct option
- The contract uses various checks to prevent unauthorized actions and ensure proper bet settlement

## Author
Chukwudi Daniel Nwaneri