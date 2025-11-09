# Cross-Contract Game Logic

This project implements a cross-contract game logic in [Clarity](contracts/cross-contract-game-logic.clar) for the Stacks blockchain. It includes smart contract code, configuration, and automated tests.

## Features

- Create and join games with STX bets
- Game state management (waiting, active, finished)
- Player statistics tracking
- Contract balance and game board management
- (Planned) Move validation and winner detection for games like Tic-Tac-Toe

## Project Structure

- [contracts/cross-contract-game-logic.clar](contracts/cross-contract-game-logic.clar): Main Clarity smart contract
- [tests/cross-contract-game-logic.test.ts](tests/cross-contract-game-logic.test.ts): Vitest-based unit tests
- [settings/](settings/): Network and account configuration files
- [Clarinet.toml](Clarinet.toml): Clarinet project configuration

## Getting Started

### Prerequisites

- [Node.js](https://nodejs.org/)
- [Clarinet](https://docs.hiro.so/clarinet/get-started)
- [npm](https://www.npmjs.com/)

### Install Dependencies

```sh
npm install

npm run test:report

clarinet check

Usage
Deploy the contract using Clarinet or your preferred Stacks devnet.
Interact with the contract using the provided public functions:
create-game
join-game
(Planned) make-move
Read-only functions for querying game state and player stats
Development
Contract logic is in contracts/cross-contract-game-logic.clar
Tests are in tests/cross-contract-game-logic.test.ts
Update settings/Devnet.toml for local devnet configuration
Resources
Clarity Language Reference
Clarinet Documentation
Vitest Documentation

