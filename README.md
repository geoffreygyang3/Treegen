# Treegen - Afforestation DAO

A decentralized autonomous organization for community-led tree planting initiatives with verifiable proof of impact.

## Overview

Treegen enables communities to propose, fund, and verify tree planting projects through a transparent blockchain-based system. Participants can stake tokens, vote on proposals, submit planting proofs, and earn rewards for verified environmental impact.

## Features

- **Proposal Creation**: Submit tree planting initiatives with location, goals, and funding requirements
- **Community Voting**: Stake-weighted voting on proposals with funding mechanism
- **Proof Submission**: Upload evidence of tree planting with photo hashes and location data
- **Verification System**: Community-driven verification of planting proofs
- **Reward Distribution**: Automatic STX rewards for verified tree planting (50,000 microSTX per tree)
- **Staking Mechanism**: Stake STX to participate in governance and earn voting power

## Contract Functions

### Public Functions

#### Governance
- `create-proposal(title, description, location, tree-count, funding-goal)` - Create new tree planting proposal
- `vote-on-proposal(proposal-id, vote, amount)` - Vote on proposals with STX stake
- `finalize-proposal(proposal-id)` - Finalize proposal after deadline
- `stake-tokens(amount)` - Stake STX for governance participation
- `withdraw-stake(amount)` - Withdraw staked STX

#### Planting & Verification
- `submit-planting-proof(proposal-id, tree-count, location, photo-hash, description)` - Submit proof of tree planting
- `verify-proof(proof-id, approved)` - Verify submitted planting proofs
- `claim-rewards()` - Claim earned STX rewards

#### Admin
- `update-min-stake(new-amount)` - Update minimum stake requirement (owner only)

### Read-Only Functions
- `get-proposal(proposal-id)` - Get proposal details
- `get-proof(proof-id)` - Get planting proof details
- `get-user-vote(proposal-id, user)` - Get user's vote on proposal
- `get-user-stake(user)` - Get user's staked amount
- `get-user-planted-trees(user)` - Get user's verified tree count
- `get-user-rewards(user)` - Get user's pending rewards
- `get-contract-stats()` - Get overall contract statistics
- `get-proposal-funding-progress(proposal-id)` - Get proposal funding status
- `is-proposal-active(proposal-id)` - Check if proposal is active

## Usage Instructions

### 1. Stake Tokens
```clarity
(contract-call? .Treegen stake-tokens u1000000) ;; Stake 1 STX
```

### 2. Create Proposal
```clarity
(contract-call? .Treegen create-proposal 
    "Urban Forest Initiative" 
    "Plant 100 trees in downtown area" 
    "Downtown Park, City Center" 
    u100 
    u50000000) ;; 50 STX funding goal
```

### 3. Vote on Proposal
```clarity
(contract-call? .Treegen vote-on-proposal u1 true u5000000) ;; Vote yes with 5 STX
```

### 4. Submit Planting Proof
```clarity
(contract-call? .Treegen submit-planting-proof 
    u1 
    u25 
    "North Section, Downtown Park" 
    "abc123...def456" ;; Photo hash
    "Planted 25 oak saplings with community volunteers")
```

### 5. Verify Proof (Community Verifiers)
```clarity
(contract-call? .Treegen verify-proof u1 true) ;; Approve proof
```

### 6. Claim Rewards
```clarity
(contract-call? .Treegen claim-rewards) ;; Claim earned STX
```

## Economic Model

- **Minimum Stake**: 1 STX (1,000,000 microSTX) to create proposals
- **Tree Reward**: 50,000 microSTX (0.05 STX) per verified tree
- **Voting Power**: Proportional to STX staked
- **Proposal Deadline**: 144 blocks (~24 hours)

## Contract States

### Proposal Status
- `active` - Open for voting
- `approved` - Passed community vote
- `rejected` - Failed community vote

### Proof Status
- `pending` - Awaiting verification
- `verified` - Approved by community
- `rejected` - Rejected by community

## Development

### Testing
```bash
clarinet test
```

### Deployment
```bash
clarinet deploy --testnet
```

## License

MIT License - See LICENSE file for details
