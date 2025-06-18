# HealthRecord Access Smart Contract

A decentralized medical record sharing platform built on Stacks blockchain that enables secure and controlled access to patient health records.

## Overview

HealthRecord Access is a smart contract system that manages medical record access permissions between patients and healthcare providers. It implements a robust consent management system with doctor verification capabilities.

## Features

- Patient self-registration and consent management
- Doctor registration and verification system  
- Granular access control permissions
- Emergency access capabilities
- Read-only verification endpoints

## Contract Functions

### Patient Management

| Function | Description |
|----------|-------------|
| `register-patient` | Allows patients to register themselves on the platform |
| `grant-access` | Patients can grant access to verified doctors |
| `revoke-access` | Patients can revoke previously granted access |

### Doctor Management  

| Function | Description |
|----------|-------------|
| `register-doctor` | Doctors can register on the platform |
| `verify-doctor` | Contract owner verifies registered doctors |
| `is-verified-doctor` | Check verification status of a doctor |

### Access Control

| Function | Description |
|----------|-------------|
| `check-access` | View access permissions between patient and doctor |

## Error Codes

| Code | Description |
|------|-------------|
| `100` | Not authorized to perform action |
| `101` | Entity already registered |
| `102` | Entity not registered |

## Data Structure

The contract uses three main data maps:

```clarity
(define-map patients 
    principal 
    {consent-status: bool}
)

(define-map doctors
    principal 
    {verified: bool}
)

(define-map access-permissions
    {patient: principal, doctor: principal}
    {granted: bool, emergency-access: bool}
)
```

## Usage

1. Deploy the contract to Stacks blockchain
2. Patients register using their wallet
3. Doctors register and await verification
4. Contract owner verifies legitimate doctors
5. Patients grant/revoke access to verified doctors
6. Doctors can check their access status for specific patients

## Security

- Only contract owner can verify doctors
- Access controls ensure proper authorization
- Patients maintain full control over their records
- Built-in verification checks for all operations

## Development

This project uses [Clarinet](https://github.com/hirosystems/clarinet) for development and testing.

### Prerequisites

- Clarinet
- Stacks wallet
- Node.js

### Local Development

1. Clone the repository
2. Install dependencies
3. Run Clarinet console
4. Deploy and test contract functions

## License

MIT

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
