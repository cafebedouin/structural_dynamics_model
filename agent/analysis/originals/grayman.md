Technical Brief: The "Sovereign Mirror" Architecture

1. Overview
   The Sovereign Mirror is an air-gapped, high-integrity recovery and archival system designed for mission-critical infrastructure (Identity, MFA, and Secret Management). It prioritizes physical custody, hardware independence, and zero-cloud reliance. It is designed for "Sovereign" environments where the risk of remote compromise or cloud-provider lockout outweighs the convenience of automated clustering.
2. The Core Components
   The Primary Node (NVMe): An encrypted server utilizing a "Capped Partition" strategy (e.g., a fixed 160GB system partition on larger hardware) to ensure bit-for-bit portability.
The Rotation Set (SATA SSDs): A triad of physical, encrypted clones (Drives A, B, and C) rotated through fireproof safes.

The Vault: A dedicated, encrypted logical volume containing the "Nexus" of data: Authentik databases, MFA keys, and tiered KeePassXC credential files.

The Ledger: A persistent, encrypted archival partition on each rotation drive that stores Grandfather-Father-Son (GFS) versioned backups.

3. Operational Workflow
   The system utilizes a dual-layer backup strategy: System Mirroring (Monthly) and Incremental Archiving (Daily).
A. Monthly Identity Mirroring (The System Recovery)
The entire system partition is bit-streamed to a rotation drive.
Identity Re-birthing: Post-clone, the mirror undergoes a metadata "re-birth"—generating new unique LUKS and LVM identifiers.
Collision Prevention: This allows the mirror to exist on the same hardware as the primary without naming conflicts, acting as a "Friendly Twin" ready for immediate takeover.

B. Daily Incremental Archiving (The Ledger)
Data from the Vault is archived daily to the Ledger on the currently inserted drive.
Residual History: Utilizing rsync with hard links, the Ledger maintains a historical record of changes to the password databases and configuration states.
Physical Air-Gap: Periodically, the drive in the machine is swapped with an older copy from a safe, ensuring a physical "circuit breaker" against ransomware or system corruption.

4. Recovery Protocol
   Recovery is a two-stage "Successive Approximation" process:
Stage 1 (Availability): Boot from the SATA Mirror. This restores the OS and core services to the state of the last monthly clone.

Stage 2 (Continuity): The "onboard" live Vault is rebuilt using the latest Daily Ledger backup. This reconciles the monthly "Gold Image" with the most recent daily records, limiting Maximum Data Loss to < 24 hours.

5. Architectural Trade-offs & Limitations
   Scheduled Downtime: Unlike high-availability clusters, this model requires periodic downtime for system imaging and recovery verification.
Recovery Time Objective (RTO): Recovery involves manual intervention (F12 boot and Vault reconciliation), resulting in minutes of downtime rather than sub-second failover.

Storage Caps: The "capped partition" strategy limits the volume of data the system can manage. While ideal for a lean "Identity Nexus," it requires careful scaling if the machine also acts as a primary file server.

Maintenance Discipline: The security of the system is entirely dependent on the rigor of the physical rotation and vault-unlocking discipline.

6. Why This Model?
   Sovereignty: Zero internet dependency. Immune to cloud outages or remote account seizure.
The Bus Factor: A yearly offsite drive, secured by a YubiKey + PIN, allows authorized survivors to restore the environment without deep technical tribal knowledge.

Forensic Fidelity: Bit-for-bit mirroring ensures that the recovery environment is a verified, hardened replica of the production system.
