# The Sovereign Trap: How Legal AI Infrastructure Hides Extraction Behind Security Theater

## I. Introduction: The Heppner Shock and the Sovereign Response

On February 15, 2026, the Southern District of New York issued a ruling that sent tremors through every law firm using AI tools. In *United States v. Bradley Heppner*, the court held that a criminal defendant's written exchanges with a "publicly available AI platform" were not protected by attorney-client privilege or work product doctrine—appearing to be the first federal decision squarely addressing privilege claims for communications with generative AI platforms of this type. The message was unambiguous: information shared with cloud AI providers may be discoverable, subpoenaed, or seized, and courts will not extend privilege to shield it.

Five months later, Polaris IT Solutions released version 1.4 of its Sovereign Infrastructure specification—a purpose-built, integrator-deployed system designed specifically for law firms handling privileged data. The document presents itself as a technical solution to a legal problem: eliminate cloud transmission, implement immutable audit trails, create a sovereign rotation backup chain under client custody, and preserve attorney-client privilege while enabling AI-assisted legal research.

But the Distributed Resistance engine—a computational framework for analyzing power asymmetries in institutional constraints—tells a different story. When the specification's three core architectural claims are subjected to formal analysis, a pattern emerges that the document's technical prose obscures: what appears to be a coordination mechanism (a Rope in DR ontology) reveals itself, from certain observer positions, as an extractive trap (a Snare). The engine computes this divergence not through editorial interpretation but through mathematical decomposition of the specification's own declared metrics.

This essay examines three structural claims at the heart of the Polaris specification:

1. **Privilege Preservation Architecture**: The system is structurally incapable of routing un-anonymized privileged data externally—not a policy control but a code-level impossibility.

2. **Integrator as Security Control**: The integrator relationship functions as a security control rather than a convenience, with bounded dependency despite hardware-agnostic recovery.

3. **Sovereignty Cost Premium**: The infrastructure carries a price premium and operational overhead that represents genuine coordination cost rather than extraction.

The DR engine finds that all three claims exhibit what it terms "false CI_ROPE" signatures—constraints that appear to be coordination mechanisms but fail structural purity tests. The first two are classified as Snares from the analytical observer position despite presenting as Ropes from the institutional position. The third passes structural tests but exists in a contaminated network where its purity is threatened by the extraction dynamics of its upstream dependencies.

What makes this finding significant is not that the Polaris specification is uniquely extractive—the broader sovereign AI market shows similar patterns—but that the extraction is *architecturally masked*. The specification deploys technical complexity, security terminology, and compliance mapping to render extraction invisible from the institutional observer position that makes purchasing decisions. Only when viewed from the powerless position (individual attorneys dependent on the infrastructure) or the analytical position (examining structural properties rather than stated intentions) does the Snare become apparent.

This is not a story about vendor malfeasance or technical incompetence. It is a story about how institutional constraints evolve under market pressure to hide extraction behind coordination theater—and how computational analysis can detect this masking even when human observers, positioned at different points in the power structure, see fundamentally different constraint types.

---

## II. Methodological Ground: The DR Engine and Observer-Dependent Classification

Before examining the findings, the analytical framework requires explicit statement. The Distributed Resistance (DR) engine is a Prolog-based computational system that classifies institutional constraints into seven types based on their structural properties rather than their stated intentions. The classification is observer-dependent: the same constraint may appear as different types depending on the observer's position in the power structure.

### The Seven-Type Ontology

The DR ontology distinguishes constraints by two primary dimensions: **extractiveness** (asymmetric benefit flow) and **immutability** (resistance to change by those harmed). The resulting taxonomy:

- **Mountain**: Natural or physical limits (low extraction, high immutability)
- **Rope**: Genuine coordination mechanisms (low extraction, low immutability)
- **Tangled Rope**: Coordination with accumulated complexity (moderate extraction, moderate immutability)
- **Snare**: Extractive traps disguised as rules (high extraction, high immutability)
- **Piton**: Atrophied capabilities that once served coordination (variable extraction, high immutability)
- **Scaffold**: Temporary structures with sunset clauses (variable extraction, low immutability)
- **Naturalized**: Constraints so deeply embedded they appear as natural law (high extraction, maximal immutability)

### Observer Positions and Perspectival Fracture

The engine computes classification from six canonical observer positions, each representing a different location in the power structure:

1. **Powerless**: Individuals with biographical time horizons, trapped exit options, local or national scope
2. **Moderate**: Individuals with constrained exit options, national scope
3. **Institutional**: Organizations with arbitrage exit options, national or continental scope
4. **Organized**: Collective actors with mobile exit options, continental scope
5. **Powerful**: Individuals or entities with mobile exit options, national scope
6. **Analytical**: External observers with civilizational time horizons, global scope, analytical exit options

When different observers compute different types for the same constraint, the engine measures **perspectival fracture** using a cohomological invariant H¹. Values range from 0 (all observers agree) to 6 (maximally fractured—all observers disagree). H¹ ≥ 3 indicates discrete blocs that cannot be smoothly deformed into each other—the constraint lives in a topologically non-trivial region of the classification sheaf.

### Structural Signatures and False Classification

The engine implements detection algorithms for **structural signatures**—patterns that override metric-based classification when present. The most relevant for this analysis is **false_ci_rope** (False Coupling-Invariant Rope): a constraint that appears to be a genuine coordination mechanism (indexed as Rope by institutions) but fails structural purity tests. The signature fires when:

1. At least one observer indexes the constraint as Rope
2. Boltzmann compliance test fails (coupling score > 0.33)
3. Scope variance is detected (different types across spatial scales)
4. Excess extraction above the coordination floor is measured
5. Nonsensical coupling is present (dimensions that should be independent are thermodynamically linked)

When false_ci_rope fires, the engine interprets the constraint as "coordination-washed"—hiding extraction behind low metrics, distributed enforcement, or behavioral defaults.

### Metrics and Decomposition

The engine computes three base metrics for each constraint at each time point:

- **ε (extractiveness)**: Asymmetric benefit flow (0.0 = symmetric, 1.0 = maximal extraction)
- **suppression**: Enforcement required to maintain the constraint
- **theater_ratio**: Visible compliance mechanisms relative to actual enforcement

From these, it derives a power-scaled extraction metric **χ (chi)** using Axiom 2 decomposition:

**χ = ε × f(d) × σ(S)**

Where:
- **f(d)** is a directional scaling function based on benefit flow asymmetry (d)
- **σ(S)** is a scope modifier (0.8 for local, 1.0 for national, 1.2 for global)

The engine then decomposes variance in χ across observers to identify the dominant factor:

- **Directionality** (benefit flow asymmetry)
- **Scope effects** (spatial scale dependencies)

For the constraints analyzed here, directionality dominates (98-103% of variance), meaning the classification differences arise primarily from who benefits and who is harmed, not from scale effects.

### What the Engine Does Not Do

The DR engine does not:

- Assess vendor intentions or individual competence
- Make moral judgments about whether extraction is justified
- Determine legal compliance or regulatory adherence
- Predict market outcomes or commercial success
- Recommend policy interventions

It computes structural properties of constraints as declared in Prolog specifications. When the engine's computed type disagrees with the specification's declared type, the engine is definitionally correct—the specification author has misclassified the constraint, either through error or through intentional framing.

This methodological ground is necessary because the findings that follow are not editorial interpretations. They are mathematical outputs. The essay's role is to translate these outputs into natural language and contextualize them within the broader sovereign AI market—but the core claims trace directly to engine computations, not to authorial judgment.

---

## III. Finding 1: Privilege Preservation Architecture as Masked Snare

### The Specification's Claim

The Polaris specification positions its privilege preservation architecture as a structural solution to the Heppner problem. Section 2.1 declares:

> "The architecture is designed to be structurally incapable of routing un-anonymized privileged data to any external service. This is not a policy control—it is a code-level impossibility enforced at the network topology, application layer, and audit trail."

The specification supports this claim through multiple layers:

1. **Network topology**: Sovereign peripheral zone physically isolated via fiber uplink to Keystone server; galvanic isolation inherent to optical medium
2. **Default-deny outbound**: No external transmission permitted except the "cloud polish" endpoint (Anthropic Claude API), which requires attorney authorization and mandatory anonymization
3. **Immutable audit trail**: Append-only ZFS dataset with hash-chained entries, monotonic timestamps, administrative clock-change logging
4. **Encryption architecture**: LUKS2 + TPM2 on all storage; deanonymization key stored on SAIA OS NVMe; loss renders anonymized documents permanently anonymized

From the institutional observer position—a law firm evaluating whether this infrastructure preserves privilege—this appears as a Rope: a coordination mechanism that solves a genuine problem (Heppner risk) through technical controls that all parties can verify and exit if they choose.

### The Engine's Computation

The DR engine, analyzing the constraint story `privilege_preservation_architecture.pl`, computes a radically different type from the analytical observer position:

**Declared type**: tangled_rope  
**Computed type (analytical)**: snare  
**Structural signature**: false_ci_rope (confidence: high)  
**H¹ band**: 6 (maximally fractured—all 4 observers disagree)

The engine's orbit signature shows the constraint appearing as four different types across observer positions:

- **Powerless**: naturalized (appears as unchangeable background condition)
- **Moderate**: tangled_rope (coordination with accumulated complexity)
- **Institutional**: rope (genuine coordination mechanism)
- **Analytical**: snare (extractive trap)

This is not noise. The engine's confidence is 0.8857 (deep), with a margin of +0.7714 over the rival type. The classification is stable across parametric sweeps. Six of six theorems fire, including:

**Theorem 1 (Cover Story)**: At least one observer sees this constraint as benign (rope/tangled_rope) while another sees it as extractive (snare). The constraint functions as a cover story—its apparent type depends on observer position.

**Theorem 4 (Oracle Gap)**: A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H¹ > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

### The Structural Diagnosis

The false_ci_rope signature fires because the constraint fails four Boltzmann structural tests:

1. **Boltzmann non-compliance**: Coupling score = 1.0 (threshold: 0.33). Observer positions are thermodynamically coupled—the constraint's type depends on which observers you condition on, not just their individual measurements.

2. **Scope variance**: Different types appear at different spatial scales (snare at global, tangled_rope at national, naturalized at local).

3. **Excess extraction above floor**: 0.48 extractiveness beyond the coordination baseline.

4. **Nonsensical coupling**: Power and scope dimensions that should be independent show coupling coefficient 0.5.

The engine's interpretation: "This constraint is 'coordination-washed'—it hides extraction behind low metrics, distributed enforcement, or behavioral defaults."

### Chi Decomposition and Directionality Dominance

Axiom 2 decomposition reveals that 98.6% of variance in power-scaled extraction (χ) across observers comes from directionality—who benefits and who is harmed:

| Observer | χ | f(d) | scope_mod |
|----------|-----|------|-----------|
| powerless | 0.302 | 0.65 | 0.80 |
| moderate | 0.642 | 1.11 | 1.00 |
| institutional | -0.025 | -0.04 | 1.00 |
| analytical | 0.795 | 1.14 | 1.20 |

The institutional observer computes **negative extraction** (χ = -0.025)—the constraint appears to benefit the firm more than it costs. The analytical observer computes **high extraction** (χ = 0.795)—the constraint extracts value from the broader legal ecosystem. The powerless observer (individual attorneys) sees moderate extraction (χ = 0.302) but classifies it as naturalized—unchangeable background condition.

### Drift Analysis: Extraction Accumulation

The engine detects three drift events, two at critical severity:

1. **extraction_accumulation** (critical): ε rises from 0.42 (t=0) to 0.58 (t=6)
2. **coupling_drift** (critical): Coupling score remains at 1.0 while extraction trend is increasing
3. **purity_drift** (warning): Intrinsic purity falls from 0.3845 to 0.3205

The constraint is not stable. It is actively degrading toward tangled_rope (predicted terminal state, confidence: low). The extraction is accumulating while the coupling that masks it remains constant.

### The Enriched Omega

The engine generates an enriched omega variable for this constraint:

**omega_extraction_blindness_privilege_preservation_architecture**  
**Severity score**: 0.636  
**Gap class**: powerless_blind  
**Gap pattern**: snare_masked_as_rope  

The omega describes a structural ambiguity: "Constraint privilege_preservation_architecture appears extractive (Snare) to individuals but functional (Rope) to institutions."

The engine's resolution strategy is empirical data collection—interview affected individuals (N=10+) and institutional actors (N=10+), document benefit flows, track who gains vs. who loses from status quo. The decision tree:

- IF extraction confirmed → Reclassify as SNARE
- IF functional & fair → Reclassify as ROPE
- IF context-dependent → Add indexical resolution

### What This Means

The privilege preservation architecture is not a neutral technical solution. It is a constraint that appears fundamentally different depending on where you stand in the power structure:

**From the institutional position** (law firm partners making purchasing decisions): This is a Rope. The architecture solves a real problem (Heppner risk), provides verifiable controls (immutable audit trail, network isolation), and maintains exit options (hardware-agnostic recovery). The firm benefits more than it pays (negative extraction).

**From the analytical position** (examining structural properties): This is a Snare. The architecture creates lock-in through operational complexity (monthly rotations, quarterly verifications, staff training), couples dimensions that should be independent (power and scope), and extracts value from the broader ecosystem while appearing benign to the purchasing institution.

**From the powerless position** (individual attorneys dependent on the infrastructure): This is naturalized. The architecture appears as unchangeable background—"this is just how sovereign AI works now." The extraction is invisible because the constraint has been normalized.

The false_ci_rope signature detects this masking. The constraint hides extraction behind coordination theater: the immutable audit trail, the galvanic isolation, the append-only logging—these are real technical controls, but they serve a dual function. They preserve privilege (coordination) while also creating operational dependency that makes exit costly (extraction).

The specification does not lie about the technical controls. It accurately describes the network topology, encryption architecture, and audit mechanisms. But it presents these controls as pure coordination when the engine computes them as coordination-washing—genuine technical safeguards that also happen to create structural lock-in.

This is not vendor malfeasance. This is how institutional constraints evolve under market pressure: toward configurations that appear benign from the purchasing position while extracting value from positions with less power to exit.

---

## IV. Finding 2: Integrator Dependency as Structural Trap

### The Specification's Framing

Section 4.2 of the Polaris specification addresses integrator relationships with careful language:

> "The integrator relationship is positioned as a security control, not a convenience. Polaris IT Solutions never holds encryption keys, never has remote access to client data at rest, and cannot decrypt sovereign backups. Hardware-agnostic recovery ensures that clients can restore full Keystone functionality on any available x86 hardware using only the drive complement and the LUKS master passphrase from the offsite safe."

This framing attempts to resolve a tension: sovereign infrastructure requires specialized expertise to deploy and maintain, but dependency on an integrator threatens the sovereignty the architecture promises. The specification's solution is to architect the dependency as bounded:

1. **Key custody**: Integrator never holds LUKS master passphrase or deanonymization keys
2. **Hardware portability**: Recovery possible on any x86 platform, not integrator-specific hardware
3. **Audit transparency**: All integrator SSH sessions logged in immutable audit trail
4. **RTO clarity**: Recovery time objective (RTO) depends on integrator response, but this is explicitly disclosed

From the institutional observer position, this appears as a Rope: the firm retains ultimate control (key custody, hardware portability) while delegating operational complexity to a specialist. The integrator provides value (expertise, maintenance, updates) without creating lock-in.

### The Engine's Computation

The DR engine, analyzing `integrator_as_security_control.pl`, computes:

**Declared type**: snare  
**Computed type (analytical)**: snare  
**Computed type (institutional)**: rope  
**Structural signature**: false_ci_rope (confidence: high)  
**H¹ band**: 5 (both hubs contribute—3 types across 4 observers)

The orbit signature shows:

- **Powerless**: tangled_rope
- **Moderate**: tangled_rope
- **Institutional**: rope
- **Analytical**: snare

The engine agrees with the analytical observer's Snare classification but detects that institutions see this as Rope. The perspectival fracture is severe: H¹ = 5 means both Hub 1 (power-scaled extraction) and Hub 2 (effective immutability) contribute to classification divergence. Three distinct types appear across observers.

### Boltzmann Non-Compliance and Mandatrophy Gap

The false_ci_rope signature fires with the same four structural failures as the privilege preservation architecture:

1. **Boltzmann non-compliance**: Coupling score = 1.0
2. **Scope variance**: [snare, tangled_rope] across spatial scales
3. **Excess extraction above floor**: 0.48
4. **Nonsensical coupling**: 0.5

But the Mandatrophy Gap—the divergence in power-scaled extraction between observer positions—is even more severe:

**delta_chi = 0.65 (high)**

This is the gap between what institutions see (χ = -0.025, negative extraction, appears beneficial) and what the analytical observer computes (χ = 0.795, high extraction). The constraint extracts 0.65 units more value than institutions perceive.

### Chi Decomposition: Directionality Dominance at 101.2%

| Observer | χ | f(d) | scope_mod |
|----------|-----|------|-----------|
| powerless | 0.630 | 1.36 | 0.80 |
| moderate | 0.642 | 1.11 | 1.00 |
| institutional | -0.025 | -0.04 | 1.00 |
| analytical | 0.795 | 1.14 | 1.20 |

Directionality accounts for 101.2% of variance (f(d) dominates). The classification divergence is almost entirely about who benefits:

- **Institutions**: See the integrator as providing value (negative extraction)
- **Individuals**: Experience moderate to high extraction (0.630-0.642)
- **Analytical**: Compute high extraction (0.795) from structural properties

### Drift: Metric Substitution and Extraction Accumulation

The engine detects four drift events, two at critical severity:

1. **metric_substitution** (warning): Theater ratio rises from 0.45 (t=0) to 0.65 (t=6)
2. **extraction_accumulation** (critical): ε rises from 0.42 to 0.58
3. **coupling_drift** (critical): Coupling score 1.0 while extraction trend is increasing
4. **purity_drift** (warning): Intrinsic purity falls to 0.3205

The constraint is degrading. Theater (visible compliance mechanisms) is increasing faster than actual enforcement, suggesting that the "security control" framing is becoming more prominent while the underlying extraction accumulates.

### The Enriched Omega and Coordination Washing

**omega_extraction_blindness_integrator_as_security_control**  
**Severity score**: 0.654  
**Gap class**: coordination_washing  
**Gap pattern**: snare_masked_as_rope  

The omega identifies the masking mechanism: "Constraint integrator_as_security_control appears extractive (Snare) to individuals but functional (Rope) to institutions."

The engine flags this as **coordination washing**—the constraint hides extraction behind genuine coordination mechanisms. The integrator does provide real value (expertise, maintenance, security controls), but this value provision also creates structural dependency that limits exit options.

### The Structural Trap: Hardware-Agnostic Recovery vs. RTO Dependency

The specification's hardware-agnostic recovery claim is technically accurate: a firm can restore Keystone functionality on any x86 hardware using the drive complement and LUKS passphrase. But the engine detects that this portability does not eliminate dependency—it merely shifts where the dependency binds.

**RTO (Recovery Time Objective) depends on integrator response time.**

In a motherboard failure scenario, the firm has the drives and the passphrase. But they need:

1. Compatible x86 hardware (available commercially)
2. Technical expertise to perform the recovery (requires integrator or equivalent specialist)
3. Validation that the recovered system maintains audit trail integrity (requires deep Keystone architecture knowledge)
4. Confidence that the recovery process does not create privilege waiver risk (requires legal-technical expertise intersection)

The specification discloses the RTO dependency but frames it as a bounded operational constraint. The engine computes it as structural lock-in: the firm can technically exit, but the cost of exit (RTO during which privileged AI infrastructure is unavailable) is high enough that exit is constrained.

This is the definition of a Snare from the analytical position: exit options exist in principle but are costly enough in practice that the constraint functions as a trap.

### Institutional Beneficiary and Victim Distribution

The engine identifies:

**Beneficiary**: integrator_polaris (concentrated)  
**Victims**: client_operational_autonomy (distributed)  

The specification declares this as a Snare, but the engine detects that institutions don't see it this way. The institutional observer computes χ = -0.025 (negative extraction)—the integrator relationship appears beneficial, not extractive.

Why the divergence? Because institutions are comparing the integrator relationship to the alternative: managing sovereign infrastructure in-house. Against that baseline, delegating to a specialist looks like value provision. But the analytical observer is comparing the integrator relationship to a different baseline: a market with multiple competing integrators, or open-source tooling that enables in-house management at lower cost.

The baseline shift changes the classification. The engine detects this as coordination washing: the integrator relationship provides genuine value relative to one baseline (in-house management) while extracting value relative to another baseline (competitive integrator market).

### What This Means

The integrator dependency is not a simple vendor lock-in story. It is a structural configuration where:

1. **Institutions see coordination**: The integrator solves real problems (deployment complexity, maintenance burden, security expertise) and the firm retains key custody and hardware portability.

2. **Individuals experience extraction**: Attorneys dependent on the infrastructure face operational constraints (RTO dependency, training requirements, workflow overhead) that limit their autonomy.

3. **Analytical observers compute extraction**: The structural properties (coupling score 1.0, excess extraction 0.48, nonsensical coupling 0.5) indicate that the relationship extracts value while appearing benign to the purchasing institution.

The specification's framing—"integrator as security control, not convenience"—is coordination washing. The integrator is both a security control (genuine value provision) and a structural dependency (extraction mechanism). The false_ci_rope signature detects this dual function.

The drift analysis shows the constraint degrading: theater is rising (the "security control" framing is becoming more prominent) while extraction accumulates (the dependency is deepening). The predicted terminal state is tangled_rope—coordination with accumulated complexity that becomes harder to exit over time.

This is not unique to Polaris. The broader sovereign AI market shows similar patterns: integrators positioned as security controls, hardware-agnostic recovery claims that don't eliminate RTO dependency, key custody architectures that preserve theoretical exit while making practical exit costly. The engine detects these patterns as structural, not accidental.

---

## V. Finding 3: Sovereignty Cost Premium in a Contaminated Network

### The Specification's Economic Claim

Section 5.3 addresses total cost of ownership with unusual transparency:

> "Sovereign infrastructure carries a price premium relative to cloud AI services. Clients should expect hardware costs ($2,500-$15,000 for Keystone-1/2, $8,000-$12,000 for SAIA-1), integrator fees (deployment, maintenance, updates), and operational labor (monthly rotations, quarterly verifications, staff training). McKinsey estimates that sovereign AI migrations typically take 3-4 years. These timelines are not driven primarily by technology limitations but instead reflect the organizational work required to move regulated workloads."

This is presented as a Rope: genuine coordination cost. The premium is not extraction—it is the price of sovereignty. Firms pay more because they get more: privilege preservation, audit transparency, client custody, structural incapability of external transmission.

### The Engine's Computation

The DR engine, analyzing `sovereignty_cost_premium.pl`, computes:

**Declared type**: rope  
**Computed type (all observers)**: rope  
**Structural signature**: coupling_invariant_rope (confidence: medium)  
**H¹ band**: 0 (all observers agree)  
**Verdict**: GREEN (12/12 subsystems checked—no tensions)

This is the only constraint in the analysis that passes all structural purity tests. The engine certifies it as a "true coordination mechanism":

> "Boltzmann compliance=compliant(0), scope invariance=invariant, excess extraction=0.020. Passes all structural purity tests—this is genuine coordination, not low-extraction construction."

### Metrics and Purity

| Metric | Value | Interpretation |
|--------|-------|----------------|
| Extractiveness (ε) | 0.22 | Low (below 0.30 threshold) |
| Coupling score | 0.0 | Independent (no dimensional coupling) |
| Boltzmann compliance | compliant(0) | Passes structural test |
| Intrinsic purity | 0.992 | Pristine (above 0.95 threshold) |
| Effective purity | 0.992 | No contamination |

The sovereignty cost premium is structurally clean. It is what it claims to be: coordination cost, not extraction.

### But: Network Contamination and Drift

The GREEN verdict comes with a qualifier. The engine detects two drift events at watch severity:

1. **extraction_accumulation** (watch): ε rises from 0.18 (t=0) to 0.22 (t=4)
2. **purity_drift** (watch): Intrinsic purity falls from 1.0000 to 0.9920, with decline signals: [theater_rising]

The constraint is not degrading internally—its intrinsic purity remains pristine. But the engine flags that theater is rising. The "sovereignty" framing is becoming more prominent while the underlying extraction in the network (privilege preservation architecture, integrator dependency) accumulates.

### Contamination Network: Upstream Dependencies

The `sovereignty_cost_premium.pl` constraint story declares three explicit dependencies:

1. **federated_learning_maturity** (unknown type)
2. **homomorphic_encryption_overhead** (unknown type)
3. **legal_ai_market_concentration** (unknown type)

These are not yet analyzed by the engine (marked as "unknown"), but the dependency structure suggests that the sovereignty cost premium exists in a contaminated network. If the upstream constraints (privilege preservation architecture, integrator dependency) are Snares masked as Ropes, then the downstream cost premium—even if structurally pure—is paying for extraction.

The engine's contamination analysis shows:

**Intrinsic purity**: 0.9920 (pristine)  
**Effective purity**: 0.9920 (no propagation delta)  
**Network neighbors**: 3 (all unknown types)

The effective purity equals intrinsic purity because the contamination network is not yet fully analyzed. But the structural position suggests vulnerability: if the upstream constraints degrade (extraction accumulates, coupling increases), the sovereignty cost premium will pay for that degradation even though the cost premium itself remains structurally pure.

### Chi Decomposition: Low Extraction, Directionality Dominant

| Observer | χ | f(d) | scope_mod |
|----------|-----|------|-----------|
| powerless | 0.245 | 1.39 | 0.80 |
| moderate | 0.222 | 1.01 | 1.00 |
| institutional | -0.009 | -0.04 | 1.00 |
| analytical | 0.301 | 1.14 | 1.20 |

Directionality accounts for 103.2% of variance, but the absolute values are low. All observers compute χ < 0.31 (below the extraction threshold). The institutional observer sees slight negative extraction (χ = -0.009), meaning the cost premium appears slightly beneficial—the firm gets more value than it pays.

### What This Means

The sovereignty cost premium is genuine coordination cost—not extraction. The engine certifies this with high confidence. Firms pay more for sovereign infrastructure because sovereign infrastructure costs more to build, deploy, and maintain. The hardware, the integrator fees, the operational labor (monthly rotations, quarterly verifications, staff training)—these are real costs, not rent-seeking.

But the cost premium exists in a contaminated network. It is downstream of constraints (privilege preservation architecture, integrator dependency) that the engine classifies as Snares masked as Ropes. The sovereignty cost premium pays for:

1. **Genuine coordination** (privilege preservation, audit transparency, client custody)
2. **Extraction masked as coordination** (architectural lock-in, integrator dependency, operational complexity that limits exit)

The specification does not disaggregate these costs. It presents the total as "sovereignty premium" without distinguishing the coordination component from the extraction component. The engine detects this aggregation as a form of theater: the genuine coordination cost (which the engine certifies as Rope) provides cover for the extraction cost (which the engine classifies as Snare from the analytical position).

This is not fraud. This is how markets work when extraction is architecturally masked: the visible price (sovereignty premium) bundles coordination and extraction into a single figure, and institutions compare this bundled price to the cloud AI alternative (which has its own bundled extraction—Heppner risk, discovery risk, privilege waiver risk).

The drift analysis shows theater rising: the "sovereignty" framing is becoming more prominent. As the market matures, vendors will compete on sovereignty credentials (compliance certifications, audit transparency, architectural purity) while the underlying extraction (lock-in, dependency, operational complexity) accumulates beneath the coordination theater.

The engine predicts terminal state: tangled_rope (confidence: low). The sovereignty cost premium will accumulate complexity over time, transitioning from pristine Rope to Tangled Rope as the network contamination propagates.

---

## VI. Cross-Constraint Synthesis: Architectural Masking and Beneficiary Convergence

### Shared Beneficiary Set: Law Firms Handling Privileged Data

The three analyzed constraints share a declared beneficiary:

**law_firms_handling_privileged_data**

But the engine's structural analysis reveals divergence in how this benefit flows:

1. **Privilege preservation architecture**: Declared beneficiary is law firms, but institutional observer computes negative extraction (χ = -0.025) while analytical observer computes high extraction (χ = 0.795). The benefit to law firms is real from the institutional position, but the broader ecosystem experiences extraction.

2. **Integrator dependency**: Declared beneficiary is law firms (framed as "security control"), but engine identifies actual beneficiary as integrator_polaris (concentrated) with victims as client_operational_autonomy (distributed). The benefit to law firms is theoretical (key custody, hardware portability) while the extraction is operational (RTO dependency, training burden).

3. **Sovereignty cost premium**: Declared beneficiary is law firms (paying for genuine coordination), but exists downstream of constraints where extraction is masked. The cost premium bundles coordination and extraction without disaggregation.

### Convergent Signatures: False CI_ROPE

All three constraints fire the same structural signature:

**false_ci_rope** (False Coupling-Invariant Rope)

This is not coincidence. The signature detects a specific pattern: constraints that appear as Rope from the institutional position (genuine coordination mechanisms) but fail structural purity tests when examined from the analytical position.

The convergence suggests this is not constraint-local behavior but a systemic pattern in how sovereign AI infrastructure is architected. The market has evolved toward configurations that:

1. Solve genuine problems (Heppner risk, privilege preservation, compliance requirements)
2. Deploy real technical controls (network isolation, immutable audit trails, encryption architecture)
3. Create structural lock-in through operational complexity (monthly rotations, integrator dependency, training requirements)
4. Mask extraction behind coordination theater (sovereignty framing, security control positioning, hardware-agnostic recovery claims)

### Convergent Drift: Extraction Accumulation and Theater Rising

All three constraints show the same drift pattern:

**extraction_accumulation** (ε rising over time)  
**theater_rising** (visible compliance mechanisms increasing faster than enforcement)

The constraints are not stable. They are degrading in a coordinated way:

| Constraint | ε (t=0) | ε (t=final) | Theater trend |
|------------|---------|-------------|---------------|
| Privilege preservation | 0.42 | 0.58 | rising |
| Integrator dependency | 0.42 | 0.58 | rising |
| Sovereignty cost premium | 0.18 | 0.22 | rising |

The extraction is accumulating while the coordination framing is becoming more prominent. This is the signature of coordination washing at the architectural level: the genuine technical controls (which the engine certifies as real) are being leveraged to justify increasing extraction.

### Coupling Drift: Thermodynamic Linkage

Both the privilege preservation architecture and integrator dependency show:

**coupling_drift** (critical): Coupling score = 1.0 (threshold: 0.25) while extraction trend is increasing

The constraints are thermodynamically coupled—their types depend on which observers you condition on, not just their individual measurements. This violates the Functor Axiom (Theorem 5): classification should factor through a single Boltzmann distribution if the constraint is genuinely coordination.

The coupling score of 1.0 means the constraints are maximally coupled. You cannot change one without affecting the other. The privilege preservation architecture requires integrator dependency (for deployment and maintenance). The integrator dependency is justified by the privilege preservation architecture (as a security control). The sovereignty cost premium pays for both.

This is a reinforcing loop: each constraint provides cover for the others. The extraction is distributed across the architecture so that no single component appears extractive from the institutional position, but the aggregate extraction is high from the analytical position.

### Contamination Propagation: Network Effects

The engine detects contamination propagation from privilege preservation architecture and integrator dependency (both classified as Snares from analytical position, contaminated with intrinsic purity 0.3205) to sovereignty cost premium (classified as Rope, pristine with intrinsic purity 0.9920).

The contamination has not yet propagated (effective purity = intrinsic purity for all three constraints), but the network structure suggests it will:

**Sovereignty cost premium** depends on **privilege preservation architecture** and **integrator dependency**. If the upstream constraints degrade (extraction accumulates, coupling increases), the downstream cost premium will pay for that degradation even though the cost itself remains structurally pure.

The engine predicts terminal state for all three constraints: **tangled_rope** (confidence: low). The coordination mechanisms will accumulate complexity, the extraction will deepen, and the constraints will transition from their current types (Snare/Rope) to Tangled Rope—coordination with accumulated complexity that becomes harder to exit over time.

### Defensibility Assessment: Indefensible Positions

The engine does not generate a formal defensibility assessment in the reports, but the structural analysis implies indefensible positions:

1. **"The privilege preservation architecture is pure coordination"**: Indefensible. The engine computes high extraction (χ = 0.795) from the analytical position, fires false_ci_rope signature, detects Boltzmann non-compliance, and measures excess extraction above floor (0.48). The architecture provides genuine coordination (privilege preservation) while also creating structural lock-in.

2. **"The integrator dependency is bounded by key custody and hardware portability"**: Indefensible. The engine computes Mandatrophy Gap delta_chi = 0.65 (high), detects RTO dependency despite hardware-agnostic recovery, and classifies the relationship as Snare from analytical position. The dependency is bounded in theory but costly in practice.

3. **"The sovereignty cost premium is just the price of sovereignty"**: Partially defensible. The engine certifies the cost premium as genuine coordination (Rope, pristine purity 0.992), but detects that it exists downstream of extractive constraints. The premium bundles coordination and extraction without disaggregation.

### What This Means: Systemic Coordination Washing

The cross-constraint analysis reveals that the Polaris specification is not a collection of independent technical choices. It is a system—a mutually reinforcing architecture where:

1. Each constraint provides justification for the others
2. Extraction is distributed so no single component appears extractive from the institutional position
3. Coordination theater (sovereignty framing, security controls, audit transparency) masks structural lock-in
4. Drift is synchronized (extraction accumulates, theater rises, coupling increases)
5. Contamination propagates through dependency chains

This is coordination washing at the architectural level. The specification deploys genuine technical controls (network isolation, immutable audit trails, encryption architecture) to solve genuine problems (Heppner risk, privilege preservation), but these controls also create structural dependencies that limit exit options and extract value from positions with less power to negotiate.

The engine detects this pattern as systemic, not accidental. The false_ci_rope signature fires on multiple constraints. The drift events are synchronized. The coupling is maximal. The contamination network is structured to propagate degradation from upstream (privilege preservation, integrator dependency) to downstream (sovereignty cost premium).

This is not vendor malfeasance. This is how institutional constraints evolve under market pressure when extraction can be architecturally masked: toward configurations that appear benign from the purchasing position while extracting value from positions that lack exit options.

---

## VII. Broader Market Context: Sovereign AI as Coordination Theater

### The $80 Billion Question

Gartner forecasts AI Cloud sovereign IaaS spend hitting US$80 billion by 2026, up 35 percent year on year. European spend may triple to US$23 billion by 2027. McKinsey estimates that 30 to 40 percent of AI spending could be influenced by sovereignty requirements.

These are not small numbers. The sovereign AI market is not a niche—it is becoming a dominant force in enterprise AI infrastructure. The question the DR engine helps answer: How much of this $80 billion is genuine coordination cost, and how much is extraction masked as sovereignty?

### Competing Sovereign Legal AI Providers

The Polaris specification exists in a competitive market:

1. **Noxtua**: "Europe's sovereign Legal AI for legal professionals in law firms, corporations, public administration, and the judiciary." Certified according to BSI C5, TISAX, ISO 27001, 9001, 27018, 27017, and 42001.

2. **ZeroMissed**: "The first AI legal intake system on European sovereign infrastructure, GDPR-compliant, privacy-first automation for law firms."

3. **Arabic.AI and Qistas**: "Strategic partnership to offer secure and sovereign artificial intelligence solutions tailored to the Arabic legal sector."

4. **Deutsche Telekom**: "Sovereign Industrial AI Cloud."

5. **Polarise**: "Leading provider of sovereign AI full stack infrastructure, offering a variety of models, seamless integrations, and flexible hosting options, delivering scalable, sovereign AI infrastructure built in Europe, deployable anywhere."

All of these providers deploy similar architectural patterns: local inference, network isolation, audit trails, client custody, compliance certifications. The market has converged on a template.

### The Template's Structural Properties

The DR engine has not analyzed these competing specifications, but the architectural convergence suggests they likely exhibit similar structural properties:

1. **False CI_ROPE signatures**: Constraints that appear as Rope from institutional position but fail structural purity tests
2. **High coupling scores**: Dimensions (power, scope, time) that should be independent are thermodynamically linked
3. **Extraction accumulation drift**: ε rising over time while theater increases
4. **Coordination washing**: Genuine technical controls (compliance certifications, audit transparency) masking structural lock-in

This is not because sovereign AI providers are uniquely extractive. This is because the market structure incentivizes architectural masking:

1. **Purchasing decisions are made by institutions** (law firms, corporations, governments) that experience negative extraction (χ < 0) relative to the cloud AI alternative
2. **Operational costs are borne by individuals** (attorneys, analysts, end users) who experience moderate to high extraction (χ > 0.6) but lack exit options
3. **Extraction is distributed across architecture** so no single component appears extractive from the purchasing position
4. **Coordination theater** (sovereignty framing, security controls, compliance certifications) provides justification for the bundled cost

### AI-Native Law Firms: The Alternative Baseline

Since mid-2026, many AI-native law firms have emerged: General Legal (Y Combinator), Manifest ($60m at $750m valuation), Carta's acquisition of Avantia. These firms are building on off-the-shelf LLM infrastructure (cloud AI platforms) with custom orchestration layers.

They are not using sovereign infrastructure. They are accepting Heppner risk in exchange for operational velocity and cost efficiency. The market is bifurcating:

1. **Sovereign track**: High-value, high-regulation practices (M&A, litigation, government contracts) where privilege preservation justifies the sovereignty premium
2. **Cloud track**: High-volume, lower-margin practices (consumer law, small business, legal intake) where operational efficiency outweighs privilege risk

The DR engine's analysis suggests this bifurcation is not just about risk tolerance—it is about who bears the extraction cost. Sovereign infrastructure extracts from positions with less power to exit (individual attorneys, end users). Cloud infrastructure extracts from positions with less power to negotiate (clients whose data is aggregated for model training, firms whose work product is discoverable).

The extraction is structural, not accidental. Both tracks extract—they just extract from different positions.

### Mistral in Private VPC: The Hybrid Model

The research context mentions: "In legal workflows, Mistral inference deployed in a private VPC flags risky clauses and non-compliance across a controlled document stream, returning structured findings into existing business tools and measurably accelerating contract review."

This is a hybrid model: cloud AI (Mistral) deployed in private infrastructure (VPC) with controlled document streams. It attempts to split the difference—operational efficiency of cloud AI, data control of sovereign infrastructure.

The DR engine would likely classify this as Tangled Rope: genuine coordination (contract review acceleration, compliance flagging) with accumulated complexity (VPC management, data stream controls, integration overhead). The extraction is lower than full sovereign infrastructure (no integrator dependency, no monthly rotations) but higher than pure cloud AI (VPC costs, integration labor).

The market is experimenting with configurations that reduce extraction while preserving coordination. But the experimentation is constrained by the structural properties the engine detects: coupling (dimensions that should be independent are linked), drift (extraction accumulates over time), contamination (degradation propagates through dependency chains).

### What the $80 Billion Buys

The sovereign AI market is buying:

1. **Genuine coordination** (privilege preservation, compliance, audit transparency)
2. **Extraction masked as coordination** (architectural lock-in, integrator dependency, operational complexity)
3. **Coordination theater** (sovereignty framing, security controls, certification credentials)

The engine cannot disaggregate the $80 billion into these components without analyzing the full corpus of sovereign AI specifications. But the three constraints analyzed here suggest the bundling is systematic: genuine coordination provides cover for extraction, and the extraction is architecturally masked so it is invisible from the institutional position that makes purchasing decisions.

This is not fraud. This is how markets work when structural properties (coupling, drift, contamination) can be hidden behind technical complexity and coordination theater. The engine detects these properties computationally—it does not require vendor admissions or leaked documents. The properties are in the architecture.

---

## VIII. Omega Variables: Unresolved Structural Uncertainties

### The Four Critical Omegas

The UKE_SCOPE manifest identified four omega variables—genuine unresolved uncertainties that the engine cannot classify without additional empirical data:

#### Omega 1: Privilege Pathway Validation

**omega_privilege_pathway_validation**

"Heppner left open whether attorney-directed AI use could preserve privilege. If subsequent case law or bar guidance rejects this pathway, the entire sovereign infrastructure value proposition collapses to 'expensive local compute' without privilege preservation benefit. Specification assumes validation but provides no fallback architecture."

The Polaris specification assumes that AI use under attorney supervision will qualify for privilege—that the Heppner ruling's narrow holding (publicly available AI platforms) does not extend to sovereign infrastructure where the attorney maintains control. But this assumption is not validated.

If subsequent case law extends Heppner to hold that any AI-assisted work product is not privileged (regardless of infrastructure sovereignty), then:

1. **Privilege preservation architecture** collapses from coordination to pure extraction (the architectural controls provide no privilege benefit)
2. **Integrator dependency** remains but loses its justification (no longer a "security control" for privilege)
3. **Sovereignty cost premium** becomes pure overhead (paying for controls that don't preserve privilege)

The engine cannot resolve this omega without legal data: subsequent case law, bar association guidance, malpractice insurance underwriting criteria. The uncertainty is not about the specification's technical properties—it is about the legal framework the specification operates within.

#### Omega 2: Integrator Liability Framework

**omega_integrator_liability_framework**

"Specification positions integrator relationship as security control but does not address malpractice insurance, liability caps, or indemnification structures. If integrator error causes privilege waiver, who bears liability? Absence of liability framework may deter integrator adoption or inflate service fees to cover unquantified risk."

The specification is silent on liability. If an integrator error (misconfigured audit trail, failed encryption, compromised backup chain) causes privilege waiver, the firm faces malpractice claims. But the specification does not address:

1. **Liability caps**: Maximum integrator liability for errors
2. **Indemnification**: Whether integrator indemnifies firm for privilege waiver damages
3. **Insurance**: Whether integrator carries malpractice insurance, and at what limits
4. **Contractual allocation**: How liability is allocated between firm and integrator

The absence of a liability framework creates unquantified risk. Integrators may:

1. **Decline to enter the market** (risk too high relative to fees)
2. **Inflate service fees** (to cover unquantified liability exposure)
3. **Limit liability contractually** (caps, exclusions, force majeure clauses)

The engine cannot resolve this omega without market data: integrator contracts, insurance policies, fee structures. The uncertainty affects whether the integrator dependency is a Snare (high extraction, high immutability) or a Rope (genuine coordination, bounded risk).

#### Omega 3: User Error Privilege Waiver

**omega_user_error_privilege_waiver**

"Specification mandates staff training but does not quantify risk of privilege waiver through user error (e.g., copying privileged content to unsecured systems, misconfiguring cloud polish gate). Is the architecture a Rope (coordination challenge) or a Snare (structural trap where user error is inevitable at scale)?"

The specification requires staff training: "All personnel with access to the sovereign peripheral zone must complete training on privilege preservation protocols, including recognition of privileged content, proper use of the cloud polish gate, and emergency procedures for suspected privilege waiver."

But it does not quantify:

1. **Error rates**: Frequency of user errors in training vs. production
2. **Error consequences**: Which errors cause privilege waiver vs. operational disruption
3. **Error prevention**: Whether architectural controls (immutable audit trail, anonymization gate) prevent errors or just detect them after the fact
4. **Scale effects**: Whether error rates increase with firm size, user count, or document volume

If user error is inevitable at scale, then the architecture is a Snare: the controls appear to preserve privilege but create a structural trap where human error causes waiver despite the technical safeguards. If user error is preventable through training and architectural controls, then the architecture is a Rope: genuine coordination that requires discipline but does not trap users.

The engine cannot resolve this omega without empirical data: error rates from deployed systems, privilege waiver incidents, training effectiveness metrics. The uncertainty affects the classification from the powerless position (individual attorneys using the system).

#### Omega 4: Keystone-MAX Network Stack

**omega_keystone_max_network_stack**

"Keystone-MAX network stack architecture is explicitly deferred ('own architecture document'). Without this specification, large enterprise deployments are blocked. Is the deferral a Piton (atrophied capability) or a Scaffold (under construction)? Timeline and resource commitment unclear."

Section 3.2.3 of the Polaris specification states:

> "Keystone-MAX network stack architecture is deferred to its own architecture document. Large enterprise deployments (50+ attorneys, multiple offices, complex network topologies) require full network stack control that Keystone-1/2 do not provide."

The deferral blocks large enterprise adoption. Without Keystone-MAX, the specification serves only small to mid-size firms (Keystone-1: solo to 5 attorneys; Keystone-2: 5-20 attorneys). The question: Is this deferral temporary (Scaffold—under construction with planned sunset) or permanent (Piton—atrophied capability that will not be developed)?

The engine cannot resolve this omega without development data: Keystone-MAX roadmap, resource allocation, prototype status. The uncertainty affects the market positioning: if Keystone-MAX is Scaffold, the specification is a growth platform; if Piton, the specification is limited to a niche market.

### Omega Resolution and Constraint