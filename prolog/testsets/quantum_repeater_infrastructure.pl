% ============================================================================
% CONSTRAINT STORY: quantum_repeater_infrastructure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_repeater_infrastructure, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quantum_repeater_infrastructure
 *   human_readable: Quantum Repeater Infrastructure Lock-In and Standardization
 *   domain: quantum_computing/telecommunications/infrastructure
 *
 * SUMMARY:
 *   Quantum repeater infrastructure represents a critical fork in quantum
 *   internet development. A coalition of hardware vendors and standardization
 *   bodies has converged on a repeater architecture and protocol,
 *   establishing de facto global standard through coordinated hardware
 *   releases, spectrum allocations, and regulatory pre-positioning. This
 *   constraint exhibits the tension between legitimate coordination (enabling
 *   global quantum networks requires interoperable repeater standards) and
 *   extractive lock-in (the standardization process concentrated dominance in
 *   early-moving vendors, excluded alternative quantum platforms, and created
 *   path-dependent costs for later adopters and regional autonomy). The
 *   extractiveness has risen over the interval (0.35 → 0.62) as the standard
 *   has calcified and the switching costs for incompatible platforms have
 *   become sunk. The theater ratio has risen (0.32 → 0.52) as vendors
 *   increasingly emphasize performance metrics that favor their protocol
 *   choice (clock speed, fidelity under specific conditions) while
 *   downplaying metrics where alternatives compete (range, flexibility). The
 *   constraint's mendatrophy resolution lies in the simultaneous viability of
 *   the scaffolding perspective — open-source repeater designs and academic
 *   coalitions are genuinely building alternative pathways, though with lower
 *   resource velocity than the proprietary standard.
 *
 * KEY AGENTS:
 *   - Dominant Hardware Vendors (Intel-backed repeater consortium, European quantum initiative members): Institutional/arbitrage beneficiaries — captured early standardization window, established IP moats, secured spectrum allocations
 *   - Competing Quantum Platforms (alternative qubit modalities, non-aligned vendors): Powerless/trapped victims — excluded from standard, face market exclusion, cannot interoperate without abandoning core architecture
 *   - Regional Telecom Authorities (national communications agencies, continental broadband initiatives): Powerful/constrained actors — benefit from quantum backbone coordination; constrained by expensive retrofits and locked vendor relationships
 *   - Small-Nation Quantum Researchers: Moderate/identity_locked agents — structurally mobile (could develop alternatives) but identity-fused with quantum internet participation; career and national competitiveness bound to standardized infrastructure
 *   - Open Quantum Standards Consortium (academic institutions, public research agencies, open-source communities): Organized/mobile coalition — building interoperable alternatives with sunset logic; have agency through pooled resources and regulatory leverage
 *   - Legacy QKD System Defenders: Institutional/arbitrage actors maintaining point-to-point QKD as supposedly 'secure' infrastructure despite functional obsolescence; sunk-cost and organizational identity drive inertia
 *   - Analytical Observer: Risks naturalizing standardization lock-in as a requirement of quantum physics rather than a contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_repeater_infrastructure, 0.58).
domain_priors:suppression_score(quantum_repeater_infrastructure, 0.65).
domain_priors:theater_ratio(quantum_repeater_infrastructure, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_repeater_infrastructure, extractiveness, 0.58).
narrative_ontology:constraint_metric(quantum_repeater_infrastructure, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(quantum_repeater_infrastructure, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_repeater_infrastructure, tangled_rope).
narrative_ontology:human_readable(quantum_repeater_infrastructure, "Quantum Repeater Infrastructure Lock-In and Standardization").
narrative_ontology:topic_domain(quantum_repeater_infrastructure, "quantum_computing/telecommunications/infrastructure").

domain_priors:requires_active_enforcement(quantum_repeater_infrastructure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_repeater_infrastructure, early_standardization_alliance).
narrative_ontology:constraint_beneficiary(quantum_repeater_infrastructure, dominant_hardware_vendors).
narrative_ontology:constraint_victim(quantum_repeater_infrastructure, competing_quantum_platforms).
narrative_ontology:constraint_victim(quantum_repeater_infrastructure, regional_deployment_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPETING PLATFORM (SNARE) — A quantum technology developer whose architecture is incompatible with the emerging repeater standard faces catastrophic market exclusion. Cannot build compatible infrastructure without abandoning core IP; cannot operate quantum networks without the repeater backbone. Trapped by the path-dependent standardization lock-in. Maximum extraction — no coordination benefit, pure exclusion.
constraint_indexing:constraint_classification(quantum_repeater_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL TELECOM AUTHORITY (TANGLED ROPE) — Benefits from quantum repeater standardization (enables quantum internet infrastructure, coordinates cross-border quantum networks). But constrained by the alliance's technical specifications — adapting legacy fiber infrastructure or national security requirements may require costly retrofits. Asymmetric extraction: early adopters captured lower transition costs; later adopters pay premium. Genuine coordination (quantum internet backbone) plus enforced path dependency.
constraint_indexing:constraint_classification(quantum_repeater_infrastructure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: STANDARDIZATION ALLIANCE (ROPE) — Primary beneficiary (dominant vendors + standardization body). Experiences the constraint as coordination: achieving consensus on repeater specifications solves collective action problem of incompatible quantum networks. Net gain from network effects and first-mover advantage. Low extraction overhead — the alignment-extraction difference is acceptable to members because they control the standard.
constraint_indexing:constraint_classification(quantum_repeater_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SMALL-NATION QUANTUM RESEARCHER (TANGLED_ROPE-IDENTITY_LOCKED) — Structurally constrained by resource barriers (expensive repeater hardware, limited spectrum access) but also identity-locked to quantum internet participation: professional identity, career trajectory, and national competitiveness aspirations are fused with adopting the standardized infrastructure. Could theoretically develop alternative; perceives exit as loss of legitimacy. Genuine coordination function (gains access to global quantum networks) and extraction (forced technology path, patent licensing costs, forced vendor lock-in) are inextricable from the agent's self-concept as a quantum researcher.
constraint_indexing:constraint_classification(quantum_repeater_infrastructure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 5: OPEN QUANTUM STANDARDS CONSORTIUM (SCAFFOLD) — Coalition of academic institutions and public research agencies mobilizing around open-source repeater designs and patent-pooling mechanisms. See the current proprietary lock-in as a temporary market phase. Have agency (pooled resources, academic publications, regulatory leverage) and perceive an exit path: interoperable open architectures with sunset horizon of 15-20 years for transition from proprietary to open-source repeater dominance. Active enforcement required now (investment in alternatives) but with declining extraction over time.
constraint_indexing:constraint_classification(quantum_repeater_infrastructure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY QKD SYSTEM (PITON) — Point-to-point quantum key distribution infrastructure deployed in the 2020s persists as 'security infrastructure' despite being superseded by repeater-based quantum networks. Maintenance costs are borne; security claims persist theatrically. The constraint here is institutional inertia: QKD vendors and installed-base defenders continue advocating for upgrades to their system rather than migrating to quantum repeater backbone. Theater ratio high (performative security theater); actual function low (repeaters do the quantum-safe coordination better). Piton classification derives from the theater gate — the legacy system is maintained through sunk cost and organizational identity, not because it works optimally.
constraint_indexing:constraint_classification(quantum_repeater_infrastructure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / QUANTUM PHYSICS VIEW (MOUNTAIN) — From a universal/civilizational perspective, quantum repeater infrastructure is presented as an immutable requirement of quantum physics: quantum states decay (decoherence), so repeaters are necessary to extend communication distance. This perspective naturalizes the standardization lock-in as flowing from physics itself. However, the structural data contradicts this — the extraction is driven by standards dominance, vendor lock-in, and path-dependent adoption, not by physics. The engine's false summit detector will flag this as naturalization of a contingent institutional arrangement masquerading as physics.
constraint_indexing:constraint_classification(quantum_repeater_infrastructure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_repeater_infrastructure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quantum_repeater_infrastructure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quantum_repeater_infrastructure, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_repeater_infrastructure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quantum_repeater_infrastructure, TR),
    TR >= 0.70.

:- end_tests(quantum_repeater_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The standardization process created genuine coordination value (interoperable quantum networks are better than fragmented incompatible systems). However, the extraction premium is substantial: vendors captured first-mover rents, locked in regional telecom authorities through expensive retrofits, and excluded competing platforms from market viability. The metric reflects the empirically observable rent-seeking (premium pricing for proprietary repeaters, patent licensing costs, forced vendor lock-in) layered onto coordination. Suppression (0.65): High. Multiple barriers to exit: switching costs for telecom infrastructure are enormous (sunk fiber investment); competing platforms face regulatory resistance (spectrum already allocated to standard repeaters); small nations face research brain-drain if excluded from quantum network participation (identity lock-in amplifies structural barriers). Theater ratio (0.48): Moderate. Performance metrics emphasizing the chosen standard (fidelity under specific conditions, clock speed) serve as performative differentiation; alternative metrics (range, flexibility) are de-emphasized. The ratio has risen as vendors increasingly publish benchmarks favoring their protocol. Requires active enforcement: YES — spectrum allocation, standards body dominance, licensing agreements, and vendor coordination all require continuous effort to maintain the lock-in.
 *
 * PERSPECTIVAL GAP:
 *   The standardization alliance's rope classification vs. competing platforms' snare classification represents the sharpest perspectival gap. Both are evaluating the same infrastructure; the difference is entirely structural position (insider vs. excluded) and exit options (can influence vs. cannot influence). The tangled rope perspectives (regional authorities, small-nation researchers) occupy middle ground: both coordinate benefits and lock-in costs are real; the balance depends on power level and exit options. The scaffold coalition's optimistic sunset differs from the piton perspective's resignation — both recognize that the current system is suboptimal, but one sees a mobilizable exit path while the other sees institutional inertia. The mountain perspective risks collapsing these distinctions by naturalizing the standardization as physics requirement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the extraction flow. The standardization alliance members (institutional/arbitrage) derive low d — they benefit from the standard, have exit options (can modify the standard or develop proprietary extensions), and the constraint subsidizes their position. Competing platforms (powerless/trapped) derive high d — they are targets of exclusion, have no viable exit (incompatible architecture is sunk), and bear full extraction cost. Regional telecom authorities (powerful/constrained) derive moderate d — they have switching costs but also political leverage, and both coordinate benefits and lock-in costs apply. Small-nation researchers (moderate/identity_locked) derive high d despite moderate power — the identity lock adds friction to their exit options beyond the structural constraint; they perceive the constraint as unchangeable within their professional frame even though structural mobility exists. The scaffold coalition (organized/mobile) derives low d — they have agency (pooled resources) and exit paths (alternatives under development), so effective extraction runs weakly toward them. The piton perspective (institutional/arbitrage) derives arbitrarily low d because the constraint is maintained through sunk cost rather than active extraction — the legacy system benefits from network inertia, not from new extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the coordinate/extraction balance shifts with observer position and time horizon. Short-term (immediate/biographical), the standardization looks like rope for beneficiaries and snare for excluded parties — polarized classification. Medium-term (generational), the scaffold perspective's alternative pathways become visible, and the constraint reclassifies as temporarily extractive rather than permanently locked (tangled rope → scaffold → piton as alternatives mature). Long-term (civilizational), the analytics observer must resist naturalizing the current standardization as a physics requirement and instead trace how the apparent immutability flows from institutional path-dependency, not from quantum mechanics. The mandatrophy resolution is structural: recognize that all six types are simultaneously valid readings from different positions, and that the system's evolution will be determined by whether the scaffold perspective's coalition can overcome the organized beneficiaries' lock-in maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    repeater_protocol_interoperability_ceiling,
    'Are competing repeater protocols fundamentally incompatible at the physical layer, or is incompatibility a choice enforced through IP and standardization?',
    'Detailed technical analysis of protocol layers (photonic, quantum state encoding, entanglement distribution) to identify whether incompatibilities are physics-constrained or policy-constrained; experimental demonstration of cross-protocol repeater linking',
    'If physics-constrained: standardization lock-in is necessary and legitimate (coordinate bottleneck, not snare). If policy-constrained: lock-in is extractive path dependency with sunset alternative (scaffolding becomes viable faster).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(repeater_protocol_interoperability_ceiling, empirical, 'Whether repeater protocol incompatibility is physics-based or policy-based').

omega_variable(
    open_source_repeater_deployment_feasibility,
    'Can open-source quantum repeater designs achieve performance parity with proprietary systems within 10-15 years?',
    'Comparison of fidelity, range, clock speed metrics between open-source prototypes (e.g., QuTiP-based designs) and commercial systems; tracking of academic publication velocity in repeater technology vs proprietary vendor R&D spending',
    'If feasible: scaffold perspective confirmed — open quantum internet sunset is real and mobilizable now. If infeasible: open-source path is aspirational; lock-in extraction persists longer than scaffold assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_repeater_deployment_feasibility, empirical, 'Whether open-source repeater designs can compete with proprietary systems').

omega_variable(
    national_security_quantum_technology_capture,
    'To what extent do national security frameworks (export controls, critical infrastructure designation) amplify standardization lock-in by making alternative technologies politically unviable?',
    'Regulatory analysis of quantum technology export control regimes and critical infrastructure classification; interviews with national research agencies in multiple jurisdictions about technology sovereignty constraints',
    'If high: repeater standardization is jointly enforced by market lock-in AND national security classification (hybrid snare-tangled_rope). The identity-locked small-nation researcher is captured through dual mechanisms (economic + security). If low: security framework is secondary; primary extraction is market-driven.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_security_quantum_technology_capture, conceptual, 'Role of national security frameworks in amplifying standardization lock-in').

omega_variable(
    coherence_window_physics_vs_engineering,
    'Is the perceived need for quantum repeaters driven by fundamental physics (unavoidable decoherence timescales) or by engineering choices in qubit platform design (e.g., trapped ion coherence times could theoretically be extended via shielding)?',
    'Literature review of decoherence limits in different qubit modalities; theoretical and experimental analysis of whether current coherence times reflect physics limits or engineering optimization boundaries',
    'If physics: repeater infrastructure is quasi-mountain (necessary infrastructure, not extractive lock-in). If engineering: repeater standardization is a contingent choice; alternative approaches (longer-lived qubits, local networks) become legitimate and the lock-in is clearly extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_window_physics_vs_engineering, empirical, 'Whether repeater necessity derives from physics or from engineering choices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_repeater_infrastructure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qri_tr_t0, quantum_repeater_infrastructure, theater_ratio, 0, 0.32).
narrative_ontology:measurement(qri_tr_t3, quantum_repeater_infrastructure, theater_ratio, 3, 0.4).
narrative_ontology:measurement(qri_tr_t6, quantum_repeater_infrastructure, theater_ratio, 6, 0.48).
narrative_ontology:measurement(qri_tr_t10, quantum_repeater_infrastructure, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(qri_be_t0, quantum_repeater_infrastructure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qri_be_t3, quantum_repeater_infrastructure, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(qri_be_t6, quantum_repeater_infrastructure, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(qri_be_t10, quantum_repeater_infrastructure, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_repeater_infrastructure, global_infrastructure).
narrative_ontology:boltzmann_floor_override(quantum_repeater_infrastructure, 0.25).
narrative_ontology:affects_constraint(quantum_repeater_infrastructure, quantum_key_distribution_obsolescence).
narrative_ontology:affects_constraint(quantum_repeater_infrastructure, quantum_internet_alliance_governance).
narrative_ontology:affects_constraint(quantum_repeater_infrastructure, cross_border_quantum_network_sovereignty).

% DUAL FORMULATION NOTE:
% Quantum repeater infrastructure is the upstream standardization constraint affecting multiple downstream ecosystem constraints. The quantum key distribution obsolescence story (captured legacy system) treats the QKD→repeater transition as a technology lifecycle. The quantum internet alliance governance story (intra-alliance coordination vs inter-alliance fragmentation) treats repeater standardization as an institutional dominance mechanism. The cross-border quantum network sovereignty story (national quantum network aspirations vs global standardization) treats repeater deployment as a geopolitical constraint. Each story has its own epsilon reflecting different observable dimensions of the same physical infrastructure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_repeater_infrastructure, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
