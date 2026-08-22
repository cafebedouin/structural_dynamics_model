% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__lapsed_alternatives_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Keyboard Standard: Coordination Lock via Network Adoption Threshold
 *   domain: technology/standards/path-dependence
 *
 * SUMMARY:
 *   The QWERTY keyboard layout persists globally despite being mechanically
 *   suboptimal and ergonomically inferior to alternatives like Dvorak or
 *   Colemak. This constraint story instantiates the
 *   lapsed_alternatives_reading of the qwerty_persistence kernel: the reading
 *   holds that QWERTY persistence is NOT primarily driven by incumbent
 *   beneficiaries defending capital investments, but rather by coordination
 *   failure among potential switchers. The mechanical originators (typewriter
 *   makers) have no ongoing material interest; the constraint persists
 *   because switching requires critical mass adoption, which no alternative
 *   layout can achieve without first solving the coordination problem — a
 *   bootstrapping trap. The reading vindicates network-effect theory: once
 *   critical mass forms around one equilibrium, switching costs alone sustain
 *   it, even absent active enforcement. The sibling
 *   incumbent_preservation_reading attributes persistence to manufacturers
 *   and software houses that have invested in QWERTY-specific tooling and
 *   actively defend the standard; this reading asserts the mechanism is
 *   passive coordination lock, not active defense.
 *
 * KEY AGENTS:
 *   - keyboard_users: globally distributed, identity-locked to learned QWERTY muscle memory, benefit from coordination but cannot escape the lock even if they prefer alternatives
 *   - hardware_manufacturers: organized, benefit from single-design mass production, constrained to QWERTY because changing the standard requires universal adoption
 *   - alternative_layout_advocates: excluded by the bootstrapping problem, not by enforcement; would benefit from better layouts but cannot coordinate the switching
 *   - stenography and specialized communities: observer seat, demonstrate that the QWERTY lock is economically rational but not physically absolute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.38).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Keyboard Standard: Coordination Lock via Network Adoption Threshold").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology/standards/path-dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, 'c4eda4c1-7953-49dc-831f-c471b88eb95d').
narrative_ontology:cs_kernel_codification('c4eda4c1-7953-49dc-831f-c471b88eb95d', implicit).
narrative_ontology:cs_authority_grounding('c4eda4c1-7953-49dc-831f-c471b88eb95d', practice).
narrative_ontology:cs_interpretation_layer_present('c4eda4c1-7953-49dc-831f-c471b88eb95d').
narrative_ontology:cs_reading_relation('c4eda4c1-7953-49dc-831f-c471b88eb95d', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('c4eda4c1-7953-49dc-831f-c471b88eb95d', foundational, critical_mass_coordination_lock).
narrative_ontology:cs_axiom_status(critical_mass_coordination_lock, holdable).
narrative_ontology:cs_axiom_grounding('c4eda4c1-7953-49dc-831f-c471b88eb95d', critical_mass_coordination_lock, empirically_contingent).
narrative_ontology:cs_axiom('c4eda4c1-7953-49dc-831f-c471b88eb95d', secondary, bootstrapping_impossibility_without_coordination).
narrative_ontology:cs_axiom_status(bootstrapping_impossibility_without_coordination, holdable).
narrative_ontology:cs_axiom_grounding('c4eda4c1-7953-49dc-831f-c471b88eb95d', bootstrapping_impossibility_without_coordination, empirically_contingent).
narrative_ontology:cs_reference_frame('c4eda4c1-7953-49dc-831f-c471b88eb95d', mechanical_typewriter_constraint_period).
narrative_ontology:cs_drift_state('c4eda4c1-7953-49dc-831f-c471b88eb95d', contemporary_digital_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('c4eda4c1-7953-49dc-831f-c471b88eb95d', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, keyboard_users).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, hardware_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, keyboard_users).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, hardware_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a single, globally standardized keyboard layout that works across all devices and contexts without relearning. The coordination value is genuine — switching to an alternative layout requires unlearning years of muscle memory and losing compatibility across work, home, and shared environments. The cost they bear is the inability to adopt ergonomically superior or linguistically optimized layouts (Dvorak, Colemak, AZERTY for French, etc.) because those alternatives never achieved critical mass.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_users, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__lapsed_alternatives_reading, keyboard_users, payer).

% Benefit from a single standard that lets them design keyboards once and sell globally without variant configurations. They pay the cost of being locked into manufacturing QWERTY keyboards even if alternative layouts would reduce repetitive-strain injuries or improve productivity for specific use cases. Switching the entire installed base is prohibitively expensive.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, hardware_manufacturers, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__lapsed_alternatives_reading, hardware_manufacturers, payer).

% Would benefit from adopting ergonomically optimized or language-specific keyboard layouts. They are excluded not by active suppression but by the coordination problem itself: any alternative layout requires critical mass adoption among users and manufacturers to become viable. Without that threshold, switching individuals bear the full switching cost while gaining no benefit (incompatibility with shared systems, relearning, isolation). This perpetuates QWERTY not through enforcement but through the economics of network effects.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_advocates, excluded,
    moderate, biographical, constrained, local).

% Have adopted alternative input systems (Dvorak, Colemak, Stenotype) for specialized tasks where the coordination cost is worth bearing because the productivity gain is high enough and the community is small enough to coordinate separately. They demonstrate that the QWERTY lock is not absolute but is economically rational for the general case.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, stenography_and_specialized_communities, observer,
    moderate, biographical, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single globally standardized keyboard layout so that users, devices, and software all operate under one mapping of physical keys to characters. Users need not relearn layouts when switching devices; manufacturers design once; software assumes a single layout. The coordination problem is genuine: if layout changed, every user would need to relearn, every manufacturer would need to retool, and every software system would need remapping. The gain from switching to a superior layout is offset by the universal relearning cost.
% TRANSFER_FUNCTION: Does not transfer wealth or resources between parties; instead, it transfers compatibility. Users transfer the opportunity cost of not adopting alternatives. Manufacturers transfer the cost of perpetual QWERTY production even when alternatives might reduce injury. The transfer is not extraction but rather the distributed cost of maintaining the coordination solution.
% ABSENT_VOICES: Alternative layout inventors and ergonomic researchers who could argue for Dvorak, Colemak, or other superior layouts are not structurally excluded but are effectively absent because their proposals cannot achieve critical mass without solving the coordination problem first — a bootstrapping problem, not enforcement.
% DISAPPEARANCE_RATIONALE: If the QWERTY standard disappeared tomorrow, keyboard markets would fragment into competing layouts. Users would face a coordination problem: should they learn Dvorak, Colemak, Workman, or some alternative? Manufacturers would have to produce multiple variants. Software would need layout detection and conversion. A new equilibrium would eventually emerge (likely a single dominant layout once critical mass forms around a better alternative), but the transition period would impose massive relearning costs. The constraint's disappearance creates chaos, which is why the standard persists.
% FOUNDING_PROBLEM: Early mechanical typewriters required key arrangement to prevent mechanical jams — the mechanical constraint forced specific key spacing. QWERTY was not optimal for speed but was engineered to reduce key collisions in mechanical machines. Once typewriters became ubiquitous and users learned QWERTY, switching to a better layout required relearning for millions of people simultaneously — the collective action problem.
% FOUNDING_PROBLEM_CORROBORATION: Mechanical engineers, typography historians, and typewriter manufacturers from the late 1800s attest that the QWERTY layout solved mechanical jam problems. Modern ergonomic researchers and computational linguists (outside the benefiting parties) attest that the founding problem is solved — modern keyboards have no mechanical constraint and digital systems could map any layout instantly. Yet QWERTY persists because the relearning cost, not mechanical necessity, sustains it.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__lapsed_alternatives_reading_tests).
:- end_tests(qwerty_persistence__lapsed_alternatives_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the coordination value is real and participants genuinely benefit from standardization, but they also bear the cost of being locked into a suboptimal solution. Suppression is very low (0.12) because there is no active enforcement machinery — the standard persists through economic incentives, not coercion. Theater is negligible (0.05) because the standard is presented and functions as what it is: a coordination equilibrium, not as a natural law or performance. Accessibility_collapse is high (0.72) because once you understand the critical-mass problem, alternatives become structurally inaccessible — you cannot switch alone without losing all coordination benefits. Resistance is low (0.28) because the standard is not strongly resisted; people complain about QWERTY ergonomics but rationally choose not to switch because the coordination cost is prohibitive. The temporal trajectory is flat-to-slightly-rising: extractiveness does not increase because there is no rent-seeking layer; suppression rises modestly as ergonomic research documents the harms and builds case for switching, creating diffuse pressure (but not organized resistance). The shared time grid allows all three metrics to be measured at every point.
 *
 * PERSPECTIVAL GAP:
 *   Payers vs. beneficiaries: The lapsed_alternatives_reading declares no beneficiaries because coordination benefits accrue equally to all parties. The incumbent_preservation_reading would declare manufacturers and software vendors as beneficiaries (they defend QWERTY to protect capital), and alternative advocates as victims (they are actively excluded). The computed directionality divergence is the measurement the kernel contest depends on.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading declares ZERO beneficiaries and ZERO victims because the coordination cost is symmetric — all parties bear it equally by virtue of their participation. Users benefit from standardization and pay the switching-cost opportunity cost. Manufacturers benefit from mass production and pay the design-lock cost. Alternative advocates are excluded by the bootstrapping problem (a structural feature of network effects), not by directed extraction. The directionality computation should converge all seats near d=0.5 (symmetric) because no party extracts from the others; instead, the constraint distributes a shared coordination cost. The suppression is structural (the math of critical mass), not enforced (no agent actively suppresses alternatives).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: The founding problem was mechanical constraint (key collision in typewriters) — that problem is dead. Yet QWERTY persists not because of vestigial institutional inertia (piton) but because the founding problem's solution (a standardized layout) created a new coordination problem (critical mass for switching) that sustains the standard independently of the original constraint. This is NOT mandatrophy because the constraint's current function (coordination) is live and functional. The founding_problem_status=dead + disappearance_verdict=world_rearranges pair flags a potential zombie (a solution to a dead problem persisting in vestigial form), but the engine's cross-check should find that the current function is genuinely coordination-critical, not ritualistic. The constraint avoids misclassification as piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_mass_threshold_empirical,
    'What is the precise critical-mass threshold for an alternative keyboard layout to become self-sustaining? Is it a sharp phase transition or a gradual phenomenon?',
    'Natural experiments with regional layout adoption (e.g., AZERTY in France, QWERTZ in Germany persist for language reasons despite global QWERTY dominance) and controlled-rollout studies of new layouts in organizations with enough scale to test threshold effects.',
    'A sharp threshold would support the network-lock reading; a gradual threshold would suggest that other factors (manufacturer preference, path dependency in software) contribute. If threshold is found to be surprisingly low (e.g., 20-30% adoption could bootstrap to dominance), it would suggest active suppression is preventing alternatives, shifting the mechanism toward incumbent_preservation_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_threshold_empirical, empirical, 'Whether the critical-mass constraint is a mathematical network-effect property or a tuned equilibrium maintained by institutional choices.').

omega_variable(
    active_defense_vs_passive_lock,
    'Are manufacturers and software vendors actively refusing to support alternative layouts, or are they simply not prioritizing them because market demand is zero (the bootstrapping trap)?',
    'Historical analysis of software development: did operating systems (Windows, macOS, Linux) and hardware makers make explicit decisions to restrict layout support, or did layout support emerge only when communities demanded it (e.g., Linux distributions supporting Dvorak through community packages, iOS supporting swipe-based layouts)? What would change if a major vendor committed to native alternative-layout support?',
    'If active refusal: shifts the mechanism toward incumbent_preservation_reading, making manufacturers beneficiaries defending capital. If passive (zero market demand): supports the lapsed_alternatives_reading, making the lock a coordination failure, not extraction. The distinction is whether the constraint persists through enforcement or through rational individual choices that collectively trap the system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(active_defense_vs_passive_lock, empirical, 'Whether QWERTY persistence is an active-defense beneficiary mechanism or a passive network-lock coordination mechanism.').

omega_variable(
    identity_lock_mechanism_internalization,
    'Is the identity-lock of keyboard users (they have learned QWERTY and resist switching) structural and internalized, or would it dissolve if a critical-mass alternative became available?',
    'Generational cohort analysis: compare switching costs for young users who have not yet fully learned QWERTY (or have learned multiple layouts for gaming, programming, or regional reasons) vs. established QWERTY users. If young users would readily switch to a better layout, the lock is behavioral/cultural, not mechanical. Controlled experiments with new-user adoption of alternative layouts in environments that support them.',
    'If the lock is internalized identity (users believe QWERTY is ''natural''), the suppression mechanism operates through cognitive capture and the constraint edges toward snare. If the lock is purely structural (alternatives are not available, so no one has rational incentive to pay the cost), it remains a coordination trap. The distinction affects whether the constraint can be broken by information alone or whether infrastructure change is required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalization, empirical, 'Whether keyboard user identity-lock is structural or internalized.').

omega_variable(
    kernel_reading_contest_mechanism,
    'What structural evidence would adjudicate between the lapsed_alternatives_reading (coordination lock via critical mass) and the incumbent_preservation_reading (active defense by beneficiaries)?',
    'Compare the QWERTY constraint to other network-lock standards (e.g., VGA connector, USB, Ethernet) and to standards actively defended by incumbents (e.g., Adobe PDF, MP3 patent pools). Historical analysis of layout adoption in contexts where manufacturers did commit to alternatives (stenography systems, regional keyboards, mobile swipe-based entry). If alternatives flourish when manufacturers support them but languish when they don''t, incumbent_preservation is the operative mechanism. If alternatives languish even when manufacturers are neutral, network lock is operative.',
    'This omega operationalizes the kernel contest itself. The two readings produce different empirical predictions: lapsed_alternatives predicts alternatives will gain traction once critical mass is achieved (manufacturer support is endogenous to demand); incumbent_preservation predicts alternatives will never reach critical mass because manufacturers prevent it (manufacturer support is withheld by design). Testing these predictions resolves which reading is supported by evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_mechanism, conceptual, 'The structural mechanism distinguishing the two readings of the qwerty_persistence kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(qwer_tr_t10, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 10, 0.03).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 20, 0.04).
narrative_ontology:measurement(qwer_tr_t30, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qwer_be_t10, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(qwer_be_t30, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(qwer_su_t10, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 10, 0.09).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement(qwer_su_t30, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(qwer_su_t50, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__lapsed_alternatives_reading, 0.1).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence__incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% The qwerty_persistence kernel admits two structurally distinct constraint readings. The lapsed_alternatives_reading (this story) attributes persistence to network-effect coordination lock: once QWERTY reached critical mass, the switching cost traps the system in a suboptimal equilibrium even in the absence of active enforcement. Epsilon is determined by switching costs alone; no beneficiary set; all parties bear symmetric coordination costs. The incumbent_preservation_reading (sibling story) attributes persistence to manufacturers and software vendors actively defending QWERTY to protect capital investments in tooling. That reading declares vendors as beneficiaries and alternative advocates as victims; epsilon includes both coordination and active rent defense. The readings share a single kernel (why QWERTY persists) but differ on mechanism and thus on beneficiary structure. Testing whether manufacturers support alternatives when demand exists (lapsed_alternatives prediction) or refuse to do so even under demand (incumbent_preservation prediction) would adjudicate the readings. Both stories are valid constraint analyses of the kernel; they model different causal mechanisms and thus different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
