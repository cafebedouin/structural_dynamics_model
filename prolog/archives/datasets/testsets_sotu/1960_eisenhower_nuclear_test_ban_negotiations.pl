% ============================================================================
% CONSTRAINT STORY: 1960_eisenhower_nuclear_test_ban_negotiations
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1960_eisenhower_nuclear_test_ban_negotiations, []).

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
 *   constraint_id: 1960_eisenhower_nuclear_test_ban_negotiations
 *   human_readable: Nuclear Test Ban Verification and Inspection Protocols (1960s Eisenhower Negotiations)
 *   domain: military/arms_control/international_security
 *
 * SUMMARY:
 *   The nuclear test ban negotiation initiated by Eisenhower in January 1960
 *   embodies a core arms control paradox: the constraint that limits nuclear
 *   proliferation and atmospheric contamination simultaneously constrains the
 *   legitimate military security interests of nuclear-armed states, requires
 *   intrusive sovereignty-violating inspections to verify compliance, and
 *   rests on verification mechanisms (seismic detection, radiochemical
 *   monitoring) whose technical adequacy is contested. The constraint is not
 *   a pure coordination mechanism (Rope) because it asymmetrically burdens
 *   nuclear-armed states with loss of testing capability and inspection
 *   intrusions. It is not pure extraction (Snare) because it genuinely
 *   coordinates reduction of fallout and proliferation risk. It is Tangled
 *   Rope: a hybrid structure where the coordination function (stabilizing
 *   arms race, reducing contamination) is genuine but asymmetrically
 *   distributed, and the enforcement mechanism (mandatory inspections) is
 *   itself extractive of sovereign independence. The theater_ratio (0.58)
 *   reflects that verification protocols combine genuinely functional
 *   technical monitoring with performative diplomacy — seismic networks
 *   provide real data but cannot reliably detect small underground tests,
 *   forcing reliance on trust and compliance incentives that function more as
 *   theater than as ironclad verification.
 *
 * KEY AGENTS:
 *   - Non-Nuclear Weapons States and Global Population: Primary victim (powerless/trapped) — bear atmospheric contamination costs with no verification capacity or exit option
 *   - USSR/Emerging Nuclear Powers: Constrained beneficiary-victim (moderate/constrained) — gain from proliferation limitation but lose testing advantage and accept intrusive inspections
 *   - United States (Eisenhower Administration): Primary beneficiary (institutional/arbitrage) — controls verification protocol design, maintains strategic advantage, can resume testing if compliance fails
 *   - Disarmament Coalitions: Organized advocate (organized/mobile) — see test ban as stepping stone to complete abolition with clear sunset logic
 *   - Verification Bureaucracy: Institutional maintainer (institutional/arbitrage) — operates inspection apparatus that persists through organizational inertia even when technical adequacy is contested
 *   - Analytical Observer: Risk of naturalizing contingent institutional arrangement (analytical/analytical) — may mistake arms race dynamics for immutable laws rather than effects of specific strategic incentives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1960_eisenhower_nuclear_test_ban_negotiations, 0.52).
domain_priors:suppression_score(1960_eisenhower_nuclear_test_ban_negotiations, 0.68).
domain_priors:theater_ratio(1960_eisenhower_nuclear_test_ban_negotiations, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1960_eisenhower_nuclear_test_ban_negotiations, extractiveness, 0.52).
narrative_ontology:constraint_metric(1960_eisenhower_nuclear_test_ban_negotiations, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(1960_eisenhower_nuclear_test_ban_negotiations, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1960_eisenhower_nuclear_test_ban_negotiations, tangled_rope).
narrative_ontology:human_readable(1960_eisenhower_nuclear_test_ban_negotiations, "Nuclear Test Ban Verification and Inspection Protocols (1960s Eisenhower Negotiations)").
narrative_ontology:topic_domain(1960_eisenhower_nuclear_test_ban_negotiations, "military/arms_control/international_security").

domain_priors:requires_active_enforcement(1960_eisenhower_nuclear_test_ban_negotiations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1960_eisenhower_nuclear_test_ban_negotiations, non_nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(1960_eisenhower_nuclear_test_ban_negotiations, global_public_health).
narrative_ontology:constraint_victim(1960_eisenhower_nuclear_test_ban_negotiations, nuclear_weapons_states).
narrative_ontology:constraint_victim(1960_eisenhower_nuclear_test_ban_negotiations, verification_compliance_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR STATES / GLOBAL POPULATION (SNARE) — Powerless to exit atmospheric contamination or nuclear proliferation. Bear full cost of weapons testing (fallout exposure, health effects) with no verification capacity. Trapped by geography and economic dependency on nuclear-armed states. No meaningful exit option; extraction flows entirely toward this agent.
constraint_indexing:constraint_classification(1960_eisenhower_nuclear_test_ban_negotiations, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING NUCLEAR POWERS (TANGLED ROPE) — Constrained by mutual verification burden: intrusive inspections on sovereign territory create security risks and loss of testing advantage. But constraint also coordinates reduction of atmospheric contamination and stabilizes arms race dynamics. Mixed extraction and coordination — constrained exit (cannot unilaterally withdraw without isolation) but genuine benefit from credible arms limitation.
constraint_indexing:constraint_classification(1960_eisenhower_nuclear_test_ban_negotiations, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNITED STATES ADMINISTRATION (ROPE) — Institutional beneficiary. Benefits from first-mover advantage in setting verification terms, controls inspection protocols, maintains strategic advantage through established test data. Can arbitrage between negotiated constraint and unilateral testing resumption. Sees constraint as coordination mechanism — establishes mutual confidence while preserving relative advantage. Low experienced extraction.
constraint_indexing:constraint_classification(1960_eisenhower_nuclear_test_ban_negotiations, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISARMAMENT COALITIONS (SCAFFOLD) — Organized agents (UN bodies, peace movements, scientific experts) see the test ban as temporary coordination with sunset logic. Full abolition of nuclear weapons is the ultimate goal; verification protocols are stepping stone. High agency and clear exit path (normative pressure for complete disarmament). Theater ratio reflects that inspections are partly performative confidence-building rather than technically definitive verification.
constraint_indexing:constraint_classification(1960_eisenhower_nuclear_test_ban_negotiations, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: VERIFICATION BUREAUCRACY (PITON) — The institutional apparatus of mutual inspection (monitoring stations, inspection teams, scientific committees) becomes an end in itself. Maintains performative verification rituals (seismic networks, on-site inspections) that lack definitive detection capability. Theater persists through organizational inertia; the constraint mechanism (mutual verification) is largely theatrical since underground testing remains largely unverifiable with 1960s technology. Degraded verification function sustained through institutional momentum.
constraint_indexing:constraint_classification(1960_eisenhower_nuclear_test_ban_negotiations, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, arms race dynamics appear as immutable properties of international anarchy: states cannot credibly commit to arms limitation without independent verification, and perfect verification is technically impossible (Heisenberg-class problem in geophysics). The constraint appears as a natural law governing great power competition. However, the structural data reveals this as a false summit — identifiable beneficiaries (non-nuclear states, disarmament advocates) gain from the constraint, and institutional actors (US administration) benefit from controlling terms.
constraint_indexing:constraint_classification(1960_eisenhower_nuclear_test_ban_negotiations, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1960_eisenhower_nuclear_test_ban_negotiations_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1960_eisenhower_nuclear_test_ban_negotiations, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1960_eisenhower_nuclear_test_ban_negotiations, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(1960_eisenhower_nuclear_test_ban_negotiations, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(1960_eisenhower_nuclear_test_ban_negotiations, TR),
    TR >= 0.70.

:- end_tests(1960_eisenhower_nuclear_test_ban_negotiations_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Nuclear powers lose testing capability and face intrusive verification burdens; non-nuclear states and global population benefit but bear remaining contamination costs. The constraint is extractive toward nuclear powers and beneficiaries of contamination reduction, but the extraction is justified by coordination function (preventing proliferation, reducing fallout). The base value reflects that the constraint is neither pure coordination nor pure extraction, but a hybrid where coordination benefits are asymmetrically distributed. Suppression (0.68): High. Nuclear states face hard constraints (testing prohibition), sovereignty violations (mandatory inspections), and loss of strategic advantage. Non-nuclear states face soft constraints (continued fallout exposure). The average suppression is high because the primary targets (nuclear states) are powerfully suppressed, even though some agents (non-nuclear states) experience lower suppression relative to their power level. Theater ratio (0.58): Moderate. Verification mechanisms combine technical functionality (seismic networks detect large tests reliably) with performative elements (cannot verify small underground tests, relies on compliance incentives and trust). The 1960s-era verification cannot definitively close the loop on compliance, forcing reliance on political confidence-building — theater that serves a real coordination function.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the US institutional beneficiary (who experiences Rope — a coordination mechanism that serves US interests through protocol control) and the constrained nuclear powers (who experience Tangled Rope — asymmetric extraction justified by coordination benefits). The secondary gap is between these strategic actors and the disarmament coalitions (who experience Scaffold — a temporary stepping stone with sunset logic toward complete abolition). The tertiary gap is between surface-level coordination narratives and the piton reality of verification theater — the bureaucratic apparatus maintains seismic stations and inspection protocols as performative confidence-building even though underground testing remains largely unverifiable. The quaternary gap is the false summit: the analytical observer risks treating arms race dynamics and verification impossibilities as natural laws, when they are actually contingent outcomes of specific strategic incentive structures and technology constraints that could shift if inspection capacity improved or testing substitutes became available.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) reflects their structural relationship to testing prohibition and verification burden: (1) Non-nuclear states: d ≈ 0.95 (full targets of extraction — locked into fallout exposure without control or exit). (2) USSR/emerging nuclear powers: d ≈ 0.65 (mixed — bear testing prohibition cost and inspection intrusion cost, but benefit from reducing adversary advantage and proliferation risk; constrained exit makes d higher than pure victim status alone would justify). (3) US administration: d ≈ 0.12 (clear beneficiary — controls protocol design, maintains testing lead, arbitrage options). (4) Disarmament coalitions: d ≈ 0.35 (beneficiary with mobile exit — benefit from norm-setting, have alternate pathways). (5) Verification bureaucracy: d ≈ 0.18 (institutional beneficiary through organizational survival). The chi formula then applies the sigmoid f(d) and scope multiplier σ(global=1.2) to compute effective extractiveness per perspective, accounting for the fact that global scope amplifies the constraint's verification difficulty and thus its hidden extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely Tangled Rope: the coordination function (reducing fallout, preventing proliferation) is real, but the enforcement mechanism (mandatory inspections, testing prohibition) creates asymmetric extraction concentrated on nuclear powers and non-nuclear states. The constraint is not mislabeled as pure coordination (Rope) because the suppression and enforcement asymmetry are structural. It is not mislabeled as pure extraction (Snare) because the coordination benefits — preventing the catastrophic outcome of unconstrained proliferation and atmospheric contamination — are genuine and substantial. The false summit risk is high: strategic actors and analysts may naturalize the constraint as a law of arms race dynamics or geopolitics, obscuring the contingent institutional and technological factors that could shift the classification (better verification technology, testing substitutes, different inspection regime designs). The theater ratio's rise over time (0.42 → 0.58) indicates that as testing prohibition persists without technical verification breakthrough, reliance on performative confidence-building increases — the constraint mechanism shifts weight from functional verification toward diplomatic theater. This trajectory is consistent with Piton degradation if verification capability fails to match inspection burden, or with Scaffold persistence if disarmament coalitions maintain political pressure sufficient to sustain the norm. The analytical observer must avoid naturalizing this dynamic as inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_technical_feasibility,
    'Can seismic and radiochemical monitoring definitively detect underground nuclear tests below militarily significant yields (< 5 kiloton) with 1960s technology?',
    'Technical analysis of detection thresholds; actual underground test detection records from 1960s onwards; signal-to-noise ratios in seismic arrays; radiochemical trace detection limits',
    'If detectable: verification constraint is functionally meaningful (Tangled Rope from all perspectives). If undetectable: verification becomes theater (Piton classification dominates, constraint is performative cover for continued covert testing).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_technical_feasibility, empirical, 'Technical feasibility of underground test detection').

omega_variable(
    soviet_compliance_intention,
    'Do Soviet leadership credibly intend to comply with inspection protocols, or is the test ban a diplomatic move to constrain US testing while preserving Soviet covert capability?',
    'Historical archive analysis of Soviet internal decision-making; retrospective assessment of Soviet underground testing after ban signature; comparison of declared vs actual test activity',
    'If sincere: constraint coordinates mutual limitation (Rope/Tangled Rope from beneficiary perspective). If deceptive: constraint is extraction mechanism masquerading as coordination (Snare from US perspective — trapped by compliance while USSR violates covertly).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soviet_compliance_intention, empirical, 'Soviet compliance intention in test ban negotiations').

omega_variable(
    atmospheric_fallout_irreversibility,
    'What percentage of global atmospheric contamination is irreversible by 1960, and what additional contamination would occur under continued testing vs test ban scenarios over the next 20 years?',
    'Radiochemical decay analysis; atmospheric dispersion modeling; epidemiological correlation between fallout deposition and health outcomes; comparison of actual fallout (if ban fails) vs modeled reduction (if ban succeeds)',
    'If fallout is substantial and partially irreversible: non-nuclear states'' victim status is structurally locked (Snare classification confirmed). If fallout is minimal or fully reversible: extraction claim against non-nuclear states weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atmospheric_fallout_irreversibility, empirical, 'Irreversibility and magnitude of atmospheric contamination').

omega_variable(
    inspection_sovereignty_interpretation,
    'Do mandatory inspection protocols constitute an intrusion on national sovereignty that violates international law, or a legitimate verification mechanism under arms control treaty law?',
    'Legal analysis of treaty precedents; comparison with other arms control verification regimes (IAEA, OSCE); Soviet/US legal objections during negotiations; retrospective assessment of which party treated inspections as sovereignty violation',
    'If violation: suppression metric rises (states see constraint as coercive intrusion) and exit options shift to trapped/constrained. If legitimate: suppression metric falls and exit options shift to constrained/mobile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inspection_sovereignty_interpretation, conceptual, 'Legal status of mandatory inspection protocols under international law').

omega_variable(
    first_mover_verification_advantage,
    'Does the state controlling verification protocols (US) maintain a strategic military advantage by designing inspection regimes optimized for detecting adversary violations while concealing own activities?',
    'Technical analysis of inspection protocol asymmetries; comparison of detection sensitivity for US vs Soviet test signatures; retrospective assessment of whether US used protocol control to maintain testing edge',
    'If yes: extraction concentration on constrained powers (USSR, China) is higher than surface symmetry suggests; beneficiary status of US is enhanced. If no: constraint is more genuinely reciprocal than tangled-rope classification implies (moves toward rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_mover_verification_advantage, empirical, 'Structural advantage from controlling verification protocol design').

omega_variable(
    technological_substitute_availability,
    'Do non-explosive subcritical testing and computer simulation provide militarily adequate substitutes for explosive testing, reducing the extraction cost to nuclear powers?',
    'Technical assessment of subcritical test capability and simulation accuracy; military strategy documents on adequacy of simulation for weapons development; comparison of testing requirements before vs after computer advances',
    'If substitutes are adequate: extraction falls (constrained powers maintain deterrent capability without tests); constraint shifts toward Rope. If substitutes are inadequate: extraction remains high (testing prohibition locks in relative power asymmetry).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_substitute_availability, empirical, 'Adequacy of non-explosive testing substitutes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1960_eisenhower_nuclear_test_ban_negotiations, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ntb_tr_t0, 1960_eisenhower_nuclear_test_ban_negotiations, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ntb_tr_t3, 1960_eisenhower_nuclear_test_ban_negotiations, theater_ratio, 3, 0.5).
narrative_ontology:measurement(ntb_tr_t6, 1960_eisenhower_nuclear_test_ban_negotiations, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(ntb_be_t0, 1960_eisenhower_nuclear_test_ban_negotiations, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ntb_be_t3, 1960_eisenhower_nuclear_test_ban_negotiations, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(ntb_be_t6, 1960_eisenhower_nuclear_test_ban_negotiations, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1960_eisenhower_nuclear_test_ban_negotiations, enforcement_mechanism).
narrative_ontology:affects_constraint(1960_eisenhower_nuclear_test_ban_negotiations, atmospheric_fallout_exposure).
narrative_ontology:affects_constraint(1960_eisenhower_nuclear_test_ban_negotiations, nuclear_proliferation_risk).
narrative_ontology:affects_constraint(1960_eisenhower_nuclear_test_ban_negotiations, sovereignty_inspection_burden).

% DUAL FORMULATION NOTE:
% The nuclear test ban verification constraint is upstream of and affects three related constraints: (1) atmospheric_fallout_exposure (ε ≈ 0.35, Mountain) — the irreversible physical phenomenon of fallout dispersal, which the test ban aims to prevent; (2) nuclear_proliferation_risk (ε ≈ 0.58, Tangled Rope) — the strategic constraint of preventing new nuclear powers, which the test ban partially coordinates; (3) sovereignty_inspection_burden (ε ≈ 0.62, Tangled Rope) — the distinct constraint of mandatory international inspections on sovereign territory, which overlaps with but is structurally separable from the testing prohibition itself. Each story has different ε values reflecting different empirical uncertainty and structural mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1960_eisenhower_nuclear_test_ban_negotiations, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
