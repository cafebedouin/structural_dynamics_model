% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__first_held_reading, []).

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
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: Electronic Money Emergence (First Institutional Holding)
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This reading instantiates electronic money emergence as a discrete
 *   institutional event: the moment when the first institutional bearer (a
 *   central bank or commercial bank) held and recognized dematerialized
 *   currency as legally equivalent to physical notes. The reading treats
 *   emergence as observable threshold tied to legal/regulatory recognition
 *   and institutional practice, not to technological possibility or
 *   statistical measurement. The constraint declares dematerialization as the
 *   structurally determinative boundary. This is a KERNEL READING — three
 *   competing framings of the same contested question (when did electronic
 *   money emerge?) are authored as three separate constraints: this one
 *   (first institutional holding), became_thinkable_reading (conceptual
 *   possibility), and m4_m5_collapse_reading (statistical artifact). Each
 *   reading has its own ε, its own beneficiary/victim structure, and its own
 *   classification; the reading relations below document how they relate.
 *
 * KEY AGENTS:
 *   - Central banks: first institutional bearers, set legal recognition of dematerialized holdings
 *   - Commercial banks: benefit from deposit-based money transfer infrastructure enabled by the constraint
 *   - Financial regulators: gain enforcement jurisdiction over electronic money once dematerialized and measurable
 *   - Technology providers: design and profit from digital infrastructure instantiating electronic holdings
 *   - Physical currency users: coexist with electronic money but not directly organized by the constraint
 *   - Monetary theorists and technology skeptics: excluded from the institutional decision-making that sets the dematerialization boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.31).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.12).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, mountain).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "Electronic Money Emergence (First Institutional Holding)").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:emerges_naturally(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, 'e1138ba8-abee-4a19-9828-7f6e2425e721').
narrative_ontology:cs_kernel_codification('e1138ba8-abee-4a19-9828-7f6e2425e721', formalized).
narrative_ontology:cs_authority_grounding('e1138ba8-abee-4a19-9828-7f6e2425e721', extraction).
narrative_ontology:cs_interpretation_layer_present('e1138ba8-abee-4a19-9828-7f6e2425e721').
narrative_ontology:cs_reading_relation('e1138ba8-abee-4a19-9828-7f6e2425e721', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1138ba8-abee-4a19-9828-7f6e2425e721', electronic_money_emergence__m4_m5_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('e1138ba8-abee-4a19-9828-7f6e2425e721', foundational, institutional_legal_recognition_constitutes_emergence).
narrative_ontology:cs_axiom_status(institutional_legal_recognition_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('e1138ba8-abee-4a19-9828-7f6e2425e721', institutional_legal_recognition_constitutes_emergence, conventional).
narrative_ontology:cs_axiom('e1138ba8-abee-4a19-9828-7f6e2425e721', foundational, dematerialization_as_ontological_threshold).
narrative_ontology:cs_axiom_status(dematerialization_as_ontological_threshold, holdable).
narrative_ontology:cs_axiom_grounding('e1138ba8-abee-4a19-9828-7f6e2425e721', dematerialization_as_ontological_threshold, deontological).
narrative_ontology:cs_reference_frame('e1138ba8-abee-4a19-9828-7f6e2425e721', institutional_legal_dematerialization_threshold).
narrative_ontology:cs_drift_state('e1138ba8-abee-4a19-9828-7f6e2425e721', contemporary_digital_asset_contestation, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('e1138ba8-abee-4a19-9828-7f6e2425e721', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, commercial_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, financial_regulators).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__first_held_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, ExtMetricName, E),
    domain_priors:suppression_score(electronic_money_emergence__first_held_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(electronic_money_emergence__first_held_reading),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(electronic_money_emergence__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is modest (0.31 at interval end) because the constraint is primarily a coordination achievement (solving the scaling problem), not a pure extraction mechanism. The beneficiaries (central banks, regulators, technology providers) do collect rents and authority from the dematerialization boundary, but those rents are necessary costs of coordination rather than pure extraction. Suppression is very low (0.12) because the constraint does not depend on coercing belief — once dematerialized currency is legally recognized and technologically accessible, institutions adopt it readily. Theater is also low (0.18): the constraint functions as intended, delivering real scaling improvements; the performative overlay is minimal. Accessibility collapse is moderate-high (0.72) because once dematerialized currency is institutionally recognized, physical cash alternatives persist but become legally demoted — alternatives do not fully collapse, but their equivalence weakens. Resistance is moderate (0.41) because technology skeptics and alternative monetary theorists resist the dematerialization boundary, but their resistance does not prevent institutional adoption. The time series show gradual increase in extractiveness and suppression over the 1920–1980 interval as the institutional framework solidified and regulatory authority accumulated.
 *
 * PERSPECTIVAL GAP:
 *   Central banks and regulators experience the dematerialization boundary as a natural organizational boundary — a threshold built into the world's monetary architecture. Commercial banks and technology providers experience it as an enabling constraint that opens profit opportunities. Monetary theorists and technology skeptics experience it as an institutional choice point: the boundary could have been drawn differently (at conceptual possibility, or at statistical measurement). The engine computes per-seat classifications from the power/exit/beneficiary data; the gaps between seats reflect their structural asymmetry in setting and accepting the boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and financial regulators are the structural beneficiaries and agenda-setters — they define the dematerialization boundary and control its enforcement. Commercial banks benefit from the enabling infrastructure the constraint provides. Technology providers benefit from the demand it creates for digital payment infrastructure. Physical currency users are observers: they coexist but are not directly organized by the constraint. Monetary theorists and technology skeptics are excluded from the institutional decision-making that sets the boundary, despite having stake in the question. The constraint's directionality is primarily beneficiary-side (d near 0.0 for the agenda-setters, d near 0.3 for beneficiaries) because the institutional framers gain authority and operational capacity, while those excluded or who disagree experience the constraint as imposed rather than chosen.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (scaling monetary supply beyond physical circulation) remains live and the constraint delivers real coordination benefit in solving it. There is no mandatrophy — the constraint has not outlived its function. The modest extractiveness (0.31) correctly reflects that the constraint's primary function is coordination, not extraction. If extraction accumulated significantly above coordination cost, that would signal institutional rent-seeking layered onto the coordination function, which would be captured in rising theater_ratio and extractiveness over time. The measurements show gradual accumulation (extractiveness rising from 0.08 to 0.31), suggesting that over the 60-year interval, regulatory and financial-institutional authority did concentrate around the dematerialization boundary. This is consistent with a rope that is slowly accumulating Tangled Rope or Snare characteristics — a pattern the corpus should track, but the constraint's current state remains primarily coordinating function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_conceptual_boundary,
    'Is the dematerialization boundary a natural threshold in the world, or a constructed institutional choice point where alternative boundaries would have been equally functional?',
    'Comparative institutional history: examine alternative monetary systems (digital-first economies, historical parallel currencies, theoretical proposals) to test whether dematerialization is the inevitable organizing boundary or one choice among alternatives that could have been made.',
    'If institutional choice: the constraint is partly false-summit (benefits particular institutions who gain authority over the boundary-setting), suggesting Tangled Rope classification. If natural threshold: the constraint is genuinely mountain-like (would re-emerge under any institutional regime attempting to scale monetary supply). High extractiveness accumulated around this boundary suggests institutional choice; conceptual inevitability would suggest lower extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_conceptual_boundary, conceptual, 'Whether dematerialization is an inevitable boundary or contingent institutional choice.').

omega_variable(
    beneficiary_vs_coordination_benefit_decoupling,
    'Do the benefits captured by central banks and regulators track the coordination costs they incur, or do they accumulate extraction above and beyond coordination function?',
    'Cost accounting: measure the actual institutional resources central banks and regulators deploy to coordinate electronic money systems versus the authority and profit they extract from controlling the dematerialization boundary. Compare to alternative coordination models (decentralized, peer-based, or technology-native payment systems) to establish baseline coordination cost.',
    'Wide decoupling (high authority/profit vs. low coordination cost) would support reclassification from Mountain or Rope toward Tangled Rope or Snare. The rising extractiveness over the measurement interval (0.08 to 0.31) suggests extractive accumulation; this omega would quantify whether it exceeds coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_vs_coordination_benefit_decoupling, empirical, 'Whether institutional beneficiaries'' gains reflect coordination costs or accumulate extraction above them.').

omega_variable(
    false_summit_candidate_natural_law_claim,
    'Is dematerialized currency a natural outcome of technological development and economic scaling (mountain), or is it a constraint whose operation benefits identifiable institutions and whose adoption depends on their enforcement (Tangled Rope or Snare disguised as mountain)?',
    'Historical counterfactual: examine whether alternative institutional choices at key moments (1920s banking crises, 1950s computerization, 1970s stagflation) could have sustained physical-currency-dominant or radically alternative monetary systems without dematerialization. Test whether dematerialization re-emerges as ''necessary'' under institutional regimes that did not adopt it.',
    'Demonstrates whether dematerialization is inevitable (mountain) or contingent on institutions that benefit from it maintaining the boundary (false summit, reclassifying to Tangled Rope or Snare per the false_summit_mountain signature override chain). The declared beneficiaries (central_banks, financial_regulators) on a constraint claimed as mountain triggers FSM evaluation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_candidate_natural_law_claim, empirical, 'Whether dematerialization is natural law or institutional constraint falsely claimed as inevitable.').

omega_variable(
    reading_boundary_vs_sibling_readings,
    'Does the ''first institutional holding'' boundary correctly mark the emergence of electronic money, or do the ''became thinkable'' or ''M4/M5 statistical'' readings better capture the actual moment when electronic money entered the world and affected behavior?',
    'Trace institutional decisions, regulatory filings, and economic outcomes: did behavior change when dematerialized currency was first held institutionally, or earlier (when it became conceptually possible) or later (when statistics defined it retroactively)? Which moment actually altered economic incentives and organizational practice?',
    'If ''became thinkable'' moment drove behavior change earlier: this reading backdates emergence and overstates the dematerialization moment''s causal weight; emergence is earlier, reducing this constraint''s distinctiveness. If ''M4/M5'' moment retroactively created the category: this reading misdates emergence and treats as ontological what was statistical; the constraint becomes a measurement artifact. The kernel contest is unresolved; each reading occupies a legitimate analytical seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_vs_sibling_readings, empirical, 'Whether institutional holding is the correct boundary for emergence, or whether earlier/later moments better explain when electronic money actually appeared and mattered.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 1920, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1920, electronic_money_emergence__first_held_reading, theater_ratio, 1920, 0.05).
narrative_ontology:measurement(elec_tr_t1935, electronic_money_emergence__first_held_reading, theater_ratio, 1935, 0.08).
narrative_ontology:measurement(elec_tr_t1950, electronic_money_emergence__first_held_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(elec_tr_t1960, electronic_money_emergence__first_held_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__first_held_reading, theater_ratio, 1970, 0.17).
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__first_held_reading, theater_ratio, 1980, 0.18).

% Extraction over time
narrative_ontology:measurement(elec_be_t1920, electronic_money_emergence__first_held_reading, base_extractiveness, 1920, 0.08).
narrative_ontology:measurement(elec_be_t1935, electronic_money_emergence__first_held_reading, base_extractiveness, 1935, 0.12).
narrative_ontology:measurement(elec_be_t1950, electronic_money_emergence__first_held_reading, base_extractiveness, 1950, 0.18).
narrative_ontology:measurement(elec_be_t1960, electronic_money_emergence__first_held_reading, base_extractiveness, 1960, 0.24).
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__first_held_reading, base_extractiveness, 1970, 0.29).
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__first_held_reading, base_extractiveness, 1980, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1920, electronic_money_emergence__first_held_reading, suppression_requirement, 1920, 0.03).
narrative_ontology:measurement(elec_su_t1935, electronic_money_emergence__first_held_reading, suppression_requirement, 1935, 0.06).
narrative_ontology:measurement(elec_su_t1950, electronic_money_emergence__first_held_reading, suppression_requirement, 1950, 0.09).
narrative_ontology:measurement(elec_su_t1960, electronic_money_emergence__first_held_reading, suppression_requirement, 1960, 0.11).
narrative_ontology:measurement(elec_su_t1970, electronic_money_emergence__first_held_reading, suppression_requirement, 1970, 0.12).
narrative_ontology:measurement(elec_su_t1980, electronic_money_emergence__first_held_reading, suppression_requirement, 1980, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__first_held_reading, 0.18).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% The electronic money emergence kernel admits three distinct readings, each identifying emergence at a different moment and deriving different structural beneficiaries and extraction profiles. The three stories form a constraint family: (1) first_held_reading (this story) marks emergence at institutional legal recognition; (2) became_thinkable_reading marks emergence at conceptual possibility, prior to institutional measurement; (3) m4_m5_collapse_reading marks emergence at statistical classification (M4/M5 distinctions), treating it as a measurement artifact. All three are readings of the same kernel. The readings coexist — different parties hold different readings simultaneously, and no single framework forecloses another. The three stories must link via network.affects_constraints to document the constraint family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
