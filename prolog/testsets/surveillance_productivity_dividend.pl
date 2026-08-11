% ============================================================================
% CONSTRAINT STORY: surveillance_productivity_dividend
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_surveillance_productivity_dividend, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: surveillance_productivity_dividend
 *   human_readable: Surveillance Productivity Dividend (Personnel-per-Capita Collapse)
 *   domain: political_economy/surveillance_studies/democratic_theory
 *
 * SUMMARY:
 *   This story authors the repressive_stabilization_reading of the
 *   stability_legitimacy_kernel: political order can be purchased through
 *   guard labor (surveillance, policing, incarceration) as a substitute for
 *   redistribution, and the marginal cost of that purchase has fallen roughly
 *   three orders of magnitude from the Stasi's personnel-per-surveilled ratio
 *   (1:80) to contemporary digital-era ratios (1:1000+). This is deliberately
 *   independent of political valence — the mechanism operates identically
 *   whether wielded by an authoritarian security service or a democratic
 *   state's fusion-center contractor. The falling personnel ratio is read
 *   here as a productivity dividend captured by whoever controls the
 *   surveillance infrastructure, letting r > g-style extraction (material or
 *   political) persist because the cost of suppressing resulting discontent
 *   falls faster than the discontent itself rises. Sibling readings of the
 *   same kernel (redistributive_stabilization, collapse_inevitability,
 *   democratic_legitimacy) are NOT folded in here — each would author a
 *   different ε and a different victim/beneficiary structure and belongs in
 *   its own file.
 *
 * KEY AGENTS:
 *   - surveillance_infrastructure_operators_and_patrons: institutional beneficiary/agenda_setter — captures the productivity dividend directly
 *   - surveilled_populations_and_dissidents: powerless/trapped victim — bears the extraction as foreclosed dissent and selective enforcement
 *   - rank_and_file_analysts_and_operators: moderate power, dual-positioned — labor that operationalizes the ratio collapse
 *   - general_citizenry_unremarkable_to_the_apparatus: organized, diffuse beneficiary/payer — incidental stability benefits, diffuse erosion costs
 *   - legislatures_and_oversight_bodies: institutional but structurally outpaced — excluded in practice despite formal authority
 *   - comparative_surveillance_historians: analytical observer — establishes the cross-regime invariance of the productivity collapse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(surveillance_productivity_dividend, 0.72).
domain_priors:suppression_score(surveillance_productivity_dividend, 0.68).
domain_priors:theater_ratio(surveillance_productivity_dividend, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(surveillance_productivity_dividend, extractiveness, 0.72).
narrative_ontology:constraint_metric(surveillance_productivity_dividend, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(surveillance_productivity_dividend, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(surveillance_productivity_dividend, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(surveillance_productivity_dividend, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(surveillance_productivity_dividend, tangled_rope).
narrative_ontology:human_readable(surveillance_productivity_dividend, "Surveillance Productivity Dividend (Personnel-per-Capita Collapse)").
narrative_ontology:topic_domain(surveillance_productivity_dividend, "political_economy/surveillance_studies/democratic_theory").

domain_priors:requires_active_enforcement(surveillance_productivity_dividend).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(surveillance_productivity_dividend, '1ccc4e8c-7121-4f6f-a11b-717aa3a4b53c').
narrative_ontology:cs_kernel_codification('1ccc4e8c-7121-4f6f-a11b-717aa3a4b53c', distributed).
narrative_ontology:cs_authority_grounding('1ccc4e8c-7121-4f6f-a11b-717aa3a4b53c', extraction).
narrative_ontology:cs_interpretation_layer_present('1ccc4e8c-7121-4f6f-a11b-717aa3a4b53c').
narrative_ontology:cs_reading_relation('1ccc4e8c-7121-4f6f-a11b-717aa3a4b53c', surveillance_productivity_dividend__redistributive_stabilization_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ccc4e8c-7121-4f6f-a11b-717aa3a4b53c', surveillance_productivity_dividend__collapse_inevitability_reading, influences).
narrative_ontology:cs_reading_relation('1ccc4e8c-7121-4f6f-a11b-717aa3a4b53c', surveillance_productivity_dividend__democratic_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('1ccc4e8c-7121-4f6f-a11b-717aa3a4b53c', foundational, guard_labor_substitutes_for_redistribution).
narrative_ontology:cs_axiom_status(guard_labor_substitutes_for_redistribution, holdable).
narrative_ontology:cs_axiom_grounding('1ccc4e8c-7121-4f6f-a11b-717aa3a4b53c', guard_labor_substitutes_for_redistribution, empirically_contingent).
narrative_ontology:cs_axiom('1ccc4e8c-7121-4f6f-a11b-717aa3a4b53c', secondary, falling_enforcement_cost_sustains_widening_inequality).
narrative_ontology:cs_axiom_status(falling_enforcement_cost_sustains_widening_inequality, holdable).
narrative_ontology:cs_axiom_grounding('1ccc4e8c-7121-4f6f-a11b-717aa3a4b53c', falling_enforcement_cost_sustains_widening_inequality, empirically_contingent).
narrative_ontology:cs_reference_frame('1ccc4e8c-7121-4f6f-a11b-717aa3a4b53c', cold_war_manual_surveillance_baseline).
narrative_ontology:cs_drift_state('1ccc4e8c-7121-4f6f-a11b-717aa3a4b53c', digital_fusion_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('1ccc4e8c-7121-4f6f-a11b-717aa3a4b53c', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(surveillance_productivity_dividend, surveillance_infrastructure_operators_and_patrons).
narrative_ontology:constraint_victim(surveillance_productivity_dividend, surveilled_populations_and_dissidents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(surveillance_productivity_dividend, rank_and_file_analysts_and_operators).
narrative_ontology:constraint_beneficiary(surveillance_productivity_dividend, general_citizenry_unremarkable_to_the_apparatus).
narrative_ontology:constraint_victim(surveillance_productivity_dividend, rank_and_file_analysts_and_operators).
narrative_ontology:constraint_victim(surveillance_productivity_dividend, general_citizenry_unremarkable_to_the_apparatus).
narrative_ontology:constraint_vindicates(surveillance_productivity_dividend, guard_labor_substitutes_for_redistribution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns or contracts the data-fusion platforms (state security services, private contractors like Palantir, allied financiers) that let a shrinking headcount monitor an exponentially larger population. Sets procurement priorities, classification rules, and the boundary of what counts as actionable intelligence. Collects contract revenue, political protection, and continuity of rule directly from the falling marginal cost of coverage.
narrative_ontology:constraint_stakeholder(surveillance_productivity_dividend, surveillance_infrastructure_operators_and_patrons, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(surveillance_productivity_dividend, surveillance_infrastructure_operators_and_patrons, beneficiary).

% Live inside the coverage envelope without having agreed to it and largely without knowing its true resolution. Bear the cost in foreclosed dissent, chilled speech, and selective prosecution (e.g., arrest for symbolic gestures). Exit means either invisibility (increasingly technically impossible) or accepting the risk of being flagged; there is no negotiated alternative on offer.
narrative_ontology:constraint_stakeholder(surveillance_productivity_dividend, surveilled_populations_and_dissidents, payer,
    powerless, biographical, trapped, national).

% Staff the reduced-headcount apparatus, drawing salaries and professional status from a function whose scale has outrun any single team's ability to meaningfully review. They benefit from employment but personally bear little of the extraction; their labor is what makes the collapsed ratio operationally real, and their professional identity increasingly depends on treating that ratio as a technical achievement rather than a political choice.
narrative_ontology:constraint_stakeholder(surveillance_productivity_dividend, rank_and_file_analysts_and_operators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(surveillance_productivity_dividend, rank_and_file_analysts_and_operators, payer).

% Not individually targeted, and may benefit from the stability, crime reduction, or fraud detection the same infrastructure produces as a byproduct. But they pay diffusely through normalized data collection, expanded legal categories of suspicion, and the erosion of the baseline within which dissent is thinkable — costs that are real but distributed too thinly for any one person to resist.
narrative_ontology:constraint_stakeholder(surveillance_productivity_dividend, general_citizenry_unremarkable_to_the_apparatus, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(surveillance_productivity_dividend, general_citizenry_unremarkable_to_the_apparatus, payer).

% Nominally charged with authorizing budgets and reviewing scope, but classification regimes, technical opacity, and the sheer velocity of contract renewal outpace their capacity to meaningfully audit personnel-to-coverage ratios. Their voice exists on paper; in practice the operators set the agenda faster than oversight can respond.
narrative_ontology:constraint_stakeholder(surveillance_productivity_dividend, legislatures_and_oversight_bodies, excluded,
    institutional, generational, constrained, national).

% Study personnel-per-surveilled-capita ratios across regimes (Stasi 1:80, digital-era agencies 1:1000+) to isolate the productivity variable from the ideological one. Their finding — that the ratio collapses regardless of regime type — is what makes visible that the dynamic is technological and economic, not merely a feature of any one government's character.
narrative_ontology:constraint_stakeholder(surveillance_productivity_dividend, comparative_surveillance_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(surveillance_productivity_dividend, surveillance_infrastructure_operators_and_patrons).
narrative_ontology:fixing_cost_class(surveillance_productivity_dividend, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregating and fusing distributed signals (financial, communications, movement, biometric) solves a genuine detection problem: catching fraud, terrorism, and organized crime that a purely manual, low-ratio surveillance apparatus could never find at scale.
% TRANSFER_FUNCTION: Moves the cost of political stability from redistribution (material concession to the governed) to coercive capacity (technical monitoring and selective enforcement), transferring risk and exposure from the operators of the apparatus onto the surveilled, whose behavioral space narrows as detection resolution rises.
% ABSENT_VOICES: Surveilled dissidents and marginal populations have no seat in procurement or oversight decisions; oversight bodies nominally represent them but are structurally out-paced. The historical comparison class (populations under prior, less efficient regimes) is invoked analytically but was never consulted about whether the productivity gain should have been pursued at all.
% DISAPPEARANCE_RATIONALE: If the productivity dividend vanished — i.e., if surveillance reverted to Stasi-era personnel ratios — the same coverage would require orders of magnitude more staff, making current levels of monitoring economically and politically unsustainable; dissent, negotiation, and redistribution would have to resume as cheaper stability mechanisms, materially changing which populations are watched and how much freedom of action they retain.
% FOUNDING_PROBLEM: Governments and their patrons needed to detect threats to order (crime, fraud, insurrection, foreign interference) without the size of a surveillance workforce constraining what could be watched.
% FOUNDING_PROBLEM_CORROBORATION: Independent security-studies scholars and comparative historians corroborate that detection needs are real and persistent across regime types; however, civil liberties organizations, dissident testimony, and oversight-body reports from outside the operator/patron class attest that the scale of monitoring now vastly exceeds what addressing the original threat-detection problem requires — the founding problem persists, but the apparatus built to solve it has outgrown it.
narrative_ontology:disappearance_verdict(surveillance_productivity_dividend, world_rearranges).
narrative_ontology:founding_problem_status(surveillance_productivity_dividend, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(surveillance_productivity_dividend, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-11',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(surveillance_productivity_dividend, 'none', 1).
narrative_ontology:epsilon_provenance(surveillance_productivity_dividend, 0.72, 'claude-sonnet-5', 'surveillance_guillotines_2026_20260811_115130', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(surveillance_productivity_dividend_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(surveillance_productivity_dividend, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(surveillance_productivity_dividend_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 to 0.72 over the interval as the personnel-per-capita ratio collapses — each unit of coverage costs less in labor, so the same fixed elite/patron interest can monitor exponentially more people without proportional investment, which is the extraction mechanism itself (falling marginal cost of surveillance funds rising surveillance scope rather than being passed back as a dividend to the surveilled). Suppression_requirement actually falls over time (0.75 to 0.50) — this is the central empirical finding: as data-fusion productivity rises, the RAW coercive force needed per unit of population monitored declines, because algorithmic sorting substitutes for human coercive presence. Theater_ratio falls modestly (0.30 to 0.22) because the function is substantially real (detection does occur) even as its scope outgrows its founding justification. This produces a genuinely tangled structure: real coordination function (crime/terror detection) plus asymmetric extraction (the ratio collapse is captured entirely by operators/patrons, not shared with the surveilled) plus active enforcement (classification, legal exclusivity of surveillance authority, selective prosecution).
 *
 * PERSPECTIVAL GAP:
 *   From the operator/patron seat this looks like an efficiency gain — doing more with less, professionalizing what used to be crude coercion. From the surveilled seat the same falling personnel ratio is experienced as an ever-shrinking space in which to act unobserved; the coordination story (catching real threats) is the same set of facts the target reads as expanding, unaccountable reach. Rank-and-file analysts sit in between, benefiting from employment while their labor is the mechanism of extraction against people they may never personally target.
 *
 * DIRECTIONALITY LOGIC:
 *   surveillance_infrastructure_operators_and_patrons get low d (near-full beneficiary) — arbitrage-grade exit (they can redeploy contracts, relocate patronage) and direct capture of the productivity gain. surveilled_populations_and_dissidents get high d (near-full target) — trapped exit, no negotiated alternative, and the entire mechanism is aimed at them. rank_and_file_analysts and general_citizenry sit closer to symmetric: real incidental benefit (employment, safety, fraud detection) but also diffuse cost-bearing as the baseline of acceptable dissent narrows around them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (detecting genuine threats) remains live — this prevents the story from being over-read as pure Snare, since a real coordination function persists and is corroborated by security-studies scholarship independent of the operators. But the scale mismatch between the founding problem and the current apparatus (corroborated by civil liberties reporting and oversight bodies from outside the beneficiary class) is exactly the tangled_rope signature: coordination function real, but extraction has decoupled from it and grown at the rate the productivity dividend allows rather than the rate the threat itself grows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productivity_dividend_capture_allocation,
    'Is the falling marginal cost of surveillance being captured entirely by operators/patrons as expanded reach, or is any share returned to the surveilled as reduced overall coercive burden (per the falling suppression_requirement series)?',
    'Compare total coercive incidents (arrests, detentions, chilling effects) per capita against the personnel ratio over the same interval; if incidents-per-capita fall in proportion to the ratio, some dividend is shared; if incidents-per-capita hold steady or rise despite the falling ratio, capture is total.',
    'If capture is total, the tangled_rope reading is conservative and the constraint drifts toward snare; if some dividend is shared, the coordination function is more genuinely load-bearing than the extraction framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productivity_dividend_capture_allocation, empirical, 'Whether the surveillance productivity gain is fully captured by operators or partially passed through as reduced coercion.').

omega_variable(
    regime_type_independence,
    'Does the personnel-ratio collapse produce the same extraction dynamic regardless of whether the regime is authoritarian or democratic, or does democratic oversight (even if currently outpaced) provide a structurally different long-run trajectory?',
    'Longitudinal comparison of ratio-adjusted extraction and suppression outcomes across regime types with equivalent surveillance productivity but different oversight architectures.',
    'If regime type is irrelevant to the trajectory, this supports reading the constraint as closer to a structural/mountain-like feature of the technology-incentive landscape (the collapse_inevitability sibling reading); if oversight architecture measurably bends the curve, the democratic_legitimacy sibling reading is better supported as the corrective mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_type_independence, conceptual, 'Whether regime type meaningfully alters the surveillance productivity extraction dynamic or is epiphenomenal to it.').

omega_variable(
    kernel_framing_stability_vs_procedural_legitimacy,
    'Should this constraint be read through the coercive-capacity lens (as authored here) or through the procedural-legitimacy lens (denial of voice to the surveilled, per the democratic_legitimacy_reading sibling)?',
    'Examine whether remedies proposed by affected populations center on reducing surveillance scope (coercive-capacity framing) or on gaining removability/accountability over surveillance authorizers (procedural framing); the dominant remedy sought is evidence for which framing the affected parties themselves hold.',
    'The two framings produce different victim sets (materially surveilled populations vs. those specifically excluded from voice) and different fixes (scope reduction vs. accountability mechanisms); this story adopts the coercive-capacity framing and treats the procedural framing as the sibling story''s subject.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_stability_vs_procedural_legitimacy, conceptual, 'Alternative framing of the same kernel via procedural/democratic legitimacy rather than coercive capacity, documented per the CS-framing under-determination guidance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(surveillance_productivity_dividend, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(surv_tr_t1950, surveillance_productivity_dividend, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(surv_tr_t1965, surveillance_productivity_dividend, theater_ratio, 1965, 0.28).
narrative_ontology:measurement(surv_tr_t1980, surveillance_productivity_dividend, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(surv_tr_t1995, surveillance_productivity_dividend, theater_ratio, 1995, 0.24).
narrative_ontology:measurement(surv_tr_t2010, surveillance_productivity_dividend, theater_ratio, 2010, 0.23).
narrative_ontology:measurement(surv_tr_t2025, surveillance_productivity_dividend, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(surv_be_t1950, surveillance_productivity_dividend, base_extractiveness, 1950, 0.38).
narrative_ontology:measurement(surv_be_t1965, surveillance_productivity_dividend, base_extractiveness, 1965, 0.42).
narrative_ontology:measurement(surv_be_t1980, surveillance_productivity_dividend, base_extractiveness, 1980, 0.47).
narrative_ontology:measurement(surv_be_t1995, surveillance_productivity_dividend, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(surv_be_t2010, surveillance_productivity_dividend, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(surv_be_t2025, surveillance_productivity_dividend, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(surv_su_t1950, surveillance_productivity_dividend, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(surv_su_t1965, surveillance_productivity_dividend, suppression_requirement, 1965, 0.72).
narrative_ontology:measurement(surv_su_t1980, surveillance_productivity_dividend, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(surv_su_t1995, surveillance_productivity_dividend, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(surv_su_t2010, surveillance_productivity_dividend, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(surv_su_t2025, surveillance_productivity_dividend, suppression_requirement, 2025, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(surveillance_productivity_dividend, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(surveillance_productivity_dividend, 0.06).
narrative_ontology:affects_constraint(surveillance_productivity_dividend, guard_labor_redistribution_substitution).
narrative_ontology:affects_constraint(surveillance_productivity_dividend, data_center_siting_democratic_exclusion).

% DUAL FORMULATION NOTE:
% This constraint instantiates the repressive_stabilization_reading of the stability_legitimacy_kernel. Sibling readings — redistributive_stabilization_reading (rope; low extraction; no victim class), collapse_inevitability_reading (mountain-adjacent; no stable beneficiary; treats the productivity dividend as deferral not resolution), and democratic_legitimacy_reading (snare specifically where procedural voice is denied) — are separate constraint files sharing the same kernel_id. Link via network.affects_constraints to guard_labor_redistribution_substitution (upstream, the Piketty-style redistribution/repression substitution mechanism this constraint's extraction rests on) and data_center_siting_democratic_exclusion (downstream, a procedural-exclusion instance that the democratic_legitimacy_reading sibling would classify as a snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
