% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty Doctrine (Capacity/Legitimacy Spectrum Reading)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   Since the early post-Cold War period, international practice has
 *   increasingly treated sovereignty as a matter of degree rather than an
 *   all-or-nothing legal status, with a state's practical sovereignty scaled
 *   to independently constructed indices of institutional capacity and
 *   governance quality. This story authors that specific reading: not the
 *   classical Westphalian doctrine of unconditional territorial sovereignty,
 *   and not the responsibility-to-protect doctrine tied to discrete
 *   human-rights triggers, but the graduated/scalar doctrine under which
 *   external actors hold ongoing discretion to reclassify a state's effective
 *   sovereignty based on capacity and legitimacy metrics they themselves
 *   construct and apply.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.62).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.58).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.62).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty Doctrine (Capacity/Legitimacy Spectrum Reading)").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, '31428a55-5ced-4b39-bad9-2ceedc4d8ade').
narrative_ontology:cs_kernel_codification('31428a55-5ced-4b39-bad9-2ceedc4d8ade', distributed).
narrative_ontology:cs_authority_grounding('31428a55-5ced-4b39-bad9-2ceedc4d8ade', distributed).
narrative_ontology:cs_reading_relation('31428a55-5ced-4b39-bad9-2ceedc4d8ade', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('31428a55-5ced-4b39-bad9-2ceedc4d8ade', westphalian_sovereignty__conditional_sovereignty, influences).
narrative_ontology:cs_axiom('31428a55-5ced-4b39-bad9-2ceedc4d8ade', foundational, sovereignty_is_scalar_not_binary).
narrative_ontology:cs_axiom_status(sovereignty_is_scalar_not_binary, holdable).
narrative_ontology:cs_axiom_grounding('31428a55-5ced-4b39-bad9-2ceedc4d8ade', sovereignty_is_scalar_not_binary, conventional).
narrative_ontology:cs_axiom('31428a55-5ced-4b39-bad9-2ceedc4d8ade', foundational, external_capacity_assessment_confers_intervention_standing).
narrative_ontology:cs_axiom_status(external_capacity_assessment_confers_intervention_standing, holdable).
narrative_ontology:cs_axiom_grounding('31428a55-5ced-4b39-bad9-2ceedc4d8ade', external_capacity_assessment_confers_intervention_standing, instrumental).
narrative_ontology:cs_reference_frame('31428a55-5ced-4b39-bad9-2ceedc4d8ade', unconditional_territorial_sovereignty).
narrative_ontology:cs_drift_state('31428a55-5ced-4b39-bad9-2ceedc4d8ade', post_cold_war_fragile_state_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('31428a55-5ced-4b39-bad9-2ceedc4d8ade', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, dominant_intervening_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, multilateral_lending_institutions).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_ratings_bodies).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, low_capacity_postcolonial_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, fragile_state_populations).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, unrecognized_or_contested_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, high_capacity_established_states).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, state_capacity_as_legitimacy_criterion).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, differentiated_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and apply the capacity/legitimacy thresholds that determine where a given state sits on the sovereignty spectrum, largely through their control of UN Security Council seats, bilateral aid conditionality, and recognition practice. They decide, case by case, which states are treated as fully sovereign and which are treated as candidates for trusteeship-style oversight, transitional administration, or conditional recognition. Their own governance is never subjected to the same graduated test.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, dominant_intervening_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Use governance-capacity scoring (rule of law indices, institutional quality metrics) as a gate for loan conditionality and debt restructuring terms. A state scored low on the spectrum receives harsher conditionality, deeper sovereignty-limiting oversight of fiscal policy, and reduced bargaining leverage — all justified by the same graduated framework.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, multilateral_lending_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Produce the capacity and governance-legitimacy indices that operationalize the spectrum — corruption perception scores, fragile states indices, rule-of-law rankings. Their methodology choices directly determine which states fall into the lower bands, and they profit institutionally (funding, influence, citation authority) from being the recognized arbiters of the scale.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_ratings_bodies, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, international_ratings_bodies, agenda_setter).

% Scored low on the capacity/legitimacy spectrum, often due to institutional weaknesses traceable to colonial-era extraction and post-independence structural adjustment. They face graduated intrusions on domestic policymaking — conditional aid, externally supervised elections, internationally administered transitional authorities — framed as calibrated response to their position on the spectrum rather than as intervention requiring separate justification. Exit from the scoring regime is not realistically available while dependent on external finance or recognition.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, low_capacity_postcolonial_states, payer,
    moderate, generational, trapped, national).

% Bear the direct costs of low sovereignty-scores: austerity imposed through IMF programs justified by low governance rankings, reduced public service capacity from conditionality, and instability from externally managed political transitions. They have no voice in how the capacity/legitimacy index is constructed or applied and cannot exit their state's position on the spectrum.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, fragile_state_populations, payer,
    powerless, biographical, trapped, local).

% Governments that emerge through coups, revolutions, or contested elections are placed at the bottom of the graduated scale almost automatically, regardless of actual domestic legitimacy or capacity, triggering sanctions, recognition withholding, and external administration proposals. The graduated framework gives this treatment a technical, scalar veneer rather than treating it as a political judgment.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, unrecognized_or_contested_governments, payer,
    powerless, biographical, trapped, regional).

% Sit permanently at the high end of the spectrum by virtue of established institutions, treated as the unquestioned reference class against which others are measured. They never face equivalent scrutiny of their own governance quality despite comparable or worse failures (financial crises, democratic backsliding) because the scale is applied asymmetrically to weaker states.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, high_capacity_established_states, beneficiary,
    institutional, civilizational, arbitrage, global).

% Domestic civil society groups, traditional governance structures, and local political movements within low-scoring states have no seat in the international bodies that construct or apply the capacity/legitimacy index, even though they are the ones whose lived governance experience the index purports to measure.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, excluded_local_civil_society, excluded,
    powerless, biographical, trapped, local).

% Study and debate the graduated sovereignty doctrine's coherence and consequences, documenting how capacity-based classification has historically tracked colonial and racial hierarchies and how the discretion it grants concentrates in the hands of the same states that already hold structural power.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__graduated_sovereignty, diffuse).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__graduated_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally solves a real problem: sovereignty as an all-or-nothing binary poorly fits states that genuinely cannot perform core governance functions (monopoly on force, basic service delivery, functioning courts), and some differentiated response to state fragility could in principle be coordinated rather than ad hoc.
% TRANSFER_FUNCTION: Moves discretionary authority over political and economic policy from lower-scoring states to the external actors empowered to assess and act on their position on the spectrum — aid conditionality, transitional administration mandates, and debt-restructuring leverage flow from high-scoring states and institutions toward oversight of low-scoring ones, while accountability does not flow back.
% ABSENT_VOICES: Local civil society, traditional authorities, and ordinary populations within the states being scored have no role in constructing the capacity/legitimacy metrics applied to them; their own account of what legitimate governance looks like in their context is displaced by externally designed indices.
% DISAPPEARANCE_RATIONALE: If the graduated sovereignty doctrine disappeared overnight, aid conditionality, debt restructuring terms, transitional administration mandates, and selective recognition practices tied to governance scoring would lose their doctrinal justification; lower-capacity states would either revert to a strict equal-sovereignty norm (absolute reading) or face intervention only under the narrower human-rights trigger (conditional reading) — either way, the discretionary reclassification apparatus built around the spectrum concept would need to be dismantled or re-grounded.
% FOUNDING_PROBLEM: Post-Cold War state collapse (Somalia, Yugoslavia's dissolution, later South Sudan and others) exposed that treating all recognized states as equally capable sovereign actors produced governance vacuums that classical Westphalian doctrine had no vocabulary for addressing.
% FOUNDING_PROBLEM_CORROBORATION: Dominant intervening states and multilateral lenders attest the founding problem remains live, citing ongoing state fragility indices. Independent international legal scholars and historians of the mandate/trusteeship system attest that the graduated framework reproduces colonial-era hierarchies of tutelage largely regardless of actual state performance, and that the scoring apparatus itself, not fragility, now drives much of the sovereignty-limiting intervention.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 and rising over the interval because the scoring apparatus has become progressively more embedded in aid conditionality, debt restructuring, and recognition practice — what began as a diagnostic framework for genuinely collapsed states (Somalia-era) has hardened into a standing discretionary lever applied to a widening set of states. Suppression (0.58) reflects that lower-scored states have essentially no venue to contest their classification; the indices are constructed by the same institutions that act on them. Theater ratio (0.4) is moderate-high: real governance failures do exist in some scored states, but a substantial and growing share of scoring activity performs technical neutrality over what is a political judgment about which states get full sovereign treatment.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (dominant states, lending institutions) the graduated framework reads as a coordination improvement over the binary Westphalian model: a more nuanced, evidence-based response to real governance variation. From the payer seat (low-capacity states, their populations) the same structure reads as a discretionary reclassification mechanism that reproduces colonial hierarchies of tutelage under a technical vocabulary. The engine computes both seats from the same structural facts; the divergence is the analytical payload, not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant intervening states, lending institutions, and ratings bodies sit near the beneficiary end: they set the metrics, apply the consequences, and are never themselves subject to the scale. Low-capacity postcolonial states and fragile-state populations sit near the full-target end: trapped exit, no input into the metric's construction, and the costs of reclassification (conditionality, oversight, instability) land on them directly. High-capacity established states benefit doubly — permanently exempt from the scale's downside while endorsing its legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine post-Cold War state collapse exposing gaps in classical sovereignty doctrine — was real and, for some cases, remains live. But the mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) flags a capture pattern: the scoring and reclassification apparatus has outgrown the narrow collapsed-state cases it was built for and now applies routinely to a much broader set of states whose governance imperfections resemble those of the very states that constructed the scale. Classifying this reading as snare rather than tangled_rope reflects that the coordination story (helping genuinely fragile states) is now substantially cover for a standing extraction mechanism (discretionary external control over policy in scored states) rather than a genuine, bounded response to collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    graduated_vs_conditional_reading_boundary,
    'Is the graduated sovereignty doctrine structurally distinct from the conditional sovereignty doctrine, or is it simply the conditional doctrine applied continuously rather than episodically?',
    'Compare intervention triggers across cases: conditional-sovereignty interventions cite a specific triggering violation (mass atrocity, genocide); graduated-sovereignty interventions cite an ongoing capacity/legitimacy score with no discrete triggering event. If the two produce different case sets and different justificatory structures in state practice, they remain distinct constraints.',
    'If the readings collapse into one, the two constraint stories should be merged or the sibling relation reclassified from coexists_with toward something closer to a subsumption; if they remain distinct, the graduated reading''s higher continuous-discretion extraction stands as a separately authored fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduated_vs_conditional_reading_boundary, conceptual, 'Whether graduated and conditional sovereignty are genuinely separate readings or one is a special case of the other.').

omega_variable(
    index_construction_neutrality,
    'Are the capacity/governance-legitimacy indices that operationalize the spectrum methodologically neutral measures of state function, or do they encode the political and economic interests of the states and institutions that construct them?',
    'Independent methodological audit of major fragility/governance indices (source selection, weighting choices, historical correlation with former colonial status and current geopolitical alignment) conducted by parties outside the index-producing institutions.',
    'If indices are substantially neutral, extraction is lower than authored and part of this story''s classification should move toward tangled_rope (genuine capacity assessment with some extractive overhead); if indices substantially encode producer interests, the snare classification and high ε are reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(index_construction_neutrality, empirical, 'Whether the scoring metrics underlying graduated sovereignty are methodologically neutral or politically encoded.').

omega_variable(
    colonial_continuity_framing,
    'Is the graduated sovereignty doctrine a genuinely novel post-Cold War response to state collapse, or a continuation of the mandate/trusteeship system''s differentiated-sovereignty logic under new technical vocabulary?',
    'Historical-legal comparison of League of Nations mandate criteria and current capacity/legitimacy indices for structural similarity in how ''readiness for full sovereignty'' is assessed and by whom.',
    'If continuous with the mandate system, this strengthens the snare classification and the neo-colonial extraction reading; if genuinely novel and disconnected from that lineage, the extractive framing is weaker and closer to good-faith institutional adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_continuity_framing, conceptual, 'Whether graduated sovereignty is continuous with historical mandate/trusteeship doctrine or a distinct post-Cold War innovation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(west_tr_t1997, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1997, 0.25).
narrative_ontology:measurement(west_tr_t2004, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2004, 0.3).
narrative_ontology:measurement(west_tr_t2011, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2011, 0.34).
narrative_ontology:measurement(west_tr_t2018, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2018, 0.38).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(west_be_t1997, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1997, 0.42).
narrative_ontology:measurement(west_be_t2004, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2004, 0.5).
narrative_ontology:measurement(west_be_t2011, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2011, 0.56).
narrative_ontology:measurement(west_be_t2018, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(west_su_t1997, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1997, 0.38).
narrative_ontology:measurement(west_su_t2004, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2004, 0.45).
narrative_ontology:measurement(west_su_t2011, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2011, 0.5).
narrative_ontology:measurement(west_su_t2018, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__graduated_sovereignty, 0.12).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__conditional_sovereignty).

% DUAL FORMULATION NOTE:
% Part of a three-reading decomposition of the westphalian_sovereignty kernel. absolute_sovereignty treats sovereignty as unconditional and categorical (ε near zero from its own framework, since no external intervention is legitimate regardless of state conduct). conditional_sovereignty ties legitimate intervention to a specific, bounded trigger (systematic human rights violations), producing moderate ε concentrated in acute crisis cases. This graduated reading authors the highest ε of the three (0.50-0.70 band) because it grants standing, continuous, non-triggered discretion to external assessors rather than bounding intervention to a discrete event — the discretion itself, applied routinely rather than exceptionally, is the extractive mechanism. All three are separate constraints sharing one contested kernel; each carries its own ε and stakeholder structure and none is the 'correct' reading of the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__graduated_sovereignty, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
