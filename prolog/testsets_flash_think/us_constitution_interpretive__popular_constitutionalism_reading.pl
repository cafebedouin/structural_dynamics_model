% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__popular_constitutionalism_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: US Constitutional Meaning: Popular Constitutionalism Reading
 *   domain: Constitutional Law / Legal Interpretation / Political Theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'popular constitutionalism'
 *   reading of the US Constitution's interpretive authority. In this reading,
 *   constitutional meaning is primarily shaped by popular political movements
 *   and democratic contestation, rather than being solely determined by
 *   judicial interpretation. It challenges the notion of judicial supremacy,
 *   positing that interpretive authority is shared among branches of
 *   government and the citizenry. The constraint operates as a Tangled Rope,
 *   coordinating political action around constitutional meaning while also
 *   extracting interpretive authority from judicial elites and potentially
 *   imposing majoritarian will on minority groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.65).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.55).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "US Constitutional Meaning: Popular Constitutionalism Reading").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "Constitutional Law / Legal Interpretation / Political Theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, 'd2efe8e8-7dfe-40eb-aa31-a1cd1eb9f370').
narrative_ontology:cs_kernel_codification('d2efe8e8-7dfe-40eb-aa31-a1cd1eb9f370', fixed_text).
narrative_ontology:cs_authority_grounding('d2efe8e8-7dfe-40eb-aa31-a1cd1eb9f370', practice).
narrative_ontology:cs_interpretation_layer_present('d2efe8e8-7dfe-40eb-aa31-a1cd1eb9f370').
narrative_ontology:cs_reading_relation('d2efe8e8-7dfe-40eb-aa31-a1cd1eb9f370', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2efe8e8-7dfe-40eb-aa31-a1cd1eb9f370', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('d2efe8e8-7dfe-40eb-aa31-a1cd1eb9f370', foundational, popular_sovereignty_is_supreme).
narrative_ontology:cs_axiom_status(popular_sovereignty_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('d2efe8e8-7dfe-40eb-aa31-a1cd1eb9f370', popular_sovereignty_is_supreme, deontological).
narrative_ontology:cs_axiom('d2efe8e8-7dfe-40eb-aa31-a1cd1eb9f370', secondary, judicial_review_is_not_final).
narrative_ontology:cs_axiom_status(judicial_review_is_not_final, holdable).
narrative_ontology:cs_axiom_grounding('d2efe8e8-7dfe-40eb-aa31-a1cd1eb9f370', judicial_review_is_not_final, conventional).
narrative_ontology:cs_reference_frame('d2efe8e8-7dfe-40eb-aa31-a1cd1eb9f370', democratic_self_governance_framework).
narrative_ontology:cs_drift_state('d2efe8e8-7dfe-40eb-aa31-a1cd1eb9f370', contemporary_era_of_polarization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d2efe8e8-7dfe-40eb-aa31-a1cd1eb9f370', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_supremacy_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, minority_groups_reliant_on_courts).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, stable_constitutional_settlement_advocates).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__popular_constitutionalism_reading, democratic_self_governance).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__popular_constitutionalism_reading, popular_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively shape constitutional meaning through protests, elections, and advocacy, benefiting from increased influence over interpretation and a reduced role for unelected judges.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements, beneficiary,
    organized, biographical, mobile, national).

% Gain greater authority to define constitutional meaning through statutes and political action, reducing judicial constraints on their agenda and reflecting the will of the electorate.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, biographical, mobile, national).

% Benefit from the delegitimization of elite judicial interpretation, finding their voices amplified in the broader political discourse and their interpretations given more weight.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants, beneficiary,
    moderate, immediate, constrained, national).

% While still interpreting the Constitution, their authority is contested and their pronouncements are subject to popular and political override or reinterpretation, reducing their finality and exclusive interpretive power.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, the_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Bear the cost of diminished judicial authority and finality, seeing their preferred mode of constitutional settlement undermined by political contestation.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_supremacy_advocates, payer,
    powerful, generational, constrained, national).

% Face increased vulnerability as counter-majoritarian judicial protections are weakened, making their rights more subject to popular will and political shifts.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, minority_groups_reliant_on_courts, payer,
    powerless, generational, trapped, national).

% Bear the cost of increased constitutional instability and ongoing political contestation over fundamental meaning, preferring clear and settled interpretations to continuous struggle.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, stable_constitutional_settlement_advocates, payer,
    powerful, civilizational, constrained, national).

% Analyze and critique the shifting locus of constitutional authority, documenting the interplay between popular movements, political branches, and the judiciary without directly participating in the contestation.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_scholars, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__popular_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the process by which constitutional meaning is debated and established through political and social action, rather than solely through judicial decree, aiming for a more democratically responsive constitutional order.
% TRANSFER_FUNCTION: Transfers interpretive authority and the power to define constitutional meaning from judicial elites to broader political and popular arenas, including legislative bodies and social movements.
% ABSENT_VOICES: Those who believe in strict judicial finality or who lack the organizational capacity to participate effectively in popular movements might be marginalized; their interpretations are often overridden by majoritarian or politically dominant views.
% DISAPPEARANCE_RATIONALE: If popular constitutionalism vanished, the US constitutional system would revert to a more purely judicially-centric model, fundamentally altering the balance of power, the process of legal change, and the perceived legitimacy of constitutional interpretation.
% FOUNDING_PROBLEM: The perceived democratic deficit and elitism of purely judicial constitutional interpretation, and the desire for constitutional meaning to reflect the ongoing will of the people and evolving societal values.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists, historians, and legal scholars (e.g., those studying democratic theory or social movements) outside the immediate beneficiaries corroborate the ongoing tension between judicial and popular authority, citing historical and contemporary examples of popular constitutional movements.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the transfer of interpretive power from one set of actors (judiciary) to another (popular movements, legislative majorities), which can still be extractive for those whose interpretations are overridden. Suppression (0.55) is present as popular movements actively suppress alternative interpretations, particularly those emphasizing judicial finality or counter-majoritarian rights. The theater ratio (0.20) is low because this reading emphasizes overt political struggle over the performative neutrality of judicial pronouncements. Accessibility collapse (0.40) is moderate, as alternatives to purely judicial interpretation are more accessible, but the political arena itself can still constrain participation. Resistance (0.70) is high, as this reading inherently represents resistance to established judicial supremacy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of popular movements and legislative majorities, this constraint functions as a legitimate coordination mechanism for democratic self-governance. However, from the perspective of judicial supremacy advocates or minority groups reliant on judicial protection, it appears as an extractive force that undermines stability and rights. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Popular movements, legislative majorities, and anti-elitist claimants are beneficiaries, as they gain interpretive power and influence. The judiciary, while still an agenda-setter, finds its authority contested. Judicial supremacy advocates, minority groups reliant on courts, and stable constitutional settlement advocates are victims, as their preferred mode of constitutional interpretation is challenged or undermined.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently resists mandatrophy by asserting that constitutional meaning must remain responsive to the living will of the people, preventing the ossification of interpretation by an elite body. The ongoing contestation is seen as a feature, not a bug, ensuring the constraint's mandate remains 'live' by continuous re-evaluation through democratic processes. The 'contested' status of the founding problem reflects this ongoing struggle over the locus of interpretive authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarianism_vs_minority_rights,
    'Does popular constitutionalism adequately protect minority rights, or does it expose them to majoritarian tyranny?',
    'Empirical analysis of historical outcomes for minority groups under periods of heightened popular constitutionalism versus judicial supremacy, assessing the incidence and severity of rights violations.',
    'If minority rights are systematically undermined, the effective extractiveness and suppression of this reading would be higher for vulnerable groups, potentially reclassifying it as a Snare for those seats. If protections are robust, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarianism_vs_minority_rights, empirical, 'The tension between democratic majoritarianism and counter-majoritarian minority protections.').

omega_variable(
    stability_vs_adaptability,
    'Does constant popular contestation lead to necessary constitutional adaptation or to dangerous instability and erosion of fundamental principles?',
    'Longitudinal historical analysis comparing periods of stable judicial interpretation with periods of popular contestation, evaluating the impact on institutional stability, rule of law, and public trust in the constitutional order.',
    'If instability is severe, the constraint''s overall coordination function is weakened, potentially shifting its classification towards a Piton (if function atrophies) or a Snare (if instability is leveraged for extraction). If adaptation is beneficial, the Rope-like qualities are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_vs_adaptability, conceptual, 'The trade-off between constitutional stability and democratic adaptability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1950, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(us_c_tr_t1964, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1964, 0.23).
narrative_ontology:measurement(us_c_tr_t1978, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1978, 0.22).
narrative_ontology:measurement(us_c_tr_t1992, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1992, 0.21).
narrative_ontology:measurement(us_c_tr_t2006, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2006, 0.2).
narrative_ontology:measurement(us_c_tr_t2020, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1950, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(us_c_be_t1964, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1964, 0.62).
narrative_ontology:measurement(us_c_be_t1978, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1978, 0.63).
narrative_ontology:measurement(us_c_be_t1992, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1992, 0.64).
narrative_ontology:measurement(us_c_be_t2006, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2006, 0.65).
narrative_ontology:measurement(us_c_be_t2020, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1950, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(us_c_su_t1964, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1964, 0.52).
narrative_ontology:measurement(us_c_su_t1978, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1978, 0.53).
narrative_ontology:measurement(us_c_su_t1992, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1992, 0.54).
narrative_ontology:measurement(us_c_su_t2006, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 2006, 0.55).
narrative_ontology:measurement(us_c_su_t2020, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__living_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'us_constitution_interpretive' kernel, each representing a distinct structural claim about constitutional authority. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
