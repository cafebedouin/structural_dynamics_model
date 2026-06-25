% ============================================================================
% CONSTRAINT STORY: intervention_target_selection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intervention_target_selection, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: intervention_target_selection
 *   human_readable: Moral Improvement Intervention Target Selection
 *   domain: moral_psychology/philosophy_of_action/social_psychology
 *
 * SUMMARY:
 *   The constraint governs how moral improvement efforts are conceptualized
 *   and resourced in policy contexts. It emerged from early moral
 *   psychology's attempt to distinguish dispositional from situational causes
 *   of moral action, but persists despite accumulating evidence that
 *   person-situation interaction is the proper unit of analysis. The
 *   constraint forces binary resource allocation between character education
 *   and institutional reform, when the evidence suggests both are necessary.
 *   The claim is tangled_rope: genuine coordination function (organizing
 *   research and policy around testable hypotheses) combined with asymmetric
 *   extraction (advocacy coalitions benefit from maintaining the dichotomy
 *   while evidence-based policy makers and interactionist researchers bear
 *   the costs).
 *
 * KEY AGENTS:
 *   - character_education_advocates: Primary beneficiary (organized/identity_locked) — professional identity depends on dispositional framework
 *   - institutional_reform_advocates: Primary beneficiary (organized/identity_locked) — professional identity depends on situational framework
 *   - evidence_based_policy_makers: Primary payer (institutional/constrained) — forced into suboptimal binary resource allocation
 *   - interdisciplinary_researchers: Secondary payer (moderate/constrained) — interactionist work structurally disadvantaged
 *   - intervention_recipients: Tertiary payer (powerless/trapped) — receive whichever intervention their jurisdiction committed to
 *   - experimental_psychologists: Agenda setter (institutional/mobile) — produce evidence both sides cite selectively
 *   - interactionist_psychologists: Excluded (moderate/constrained) — integrated framework not adopted despite evidence
 *   - analytical_philosophers: Observer (analytical/analytical) — see the dichotomy as false binary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intervention_target_selection, 0.68).
domain_priors:suppression_score(intervention_target_selection, 0.72).
domain_priors:theater_ratio(intervention_target_selection, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intervention_target_selection, extractiveness, 0.68).
narrative_ontology:constraint_metric(intervention_target_selection, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(intervention_target_selection, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(intervention_target_selection, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(intervention_target_selection, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intervention_target_selection, tangled_rope).
narrative_ontology:human_readable(intervention_target_selection, "Moral Improvement Intervention Target Selection").
narrative_ontology:topic_domain(intervention_target_selection, "moral_psychology/philosophy_of_action/social_psychology").

domain_priors:requires_active_enforcement(intervention_target_selection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(intervention_target_selection, '110eb093-33af-4adb-8289-9f0617a371bf').
narrative_ontology:cs_kernel_codification('110eb093-33af-4adb-8289-9f0617a371bf', distributed).
narrative_ontology:cs_authority_grounding('110eb093-33af-4adb-8289-9f0617a371bf', distributed).
narrative_ontology:cs_reading_relation('110eb093-33af-4adb-8289-9f0617a371bf', intervention_target_selection__dispositional_reading, coexists_with).
narrative_ontology:cs_reading_relation('110eb093-33af-4adb-8289-9f0617a371bf', intervention_target_selection__situational_reading, coexists_with).
narrative_ontology:cs_axiom('110eb093-33af-4adb-8289-9f0617a371bf', foundational, person_situation_interaction_primacy).
narrative_ontology:cs_axiom_status(person_situation_interaction_primacy, holdable).
narrative_ontology:cs_axiom_grounding('110eb093-33af-4adb-8289-9f0617a371bf', person_situation_interaction_primacy, empirically_contingent).
narrative_ontology:cs_axiom('110eb093-33af-4adb-8289-9f0617a371bf', secondary, integrated_intervention_necessity).
narrative_ontology:cs_axiom_status(integrated_intervention_necessity, holdable).
narrative_ontology:cs_axiom_grounding('110eb093-33af-4adb-8289-9f0617a371bf', integrated_intervention_necessity, instrumental).
narrative_ontology:cs_reference_frame('110eb093-33af-4adb-8289-9f0617a371bf', early_moral_psychology_dichotomy).
narrative_ontology:cs_drift_state('110eb093-33af-4adb-8289-9f0617a371bf', contemporary_interaction_research_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('110eb093-33af-4adb-8289-9f0617a371bf', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intervention_target_selection, character_education_advocates).
narrative_ontology:constraint_beneficiary(intervention_target_selection, institutional_reform_advocates).
narrative_ontology:constraint_victim(intervention_target_selection, evidence_based_policy_makers).
narrative_ontology:constraint_victim(intervention_target_selection, interdisciplinary_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(intervention_target_selection, virtue_ethicists).
narrative_ontology:constraint_beneficiary(intervention_target_selection, social_justice_theorists).
narrative_ontology:constraint_victim(intervention_target_selection, intervention_recipients).
narrative_ontology:constraint_vindicates(intervention_target_selection, dispositional_moral_causation).
narrative_ontology:constraint_vindicates(intervention_target_selection, situational_moral_causation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote character education programs, virtue ethics curricula, and individual moral development initiatives. Their professional identity and institutional funding depend on the dispositional framework being treated as primary. They benefit when policy resources flow to character-building interventions and when moral failures are attributed to defective character formation rather than situational factors.
narrative_ontology:constraint_stakeholder(intervention_target_selection, character_education_advocates, beneficiary,
    organized, generational, identity_locked, national).

% Promote social welfare programs, institutional redesign, and situational interventions. Their professional identity and policy influence depend on the situational framework being treated as primary. They benefit when policy resources flow to structural reforms and when moral failures are attributed to corrupting circumstances rather than individual character defects.
narrative_ontology:constraint_stakeholder(intervention_target_selection, institutional_reform_advocates, beneficiary,
    organized, generational, identity_locked, national).

% Must allocate limited policy resources between competing intervention frameworks without clear empirical guidance on which approach is more effective. The constraint forces binary choices between character education and institutional reform when the evidence suggests both matter. They bear the cost of suboptimal resource allocation and political pressure from both advocacy coalitions.
narrative_ontology:constraint_stakeholder(intervention_target_selection, evidence_based_policy_makers, payer,
    institutional, biographical, constrained, national).

% Attempt to conduct research on person-situation interaction and integrated intervention models but face funding structures, publication incentives, and disciplinary boundaries that reward taking sides in the dispositional-situational debate. Their work is evaluated by reviewers committed to one reading or the other, making nuanced interactionist findings harder to publish and fund.
narrative_ontology:constraint_stakeholder(intervention_target_selection, interdisciplinary_researchers, payer,
    moderate, biographical, constrained, global).

% Conservative ideologies are vindicated by dispositional readings that justify individual responsibility and character-focused interventions; progressive ideologies are vindicated by situational readings that justify structural reform and social welfare. The constraint's persistence allows both sides to claim empirical support for their prior commitments.
narrative_ontology:constraint_stakeholder(intervention_target_selection, political_ideologies, beneficiary,
    organized, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(intervention_target_selection, political_ideologies).

% Receive whichever intervention type their jurisdiction has committed to, regardless of whether it addresses their actual needs. A person in a corrupting situation who receives only character education, or a person with genuine character deficits who receives only situational modification, bears the cost of the constraint's forced binary choice.
narrative_ontology:constraint_stakeholder(intervention_target_selection, intervention_recipients, payer,
    powerless, biographical, trapped, local).

% Produce the empirical evidence that both sides cite selectively. They set the research agenda and determine which studies get conducted, but their findings are interpreted through competing philosophical frameworks. They maintain authority by continuing to produce studies that can be read as supporting either position, which perpetuates the debate rather than resolving it.
narrative_ontology:constraint_stakeholder(intervention_target_selection, experimental_psychologists, agenda_setter,
    institutional, generational, mobile, global).

% Provide the philosophical framework that interprets dispositional evidence as supporting character-based interventions. Their tradition's relevance depends on character traits being treated as causally primary and stable across situations. They benefit from the constraint's persistence because it maintains demand for virtue ethics expertise in policy contexts.
narrative_ontology:constraint_stakeholder(intervention_target_selection, virtue_ethicists, agenda_setter,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(intervention_target_selection, virtue_ethicists, beneficiary).

% Provide the philosophical framework that interprets situational evidence as supporting structural reform. Their theoretical relevance depends on circumstances being treated as causally primary over individual dispositions. They benefit from the constraint's persistence because it maintains demand for structural analysis in policy contexts.
narrative_ontology:constraint_stakeholder(intervention_target_selection, social_justice_theorists, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(intervention_target_selection, social_justice_theorists, beneficiary).

% Argue that person-situation interaction is the proper unit of analysis and that both character development and situational design are necessary. They are structurally excluded from the policy debate because their position does not validate either advocacy coalition's funding priorities or ideological commitments. Their research is cited selectively by both sides but their integrated framework is not adopted.
narrative_ontology:constraint_stakeholder(intervention_target_selection, interactionist_psychologists, excluded,
    moderate, biographical, constrained, global).

% Examine the conceptual structure of the debate and note that the dispositional-situational dichotomy may be a false binary that obscures the actual causal structure of moral action. They observe that both advocacy coalitions benefit from maintaining the dichotomy and that the constraint's extractiveness increases as the empirical evidence for interaction accumulates but policy remains locked in binary choice.
narrative_ontology:constraint_stakeholder(intervention_target_selection, analytical_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for allocating moral improvement resources between individual-focused and structure-focused interventions, allowing policy makers to justify resource allocation decisions with reference to empirical psychology and philosophical tradition.
% TRANSFER_FUNCTION: Moves policy resources, institutional authority, and professional legitimacy from evidence-based integrative approaches to advocacy coalitions committed to either dispositional or situational readings. Moves research funding from interactionist studies to studies that can be read as supporting one pole or the other.
% ABSENT_VOICES: Interactionist psychologists and philosophers who argue for integrated intervention models are structurally excluded from policy influence because their position does not validate either advocacy coalition's funding priorities. Intervention recipients whose needs require both character development and situational modification have no organized representation in the debate.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, policy resources would flow to integrated intervention models that combine character development with situational design based on empirical evidence of person-situation interaction. The advocacy coalitions would lose their ability to claim exclusive empirical support, research funding would shift to interactionist studies, and the left-right political divide on crime prevention and moral education would lose its apparent empirical foundation.
% FOUNDING_PROBLEM: Early moral psychology lacked empirical methods to distinguish dispositional from situational causes of moral action. The constraint emerged to organize research programs and policy debates around testable hypotheses about the locus of moral causation.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary personality psychology and interactionist research (Mischel, Fleeson, trait-situation interaction studies) establish that the dispositional-situational dichotomy is empirically false: both character and circumstances matter, and their interaction is the proper unit of analysis. This is attested by researchers outside both advocacy coalitions. The advocacy coalitions themselves continue to assert the founding problem is live, but their assertion is contradicted by the accumulated evidence.
narrative_ontology:disappearance_verdict(intervention_target_selection, world_rearranges).
narrative_ontology:founding_problem_status(intervention_target_selection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(intervention_target_selection, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(intervention_target_selection, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intervention_target_selection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(intervention_target_selection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(intervention_target_selection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68 at interval end) because the constraint diverts resources from evidence-based integrative approaches to advocacy coalitions whose positions are contradicted by contemporary research. Suppression is high (0.72) because maintaining the dichotomy requires actively excluding interactionist frameworks from policy influence and structuring funding to reward taking sides. Theater ratio is moderate-high (0.58): both advocacy coalitions perform empirical rigor by citing psychology studies, but the performance increasingly substitutes for genuine engagement with the interaction evidence. The measurements show steady increases across all three metrics as the gap between the constraint's binary structure and the empirical evidence widens over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the advocacy coalitions' seats, the constraint is genuine coordination that organizes research and policy around competing empirical hypotheses. From the evidence-based policy makers' and researchers' seats, the same structure operates as enforced extraction that prevents integration of the accumulated evidence. The analytical philosophers observe that the dichotomy itself may be the extractive mechanism: it creates a false binary that both advocacy coalitions benefit from maintaining.
 *
 * DIRECTIONALITY LOGIC:
 *   The advocacy coalitions are structural beneficiaries (collect professional legitimacy and policy influence from maintaining their respective readings; identity_locked exit because their professional identities are fused with their positions). Evidence-based policy makers and interdisciplinary researchers are targets (bear costs of suboptimal allocation and structural disadvantage; constrained exit because leaving means abandoning their professional roles). Intervention recipients are full targets (powerless/trapped, receive interventions that may not address their actual needs). The experimental psychologists who produce the evidence sit near symmetric: they maintain authority by continuing to produce studies both sides can cite, but they do not capture the extraction the way the advocacy coalitions do.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing dispositional from situational causes) is dead: contemporary research establishes that person-situation interaction is the proper unit of analysis. The constraint persists because both advocacy coalitions benefit from the dichotomy and would lose professional legitimacy and funding if integrated models were adopted. This is mandatrophy: the arrangement outlived its founding justification but persists because concentrated beneficiaries (advocacy coalitions) can maintain it while diffuse payers (policy makers, researchers, intervention recipients) cannot coordinate to remove it. The R5 mismatch (founding_problem_status=dead + disappearance_verdict=world_rearranges) flags this as a capture/zombie case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_resolution_threshold,
    'How much evidence of person-situation interaction is required before the dispositional-situational dichotomy loses legitimacy in policy contexts?',
    'Track citation patterns in policy documents and funding decisions: if interactionist research accumulates but policy remains locked in binary choice, the threshold is not empirical but political.',
    'If the threshold is empirical, accumulating evidence should shift policy toward integrated models. If the threshold is political, the constraint''s extractiveness will continue to increase as the evidence-policy gap widens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_resolution_threshold, empirical, 'Whether evidence accumulation can overcome advocacy coalition lock-in').

omega_variable(
    coordination_extraction_separability,
    'Is the constraint''s coordination function (organizing research around testable hypotheses) separable from its extraction function (maintaining advocacy coalition influence)?',
    'Natural experiment from jurisdictions that adopt integrated intervention models: if research productivity and policy effectiveness improve while advocacy coalition influence declines, the functions are separable.',
    'If separable, the extraction is pure rent-seeking riding on a former coordination function. If inseparable, some of the measured extraction is the price of maintaining organized research programs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction components are structurally separable').

omega_variable(
    ideological_validation_mechanism,
    'Does the constraint persist primarily because it validates pre-existing political ideologies (conservative dispositional, progressive situational), or because the advocacy coalitions have independent institutional power?',
    'Compare constraint strength across jurisdictions with different political cultures but similar research institutions. If strength tracks political polarization more than research funding, ideology is primary.',
    'If ideological validation is primary, the constraint is more deeply entrenched than institutional analysis suggests. If institutional power is primary, the constraint could be disrupted by funding reforms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_validation_mechanism, empirical, 'Whether political ideology or institutional power is the primary maintenance mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intervention_target_selection, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inte_tr_t0, intervention_target_selection, theater_ratio, 0, 0.25).
narrative_ontology:measurement(inte_tr_t10, intervention_target_selection, theater_ratio, 10, 0.32).
narrative_ontology:measurement(inte_tr_t20, intervention_target_selection, theater_ratio, 20, 0.39).
narrative_ontology:measurement(inte_tr_t30, intervention_target_selection, theater_ratio, 30, 0.45).
narrative_ontology:measurement(inte_tr_t40, intervention_target_selection, theater_ratio, 40, 0.51).
narrative_ontology:measurement(inte_tr_t50, intervention_target_selection, theater_ratio, 50, 0.55).
narrative_ontology:measurement(inte_tr_t60, intervention_target_selection, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(inte_be_t0, intervention_target_selection, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(inte_be_t10, intervention_target_selection, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(inte_be_t20, intervention_target_selection, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(inte_be_t30, intervention_target_selection, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(inte_be_t40, intervention_target_selection, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(inte_be_t50, intervention_target_selection, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(inte_be_t60, intervention_target_selection, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(inte_su_t0, intervention_target_selection, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(inte_su_t10, intervention_target_selection, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(inte_su_t20, intervention_target_selection, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(inte_su_t30, intervention_target_selection, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(inte_su_t40, intervention_target_selection, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(inte_su_t50, intervention_target_selection, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(inte_su_t60, intervention_target_selection, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intervention_target_selection, identity_coordination).
narrative_ontology:boltzmann_floor_override(intervention_target_selection, 0.08).
narrative_ontology:affects_constraint(intervention_target_selection, criminal_justice_intervention_design).
narrative_ontology:affects_constraint(intervention_target_selection, educational_curriculum_structure).
narrative_ontology:affects_constraint(intervention_target_selection, mental_health_treatment_models).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the moral_causation_locus kernel. The dispositional_reading and situational_reading are sibling constraints that decompose the same natural-language concept ('where moral action comes from') into structurally distinct claims with different beneficiary sets and different empirical status. All three readings should be linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
