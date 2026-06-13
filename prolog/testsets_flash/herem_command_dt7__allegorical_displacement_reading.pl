% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__allegorical_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__allegorical_displacement_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: herem_command_dt7__allegorical_displacement_reading
 *   human_readable: Herem Command as Allegorical Spiritual Warfare
 *   domain: religious_ethics/biblical_hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the allegorical displacement reading of the
 *   Herem command in Deuteronomy 7, where the 'nations' to be conquered are
 *   interpreted as typological placeholders for spiritual enemies (sin,
 *   temptation) rather than ethnic groups. The 'conquest' is thus reframed as
 *   internal moral warfare. This reading aims to resolve the ethical tension
 *   of violent biblical texts by relocating their application to the
 *   spiritual domain, thereby eliminating any literal interethnic
 *   extractiveness or violence. It is claimed as a Mountain due to its
 *   proponents viewing it as an inherent, timeless spiritual truth of the
 *   text, not a human construct.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.05).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.1).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, mountain).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem Command as Allegorical Spiritual Warfare").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "religious_ethics/biblical_hermeneutics").

domain_priors:emerges_naturally(herem_command_dt7__allegorical_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, '0ef3a029-5021-4a80-ac7e-bf1f7b44cb8b').
narrative_ontology:cs_kernel_codification('0ef3a029-5021-4a80-ac7e-bf1f7b44cb8b', fixed_text).
narrative_ontology:cs_authority_grounding('0ef3a029-5021-4a80-ac7e-bf1f7b44cb8b', lineage).
narrative_ontology:cs_interpretation_layer_present('0ef3a029-5021-4a80-ac7e-bf1f7b44cb8b').
narrative_ontology:cs_reading_relation('0ef3a029-5021-4a80-ac7e-bf1f7b44cb8b', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('0ef3a029-5021-4a80-ac7e-bf1f7b44cb8b', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_axiom('0ef3a029-5021-4a80-ac7e-bf1f7b44cb8b', foundational, scripture_primarily_spiritual_moral).
narrative_ontology:cs_axiom_status(scripture_primarily_spiritual_moral, holdable).
narrative_ontology:cs_axiom_grounding('0ef3a029-5021-4a80-ac7e-bf1f7b44cb8b', scripture_primarily_spiritual_moral, deontological).
narrative_ontology:cs_axiom('0ef3a029-5021-4a80-ac7e-bf1f7b44cb8b', foundational, divine_commands_non_violent_in_essence).
narrative_ontology:cs_axiom_status(divine_commands_non_violent_in_essence, holdable).
narrative_ontology:cs_axiom_grounding('0ef3a029-5021-4a80-ac7e-bf1f7b44cb8b', divine_commands_non_violent_in_essence, deontological).
narrative_ontology:cs_reference_frame('0ef3a029-5021-4a80-ac7e-bf1f7b44cb8b', spiritual_hermeneutic_primacy).
narrative_ontology:cs_drift_state('0ef3a029-5021-4a80-ac7e-bf1f7b44cb8b', contemporary_ethical_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0ef3a029-5021-4a80-ac7e-bf1f7b44cb8b', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, individual_believers).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, religious_community).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, spiritual_interpretation_of_scripture).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, non_violence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives moral guidance for personal spiritual struggle, reframing potentially violent texts into calls for self-discipline and resistance to sin. This interpretation aligns with a non-violent ethical framework, reinforcing their identity as a moral agent.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, individual_believers, beneficiary,
    moderate, biographical, identity_locked, local).

% Benefits from an interpretation that resolves ethical tensions within sacred texts, presenting a coherent and morally defensible theological system. It avoids accusations of promoting violence or ethnic cleansing, enhancing its legitimacy and appeal.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, religious_community, beneficiary,
    organized, generational, constrained, national).

% Observes the interpretive move, often skeptical of its historical grounding but acknowledging its ethical intent. They may question whether the allegorical reading fully addresses the original text's historical context or merely displaces the problem.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, secular_critics, observer,
    analytical, generational, analytical, global).

% As the metaphorical 'enemies' to be conquered, these abstract concepts (sin, temptation, evil) are the targets of the 'warfare.' They are not agents but represent the internal forces that believers are called to overcome.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, abstract_vices, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(herem_command_dt7__allegorical_displacement_reading, abstract_vices).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the moral and spiritual interpretation of difficult biblical texts, providing a consistent ethical framework for believers and communities by re-directing potentially problematic commands towards internal, metaphorical application.
% TRANSFER_FUNCTION: Transfers the concept of 'conquest' from literal ethnic groups to abstract spiritual enemies (sin, temptation), thereby transferring the ethical imperative from external violence to internal moral struggle and self-discipline.
% ABSENT_VOICES: Literalist interpreters who insist on the historical and ethnic specificity of the Herem command, and those who would argue for a more direct, albeit ethically challenging, engagement with the text's original meaning, are absent from this allegorical framing.
% DISAPPEARANCE_RATIONALE: If this allegorical reading disappeared, many believers and religious communities would face a significant ethical and theological crisis regarding the interpretation of violent biblical texts. The coherence of their non-violent ethical systems would be challenged, requiring a fundamental re-evaluation of their hermeneutical approach.
% FOUNDING_PROBLEM: The ethical dilemma of reconciling violent divine commands in ancient texts (like the Herem command in Deuteronomy 7) with contemporary moral sensibilities and a commitment to non-violence.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing theological debates, ethical discussions within religious communities, and academic scholarship across various denominations and secular ethics departments attest to the persistent challenge of interpreting such texts. This corroboration comes from outside the immediate beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(herem_command_dt7__allegorical_displacement_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__allegorical_displacement_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, ExtMetricName, E),
    domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(herem_command_dt7__allegorical_displacement_reading),
    narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(herem_command_dt7__allegorical_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because this reading explicitly removes any material or interethnic extraction, reframing it as metaphorical self-discipline. Suppression is also low (0.1) as it primarily involves an interpretive framework rather than active coercion against external groups. Theater ratio is low (0.05) because the spiritual interpretation is genuinely functional for its adherents, providing a coherent ethical path. Accessibility collapse is high (0.9) as, within this interpretive framework, alternative literal readings are largely foreclosed. Resistance is low (0.05) from within the interpretive community, though it faces external resistance from other hermeneutical approaches.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individual believers and the religious community, this reading is a beneficial and necessary interpretive move that resolves ethical dilemmas. From the perspective of secular critics or literalist interpreters, it might be seen as an evasion or a reinterpretation that sacrifices historical accuracy for ethical comfort. However, within this specific reading, the structural position is one of low extraction and high coordination for spiritual guidance.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual believers and the religious community are clear beneficiaries (d near 0.0) as this reading provides a morally coherent framework for their faith and practice. The 'abstract vices' are the metaphorical 'victims' or targets of this spiritual warfare (d near 1.0), but as non-agents, they do not experience extraction in a material sense. Secular critics are observers (d near 0.5) as they analyze the interpretive move without being directly subject to its spiritual demands.
 *
 * MANDATROPHY ANALYSIS:
 *   This allegorical reading prevents mandatrophy by re-purposing the 'mandate' of the Herem command from a literal, ethically problematic directive to a timeless spiritual principle. The original 'function' (literal conquest) is deemed obsolete or misread, and a new, enduring function (spiritual self-mastery) is established. This re-interpretation ensures the constraint remains 'live' by transforming its object, rather than allowing its original, literal mandate to atrophy and become a mere performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_vs_allegorical_primacy,
    'Is the allegorical reading a primary, intended meaning of the original text, or a later interpretive move to resolve ethical tensions?',
    'Historical-critical analysis of ancient interpretive traditions and the linguistic/cultural context of Deuteronomy 7. If early interpretive traditions consistently show allegorical readings, it supports primacy; if they emerge later, it suggests a secondary ethical re-interpretation.',
    'If primary, the Mountain classification is strengthened, as the spiritual nature is inherent. If secondary, it suggests a constructed interpretive layer, potentially shifting the classification towards a Rope or Tangled Rope that coordinates ethical dilemmas rather than reflecting a natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_vs_allegorical_primacy, empirical, 'Whether the allegorical interpretation is inherent to the text or a later ethical construction.').

omega_variable(
    victim_set_displacement_validity,
    'Does displacing the victim set from ethnic groups to abstract vices fully resolve the ethical problem, or does it merely avoid confronting the text''s original, literal implications?',
    'Philosophical and ethical analysis of hermeneutical responsibility: does a ''good'' interpretation require confronting the most challenging aspects of a text, or is ethical coherence the primary goal? This is a conceptual debate.',
    'If it''s deemed an avoidance, the ''zero extractiveness on interethnic relations'' claim might be seen as a form of ''theater'' or ''suppression'' of historical truth, potentially raising the effective extractiveness or theater_ratio from an external analytical perspective. If it''s a valid ethical move, the current metrics hold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_displacement_validity, conceptual, 'Ethical validity of re-interpreting victims as abstract vices.').

omega_variable(
    false_summit_allegorical_mountain,
    'Is this allegorical reading a genuine spiritual ''Mountain'' (an inherent truth of the text), or a constructed ''Rope'' or ''Tangled Rope'' that coordinates ethical dilemmas for identifiable beneficiaries?',
    'Theological and philosophical debate on the nature of scriptural authority and interpretation. If the reading is seen as a human-constructed solution to a problem, it''s a Rope; if it''s seen as revealing an eternal truth, it''s a Mountain. The presence of beneficiaries (individual believers, religious community) triggers this FSM evaluation.',
    'If reclassified as a Rope or Tangled Rope, it implies the constraint is a human-made coordination mechanism, not a natural law, and its persistence depends on its continued utility in resolving ethical tensions for its beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_allegorical_mountain, conceptual, 'Ambiguity between inherent spiritual truth and constructed ethical coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 100, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t100, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 100, 0.05).
narrative_ontology:measurement(here_tr_t500, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 500, 0.05).
narrative_ontology:measurement(here_tr_t1000, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(here_tr_t1500, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(here_tr_t2000, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(here_tr_t2024, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(here_be_t100, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 100, 0.05).
narrative_ontology:measurement(here_be_t500, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(here_be_t1000, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(here_be_t1500, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(here_be_t2000, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(here_be_t2024, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t100, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 100, 0.1).
narrative_ontology:measurement(here_su_t500, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 500, 0.1).
narrative_ontology:measurement(here_su_t1000, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(here_su_t1500, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1500, 0.1).
narrative_ontology:measurement(here_su_t2000, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(here_su_t2024, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
