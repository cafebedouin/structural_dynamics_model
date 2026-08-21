% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__bodily_autonomy_primary, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary Right (Mandate Legitimacy Scope Reading)
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'bodily_autonomy_primary' reading
 *   of the 'mandate_legitimacy_scope' kernel. It asserts that any medical
 *   intervention without informed consent constitutes a violation of
 *   fundamental bodily integrity, irrespective of any claimed collective
 *   benefit. From this perspective, state-imposed medical mandates are
 *   inherently extractive and suppressive, turning the state into a rights
 *   violator and individuals into victims. The high extractiveness and
 *   suppression metrics reflect this reading's interpretation of the impact
 *   of such mandates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.9).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.95).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.9).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Bodily Autonomy as Primary Right (Mandate Legitimacy Scope Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, 'f2b95d3f-30d9-4283-8580-7666b8d488cc').
narrative_ontology:cs_kernel_codification('f2b95d3f-30d9-4283-8580-7666b8d488cc', formalized).
narrative_ontology:cs_authority_grounding('f2b95d3f-30d9-4283-8580-7666b8d488cc', lineage).
narrative_ontology:cs_interpretation_layer_present('f2b95d3f-30d9-4283-8580-7666b8d488cc').
narrative_ontology:cs_reading_relation('f2b95d3f-30d9-4283-8580-7666b8d488cc', mandate_legitimacy_scope__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('f2b95d3f-30d9-4283-8580-7666b8d488cc', mandate_legitimacy_scope__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('f2b95d3f-30d9-4283-8580-7666b8d488cc', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('f2b95d3f-30d9-4283-8580-7666b8d488cc', bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('f2b95d3f-30d9-4283-8580-7666b8d488cc', foundational, informed_consent_unwaivable).
narrative_ontology:cs_axiom_status(informed_consent_unwaivable, holdable).
narrative_ontology:cs_axiom_grounding('f2b95d3f-30d9-4283-8580-7666b8d488cc', informed_consent_unwaivable, deontological).
narrative_ontology:cs_reference_frame('f2b95d3f-30d9-4283-8580-7666b8d488cc', unwaivable_individual_sovereignty).
narrative_ontology:cs_drift_state('f2b95d3f-30d9-4283-8580-7666b8d488cc', covid_19_mandate_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('f2b95d3f-30d9-4283-8580-7666b8d488cc', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_authorities).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, individuals_seeking_medical_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who face severe social, economic, or legal penalties (e.g., job loss, exclusion from public spaces) if they do not comply with medical mandates, despite their lack of informed consent. From this reading, they are direct victims of rights violation.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, immediate, trapped, national).

% Government bodies responsible for public health policy, including the implementation and enforcement of medical mandates. From this reading's perspective, they are the agents of rights violation, benefiting from the perceived collective good at the expense of individual autonomy.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Citizens and advocacy groups who actively resist medical mandates, citing fundamental rights to bodily integrity and informed consent. They bear the costs of resistance and non-compliance.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, individuals_seeking_medical_autonomy, payer,
    moderate, biographical, constrained, local).

% Legal experts and ethicists who interpret constitutional and human rights frameworks to prioritize individual bodily autonomy, often challenging state authority in medical matters. They analyze the constraint's operation from a rights-based perspective.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, constitutional_law_scholars_autonomy_advocates, observer,
    analytical, generational, analytical, national).

% Groups and professionals who prioritize collective health outcomes and advocate for state interventions to protect vulnerable populations. From the 'bodily_autonomy_primary' reading, their arguments for collective benefit are structurally excluded from the foundational premise.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, any perceived 'coordination' function of mandates (e.g., disease control) is secondary to, and cannot justify, the violation of fundamental bodily integrity. The constraint primarily functions as a mechanism of state coercion.
% TRANSFER_FUNCTION: Transfers individual control over one's body and medical decisions to the state, in exchange for a perceived (but, from this reading, illegitimate) collective health benefit. The cost is borne by individuals through loss of autonomy and potential physical intervention.
% ABSENT_VOICES: The voices of those who prioritize collective health or a utilitarian calculus that would permit medical mandates are structurally excluded from the foundational premise of this reading, which asserts the primacy of individual rights regardless of collective benefit.
% DISAPPEARANCE_RATIONALE: If the principle that 'Medical intervention without informed consent violates fundamental bodily integrity regardless of collective benefit' vanished, the legal and ethical landscape around public health would fundamentally shift. States would have significantly expanded powers to compel medical procedures, reorganizing the relationship between individual rights and state authority in a way that would be unrecognizable to current human rights frameworks.
% FOUNDING_PROBLEM: To establish and protect the individual's right to self-determination over their body, preventing state or medical authorities from imposing interventions without free and informed consent, thereby safeguarding against medical paternalism and authoritarian overreach.
% FOUNDING_PROBLEM_CORROBORATION: International human rights declarations (e.g., Universal Declaration of Human Rights, International Covenant on Civil and Political Rights), historical medical ethics codes (e.g., Nuremberg Code, Helsinki Declaration), and numerous constitutional law precedents from independent legal scholars and human rights organizations corroborate the foundational importance of informed consent and bodily integrity, often in direct opposition to state claims of public health necessity.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.90) stems from the fundamental violation of self-ownership and decision-making power over one's body. The high suppression (0.95) reflects the direct coercion and penalties imposed by state mandates, which eliminate the alternative of refusal. The low theater ratio (0.10) indicates that the violation is direct and functional, not merely performative; the harm to autonomy is real and immediate. The metrics reflect the period of active public health mandates (e.g., 2020-2023), where the intensity of mandates and associated resistance fluctuated.
 *
 * PERSPECTIVAL GAP:
 *   This story deliberately presents a strong, rights-centric perspective. Other readings of the 'mandate_legitimacy_scope' kernel (e.g., 'public_health_primary' or 'proportionality_reading') would yield vastly different metrics and classifications, as they would weigh collective benefit or a balancing test differently. This story does not attempt to reconcile these perspectives but rather articulates one specific, internally consistent reading.
 *
 * DIRECTIONALITY LOGIC:
 *   From this reading, 'unvaccinated_coerced_individuals' and 'individuals_seeking_medical_autonomy' are clear targets, bearing the direct costs of lost autonomy and potential penalties. 'State_public_health_authorities' are the beneficiaries, as they achieve their public health objectives through these mandates, albeit by violating what this reading considers fundamental rights. 'Constitutional_law_scholars_autonomy_advocates' serve as analytical observers, while 'public_health_advocates' are excluded from the core premise of this rights-based framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''bodily_autonomy_primary'' reading of the ''mandate_legitimacy_scope'' kernel?',
    'Expert review by legal and ethical scholars specializing in bodily autonomy and constitutional rights, comparing the story''s structural claims to the tenets of this specific reading.',
    'If misidentified, the classification of this constraint would be inaccurate, potentially conflating distinct ethical and legal positions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verification of the specific kernel reading being instantiated.').

omega_variable(
    sibling_impact_public_health_primary,
    'How would the ''public_health_primary'' reading of the ''mandate_legitimacy_scope'' kernel alter the victim set and perceived extraction?',
    'Generate a separate constraint story for the ''public_health_primary'' reading, which would likely identify ''vulnerable_populations'' as victims of non-mandate scenarios and frame mandates as coordination, leading to lower extraction and a different victim set.',
    'A shift in reading would fundamentally reclassify the constraint, identifying different beneficiaries and victims and altering the perceived legitimacy of mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_impact_public_health_primary, conceptual, 'Impact of the ''public_health_primary'' sibling reading on classification.').

omega_variable(
    sibling_impact_proportionality_reading,
    'How would the ''proportionality_reading'' of the ''mandate_legitimacy_scope'' kernel alter the victim set and perceived extraction?',
    'Generate a separate constraint story for the ''proportionality_reading'', which would introduce a balancing test. This would likely lead to a more nuanced view of extraction, potentially identifying victims only when mandates fail the proportionality test, and thus a different classification.',
    'A shift to a proportionality reading would introduce conditional legitimacy, making extraction and victimhood dependent on specific circumstances rather than an absolute principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_impact_proportionality_reading, conceptual, 'Impact of the ''proportionality_reading'' sibling on classification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal penalties, job loss) or internalized (social pressure, fear of ostracization)?',
    'Post-mandate trajectory analysis: if suppression effects (e.g., self-censorship, continued avoidance of medical services) persist after legal mandates are removed, it suggests a significant internalized component. If effects dissipate rapidly, it points to primarily structural suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them even after formal mandates are lifted, making exit more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in medical mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mand_tr_t1, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 1, 0.1).
narrative_ontology:measurement(mand_tr_t2, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 2, 0.1).
narrative_ontology:measurement(mand_tr_t3, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 3, 0.1).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(mand_be_t1, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 1, 0.9).
narrative_ontology:measurement(mand_be_t2, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 2, 0.92).
narrative_ontology:measurement(mand_be_t3, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 3, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(mand_su_t1, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 1, 0.95).
narrative_ontology:measurement(mand_su_t2, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 2, 0.93).
narrative_ontology:measurement(mand_su_t3, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 3, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_mandates_enforcement).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, vaccine_hesitancy_discourse).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'mandate_legitimacy_scope' kernel, focusing on the primacy of bodily autonomy. It is structurally linked to other readings of the same kernel, as well as to downstream constraints related to public health enforcement and discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
