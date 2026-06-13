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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary in Mandate Legitimacy
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'bodily_autonomy_primary' reading
 *   of the 'mandate_legitimacy_scope' kernel. It asserts that any medical
 *   intervention without informed consent, regardless of collective benefit,
 *   constitutes a violation of fundamental bodily integrity. When state
 *   mandates for such interventions are present, this reading classifies the
 *   constraint as a snare, with unvaccinated-coerced individuals entering the
 *   victim set and the state acting as a rights violator. The high
 *   extractiveness and suppression reflect the direct infringement on
 *   individual liberty and the coercive force required to implement such
 *   mandates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.85).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.9).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Bodily Autonomy as Primary in Mandate Legitimacy").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, '503de11a-f36e-49fe-a22d-5b836343e7fc').
narrative_ontology:cs_kernel_codification('503de11a-f36e-49fe-a22d-5b836343e7fc', formalized).
narrative_ontology:cs_authority_grounding('503de11a-f36e-49fe-a22d-5b836343e7fc', lineage).
narrative_ontology:cs_interpretation_layer_present('503de11a-f36e-49fe-a22d-5b836343e7fc').
narrative_ontology:cs_reading_relation('503de11a-f36e-49fe-a22d-5b836343e7fc', mandate_legitimacy_scope__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('503de11a-f36e-49fe-a22d-5b836343e7fc', mandate_legitimacy_scope__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('503de11a-f36e-49fe-a22d-5b836343e7fc', foundational, bodily_integrity_is_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('503de11a-f36e-49fe-a22d-5b836343e7fc', bodily_integrity_is_absolute, deontological).
narrative_ontology:cs_axiom('503de11a-f36e-49fe-a22d-5b836343e7fc', foundational, informed_consent_is_non_derogable).
narrative_ontology:cs_axiom_status(informed_consent_is_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('503de11a-f36e-49fe-a22d-5b836343e7fc', informed_consent_is_non_derogable, deontological).
narrative_ontology:cs_reference_frame('503de11a-f36e-49fe-a22d-5b836343e7fc', unconditional_individual_sovereignty).
narrative_ontology:cs_drift_state('503de11a-f36e-49fe-a22d-5b836343e7fc', public_health_emergency_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('503de11a-f36e-49fe-a22d-5b836343e7fc', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, medical_autonomy_advocates).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__bodily_autonomy_primary, individual_rights_doctrine).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__bodily_autonomy_primary, informed_consent_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These authorities issue and enforce mandates for medical interventions, believing they are acting for the collective good. From this reading's perspective, they are the agents of rights violation when mandates are present.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Individuals who face severe penalties (loss of employment, exclusion from public life) for refusing a mandated medical intervention. They bear the direct cost of the constraint, experiencing it as a violation of their bodily integrity.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, immediate, trapped, local).

% Organizations and legal professionals who champion individual bodily autonomy and informed consent. They bear the costs of litigation and advocacy against mandates, seeing them as fundamental rights infringements.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, medical_autonomy_advocates, payer,
    organized, biographical, constrained, national).

% The abstract concept of a healthier population, which is the stated goal of public health mandates. While not an agent, it is listed as a beneficiary to acknowledge the claimed coordination function, even if this reading disputes its legitimacy.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, collective_public_health, beneficiary,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(mandate_legitimacy_scope__bodily_autonomy_primary, collective_public_health).

% Judicial bodies tasked with interpreting constitutional rights, including bodily autonomy, against state powers. They observe the conflict and adjudicate the legality of mandates, potentially altering the constraint's enforcement.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint, when mandates are present, aims to coordinate collective health outcomes by ensuring widespread participation in medical interventions, thereby reducing disease transmission and protecting vulnerable populations.
% TRANSFER_FUNCTION: It transfers the burden of potential bodily violation and loss of autonomy from the collective (risk of disease) to the individual (compelled intervention), enforced by state power.
% ABSENT_VOICES: Individuals who have experienced adverse reactions to mandated interventions, or those with deeply held philosophical objections, are often marginalized in public health discourse, their concerns dismissed in favor of collective utility. Their voices would highlight the direct and severe costs of the constraint.
% DISAPPEARANCE_RATIONALE: If the principle of bodily autonomy as primary vanished, public health authorities would face fewer legal and ethical barriers to implementing widespread mandates, potentially leading to a significant shift in individual rights relative to state power in medical decisions. The legal and ethical landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: The tension between individual liberty and collective welfare in public health crises, particularly concerning medical interventions that carry individual risks but offer collective benefits.
% FOUNDING_PROBLEM_CORROBORATION: Philosophers, legal scholars, and civil liberties organizations from outside public health institutions consistently attest to the enduring nature of this ethical and legal dilemma, citing ongoing debates and court cases globally.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).

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
 *   The extractiveness (0.85) is high because the constraint directly compels individuals to undergo medical procedures against their will, imposing a severe cost on their autonomy. Suppression (0.90) is also high, as mandates typically involve significant penalties (e.g., job loss, exclusion from public spaces) to enforce compliance and suppress dissent. The theater ratio is low (0.10) because the enforcement is direct and functional, not performative; the state genuinely intends to compel compliance. Accessibility collapse is high (0.75) because for those targeted by mandates, alternatives to compliance are severely limited or carry prohibitive costs. Resistance is high (0.80) due to the fundamental nature of the rights being asserted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state public health authorities, the constraint might be framed as a necessary rope or scaffold for collective well-being. However, from the 'bodily_autonomy_primary' reading, and the perspective of the coerced individuals, it is a clear snare, directly extracting autonomy through coercive suppression. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State public health authorities, while claiming to act for collective benefit, are the agenda-setters and enforcers of the mandates. Unvaccinated-coerced individuals and medical autonomy advocates are the primary payers/victims, bearing the direct costs of compelled intervention or the fight against it. Collective public health is an abstract beneficiary, representing the claimed outcome, but not an agent in the extraction. Constitutional courts act as observers, adjudicating the conflict.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_collective_benefit,
    'Is the claimed collective benefit from mandated medical interventions sufficiently robust and certain to justify any infringement on individual bodily autonomy?',
    'Epidemiological data on disease transmission and severity, vaccine efficacy and safety profiles, and public health modeling, rigorously evaluated by independent scientific bodies.',
    'If the collective benefit is found to be negligible or highly uncertain, it would further strengthen the ''snare'' classification of mandates under this reading, as the justification for extraction would collapse. If robust, it would highlight the fundamental conflict between this reading and those prioritizing public health.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_collective_benefit, empirical, 'The empirical basis for the collective benefit claimed by public health mandates.').

omega_variable(
    scope_of_bodily_autonomy,
    'What are the precise boundaries of ''fundamental bodily integrity'' in a society, and does it include absolute refusal of all mandated medical interventions, even in a public health emergency?',
    'Ongoing philosophical and legal debate, informed by historical precedents, international human rights law, and evolving societal norms regarding individual rights versus collective responsibilities.',
    'A narrower interpretation of bodily autonomy might allow for some mandates under specific conditions, potentially shifting the classification towards a ''tangled_rope'' or ''scaffold'' if a coordination function is acknowledged. A broader interpretation reinforces the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_bodily_autonomy, conceptual, 'The conceptual scope and limits of bodily autonomy in public health contexts.').

omega_variable(
    framing_under_determination_mandate_legitimacy,
    'Does the ''bodily_autonomy_primary'' reading represent the only defensible framing of mandate legitimacy, or do alternative framings (e.g., ''public_health_primary'', ''proportionality_reading'') offer equally coherent, albeit different, classifications?',
    'Analysis of the logical consistency and empirical fit of each reading''s axioms and their implications for policy. The choice of framing is ultimately a conceptual and preference-based decision, not purely empirical.',
    'Acknowledging the coherence of alternative framings would highlight the deep, irreducible conflict within the ''mandate_legitimacy_scope'' kernel, demonstrating that different foundational axioms lead to fundamentally different classifications of the same real-world phenomenon. This would underscore the role of values in constraint classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination_mandate_legitimacy, conceptual, 'Alternative coherent framings of mandate legitimacy and their impact on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 2020, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t2020, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(mand_tr_t2021, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 2021, 0.1).
narrative_ontology:measurement(mand_tr_t2022, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 2022, 0.1).
narrative_ontology:measurement(mand_tr_t2023, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(mand_be_t2020, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(mand_be_t2021, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 2021, 0.85).
narrative_ontology:measurement(mand_be_t2022, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 2022, 0.8).
narrative_ontology:measurement(mand_be_t2023, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 2023, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t2020, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(mand_su_t2021, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 2021, 0.9).
narrative_ontology:measurement(mand_su_t2022, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 2022, 0.85).
narrative_ontology:measurement(mand_su_t2023, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 2023, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'mandate_legitimacy_scope' kernel, focusing on individual bodily autonomy. Other readings (e.g., 'public_health_primary', 'proportionality_reading') would yield different classifications and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
