% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__bodily_autonomy_primary, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Public Health Mandate as Bodily Autonomy Violation
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint models the public health mandate from the perspective
 *   that it is a categorical violation of bodily sovereignty, where no
 *   collective benefit can justify non-consensual medical intervention. It is
 *   one reading of the 'public_health_mandate_authority' kernel. From this
 *   reading, the mandate operates as a Snare, extracting bodily autonomy
 *   through high suppression and coercion, with identifiable victims
 *   (unvaccinated individuals, conscientious objectors) and no legitimate
 *   beneficiaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.95).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.9).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.95).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Public Health Mandate as Bodily Autonomy Violation").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, 'ea1cdd43-fb99-482d-8dca-1e28c71e2f51').
narrative_ontology:cs_kernel_codification('ea1cdd43-fb99-482d-8dca-1e28c71e2f51', formalized).
narrative_ontology:cs_authority_grounding('ea1cdd43-fb99-482d-8dca-1e28c71e2f51', lineage).
narrative_ontology:cs_interpretation_layer_present('ea1cdd43-fb99-482d-8dca-1e28c71e2f51').
narrative_ontology:cs_reading_relation('ea1cdd43-fb99-482d-8dca-1e28c71e2f51', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('ea1cdd43-fb99-482d-8dca-1e28c71e2f51', public_health_mandate_authority__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('ea1cdd43-fb99-482d-8dca-1e28c71e2f51', foundational, bodily_sovereignty_absolute).
narrative_ontology:cs_axiom_status(bodily_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('ea1cdd43-fb99-482d-8dca-1e28c71e2f51', bodily_sovereignty_absolute, deontological).
narrative_ontology:cs_axiom('ea1cdd43-fb99-482d-8dca-1e28c71e2f51', foundational, collective_benefit_cannot_justify_coercion).
narrative_ontology:cs_axiom_status(collective_benefit_cannot_justify_coercion, holdable).
narrative_ontology:cs_axiom_grounding('ea1cdd43-fb99-482d-8dca-1e28c71e2f51', collective_benefit_cannot_justify_coercion, deontological).
narrative_ontology:cs_reference_frame('ea1cdd43-fb99-482d-8dca-1e28c71e2f51', absolute_individual_rights_framework).
narrative_ontology:cs_drift_state('ea1cdd43-fb99-482d-8dca-1e28c71e2f51', contemporary_public_health_crises, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ea1cdd43-fb99-482d-8dca-1e28c71e2f51', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, conscientious_objectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals are directly subjected to the mandate, facing exclusion from public spaces, employment, or education if they do not comply. Their bodily autonomy is directly violated by the state's coercive power, with no justification from collective benefit being accepted.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals, payer,
    powerless, immediate, trapped, national).

% Individuals whose objections are rooted in deeply held beliefs, for whom compliance with the mandate would constitute a violation of their core identity. They face similar coercive pressures as unvaccinated individuals, but their 'exit' is further constrained by identity fusion.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, conscientious_objectors, payer,
    powerless, biographical, identity_locked, national).

% These bodies issue and enforce the mandates, believing they are acting to protect collective well-being. From this reading's perspective, they are the agents of an illegitimate coercion, imposing non-consensual medical interventions.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Individuals who are medically vulnerable and rely on herd immunity for protection. From this reading's perspective, their vulnerability does not create a moral claim that justifies violating the bodily autonomy of others, and thus they are not considered beneficiaries of the mandate.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_individuals, excluded,
    powerless, immediate, trapped, local).

% These groups articulate and defend the principle of absolute bodily sovereignty, viewing any mandate as a fundamental rights violation. They analyze the constraint from a position of principled opposition.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, advocates_for_bodily_autonomy, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, there is no legitimate coordination function that can justify non-consensual medical intervention. The mandate attempts to coordinate collective health outcomes through coercion.
% TRANSFER_FUNCTION: Transfers the burden of collective health risk from the general population (and potentially the healthcare system) onto individuals through the imposition of non-consensual medical interventions, extracting their bodily autonomy.
% ABSENT_VOICES: The voices of those who believe collective benefit can justify some limits on individual autonomy are absent from this reading's core premise, as their arguments are categorically rejected. Similarly, the specific needs of the immunocompromised are not considered a justification for mandates.
% DISAPPEARANCE_RATIONALE: If the mandate disappeared, individuals would regain full control over their medical decisions. Public health authorities would lose a primary tool for managing epidemics, leading to a reorganization of public health strategies around voluntary compliance and education. The legal landscape regarding state power over individual bodies would fundamentally shift.
% FOUNDING_PROBLEM: The mandate was established to address public health crises, such as pandemics, by ensuring high rates of vaccination or other medical interventions to protect the population and healthcare infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and many medical professionals corroborate the ongoing problem of communicable diseases and the need for collective action. However, advocates for bodily autonomy and civil liberties groups contest the legitimacy of the mandate as a solution, arguing it creates a greater problem of rights violation. Legal challenges and public protests from outside the benefiting parties (public health authorities) corroborate the contested status.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is near maximal (0.95) because the constraint demands a non-consensual medical act, which is seen as the ultimate violation of bodily integrity. Suppression is also very high (0.9) due to the severe social, economic, and legal penalties for non-compliance. Theater ratio is low (0.1) because the enforcement is direct and functional, not performative; the mandate genuinely aims to compel compliance. Resistance is high (0.7) reflecting the significant opposition and legal challenges this reading generates.
 *
 * PERSPECTIVAL GAP:
 *   The public health authorities, from their own perspective, would see this as a Rope or Scaffold, a necessary coordination mechanism for collective well-being. However, from the 'bodily_autonomy_primary' reading, their actions are purely extractive and suppressive, creating a fundamental divergence in classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated individuals and conscientious objectors are full targets (d=1.0) as they bear the direct costs of bodily invasion and social exclusion. Public health authorities are the agenda-setters (d=0.0) as they impose the constraint, but from this reading's perspective, they do not 'benefit' in a legitimate sense, but rather exercise illegitimate power. Immunocompromised individuals are explicitly excluded from the beneficiary set, as their needs are not seen as justifying the violation of others' autonomy.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading asserts that the mandate's 'mandate' (to protect public health) is fundamentally flawed when it infringes on bodily autonomy. It prevents mislabeling coercion as coordination by foregrounding the individual rights violation, regardless of the stated collective goal. The constraint is not seen as having atrophied, but as being fundamentally illegitimate from its inception.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_collective_benefit,
    'Can any level of collective benefit or public health emergency legitimately override individual bodily autonomy?',
    'Philosophical and legal consensus on the hierarchy of rights, or a constitutional amendment explicitly defining the limits of state power over individual bodies in public health contexts.',
    'If collective benefit is deemed to sometimes override autonomy, this constraint''s extractiveness would be re-evaluated downward, potentially shifting its classification from Snare to Tangled Rope or even Scaffold. If not, its Snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_collective_benefit, conceptual, 'The fundamental conceptual disagreement on the balance between individual rights and collective good.').

omega_variable(
    coercion_vs_persuasion,
    'Is the mandate''s effect primarily coercive (forcing compliance) or persuasive (incentivizing voluntary action)?',
    'Empirical studies on compliance rates under varying levels of enforcement and alternative incentives. Analysis of whether individuals comply due to fear of penalty or genuine belief in collective good.',
    'If compliance is found to be largely voluntary, the suppression metric would decrease, potentially altering the classification. If it''s primarily coercive, the high suppression and Snare classification are validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_persuasion, empirical, 'Distinguishing between genuine coercion and strong incentives.').

omega_variable(
    mandate_as_rights_violation,
    'Is the public health mandate a categorical violation of fundamental human rights, or a legitimate exercise of state power within a rights framework?',
    'International human rights court rulings or domestic constitutional court decisions that explicitly address the legality and ethicality of such mandates in relation to bodily autonomy.',
    'A ruling affirming it as a categorical rights violation would solidify its Snare classification and high extractiveness. A ruling affirming it as legitimate state power would challenge this reading''s core premise, potentially leading to reclassification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_as_rights_violation, conceptual, 'Legal and ethical status of public health mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 2020, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t2020, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(publ_tr_t2021, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2021, 0.1).
narrative_ontology:measurement(publ_tr_t2022, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2022, 0.1).
narrative_ontology:measurement(publ_tr_t2023, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2023, 0.1).
narrative_ontology:measurement(publ_tr_t2024, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(publ_be_t2020, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2020, 0.85).
narrative_ontology:measurement(publ_be_t2021, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2021, 0.9).
narrative_ontology:measurement(publ_be_t2022, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2022, 0.95).
narrative_ontology:measurement(publ_be_t2023, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2023, 0.95).
narrative_ontology:measurement(publ_be_t2024, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t2020, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(publ_su_t2021, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2021, 0.85).
narrative_ontology:measurement(publ_su_t2022, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2022, 0.9).
narrative_ontology:measurement(publ_su_t2023, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2023, 0.9).
narrative_ontology:measurement(publ_su_t2024, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'public_health_mandate_authority' kernel, focusing on bodily autonomy as primary. The other readings ('public_health_primary' and 'proportionality_reading') offer alternative justifications or evaluations of public health mandates, leading to different structural classifications and metric profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
