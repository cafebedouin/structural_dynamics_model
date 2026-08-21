% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary Constraint on Medical Coercion
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the reading of the 'coercion legitimacy
 *   boundary' kernel that prioritizes individual bodily autonomy above
 *   collective benefit in medical interventions. It asserts that medical
 *   intervention without consent is categorically impermissible. This reading
 *   emerged strongly post-WWII with the Nuremberg Code and has been
 *   reinforced by subsequent bioethical principles. While framed as a 'rope'
 *   due to its coordination function in establishing trust and clear ethical
 *   boundaries, its operation imposes costs on public health efforts and
 *   vulnerable populations, leading to a moderate extractiveness score. The
 *   claimed type 'rope' reflects the ideal of a universally beneficial
 *   ethical standard, while the metrics reflect the real-world trade-offs and
 *   'victims' of this strong autonomy stance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.45).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.2).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.45).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Bodily Autonomy as Primary Constraint on Medical Coercion").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, '5933f866-dbad-49c7-ae52-fe094f90ba79').
narrative_ontology:cs_kernel_codification('5933f866-dbad-49c7-ae52-fe094f90ba79', formalized).
narrative_ontology:cs_authority_grounding('5933f866-dbad-49c7-ae52-fe094f90ba79', lineage).
narrative_ontology:cs_interpretation_layer_present('5933f866-dbad-49c7-ae52-fe094f90ba79').
narrative_ontology:cs_reading_relation('5933f866-dbad-49c7-ae52-fe094f90ba79', coercion_legitimacy_boundary__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('5933f866-dbad-49c7-ae52-fe094f90ba79', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('5933f866-dbad-49c7-ae52-fe094f90ba79', foundational, individual_bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(individual_bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('5933f866-dbad-49c7-ae52-fe094f90ba79', individual_bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('5933f866-dbad-49c7-ae52-fe094f90ba79', foundational, consent_is_precondition_for_intervention).
narrative_ontology:cs_axiom_status(consent_is_precondition_for_intervention, holdable).
narrative_ontology:cs_axiom_grounding('5933f866-dbad-49c7-ae52-fe094f90ba79', consent_is_precondition_for_intervention, conventional).
narrative_ontology:cs_reference_frame('5933f866-dbad-49c7-ae52-fe094f90ba79', nuremberg_code_principles).
narrative_ontology:cs_drift_state('5933f866-dbad-49c7-ae52-fe094f90ba79', covid_19_pandemic_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5933f866-dbad-49c7-ae52-fe094f90ba79', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, individuals_asserting_autonomy).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, civil_liberties_advocates).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals benefit from the legal and ethical framework that prioritizes their right to refuse medical intervention, even if it carries collective risks. Their exit option is to move to jurisdictions with similar protections, or to resist mandates through legal challenge.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, individuals_asserting_autonomy, beneficiary,
    moderate, biographical, mobile, national).

% These groups benefit from the precedent set by strong bodily autonomy protections, which aligns with their broader mission to defend individual rights against state overreach. Their 'exit' is to shift focus to other rights issues, but they are deeply invested in this principle.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, civil_liberties_advocates, beneficiary,
    organized, generational, analytical, national).

% These individuals bear the cost of reduced herd immunity and increased exposure to preventable diseases when others decline vaccination. They are often medically vulnerable and have limited options to avoid exposure, making them effectively 'trapped' by the choices of others.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals, payer,
    powerless, immediate, trapped, local).

% These authorities bear the cost of reduced ability to implement population-level health interventions, leading to higher disease burden and strain on healthcare systems. Their options are constrained by legal challenges and public resistance, forcing them to rely on persuasion rather than mandates.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities, payer,
    institutional, generational, constrained, national).

% These are the governmental or institutional bodies tasked with implementing public health policies. Under this reading, their ability to enforce mandates is severely curtailed, forcing them to operate within strict limits of consent. They are beneficiaries of a clear legal boundary, but victims of reduced policy tools.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcers, agenda_setter,
    institutional, biographical, constrained, national).

% These professionals navigate the ethical and practical challenges of respecting patient autonomy while also managing public health risks. They are observers of the legal and ethical debates, often caught between conflicting duties.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, healthcare_providers, observer,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear ethical and legal boundary for individual medical decision-making, fostering trust between patients and the medical system by guaranteeing non-coercion.
% TRANSFER_FUNCTION: Transfers the burden of collective health risk from individuals to the community, and from the state to individual choice, in exchange for guaranteed personal autonomy.
% ABSENT_VOICES: Future generations and populations in developing nations, who would benefit from stronger global health security and precedents for collective action, are not directly represented in the current debate. Their interests are often subordinated to immediate individual rights claims.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the legal and ethical landscape of medicine would fundamentally shift. States would gain broad powers to compel medical interventions, leading to widespread public resistance, ethical crises, and a complete re-evaluation of patient rights. The relationship between individual and state would be profoundly altered.
% FOUNDING_PROBLEM: The historical problem of medical abuses, forced sterilization, and unethical human experimentation, which demonstrated the critical need for individual consent and protection against state-sanctioned medical coercion.
% FOUNDING_PROBLEM_CORROBORATION: Medical ethicists, human rights organizations, and historical records universally corroborate the founding problem as live, citing ongoing concerns about patient rights and the potential for medical overreach. This corroboration comes from outside the direct beneficiaries of the constraint.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).
:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while it protects individual rights, it imposes a cost on public health by limiting interventions that could benefit the collective, particularly vulnerable groups like the immunocompromised. Suppression (0.20) is low because this constraint is about limiting state coercion, not imposing it; its persistence relies on ethical consensus and legal precedent rather than active enforcement against dissenters. Theater ratio (0.10) is low as the principle is genuinely applied, though its interpretation is contested. Accessibility collapse (0.30) is low as alternatives (e.g., voluntary vaccination campaigns, public education) are still available, though less effective. Resistance (0.15) is low because the principle itself is widely accepted, though its application in specific public health crises generates resistance from those advocating for collective measures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals asserting autonomy, this constraint is a pure rope, a fundamental protection. From the perspective of immunocompromised individuals, it functions as a snare, exposing them to risk due to others' choices. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals asserting autonomy and civil liberties advocates are clear beneficiaries, as the constraint directly protects their rights and mission. Immunocompromised individuals and public health authorities are 'victims' in this reading, as they bear the costs of reduced collective protection and limited policy tools, respectively. Mandate enforcers are both beneficiaries (of clear legal boundaries) and payers (of reduced coercive power). Healthcare providers are observers, navigating the ethical landscape.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandate (protecting individual autonomy from medical coercion) remains highly live and relevant, especially given historical abuses and ongoing ethical debates. It prevents mislabeling a fundamental ethical protection as pure extraction, even while acknowledging its costs to public health. The 'rope' classification reflects its foundational coordination function in establishing trust and ethical boundaries, despite the 'victim' seats it creates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_harm_quantification,
    'How can the collective harm from non-intervention be reliably quantified and compared against individual autonomy infringements?',
    'Development of robust epidemiological models and ethical frameworks for comparing individual rights with population-level health outcomes, with broad societal consensus.',
    'If collective harm is demonstrably severe and quantifiable, it could shift the balance towards a ''proportionality_reading'' or ''public_health_primary'' classification, increasing extractiveness for individuals asserting autonomy. If not, this reading''s ''rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_harm_quantification, empirical, 'Uncertainty in quantifying collective harm vs. individual rights.').

omega_variable(
    coercion_legitimacy_framing,
    'Is the ''coercion_legitimacy_boundary'' kernel fundamentally about individual rights, or about the state''s legitimate scope of action?',
    'Societal and legal consensus on the foundational principles of state power versus individual liberty in public health contexts.',
    'If framed primarily as state scope, the ''public_health_primary'' reading gains legitimacy, potentially reclassifying this constraint as a ''snare'' from the state''s perspective. If individual rights remain primary, this reading''s ''rope'' classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_legitimacy_framing, conceptual, 'Conceptual framing of the coercion legitimacy boundary.').

omega_variable(
    immunocompromised_victim_status,
    'Is the ''victim'' status of immunocompromised individuals a direct consequence of this constraint, or an unavoidable feature of a free society?',
    'Ethical and legal analysis of the state''s positive obligations to protect vulnerable groups versus its negative obligation not to coerce individuals.',
    'If direct consequence, it reinforces the moderate extractiveness of this reading. If unavoidable, it reduces the perceived extractiveness, pushing the classification closer to a pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_victim_status, preference, 'Whether vulnerability is a direct cost of autonomy or an inherent societal condition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t1947, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(coer_tr_t1970, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(coer_tr_t1990, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(coer_tr_t2010, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(coer_tr_t2020, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(coer_tr_t2024, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(coer_be_t1947, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 1947, 0.3).
narrative_ontology:measurement(coer_be_t1970, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(coer_be_t1990, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(coer_be_t2010, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(coer_be_t2020, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2020, 0.44).
narrative_ontology:measurement(coer_be_t2024, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t1947, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 1947, 0.1).
narrative_ontology:measurement(coer_su_t1970, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 1970, 0.12).
narrative_ontology:measurement(coer_su_t1990, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(coer_su_t2010, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement(coer_su_t2020, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2020, 0.19).
narrative_ontology:measurement(coer_su_t2024, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, identity_coordination).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_mandate_legitimacy).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_hesitancy_dynamics).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'coercion_legitimacy_boundary' kernel. Other readings (public_health_primary, proportionality_reading) represent alternative structural claims about the same underlying ethical and legal tension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
