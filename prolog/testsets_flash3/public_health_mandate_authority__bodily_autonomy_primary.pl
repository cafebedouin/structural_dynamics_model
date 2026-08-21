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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Public Health Mandate as Bodily Autonomy Violation
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint represents the 'bodily_autonomy_primary' reading of the
 *   'public_health_mandate_authority' kernel. From this perspective, public
 *   health mandates are a categorical violation of individual bodily
 *   sovereignty, and no collective benefit can justify non-consensual medical
 *   intervention. The constraint is classified as a Snare because it is seen
 *   as purely extractive, coercing individuals into medical interventions
 *   against their will, with no genuine coordination function from this
 *   ethical standpoint. The unvaccinated and those advocating for medical
 *   freedom are the primary victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.95).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.88).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.95).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Public Health Mandate as Bodily Autonomy Violation").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, 'fe034b4c-c53b-4c13-9491-682dc2c137d2').
narrative_ontology:cs_kernel_codification('fe034b4c-c53b-4c13-9491-682dc2c137d2', formalized).
narrative_ontology:cs_authority_grounding('fe034b4c-c53b-4c13-9491-682dc2c137d2', lineage).
narrative_ontology:cs_reading_relation('fe034b4c-c53b-4c13-9491-682dc2c137d2', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('fe034b4c-c53b-4c13-9491-682dc2c137d2', public_health_mandate_authority__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('fe034b4c-c53b-4c13-9491-682dc2c137d2', foundational, bodily_autonomy_is_absolute).
narrative_ontology:cs_axiom_status(bodily_autonomy_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('fe034b4c-c53b-4c13-9491-682dc2c137d2', bodily_autonomy_is_absolute, deontological).
narrative_ontology:cs_axiom('fe034b4c-c53b-4c13-9491-682dc2c137d2', foundational, collective_benefit_does_not_justify_non_consensual_intervention).
narrative_ontology:cs_axiom_status(collective_benefit_does_not_justify_non_consensual_intervention, holdable).
narrative_ontology:cs_axiom_grounding('fe034b4c-c53b-4c13-9491-682dc2c137d2', collective_benefit_does_not_justify_non_consensual_intervention, deontological).
narrative_ontology:cs_reference_frame('fe034b4c-c53b-4c13-9491-682dc2c137d2', absolute_bodily_sovereignty).
narrative_ontology:cs_drift_state('fe034b4c-c53b-4c13-9491-682dc2c137d2', contemporary_public_health_crises, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fe034b4c-c53b-4c13-9491-682dc2c137d2', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, individuals_seeking_medical_freedom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals are directly subjected to mandates, facing exclusion from public spaces, employment, or education if they do not comply. They experience the mandate as a direct violation of their bodily autonomy and a coercive imposition.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals, payer,
    powerless, immediate, trapped, national).

% Advocate for the right to refuse medical interventions without coercion. They bear the social and economic costs of non-compliance and actively resist mandates, viewing them as an overreach of state power into personal medical decisions.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, individuals_seeking_medical_freedom, payer,
    moderate, biographical, constrained, national).

% Issue and enforce public health mandates, believing they are acting to protect the collective good. From this reading's perspective, they are imposing a categorical violation, even if they believe their actions are justified.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Are vulnerable to infectious diseases and rely on herd immunity for protection. From this reading's perspective, their vulnerability does not create a moral claim that justifies infringing on the bodily autonomy of others, and they are excluded from the victim set of this specific constraint.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_individuals, excluded,
    powerless, immediate, trapped, local).

% Believe public health mandates are a necessary and ethical tool for collective well-being. From this reading's perspective, they are not beneficiaries of the constraint (as no coercion is imposed on them) but rather proponents of a conflicting ethical framework, and are excluded from the victim set.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. From this reading's perspective, the mandate does not solve a coordination problem but rather imposes a non-consensual intervention, creating a coercive structure.
% TRANSFER_FUNCTION: Transfers the burden of non-consensual medical intervention and its associated social/economic penalties from the collective (or the state) to individuals who refuse, in exchange for access to public life.
% ABSENT_VOICES: The voices of those who believe in absolute bodily sovereignty are often marginalized or dismissed in public health discourse, framed as 'anti-science' or 'selfish,' rather than as legitimate ethical positions. Immunocompromised individuals are also absent from the victim set, as their claims for protection are not seen as justifying bodily invasion.
% DISAPPEARANCE_RATIONALE: If public health mandates vanished, individuals would regain full control over their medical decisions without state coercion. Society would need to rearrange its approach to collective health, relying on voluntary measures, education, and individual risk assessment, rather than mandates. The structure of public life would shift to accommodate diverse medical choices.
% FOUNDING_PROBLEM: The problem of balancing individual liberty with collective well-being, particularly in times of public health crises.
% FOUNDING_PROBLEM_CORROBORATION: The problem of balancing individual liberty and collective well-being is a perennial philosophical and legal challenge, attested by constitutional scholars, bioethicists, and legal precedents across various jurisdictions, not just by those who benefit from mandates.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.95, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.95) because the constraint imposes a fundamental violation of bodily autonomy, which is considered an irreducible harm. Suppression is also very high (0.88) as mandates are enforced through legal, social, and economic penalties, leaving individuals with severely constrained or no exit options without significant personal cost. Resistance is high (0.9) due to the perceived fundamental nature of the violation. Theater ratio is low (0.1) because the enforcement is direct and functional in achieving compliance, not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between this reading, which prioritizes individual bodily autonomy as an absolute, and other readings that prioritize collective health or apply a proportionality test. From this reading, the mandate is a Snare; from a public health primary reading, it might be a Rope or Tangled Rope. The engine will compute these divergences based on the structural data provided for each reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated individuals and those seeking medical freedom are full targets (high d) as they bear the direct costs and coercion. Public health authorities, while acting from a different ethical framework, are seen as the agenda-setters imposing this extractive structure. Immunocompromised individuals and public health primary advocates are excluded from the victim/beneficiary sets of this specific reading, as their claims are either not seen as justifying bodily invasion or are proponents of a conflicting ethical framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolute_vs_proportional_autonomy,
    'Is bodily autonomy an absolute right, or can it be proportionally limited for collective benefit under certain conditions?',
    'Philosophical and legal consensus on the limits of rights in a social context, potentially informed by judicial rulings on public health powers.',
    'If autonomy is absolute, this reading''s Snare classification holds. If it can be proportionally limited, the constraint might be reclassified as a Tangled Rope or Scaffold under a different reading, depending on the proportionality assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolute_vs_proportional_autonomy, conceptual, 'The fundamental conceptual disagreement over the nature and limits of bodily autonomy.').

omega_variable(
    coercion_definition_ambiguity,
    'At what point do incentives or restrictions (e.g., access to public spaces) become ''coercion'' in the context of medical decisions?',
    'Empirical studies on perceived freedom of choice under various public health measures, combined with ethical frameworks for defining coercion in medical contexts.',
    'A broader definition of coercion strengthens the Snare classification. A narrower definition might shift the classification towards a Tangled Rope if some restrictions are deemed non-coercive but still extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_definition_ambiguity, empirical, 'Ambiguity in defining the threshold for coercive medical intervention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t5, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(publ_tr_t10, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(publ_be_t5, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 5, 0.93).
narrative_ontology:measurement(publ_be_t10, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 10, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(publ_su_t5, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(publ_su_t10, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 10, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
