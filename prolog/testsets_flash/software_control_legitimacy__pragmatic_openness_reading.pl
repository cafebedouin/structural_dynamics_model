% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__pragmatic_openness_reading, []).

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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Pragmatic Openness Reading of Software Control Legitimacy
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'pragmatic openness' reading of software
 *   control legitimacy, which views both open source and proprietary
 *   development models as legitimate choices. It asserts that open source
 *   often leads to better software through peer review and collaboration, but
 *   does not deny the validity of proprietary alternatives. The constraint's
 *   low extractiveness and suppression reflect its acceptance of diverse
 *   models and its focus on quality optimization rather than ideological
 *   enforcement. It functions as a coordination mechanism for developers and
 *   users seeking optimal software solutions, regardless of licensing model.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.15).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.05).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Pragmatic Openness Reading of Software Control Legitimacy").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "software_engineering/political_economy_of_technology/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, 'ec9a00a5-26fa-4371-9782-a8b73b27383b').
narrative_ontology:cs_kernel_codification('ec9a00a5-26fa-4371-9782-a8b73b27383b', distributed).
narrative_ontology:cs_authority_grounding('ec9a00a5-26fa-4371-9782-a8b73b27383b', expertise).
narrative_ontology:cs_interpretation_layer_present('ec9a00a5-26fa-4371-9782-a8b73b27383b').
narrative_ontology:cs_reading_relation('ec9a00a5-26fa-4371-9782-a8b73b27383b', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec9a00a5-26fa-4371-9782-a8b73b27383b', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec9a00a5-26fa-4371-9782-a8b73b27383b', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('ec9a00a5-26fa-4371-9782-a8b73b27383b', foundational, development_model_pluralism_is_optimal).
narrative_ontology:cs_axiom_status(development_model_pluralism_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('ec9a00a5-26fa-4371-9782-a8b73b27383b', development_model_pluralism_is_optimal, instrumental).
narrative_ontology:cs_axiom('ec9a00a5-26fa-4371-9782-a8b73b27383b', foundational, quality_is_primary_metric).
narrative_ontology:cs_axiom_status(quality_is_primary_metric, holdable).
narrative_ontology:cs_axiom_grounding('ec9a00a5-26fa-4371-9782-a8b73b27383b', quality_is_primary_metric, empirically_contingent).
narrative_ontology:cs_reference_frame('ec9a00a5-26fa-4371-9782-a8b73b27383b', quality_driven_ecosystem).
narrative_ontology:cs_drift_state('ec9a00a5-26fa-4371-9782-a8b73b27383b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ec9a00a5-26fa-4371-9782-a8b73b27383b', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_developers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_companies).
narrative_ontology:constraint_vindicates(software_control_legitimacy__pragmatic_openness_reading, quality_through_collaboration_hypothesis).
narrative_ontology:constraint_vindicates(software_control_legitimacy__pragmatic_openness_reading, market_pluralism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the legitimacy of both open source and proprietary models, allowing them to choose development methodologies based on project needs and quality goals. They are encouraged to adopt practices that lead to 'better software' through peer review and collaboration.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_developers, beneficiary,
    organized, biographical, mobile, global).

% Benefit from a diverse software ecosystem where quality is prioritized, regardless of the underlying licensing model. They have access to a wider range of solutions and benefit from the peer review and collaboration inherent in open source, as well as the commercial support of proprietary software.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_users, beneficiary,
    organized, biographical, mobile, global).

% Promote open source as a superior development methodology due to its collaborative nature and peer review, but acknowledge the legitimacy of proprietary alternatives. They shape the discourse around software quality and development practices.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_advocates, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from the recognition of their development model as legitimate, allowing them to operate and innovate within the broader software ecosystem. They are not subjected to ethical condemnation or calls for their abolition by this reading.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_companies, beneficiary,
    institutional, generational, mobile, global).

% Analyze the legal implications of different software control models, ensuring that both open source and proprietary licenses are respected within their respective frameworks. They observe the practical outcomes of each model.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, intellectual_property_lawyers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the software development community around a shared understanding that both open source and proprietary models are legitimate, fostering collaboration and competition based on quality and pragmatic outcomes rather than ideological purity.
% TRANSFER_FUNCTION: Transfers legitimacy and acceptance to both open source and proprietary development models, from a discourse that might otherwise be dominated by ideological conflict. It transfers focus from 'which model is right' to 'which model works best for a given problem'.
% ABSENT_VOICES: Those who hold extreme ideological positions (e.g., absolute freedom advocates who condemn all proprietary software, or absolute property rights advocates who dismiss open source as unsustainable) are marginalized by this pragmatic framing. They would argue for the exclusive legitimacy of their preferred model.
% DISAPPEARANCE_RATIONALE: If this pragmatic reading vanished, the discourse around software control would likely revert to more polarized, ideological debates. This would disrupt collaboration between open source and proprietary projects, potentially leading to less innovation and a less diverse software ecosystem, as each side would delegitimize the other.
% FOUNDING_PROBLEM: The founding problem was the ideological polarization and conflict within the software community regarding the legitimacy of open source versus proprietary models, hindering collaboration and pragmatic decision-making.
% FOUNDING_PROBLEM_CORROBORATION: The continued existence of strong ideological factions and ongoing debates in policy and academic circles, as attested by independent technology journalists and policy analysts, corroborates that the problem of ideological polarization remains live, even if this reading attempts to bridge it.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__pragmatic_openness_reading_tests).
:- end_tests(software_control_legitimacy__pragmatic_openness_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.05) reflect this reading's non-coercive stance. It doesn't extract from any party because it legitimizes multiple approaches, and it doesn't suppress alternatives. The 'better software' claim is presented as an empirical observation, not a mandate. Theater ratio is 0.0 as there's no performative maintenance; the constraint's function is genuinely about guiding development choices based on observed outcomes. Accessibility collapse is high (0.8) because once the pragmatic benefits of both models are understood, the 'alternatives' are simply the other legitimate choice, not suppressed options.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of developers and users, this constraint is a Rope, facilitating choice and quality. There is no significant perspectival gap because the reading itself is about accommodating diverse perspectives and optimizing for shared goals (quality).
 *
 * DIRECTIONALITY LOGIC:
 *   Software developers and users are the primary beneficiaries (d near 0.0) as they gain from the focus on quality and the legitimacy of diverse development models. There are no direct victims, as the constraint explicitly accepts the legitimacy of both open source and proprietary models, avoiding extraction or suppression of either. The constraint subsidizes the ecosystem by promoting a balanced view that reduces ideological conflict.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by remaining focused on the pragmatic outcomes of software development (quality, collaboration) rather than rigid ideological adherence. Its mandate is to foster an environment where both open source and proprietary models can thrive based on their merits, which remains a live problem. If it were to become a Snare, it would likely be by implicitly or explicitly favoring one model while claiming neutrality, which would be detected by rising extractiveness and suppression for the disfavored model.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''pragmatic openness'' reading, or does it implicitly favor one model over the other?',
    'Analysis of resource allocation and institutional support for both open source and proprietary models within the framework''s influence. If support is asymmetric, reclassify as implicitly favoring the dominant model.',
    'If implicitly biased, the constraint''s true extractiveness and suppression would be higher for the disfavored model, potentially shifting classification towards a Tangled Rope or Snare for that model''s adherents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''pragmatic openness'' reading of the ''software_control_legitimacy'' kernel. Sibling readings include ''freedom_imperative_reading'', ''property_rights_reading'', and ''commons_reading''. This reading differs by accepting the legitimacy of both open source and proprietary models, focusing on quality and choice rather than ethical imperatives or absolute rights.').

omega_variable(
    quality_optimization_metrics,
    'Are ''better software'' claims empirically verifiable across both open source and proprietary models, or are they subjective preferences?',
    'Longitudinal studies comparing software quality metrics (security vulnerabilities, bug density, performance, maintainability) across diverse open source and proprietary projects, controlling for project size, funding, and team expertise.',
    'If quality claims are subjective, the ''pragmatic openness'' reading loses its primary empirical grounding, potentially weakening its influence on policy and development choices.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quality_optimization_metrics, empirical, 'Empirical basis for ''better software'' claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 10, 0.0).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 20, 0.0).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 30, 0.0).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 30, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 30, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_control_legitimacy' kernel, each representing a distinct structural claim about software development and intellectual property.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
