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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Software Control Legitimacy (Pragmatic Openness Reading)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'pragmatic openness' reading of software
 *   control legitimacy, which views both open source and proprietary models
 *   as valid development methodologies. It emphasizes that open source often
 *   leads to better software through peer review and collaboration, but
 *   acknowledges the legitimate role of proprietary models for investment
 *   protection and commercial sustainability. This reading aims to foster a
 *   diverse and innovative software ecosystem by accepting coexistence rather
 *   than imposing a single ideological standard.
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
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Software Control Legitimacy (Pragmatic Openness Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "software_engineering/political_economy_of_technology/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, '6cc0b481-54dd-4038-a317-16b6cf1cd040').
narrative_ontology:cs_kernel_codification('6cc0b481-54dd-4038-a317-16b6cf1cd040', distributed).
narrative_ontology:cs_authority_grounding('6cc0b481-54dd-4038-a317-16b6cf1cd040', practice).
narrative_ontology:cs_interpretation_layer_present('6cc0b481-54dd-4038-a317-16b6cf1cd040').
narrative_ontology:cs_reading_relation('6cc0b481-54dd-4038-a317-16b6cf1cd040', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cc0b481-54dd-4038-a317-16b6cf1cd040', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cc0b481-54dd-4038-a317-16b6cf1cd040', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('6cc0b481-54dd-4038-a317-16b6cf1cd040', foundational, methodological_pluralism_is_optimal).
narrative_ontology:cs_axiom_status(methodological_pluralism_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('6cc0b481-54dd-4038-a317-16b6cf1cd040', methodological_pluralism_is_optimal, instrumental).
narrative_ontology:cs_axiom('6cc0b481-54dd-4038-a317-16b6cf1cd040', foundational, quality_and_collaboration_are_key_metrics).
narrative_ontology:cs_axiom_status(quality_and_collaboration_are_key_metrics, holdable).
narrative_ontology:cs_axiom_grounding('6cc0b481-54dd-4038-a317-16b6cf1cd040', quality_and_collaboration_are_key_metrics, empirically_contingent).
narrative_ontology:cs_reference_frame('6cc0b481-54dd-4038-a317-16b6cf1cd040', diverse_software_ecosystem).
narrative_ontology:cs_drift_state('6cc0b481-54dd-4038-a317-16b6cf1cd040', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6cc0b481-54dd-4038-a317-16b6cf1cd040', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_developers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_companies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the flexibility to choose between open source and proprietary models based on project needs and quality goals. They can leverage peer review and collaboration in open source, or commercial sustainability in proprietary models.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_developers, beneficiary,
    organized, biographical, mobile, global).

% Benefit from a diverse software ecosystem where both open source and proprietary options compete on quality, features, and support. They are not locked into a single model and can choose based on their specific requirements.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_users, beneficiary,
    moderate, biographical, constrained, global).

% Observe the software landscape, promoting open source as a superior development methodology due to its peer review and collaborative benefits, but acknowledging the practical legitimacy of proprietary alternatives.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_advocates, observer,
    organized, generational, analytical, global).

% Benefit from the recognition of their proprietary models as legitimate, allowing them to protect their investments and pursue commercial sustainability without being ethically condemned by this reading.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_companies, beneficiary,
    institutional, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the recognition of diverse software development and distribution models (open source and proprietary) as legitimate choices, allowing for a pluralistic and competitive software ecosystem.
% TRANSFER_FUNCTION: Facilitates the flow of innovation and diverse software solutions to users by validating multiple approaches, rather than transferring specific resources.
% ABSENT_VOICES: Extremist views from either the 'absolute freedom' or 'absolute property rights' camps might object, as this reading seeks a pragmatic middle ground, but they are not structurally excluded from the broader discourse.
% DISAPPEARANCE_RATIONALE: If this pragmatic understanding of software control legitimacy vanished, the discourse would likely polarize, leading to increased conflict between open source and proprietary camps, potentially hindering collaboration and innovation across the industry.
% FOUNDING_PROBLEM: The initial ideological clashes between proponents of open source and proprietary software created an environment of mutual delegitimization, hindering practical collaboration and diverse innovation.
% FOUNDING_PROBLEM_CORROBORATION: Industry analysts, software engineering academics, and cross-platform development teams corroborate that a pragmatic approach is essential for a healthy, innovative software ecosystem, preventing ideological purity tests from stifling progress.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.15) because this reading primarily coordinates acceptance of diverse models rather than extracting resources. Suppression is minimal (0.05) as it doesn't actively suppress alternatives but rather legitimizes their coexistence. The 'claimed_type' is 'rope' because it facilitates coordination and mutual benefit (diverse software, quality optimization) with low coercive overhead. The metrics reflect a decrease in extractiveness and suppression over time as the pragmatic view gained wider acceptance in the industry.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of developers and users, this reading is highly beneficial, offering choice and quality. From the perspective of more ideologically rigid advocates (e.g., 'freedom imperative' or 'property rights' readings), this pragmatic stance might be seen as a compromise that dilutes their core principles, but this reading does not structurally extract from them.
 *
 * DIRECTIONALITY LOGIC:
 *   Software developers and users are beneficiaries (d near 0.0) as they gain from the flexibility and diversity this reading promotes. Proprietary software companies also benefit from the legitimacy granted to their models. Open source advocates, while perhaps preferring a stronger stance, are also beneficiaries in that their model is recognized for its quality benefits without being forced into an ideological battle.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively resolves potential mandatrophy by preventing the ideological ossification of either open source or proprietary models. It ensures that the 'mandate' remains live by adapting to the practical needs of the software industry, rather than clinging to an outdated or overly rigid founding problem. The 'contested' status of the founding problem reflects the ongoing need for this pragmatic coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_pragmatic_openness,
    'Is this constraint a genuine pragmatic coordination, or a subtle form of extraction that legitimizes proprietary models at the expense of open source principles?',
    'Analysis of resource flows and power dynamics in the software industry: if proprietary models consistently capture disproportionate value or suppress open source innovation despite this reading, reclassify as a more extractive type.',
    'If reclassified as extractive, the ''rope'' claim would be falsified, likely shifting to ''tangled_rope'' or ''snare'' depending on the degree of suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_pragmatic_openness, conceptual, 'Ambiguity regarding the true nature of the ''pragmatic openness'' reading within the software control legitimacy kernel.').

omega_variable(
    sibling_reading_impact_on_pragmatism,
    'How would a stronger ''freedom imperative'' or ''property rights'' reading impact the stability and acceptance of this pragmatic openness reading?',
    'Empirical observation of legal challenges, policy debates, and market shifts in response to more absolutist claims from sibling readings.',
    'If a sibling reading gains dominance, it could either ''foreclose'' this pragmatic reading (if it becomes logically impossible to hold both) or ''influence'' it by shifting the Overton window, making the pragmatic stance less tenable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_impact_on_pragmatism, empirical, 'The potential for sibling readings to destabilize the pragmatic openness perspective.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(soft_be_t1990, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement(soft_be_t2000, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(soft_be_t2010, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2010, 0.16).
narrative_ontology:measurement(soft_be_t2024, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1990, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(soft_su_t2000, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(soft_su_t2010, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2010, 0.06).
narrative_ontology:measurement(soft_su_t2024, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, information_standard).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_licensing_practices).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, developer_toolchain_choices).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, digital_rights_management_norms).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_control_legitimacy' kernel, each representing a distinct structural claim. This 'pragmatic_openness_reading' focuses on methodological choice and quality optimization, distinct from 'freedom_imperative_reading' (user control), 'property_rights_reading' (creator investment), and 'commons_reading' (collective management).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
