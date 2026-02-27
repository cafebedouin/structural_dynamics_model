% ============================================================================
% CONSTRAINT STORY: dsa_transparency_requirements
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsa_transparency_requirements, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dsa_transparency_requirements
 *   human_readable: EU Digital Services Act (DSA) Transparency Requirements
 *   domain: political/technological
 *
 * SUMMARY:
 *   The European Union's Digital Services Act (DSA) imposes strict
 *   transparency obligations on Very Large Online Platforms (VLOPs), aiming
 *   to protect users from illegal content and harmful online practices. While
 *   the DSA aims to foster a safer online environment, it also presents
 *   challenges for platforms and users, creating a complex interplay of
 *   benefits and costs. The DSA transparency requirements are a tangled rope,
 *   designed to coordinate platform accountability while extracting effort
 *   and data from them.
 *
 * KEY AGENTS:
 *   - European Commission: Primary beneficiary (institutional/arbitrage) - Gains increased regulatory power and oversight.
 *   - EU Citizens: Secondary beneficiary (moderate/mobile) - Experience safer online environments.
 *   - Researchers: Tertiary beneficiary (analytical/analytical) - Gain greater access to platform data for research purposes.
 *   - VLOPs: Primary target (institutional/constrained) - Face increased compliance costs and regulatory scrutiny.
 *   - Vulnerable Users: Secondary target (powerless/trapped) - Remain vulnerable to manipulative content if DSA implementation is weak.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsa_transparency_requirements, 0.55).
domain_priors:suppression_score(dsa_transparency_requirements, 0.45).
domain_priors:theater_ratio(dsa_transparency_requirements, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsa_transparency_requirements, extractiveness, 0.55).
narrative_ontology:constraint_metric(dsa_transparency_requirements, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(dsa_transparency_requirements, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsa_transparency_requirements, tangled_rope).
narrative_ontology:human_readable(dsa_transparency_requirements, "EU Digital Services Act (DSA) Transparency Requirements").
narrative_ontology:topic_domain(dsa_transparency_requirements, "political/technological").

domain_priors:requires_active_enforcement(dsa_transparency_requirements).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsa_transparency_requirements, european_commission).
narrative_ontology:constraint_beneficiary(dsa_transparency_requirements, eu_citizens).
narrative_ontology:constraint_beneficiary(dsa_transparency_requirements, researchers).
narrative_ontology:constraint_victim(dsa_transparency_requirements, vulnerable_users).
narrative_ontology:constraint_victim(dsa_transparency_requirements, vlo_platforms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Vulnerable users are trapped by manipulative or illegal content and lack the resources to effectively hold platforms accountable.
constraint_indexing:constraint_classification(dsa_transparency_requirements, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% VLOPs are constrained by compliance costs but also benefit from increased user trust and a level playing field.
constraint_indexing:constraint_classification(dsa_transparency_requirements, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% The European Commission benefits from increased platform accountability and regulatory power.
constraint_indexing:constraint_classification(dsa_transparency_requirements, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% Researchers benefit from greater access to data but must navigate complex legal and technical frameworks.
constraint_indexing:constraint_classification(dsa_transparency_requirements, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% EU citizens benefit from safer online experiences but might see some services diminished or disappear, while their data are more actively scrutinized.
constraint_indexing:constraint_classification(dsa_transparency_requirements, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsa_transparency_requirements_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dsa_transparency_requirements, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dsa_transparency_requirements, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsa_transparency_requirements, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsa_transparency_requirements_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The DSA's extractiveness score reflects the compliance costs and data reporting burdens imposed on VLOPs. The suppression score indicates the constraints on platform autonomy and content moderation practices. The theater ratio is low because the DSA is expected to have a genuine function: greater visibility into online content, advertising, and recommender systems.
 *
 * PERSPECTIVAL GAP:
 *   The DSA presents a perspectival gap between beneficiaries and targets. The European Commission sees the DSA as a necessary measure to ensure platform accountability, while VLOPs perceive it as an intrusive regulatory burden. Vulnerable users, though intended beneficiaries, may not experience immediate or significant improvements in online safety if the DSA is not effectively enforced.
 *
 * DIRECTIONALITY LOGIC:
 *   The European Commission and EU citizens benefit from the DSA because of the increased safety and regulatory clarity it provides. VLOPs are targeted because they face significant compliance costs and potential penalties for non-compliance. The DSA's directionality is derived from the structural relationship between these agents and the policy's objectives.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_vulnerable_users,
    'How to define and identify vulnerable users in the online environment?',
    'Development of standardized vulnerability metrics and risk assessment tools.',
    'Narrow definition: Snare classification weakened. Broad definition: Snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_vulnerable_users, conceptual, 'Defining and identifying vulnerable users.').

omega_variable(
    degree_of_platform_compliance,
    'To what extent are VLOPs genuinely committed to transparency and accountability?',
    'Independent audits of VLOP transparency reports and algorithmic impact assessments.',
    'High compliance: Tangled Rope classification weakened, moving towards Rope. Low compliance: Tangled Rope classification strengthened, moving towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_platform_compliance, empirical, 'Assessing VLOP''s commitment to transparency and accountability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsa_transparency_requirements, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsa__tr_t0, dsa_transparency_requirements, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dsa__tr_t5, dsa_transparency_requirements, theater_ratio, 5, 0.3).
narrative_ontology:measurement(dsa__tr_t10, dsa_transparency_requirements, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(dsa__be_t0, dsa_transparency_requirements, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dsa__be_t5, dsa_transparency_requirements, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(dsa__be_t10, dsa_transparency_requirements, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsa_transparency_requirements, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
