% ============================================================================
% CONSTRAINT STORY: paxsilica_framework
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paxsilica_framework, []).

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
 *   constraint_id: paxsilica_framework
 *   human_readable: PaxSilica AI and Silicon Governance Framework
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   The PaxSilica framework is a proposed international agreement to
 *   coordinate policy on AI and silicon chips. This framework aims to address
 *   concerns about technological risks and promote responsible development.
 *   However, it also carries the potential for geopolitical tensions and
 *   economic disparities. The framework's structure creates both coordination
 *   benefits and extraction risks, classifying as a Tangled Rope. The balance
 *   depends on its inclusivity and enforcement mechanisms.
 *
 * KEY AGENTS:
 *   - US Hegemony: Primary beneficiary (institutional/arbitrage) - architect of the framework.
 *   - Aligned Chipmakers: Secondary beneficiary (powerful/constrained) - benefits from market stability but constrained by regulation.
 *   - Non-Aligned Nations: Primary victim (powerless/trapped) - excluded from the framework and potentially disadvantaged.
 *   - Domestic Innovation: Secondary victim (moderate/constrained) - constrained by framework rules but benefits from overall stability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paxsilica_framework, 0.5).
domain_priors:suppression_score(paxsilica_framework, 0.4).
domain_priors:theater_ratio(paxsilica_framework, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paxsilica_framework, extractiveness, 0.5).
narrative_ontology:constraint_metric(paxsilica_framework, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(paxsilica_framework, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paxsilica_framework, tangled_rope).
narrative_ontology:human_readable(paxsilica_framework, "PaxSilica AI and Silicon Governance Framework").
narrative_ontology:topic_domain(paxsilica_framework, "geopolitical/technological").

domain_priors:requires_active_enforcement(paxsilica_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paxsilica_framework, us_hegemony).
narrative_ontology:constraint_beneficiary(paxsilica_framework, aligned_chipmakers).
narrative_ontology:constraint_victim(paxsilica_framework, non_aligned_nations).
narrative_ontology:constraint_victim(paxsilica_framework, domestic_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of nations excluded from the PaxSilica framework. They are trapped by the global silicon supply chain and AI development ecosystem, facing restricted access and limited alternatives. Experienced extractiveness is high due to dependency and lack of agency.
constraint_indexing:constraint_classification(paxsilica_framework, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective of smaller AI companies and researchers within nations aligned with the PaxSilica framework. They benefit from the overall stability and standardization but are constrained by the framework's specific rules and potential biases toward larger players. They have limited mobility due to funding and infrastructure dependencies.
constraint_indexing:constraint_classification(paxsilica_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of the United States, the architect and primary beneficiary of the PaxSilica framework. It experiences the framework as a tool for coordinating international policy and maintaining its technological leadership. The exit option is arbitrage because the US can potentially leverage its influence to circumvent the framework if needed. Coordination benefit outweighs extraction.
constraint_indexing:constraint_classification(paxsilica_framework, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective of large chip manufacturers aligned with the US. They benefit from a stable and predictable market but are also constrained by regulations and potential geopolitical risks. They benefit from coordination but also face extraction through regulatory oversight.
constraint_indexing:constraint_classification(paxsilica_framework, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Analytical observer's perspective: sees the PaxSilica framework as a mixed coordination and extraction mechanism. It aims to promote responsible AI development and silicon governance, but it also risks creating a two-tiered system that benefits aligned nations at the expense of others. The classification depends on balancing coordination and extraction dynamics.
constraint_indexing:constraint_classification(paxsilica_framework, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paxsilica_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(paxsilica_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(paxsilica_framework, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(paxsilica_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paxsilica_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50): The framework extracts value from non-aligned nations by restricting their access to advanced silicon and AI technologies. Suppression (0.40): The framework suppresses alternative technological ecosystems by setting global standards and incentivizing alignment. Theater ratio (0.30): The framework exhibits some performative aspects, with symbolic gestures and political declarations potentially outweighing practical impact.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing structural positions of the key agents. The US sees the framework as a tool for coordination and maintaining its technological leadership (Rope), while non-aligned nations perceive it as a source of extraction and constraint (Snare). Domestic innovation experiences a mix of coordination and extraction, reflecting the hybrid nature of the framework (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural relationships between agents. The US, as the architect and primary beneficiary, has a low directionality value. Non-aligned nations, as victims of restricted access, have a high directionality value. The directionality for domestic innovation and aligned chipmakers reflects their mixed experiences of coordination and extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The PaxSilica framework can be classified as a Tangled Rope, reflecting its hybrid nature. It is not a pure Rope, as it involves significant extraction from non-aligned nations. It is not a pure Snare, as it also offers coordination benefits to aligned nations. The classification depends on balancing the coordination and extraction dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_effectiveness,
    'How effectively can the PaxSilica framework enforce its regulations on silicon trade and AI development?',
    'Analysis of trade data, monitoring of AI projects, and evaluation of sanctions imposed on non-compliant actors.',
    'If highly effective: the framework becomes a strong global standard, reducing risks but potentially stifling innovation. If ineffective: the framework becomes a paper tiger, with little impact on actual behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness, empirical, 'Effectiveness of PaxSilica enforcement mechanisms.').

omega_variable(
    inclusion_criteria,
    'What criteria determine which nations and companies are included in the PaxSilica framework?',
    'Examination of the framework''s official documents, analysis of diplomatic negotiations, and assessment of geopolitical factors influencing membership.',
    'If inclusive: the framework gains broader legitimacy and reduces the risk of a fragmented global order. If exclusive: the framework creates a geopolitical divide and incentivizes the development of alternative technological ecosystems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inclusion_criteria, conceptual, 'Inclusion criteria for the PaxSilica framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paxsilica_framework, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paxs_tr_t0, paxsilica_framework, theater_ratio, 0, 0.2).
narrative_ontology:measurement(paxs_tr_t5, paxsilica_framework, theater_ratio, 5, 0.3).
narrative_ontology:measurement(paxs_tr_t10, paxsilica_framework, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(paxs_be_t0, paxsilica_framework, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(paxs_be_t5, paxsilica_framework, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(paxs_be_t10, paxsilica_framework, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paxsilica_framework, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
