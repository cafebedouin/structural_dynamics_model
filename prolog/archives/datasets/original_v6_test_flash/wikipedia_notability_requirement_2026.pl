% ============================================================================
% CONSTRAINT STORY: wikipedia_notability_requirement_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wikipedia_notability_requirement_2026, []).

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
 *   constraint_id: wikipedia_notability_requirement_2026
 *   human_readable: Wikipedia Notability Requirement (2026)
 *   domain: social/technological
 *
 * SUMMARY:
 *   The Wikipedia notability requirement is a gatekeeping policy that
 *   determines which subjects merit a dedicated article. While intended to
 *   maintain quality and prevent spam, it can also create barriers for niche
 *   topics and emerging communities. The requirement reflects a structural
 *   tension between the desire for comprehensive coverage and the need for
 *   editorial control.
 *
 * KEY AGENTS:
 *   - Wikipedia Core Editors: Primary beneficiary (institutional/arbitrage) - benefit from reduced workload and improved content quality.
 *   - Wikipedia Readers: Secondary beneficiary (powerful/arbitrage) - benefit from increased signal-to-noise ratio.
 *   - Niche Topic Advocates: Primary victim (powerless/trapped) - lack the resources to generate sufficient secondary source coverage.
 *   - Emerging Topic Communities: Secondary victim (moderate/constrained) - face delayed or denied coverage due to lack of established notability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wikipedia_notability_requirement_2026, 0.55).
domain_priors:suppression_score(wikipedia_notability_requirement_2026, 0.7).
domain_priors:theater_ratio(wikipedia_notability_requirement_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wikipedia_notability_requirement_2026, tangled_rope).
narrative_ontology:human_readable(wikipedia_notability_requirement_2026, "Wikipedia Notability Requirement (2026)").
narrative_ontology:topic_domain(wikipedia_notability_requirement_2026, "social/technological").

domain_priors:requires_active_enforcement(wikipedia_notability_requirement_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wikipedia_notability_requirement_2026, wikipedia_core_editors).
narrative_ontology:constraint_beneficiary(wikipedia_notability_requirement_2026, wikipedia_readers).
narrative_ontology:constraint_victim(wikipedia_notability_requirement_2026, niche_topic_advocates).
narrative_ontology:constraint_victim(wikipedia_notability_requirement_2026, emerging_topic_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Niche topic advocates seeking Wikipedia coverage find themselves trapped by the notability requirement, which can be difficult to meet for specialized or emerging areas. They often lack the resources or connections to generate the necessary secondary source coverage. This perspective represents a high degree of extraction and suppression.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Emerging topic communities may benefit from the existence of Wikipedia as a platform but are constrained by the notability requirement, which favors established subjects. While they may eventually gain coverage, the initial hurdle can be significant, leading to a tangled rope scenario. There is a degree of both extraction (delayed or denied coverage) and coordination (access to the platform itself).
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Wikipedia core editors benefit from the notability requirement as it helps maintain the quality and reliability of the encyclopedia. It reduces the burden of dealing with poorly sourced or promotional content. This can be seen as a coordination mechanism, ensuring that the platform remains a valuable resource. While there are costs associated with enforcement, the benefits outweigh them for the core editors.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% Wikipedia readers benefit from the notability requirement as it helps them find information about established and relevant subjects. The encyclopedia's focus on notability increases the signal-to-noise ratio, improving the overall user experience.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% An analytical observer recognizes the notability requirement as a tangled rope. It serves a coordination function by maintaining quality and preventing spam, but it also extracts from niche topics and emerging communities by limiting their representation. The balance between these two aspects determines the overall classification.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wikipedia_notability_requirement_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wikipedia_notability_requirement_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wikipedia_notability_requirement_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The notability requirement restricts the inclusion of niche topics and emerging communities, but it also prevents the encyclopedia from being overrun with spam and promotional content. The extractiveness is not maximal, as some pathways to inclusion exist (e.g., significant event coverage). Suppression (0.70): High. The notability requirement creates a significant barrier to entry for non-notable topics. Lack of secondary sources effectively suppresses their representation on Wikipedia. Theater ratio (0.30): Low. While there are performative aspects to the notability process (e.g., reliance on certain types of sources), the requirement is primarily functional in maintaining quality control.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap stems from the different experiences of those who benefit from and those who are constrained by the notability requirement. Core editors and readers experience it as a coordination mechanism (rope), while niche topic advocates and emerging communities experience it as a constraint (snare or tangled rope). The analytical observer recognizes that it is both, hence the tangled rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values reflect the structural positions of the different agents. Core editors and readers benefit from the notability requirement, giving them low d values. Niche topic advocates and emerging communities are constrained by it, giving them higher d values. The analytical observer's d value reflects a balanced view of the costs and benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification resolves the mandatrophy by acknowledging that the notability requirement serves both coordination and extraction functions. It is not simply a beneficial policy (rope) or a purely restrictive one (snare), but a hybrid that reflects the complex realities of managing a large, open-source encyclopedia. The different perspectives highlight the trade-offs involved in balancing quality control with broader representation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_inclusion_criteria,
    'Are there alternative inclusion criteria that could balance quality control with broader representation of niche and emerging topics?',
    'Experimentation with alternative notability guidelines, community feedback, analysis of content quality and user engagement',
    'If effective, could shift the classification from tangled rope to rope or scaffold. If ineffective, could degrade the overall quality of Wikipedia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_inclusion_criteria, empirical, 'Exploration of alternative inclusion policies').

omega_variable(
    decentralized_knowledge_platforms,
    'Will decentralized knowledge platforms emerge to challenge Wikipedia''s dominance, providing alternative spaces for niche and emerging topics?',
    'Monitoring the development and adoption of decentralized knowledge platforms, analyzing their content quality and community engagement',
    'If successful, could reduce Wikipedia''s extraction by providing alternative exit options for niche topic advocates. If unsuccessful, Wikipedia''s dominance will continue, and extraction will remain high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_knowledge_platforms, conceptual, 'Emergence of decentralized knowledge platforms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wikipedia_notability_requirement_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wiki_tr_t0, wikipedia_notability_requirement_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(wiki_tr_t5, wikipedia_notability_requirement_2026, theater_ratio, 5, 0.25).
narrative_ontology:measurement(wiki_tr_t10, wikipedia_notability_requirement_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(wiki_be_t0, wikipedia_notability_requirement_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(wiki_be_t5, wikipedia_notability_requirement_2026, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(wiki_be_t10, wikipedia_notability_requirement_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wikipedia_notability_requirement_2026, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
