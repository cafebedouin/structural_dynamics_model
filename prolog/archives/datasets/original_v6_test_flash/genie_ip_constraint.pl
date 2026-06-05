% ============================================================================
% CONSTRAINT STORY: genie_ip_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genie_ip_constraint, []).

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
 *   constraint_id: genie_ip_constraint
 *   human_readable: Google's Project Genie IP Concerns
 *   domain: technological
 *
 * SUMMARY:
 *   Google's Project Genie allows users to create playable game worlds from
 *   text prompts. This raises concerns about potential IP infringement and
 *   the devaluation of original creative works. The constraint is a Tangled
 *   Rope as it provides a coordination mechanism for content creation but
 *   also presents asymmetric extraction risks. Google and Project Genie users
 *   benefit from the ease of content generation, while original IP holders
 *   and independent game developers face potential harms.
 *
 * KEY AGENTS:
 *   - Google: Benefits (institutional/arbitrage) — increased user engagement and data collection
 *   - Project Genie Users: Benefits (institutional/arbitrage) — ability to create and share content without technical expertise
 *   - Original IP Holders: Victims (powerless/trapped) — potential copyright infringement of their works
 *   - Independent Game Developers: Victims (moderate/constrained) — devaluation of their original work and increased competition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genie_ip_constraint, 0.6).
domain_priors:suppression_score(genie_ip_constraint, 0.4).
domain_priors:theater_ratio(genie_ip_constraint, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genie_ip_constraint, extractiveness, 0.6).
narrative_ontology:constraint_metric(genie_ip_constraint, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(genie_ip_constraint, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genie_ip_constraint, tangled_rope).
narrative_ontology:human_readable(genie_ip_constraint, "Google's Project Genie IP Concerns").
narrative_ontology:topic_domain(genie_ip_constraint, "technological").

domain_priors:requires_active_enforcement(genie_ip_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genie_ip_constraint, google).
narrative_ontology:constraint_beneficiary(genie_ip_constraint, project_genie_users).
narrative_ontology:constraint_victim(genie_ip_constraint, original_ip_holders).
narrative_ontology:constraint_victim(genie_ip_constraint, independent_game_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Original IP Holders - Powerless to prevent unauthorized content creation using Genie, with limited legal recourse due to the difficulty of proving direct copyright infringement in AI-generated content. Trapped by the scale and automation of the platform.
constraint_indexing:constraint_classification(genie_ip_constraint, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Independent Game Developers - Constrained by Google's dominance in the game development market and the potential for AI-generated content to devalue their original work. Benefits from the accessibility and ease of use provided by Genie, but also suffers from increased competition and potential IP infringement.
constraint_indexing:constraint_classification(genie_ip_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 3: Google - Benefits from the increased user engagement and data collection enabled by Project Genie. Experiences the constraint as coordination, providing users with a platform to create and share content, but also faces legal and ethical challenges related to IP infringement.
constraint_indexing:constraint_classification(genie_ip_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: Project Genie Users - Benefits from the ability to create and share game worlds without requiring technical expertise or significant resources. Experiences the constraint as coordination, enabling them to express their creativity and connect with others.
constraint_indexing:constraint_classification(genie_ip_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 5: Analytical Observer - Sees the mixed coordination and extraction present in this technology. Facilitates content creation and innovation, but also raises significant IP concerns and could undermine the value of original creative work.
constraint_indexing:constraint_classification(genie_ip_constraint, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genie_ip_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(genie_ip_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genie_ip_constraint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(genie_ip_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(genie_ip_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): Original IP holders may lose control over how their content is used, and indy developers may see their market diminished. Suppression (0.40): IP holders and developers face significant hurdles in preventing infringement, including the scale and automation of Genie's content creation and difficulty in proving direct copyright infringement. Theater ratio (0.20): The low theater score reflects the relatively low level of performative activity associated with this technology, however the risk of IP infringement remains significant.
 *
 * PERSPECTIVAL GAP:
 *   Google views Project Genie as a coordination tool, enabling users to create content. Original IP holders see it as a snare, with their IP being used without permission. Independent game developers see a tangled rope – the barrier to entry is lower but the protection for their work is weakened.
 *
 * DIRECTIONALITY LOGIC:
 *   Google and Project Genie users have low directionality, as they are beneficiaries of the constraint. Original IP holders have high directionality, as their IP is potentially being infringed. Independent game developers have moderate directionality, as they face increased competition and the potential for IP infringement.
 *
 * MANDATROPHY ANALYSIS:
 *   The analysis reveals that different actors experience Project Genie differently based on their position relative to the technology and the existing IP framework. Google benefits from the platform's coordination function, while IP holders are potentially harmed by its extractive nature. The framework's ability to classify this situation from multiple perspectives demonstrates its effectiveness in resolving the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ip_infringement_threshold,
    'What level of similarity between AI-generated content and original works constitutes copyright infringement?',
    'Legal precedent and expert testimony on the detectability of infringement.',
    'Determines the legal liability of Google and Project Genie users for IP infringement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ip_infringement_threshold, conceptual, 'Threshold for determining IP infringement by AI-generated content').

omega_variable(
    content_filtering_effectiveness,
    'How effective are Google''s content filtering mechanisms at preventing the creation and distribution of infringing content?',
    'Audits and testing of the filtering system.',
    'Determines the level of risk of IP infringement on the platform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(content_filtering_effectiveness, empirical, 'The ability of content filters to catch IP violations.').

omega_variable(
    business_model_viability,
    'Can Google create a viable business model for Project Genie that fairly compensates IP holders and incentivizes original content creation?',
    'Economic analysis and user testing of different monetization models.',
    'Determines the long-term sustainability and fairness of the platform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(business_model_viability, preference, 'Whether it''s possible to build a sustainable business around generative AI that both respects and enables human creativity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genie_ip_constraint, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geni_tr_t0, genie_ip_constraint, theater_ratio, 0, 0.1).
narrative_ontology:measurement(geni_tr_t1, genie_ip_constraint, theater_ratio, 1, 0.15).
narrative_ontology:measurement(geni_tr_t2, genie_ip_constraint, theater_ratio, 2, 0.2).

% Extraction over time
narrative_ontology:measurement(geni_be_t0, genie_ip_constraint, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(geni_be_t1, genie_ip_constraint, base_extractiveness, 1, 0.5).
narrative_ontology:measurement(geni_be_t2, genie_ip_constraint, base_extractiveness, 2, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genie_ip_constraint, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
