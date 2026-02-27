% ============================================================================
% CONSTRAINT STORY: amish_technological_renunciation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_amish_technological_renunciation, []).

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
 *   constraint_id: amish_technological_renunciation
 *   human_readable: The Television Test (Amish Renunciation)
 *   domain: social/technological/religious
 *
 * SUMMARY:
 *   The Amish renunciation of technology serves as a deliberate constraint
 *   aimed at preserving community values and identity. This constraint
 *   involves a tension between collective adherence and individual desires,
 *   as well as the impact on external technology producers. The 'television
 *   test' symbolizes the broader principle of selectively adopting
 *   technologies deemed compatible with Amish life. The community acts as a
 *   coordinator, while individual members may feel trapped and technology
 *   producers face a limited market.
 *
 * KEY AGENTS:
 *   - Amish Community: Primary beneficiary (moderate/constrained) - Collective action to preserve cultural identity.
 *   - Individual Amish Members: Primary target (powerless/trapped) - Subject to the renunciation rules.
 *   - Technology Producers: Secondary target (moderate/constrained) - Limited market reach.
 *   - Analytical Observer: Observational role (analytical/analytical) - Analyzes the constraint's structure and impact.
 *   - Amish Leadership: Institutional actor (institutional/constrained) - Enforces technological renunciation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(amish_technological_renunciation, 0.6).
domain_priors:suppression_score(amish_technological_renunciation, 0.7).
domain_priors:theater_ratio(amish_technological_renunciation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(amish_technological_renunciation, extractiveness, 0.6).
narrative_ontology:constraint_metric(amish_technological_renunciation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(amish_technological_renunciation, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(amish_technological_renunciation, tangled_rope).
narrative_ontology:human_readable(amish_technological_renunciation, "The Television Test (Amish Renunciation)").
narrative_ontology:topic_domain(amish_technological_renunciation, "social/technological/religious").

domain_priors:requires_active_enforcement(amish_technological_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(amish_technological_renunciation, amish_community).
narrative_ontology:constraint_victim(amish_technological_renunciation, individual_amish_members).
narrative_ontology:constraint_victim(amish_technological_renunciation, technology_producers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual Amish members may feel trapped by the renunciation of technology, especially if they desire access to information or entertainment that is common in the outside world. The enforcement of these rules can feel highly extractive.
constraint_indexing:constraint_classification(amish_technological_renunciation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The Amish community benefits from the renunciation of technology through the preservation of their unique culture and values. However, this also requires active enforcement and suppression of alternative options, making it a Tangled Rope.
constraint_indexing:constraint_classification(amish_technological_renunciation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Technology producers are negatively affected by the Amish renunciation of technology, as it limits their potential market reach within Amish communities. They are constrained because they cannot easily alter their products to conform to Amish values, as the community actively filters out technology it deems harmful.
constraint_indexing:constraint_classification(amish_technological_renunciation, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, the Amish renunciation of technology is a complex constraint with both coordination and extraction aspects. It's a strategy to preserve a unique cultural identity in the face of globalization, but it also involves suppressing individual freedoms and limiting access to certain technologies.
constraint_indexing:constraint_classification(amish_technological_renunciation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Amish leadership views the technological renunciation as a necessary coordination mechanism to preserve their cultural identity and community cohesion. They see it as a way to protect their members from the perceived negative influences of modern technology.
constraint_indexing:constraint_classification(amish_technological_renunciation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(amish_technological_renunciation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(amish_technological_renunciation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(amish_technological_renunciation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(amish_technological_renunciation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(amish_technological_renunciation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The renunciation of technology has a significant impact on individual Amish members who desire access to modern conveniences and information. This creates a real cost in terms of foregone opportunities and personal satisfaction. Suppression (0.70): High. The Amish community actively enforces technological renunciation through social pressure, religious teachings, and formal rules. This creates a strong barrier to adopting forbidden technologies. Theater Ratio (0.30): Low. While there is some performative aspect to demonstrating adherence to Amish values, the renunciation is primarily functional in preserving community separation and cultural distinctiveness.
 *
 * PERSPECTIVAL GAP:
 *   The individual Amish member experiences the constraint as a Snare, feeling trapped by the community's restrictions. The Amish community views it as a Tangled Rope, balancing the benefits of preserving their unique culture with the costs of suppressing individual freedoms. The technology producers experience it as a Snare, because the community actively prevents product adoption. The analytical observer sees it as a Tangled Rope reflecting both coordination and extraction dynamics. Amish leadership sees it as a rope, a necessary coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the beneficiary/victim relationship. The Amish community benefits from the preservation of their unique culture, leading to a low 'd' value from their perspective. Individual Amish members bear the cost of technological renunciation, resulting in a high 'd' value from their perspective. Technology producers face a limited market and thus also experience a high 'd' value. Amish leadership benefits from community cohesion.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    community_vs_individual,
    'How does the balance between community integrity and individual freedom influence the enforcement of technological renunciation?',
    'Sociological studies on individual well-being within the Amish community versus those who leave. Analysis of internal debates within the community regarding technology adoption.',
    'If community integrity is prioritized: stricter enforcement, Snare classification more prominent. If individual freedom is emphasized: more flexibility, Rope classification more likely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_vs_individual, empirical, 'The trade-off between community integrity and individual freedom in technology renunciation.').

omega_variable(
    technological_definition,
    'What criteria are used to determine which technologies are harmful and should be renounced?',
    'Historical analysis of evolving technological standards within Amish communities. Comparison of renunciation practices across different Amish settlements.',
    'Broad definition: higher suppression, potential for Snare. Narrow definition: lower suppression, potential for Scaffold (temporary renunciation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_definition, conceptual, 'The definition of harmful technology and its influence on renunciation practices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(amish_technological_renunciation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amis_tr_t0, amish_technological_renunciation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(amis_tr_t50, amish_technological_renunciation, theater_ratio, 50, 0.3).
narrative_ontology:measurement(amis_tr_t100, amish_technological_renunciation, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(amis_be_t0, amish_technological_renunciation, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(amis_be_t50, amish_technological_renunciation, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(amis_be_t100, amish_technological_renunciation, base_extractiveness, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(amish_technological_renunciation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
