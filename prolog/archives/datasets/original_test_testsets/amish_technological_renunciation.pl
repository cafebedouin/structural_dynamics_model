% ============================================================================
% CONSTRAINT STORY: amish_technological_renunciation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
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
    narrative_ontology:affects_constraint/2,
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
 *   The Amish renunciation of specific technologies, exemplified by the
 *   television, is governed by the 'Ordnung,' an unwritten code of conduct
 *   that preserves community separation from the outside world. This
 *   constraint is not a blanket rejection of all technology but a selective,
 *   community-driven process to evaluate and forbid innovations seen as
 *   threatening to family life, social cohesion, and religious values. The
 *   television is a canonical example of a forbidden technology because it
 *   acts as a direct conduit for worldly values into the home, undermining
 *   communal principles.
 *
 * KEY AGENTS:
 *   - Devout Community Members: Primary beneficiaries and enforcers (organized/mobile) — see the rules as essential coordination for cultural survival.
 *   - Questioning Youth / Potential Leavers: Primary victims (powerless/trapped) — face the catastrophic social cost of shunning if they violate the Ordnung and do not repent.
 *   - Community Elders: Institutional beneficiaries (organized/mobile) — act as arbiters and maintainers of the Ordnung.
 *   - Non-Amish Neighbors: External observers (powerful/arbitrage) — interact with the community based on its predictable rules, often in mutually beneficial ways.
 *   - Analytical Observer: External analyst (analytical/analytical) — views the system's dual function of coordination and coercion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(amish_technological_renunciation, 0.55).
domain_priors:suppression_score(amish_technological_renunciation, 0.75).
domain_priors:theater_ratio(amish_technological_renunciation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(amish_technological_renunciation, extractiveness, 0.55).
narrative_ontology:constraint_metric(amish_technological_renunciation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(amish_technological_renunciation, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(amish_technological_renunciation, tangled_rope).
narrative_ontology:human_readable(amish_technological_renunciation, "The Television Test (Amish Renunciation)").
narrative_ontology:topic_domain(amish_technological_renunciation, "social/technological/religious").

domain_priors:requires_active_enforcement(amish_technological_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(amish_technological_renunciation, amish_community_elders).
narrative_ontology:constraint_beneficiary(amish_technological_renunciation, traditionalist_families).
narrative_ontology:constraint_victim(amish_technological_renunciation, individual_members_desiring_exit).
narrative_ontology:constraint_victim(amish_technological_renunciation, amish_youth_in_rumspringa).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVOUT MEMBER (ROPE) — For a fully integrated member, the rules are a pure coordination mechanism to achieve the shared goal of community preservation and spiritual purity. The 'cost' is a willing sacrifice for a greater good. They have the legal right to exit but are psychologically and socially bound to the community, which they perceive as a benefit.
constraint_indexing:constraint_classification(amish_technological_renunciation, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: QUESTIONING YOUTH (SNARE) — For an individual contemplating leaving, the constraint is a snare. The cost of exit is the complete loss of family, social structure, and identity (shunning). This makes the 'choice' to stay feel highly coercive. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.70.
constraint_indexing:constraint_classification(amish_technological_renunciation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — The analyst sees both the genuine coordination function (preserving a unique culture) and the severe, asymmetric extraction from individuals who wish to leave. It has both a Rope and a Snare component, which is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(amish_technological_renunciation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: NON-AMISH NEIGHBOR (ROPE) — A non-Amish person living nearby experiences the constraint as a set of predictable rules for social and economic interaction. They are not bound by it and can even benefit (e.g., by providing taxi services or phone access), making it a simple coordination problem from their view.
constraint_indexing:constraint_classification(amish_technological_renunciation, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: TECHNOLOGICAL DETERMINIST (MOUNTAIN) — This perspective frames technological adoption as an inevitable, natural force. The Amish renunciation is seen as a futile attempt to resist an unchangeable law of social evolution. The engine will flag this as a 'false summit' because the base properties (requires_active_enforcement: true, emerges_naturally: false) contradict the Mountain classification.
constraint_indexing:constraint_classification(amish_technological_renunciation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(amish_technological_renunciation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(amish_technological_renunciation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(amish_technological_renunciation, TypeOther, context(agent_power(organized), _, _, _)),
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
 *   Extractiveness (ε=0.55): Moderate-High. The constraint extracts individual autonomy and access to the global information commons. The cost of exit—complete social and familial ostracism—is extremely high, representing a significant extraction of social capital from any member who deviates. Suppression (0.75): High. Alternatives are actively and effectively suppressed through powerful social sanctions (shunning). There is no accepted way to be 'Amish' and own a television. Theater Ratio (0.10): Very Low. This is a deeply functional constraint, not performative. It is central to the Amish lived experience and identity.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For insiders who accept the premises (Devout Members), the constraint is a Rope—a tool for achieving a shared utopia. For those on the boundary or wishing to exit (Questioning Youth), it is a Snare—a coercive trap where the price of freedom is one's entire world. The Analytical Observer sees both functions simultaneously, classifying it as a Tangled Rope. This highlights the core DR principle that classification is indexical to the observer's structural position relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality `d` is derived from structural relationships. The Devout Member is a beneficiary with a chosen 'mobile' status, resulting in a low `d` and a Rope classification. The Questioning Youth is a victim who is 'trapped' by social costs, resulting in a very high `d` and a Snare classification. The Analytical Observer's default `d` value places the effective extraction `χ` squarely in the Tangled Rope category, reflecting the mixed nature of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by demonstrating that a single social rule can be correctly classified as both a Rope and a Snare. Attempting to assign a single 'true' classification would be a category error. The system is a coordination mechanism *for those who consent* and an extraction mechanism *for those who do not*. The Tangled Rope classification from the analytical perspective correctly captures this duality, preventing the mislabeling of a coercive system as pure coordination, or a functional community's rules as pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    choice_authenticity,
    'Is the adult choice to be baptized into the Amish church a truly free one, given that the alternative is shunning and total social alienation?',
    'Comparative analysis of life outcomes and stated well-being for those who stay versus those who leave; qualitative interviews on perceived choice.',
    'If choice is deemed authentic, the system is closer to a Rope. If deemed coercive, it is closer to a Snare. This is the central ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(choice_authenticity, conceptual, 'Whether the choice to join the church is free or coercive').

omega_variable(
    community_viability,
    'Can the Amish community maintain its economic viability and social cohesion over the next 50 years without adopting technologies currently forbidden by the Ordnung?',
    'Economic modeling of Amish enterprises; demographic projections of population density and land availability.',
    'If non-viable, the constraint may be a Piton in waiting, maintained by inertia against economic reality. If viable, its function as a Rope/Tangled Rope is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_viability, empirical, 'Long-term economic and social viability of technological separation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(amish_technological_renunciation, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amis_tr_t1950, amish_technological_renunciation, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(amis_tr_t1985, amish_technological_renunciation, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(amis_tr_t2020, amish_technological_renunciation, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(amis_be_t1950, amish_technological_renunciation, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(amis_be_t1985, amish_technological_renunciation, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(amis_be_t2020, amish_technological_renunciation, base_extractiveness, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(amish_technological_renunciation, enforcement_mechanism).
narrative_ontology:affects_constraint(amish_technological_renunciation, compulsory_education_laws).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
