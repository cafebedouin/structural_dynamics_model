% ============================================================================
% CONSTRAINT STORY: abstraction_leakage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abstraction_leakage, []).

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
 *   constraint_id: abstraction_leakage
 *   human_readable: The Law of Leaky Abstractions
 *   domain: technological/cognitive
 *
 * SUMMARY:
 *   The Law of Leaky Abstractions states that all non-trivial abstractions,
 *   to some degree, fail to hide the underlying details of the system they
 *   are meant to simplify. This creates situations where users must
 *   understand those underlying details to use the abstraction effectively,
 *   negating its intended benefits. This constraint represents the inherent
 *   tension between simplification and complete encapsulation in complex
 *   systems, and the consequences when the abstraction fails.
 *
 * KEY AGENTS:
 *   - Abstraction Creators: Benefit from adoption, but are not always available to resolve leaks (institutional/arbitrage)
 *   - Naive Users: Bear the cost of leaks, often lack the knowledge to fix them (powerless/trapped)
 *   - Downstream Developers: Constrained by leaks, must spend time working around them (moderate/constrained)
 *   - Early Adopters: Able to work around leaks, often provide feedback (powerful/mobile)
 *   - Analytical Observer: Sees the systemic nature of the problem (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abstraction_leakage, 0.6).
domain_priors:suppression_score(abstraction_leakage, 0.7).
domain_priors:theater_ratio(abstraction_leakage, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abstraction_leakage, extractiveness, 0.6).
narrative_ontology:constraint_metric(abstraction_leakage, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(abstraction_leakage, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abstraction_leakage, tangled_rope).
narrative_ontology:human_readable(abstraction_leakage, "The Law of Leaky Abstractions").
narrative_ontology:topic_domain(abstraction_leakage, "technological/cognitive").

domain_priors:requires_active_enforcement(abstraction_leakage).
narrative_ontology:has_sunset_clause(abstraction_leakage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abstraction_leakage, abstraction_creators).
narrative_ontology:constraint_beneficiary(abstraction_leakage, early_adopters).
narrative_ontology:constraint_victim(abstraction_leakage, naive_users).
narrative_ontology:constraint_victim(abstraction_leakage, downstream_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of the user who trusts the abstraction completely and is caught off guard when it fails, leading to confusion and errors. They are trapped because they lack the deeper understanding to debug effectively.
constraint_indexing:constraint_classification(abstraction_leakage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The developer building upon the leaky abstraction. They benefit from the initial simplification but are constrained by having to understand and work around the leaks. They experience a mix of coordination and extraction, as they must actively manage the abstraction's failures while still relying on its basic functionality.
constraint_indexing:constraint_classification(abstraction_leakage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The creator of the abstraction, who initially benefits from its creation through increased adoption and perceived utility. They have arbitrage because they can always shift to another project or abstraction. From their perspective, it's a form of coordination that enables others to use their work.
constraint_indexing:constraint_classification(abstraction_leakage, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Early adopters who understand the underlying system and can work around the leaks. They can 'move' to another technology if the abstraction becomes too problematic. This represents a temporary coordination as they are essentially beta testers and eventually the abstraction may be improved or abandoned.
constraint_indexing:constraint_classification(abstraction_leakage, scaffold,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% Analytical perspective recognizing the inherent trade-off between simplicity and fidelity in abstractions. Sees the long-term cycle of abstraction, leakage, and reinvention as a tangled rope.
constraint_indexing:constraint_classification(abstraction_leakage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abstraction_leakage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(abstraction_leakage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(abstraction_leakage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(abstraction_leakage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abstraction_leakage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): Moderate. The abstraction 'extracts' cognitive resources from users who must understand the underlying system to use it correctly. Suppression (0.70): Significant suppression as the abstraction encourages users to ignore underlying details, making leaks more surprising and difficult to handle when they occur. Theater ratio (0.30): Relatively low, the focus is generally on improving usability and correctness rather than performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing levels of understanding and engagement with the underlying system. The abstraction creator sees their work as beneficial coordination, while the naive user experiences frustration and confusion due to unexpected leaks. The downstream developer understands both the benefits and limitations, and the early adopter actively mitigates the leaks.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by how much each agent benefits or suffers from the abstraction. Creators benefit from its adoption, while naive users suffer when it leaks. Developers have a mixed experience, and early adopters benefit from their deeper understanding. The analytical observer's experience is neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is a tangled rope because there is both a coordination function (simplification and ease of use for many users), and an extraction mechanism (requiring some users to understand the underlying complexities to a degree that nullifies the simplicity that the abstraction was supposed to provide). The law of leaky abstractions demonstrates the limits of pure coordination in complex systems because over-abstraction without concern for transparency will always create a snare for a set of users.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abstraction_complexity_threshold,
    'At what level of underlying system complexity does the ''law'' become unavoidable?',
    'Empirical study of different abstraction layers and the incidence of ''leaks''. Correlation between system complexity and leak frequency.',
    'Determines whether more complex systems are fundamentally more vulnerable to the law, influencing abstraction strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abstraction_complexity_threshold, empirical, 'Identifies the complexity threshold where abstractions inevitably leak.').

omega_variable(
    developer_skill_variability,
    'How does the skill level of developers using an abstraction affect the severity of the consequences of leaks?',
    'Controlled experiments where developers of varying skill levels use a leaky abstraction to complete tasks.',
    'If high skill mitigates leaks: abstractions can be targeted towards experienced users. If skill has little effect: leaks pose a broad risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(developer_skill_variability, empirical, 'Determines the role of user skill in mitigating abstraction leakage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abstraction_leakage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abst_tr_t0, abstraction_leakage, theater_ratio, 0, 0.1).
narrative_ontology:measurement(abst_tr_t5, abstraction_leakage, theater_ratio, 5, 0.2).
narrative_ontology:measurement(abst_tr_t10, abstraction_leakage, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(abst_be_t0, abstraction_leakage, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(abst_be_t5, abstraction_leakage, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(abst_be_t10, abstraction_leakage, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abstraction_leakage, information_standard).
narrative_ontology:affects_constraint(abstraction_leakage, technical_debt).
narrative_ontology:affects_constraint(abstraction_leakage, feature_creep).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
