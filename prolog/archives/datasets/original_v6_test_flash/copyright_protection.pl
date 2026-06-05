% ============================================================================
% CONSTRAINT STORY: copyright_protection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_protection, []).

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
 *   constraint_id: copyright_protection
 *   human_readable: Copyright Protection Framework
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   Copyright is a legal framework granting creators exclusive rights to
 *   their original works for a limited time. It aims to incentivize
 *   creativity and innovation by providing creators with control over their
 *   creations and the ability to profit from them. However, copyright also
 *   creates restrictions on access to and use of copyrighted material, which
 *   can impact consumers, derivative creators, and the broader public. The
 *   effectiveness and fairness of the copyright system are subjects of
 *   ongoing debate.
 *
 * KEY AGENTS:
 *   - Copyright Holders: Primary beneficiaries (institutional/arbitrage) - Benefit from exclusive rights and potential monetization.
 *   - Creative Industries: Secondary beneficiaries (institutional/constrained) - Benefit from copyright protection but constrained by licensing complexities.
 *   - Consumers: Primary victims (powerless/trapped) - Face restrictions on access and use of copyrighted material.
 *   - Derivative Creators: Secondary victims (moderate/constrained) - Constrained by copyright but benefit from the existence of original works.
 *   - Analytical Observer: Neutral observer (analytical/analytical) - Evaluates the overall impact of copyright on society.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_protection, 0.5).
domain_priors:suppression_score(copyright_protection, 0.6).
domain_priors:theater_ratio(copyright_protection, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_protection, extractiveness, 0.5).
narrative_ontology:constraint_metric(copyright_protection, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(copyright_protection, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_protection, tangled_rope).
narrative_ontology:human_readable(copyright_protection, "Copyright Protection Framework").
narrative_ontology:topic_domain(copyright_protection, "economic/legal/technological").

domain_priors:requires_active_enforcement(copyright_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_protection, copyright_holders).
narrative_ontology:constraint_beneficiary(copyright_protection, creative_industries).
narrative_ontology:constraint_victim(copyright_protection, consumers).
narrative_ontology:constraint_victim(copyright_protection, derivative_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Consumers face limited access and higher prices due to copyright restrictions. They are trapped within the legal framework and cannot easily circumvent it.
constraint_indexing:constraint_classification(copyright_protection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Derivative creators are constrained by copyright laws but also benefit from the original works protected by copyright, enabling them to build upon existing creative content. They have limited mobility due to legal constraints but some ability to create under fair use doctrines.
constraint_indexing:constraint_classification(copyright_protection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Copyright holders benefit from the legal protections afforded by copyright, allowing them to monetize and control their creative works. They can arbitrage the system by licensing, selling, or distributing their content globally.
constraint_indexing:constraint_classification(copyright_protection, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Creative industries, such as film studios and music labels, benefit from copyright protection but are also constrained by its limitations and the need to navigate complex licensing agreements. Their mobility is constrained by global copyright laws but they still have considerable power.
constraint_indexing:constraint_classification(copyright_protection, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% From a long-term, global perspective, copyright presents a complex mix of incentives and constraints. Copyright aims to promote creativity by granting exclusive rights, but it can also stifle innovation by restricting access to existing works. It is a tangled rope that attempts to balance competing interests.
constraint_indexing:constraint_classification(copyright_protection, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_protection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(copyright_protection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(copyright_protection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_protection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_protection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50): Moderate. Copyright provides significant benefits to creators but also imposes costs on users and derivative creators. The extraction is not extreme but is clearly present. Suppression (0.60): Moderate to High. Copyright laws restrict access to copyrighted material and suppress certain types of uses, particularly unauthorized reproduction and distribution. Theater ratio (0.30): Low. While there is some degree of performative compliance (e.g., copyright notices), copyright enforcement generally requires concrete action and is not primarily theatrical.
 *
 * PERSPECTIVAL GAP:
 *   Copyright holders see copyright as a coordination mechanism (rope) that enables them to control and monetize their work. Consumers see copyright as a snare that restricts their access to and use of creative content. Derivative creators see copyright as a tangled rope that both enables and constrains their creative activities. The analytical observer recognizes that copyright is a complex system with both benefits and drawbacks.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality value is based on the agent's power and exit options. Copyright holders have institutional power and can arbitrage the system. Consumers have little power and are trapped by copyright laws. Derivative creators have moderate power and constrained exit options. Therefore, the directionality value reflects these differences.
 *
 * MANDATROPHY ANALYSIS:
 *   Copyright is a classic example of a tangled rope, balancing incentives for creation with access restrictions. It is not pure extraction because it incentivizes creation. It is not pure coordination because it limits access and creates restrictions. The different perspectives reveal the complexities of copyright and the need to balance competing interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fair_use_definition,
    'How broadly should fair use exceptions be defined to balance creators'' rights with public access and derivative works?',
    'Legal precedent, legislative action, and technological developments shaping the interpretation of fair use.',
    'Narrow definition: stronger protection for copyright holders, potentially limiting innovation. Broad definition: greater access for consumers and derivative creators, potentially reducing incentives for original creation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_definition, conceptual, 'The scope of fair use exceptions to copyright.').

omega_variable(
    enforcement_effectiveness,
    'How effective are current copyright enforcement mechanisms in preventing infringement, considering the costs and impacts on privacy and free expression?',
    'Empirical studies on the deterrent effects of enforcement, analysis of costs to society, and evaluation of impacts on legitimate uses of copyrighted material.',
    'Highly effective enforcement: reduced infringement but potential chilling effect on legal uses and increased compliance costs. Ineffective enforcement: widespread infringement, reduced incentives for creation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness, empirical, 'Effectiveness of copyright enforcement mechanisms.').

omega_variable(
    term_length_optimization,
    'What is the optimal copyright term length that balances incentives for creation with the public domain''s ability to foster new works?',
    'Economic modeling of the relationship between term length and creative output, historical analysis of the impact of copyright expiration on cultural production, and stakeholder input.',
    'Long term: stronger incentives for creators but delayed access for the public and potential rent-seeking. Short term: quicker public access but potentially reduced incentives for investment in creative projects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(term_length_optimization, preference, 'The optimal length of copyright terms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_protection, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_protection, theater_ratio, 0, 0.2).
narrative_ontology:measurement(copy_tr_t5, copyright_protection, theater_ratio, 5, 0.3).
narrative_ontology:measurement(copy_tr_t10, copyright_protection, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_protection, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(copy_be_t5, copyright_protection, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(copy_be_t10, copyright_protection, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_protection, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
