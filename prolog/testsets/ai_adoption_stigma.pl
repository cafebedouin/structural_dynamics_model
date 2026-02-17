% ============================================================================
% CONSTRAINT STORY: ai_adoption_stigma
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_ai_adoption_stigma, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_adoption_stigma
 *   human_readable: Workplace Stigma Against Using Generative AI
 *   domain: technological
 *
 * SUMMARY:
 *   An emergent social norm in many workplaces treats the use of generative AI
 *   for core tasks as a form of cheating, incompetence, or a threat to job
 *   security. This stigma forces many employees to use AI tools in secret,
 *   suppressing open discussion, formal training, and the development of new
 *   best practices. The constraint transfers the risk of technological
 *   adoption from the employer to the individual employee.
 *
 * KEY AGENTS (by structural relationship):
 *   - non_technical_knowledge_workers: Primary target (powerless/trapped) — Fear being replaced or seen as incompetent, forcing them to use AI secretly and bear the risks.
 *   - corporate_employers: Primary beneficiary (institutional/arbitrage) — Benefit from productivity gains of secret AI use without investing in training or policy, while retaining leverage over their workforce.
 *   - ai_tool_providers: Secondary beneficiary (powerful/mobile) - Benefit from widespread, albeit unofficial, adoption.
 *   - analytical_observer: Analytical observer — Sees the full structure of asymmetric risk transfer and suppressed coordination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_adoption_stigma, 0.52). % Extraction of psychological safety, career risk, and uncompensated skill development.
domain_priors:suppression_score(ai_adoption_stigma, 0.75).   % High suppression of open AI use due to fear of reprisal.
domain_priors:theater_ratio(ai_adoption_stigma, 0.15).       % Low theater; this is an active, coercive norm, not a vestigial one.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_adoption_stigma, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_adoption_stigma, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ai_adoption_stigma, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ai_adoption_stigma, tangled_rope).
narrative_ontology:human_readable(ai_adoption_stigma, "Workplace Stigma Against Using Generative AI").

% --- Binary flags ---
domain_priors:requires_active_enforcement(ai_adoption_stigma). % Required for Tangled Rope. The stigma is enforced by fear of job loss/demotion.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
narrative_ontology:constraint_beneficiary(ai_adoption_stigma, corporate_employers).
narrative_ontology:constraint_victim(ai_adoption_stigma, non_technical_knowledge_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (NON-TECHNICAL KNOWLEDGE WORKERS)
% From the perspective of an employee fearing replacement, the stigma is a
% coercive trap that forces them into a high-risk, low-reward situation.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
constraint_indexing:constraint_classification(ai_adoption_stigma, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (CORPORATE EMPLOYERS)
% From the employer's perspective, the situation is a coordination challenge to
% be managed. The ambiguity allows them to extract productivity gains while
% minimizing investment and retaining leverage. It's a "rope" for managing labor.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ.
constraint_indexing:constraint_classification(ai_adoption_stigma, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical view sees both the coordination function (integrating a new
% technology into the economy) AND the highly asymmetric extraction (transferring
% risk to workers). The presence of beneficiaries, victims, and active enforcement
% makes this a canonical Tangled Rope.
constraint_indexing:constraint_classification(ai_adoption_stigma, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_adoption_stigma_tests).

test(perspectival_gap_is_snare_vs_rope, [nondet]) :-
    constraint_indexing:constraint_classification(ai_adoption_stigma, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(ai_adoption_stigma, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    constraint_indexing:constraint_classification(ai_adoption_stigma, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    narrative_ontology:constraint_beneficiary(ai_adoption_stigma, _),
    narrative_ontology:constraint_victim(ai_adoption_stigma, _),
    domain_priors:requires_active_enforcement(ai_adoption_stigma).

:- end_tests(ai_adoption_stigma_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.52): This value reflects the non-monetary, but
 *     significant, extraction from workers. It includes the psychological cost
 *     of fear and secrecy, the uncompensated labor of self-training on new
 *     tools, and the career risk of being discovered or becoming obsolete.
 *   - Suppression Score (0.75): The AP News poll indicates nearly 25% of users
 *     are secret, demonstrating a powerful coercive pressure against open use.
 *     This high score reflects the effectiveness of the stigma in suppressing
 *     alternative, more collaborative norms.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the employee ('powerless'/'trapped'), the situation is
 *   a Snare: a coercive environment where they are forced to take risks to
 *   remain competitive, with the benefits accruing to their employer. For the
 *   employer ('institutional'/'arbitrage'), this is a Rope: a coordination
 *   problem of managing a new technology to maximize productivity and maintain
 *   workforce control. They benefit from the ambiguity that the employee
 *   experiences as a trap.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: 'corporate_employers' benefit by receiving productivity
 *     gains without the associated costs of training, policy development, or
 *     severance. They maintain information asymmetry and leverage.
 *   - Victim: 'non_technical_knowledge_workers' bear the costs. They face a
 *     "damned if you do, damned if you don't" dilemma: risk falling behind by
 *     not using AI, or risk being seen as a "cheater" or being made redundant
 *     if they do. This structural relationship drives the directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the phenomenon.
 *   A simplistic analysis might label it purely as "technological disruption"
 *   (a Mountain) or a simple coordination problem (a Rope). The Tangled Rope
 *   classification correctly captures that while there is a genuine need to
 *   coordinate the adoption of a new technology, the current emergent solution
 *   is deeply entangled with asymmetric extraction and risk transfer. It avoids
 *   mislabeling the coercive pressure on workers as a simple, neutral
 *   coordination challenge. It also highlights the potential for the 'powerless'
 *   group to become 'organized' if they form coalitions (e.g., unions) to
 *   negotiate AI policies, which could alter the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_ai_stigma_1,
    'Is the workplace stigma an intentional, implicit strategy by employers to maximize leverage, or is it a temporary, emergent phenomenon of cultural lag that will fade with normalization?',
    'Analysis of corporate internal communications, paired with longitudinal studies of AI adoption policies over the next 5-10 years.',
    'If intentional, this is a stable Tangled Rope. If emergent, it is a Scaffold that will eventually be replaced by formal Rope-like policies.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_adoption_stigma, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint is relatively new but has intensified rapidly.
% T=0: ~2021 (pre-ChatGPT mainstreaming)
% T=5: ~2024 (current state)
% T=10: ~2029 (projected future state, possibly with norms solidifying)

% Theater ratio over time:
narrative_ontology:measurement(ai_adoption_stigma_tr_t0, ai_adoption_stigma, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ai_adoption_stigma_tr_t5, ai_adoption_stigma, theater_ratio, 5, 0.15).
narrative_ontology:measurement(ai_adoption_stigma_tr_t10, ai_adoption_stigma, theater_ratio, 10, 0.20).

% Extraction over time: The risk and psychological burden on workers have spiked.
% It may decrease in the future if/as policies and norms are established.
narrative_ontology:measurement(ai_adoption_stigma_ex_t0, ai_adoption_stigma, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(ai_adoption_stigma_ex_t5, ai_adoption_stigma, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(ai_adoption_stigma_ex_t10, ai_adoption_stigma, base_extractiveness, 10, 0.30).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The constraint is fundamentally about establishing norms
% and standards for a new form of information work.
narrative_ontology:coordination_type(ai_adoption_stigma, information_standard).

% Network relationships: This stigma is structurally linked to broader trends
% in labor market instability and corporate investment strategies.
narrative_ontology:affects_constraint(ai_adoption_stigma, labor_market_precarity).
narrative_ontology:affects_constraint(corporate_training_austerity, ai_adoption_stigma).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The standard structural
% derivation from beneficiary/victim declarations and exit options
% accurately models the directionality of the constraint for the key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */