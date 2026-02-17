% ============================================================================
% CONSTRAINT STORY: advice_as_dangerous_gift
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_advice_as_dangerous_gift, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: advice_as_dangerous_gift
 *   human_readable: The Hazard of Counsel
 *   domain: social/philosophical
 *
 * SUMMARY:
 *   From J.R.R. Tolkien, "The Fellowship of the Ring": "Advice is a dangerous
 *   gift, even from the wise to the wise, and all courses may run ill." This
 *   constraint models the social dynamic where giving advice creates a systemic
 *   risk. It attempts to coordinate action (a benefit) but also transfers
 *   responsibility to the giver and potentially extracts autonomy from the
 *   receiver, especially if the advice leads to a poor outcome.
 *
 * KEY AGENTS (by structural relationship):
 *   - Advice Seekers (e.g., Frodo Baggins): Primary target (powerless/trapped) — bears the risk of following bad advice.
 *   - Advice Givers (e.g., Gildor, Elrond): Primary beneficiary (institutional/arbitrage) — benefits from coordinating action, but bears the risk of responsibility.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(advice_as_dangerous_gift, 0.35).
domain_priors:suppression_score(advice_as_dangerous_gift, 0.40).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(advice_as_dangerous_gift, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(advice_as_dangerous_gift, extractiveness, 0.35).
narrative_ontology:constraint_metric(advice_as_dangerous_gift, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(advice_as_dangerous_gift, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(advice_as_dangerous_gift, tangled_rope).
narrative_ontology:human_readable(advice_as_dangerous_gift, "The Hazard of Counsel").

% --- Binary flags ---
domain_priors:requires_active_enforcement(advice_as_dangerous_gift). % Enforced by social norms of responsibility/blame.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(advice_as_dangerous_gift, advice_givers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(advice_as_dangerous_gift, advice_seekers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE ADVICE SEEKER (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% For Frodo, trapped by circumstance, bad advice is a snare; it forecloses
% better paths and locks him into a course of action he did not fully author.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE INSTITUTIONAL ADVISOR (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% For Elrond's Council, advice is a 'Rope'—a crucial tool to coordinate the
% actions of disparate groups against a common threat. The extractive risk is
% managed and accepted as a cost of coordination.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% Sees both the coordination function and the asymmetric extraction of risk.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(advice_as_dangerous_gift_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(advice_as_dangerous_gift, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(advice_as_dangerous_gift, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == snare,
    TypeBeneficiary == rope.

test(analytical_claim_consistency) :-
    narrative_ontology:constraint_claim(advice_as_dangerous_gift, ClaimedType),
    constraint_indexing:constraint_classification(advice_as_dangerous_gift, AnalyticalType, context(agent_power(analytical), _, _, _)),
    ClaimedType == AnalyticalType.

:- end_tests(advice_as_dangerous_gift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.35) and suppression (0.40) are set to meet
 *   the Tangled Rope thresholds. The extraction isn't direct value, but the
 *   transfer of risk and the potential loss of autonomy for the receiver.
 *   Suppression reflects the social pressure to offer/accept counsel, which
 *   can crowd out the alternative of independent discovery. The constraint is
 *   enforced by social norms of responsibility and blame.
 *
 * PERSPECTIVAL GAP:
 *   The advice seeker (Frodo), trapped and facing immediate danger, perceives
 *   the constraint as a Snare. A wrong move based on external advice could be
 *   fatal. The institutional advisor (Elrond's Council), operating at a
 *   strategic level with multiple options, sees it as a Rope—a necessary tool
 *   for coordination, where the risks are manageable parts of a larger strategy.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `advice_givers` (e.g., Elrond) benefit by achieving coordination and influencing outcomes.
 *   - Victims: `advice_seekers` (e.g., Frodo) bear the direct cost if the advice "runs ill," having outsourced a portion of their agency.
 *   This asymmetric risk profile is the core of the Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies that advice is not pure coordination
 *   (Rope) nor pure extraction (Snare). It has a genuine coordination function
 *   but carries an inherent, asymmetric extractive potential based on the outcome
 *   and the power dynamics between giver and receiver. The framework captures
 *   this duality, preventing mislabeling it as a simple Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_advice_as_dangerous_gift,
    'Is the "danger" in advice inherent to the act, or is it purely a function of the external circumstances (i.e., the baseline risk of the situation)?',
    'Comparative analysis of outcomes where the same advice is given in low-risk vs. high-risk environments.',
    'If inherent, it is a Tangled Rope. If purely circumstantial, the advice itself is a Rope, and the circumstance is a separate Snare or Mountain.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_advice_as_dangerous_gift, conceptual, 'Distinguishing inherent risk of advice from circumstantial risk.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(advice_as_dangerous_gift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this social constraint is modeled as stable, as the
% dynamic is considered a persistent feature of human/social interaction.
%
% Theater ratio over time (stable):
narrative_ontology:measurement(advice_tr_t0, advice_as_dangerous_gift, theater_ratio, 0, 0.15).
narrative_ontology:measurement(advice_tr_t5, advice_as_dangerous_gift, theater_ratio, 5, 0.15).
narrative_ontology:measurement(advice_tr_t10, advice_as_dangerous_gift, theater_ratio, 10, 0.15).

% Extraction over time (stable):
narrative_ontology:measurement(advice_ex_t0, advice_as_dangerous_gift, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(advice_ex_t5, advice_as_dangerous_gift, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(advice_ex_t10, advice_as_dangerous_gift, base_extractiveness, 10, 0.35).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Advice is a fundamental mechanism for sharing information to coordinate action.
narrative_ontology:coordination_type(advice_as_dangerous_gift, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the directionality of this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */