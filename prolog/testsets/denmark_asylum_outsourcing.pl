% ============================================================================
% CONSTRAINT STORY: denmark_asylum_outsourcing
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_denmark_asylum_outsourcing, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: denmark_asylum_outsourcing
 *   human_readable: "Denmark's policy of outsourcing asylum processing to non-EU countries"
 *   domain: political
 *
 * SUMMARY:
 *   This constraint represents the Danish government's policy to process asylum
 *   claims of individuals arriving in Denmark in third countries outside the EU.
 *   The policy is presented as a mechanism to deter irregular migration and
 *   "break the business model of human traffickers," but it functions by
 *   extracting legal rights and certainty from asylum seekers while challenging
 *   the EU's common asylum framework.
 *
 * KEY AGENTS (by structural relationship):
 *   - Asylum Seekers: Primary target (powerless/trapped) — bear the extraction of legal rights and due process.
 *   - Danish Government: Primary beneficiary (institutional/arbitrage) — gains political capital and perceived control over migration.
 *   - European Union: Secondary institutional actor (institutional/constrained) — its common legal framework is undermined, making it a victim of the policy's precedent.
 *   - Analytical Observer: Analytical observer — sees the dual function of claimed coordination and actual extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(denmark_asylum_outsourcing, 0.75).
domain_priors:suppression_score(denmark_asylum_outsourcing, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(denmark_asylum_outsourcing, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(denmark_asylum_outsourcing, extractiveness, 0.75).
narrative_ontology:constraint_metric(denmark_asylum_outsourcing, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(denmark_asylum_outsourcing, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(denmark_asylum_outsourcing, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(denmark_asylum_outsourcing).
domain_priors:requires_active_enforcement(denmark_asylum_outsourcing). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% N/A. This is a human-constructed policy.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(denmark_asylum_outsourcing, danish_state_actors).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(denmark_asylum_outsourcing, asylum_seekers_in_denmark).
narrative_ontology:constraint_victim(denmark_asylum_outsourcing, eu_institutions).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE ASYLUM SEEKER (PRIMARY TARGET)
% Bears full extraction of rights and certainty. Engine derives:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% From this view, the "coordination" is invisible; it is pure coercion.
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE DANISH GOVERNMENT (PRIMARY BENEFICIARY)
% Perceives the policy as a tool for managing a complex problem. Engine derives:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% The policy is seen as a net benefit, a coordination mechanism (Rope).
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the claimed coordination function and the asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% The high base extraction and suppression, combined with a genuine beneficiary,
% points unambiguously to a Tangled Rope.
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% The EU is an institutional actor, but unlike the Danish government, it is a
% victim of this policy, which undermines the common asylum framework.
% Its exit is constrained, as it cannot easily expel Denmark or ignore the precedent.
% The derived `d` is higher than the beneficiary's, revealing the tension.
% victim membership + institutional power + constrained exit → intermediate d → intermediate χ
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(denmark_asylum_outsourcing_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    constraint_indexing:constraint_classification(denmark_asylum_outsourcing, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(denmark_asylum_outsourcing, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(perspectival_gap_beneficiary_vs_peer) :-
    constraint_indexing:constraint_classification(denmark_asylum_outsourcing, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(denmark_asylum_outsourcing, tangled_rope, context(agent_power(institutional), _, exit_options(constrained), _)).

test(analytical_claim_matches_type) :-
    constraint_indexing:constraint_classification(denmark_asylum_outsourcing, Type, context(agent_power(analytical), _, _, _)),
    narrative_ontology:constraint_claim(denmark_asylum_outsourcing, Type).

test(tangled_rope_gate_validation) :-
    narrative_ontology:constraint_beneficiary(denmark_asylum_outsourcing, _),
    narrative_ontology:constraint_victim(denmark_asylum_outsourcing, _),
    domain_priors:requires_active_enforcement(denmark_asylum_outsourcing).

:- end_tests(denmark_asylum_outsourcing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): Extremely high. The policy extracts fundamental
 *     legal rights—the right to have an asylum claim heard in the territory of
 *     arrival, access to specific legal standards, and the potential for
 *     integration. The cost to the individual is profound uncertainty and risk.
 *   - Suppression (S=0.80): High. The policy's primary function is to suppress
 *     the default alternative: the standard EU asylum process. For an individual
 *     arriving in Denmark, there is no other path offered.
 *   - The combination of a real coordination goal (deterrence, political signaling)
 *     for a beneficiary (the state) and severe, coercive extraction from a
 *     target (asylum seekers) makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. The Danish government, as the architect and beneficiary,
 *   views the policy as a Rope—an instrument of sovereign control and a solution
 *   to the "problem" of irregular migration. Their `d` is low, so `χ` is negative;
 *   it's a net gain. For the asylum seeker, who is trapped and has their rights
 *   stripped, the policy is a Snare. Their `d` is high, so `χ` is maximized; it's
 *   pure extraction with no discernible coordinating benefit for them.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `danish_state_actors` benefit directly through political gains
 *     and the externalization of costs associated with asylum processing.
 *   - Victim: `asylum_seekers_in_denmark` bear the direct costs. The `eu_institutions`
 *     are a secondary victim, as the policy erodes the common legal framework they
 *     are supposed to uphold. This dual-victim structure is key to understanding
 *     the constraint's systemic impact.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The dynamic between Denmark and the EU is critical. Both are `institutional`
 *   actors, but their relationship to the constraint is inverted. Denmark has
 *   `arbitrage` exit (it designed and can rescind the policy). The EU has
 *   `constrained` exit (it cannot easily force compliance and is bound by treaties).
 *   This difference in exit options, combined with their opposing victim/beneficiary
 *   roles, correctly generates different `d` values and thus different classifications,
 *   capturing the political tension within the EU framework.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This classification correctly avoids two errors. It is not a pure Snare,
 *   because that would ignore the genuine (from the state's perspective) political
 *   and policy coordination goals that motivate its existence. It is not a pure
 *   Rope, because that would whitewash the immense, non-consensual extraction
 *   imposed on a vulnerable group. Tangled Rope correctly identifies it as a
 *   system that uses the language of coordination to justify coercive extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_denmark_asylum_outsourcing,
    'Does the policy genuinely "break the business model of human traffickers" or merely displace trafficking routes and increase risks?',
    'Empirical studies on migration patterns and trafficking economics pre- and post-policy implementation in a control group of countries.',
    'If it genuinely breaks the model, the coordination function is stronger, though the extraction remains. If it merely displaces routes, the coordination claim is largely theater, pushing the constraint closer to a pure Snare.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
% The reporting engine reads narrative_ontology:omega_variable/3 with structure
% (ID, TypeClass, Description) where TypeClass is one of:
%   empirical   — resolvable by gathering more data
%   conceptual  — depends on definitional or theoretical framing
%   preference  — depends on value judgments or policy choices
% The /3 form is what the engine reads; /5 provides narrative context.
narrative_ontology:omega_variable(omega_denmark_asylum_outsourcing, empirical, 'Whether the policy deters human trafficking or merely displaces it.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(denmark_asylum_outsourcing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This policy represents an intensification of a harder line on immigration
% that has developed over the last decade. ε has risen as policies moved
% from rhetorical to concrete and extractive.

% Theater ratio over time (stable):
narrative_ontology:measurement(dao_tr_t0, denmark_asylum_outsourcing, theater_ratio, 0, 0.30).
narrative_ontology:measurement(dao_tr_t5, denmark_asylum_outsourcing, theater_ratio, 5, 0.35).
narrative_ontology:measurement(dao_tr_t10, denmark_asylum_outsourcing, theater_ratio, 10, 0.40).

% Extraction over time (intensifying):
narrative_ontology:measurement(dao_ex_t0, denmark_asylum_outsourcing, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(dao_ex_t5, denmark_asylum_outsourcing, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(dao_ex_t10, denmark_asylum_outsourcing, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The policy is framed as a way to enforce a particular migration outcome.
narrative_ontology:coordination_type(denmark_asylum_outsourcing, enforcement_mechanism).

% Network relationships (structural influence edges)
% This policy directly challenges and therefore affects the EU's common system.
narrative_ontology:affects_constraint(denmark_asylum_outsourcing, eu_common_asylum_system).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately captures the
% power dynamics between the Danish government (beneficiary/arbitrage),
% asylum seekers (victim/trapped), and the EU (victim/constrained).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */