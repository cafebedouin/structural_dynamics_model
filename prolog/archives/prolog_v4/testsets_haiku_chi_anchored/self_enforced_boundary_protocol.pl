% ============================================================================
% CONSTRAINT STORY: self_enforced_boundary_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_self_enforced_boundary_protocol, []).

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
 *   constraint_id: self_enforced_boundary_protocol
 *   human_readable: The Self-Enforced Boundary Protocol
 *   domain: psychological/social
 *
 * SUMMARY:
 *   A boundary protocol is a self-enforced action rule: 'If X happens, I will
 *   do Y.' It is fundamentally different from a request for the other party
 *   to change. The subject unilaterally determines both the trigger (X) and
 *   their response (Y), placing the entire locus of control within their own
 *   volition. This structural design eliminates coercion of the other party —
 *   they are informed of the protocol but face no enforcement burden or
 *   penalty for not complying. The constraint represents a pure coordination
 *   mechanism at the dyadic level: the subject organizes their own behavior
 *   to maintain psychological autonomy and relational stability. However, the
 *   institutional discourse around boundary-setting has increasingly
 *   decoupled from actual protocol implementation, creating a therapeutic
 *   theater in which announcing a boundary ('I'm setting a boundary')
 *   substitutes for executing one. The self-enforced boundary protocol
 *   demonstrates how a logically necessary feature of autonomy (the ability
 *   to control one's own actions) can be reframed as a contingent social
 *   practice, and then gradually institutionalized as performative ritual.
 *
 * KEY AGENTS:
 *   - Subject / Boundary Keeper (moderate/mobile): Primary implementer — controls protocol execution entirely; benefits from autonomy and relational clarity
 *   - Other Party / Boundary Recipient (moderate/mobile): Passive participant — receives notification but bears no enforcement burden; retains full exit options
 *   - Therapeutic / Coaching Community (organized/constrained): Teaches boundary protocols as transitional scaffolding during skill development; expects sunset as relational maturity increases
 *   - Institutional Boundary Discourse (institutional/arbitrage): Maintains institutional authority over 'correct' boundary-setting; benefits from symbolic association with psychological health; theater-heavy implementation
 *   - Analytical Observer (analytical/analytical): Recognizes boundary protocol as a logical necessity of autonomy; identifies false summit when institutional theater naturalizes contingent social practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(self_enforced_boundary_protocol, 0.28).
domain_priors:suppression_score(self_enforced_boundary_protocol, 0.42).
domain_priors:theater_ratio(self_enforced_boundary_protocol, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(self_enforced_boundary_protocol, extractiveness, 0.28).
narrative_ontology:constraint_metric(self_enforced_boundary_protocol, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(self_enforced_boundary_protocol, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(self_enforced_boundary_protocol, rope).
narrative_ontology:human_readable(self_enforced_boundary_protocol, "The Self-Enforced Boundary Protocol").
narrative_ontology:topic_domain(self_enforced_boundary_protocol, "psychological/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(self_enforced_boundary_protocol, subject_psychological_autonomy).
narrative_ontology:constraint_beneficiary(self_enforced_boundary_protocol, relational_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT / BOUNDARY KEEPER (ROPE) — The subject implements the protocol unilaterally. Exit options are fully in their control ('If X, then I do Y'). No coercion of the other party is required. The subject benefits from psychological autonomy and relational clarity. d≈0.50, f(d)≈0.65, σ=0.8 → χ≈0.15. Pure coordination: the subject organizes their own behavior to maintain relational stability.
constraint_indexing:constraint_classification(self_enforced_boundary_protocol, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: OTHER PARTY / BOUNDARY RECIPIENT (ROPE) — The other party is informed of the protocol but bears no enforcement burden. They can continue their behavior unchanged; the subject will execute their promised response. The other party benefits from clarity and reduced relational ambiguity. Exit options remain open (they can modify their behavior, or accept the subject's response). d≈0.50, f(d)≈0.65, σ=0.8 → χ≈0.15. Coordination without coercion.
constraint_indexing:constraint_classification(self_enforced_boundary_protocol, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THERAPEUTIC / COACHING COMMUNITY (SCAFFOLD) — Boundary protocols are taught as temporary scaffolding during relational repair or skill-building phases. The expectation is that as communication improves, explicit 'If X then Y' protocols decline in necessity — subjects move toward authentic negotiation and mutual adjustment. The boundary protocol is a developmental stage with an implicit sunset. χ ≤ 0.30 (low extraction during transition), theater ≤ 0.70 (authentic skill-building, not performative).
constraint_indexing:constraint_classification(self_enforced_boundary_protocol, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INSTITUTIONAL BOUNDARY DISCOURSE (PITON) — In mainstream psychotherapy and self-help literature, 'setting boundaries' has become a symbolic virtue with substantial theatrical performance. Many people announce boundaries without executing them ('I'm setting a boundary'), creating a performative ritual detached from actual behavior change. The institutional discourse treats boundary-setting as inherently good, regardless of implementation rigor. theater_ratio ≥ 0.70 reflects that institutional talk about boundaries often exceeds actual protocol enforcement.
constraint_indexing:constraint_classification(self_enforced_boundary_protocol, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / LOGICAL NECESSITY (MOUNTAIN) — From a purely logical standpoint, a self-enforced protocol that requires zero change from the other party is an irreducible feature of autonomy itself. Any relational constraint that preserves unilateral exit options exhibits low base extractiveness (ε ≤ 0.25) and minimal suppression (σ ≤ 0.05). The boundary protocol is a mathematical consequence of the definition of autonomy: an action that depends entirely on one's own choices. accessibility_collapse=0.90 (cannot design away the requirement that 'I control my own actions'), resistance=0.05 (no meaningful resistance, only acceptance or rejection of one's own agency).
constraint_indexing:constraint_classification(self_enforced_boundary_protocol, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(self_enforced_boundary_protocol_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(self_enforced_boundary_protocol, TR),
    TR >= 0.70.

:- end_tests(self_enforced_boundary_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The protocol requires zero change from the other party and zero coercion. The subject bears all implementation burden. However, extractiveness is not zero because: (1) the protocol may reduce the other party's options if they have grown accustomed to the subject's prior accommodation, (2) clarity about boundaries can shift relational dynamics in ways the other party did not choose, (3) institutional coaching creates a market for boundary-setting knowledge that extracts value from relational anxiety. The rise over the interval (0.15 → 0.28) reflects the increasingly extractive framing of boundary-setting in popular psychology: what began as autonomous self-management has been repackaged as a purchasable skill. Suppression (0.42): Moderate. The subject may suppress alternative responses ('I could negotiate more,' 'I could just end the relationship') in favor of the protocol structure. The other party faces suppression of their prior relational assumption ('I can adapt you indefinitely'). Theater ratio (0.35): Low-moderate. In authentic therapeutic contexts, boundary protocols are behavioral (subject executes them), not performative. However, institutional boundary discourse increasingly emphasizes announcement over execution — 'setting a boundary' becomes a speech act rather than an action protocol. The rise in theater (0.20 → 0.35) reflects this institutional drift toward symbolic boundary-setting.
 *
 * PERSPECTIVAL GAP:
 *   The subject and other party experience the constraint identically as Rope — pure coordination with mobile exit options. Both benefit from clarity; neither is coerced. The therapeutic community sees Scaffold — a temporary structure expected to fade as relational skills mature. The institutional discourse sees Piton — boundary-setting as a symbolic virtue maintained through theatrical repetition rather than functional necessity. The analytical observer sees Mountain — the logical necessity that 'I control my own actions' cannot be eliminated, only expressed or suppressed. The perspectival gap is not about disagreement on facts but about the function of the constraint in each context. For the dyad, it is coordination. For the institution, it is theatrical authority. For the analyst, it is logical inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Subject / Boundary Keeper: Mobile exit (can modify protocol, abandon it, or escalate to relationship termination) + moderate power → d ≈ 0.50, f(d) ≈ 0.65. Neutral directionality; subject is neither beneficiary nor victim in the extraction sense. They are the implementer. Other Party / Boundary Recipient: Mobile exit (can continue behavior unchanged, adapt, or leave relationship) + moderate power → d ≈ 0.50, f(d) ≈ 0.65. Neutral directionality; they are informed but not coerced. Therapeutic Community: Constrained (benefits from ongoing demand for boundary-coaching) + organized → d ≈ 0.40, f(d) ≈ 0.40. Low effective extraction during authentic scaffolding phases. Institutional Discourse: Arbitrage (sells authority and techniques) + institutional → d ≈ 0.05, f(d) ≈ -0.12. Net beneficiary through symbolic association with psychological health.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    protocol_credibility_threshold,
    'At what execution rate does a stated boundary protocol transition from genuine coordination to theatrical performance?',
    'Longitudinal tracking of announced boundaries vs actual implementation; correlation between consistency rates and relational outcomes',
    'If threshold low (>50% execution): boundary-setting as practiced is mostly theater (Piton). If threshold high (>90% execution): boundary-setting is mostly Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protocol_credibility_threshold, empirical, 'Execution rate threshold for genuine vs performative boundary-setting').

omega_variable(
    mutual_adaptation_dependency,
    'Does a self-enforced boundary eliminate interdependence or merely clarify it?',
    'Analysis of whether subjects using boundary protocols report increased relational satisfaction or increased isolation; whether the other party adapts or disengages',
    'If elimination: boundary protocol is pure autonomy expression (Mountain). If clarification: the other party retains agency to adapt or separate, making it true Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mutual_adaptation_dependency, conceptual, 'Whether boundaries eliminate or clarify mutual interdependence').

omega_variable(
    coaching_sunset_actualization,
    'Do boundary protocols actually function as transitional scaffolding, or do subjects maintain them indefinitely as permanent relational infrastructure?',
    'Longitudinal tracking of subjects taught boundary protocols; measurement of protocol abandon or evolution over 2+ years; comparison with therapeutic theory predictions',
    'If sunset achieved: Scaffold classification is structural (protocols naturally fade). If permanent: Scaffold is aspirational; actual constraint is Rope or Piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coaching_sunset_actualization, empirical, 'Whether boundary protocols function as temporary scaffolding or permanent structures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(self_enforced_boundary_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sebp_tr_t0, self_enforced_boundary_protocol, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sebp_tr_t5, self_enforced_boundary_protocol, theater_ratio, 5, 0.3).
narrative_ontology:measurement(sebp_tr_t10, self_enforced_boundary_protocol, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(sebp_be_t0, self_enforced_boundary_protocol, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sebp_be_t5, self_enforced_boundary_protocol, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(sebp_be_t10, self_enforced_boundary_protocol, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(self_enforced_boundary_protocol, resource_allocation).
narrative_ontology:affects_constraint(self_enforced_boundary_protocol, relational_autonomy_paradox).
narrative_ontology:affects_constraint(self_enforced_boundary_protocol, therapeutic_theater_inflation).

% DUAL FORMULATION NOTE:
% The self-enforced boundary protocol decomposes into two structurally distinct constraints: (1) the logical structure of autonomy itself (low extractiveness, universal scope) which appears as Mountain, and (2) the institutional practice of boundary-setting discourse (higher theater ratio, extractive coaching market) which appears as Piton. Both are true; they measure different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
