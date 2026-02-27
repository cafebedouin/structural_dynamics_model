% ============================================================================
% CONSTRAINT STORY: litany_of_the_real
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_litany_of_the_real, []).

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
 *   constraint_id: litany_of_the_real
 *   human_readable: The Litany of the Real (Deferential Realism)
 *   domain: philosophical/cognitive
 *
 * SUMMARY:
 *   The Litany of the Real is a formal set of cognitive protocols designed to
 *   reduce the gap between an agent's internal model of reality and external
 *   reality itself. Central to Deferential Realism (Yudkowsky et al.), the
 *   litany operates as a coordination mechanism that allows agents to: (1)
 *   acknowledge the mind-independence of reality, (2) accept the possibility
 *   of their own error, (3) update beliefs in response to evidence, and (4)
 *   communicate about shared external referents with minimal model drift. The
 *   constraint is fundamentally non-extractive—it creates no systematic
 *   winners and losers, imposes no asymmetric costs, and operates through
 *   voluntary adoption rather than coercion. However, the litany's
 *   relationship to institutional structures (academia, rationalist
 *   communities, epistemic commons) generates perspectival diversity: some
 *   agents experience it as pure coordination (Rope), others as performative
 *   ritual (Piton), and some analytical observers risk treating it as a
 *   natural law of rationality (false Mountain). The key diagnostic is
 *   whether the litany's power comes from its content (genuine epistemic
 *   benefit) or its social coordination function (shared commitment to a
 *   protocol). The trajectory from low theater_ratio (0.08) to modest
 *   elevation (0.15) suggests that as the litany has been adopted and
 *   formalized within rationalist communities, it has accumulated some
 *   performative aspects—ritual invocation, in-group signaling—without losing
 *   its core coordination function.
 *
 * KEY AGENTS:
 *   - Epistemic Agents (moderate/mobile): Individual researchers, decision-makers, rationalists who voluntarily adopt the litany; benefit from reduced model-reality gap and improved decision quality
 *   - Rationalist Community (powerful/arbitrage): Organized epistemic network (LessWrong, AI safety communities, research organizations) that uses litany as shared coordination standard; experiences low extraction overhead
 *   - Academic Philosophy Establishment (institutional/constrained): Traditional philosophy departments and institutions that view the litany as external to disciplinary practice; experience it as performative rather than core
 *   - Decentralized Epistemic Commons (organized/mobile): Open-source science networks, arXiv communities, distributed verification mechanisms that treat the litany as temporary scaffold
 *   - Analytical Observer (analytical/analytical): Civilizational perspective that evaluates the litany's structural role in epistemic coordination; risks naturalizing contingent protocols as immutable laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(litany_of_the_real, 0.18).
domain_priors:suppression_score(litany_of_the_real, 0.08).
domain_priors:theater_ratio(litany_of_the_real, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(litany_of_the_real, extractiveness, 0.18).
narrative_ontology:constraint_metric(litany_of_the_real, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(litany_of_the_real, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(litany_of_the_real, rope).
narrative_ontology:human_readable(litany_of_the_real, "The Litany of the Real (Deferential Realism)").
narrative_ontology:topic_domain(litany_of_the_real, "philosophical/cognitive").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(litany_of_the_real, epistemic_agents).
narrative_ontology:constraint_beneficiary(litany_of_the_real, reality_alignment_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTICING RATIONALIST (ROPE) — Agent who actively adopts the litany's protocols experiences genuine coordination benefit: reduced model-reality gap, improved decision quality, lowered cognitive load from maintaining multiple incompatible beliefs. d≈0.35, f(d)≈0.30, σ=1.2 → χ≈0.07.
constraint_indexing:constraint_classification(litany_of_the_real, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: RATIONALIST COMMUNITY (ROPE) — Organized epistemic agents (LessWrong, research communities, decision-makers) who adopt the litany as a coordination standard experience low extraction. The protocol solves a collective action problem (shared model of reality) with minimal coercive overhead. d≈0.10, f(d)≈-0.05, σ=1.2 → χ≈-0.01. Net beneficiary.
constraint_indexing:constraint_classification(litany_of_the_real, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (ROPE) — From a civilizational/universal frame, the litany is a pure coordination mechanism: it solves the problem of aligning internal models with external reality without creating asymmetric extraction. No agent bears systematic cost; all agents improve their epistemic position. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.12.
constraint_indexing:constraint_classification(litany_of_the_real, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ACADEMIC PHILOSOPHY ESTABLISHMENT (PITON) — Traditional philosophy departments may view the litany as exogenous to their field, performed as intellectual theater (rationalist 'self-help' protocols) rather than genuine philosophical method. theater_ratio≈0.35 (ritual invocation without deep integration into institutional practice). The constraint persists through institutional inertia among rationalist communities, but academic philosophy largely ignores it. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.05.
constraint_indexing:constraint_classification(litany_of_the_real, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: DECENTRALIZED EPISTEMIC COMMONS (SCAFFOLD) — Open epistemic networks (arXiv, open-source science, decentralized knowledge commons) see the litany as a temporary coordination scaffold: a transitional protocol that helps bootstrap better model-reality alignment until more robust distributed verification mechanisms mature. χ≤0.30, and the sunset is real—as epistemic technology improves, explicit litany-style protocols become less necessary. d≈0.35, f(d)≈0.30, σ=1.2 → χ≈0.08.
constraint_indexing:constraint_classification(litany_of_the_real, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW VIEW / FALSE SUMMIT (MOUNTAIN?) — Risk perspective: if the litany is framed as an immutable law of rationality itself ('you cannot avoid deference to reality'), it collapses into a tautology and loses prescriptive force. However, the base properties (ε=0.18, suppression=0.08, theater=0.15) all contradict the mountain gates (ε≤0.25✓, suppression≤0.05✗). This is a FALSE SUMMIT: the litany is a contingent protocol, not a law of nature. accessibility_collapse=0.25, resistance=0.85.
constraint_indexing:constraint_classification(litany_of_the_real, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(litany_of_the_real_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(litany_of_the_real, TR),
    TR >= 0.70.

:- end_tests(litany_of_the_real_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The litany solves the coordination problem of model-reality alignment without creating systematic beneficiaries or victims. Adopting agents benefit directly (better models, better decisions). Non-adopting agents are not harmed—they simply lack the coordination benefit. No extraction rent is collected by any institutional actor. The slight elevation above zero reflects the reality that early adopters and rationalist community leaders do gain minor status/credibility benefits from community adoption of 'their' protocol. Suppression (0.08): Very low. Agents can costlessly exit the litany (no legal, financial, or social prohibition on belief formation). Alternative epistemic methods are openly available. The protocol is fully transparent in its aims and mechanisms. No alternative is suppressed by the litany's existence. Theater ratio (0.15): Low-moderate. The litany does have some performative aspects: ritual recitation, community signaling, in-group identity. But the core function (alignment with reality) is genuine and measurable. Theater has increased slightly over 10 years as the litany has become more formalized and community-branded.
 *
 * PERSPECTIVAL GAP:
 *   The practicing rationalist sees genuine epistemic coordination (Rope)—the protocol works; their models improve. The rationalist community sees institutional coordination with no overhead (Rope)—they communicate about reality using shared standards. The analytical observer sees pure coordination (Rope)—no extraction possible. The academic establishment sees performative ritual (Piton)—the litany is external to their field and functions as intellectual theater among rationalists. The epistemic commons sees a temporary scaffold (Scaffold)—as epistemic technology matures, explicit protocols become unnecessary. The false summit perspective risks seeing an immutable law of rationality (Mountain) but the base properties contradict the mountain gates: suppression=0.08 > 0.05 disqualifies it. The perspectival gap reveals that all evaluators agree on the rope classification, but differ on whether the performance aspects are essential or incidental.
 *
 * DIRECTIONALITY LOGIC:
 *   Epistemic agents (moderate/mobile, adopters): Beneficiaries of improved model alignment. d≈0.35, f(d)≈0.30. Rationalist community (powerful/arbitrage, institutional): Net beneficiaries of coordination standard. d≈0.10, f(d)≈-0.05. Academic establishment (institutional/constrained, non-adopters): Neither beneficiaries nor victims; experience constraint as external. d≈0.55, f(d)≈0.75, but exit is cheap (constrained, not trapped). Epistemic commons (organized/mobile, partial adopters): See litany as useful but temporary. d≈0.35, f(d)≈0.30. Analytical observer: d≈0.50, f(d)≈0.65 (neutral observation). No agent is trapped or coerced. No agent bears systematic extraction cost. The low χ values across all perspectives confirm rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The litany resolves the mandatrophy by being unambiguously non-extractive: it cannot be mislabeled as pure extraction (no victims, no asymmetric costs, no suppression of alternatives) nor as pure natural law (theater_ratio too high, suppression insufficient, emerges_naturally false). The false summit perspective (perspective 6) is diagnostic: it shows that naturalizing the litany as immutable rationality is the actual epistemic error the litany warns against. The litany teaches deference to reality; treating the litany itself as immune to criticism or revision would be precisely the epistemic failure it targets. All legitimate perspectives converge on Rope with possible performative inflation to Piton (but not Snare, not Tangled Rope with asymmetric extraction). The constraint is cleanly classified once the false summit is explicitly rejected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_vs_explicit_protocols,
    'Does the litany function more powerfully when explicitly recited as a formal protocol, or is it equally effective as an implicit cognitive habit?',
    'Comparative cognitive science studies: measure model-reality gap alignment and decision quality in populations that use explicit litany recitation vs implicit protocol internalization vs control groups',
    'If explicit outperforms implicit: the litany is a coordination mechanism that requires shared acknowledgment (Rope confirmed). If equivalent: the underlying principle is what matters, and the litany is a proxy or theater (Piton status elevated). If implicit outperforms explicit: the recitation is performative overhead (strong evidence for theater component).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_vs_explicit_protocols, empirical, 'Whether explicit litany recitation vs implicit protocol internalization matters').

omega_variable(
    reality_alignment_verification,
    'What constitutes valid evidence that an agent''s model is actually better aligned with reality by virtue of adopting the litany?',
    'Longitudinal tracking of prediction accuracy, decision quality, and model-revision patterns in populations practicing the litany vs control groups; measurement of confidence calibration, surprise rate, and rapid adaptation to disconfirming evidence',
    'If strong evidence: extractiveness should be lowered further (ε<0.10, pure public good). If weak evidence: extractiveness should be raised (ε>0.30, theater component dominant). If inconclusive: the litany''s epistemic value is subjective (preference-class omega).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reality_alignment_verification, empirical, 'Whether model-reality alignment is demonstrably improved by litany adoption').

omega_variable(
    community_fragmentation_risk,
    'Does explicit adoption of the litany as a community standard create in-group/out-group epistemic fragmentation between rationalist and non-rationalist communities?',
    'Analysis of epistemic dialogue patterns, citation networks, and shared-reference standards between communities that adopt the litany vs those that don''t; measurement of belief-updating cascades and convergence/divergence in model-reality alignment over time',
    'If true fragmentation: the litany is coordination-within-group but extraction-between-groups (Tangled Rope candidate). If no fragmentation: the litany is truly universal coordination (pure Rope confirmed). If increases convergence: the litany solves collective model alignment (strong Rope evidence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_fragmentation_risk, empirical, 'Whether litany adoption creates epistemic fragmentation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(litany_of_the_real, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(litany_tr_t0, litany_of_the_real, theater_ratio, 0, 0.08).
narrative_ontology:measurement(litany_tr_t5, litany_of_the_real, theater_ratio, 5, 0.12).
narrative_ontology:measurement(litany_tr_t10, litany_of_the_real, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(litany_be_t0, litany_of_the_real, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(litany_be_t5, litany_of_the_real, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(litany_be_t10, litany_of_the_real, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(litany_of_the_real, information_standard).
narrative_ontology:affects_constraint(litany_of_the_real, calibration_accuracy).
narrative_ontology:affects_constraint(litany_of_the_real, belief_updating_cascades).

% DUAL FORMULATION NOTE:
% The litany operates as both a formal protocol (explicit recitation) and an implicit cognitive habit (internalized deference to reality). These are separable constraints with potentially different ε values, but the structure of the litany suggests they converge on the same classification. The network link to calibration_accuracy captures the downstream epistemic consequences; the link to belief_updating_cascades captures the coordination effects on distributed epistemic communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
