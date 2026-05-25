% ============================================================================
% CONSTRAINT STORY: awareness_without_leverage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_awareness_without_leverage, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: awareness_without_leverage
 *   human_readable: The Cassandra Paradox: Awareness Without Leverage
 *   domain: social/political
 *
 * SUMMARY:
 *   The Cassandra Paradox describes a structural constraint where accurate
 *   knowledge of systemic failure provides no leverage to prevent it. The
 *   constraint manifests across domains: climate scientists publishing
 *   peer-reviewed predictions that policy-makers ignore; financial system
 *   analysts warning of bubble dynamics before collapses; epidemiologists
 *   forecasting pandemic waves unheeded; security specialists predicting
 *   infrastructure vulnerabilities left unpatched. The constraint exhibits
 *   high extractiveness (0.52) because the suppression mechanism (0.68)
 *   operates not through rational disagreement but through institutional
 *   inertia, retaliation threat, and performative response theater (0.64).
 *   The Cassandra bears full cost of awareness — psychological burden,
 *   credibility damage, isolation — while decision-making authorities capture
 *   benefits of inaction through deferral of expensive corrections. The
 *   constraint's extractiveness has increased over the interval (0.32 → 0.52)
 *   as information asymmetry and institutional complexity have grown: more
 *   actors possess more data, fewer decision-makers integrate it, and theater
 *   (official acknowledgment without behavioral change) has become more
 *   sophisticated as an avoidance mechanism.
 *
 * KEY AGENTS:
 *   - Cassandra (informed observer): Powerless/trapped — possesses accurate foresight; bears costs of disbelief and isolation
 *   - Institutional whistleblower: Moderate/constrained — has technical expertise and internal platform; faces retaliation risk and career jeopardy
 *   - Decision-making authority: Institutional/arbitrage — receives warnings; benefits from inaction through deferral costs and maintains status quo
 *   - Institutional reputation system: Organized/constrained — theoretically constrains authority but operates through theater rather than enforcement
 *   - Media/epistemic intermediary: Powerful/mobile — amplifies warnings but benefits from catastrophe narratives and attention scarcity; mixed coordination-extraction
 *   - Analytical observer: Civilizational/analytical — risks naturalizing contingent suppression mechanisms as immutable features of governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(awareness_without_leverage, 0.52).
domain_priors:suppression_score(awareness_without_leverage, 0.68).
domain_priors:theater_ratio(awareness_without_leverage, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(awareness_without_leverage, extractiveness, 0.52).
narrative_ontology:constraint_metric(awareness_without_leverage, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(awareness_without_leverage, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(awareness_without_leverage, snare).
narrative_ontology:human_readable(awareness_without_leverage, "The Cassandra Paradox: Awareness Without Leverage").
narrative_ontology:topic_domain(awareness_without_leverage, "social/political").

domain_priors:requires_active_enforcement(awareness_without_leverage).
% --- Structural relationships ---
narrative_ontology:constraint_victim(awareness_without_leverage, informed_observers).
narrative_ontology:constraint_victim(awareness_without_leverage, epistemic_commons).
narrative_ontology:constraint_victim(awareness_without_leverage, prediction_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CASSANDRA (SNARE) — Possesses accurate foresight but no mechanism to alter outcomes. Bears full cost of being disbelieved: credibility damage, psychological burden of futile warning, isolation. No exit available — cannot unknow the failure, cannot compel action, cannot protect themselves from reputational harm when prediction fails or succeeds. Maximum structural extraction from a powerless epistemic position.
constraint_indexing:constraint_classification(awareness_without_leverage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL WHISTLEBLOWER (SNARE) — Has internal credibility and technical expertise (moderate power) but constrained exit: retaliation risk, career destruction, legal jeopardy. The institutional position gives them platform but the suppression mechanisms (NDAs, retaliation threat, institutional reputation protection) remove their exit option. Aware of structural failure and has some voice, but unable to amplify without bearing severe costs.
constraint_indexing:constraint_classification(awareness_without_leverage, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DECISION-MAKING AUTHORITY (ROPE) — Receives early warnings but benefits from the status quo through institutional inertia, deferral of action costs, and information asymmetry. Experiences the Cassandra constraint as a coordination mechanism: the predictive failure allows them to claim surprise when disaster strikes ('nobody could have foreseen this'). Can exit by choosing to act or not; experiences no suppression because they control the enforcement apparatus.
constraint_indexing:constraint_classification(awareness_without_leverage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL REPUTATION SYSTEM (PITON) — Theoretically, reputation damage from ignoring accurate warnings should constrain decision-makers. But institutional reputation systems are heavily theatrical: organizations survive public scandals, bad predictions are redefined or forgotten, and the act of 'taking warnings seriously' is performed without actual behavior change. The reputation mechanism persists as scaffolding but has lost functional leverage. Theater ratio high because institutional responses (crisis committees, official apologies, token reforms) are performative rather than structural.
constraint_indexing:constraint_classification(awareness_without_leverage, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MEDIA/EPISTEMIC INTERMEDIARY (TANGLED ROPE) — Has platform and reach (powerful, mobile) but also embedded in attention economy and advertisers' interests. Benefits from catastrophe narratives (coordination function: alerts public to risk). But also extracts through sensationalism, false balance (giving credibility to deniers), and attention-capture without action mechanisms. Possesses leverage but partly uses it for extraction rather than warning escalation.
constraint_indexing:constraint_classification(awareness_without_leverage, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (NATURAL LAW VIEW) — From a civilizational perspective, information asymmetry in complex systems is an immutable structural property: decision-makers always have incomplete information, incentives are always misaligned, and accurate predictions about low-probability high-impact events always face credibility barriers. The paradox appears as inherent to governance under uncertainty. HOWEVER: the structural data contradicts this — the extractiveness arises from specific institutional choices (suppression, retaliation mechanisms, reputational theater), not from physics or logic.
constraint_indexing:constraint_classification(awareness_without_leverage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(awareness_without_leverage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(awareness_without_leverage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(awareness_without_leverage, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(awareness_without_leverage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(awareness_without_leverage, TR),
    TR >= 0.70.

:- end_tests(awareness_without_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The constraint extracts from Cassandras through credibility damage, psychological cost, and isolation. Decision-making authorities extract through inaction benefits: deferral of costly corrections, maintenance of existing power structures, avoidance of admitting prior failures. The extraction is not maximal (snare threshold is 0.46 effective extraction) because some accurate predictions do eventually change behavior, creating partial exit for Cassandras and some accountability for authorities. Suppression (0.68): High. Multiple suppression mechanisms operate: (1) institutional retaliation against whistleblowers; (2) structural incentive misalignment — Cassandras' positions (researchers, junior analysts, outside observers) lack institutional decision-making power; (3) performative institutional response theater that simulates action without behavioral change; (4) attention economy dynamics that favor dramatic narratives over sustained technical warnings; (5) temporal mismatch — warnings about slow-moving systemic failures compete for urgency against immediate operational demands. Theater ratio (0.64): Moderate-high and increasing. The institutional response to warnings has become increasingly performative: crisis committees are convened (signal of seriousness), apologies are issued, official reviews are commissioned, future prevention is promised — all with low probability of actual structural change. The theater has increased over the measurement interval (0.48 → 0.64) as institutions have learned that performance of responsiveness can substitute for actual response, at least in the medium term.
 *
 * PERSPECTIVAL GAP:
 *   The Cassandra and whistleblower perspectives classify as pure snare: high extraction, no exit, trapped in awareness without leverage. The decision-making authority classifies as rope: they experience the warning system as pure coordination (information provision without enforcement) because they control the response decision and benefit from inaction. The media/intermediary perspective is tangled rope: genuine coordination function (alerting public to risk) combined with extraction (attention capture, sensationalism profit). The reputation system is piton: theoretically constrains authority but persists through theatrical performance rather than functional constraint. The analytical observer risks mountain classification (information asymmetry is inherent to governance) but the structural data reveals contingent institutional choices, not natural law. The perspectival gap is maximum because powerless observers and powerful decision-makers have completely opposite experiences of the same constraint: one experiences extraction without exit, the other experiences low-cost coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for Cassandra (powerless/trapped): d ≈ 0.95, experiences maximum effective extraction (high f(d)). No beneficiary relationship; full victim status. Exit options: none — cannot unknow the future, cannot force action, cannot protect reputation. Directionality for whistleblower (moderate/constrained): d ≈ 0.70, experiences significant extraction. Victim status (bears retaliation risk); constrained exit (cannot leave without career cost). Directionality for authority (institutional/arbitrage): d ≈ 0.10, experiences near-zero or negative extraction. Beneficiary status (inaction benefits); arbitrage exit (can choose response or inaction at low structural cost). Directionality for media (powerful/mobile): d ≈ 0.50, experiences symmetric mixed extraction and benefit. Both beneficiary (attention profit, narrative material) and victim (credibility cost if predictions fail); mobile exit (can choose coverage or ignore). The engine derives d from beneficiary/victim declarations and exit options; in this case, the constraints are heavily asymmetric across perspectives, producing the maximum perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   EXTRACTIVE VS COORDINATION AMBIGUITY: The Cassandra constraint initially appears as pure coordination failure (information provision without response) but resolves as extraction when institutional incentives are examined. The authority benefits from inaction (deferral of costly corrections, maintained status quo), making the constraint extractive rather than purely coordinative. However, the authority does not actively prevent warnings from being issued (no censorship in most cases) — suppression operates through structural incentive misalignment and retaliation threat, not through active enforcement of silence. This is the defining feature of tangled rope candidates: genuine coordination function (warnings are issued and disseminated) combined with asymmetric extraction (Cassandras bear costs, authorities capture benefits). The snare classification at powerless/trapped perspective is robust because the epistemic commons and informed observers have zero leverage and bear all costs. The rope classification at institutional/arbitrage perspective reflects the authority's genuine experience: warnings are a low-cost informational input they can ignore without structural consequence. The divergence resolves by noting that the constraint operates at different structural levels: (1) information level: coordination (warnings are transmitted); (2) decision level: extraction (authorities benefit from ignoring warnings); (3) epistemic level: snare (epistemic reliability is degraded and no agent advocates for it). The presheaf over these observation sites shows the constraint simultaneously coordinates on information transmission and extracts on decision outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_threshold_mechanism,
    'What determines whether an accurate prediction achieves behavioral change: the quality of evidence, the source''s institutional status, the catastrophe''s proximity, or the decision-maker''s prior commitment?',
    'Historical case comparison (ignored accurate warnings vs warnings that triggered action); interview analysis of decision-maker reasoning; causal pathway detection',
    'If evidence quality dominates: Cassandra problem is solvable through better communication. If institutional status dominates: problem requires structural power redistribution. If proximity dominates: problem is time-horizon misalignment. If prior commitment dominates: problem is intractable without replacing decision-makers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_threshold_mechanism, empirical, 'Mechanism determining whether predictions trigger behavioral change').

omega_variable(
    suppression_enforcement_agent,
    'Does suppression of Cassandra warnings operate through active institutional enforcement (retaliation, censorship) or through structural incentive misalignment (career risk, attention scarcity)?',
    'Detailed case analysis of suppression mechanisms in specific predictions (climate models suppressed how?, financial system warnings suppressed how?); distinction between explicit retaliation vs passive neglect',
    'If active enforcement: suppression coefficient ≥ 0.70, snare classification robust. If structural incentive only: suppression coefficient may be overestimated, constraint shifts toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_enforcement_agent, empirical, 'Nature of suppression mechanism (enforcement vs incentive)').

omega_variable(
    extractive_vs_coordinate_benefit,
    'Does the decision-making authority benefit from their own prediction-ignoring (extraction from Cassandra), or do they simply fail to pay the cost of action (coordination failure)?',
    'Analysis of whether decision-maker receives direct benefit from catastrophe (insurance payoff, avoided transition costs, maintained power structure) vs simply defers action costs; counterfactual analysis of what decision-maker gains from inaction',
    'If extraction benefit present: snare classification confirmed across perspectives. If only coordination failure: multiple perspectives should classify as rope or scaffold, not snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_vs_coordinate_benefit, conceptual, 'Whether decision-maker extracts from ignoring predictions or simply fails to coordinate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(awareness_without_leverage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cassandra_tr_t0, awareness_without_leverage, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cassandra_tr_t5, awareness_without_leverage, theater_ratio, 5, 0.58).
narrative_ontology:measurement(cassandra_tr_t10, awareness_without_leverage, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(cassandra_be_t0, awareness_without_leverage, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cassandra_be_t5, awareness_without_leverage, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(cassandra_be_t10, awareness_without_leverage, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(awareness_without_leverage, information_standard).
narrative_ontology:affects_constraint(awareness_without_leverage, reputational_theater).
narrative_ontology:affects_constraint(awareness_without_leverage, temporal_mismatch_governance).
narrative_ontology:affects_constraint(awareness_without_leverage, institutional_learned_helplessness).

% DUAL FORMULATION NOTE:
% The Cassandra Paradox decomposes into multiple structurally distinct constraints: (1) information asymmetry (who knows what) — coordination problem; (2) decision-making authority alignment (who decides) — extraction problem; (3) institutional reputation response (how authority responds publicly) — piton problem. Each has its own ε value and classification. This story focuses on the integrated constraint of awareness without leverage, which is the snare intersection of all three. The downstream constraints map specific institutional contexts: reputational theater in governance, temporal mismatch between warning timescales and decision timescales, and learned helplessness (Cassandras stopping warnings after repeated failure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(awareness_without_leverage, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
