% ============================================================================
% CONSTRAINT STORY: elite_legitimacy_fracture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_elite_legitimacy_fracture, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: elite_legitimacy_fracture
 *   human_readable: Elite Legitimacy Fracture in Putin's Russia
 *   domain: political_economy/regime_stability/military_conflict
 *
 * SUMMARY:
 *   The elite legitimacy fracture in Putin's Russia represents a structural
 *   shift from the traditional 'bad boyars' scapegoating mechanism to direct
 *   criticism of Putin himself among formerly loyal elites. This transition
 *   is driven by cascading military defeats in Ukraine, economic collapse
 *   under sanctions, and the regime's inability to deliver on nationalist
 *   promises. The constraint exhibits tangled rope characteristics: it
 *   coordinates elite defection (each public criticism reduces the cost of
 *   subsequent defections, creating common knowledge of regime vulnerability)
 *   while extracting heavily from both the regime (which loses legitimacy and
 *   control) and from defectors (who face arrest, asset seizure, and career
 *   destruction). The fracture is observable through multiple channels:
 *   lawyers like Remeslo calling for Putin's trial, military bloggers like
 *   Girkin acknowledging defeat, nationalist figures like Gubarev questioning
 *   war objectives, high-profile arrests like the Tsalikov $81.2M case, and
 *   open speculation about palace coups in nationalist Telegram channels. The
 *   theater_ratio (0.58) reflects that much of the regime's response is
 *   performative: arrests are selective rather than comprehensive, defection
 *   statements are sometimes tolerated, and the regime oscillates between
 *   suppression and accommodation, revealing uncertainty about its own
 *   capacity to contain the fracture.
 *
 * KEY AGENTS:
 *   - Putin Regime Core: Primary victim (powerless/trapped) — cannot exit the legitimacy crisis; loses the boyar-scapegoat mechanism that previously insulated Putin from blame
 *   - Defecting Elite Coalition: Primary beneficiary (organized/mobile) — Remeslo, Girkin, Gubarev, nationalist bloggers coordinate through defection signals; position for post-Putin scenarios
 *   - Loyalist Elites: Secondary victim (moderate/constrained) — face career risk whether they remain loyal to a failing regime or defect; experience mixed coordination and extraction
 *   - Oligarch Class: Mixed position (powerful/arbitrage) — can exit via offshore assets but still embedded in Russian political economy; face asset seizure risk (Tsalikov precedent) while gaining repositioning opportunities
 *   - Security Apparatus: Institutional manager (institutional/constrained) — sees the fracture as transitional; will resolve through either regime change or reconsolidation
 *   - Analytical Observer: Global perspective (analytical/analytical) — sees genuine coordination function (defection cascade) and substantial extraction (suppression, arrests, career destruction)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(elite_legitimacy_fracture, 0.68).
domain_priors:suppression_score(elite_legitimacy_fracture, 0.72).
domain_priors:theater_ratio(elite_legitimacy_fracture, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(elite_legitimacy_fracture, extractiveness, 0.68).
narrative_ontology:constraint_metric(elite_legitimacy_fracture, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(elite_legitimacy_fracture, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(elite_legitimacy_fracture, tangled_rope).
narrative_ontology:human_readable(elite_legitimacy_fracture, "Elite Legitimacy Fracture in Putin's Russia").
narrative_ontology:topic_domain(elite_legitimacy_fracture, "political_economy/regime_stability/military_conflict").

domain_priors:requires_active_enforcement(elite_legitimacy_fracture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(elite_legitimacy_fracture, defecting_elites).
narrative_ontology:constraint_beneficiary(elite_legitimacy_fracture, nationalist_opposition).
narrative_ontology:constraint_beneficiary(elite_legitimacy_fracture, post_putin_coalition_candidates).
narrative_ontology:constraint_victim(elite_legitimacy_fracture, putin_regime).
narrative_ontology:constraint_victim(elite_legitimacy_fracture, loyalist_elites).
narrative_ontology:constraint_victim(elite_legitimacy_fracture, regime_stability_apparatus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUTIN REGIME CORE (SNARE) — Trapped by the legitimacy fracture with no exit. The regime cannot acknowledge elite defection without validating criticism, cannot suppress all defectors without revealing weakness, and cannot reverse military failures that drive the fracture. Maximum extraction: the constraint strips away the boyar-scapegoat mechanism that previously insulated Putin from direct blame.
constraint_indexing:constraint_classification(elite_legitimacy_fracture, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOYALIST ELITE (TANGLED ROPE) — Constrained by career dependency on regime survival but also coordinating through the fracture: defection signals create common knowledge of regime vulnerability, enabling coalition formation. Mixed experience: the constraint both threatens their positions (if they remain loyal to a failing regime) and offers coordination benefits (defection becomes safer as more elites defect). Substantial extraction but not maximal.
constraint_indexing:constraint_classification(elite_legitimacy_fracture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEFECTING ELITE COALITION (ROPE) — Organized agents (Remeslo, Girkin, Gubarev, nationalist bloggers) experience the fracture as coordination: each defection signal reduces the cost of subsequent defections, creating a cascade. Mobile exit options (exile, political repositioning, nationalist alternative coalitions). Net beneficiaries: the constraint enables them to exit a failing regime and position for post-Putin scenarios. Low effective extraction.
constraint_indexing:constraint_classification(elite_legitimacy_fracture, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: SECURITY APPARATUS (SCAFFOLD) — Institutional actors managing the transition see the fracture as temporary: the current legitimacy crisis will resolve either through regime change (sunset via collapse) or through successful suppression and reconsolidation (sunset via restoration). The constraint is transitional by definition — elite legitimacy fractures do not persist indefinitely; they resolve into new equilibria. Constrained exit (cannot abandon institutional role) but sees a definite endpoint.
constraint_indexing:constraint_classification(elite_legitimacy_fracture, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OLIGARCH CLASS (TANGLED ROPE) — Powerful agents with arbitrage-level exit (offshore assets, foreign residency options, business diversification) but still embedded in the Russian political economy. Experience mixed coordination and extraction: the fracture enables repositioning and coalition-building (coordination function) while threatening asset seizures and arrests (extraction function, as demonstrated by Tsalikov case). The constraint both coordinates their exit strategies and extracts through enforcement risk.
constraint_indexing:constraint_classification(elite_legitimacy_fracture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the fracture exhibits both genuine coordination (elite defection signals create common knowledge, enabling collective action against a failing regime) and substantial extraction (the regime's suppression apparatus targets defectors, arrests senior officials, and imposes career/personal costs). The constraint is not pure extraction (snare) because the coordination function is real and measurable; it is not pure coordination (rope) because suppression and career destruction are substantial. Tangled rope is the structurally accurate classification.
constraint_indexing:constraint_classification(elite_legitimacy_fracture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(elite_legitimacy_fracture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(elite_legitimacy_fracture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(elite_legitimacy_fracture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(elite_legitimacy_fracture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(elite_legitimacy_fracture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts heavily from the regime (legitimacy collapse, loss of elite loyalty, exposure of Putin to direct criticism) and from defectors (arrest risk, asset seizure, career destruction, exile). The extraction is not maximal (0.9+) because some defectors successfully exit and reposition, and the regime retains partial suppression capacity. The value has risen from 0.35 at invasion start to 0.68 currently, tracking the accumulation of military defeats and the acceleration of elite defection. Suppression (0.72): High. The regime maintains substantial suppression capacity through selective arrests (Tsalikov case), implicit threats to oligarchs, and control over nationalist channels. However, suppression is not total (0.9+) because defection statements continue, nationalist bloggers operate openly, and coup speculation circulates. The rising trajectory (0.45 to 0.72) reflects the regime's increasing reliance on coercion as legitimacy erodes. Theater ratio (0.58): Moderate-high. Much of the regime's response is performative: arrests are selective and symbolic rather than comprehensive; some defection statements are tolerated while others are suppressed; the regime oscillates between accommodation and crackdown, revealing strategic uncertainty. The theater has increased over the interval (0.35 to 0.58) as the regime's actual capacity to suppress elite defection has diverged from its performative displays of control.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a clear perspectival gap between the regime core (snare — trapped in terminal legitimacy decline with no exit), loyalist elites (tangled rope — mixed coordination and extraction as they navigate regime collapse), and defecting elites (rope — coordination mechanism enabling collective exit and coalition formation). The oligarch class experiences tangled rope from a different structural position: arbitrage-level exit options but still embedded enough to face extraction through asset seizure. The security apparatus sees scaffold (transitional crisis with definite sunset), while the analytical observer sees tangled rope (genuine coordination function coexisting with substantial extraction). The gap reveals that the same structural phenomenon — elite defection from a failing authoritarian regime — appears as pure extraction to those trapped within it, as coordination to those organizing the exit, and as a mixed hybrid to those caught between loyalty and defection.
 *
 * DIRECTIONALITY LOGIC:
 *   The regime core is a full victim (d → 1.0): trapped with no exit, bearing maximum extraction as legitimacy collapses. Loyalist elites are partial victims (d → 0.6-0.7): constrained by career dependency but gaining some coordination benefits as defection becomes normalized. Defecting elites are beneficiaries (d → 0.2-0.3): organized with mobile exit, experiencing the constraint as coordination that enables collective action. Oligarchs are mixed (d → 0.4-0.5): powerful with arbitrage exit but still facing extraction through asset seizure risk; the constraint both coordinates their repositioning and extracts through enforcement. The security apparatus is near-neutral (d → 0.45-0.55): institutional role requires managing the transition regardless of outcome; experiences neither strong extraction nor strong benefit. These directionality values are derived from the agents' structural positions (power level, exit options, beneficiary/victim status) and determine their experienced extractiveness through the engine's chi computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that tangled rope is the structurally accurate classification from the analytical perspective: the elite legitimacy fracture exhibits both genuine coordination (defection signals create common knowledge, reduce individual defection costs, enable coalition formation) and substantial extraction (regime suppression, arrests, asset seizures, career destruction). The coordination function is not merely cover for extraction — it is a real structural feature enabling collective action against a failing regime. The extraction is not incidental overhead — it is a substantial cost imposed on both defectors and the regime itself. The perspectival gap (snare from regime core, rope from defecting elites, tangled rope from loyalists and analytical observer) reflects different structural positions relative to the same constraint, not different constraints. The mandate (elite loyalty to Putin) has outlived its function (regime cannot deliver on nationalist promises or military victory), but the constraint persists through suppression and career dependency, creating the mixed coordination-extraction profile characteristic of tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fracture_reversibility,
    'Is the elite legitimacy fracture reversible through military victory or successful suppression, or has it crossed an irreversible threshold?',
    'Historical analysis of authoritarian regime legitimacy crises; identification of cases where elite defection cascades were reversed vs. cases where they led to regime change. Key indicators: whether arrested elites are rehabilitated or purged; whether defection statements are retracted or amplified; whether coup speculation subsides or intensifies.',
    'If reversible: scaffold classification gains support (temporary crisis with sunset via restoration). If irreversible: snare classification gains support from regime perspective (trapped in terminal decline).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fracture_reversibility, empirical, 'Whether elite legitimacy fracture can be reversed or is terminal').

omega_variable(
    defection_coordination_threshold,
    'What proportion of elite defections is required to trigger a coordination cascade vs. isolated dissent that can be suppressed?',
    'Network analysis of elite defection timing and clustering; identification of critical mass thresholds in historical authoritarian collapses; measurement of defection rate acceleration.',
    'If threshold not yet reached: current defections are noise, regime remains stable. If threshold exceeded: coordination cascade is underway, regime faces existential threat. Determines whether the constraint is primarily extractive (regime can suppress) or primarily coordinative (defectors can organize).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(defection_coordination_threshold, empirical, 'Critical mass threshold for elite defection cascade').

omega_variable(
    post_putin_coalition_coherence,
    'Do defecting elites share a coherent alternative vision (nationalist, technocratic, or other), or are they united only in opposition to Putin?',
    'Content analysis of defection statements; identification of shared policy positions vs. pure anti-Putin sentiment; assessment of whether defectors are coordinating on a successor regime or merely exiting a failing one.',
    'If coherent coalition exists: rope classification gains support from defector perspective (genuine coordination toward alternative). If incoherent: defections are individual exit strategies, not collective action (less coordination function, more extraction from regime).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_putin_coalition_coherence, conceptual, 'Whether defecting elites form a coherent alternative coalition').

omega_variable(
    suppression_capacity_exhaustion,
    'Has the regime''s suppression capacity been exhausted by the scale of elite defection, or does it retain the ability to arrest and silence dissent?',
    'Tracking arrest rates of senior officials over time; monitoring whether defection statements are retracted under pressure; assessing whether nationalist bloggers face consequences or continue operating; measuring regime response time to defection signals.',
    'If suppression capacity intact: regime can contain the fracture (higher suppression metric, snare from regime perspective). If exhausted: defection becomes safer, cascade accelerates (lower suppression metric, rope from defector perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_capacity_exhaustion, empirical, 'Whether regime retains capacity to suppress elite defection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(elite_legitimacy_fracture, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elite_frac_theater_2022_02, elite_legitimacy_fracture, theater_ratio, 0, 0.35).
narrative_ontology:measurement(elite_frac_theater_2022_08, elite_legitimacy_fracture, theater_ratio, 6, 0.42).
narrative_ontology:measurement(elite_frac_theater_2023_02, elite_legitimacy_fracture, theater_ratio, 12, 0.5).
narrative_ontology:measurement(elite_frac_theater_2023_08, elite_legitimacy_fracture, theater_ratio, 18, 0.55).
narrative_ontology:measurement(elite_frac_theater_2024_02, elite_legitimacy_fracture, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(elite_frac_extract_2022_02, elite_legitimacy_fracture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(elite_frac_extract_2022_08, elite_legitimacy_fracture, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(elite_frac_extract_2023_02, elite_legitimacy_fracture, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(elite_frac_extract_2023_08, elite_legitimacy_fracture, base_extractiveness, 18, 0.65).
narrative_ontology:measurement(elite_frac_extract_2024_02, elite_legitimacy_fracture, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(elite_frac_suppress_2022_02, elite_legitimacy_fracture, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(elite_frac_suppress_2022_08, elite_legitimacy_fracture, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(elite_frac_suppress_2023_02, elite_legitimacy_fracture, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(elite_frac_suppress_2023_08, elite_legitimacy_fracture, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(elite_frac_suppress_2024_02, elite_legitimacy_fracture, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(elite_legitimacy_fracture, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of military_defeat_cascade, deathonomics_collapse, control_mechanism_backfire, and public_confidence_erosion. Each upstream constraint contributes to the elite legitimacy fracture: military defeats remove the regime's performance legitimacy, economic collapse removes material incentives for loyalty, control mechanism backfire demonstrates regime weakness, and public confidence erosion removes the regime's mass base. The elite legitimacy fracture is a distinct structural constraint with its own extractiveness value (0.68) reflecting the career and personal costs of defection, separate from the upstream constraints' extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(elite_legitimacy_fracture, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
