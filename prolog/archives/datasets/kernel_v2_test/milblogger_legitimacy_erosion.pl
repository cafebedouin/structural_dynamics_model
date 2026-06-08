% ============================================================================
% CONSTRAINT STORY: milblogger_legitimacy_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_milblogger_legitimacy_erosion, []).

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
 *   constraint_id: milblogger_legitimacy_erosion
 *   human_readable: Milblogger Legitimacy Erosion in Russian Information Space
 *   domain: military_operations_analysis/information_warfare/institutional_dysfunction
 *
 * SUMMARY:
 *   The milblogger legitimacy erosion constraint describes the structural
 *   collapse of Russian state military information authority during the
 *   2022-2024 period. Pro-Russian military commentators (milbloggers) —
 *   initially tolerated as patriotic voices — increasingly mocked official
 *   Ministry of Defense claims, warned of command dysfunction, and provided
 *   battlefield analysis that contradicted official narratives. The domestic
 *   Russian audience shifted trust from state sources to these non-state
 *   commentators, creating an information environment where the official
 *   apparatus continues to perform its function (press conferences, victory
 *   announcements, casualty denials) but the performance is widely recognized
 *   as theater. This is a resolved mandatrophy: the state information
 *   apparatus's mandate to shape domestic perception of military operations
 *   has outlived its function, but the apparatus persists because it has no
 *   alternative institutional identity. The constraint is downstream of two
 *   tangled_rope constraints: beautiful_reports_feedback_loop (which created
 *   the credibility gap milbloggers exploit) and
 *   verification_authority_fragmentation (which enabled alternative
 *   information sources to gain legitimacy). The theater_ratio trajectory
 *   shows steady increase from 0.35 to 0.78 as official statements became
 *   increasingly performative. The extractiveness trajectory shows steady
 *   decrease from 0.45 to 0.28 as the constraint shifted from active
 *   information control (extractive) to degraded ritual (inertial). The
 *   suppression_requirement trajectory shows decay from 0.65 to 0.35 as the
 *   state's capacity to enforce information monopoly eroded — not because
 *   policy changed, but because enforcement became ineffective once the
 *   audience had credible alternatives.
 *
 * KEY AGENTS:
 *   - State Military Authority: Primary victim (institutional/constrained) — official information apparatus that continues performing despite audience disbelief; trapped in credibility death spiral
 *   - Milblogger Commentators: Primary beneficiary (moderate/mobile) — provide battlefield analysis that official sources cannot; audience growth and influence without severe state suppression
 *   - Domestic Audience: Secondary beneficiary (powerless/mobile) — gains access to more credible information; can choose sources
 *   - Official Information Apparatus: Institutional victim (institutional/trapped) — the bureaucratic structure that produces official statements; cannot exit the performance without admitting institutional failure
 *   - Military Command Structure: Mixed position (organized/constrained) — benefits from feedback pressure but suffers authority erosion; cannot suppress criticism without further legitimacy loss
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees degraded information control regime maintained through institutional inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(milblogger_legitimacy_erosion, 0.28).
domain_priors:suppression_score(milblogger_legitimacy_erosion, 0.35).
domain_priors:theater_ratio(milblogger_legitimacy_erosion, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(milblogger_legitimacy_erosion, extractiveness, 0.28).
narrative_ontology:constraint_metric(milblogger_legitimacy_erosion, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(milblogger_legitimacy_erosion, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(milblogger_legitimacy_erosion, piton).
narrative_ontology:human_readable(milblogger_legitimacy_erosion, "Milblogger Legitimacy Erosion in Russian Information Space").
narrative_ontology:topic_domain(milblogger_legitimacy_erosion, "military_operations_analysis/information_warfare/institutional_dysfunction").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(milblogger_legitimacy_erosion, milblogger_commentators).
narrative_ontology:constraint_beneficiary(milblogger_legitimacy_erosion, domestic_audience_seeking_truth).
narrative_ontology:constraint_victim(milblogger_legitimacy_erosion, state_military_authority).
narrative_ontology:constraint_victim(milblogger_legitimacy_erosion, official_information_apparatus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE MILITARY AUTHORITY (PITON) — The official information apparatus continues to issue optimistic reports and victory narratives, but the function has atrophied. What remains is theatrical maintenance: press conferences, official statements, and ministry briefings that the domestic audience no longer believes. The authority structure persists through institutional inertia, not because it successfully shapes perception. Constrained exit because abandoning the performance would be an admission of dysfunction.
constraint_indexing:constraint_classification(milblogger_legitimacy_erosion, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: MILBLOGGER COMMENTATORS (ROPE) — Experience the constraint as coordination: they are solving the genuine problem of providing battlefield analysis that the official apparatus cannot or will not provide. Mobile exit (can stop posting, shift platforms, or moderate criticism) and net beneficiaries (audience growth, influence, some protection from state due to audience size). Low effective extraction — the constraint enables their function rather than extracting from it.
constraint_indexing:constraint_classification(milblogger_legitimacy_erosion, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMESTIC AUDIENCE (ROPE) — The audience benefits from access to more credible battlefield analysis. Mobile exit (can choose to consume official media, ignore both, or seek foreign sources) and experiences the milblogger ecosystem as coordination rather than extraction. The constraint solves their information problem at low cost.
constraint_indexing:constraint_classification(milblogger_legitimacy_erosion, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: OFFICIAL INFORMATION APPARATUS (SNARE) — Trapped in a credibility death spiral. Cannot exit the performance (stopping would admit defeat), cannot restore credibility (the audience has alternative sources), and bears the full cost of legitimacy erosion. The apparatus is structurally locked into producing content that its own audience mocks. Maximum experienced extraction from a generational perspective — the institutional function is collapsing but the institution cannot stop performing it.
constraint_indexing:constraint_classification(milblogger_legitimacy_erosion, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: MILITARY COMMAND STRUCTURE (TANGLED ROPE) — Experiences both coordination and extraction. Coordination: milblogger criticism creates feedback pressure that can (in principle) improve operational planning. Extraction: public mockery undermines command authority and morale, and the command structure cannot suppress the criticism without further legitimacy loss. Constrained exit (cannot fully silence milbloggers without domestic backlash) and mixed beneficiary/victim status.
constraint_indexing:constraint_classification(milblogger_legitimacy_erosion, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — From a civilizational perspective, this is a degraded information control regime. The state's monopoly on military narrative was once functional (Soviet era), then became extractive (early Putin era), and has now atrophied into performance. What persists is the ritual of official briefings and ministry statements, maintained because the institutional apparatus has no alternative identity. The primary function (shaping domestic perception of military operations) has collapsed, but the constraint remains due to institutional inertia.
constraint_indexing:constraint_classification(milblogger_legitimacy_erosion, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(milblogger_legitimacy_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(milblogger_legitimacy_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(milblogger_legitimacy_erosion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(milblogger_legitimacy_erosion, TR),
    TR >= 0.70.

:- end_tests(milblogger_legitimacy_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The constraint extracts from state military authority (legitimacy loss, credibility erosion) but the extraction has declined over the interval as the constraint shifted from active control to degraded ritual. Initial extractiveness (0.45) reflected genuine information suppression and audience manipulation; final extractiveness (0.28) reflects mostly inertial performance with minimal functional extraction. The state apparatus continues to pay reputational costs, but the mechanism is no longer actively extractive — it is passively theatrical. Suppression (0.35): Moderate-low. The state retains legal and technical capacity to suppress milbloggers (arrests, platform bans, censorship) but enforcement has become sporadic and ineffective. Initial suppression (0.65) reflected active information control; final suppression (0.35) reflects enforcement decay as the audience gained credible alternatives and suppression became counterproductive (Streisand effect). The suppression trajectory models enforcement attrition, not policy liberalization. Theater ratio (0.78): High. The official information apparatus's primary function (shaping domestic perception) has atrophied, but the performance persists. Press conferences, ministry briefings, and official statements continue on schedule, but the content is widely recognized as disconnected from battlefield reality. The theater_ratio trajectory models the gradual recognition by both the apparatus and its audience that the performance is no longer functional. Initial theater_ratio (0.35) reflected a still-credible information apparatus; final theater_ratio (0.78) reflects near-total functional collapse with ritual maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The state military authority sees piton — a degraded function maintained through inertia. Milbloggers see rope — they are solving a genuine coordination problem (providing accurate battlefield analysis). The domestic audience also sees rope — the milblogger ecosystem solves their information problem at low cost. The official information apparatus sees snare — it is trapped in a credibility death spiral, cannot exit the performance, and bears maximum extraction from a generational perspective. The military command structure sees tangled_rope — it experiences both coordination (feedback pressure) and extraction (authority erosion). The analytical observer sees piton from a civilizational perspective — a degraded information control regime where the primary function has collapsed but the ritual persists. The perspectival gap reveals that what looks like coordination from below (milbloggers and audience) looks like institutional collapse from within (the apparatus itself) and looks like degraded performance from the analytical distance. All perspectives are structurally valid readings of the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   State military authority is the primary victim — it bears the legitimacy cost and cannot exit the performance. The engine derives high d from victim status + constrained exit, producing high experienced extraction despite moderate base extractiveness. Milblogger commentators are primary beneficiaries with mobile exit options — they can moderate criticism, shift platforms, or stop posting without severe cost. The engine derives low d from beneficiary status + mobile exit, producing low or negative experienced extraction (the constraint subsidizes their function). The domestic audience is a secondary beneficiary with mobile exit — they benefit from information access and can choose sources freely. The official information apparatus (as distinct from state military authority) is an institutional victim with trapped exit — it cannot stop performing without admitting institutional failure, and it bears the full reputational cost of the credibility collapse. The military command structure has mixed directionality — it is both victim (authority erosion) and beneficiary (feedback pressure that can improve decisions), with constrained exit (cannot fully suppress criticism without backlash). No directionality overrides are needed — the structural declarations (beneficiaries, victims, exit options) sufficiently differentiate the agents.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: The state information apparatus's mandate to shape domestic perception of military operations has outlived its function. The apparatus continues to perform (press conferences, official statements, victory narratives) but the performance no longer achieves its intended effect — the domestic audience trusts non-state sources over state sources. The constraint is a piton because the function has atrophied but the structure persists through institutional inertia. The apparatus has no alternative identity: it cannot admit that its core function has failed, it cannot stop performing without dissolving the institution, and it cannot restore credibility because the audience has access to more credible alternatives. The mandatrophy is resolved in the sense that the constraint's original purpose (information control) is no longer operative, but the constraint persists as ritual. The theater_ratio trajectory (0.35 → 0.78) models the gradual recognition of this mandatrophy by both the apparatus and its audience. The extractiveness trajectory (0.45 → 0.28) models the shift from active extraction (information suppression) to passive theater (ritual maintenance). The suppression_requirement trajectory (0.65 → 0.35) models enforcement decay as the state's capacity to maintain information monopoly eroded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    milblogger_suppression_threshold,
    'At what point does state tolerance of milblogger criticism flip to suppression, and what triggers the flip?',
    'Historical analysis of state responses to milblogger criticism; identification of red lines (direct criticism of leadership vs criticism of operational decisions); tracking of arrests, platform bans, or forced retractions',
    'If threshold is low and enforcement is rising: constraint shifts from piton toward snare (suppression increases). If threshold is high and stable: piton classification holds (performance persists without effective enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(milblogger_suppression_threshold, empirical, 'State suppression threshold for milblogger criticism').

omega_variable(
    audience_trust_recovery_path,
    'Can the official information apparatus recover credibility, or is the legitimacy loss irreversible within the current institutional structure?',
    'Longitudinal tracking of audience trust metrics; comparison with historical cases of information apparatus credibility collapse and recovery (e.g., post-Vietnam US military, post-Afghanistan Soviet military)',
    'If recovery is possible: piton is temporary and constraint could revert to rope or tangled_rope. If irreversible: piton is terminal state and mandatrophy is fully resolved (the apparatus''s mandate to shape perception has outlived its function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(audience_trust_recovery_path, empirical, 'Whether official apparatus can recover domestic credibility').

omega_variable(
    milblogger_coordination_vs_extraction,
    'Are milbloggers primarily solving a coordination problem (providing accurate battlefield analysis) or extracting rents (building influence through contrarianism regardless of accuracy)?',
    'Accuracy tracking of milblogger claims vs official claims vs verified battlefield outcomes; correlation analysis between milblogger criticism severity and actual operational failures',
    'If primarily coordination: milblogger perspectives remain rope. If primarily extraction: milblogger perspectives shift toward tangled_rope or snare (they benefit from dysfunction rather than solving it).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(milblogger_coordination_vs_extraction, empirical, 'Whether milbloggers coordinate or extract').

omega_variable(
    command_dysfunction_causality,
    'Does milblogger criticism cause command dysfunction (by undermining authority), reflect pre-existing dysfunction (by reporting it), or both?',
    'Causal analysis: timeline comparison of operational failures vs milblogger criticism; identification of cases where criticism preceded vs followed command decisions; assessment of whether command decisions changed in response to milblogger pressure',
    'If causal: milbloggers are extractive (creating the problem they report). If reflective: milbloggers are coordinative (solving information asymmetry). If both: tangled_rope from more perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(command_dysfunction_causality, empirical, 'Causal relationship between milblogger criticism and command dysfunction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(milblogger_legitimacy_erosion, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(milblog_theater_t0, milblogger_legitimacy_erosion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(milblog_theater_t6, milblogger_legitimacy_erosion, theater_ratio, 6, 0.52).
narrative_ontology:measurement(milblog_theater_t12, milblogger_legitimacy_erosion, theater_ratio, 12, 0.68).
narrative_ontology:measurement(milblog_theater_t18, milblogger_legitimacy_erosion, theater_ratio, 18, 0.75).
narrative_ontology:measurement(milblog_theater_t24, milblogger_legitimacy_erosion, theater_ratio, 24, 0.78).

% Extraction over time
narrative_ontology:measurement(milblog_extract_t0, milblogger_legitimacy_erosion, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(milblog_extract_t6, milblogger_legitimacy_erosion, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(milblog_extract_t12, milblogger_legitimacy_erosion, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(milblog_extract_t18, milblogger_legitimacy_erosion, base_extractiveness, 18, 0.3).
narrative_ontology:measurement(milblog_extract_t24, milblogger_legitimacy_erosion, base_extractiveness, 24, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(milblog_suppress_t0, milblogger_legitimacy_erosion, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(milblog_suppress_t6, milblogger_legitimacy_erosion, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(milblog_suppress_t12, milblogger_legitimacy_erosion, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(milblog_suppress_t18, milblogger_legitimacy_erosion, suppression_requirement, 18, 0.4).
narrative_ontology:measurement(milblog_suppress_t24, milblogger_legitimacy_erosion, suppression_requirement, 24, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(milblogger_legitimacy_erosion, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of beautiful_reports_feedback_loop (which created the credibility gap) and verification_authority_fragmentation (which enabled alternative sources). The milblogger legitimacy erosion is the terminal state of an information control regime that has collapsed into performance. It does not affect downstream constraints because it is the endpoint of the causal chain — the state apparatus has no remaining credibility to lose.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
