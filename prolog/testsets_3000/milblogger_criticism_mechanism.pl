% ============================================================================
% CONSTRAINT STORY: milblogger_criticism_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_milblogger_criticism_mechanism, []).

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
 *   constraint_id: milblogger_criticism_mechanism
 *   human_readable: Milblogger Criticism Mechanism in Russian Military Information Space
 *   domain: military_operations/information_warfare/organizational_pathology
 *
 * SUMMARY:
 *   The milblogger criticism mechanism emerged in the Russian military
 *   information space as a parallel channel for tactical ground-truth that
 *   official reporting structures systematically suppress. Pro-Russian
 *   independent military commentators (milbloggers) publicly contradict
 *   Ministry of Defense claims, report equipment failures and tactical
 *   setbacks that official channels conceal, and advocate for frontline
 *   personnel whose complaints are filtered out by hierarchical
 *   beautiful-reports dynamics. This constraint is structurally downstream of
 *   the beautiful-reports feedback loop: it exists because official channels
 *   are unreliable, and it functions by routing information around
 *   institutional dishonesty. The mechanism exhibits rope characteristics
 *   from all measured perspectives because it solves a genuine coordination
 *   problem (getting accurate tactical information to strategic
 *   decision-makers) with minimal extraction. The low extractiveness (0.18)
 *   reflects that the constraint benefits field commanders, tactical units,
 *   strategic planners, and civilian leadership by reducing information
 *   asymmetry, while imposing minimal costs on any identifiable victim group.
 *   The modest theater ratio (0.15) indicates that milblogger criticism is
 *   functionally effective rather than performative — the criticism produces
 *   observable policy responses and operational adjustments.
 *
 * KEY AGENTS:
 *   - Field Commanders: Primary beneficiary (moderate/mobile) — receive ground-truth feedback that official channels suppress; use milblogger criticism to validate tactical assessments
 *   - Tactical Units: Primary beneficiary (powerless/trapped) — milblogger advocacy amplifies otherwise-ignored complaints about equipment, supply, and tactical incompetence
 *   - Strategic Planners: Institutional beneficiary (institutional/constrained) — independent information channel bypasses beautiful-reports filtering; reveals operational reality
 *   - Civilian Leadership: Institutional beneficiary (institutional/arbitrage) — milblogger criticism reduces principal-agent information asymmetry; checks military institutional dishonesty
 *   - Reform-Oriented Military Factions: Organized beneficiary (organized/mobile) — use external criticism as pressure for internal institutional change
 *   - Analytical Observer: Sees low-extraction coordination mechanism routing information around hierarchical suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(milblogger_criticism_mechanism, 0.18).
domain_priors:suppression_score(milblogger_criticism_mechanism, 0.25).
domain_priors:theater_ratio(milblogger_criticism_mechanism, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(milblogger_criticism_mechanism, extractiveness, 0.18).
narrative_ontology:constraint_metric(milblogger_criticism_mechanism, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(milblogger_criticism_mechanism, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(milblogger_criticism_mechanism, rope).
narrative_ontology:human_readable(milblogger_criticism_mechanism, "Milblogger Criticism Mechanism in Russian Military Information Space").
narrative_ontology:topic_domain(milblogger_criticism_mechanism, "military_operations/information_warfare/organizational_pathology").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(milblogger_criticism_mechanism, field_commanders).
narrative_ontology:constraint_beneficiary(milblogger_criticism_mechanism, tactical_units).
narrative_ontology:constraint_beneficiary(milblogger_criticism_mechanism, strategic_planners).
narrative_ontology:constraint_beneficiary(milblogger_criticism_mechanism, civilian_leadership).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD COMMANDERS (ROPE) — Milblogger criticism provides rapid ground-truth feedback that official channels suppress. Commanders benefit from independent verification of tactical reality and early warning of systemic failures. Low extraction — the mechanism coordinates information flow around institutional blockages.
constraint_indexing:constraint_classification(milblogger_criticism_mechanism, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: TACTICAL UNITS (ROPE) — Frontline personnel benefit from milblogger advocacy when official channels ignore equipment failures, supply breakdowns, or tactical incompetence. The criticism mechanism amplifies their otherwise-unheard complaints to audiences that include decision-makers. Despite trapped exit options, extraction is low because the constraint solves a coordination problem (getting accurate tactical information to strategic level).
constraint_indexing:constraint_classification(milblogger_criticism_mechanism, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: STRATEGIC PLANNERS (ROPE) — Institutional actors constrained by hierarchical reporting structures benefit from independent information channel that bypasses beautiful-reports filtering. Milblogger criticism reveals operational reality that subordinates conceal. Low extraction — the mechanism provides coordination value by routing around institutional suppression of negative information.
constraint_indexing:constraint_classification(milblogger_criticism_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVILIAN LEADERSHIP (ROPE) — Political leadership with arbitrage exit options benefits from milblogger criticism as a check on military institutional dishonesty. The parallel information channel reduces principal-agent information asymmetry. Minimal extraction — the constraint coordinates information flow between hierarchical levels that official channels distort.
constraint_indexing:constraint_classification(milblogger_criticism_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM-ORIENTED MILITARY FACTIONS (ROPE) — Organized internal reformers use milblogger criticism as external pressure for institutional change. The mechanism amplifies reform arguments by making operational failures publicly undeniable. Low extraction — coordination function dominates.
constraint_indexing:constraint_classification(milblogger_criticism_mechanism, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From analytical perspective, milblogger criticism is a low-extraction coordination mechanism that routes information around hierarchical suppression. The constraint solves a genuine collective action problem (accurate tactical information reaching strategic decision-makers) with minimal coercive overhead. Effective extraction is low across all perspectives because beneficiaries vastly outnumber any potential victims, and the coordination function is genuine.
constraint_indexing:constraint_classification(milblogger_criticism_mechanism, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(milblogger_criticism_mechanism_tests).
:- end_tests(milblogger_criticism_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The milblogger criticism mechanism imposes minimal costs while providing substantial coordination benefits. Field commanders gain tactical validation, frontline units gain advocacy, strategic planners gain accurate information, and civilian leadership gains oversight. No identifiable victim group bears significant extraction — the constraint's costs fall on institutional dishonesty itself (the beautiful-reports dynamic), not on specific agents. The slight increase over the interval (0.15 → 0.18) reflects growing tension between milblogger criticism and command authority, but extraction remains well below coordination thresholds. Suppression (0.25): Low-moderate. The Russian state tolerates milblogger criticism within boundaries — suppression occurs when criticism crosses into direct challenges to political leadership or reveals operationally sensitive information, but tactical criticism of military performance is largely permitted. This tolerance reflects that the mechanism serves regime interests by providing ground-truth feedback and scapegoating military leadership for failures. Theater ratio (0.15): Very low. Milblogger criticism is functionally effective — it produces observable policy responses (equipment procurement changes, tactical adjustments, personnel removals) rather than performative gestures. The mechanism's low theater content distinguishes it from official military communications, which have much higher performative ratios.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all measured perspectives classify as rope because the coordination function dominates from every structural position. The uniformity is diagnostically significant: it indicates that the constraint solves a collective action problem (accurate information flow) that benefits agents across power levels, time horizons, and exit options. The slight variation in effective extraction across perspectives (powerless/trapped agents experience slightly higher chi than institutional/arbitrage agents due to the sigmoid function) does not change the classification because all values remain well below rope thresholds. The constraint's uniformity distinguishes it from its upstream dependency (beautiful-reports feedback loop, classified as tangled_rope) — where beautiful-reports exhibits significant perspectival gaps between beneficiaries and victims, milblogger criticism shows coordination across perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   All measured perspectives derive low directionality values because all agents are beneficiaries of the coordination function. Field commanders (moderate/mobile/beneficiary) experience the constraint as solving the problem of getting accurate tactical feedback when official channels suppress it. Tactical units (powerless/trapped/beneficiary) experience the constraint as amplifying their otherwise-unheard complaints — despite trapped exit options, they are beneficiaries rather than victims, so directionality is low and effective extraction is minimal. Strategic planners and civilian leadership (institutional/constrained or arbitrage/beneficiary) experience the constraint as reducing information asymmetry imposed by subordinate dishonesty. The analytical observer sees a coordination mechanism with genuine function and minimal extraction across all structural positions. No perspective produces high directionality because no agent group is a victim — the constraint's 'target' is institutional dishonesty (the beautiful-reports dynamic), not a specific agent class.
 *
 * MANDATROPHY ANALYSIS:
 *   The milblogger criticism mechanism resolves potential mandatrophy by demonstrating that low extraction and genuine coordination function can coexist with institutional tension. The constraint undermines command authority (creating friction with hierarchical control) while simultaneously serving regime interests (providing ground-truth feedback and scapegoating mechanisms). This is not extraction disguised as coordination — it is coordination that produces institutional friction as a side effect. The low extractiveness (0.18) and low suppression (0.25) confirm that the mechanism is not a snare or tangled rope: no agent group is systematically victimized, and the state's tolerance (despite authority erosion) reveals that the coordination benefits outweigh the institutional costs. The constraint's classification as rope from all perspectives, including the analytical observer, indicates that the coordination function is genuine rather than theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_tolerance_threshold,
    'At what point does milblogger criticism cross from tolerated feedback mechanism to suppressed dissent?',
    'Longitudinal tracking of milblogger censorship events; correlation between criticism severity and state response; identification of red-line topics that trigger suppression',
    'If threshold is low and frequently enforced: constraint reclassifies as scaffold (temporary tolerance) or tangled_rope (mixed coordination and suppression). If threshold is high and rarely enforced: rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_tolerance_threshold, empirical, 'State tolerance threshold for milblogger criticism before suppression').

omega_variable(
    coordination_vs_fragmentation,
    'Does milblogger criticism coordinate information flow or fragment command authority?',
    'Analysis of decision-maker responses to milblogger warnings vs. official reports; measurement of command coherence metrics before/after major milblogger criticism episodes; tracking of policy changes attributable to milblogger pressure vs. official channels',
    'If coordination dominates: rope confirmed. If fragmentation dominates: reclassify as tangled_rope (coordination function exists but extraction via authority erosion is significant).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_fragmentation, empirical, 'Whether mechanism coordinates or fragments command structure').

omega_variable(
    beautiful_reports_dependency,
    'Is milblogger criticism mechanism structurally dependent on the beautiful-reports feedback loop, or would it persist if official reporting became accurate?',
    'Counterfactual analysis: in military organizations with accurate internal reporting, do independent critic channels emerge? Historical comparison of milblogger-equivalent phenomena across different institutional reporting cultures.',
    'If structurally dependent: milblogger mechanism is a compensatory adaptation to beautiful-reports pathology, not an independent coordination mechanism. If independent: mechanism has intrinsic coordination value beyond compensating for institutional dishonesty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beautiful_reports_dependency, conceptual, 'Structural dependency on beautiful-reports feedback loop').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(milblogger_criticism_mechanism, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(milblog_tr_t0, milblogger_criticism_mechanism, theater_ratio, 0, 0.1).
narrative_ontology:measurement(milblog_tr_t6, milblogger_criticism_mechanism, theater_ratio, 6, 0.12).
narrative_ontology:measurement(milblog_tr_t12, milblogger_criticism_mechanism, theater_ratio, 12, 0.15).

% Extraction over time
narrative_ontology:measurement(milblog_be_t0, milblogger_criticism_mechanism, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(milblog_be_t6, milblogger_criticism_mechanism, base_extractiveness, 6, 0.16).
narrative_ontology:measurement(milblog_be_t12, milblogger_criticism_mechanism, base_extractiveness, 12, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(milblogger_criticism_mechanism, information_standard).

% DUAL FORMULATION NOTE:
% The milblogger criticism mechanism is structurally downstream of the beautiful-reports feedback loop. It exists because official reporting channels are unreliable (beautiful-reports dynamic suppresses negative information), and it functions by routing tactical ground-truth around institutional dishonesty. The two constraints have different extractiveness values reflecting their different structural positions: beautiful-reports (ε ≈ 0.55, tangled_rope) extracts from field-level honesty and epistemic reliability, while milblogger criticism (ε = 0.18, rope) coordinates information flow with minimal extraction. The network relationship is asymmetric: beautiful-reports creates the information vacuum that milblogger criticism fills, but milblogger criticism does not structurally affect beautiful-reports (institutional dishonesty persists regardless of external criticism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
