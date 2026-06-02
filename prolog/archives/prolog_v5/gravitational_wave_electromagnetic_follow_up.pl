% ============================================================================
% CONSTRAINT STORY: gravitational_wave_electromagnetic_follow_up
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gravitational_wave_electromagnetic_follow_up, []).

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
 *   constraint_id: gravitational_wave_electromagnetic_follow_up
 *   human_readable: Gravitational Wave Electromagnetic Follow-Up Constraint
 *   domain: multi_messenger_astronomy/observational_astrophysics
 *
 * SUMMARY:
 *   Gravitational wave electromagnetic follow-up creates a structural
 *   constraint that coordinates multi-messenger astronomy — simultaneous
 *   observation of GW and EM signals enables source characterization
 *   impossible from either messenger alone. However, the follow-up protocol
 *   simultaneously extracts through asymmetric resource allocation,
 *   interrupts long-baseline survey programs, and entrenches infrastructure
 *   inequality between well-resourced (primarily Western) observatory
 *   networks and under-resourced (primarily non-Western) networks. The
 *   constraint exhibits tangled_rope structure: genuine coordination function
 *   (multi-messenger science requires rapid synchronized observations)
 *   combined with persistent extraction (survey interruption, latency-based
 *   inequality, theater in formal approval processes). The extractiveness has
 *   increased over the 10-year interval as GW detection rates improved and
 *   alert distribution became more sophisticated, but the underlying
 *   coordination asymmetry remained unchanged. Theater ratio has risen as
 *   formal approval committees have become increasingly separated from
 *   operational decision-making that occurs within GW detection pipelines and
 *   first-responder observatory networks.
 *
 * KEY AGENTS:
 *   - First-Responder Observatory Network: Primary beneficiary (institutional/arbitrage) — receives alerts first, claims publication priority, benefits from infrastructure investment in rapid-response instrumentation
 *   - Small Aperture Observatory: Primary victim (powerless/trapped) — forced to suspend programs for follow-up; has no alternative GW alert access pathway
 *   - Deep Survey Program: Primary victim (powerless/trapped) — survey continuity interrupted by alerts; cannot exit without losing access to multi-messenger data
 *   - Regional Observatory Director: Secondary actor (moderate/constrained) — manages tension between survey commitments and follow-up obligations; constrained exit through career/funding incentives
 *   - Non-Western Observatory Consortium: Secondary victim (organized/constrained) — included in alert distribution but with latency disadvantage; infrastructure inequality enforced through cost barriers and prioritization decisions
 *   - Observatory Time Allocation Committee: Institutional actor (institutional/arbitrage) — formal authority over follow-up scheduling; actual decision-making locus has migrated to alert pipelines
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees constraint as genuine multi-messenger coordination with embedded inequality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gravitational_wave_electromagnetic_follow_up, 0.52).
domain_priors:suppression_score(gravitational_wave_electromagnetic_follow_up, 0.48).
domain_priors:theater_ratio(gravitational_wave_electromagnetic_follow_up, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gravitational_wave_electromagnetic_follow_up, extractiveness, 0.52).
narrative_ontology:constraint_metric(gravitational_wave_electromagnetic_follow_up, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(gravitational_wave_electromagnetic_follow_up, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gravitational_wave_electromagnetic_follow_up, tangled_rope).
narrative_ontology:human_readable(gravitational_wave_electromagnetic_follow_up, "Gravitational Wave Electromagnetic Follow-Up Constraint").
narrative_ontology:topic_domain(gravitational_wave_electromagnetic_follow_up, "multi_messenger_astronomy/observational_astrophysics").

domain_priors:requires_active_enforcement(gravitational_wave_electromagnetic_follow_up).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gravitational_wave_electromagnetic_follow_up, first_responder_observatories).
narrative_ontology:constraint_beneficiary(gravitational_wave_electromagnetic_follow_up, astrophysics_priority_setting_committees).
narrative_ontology:constraint_victim(gravitational_wave_electromagnetic_follow_up, deep_survey_programs).
narrative_ontology:constraint_victim(gravitational_wave_electromagnetic_follow_up, small_aperture_astronomers).
narrative_ontology:constraint_victim(gravitational_wave_electromagnetic_follow_up, non_western_observatories).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL APERTURE OBSERVER (SNARE) — Trapped by the follow-up cascade. Once a GW alert fires, observatory time becomes conscripted for rapid response. The observer loses agency over their observing program; cannot exit without losing access to future multi-messenger data. Suppression is structural: no alternative follow-up pathway exists that grants access to rapid alerts and source localization data.
constraint_indexing:constraint_classification(gravitational_wave_electromagnetic_follow_up, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEEP SURVEY PROGRAM (SNARE) — Long-baseline surveys (photometric redshift calibration, faint galaxy evolution studies) are systematically interrupted by GW alerts. Observatories suspend survey operations for rapid response. The survey cannot exit — its science depends on continuous observing runs, yet the constraints prevent this. Bears extraction (lost survey data) with no compensation and no alternative.
constraint_indexing:constraint_classification(gravitational_wave_electromagnetic_follow_up, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL OBSERVATORY DIRECTOR (TANGLED ROPE) — Experiences both coordination benefit (access to GW alerts enables multi-messenger science, improving publication output and funding competitiveness) and extraction (must interrupt programs, manage rapid reallocation, operate instruments outside designed parameters under time pressure). Has constrained exit: can lobby for reduced alert participation but risks losing multi-messenger program access.
constraint_indexing:constraint_classification(gravitational_wave_electromagnetic_follow_up, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FIRST-RESPONDER OBSERVATORY NETWORK (ROPE) — Primary beneficiary. Benefits from alert priority, publication priority (first EM counterpart claims), funding allocation for rapid-response instrumentation. Experiences the follow-up constraint as pure coordination: alerts enable their science program. Network has full arbitrage: can set alert timing, coordinate with GW pipelines, adjust participation levels.
constraint_indexing:constraint_classification(gravitational_wave_electromagnetic_follow_up, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NON-WESTERN OBSERVATORY CONSORTIUM (TANGLED ROPE) — Benefits from access to GW alerts and international collaboration but experiences extraction through alert latency and infrastructure asymmetry. Receives alert data with delay; cannot achieve rapid follow-up with same capability as Western networks. Has some coordination function (their observations constrain source properties) but extraction through information asymmetry and infrastructure inequality. Constrained exit: infrastructure investment required to match latency.
constraint_indexing:constraint_classification(gravitational_wave_electromagnetic_follow_up, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: OBSERVATORY TIME ALLOCATION COMMITTEE (PITON) — The formal approval process for follow-up observations is substantially theatrical. Committees meet after alerts arrive; decisions ratify what observatory directors have already operationally executed. The committee process persists through institutional inertia — it appears to govern rapid-response decisions while actually rubber-stamping operational reality. Theater ratio is high; functional decision-making has migrated to alert-detection pipelines and alert distribution networks.
constraint_indexing:constraint_classification(gravitational_wave_electromagnetic_follow_up, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the follow-up constraint coordinates multi-messenger science (genuine function: correlating GW and EM observables requires rapid synchronized observations) while extracting through asymmetric resource allocation and institutional inequality. The constraint is neither pure coordination nor pure extraction — it is stabilized by genuine scientific benefit for participating institutions alongside systematic extraction from non-participating or under-resourced programs.
constraint_indexing:constraint_classification(gravitational_wave_electromagnetic_follow_up, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gravitational_wave_electromagnetic_follow_up_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gravitational_wave_electromagnetic_follow_up, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gravitational_wave_electromagnetic_follow_up, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gravitational_wave_electromagnetic_follow_up, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gravitational_wave_electromagnetic_follow_up, TR),
    TR >= 0.70.

:- end_tests(gravitational_wave_electromagnetic_follow_up_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and increasing. Initial extractiveness (0.25) reflected early multi-messenger detections where follow-up coordination yielded high science return with manageable survey interruption. Current extractiveness (0.52) reflects that follow-up alert frequency has increased faster than observatory capacity has expanded, creating sustained extraction from survey programs. The upward trajectory shows rent-seeking layering onto the coordination function — institutions defending follow-up priority by constraining alternative access pathways. Suppression (0.48): Moderate. Barriers to non-participation include epistemic (being excluded from multi-messenger data), economic (cannot fund rapid-response instrumentation without alert access), and institutional (follow-up participation is now standard expectation for observatory approval committees). However, suppression is not total — some observatories successfully negotiate reduced alert participation; some surveys operate on non-GW triggers. Theater ratio (0.65): High. The formal Observatory Time Allocation Committee process is substantially theatrical. Committees meet after operational decisions have effectively been made by GW pipelines. The theater serves to rationalize extraction post-hoc rather than to govern it prospectively.
 *
 * PERSPECTIVAL GAP:
 *   The first-responder network and small observatory occupy opposite ends of the directionality spectrum: beneficiary with low d vs victim with high d. Their perspectives (Rope vs Snare) accurately reflect their structural positions. The regional director occupies the middle (Tangled Rope) — real benefits from multi-messenger access but real costs from program interruption. The non-Western consortium is structurally similar to the regional director (mixed benefits and costs) but with the added asymmetry of latency inequality — they see Tangled Rope but with higher experienced extraction due to the latency tax. The analytical observer (Tangled Rope) sees the constraint as coordinating while simultaneously extracting, which is structurally accurate but risks naturalizing the inequality as necessary (the omega variable on alert latency necessity tests this).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is determined by the agent's relationship to the alert distribution and follow-up resource flow. First-responder networks (beneficiary + arbitrage) derive d ≈ 0.05, producing negative χ (experienced as coordination benefit). Small observatories (victim + trapped) derive d ≈ 0.95, producing maximum f(d) ≈ 1.42 (maximum experienced extraction). Regional directors (victim + constrained, but also partial beneficiary through publication access) derive d ≈ 0.60, producing moderate χ ≈ 0.50 (tangled experience). Non-Western consortiums (victim + constrained, but with institutional power organizing) derive d ≈ 0.72, producing f(d) ≈ 1.15 (high extraction but not maximum, because organized power provides some negotiating capacity). The committee (institutional/arbitrage with ceremonial function) derives d ≈ 0.15 from its formal authority, but actual decision locus has migrated — explaining the piton classification (theater high but function low).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing that the multi-messenger coordination function is genuine and the extraction is real and structural. Neither pure coordination nor pure extraction explains the constraint. The tangled_rope classification correctly identifies both the coordination benefit (multi-messenger science enables discoveries impossible from single messengers) and the extraction asymmetry (resource allocation to first-responder networks, survey interruption for others, latency inequality for non-Western observatories). The theater_ratio elevation (0.65) reflects that formal approval processes have become performative relative to operational reality — the real gatekeeping happens in alert distribution pipelines. The mandatrophy resolution is that the constraint is not mislabeled as pure extraction; the existence of genuine multi-messenger science coordination prevents snare classification at the analytical level. However, the individual victim perspectives (small observatory, survey program) correctly classify the constraint as snare from their structural position — they experience only extraction, not coordination. The presheaf structure (different types from different observatories) is the complete answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alert_latency_irreducibility,
    'What portion of the follow-up constraint''s extraction is due to irremediable physical/computational limits on GW source localization and what portion is due to institutional choice about alert distribution latency?',
    'Technical analysis of GW pipeline processing times vs alert distribution network latency; comparison of minimum achievable localization time vs actual alert distribution delays across observatory networks',
    'If latency is physically irreducible: suppression is structurally inherent (reclassify as more mountain-like). If latency is institutional (alert prioritization, infrastructure inequality): suppression is imposed through choice (confirms snare/tangled_rope classification). Directionality of extraction changes: if institutional, first-responder networks are actively maintaining inequalities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alert_latency_irreducibility, empirical, 'Whether alert latency is physical or institutional constraint').

omega_variable(
    survey_interruption_necessity,
    'Do the vast majority of GW alerts require immediate observation, or could the follow-up cascade operate with a delayed-observation window that preserves survey continuity?',
    'Analysis of source discovery rate vs time delay post-alert; comparison of science yield from immediate follow-up vs delayed follow-up for different event types (BNS, NSBH, BBH). Simulation of survey program recovery if 6-hour observation windows were reserved for follow-ups instead of immediate conscription.',
    'If delayed follow-up recovers 80%+ science yield: the interruption constraint is extractive choice, not necessity (confirms snare classification for surveys). If immediate follow-up is necessary: extraction is coordinated with genuine scientific function (confirms tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survey_interruption_necessity, empirical, 'Whether GW follow-up requires immediate observation or can be delayed').

omega_variable(
    non_western_infrastructure_catch_up_timeline,
    'What is the realistic timeline for non-Western observatories to achieve GW alert latency parity with existing first-responder networks, and what are the political/economic barriers that extend this timeline beyond technical necessity?',
    'Cost-benefit analysis of latency-reducing infrastructure (dedicated fiber, local alert processing nodes); documentation of funding allocation decisions and advocacy barriers for non-Western observatory upgrades; comparison of build times for similar infrastructure in Western vs non-Western contexts',
    'If barriers are primarily economic (cost of infrastructure): extraction is remediable through funding allocation. If barriers are institutional (strategic gatekeeping, access denial): extraction is maintained through choice. Determines whether the non-Western observatory constraint is transient (tangled_rope with sunset) or structural (permanent snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_western_infrastructure_catch_up_timeline, empirical, 'Realistic timeline and barriers for non-Western observatory alert latency parity').

omega_variable(
    committee_decision_locus,
    'Does the Observatory Time Allocation Committee''s formal approval process actually influence GW follow-up decisions, or is the process theater with operational decisions already made by alert pipelines and observatory directors?',
    'Process analysis: comparison of committee meeting times vs alert arrival times; examination of cases where committee rejected or modified follow-up requests vs operational reality; interviews with directors on decision-making sequence',
    'If theater: committee process is institutional inertia (piton classification confirmed). If functional: committee has real gate power (tangled_rope classification for some actors). Theater_ratio adjustment reflects actual decision locus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committee_decision_locus, empirical, 'Whether time allocation committee is functional or theatrical in GW follow-up decisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gravitational_wave_electromagnetic_follow_up, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gwem_tr_t0, gravitational_wave_electromagnetic_follow_up, theater_ratio, 0, 0.48).
narrative_ontology:measurement(gwem_tr_t5, gravitational_wave_electromagnetic_follow_up, theater_ratio, 5, 0.58).
narrative_ontology:measurement(gwem_tr_t10, gravitational_wave_electromagnetic_follow_up, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(gwem_be_t0, gravitational_wave_electromagnetic_follow_up, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(gwem_be_t5, gravitational_wave_electromagnetic_follow_up, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(gwem_be_t10, gravitational_wave_electromagnetic_follow_up, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gravitational_wave_electromagnetic_follow_up, resource_allocation).
narrative_ontology:affects_constraint(gravitational_wave_electromagnetic_follow_up, gravitational_wave_detector_network_inequality).
narrative_ontology:affects_constraint(gravitational_wave_electromagnetic_follow_up, electromagnetic_survey_program_resource_allocation).

% DUAL FORMULATION NOTE:
% The GW-EM follow-up constraint is downstream of GW detector sensitivity improvements, which increase alert frequency, which increases extraction pressure on survey programs and non-first-responder observatories. The upstream GW detector network inequality (unequal distribution of detector infrastructure) shapes which observatories are first-responders and which are secondary participants. The follow-up constraint itself generates downstream resource allocation decisions that affect long-baseline survey design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gravitational_wave_electromagnetic_follow_up, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
