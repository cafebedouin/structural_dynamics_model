% ============================================================================
% CONSTRAINT STORY: legibility_asymmetry_as_survival
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legibility_asymmetry_as_survival, []).

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
 *   constraint_id: legibility_asymmetry_as_survival
 *   human_readable: Legibility Asymmetry as Survival Strategy
 *   domain: social_ontology/power_dynamics/collective_memory
 *
 * SUMMARY:
 *   Legibility asymmetry as a survival strategy emerges when communities
 *   under surveillance develop dual-register operation: maintaining elevated,
 *   stable compliance metrics in registers the surveillance apparatus
 *   monitors while preserving cultural practices through substrate signal
 *   complexity the apparatus cannot decode. This constraint is downstream of
 *   substrate_as_unrecognized_archive (the structural fact that practices can
 *   be encoded in substrates surveillance does not recognize as
 *   information-bearing). The legibility asymmetry is the active coordination
 *   mechanism that exploits this structural property. Communities coordinate
 *   on which registers to make legible (compliance performance) and which to
 *   keep opaque (substrate complexity), enabling practice transmission across
 *   generations despite hostile observation. The constraint exhibits low
 *   extraction (0.18) because it solves a genuine coordination problem: how
 *   to satisfy surveillance requirements without abandoning cultural
 *   continuity. The theater ratio (0.28) reflects that some compliance
 *   performance is genuinely performative (satisfying metrics without
 *   substantive change) but much of it serves the dual function of deflecting
 *   attention while substrate work proceeds.
 *
 * KEY AGENTS:
 *   - Monitored Communities: Primary beneficiary (powerless/constrained) — use legibility asymmetry to survive surveillance while preserving practices
 *   - Substrate Practitioners: Primary beneficiary (moderate/mobile) — maintain substrate complexity across generations; coordinate encoding protocols
 *   - Surveillance Apparatus: Institutional observer (institutional/arbitrage) — perceives elevated compliance as success; cannot decode substrate complexity
 *   - Diaspora Network: Organized beneficiary (organized/mobile) — coordinate substrate preservation across multiple surveillance regimes
 *   - Archival Activists: Organized beneficiary with sunset view (organized/constrained) — work to make substrate legible to future generations after surveillance threat passes
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees legibility asymmetry as coordination solution to practice preservation under hostile observation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legibility_asymmetry_as_survival, 0.18).
domain_priors:suppression_score(legibility_asymmetry_as_survival, 0.32).
domain_priors:theater_ratio(legibility_asymmetry_as_survival, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legibility_asymmetry_as_survival, extractiveness, 0.18).
narrative_ontology:constraint_metric(legibility_asymmetry_as_survival, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(legibility_asymmetry_as_survival, theater_ratio, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legibility_asymmetry_as_survival, rope).
narrative_ontology:human_readable(legibility_asymmetry_as_survival, "Legibility Asymmetry as Survival Strategy").
narrative_ontology:topic_domain(legibility_asymmetry_as_survival, "social_ontology/power_dynamics/collective_memory").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legibility_asymmetry_as_survival, monitored_communities).
narrative_ontology:constraint_beneficiary(legibility_asymmetry_as_survival, substrate_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MONITORED COMMUNITY (ROPE) — Constrained by surveillance but benefits from the coordination function: legibility asymmetry enables survival through dual-register operation. Compliance metrics satisfy surveillance requirements while substrate complexity preserves practices. Low extraction because the constraint solves a genuine coordination problem under hostile observation.
constraint_indexing:constraint_classification(legibility_asymmetry_as_survival, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: SUBSTRATE PRACTITIONERS (ROPE) — Mobile agents who maintain substrate signal complexity across generations. Experience the constraint as coordination: the legibility asymmetry is a shared protocol for encoding practices in registers surveillance cannot decode. Benefits from preservation of cultural knowledge without triggering suppression.
constraint_indexing:constraint_classification(legibility_asymmetry_as_survival, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: SURVEILLANCE APPARATUS (MOUNTAIN) — Institutional actor with arbitrage exit perceives the constraint as immutable: compliance metrics are elevated and stable, indicating successful monitoring. Cannot decode substrate complexity, so perceives no resistance. The legibility asymmetry is invisible from this position — what cannot be measured appears not to exist.
constraint_indexing:constraint_classification(legibility_asymmetry_as_survival, mountain,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DIASPORA NETWORK (ROPE) — Organized agents across multiple surveillance regimes who coordinate substrate preservation. Mobile across jurisdictions, they experience the constraint as a coordination mechanism: standardized legibility protocols enable practice transmission while maintaining opacity to local surveillance. Low effective extraction because the network has agency and the constraint serves their coordination needs.
constraint_indexing:constraint_classification(legibility_asymmetry_as_survival, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: ARCHIVAL ACTIVISTS (SCAFFOLD) — Organized agents working to make substrate legible to future generations while maintaining current opacity. See the constraint as temporary: as surveillance regimes change or collapse, the substrate can be decoded openly. The legibility asymmetry has a sunset — it is needed only while hostile observation persists.
constraint_indexing:constraint_classification(legibility_asymmetry_as_survival, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, legibility asymmetry is a coordination solution to the structural problem of practice preservation under surveillance. The constraint enables communities to satisfy monitoring requirements (elevated compliance metrics) while maintaining cultural continuity through substrate complexity. Pure coordination with minimal extraction — the asymmetry serves the monitored, not the monitor.
constraint_indexing:constraint_classification(legibility_asymmetry_as_survival, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legibility_asymmetry_as_survival_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legibility_asymmetry_as_survival, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legibility_asymmetry_as_survival, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(legibility_asymmetry_as_survival_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint imposes costs (maintaining dual-register operation requires resources and cognitive load) but these are coordination costs, not extractive overhead. The legibility asymmetry serves the monitored communities, not the surveillance apparatus. The slight extraction reflects that some community members bear disproportionate burden of substrate maintenance, but this is distributed coordination cost rather than asymmetric extraction to an external beneficiary. Suppression (0.32): Moderate-low. The surveillance context creates suppression (communities cannot operate openly) but the legibility asymmetry reduces effective suppression by creating protected space for substrate practices. Suppression is real but mitigated by the coordination mechanism. Theater ratio (0.28): Low-moderate. Some compliance performance is genuinely theatrical (metrics without substance) but much serves the dual function of deflecting surveillance attention while substrate work proceeds. The theater is strategic rather than purely performative — it enables the coordination function.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between the surveillance apparatus (mountain — perceives elevated compliance as immutable success) and the monitored communities (rope — experience legibility asymmetry as coordination enabling survival). The apparatus cannot see the substrate complexity, so it perceives no resistance and no coordination. From its perspective, the communities have been successfully integrated (compliance metrics are elevated and stable). From the communities' perspective, the legibility asymmetry is an active coordination mechanism that preserves practices while satisfying surveillance requirements. The scaffold perspective (archival activists) introduces a temporal dimension: the legibility asymmetry is needed only while hostile observation persists. As surveillance regimes change or collapse, the substrate can be decoded openly. The constraint has a sunset, but the sunset is external (regime change) rather than internal (degradation of the coordination mechanism).
 *
 * DIRECTIONALITY LOGIC:
 *   All primary agents (monitored communities, substrate practitioners, diaspora network, archival activists) are beneficiaries with constrained or mobile exit options. The constraint serves their coordination needs — it enables practice preservation under surveillance. Directionality values are low (beneficiary status) across all non-institutional perspectives. The surveillance apparatus is the only institutional actor, and it perceives the constraint as a mountain (immutable compliance success) because it cannot decode substrate complexity. The apparatus is not a beneficiary of the legibility asymmetry — it is the hostile observer the asymmetry is designed to evade. The constraint's low extraction reflects that it redistributes power toward the monitored (enabling survival) rather than concentrating it toward the monitor.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that low extraction does not imply absence of power dynamics. The legibility asymmetry is a coordination mechanism (rope from all non-institutional perspectives) but it operates within a context of surveillance and suppression. The coordination serves the monitored communities by enabling survival — it redistributes agency toward the powerless rather than extracting from them. The surveillance apparatus perceives a mountain (immutable compliance) because it cannot decode the substrate, but this is a false summit: the compliance is strategic performance, not genuine integration. The analytical observer sees the full structure: legibility asymmetry as a coordination solution to practice preservation under hostile observation, with minimal extraction because the constraint serves the monitored rather than the monitor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_decoding_timeline,
    'How long can substrate complexity remain uninterpreted before surveillance apparatus develops decoding capacity?',
    'Historical analysis of surveillance technology evolution vs substrate encoding sophistication; machine learning pattern recognition advances vs cultural encryption techniques',
    'If decoding timeline < generation time: constraint becomes snare (communities trapped in legibility game they cannot win). If timeline > generation time: rope classification confirmed (coordination function persists across transmission cycles).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_decoding_timeline, empirical, 'Timeline for surveillance apparatus to decode substrate complexity').

omega_variable(
    compliance_theater_threshold,
    'At what point does maintaining elevated compliance metrics become extractive rather than protective?',
    'Resource allocation analysis: cost of compliance performance vs cost of substrate maintenance; community testimony on burden distribution',
    'If compliance cost > substrate preservation value: constraint shifts toward tangled_rope (coordination with embedded extraction). If compliance cost < substrate value: rope classification holds (efficient coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_theater_threshold, empirical, 'Threshold where compliance performance becomes net burden').

omega_variable(
    intergenerational_transmission_fidelity,
    'Does substrate complexity preserve practice fidelity across generations or does encoding introduce drift?',
    'Comparison of practice continuity in communities using legibility asymmetry vs those with open transmission; measurement of semantic drift in encoded vs plaintext cultural knowledge',
    'If high fidelity: rope classification confirmed (coordination succeeds at preservation). If significant drift: constraint may be piton (performative preservation that loses content).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_transmission_fidelity, empirical, 'Whether substrate encoding preserves or degrades practice fidelity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legibility_asymmetry_as_survival, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legibility_asym_tr_t0, legibility_asymmetry_as_survival, theater_ratio, 0, 0.15).
narrative_ontology:measurement(legibility_asym_tr_t3, legibility_asymmetry_as_survival, theater_ratio, 3, 0.22).
narrative_ontology:measurement(legibility_asym_tr_t6, legibility_asymmetry_as_survival, theater_ratio, 6, 0.28).

% Extraction over time
narrative_ontology:measurement(legibility_asym_be_t0, legibility_asymmetry_as_survival, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(legibility_asym_be_t3, legibility_asymmetry_as_survival, base_extractiveness, 3, 0.15).
narrative_ontology:measurement(legibility_asym_be_t6, legibility_asymmetry_as_survival, base_extractiveness, 6, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legibility_asymmetry_as_survival, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of substrate_as_unrecognized_archive. The upstream constraint (mountain) establishes that practices can be encoded in substrates surveillance does not recognize as information-bearing. This constraint (rope) is the active coordination mechanism that exploits that structural property: communities coordinate on dual-register operation to preserve practices while satisfying surveillance requirements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
