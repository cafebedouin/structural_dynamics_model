% ============================================================================
% CONSTRAINT STORY: escalation_control_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_escalation_control_asymmetry, []).

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
 *   constraint_id: escalation_control_asymmetry
 *   human_readable: Escalation Control Asymmetry in US-Iran Strategic Competition
 *   domain: international_relations/military_strategy/energy_security
 *
 * SUMMARY:
 *   The escalation control asymmetry between the United States and Iran
 *   represents a structural mismatch where both sides' domestic political
 *   constraints reduce flexibility for de-escalation while creating mutual
 *   miscalculation risk. Iranian regime faces internal legitimacy crisis
 *   (32,000 protest deaths claimed, 60% inflation, energy crisis) requiring
 *   high domestic security force deployment ratios (internal vs external),
 *   which reduces operational flexibility for external deterrence. US force
 *   posture in region lacks ground/SOF components necessary for sustained
 *   operations, creating vulnerability to unattributed attacks on bases while
 *   maintaining overwhelming air/naval superiority that cannot address
 *   ground-level proxy threats. This asymmetry exhibits genuine coordination
 *   function (mutual deterrence preventing full-scale war) alongside
 *   significant extraction (domestic political imperatives on both sides
 *   drive risk-taking that threatens regional stability and civilian
 *   populations). Theater ratio (0.42) reflects that regional multilateral
 *   security forums and public diplomatic statements are substantially
 *   performative, while actual escalation control occurs through
 *   back-channels and military signaling. Constraint has degraded moderately
 *   over 2015-2024 interval as Iranian internal crisis deepened and US force
 *   composition gaps persisted despite repeated base attacks, but the
 *   degradation has been gradual and stabilizing rather than runaway, with
 *   coordination function remaining intact and even strengthening through
 *   back-channel development.
 *
 * KEY AGENTS:
 *   - Iranian Civilian Population: Primary victim (powerless/trapped) — bears significant cost of regime repression, economic collapse, and external military threat, but also benefits from deterrence coordination that prevents full-scale war
 *   - Regional Stability: Primary victim (powerless/trapped) — abstract collective good; shipping lanes, energy infrastructure, Gulf state populations bear cascading costs of miscalculation but benefit from war prevention
 *   - US Forward-Deployed Personnel: Secondary victim (moderate/constrained) — exposed by force composition gaps while serving genuine deterrence function; constrained by deployment orders
 *   - Iranian Revolutionary Guard Corps: Mixed actor (moderate/constrained) — benefits from external threat narrative for domestic control but constrained by regime survival imperatives and resource limitations
 *   - Hardliners Both Sides: Primary beneficiaries (institutional/arbitrage) — US hardliners extract defense budgets and political positioning; Iranian hardliners extract domestic repression justification; both can exit via back-channels when convenient
 *   - Diplomatic Back-Channel Networks: Organized actors (organized/mobile) — Oman mediation, Swiss protecting power, Track II networks building alternative escalation control pathways with sunset logic
 *   - Regional Multilateral Security Forums: Institutional actors (institutional/constrained) — GCC summits, Arab League maintain performative rituals with minimal functional impact (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees irreducible hybrid of coordination (deterrence) and extraction (domestic political constraints driving risk-taking)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(escalation_control_asymmetry, 0.48).
domain_priors:suppression_score(escalation_control_asymmetry, 0.58).
domain_priors:theater_ratio(escalation_control_asymmetry, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(escalation_control_asymmetry, extractiveness, 0.48).
narrative_ontology:constraint_metric(escalation_control_asymmetry, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(escalation_control_asymmetry, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(escalation_control_asymmetry, tangled_rope).
narrative_ontology:human_readable(escalation_control_asymmetry, "Escalation Control Asymmetry in US-Iran Strategic Competition").
narrative_ontology:topic_domain(escalation_control_asymmetry, "international_relations/military_strategy/energy_security").

domain_priors:requires_active_enforcement(escalation_control_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(escalation_control_asymmetry, hardliners_both_sides).
narrative_ontology:constraint_beneficiary(escalation_control_asymmetry, regional_arms_suppliers).
narrative_ontology:constraint_beneficiary(escalation_control_asymmetry, domestic_security_apparatus_iran).
narrative_ontology:constraint_victim(escalation_control_asymmetry, regional_stability).
narrative_ontology:constraint_victim(escalation_control_asymmetry, iranian_civilian_population).
narrative_ontology:constraint_victim(escalation_control_asymmetry, us_forward_deployed_personnel).
narrative_ontology:constraint_victim(escalation_control_asymmetry, gulf_shipping_industry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRANIAN CIVILIAN POPULATION (TANGLED ROPE) — Trapped between regime repression (32,000 protest deaths, 60% inflation, energy crisis) and external military threat. Cannot exit geographically or politically. Bears significant cost of escalation spiral through economic collapse and internal security crackdowns, but also benefits from deterrence coordination that prevents full-scale war and occupation. Mixed experience: high extraction but not pure snare because mutual deterrence provides genuine protection from worse outcomes.
constraint_indexing:constraint_classification(escalation_control_asymmetry, tangled_rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL STABILITY (TANGLED ROPE) — Abstract collective good with no advocate. Trapped by geographic proximity to escalation zone. Shipping lanes, energy infrastructure, civilian populations across Gulf states bear cascading costs of miscalculation. However, mutual deterrence coordination prevents full-scale war that would be catastrophically worse. Mixed experience: extraction through risk-taking but coordination through war prevention.
constraint_indexing:constraint_classification(escalation_control_asymmetry, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: US FORWARD-DEPLOYED PERSONNEL (TANGLED ROPE) — Constrained by deployment orders and force composition gaps (insufficient Marines/SOF for sustained operations). Face direct threat from unattributed attacks on bases while also serving genuine deterrence coordination function. Mixed experience: real security mission but exposed by strategic mismatch between force posture and operational requirements.
constraint_indexing:constraint_classification(escalation_control_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: IRGC (TANGLED ROPE) — Constrained by regime survival imperatives and internal repression requirements (domestic security force deployment ratios favor internal over external). Benefits from external threat narrative for domestic control but also bears operational risk of miscalculation. Mixed coordination (regime defense) and extraction (forced escalation despite resource constraints).
constraint_indexing:constraint_classification(escalation_control_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: HARDLINERS BOTH SIDES (ROPE) — Primary beneficiaries with arbitrage exit options. US hardliners benefit from threat inflation for defense budgets and political positioning; Iranian hardliners benefit from external threat for domestic repression justification. Both can exit the escalation spiral through back-channel diplomacy when convenient. Experience constraint as coordination mechanism for domestic political extraction.
constraint_indexing:constraint_classification(escalation_control_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: DIPLOMATIC BACK-CHANNELS (SCAFFOLD) — Organized actors (Oman mediation, Swiss protecting power, Track II networks) see escalation control asymmetry as temporary coordination failure with sunset logic. Building alternative communication pathways to bypass domestic political theater. Frequency of back-channel contacts inversely correlates with public escalation rhetoric. Sunset mechanism: as both sides exhaust domestic political utility of confrontation, back-channels enable face-saving de-escalation.
constraint_indexing:constraint_classification(escalation_control_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: REGIONAL SECURITY FORUMS (PITON) — GCC summits, Arab League statements, regional security dialogues persist as performative rituals with minimal functional escalation control. Theater ratio high: public communiques and diplomatic choreography continue despite zero impact on actual force deployments or attack frequencies. Maintained through institutional inertia, not effectiveness.
constraint_indexing:constraint_classification(escalation_control_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global perspective, escalation control asymmetry exhibits genuine coordination function (mutual deterrence preventing full-scale war) alongside significant extraction (domestic political constraints on both sides drive risk-taking that threatens regional stability). Iranian regime's internal repression reduces flexibility for de-escalation; US force posture gaps create operational vulnerabilities. Both structural features are contingent institutional arrangements, not natural laws. Tangled rope classification reflects irreducible hybrid: cannot separate deterrence coordination from extraction mechanisms.
constraint_indexing:constraint_classification(escalation_control_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(escalation_control_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(escalation_control_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(escalation_control_asymmetry, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(escalation_control_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(escalation_control_asymmetry, TR),
    TR >= 0.70.

:- end_tests(escalation_control_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. Iranian regime's internal repression requirements (domestic security force deployment ratios favor internal over external) reduce flexibility for de-escalation, forcing risk-taking despite resource constraints. US force composition gaps (insufficient Marines/SOF) create operational vulnerabilities that Iranian proxy networks exploit through unattributed attacks. Both structural features drive extraction from regional stability and civilian populations. However, extractiveness is moderate rather than high because genuine mutual deterrence coordination function exists — full-scale war has been avoided despite repeated crises, and this coordination provides real protection value to all regional actors including victims. The coordination function has strengthened over time through back-channel development, preventing runaway extraction accumulation. Suppression (0.58): Moderate. Iranian civilian population faces regime repression with limited exit options. US forward-deployed personnel constrained by deployment orders and force composition gaps. Regional states trapped by geographic proximity. Back-channel diplomatic options exist and are actively used, though constrained by domestic political costs on both sides. Theater ratio (0.42): Moderate-low. Regional multilateral security forums (GCC summits, Arab League statements) are substantially performative with minimal impact on actual force deployments or attack frequencies. Public diplomatic rhetoric diverges from back-channel communications. However, theater is moderate-low rather than high because coordination mechanisms (back-channels, military signaling) retain significant functional content and are actively used for escalation management, and their effectiveness has increased over the interval.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how domestic political constraints on both sides create extraction that appears as coordination from beneficiary perspectives. Hardliners on both sides see rope — they are solving legitimate problems (US: deterrence and regional security; Iran: regime defense and sovereignty). Diplomatic back-channels see scaffold — they are building alternative pathways with sunset logic as both sides exhaust domestic political utility of confrontation. Regional security forums see piton — their own processes have degraded to theater. US forward-deployed personnel and IRGC see tangled rope — genuine security missions mixed with exposure to risks created by strategic mismatches. Iranian civilians and regional stability see tangled rope rather than pure snare — trapped by geography and politics, bearing significant cost of escalation spiral, but also benefiting from deterrence coordination that prevents catastrophically worse outcomes. Analytical observer sees tangled rope at civilizational scale — cannot separate mutual deterrence coordination from extraction mechanisms driven by domestic political constraints. The perspectival gap reveals that 'escalation control' naturalizes what is actually contingent institutional arrangement: Iranian regime's choice to prioritize internal repression over external flexibility, and US choice to maintain force posture with known composition gaps.
 *
 * DIRECTIONALITY LOGIC:
 *   Iranian civilian population is primary victim with trapped exit options, but experiences tangled rope rather than pure snare because mutual deterrence coordination provides genuine protection from worse outcomes (full-scale war, occupation). Regional stability as abstract collective good also experiences mixed extraction and coordination. US forward-deployed personnel and IRGC are moderate-power actors with constrained exit options and mixed beneficiary/victim status, yielding mid-range directionality (d ≈ 0.55-0.65) and moderate experienced extraction — both serve genuine coordination functions while bearing operational risks. Hardliners on both sides are institutional beneficiaries with arbitrage exit options (can use back-channels when convenient), yielding low directionality (d ≈ 0.10-0.15) and low/negative experienced extraction — they extract political benefits from escalation spiral. Diplomatic back-channel networks are organized actors with mobile exit options and beneficiary status (their coordination function gains importance during crises), yielding low directionality (d ≈ 0.25) and low experienced extraction. Regional security forums are institutional actors with constrained exit options but minimal functional impact, yielding piton classification through theater gate.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves mandatrophy by demonstrating irreducible hybrid structure. Genuine coordination function exists: mutual deterrence has prevented full-scale war despite repeated crises, back-channel networks enable face-saving de-escalation, and both sides benefit from avoiding catastrophic miscalculation. This coordination provides real protection value even to victims — Iranian civilians and regional stability are better off with managed escalation than with full-scale war. Simultaneously, significant extraction exists: Iranian regime's internal repression requirements and US force composition gaps create structural vulnerabilities that drive risk-taking, threatening regional stability and civilian populations beyond what pure coordination would require. Cannot decompose into separate rope and snare stories because the coordination and extraction mechanisms are coupled: the same domestic political constraints that enable hardliners to extract political benefits (external threat narrative, defense budgets) also create the deterrence coordination function that prevents war. The asymmetry is not 'coordination that happens to have some extraction' or 'extraction that happens to have some coordination' — it is structurally both, and the coupling is the constraint. Beneficiary/victim declarations confirm hybrid: hardliners benefit while civilians and regional stability bear costs, with forward-deployed personnel and IRGC experiencing mixed effects. Active enforcement required: both sides must continuously manage escalation through back-channels and military signaling to prevent spiral. This is canonical tangled rope: ε ≥ 0.30, suppression ≥ 0.40, 0.40 ≤ χ ≤ 0.90, beneficiaries + victims + enforcement. The moderate extractiveness (0.48 rather than higher) reflects that coordination function is substantial and provides real protection value, and has strengthened over time through back-channel development, preventing classification drift toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_survival_threshold,
    'At what level of internal instability does Iranian regime prioritize domestic repression over external deterrence posture, and does this threshold create predictable de-escalation windows or increase miscalculation risk?',
    'Historical analysis of Iranian domestic security force deployment ratios during previous crisis periods; correlation between protest intensity, inflation rates, and external military posture changes; identification of threshold points where internal/external force allocation shifted',
    'If threshold is predictable and creates de-escalation windows: escalation control asymmetry is manageable coordination problem (lower extractiveness). If threshold is unpredictable or increases risk-taking: asymmetry is extraction mechanism (higher extractiveness, potential reclassification toward snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_survival_threshold, empirical, 'Whether regime survival threshold creates predictable de-escalation windows or increases miscalculation risk').

omega_variable(
    force_composition_substitutability,
    'Can US air/naval assets substitute for ground/SOF presence in sustained escalation control, or does force composition gap create structural vulnerability that Iranian proxy networks can exploit?',
    'Operational analysis of unattributed attack patterns on US bases; correlation between force composition (Marines/SOF presence levels) and attack frequency/success rates; assessment of whether air/naval response options provide credible deterrence for ground-level threats',
    'If air/naval assets provide adequate deterrence: force composition gap is coordination problem with technical solution (lower extractiveness). If gap creates exploitable vulnerability: asymmetry is structural extraction mechanism favoring Iranian proxy strategy (higher extractiveness).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(force_composition_substitutability, empirical, 'Whether US force composition gap creates exploitable structural vulnerability').

omega_variable(
    back_channel_effectiveness,
    'Do diplomatic back-channels (Oman mediation, Swiss protecting power, Track II networks) provide effective escalation control during crisis, or do they serve primarily as face-saving mechanisms after decisions are made through other channels?',
    'Temporal analysis of back-channel communication frequency relative to escalation events; identification of cases where back-channel contacts preceded de-escalation vs cases where contacts followed decisions made through military signaling; assessment of whether back-channels enable coordination or merely ratify outcomes',
    'If back-channels provide effective real-time escalation control: scaffold perspective confirmed, sunset logic is structural. If back-channels are post-hoc face-saving: scaffold perspective is aspirational, and the constraint''s coordination function is weaker than claimed (higher extractiveness, potential reclassification toward snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(back_channel_effectiveness, empirical, 'Whether back-channels provide effective escalation control or post-hoc face-saving').

omega_variable(
    hardliner_exit_credibility,
    'Can hardliners on both sides credibly exit the escalation spiral through back-channel diplomacy without domestic political cost, or are they identity-locked into confrontational postures?',
    'Historical analysis of previous de-escalation attempts; assessment of domestic political consequences for leaders who pursued diplomatic solutions; identification of whether exit options are structural (arbitrage) or constrained by identity fusion with hardline positioning',
    'If hardliners have credible exit options: beneficiary classification is accurate, extractiveness flows as modeled. If hardliners are identity-locked: they are not pure beneficiaries but also victims of their own framing, reducing their effective power and potentially increasing overall extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hardliner_exit_credibility, conceptual, 'Whether hardliners have credible exit options or are identity-locked into confrontation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(escalation_control_asymmetry, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(esca_theater_2015, escalation_control_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(esca_theater_2018, escalation_control_asymmetry, theater_ratio, 3, 0.38).
narrative_ontology:measurement(esca_theater_2021, escalation_control_asymmetry, theater_ratio, 6, 0.4).
narrative_ontology:measurement(esca_theater_2024, escalation_control_asymmetry, theater_ratio, 9, 0.42).

% Extraction over time
narrative_ontology:measurement(esca_extract_2015, escalation_control_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(esca_extract_2018, escalation_control_asymmetry, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(esca_extract_2021, escalation_control_asymmetry, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(esca_extract_2024, escalation_control_asymmetry, base_extractiveness, 9, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(escalation_control_asymmetry, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% Escalation control asymmetry is downstream of degraded_deterrence_architecture (mountain — structural limits on deterrence credibility in asymmetric conflicts) and hormuz_leverage_paradox (tangled rope — Iranian ability to threaten Strait of Hormuz creates leverage but also invites preemption). The upstream constraints establish the strategic context; escalation control asymmetry models the specific domestic political constraints that reduce both sides' flexibility within that context. Decomposition follows ε-invariance principle: degraded deterrence architecture has low ε (structural limit), hormuz leverage paradox has moderate ε (genuine coordination mixed with extraction), escalation control asymmetry has higher ε (domestic political constraints drive risk-taking).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(escalation_control_asymmetry, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
