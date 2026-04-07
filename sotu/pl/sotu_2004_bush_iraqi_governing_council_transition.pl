% ============================================================================
% CONSTRAINT STORY: sotu_2004_bush_iraqi_governing_council_transition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2004_bush_iraqi_governing_council_transition, []).

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
 *   constraint_id: sotu_2004_bush_iraqi_governing_council_transition
 *   human_readable: Iraqi Governing Council Constitutional Transition and Transfer of Sovereignty (2003-2004)
 *   domain: foreign_policy/state_transition
 *
 * SUMMARY:
 *   The Iraqi Governing Council constitutional transition (November 2003 to
 *   June 2004, extending to January 2005) represents a structural attempt to
 *   convert military occupation into constitutional sovereignty, transferring
 *   de jure political authority to an Iraqi interim government while
 *   maintaining de facto coalition military control. The constraint exhibits
 *   the classical structure of tangled rope: a genuine coordination function
 *   (transitioning from occupation to recognized statehood) is embedded
 *   within asymmetric extraction mechanisms (military occupation's burden
 *   falling on Iraqi security forces and civilians, continued coalition
 *   military presence despite nominal sovereignty transfer, constitutional
 *   process constrained by security collapse and foreign veto power). The
 *   theater_ratio rises over the interval (0.35 to 0.72) as the gap widens
 *   between constitutional promise and security reality — the formal process
 *   proceeds (drafting, committee work, meetings with coalition advisors)
 *   while insurgency accelerates, sectarian violence emerges, and the June
 *   2004 deadline becomes increasingly unachievable. The beneficiaries (Iraqi
 *   political elites gaining recognition and legitimacy, U.S. coalition
 *   reducing political exposure for occupation costs) experience minimal
 *   extraction, while the victims (Iraqi civilian population bearing security
 *   vacuum costs, Iraqi security apparatus inheriting institutional
 *   responsibility without capacity, Iraqi state institutional continuity
 *   disrupted by ongoing foreign military authority) experience severe
 *   extraction. The constraint's suppression (0.68) reflects multiple
 *   overlapping barriers: territorial jurisdiction (Iraqis cannot exit Iraq);
 *   security dependence (Iraqi security forces depend on coalition support
 *   for capability and intelligence); and institutional duress
 *   (constitutional drafting occurs under conditions of military occupation,
 *   insurgency, and sectarian mobilization rather than free political
 *   deliberation).
 *
 * KEY AGENTS:
 *   - Iraqi Civilian Population: Primary victim (powerless/trapped) — bears full cost of security vacuum, insurgent attacks, sectarian violence, infrastructure collapse; no exit option; no say in transition design
 *   - Iraqi Security Apparatus (Military, Police, Intelligence): Secondary victim (moderate/constrained) — inherits security responsibility for destabilized territory; benefits from institutional reconstruction but constrained by capability gaps, foreign equipment dependence, and sectarian fragmentation
 *   - Iraqi Political Elites (Governing Council, Constitutional Drafters): Primary beneficiary (organized/arbitrage) — gain political legitimacy, international recognition, access to transitional authority; exit options strong (external backers, security guarantees, funding)
 *   - U.S. Coalition Military and Leadership: Primary beneficiary (institutional/arbitrage) — reduce political cost of occupation, gain legitimacy through Iraqi consent, convert unilateral military authority into constitutional governance framework; near-zero experienced extraction; strong arbitrage options
 *   - International Community and Regional States: Mixed beneficiary-victim (powerful/mobile) — benefit from stabilized state actor but suffer from regional influence competition and refugee flows; mobile exit options moderate extraction
 *   - Constitutional and Democratic Institutions (Framework): Performative theater (institutional/arbitrage) — constitutional process and bill of rights largely rhetorical under military occupation and security collapse; persists through mutual justification (U.S. claims enabling democracy, Iraq claims achieving sovereignty)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2004_bush_iraqi_governing_council_transition, 0.58).
domain_priors:suppression_score(sotu_2004_bush_iraqi_governing_council_transition, 0.68).
domain_priors:theater_ratio(sotu_2004_bush_iraqi_governing_council_transition, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2004_bush_iraqi_governing_council_transition, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_2004_bush_iraqi_governing_council_transition, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sotu_2004_bush_iraqi_governing_council_transition, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2004_bush_iraqi_governing_council_transition, tangled_rope).
narrative_ontology:human_readable(sotu_2004_bush_iraqi_governing_council_transition, "Iraqi Governing Council Constitutional Transition and Transfer of Sovereignty (2003-2004)").
narrative_ontology:topic_domain(sotu_2004_bush_iraqi_governing_council_transition, "foreign_policy/state_transition").

domain_priors:requires_active_enforcement(sotu_2004_bush_iraqi_governing_council_transition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2004_bush_iraqi_governing_council_transition, iraqi_political_elites).
narrative_ontology:constraint_beneficiary(sotu_2004_bush_iraqi_governing_council_transition, us_coalition_military).
narrative_ontology:constraint_beneficiary(sotu_2004_bush_iraqi_governing_council_transition, us_geopolitical_position).
narrative_ontology:constraint_victim(sotu_2004_bush_iraqi_governing_council_transition, iraqi_security_apparatus).
narrative_ontology:constraint_victim(sotu_2004_bush_iraqi_governing_council_transition, iraqi_civilian_population).
narrative_ontology:constraint_victim(sotu_2004_bush_iraqi_governing_council_transition, iraqi_state_institutional_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRAQI CIVILIAN POPULATION (SNARE) — Trapped by territorial jurisdiction and kinship obligation. Bears full cost of security vacuum during transition: insurgent attacks, sectarian violence, criminal opportunism, and infrastructure collapse. No exit option exists (immobile, dependent on state services). Maximum experienced extraction — the constraint imposes mortality, displacement, and material deprivation with no reciprocal benefit or say in its design. The transition's legitimacy narrative (drafting constitution, democratic process) provides no protection from violence.
constraint_indexing:constraint_classification(sotu_2004_bush_iraqi_governing_council_transition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IRAQI SECURITY APPARATUS (TANGLED ROPE) — Tasked with inheriting control of a territory destabilized by occupation, insurgency, and sectarian mobilization. Benefits from institutional reconstitution (army, police, intelligence services rebuilt with training and equipment) but bears asymmetric extraction: assume security responsibility with fragmented legitimacy, foreign-trained officer corps, and inherited equipment dependencies. Constrained by resource scarcity and external security guarantees. Genuine coordination function (security coordination) exists alongside asymmetric extraction (institutional weakness, external reliance). Neither pure extraction nor pure coordination.
constraint_indexing:constraint_classification(sotu_2004_bush_iraqi_governing_council_transition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IRAQI POLITICAL ELITES (ROPE) — Benefit from transition: constitutional drafting confers legitimacy within Iraq, U.S. backing provides initial security/funding, and political positions translate to post-sovereignty authority. Exit options are strong (organized actors with external recognition, security guarantees, and funding). The constraint functions as coordination for this agent: drafting a constitution solves their collective action problem (moving from occupation to recognized statehood). Extraction they experience is minimal — the U.S. backs the outcome that benefits them. Arbitrage options (multiple international backers, recognition, funding flows) reduce experienced extraction.
constraint_indexing:constraint_classification(sotu_2004_bush_iraqi_governing_council_transition, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: U.S. COALITION MILITARY AND POLITICAL LEADERSHIP (ROPE) — Primary beneficiary in the immediate term. The transition converts military occupation (politically costly, resource-intensive, vulnerable to insurgency) into constitutional governance ostensibly requested by Iraqi authority. Extraction experienced by the coalition is minimal (benefits from reduced political exposure, lower cost per occupation year, legitimacy through Iraqi consent). Arbitrage options are strong (can escalate, reduce, or transfer military burden; can unilaterally modify rules of engagement; maintains security guarantees). The constraint functions as coordination: managing the occupation through interim Iraqi institutions reduces the coalition's political burden and provides justification for continued military presence. Experienced extraction is near-zero; experienced benefit is substantial.
constraint_indexing:constraint_classification(sotu_2004_bush_iraqi_governing_council_transition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL COMMUNITY AND REGIONAL STATES (TANGLED ROPE) — Coordination benefit: a legitimate Iraqi government reduces the cost of occupation (distributed internationally through UN legitimacy frameworks) and creates a stable state actor. Asymmetric extraction: some regional actors (Iran, Syria) gain influence over internal Iraqi politics during transition; others (Saudi Arabia, Jordan) face instability and refugee flows; international community experiences reputational extraction (staking legitimacy on Iraq's democratic transition). Mobile exit options (diplomatic recognition, aid conditionality, military support) moderate the experienced extraction. Genuine coordination function (stabilizing a critical region) exists alongside asymmetric regional influence flows.
constraint_indexing:constraint_classification(sotu_2004_bush_iraqi_governing_council_transition, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL CONSTITUTIONAL DEVELOPMENT FRAMEWORK (PITON) — The rhetoric and process (constitution-drafting, bill of rights, democratic transition) are largely performative when mapped against actual institutional capacity and security conditions. Constitutional drafting during active insurgency cannot follow normal deliberative processes; Bill of Rights provisions cannot be enforced without functioning judiciary; elections cannot be freely conducted under military occupation. The theater ratio (0.65) reflects that the constitutional process is substantially theatrical — it performs legitimacy and democratic process while structural realities (foreign military control, sectarian fragmentation, security collapse) determine actual governance. The framework persists because it provides mutual justification: the U.S. claims it is enabling democracy; Iraqi elites claim they are achieving sovereignty; the international community claims it is supporting state-building. Without this theater, the occupation loses legitimacy. Theater_ratio increased over the interval as the gap between constitutional promise and security reality widened.
constraint_indexing:constraint_classification(sotu_2004_bush_iraqi_governing_council_transition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the constraint exhibits both genuine coordination and asymmetric extraction. Coordination: the transition from military occupation to constitutional governance is structurally necessary for achieving stable statehood. Extraction: the timing (June 2004 deadline) is arbitrary and politically driven; the security vacuum created by transition is non-trivial; the constitutional framework is drafted under duress with limited legitimacy; and continued U.S. military presence remains despite nominal sovereignty transfer. The analytical observer sees χ as driven by scope (global scale) and beneficiary-victim differentiation (organized beneficiaries vs powerless victims). This perspective produces Tangled Rope rather than Snare because genuine state-building coordination exists alongside coercive institutional imposition, and because the constraint has sunset potential (eventual end of occupation) even if the June 2004 date is not achievable.
constraint_indexing:constraint_classification(sotu_2004_bush_iraqi_governing_council_transition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2004_bush_iraqi_governing_council_transition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2004_bush_iraqi_governing_council_transition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2004_bush_iraqi_governing_council_transition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2004_bush_iraqi_governing_council_transition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2004_bush_iraqi_governing_council_transition, TR),
    TR >= 0.70.

:- end_tests(sotu_2004_bush_iraqi_governing_council_transition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significantly from civilians and security apparatus, but genuine state-building coordination exists (not pure extraction like a Snare would be). The beneficiary-victim split is asymmetric (organized elites and coalition benefit substantially; powerless civilians and constrained security apparatus bear costs), and the extraction mechanism is durable (continues through June 2004 and beyond, not self-limiting). Suppression (0.68): High. Multiple overlapping barriers prevent exit and alternatives: territorial jurisdiction (civilians cannot leave Iraq), institutional dependence (security apparatus depends on coalition for capability), and political duress (constitutional process occurs under foreign military occupation). Suppression is not maximal (some Iraqis do exit through emigration, some security force members defect, some political participants resist) but represents high structural barriers. Theater_ratio (0.65): Moderate-high. The constitutional process is substantially performative — formal structure (drafting committees, constitutional text, bill of rights) exists and proceeds according to timeline, but the actual governance reality is military occupation, insurgency, and sectarian conflict. Constitutional provisions cannot be enforced (judiciary non-functional), elections cannot be freely conducted (security conditions prevent free movement and expression), and deliberation occurs under military-imposed constraints (occupation curfews, limited population input, foreign veto power on key issues). Theater increases over interval as deadline pressures accelerate formal process while security deteriorates. The rise in theater_ratio from 0.35 to 0.72 reflects Goodhart drift: the constitutional process becomes increasingly performative as the actual security conditions diverge from what functioning constitutional governance would require.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The powerless civilian trapped in the security vacuum sees pure extraction (Snare) — the transition imposes costs with no benefit and no exit. The constrained security apparatus sees mixed coordination-extraction (Tangled Rope) — genuine security responsibility alongside institutional weakness and extraction. The organized political elites see pure coordination (Rope) — the transition solves their collective action problem and benefits them substantially. The coalition sees pure coordination (Rope) — reduced occupation costs and political exposure. The international community sees mixed (Tangled Rope) — regional stability benefit alongside influence competition extraction. The constitutional/democratic institutional framework sees itself as degraded theater (Piton) — the process persists despite non-functionality, maintained by mutual justification narratives. The analytical observer at civilizational scale sees Tangled Rope — genuine state-building coordination embedded within asymmetric extraction, with a theoretically-achievable but practically-unachievable June 2004 sunset (making it not a true Scaffold despite sunset clause rhetoric). The perspectival divergence is driven by the agent's structural position: beneficiaries with exit options experience minimal extraction; victims without exit options experience maximal extraction; intermediate agents experience mixed outcomes based on their particular institutional position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status and exit options. Iraqi civilians: victims + trapped → d ≈ 0.95 → f(d) ≈ 1.42 → high experienced extraction. Iraqi security apparatus: victims + constrained → d ≈ 0.75 → f(d) ≈ 1.05 → moderate-high extraction. Iraqi political elites: beneficiaries + arbitrage → d ≈ 0.15 → f(d) ≈ -0.01 → near-zero or negative extraction (benefits). U.S. coalition: beneficiaries + arbitrage → d ≈ 0.05 → f(d) ≈ -0.12 → negative extraction (substantial benefits). International community: mixed (powerful/mobile) → d ≈ 0.50 → f(d) ≈ 0.65 → moderate extraction. The perspectival gap emerges because high-extraction civilians and security apparatus see Snare or severe Tangled Rope, while beneficiaries (elites, coalition) see Rope or light Tangled Rope, and the theatrical institutional framework sees Piton. No single perspective captures the structure; the presheaf over observation sites is the model.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint presents the core mandatrophy as a question of timing and institutional capacity: Is the June 2004 transition timing a genuine sunset clause enabling eventual occupation exit (Scaffold logic) or a performative deadline serving political communication rather than institutional reality (Piton logic)? The measured trajectory supports the latter: theater_ratio rises sharply as the deadline approaches, indicating that the constitutional process accelerates toward performative completion rather than substantive deliberation. The extractiveness also rises (0.42 to 0.62) as the security burden increases during transition, contradicting the hypothesis that transitioning to Iraqi authority would reduce extraction for victims. The constraint does not resolve as Scaffold (temporary support with declining suppression) because suppression remains high throughout and would increase post-June 2004 when U.S. coalition presence continues despite nominal sovereignty transfer. Instead, the constraint resolves as Tangled Rope with performative theater (Piton-adjacent characteristics): genuine state-building coordination exists, but extraction persists because the actual military-institutional control structure continues beyond the nominal sovereignty transfer. The mandatrophy resolves when analyzing what 'sovereignty' means in this context: if it means de jure legal authority, the June 2004 transfer is real (Rope becomes achievable); if it means de facto control and decision-making power, the transfer is theater and extraction continues (Tangled Rope persists). The 2011 U.S. withdrawal and subsequent 2014 ISIS insurgence provide empirical resolution: the Iraqi state's actual capacity (measured by its inability to prevent ISIS's 2014 offensive in the absence of U.S. troops) was insufficient for independent security, confirming that June 2004 'sovereignty' was nominally real but functionally theater. The constraint's classification as Tangled Rope (rather than Snare) is justified by the genuine coordination benefit: the transition does create an Iraqi state, does end the formal occupation, and does establish institutions that persist and evolve post-2004. But the classification incorporates the reality that these benefits accrue asymmetrically (elites and coalition benefit most; civilians and security apparatus benefit least) and that continued extraction persists through military presence and institutional dependence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    iraqi_state_capacity_baseline,
    'What is the inherent state capacity of post-Baath Iraqi institutions for security and governance without external support?',
    'Post-2011 U.S. withdrawal empirical data; performance metrics of Iraqi security forces, judiciary, and revenue collection absent coalition support; comparison to pre-1990s Iraqi state capacity',
    'If baseline is high: transition is primarily coordination problem (Rope) with manageable security deficit. If baseline is low: transition imposes institutional responsibility without capacity, increasing extraction experienced by security apparatus and civilians (Snare). June 2004 date becomes impossible timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iraqi_state_capacity_baseline, empirical, 'Whether Iraqi state institutions have inherent capacity for post-occupation governance').

omega_variable(
    governing_council_legitimacy_gap,
    'What percentage of Iraqi population perceives the Governing Council as legitimate representatives vs. foreign appointees?',
    'Public polling data (Iraqi IRI surveys, Gallup Iraq tracking); election participation rates and patterns in 2005 elections; insurgent targeting patterns (GC member attacks vs. broader civilian targeting)',
    'If legitimacy > 60%: constitution-drafting constraint functions as genuine political process (closer to Rope). If legitimacy < 30%: constitutional framing is theater masking foreign imposition (moves toward Snare). Impacts how much coordination function the constraint genuinely contains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(governing_council_legitimacy_gap, empirical, 'Actual legitimacy of Iraqi Governing Council among Iraqi population').

omega_variable(
    sectarian_mobilization_timing,
    'Was sectarian violence primarily driven by the transition''s security vacuum, or was the transition timing selected to coincide with ongoing sectarian mobilization?',
    'Timeline correlation between constitutional process stages and violence escalation; comparison to pre-transition violence trends; analysis of external actors (Iran, Syria, Saudi influence) entering Iraq before vs. after transition start',
    'If violence is transition-induced: suppression (0.68) is primarily caused by the constraint itself (institutional artifact). If sectarian mobilization was pre-existing: suppression reflects inherited structural violence, not constraint-specific extraction. Changes attribution of civilian casualties to the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sectarian_mobilization_timing, empirical, 'Whether sectarian violence was caused by transition or pre-existing').

omega_variable(
    june_2004_deadline_achievability,
    'Was June 2004 a realistic deadline for constitutional drafting and sovereignty transfer given Iraqi institutional capacity and security conditions?',
    'Ex-post analysis: actual timeline to December 2005 election and permanent constitution; comparison to historical state transition timelines; security metrics (insurgent capacity, casualties, territory control) at each phase',
    'If deadline was realistic: constraint is well-designed coordination. If deadline was politically motivated despite infeasibility: constraint is theater designed to signal progress rather than enable it (increases theater_ratio toward Piton). Affects whether June 2004 is genuine sunset or arbitrary performance target.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(june_2004_deadline_achievability, preference, 'Whether June 2004 sovereignty transfer deadline was achievable or performative').

omega_variable(
    continued_us_military_presence_rationale,
    'Does continued U.S. military presence after June 2004 ''sovereignty transfer'' constitute genuine security guarantee or de facto occupation under Iraqi legal cover?',
    'Analysis of rules of engagement, command authority, detention authority, and base control under pre-June 2004 vs. post-June 2004 arrangements; comparison to security agreements of other post-occupation states; interview data from U.S. and Iraqi military leadership on operational continuity',
    'If presence is genuine security guarantee: constraint enables state-building (Rope/Tangled Rope). If presence is de facto occupation: constraint is legitimation theater converting military control into legal cover (moves toward Snare/Piton). Core question for mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continued_us_military_presence_rationale, empirical, 'Whether post-2004 U.S. military presence is genuine security guarantee or de facto occupation').

omega_variable(
    beneficiary_definition_ambiguity,
    'Does ''Iraqi political elites'' benefit from the transition, or does the transition extract from them by imposing governance responsibility without institutional capacity?',
    'Analysis of political elite fatality rates, threat levels, institutional authority/capacity ratio, revenue control, and career trajectories post-transition; comparison of elite positions under Governing Council vs. post-2005 elected government; exit option exercise (who left Iraq, when, why)',
    'If elites are genuine beneficiaries: constraint is Tangled Rope with clear beneficiary-victim division. If elites are also victims (imposed responsibility without capacity): constraint is more symmetric extraction (moves toward Snare for multiple agent classes). Changes directionality calculus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_definition_ambiguity, empirical, 'Whether Iraqi political elites are beneficiaries or victims of institutional responsibility transfer').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2004_bush_iraqi_governing_council_transition, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(igc_trans_tr_t0, sotu_2004_bush_iraqi_governing_council_transition, theater_ratio, 0, 0.35).
narrative_ontology:measurement(igc_trans_tr_t4, sotu_2004_bush_iraqi_governing_council_transition, theater_ratio, 4, 0.52).
narrative_ontology:measurement(igc_trans_tr_t8, sotu_2004_bush_iraqi_governing_council_transition, theater_ratio, 8, 0.65).
narrative_ontology:measurement(igc_trans_tr_t12, sotu_2004_bush_iraqi_governing_council_transition, theater_ratio, 12, 0.72).

% Extraction over time
narrative_ontology:measurement(igc_trans_be_t0, sotu_2004_bush_iraqi_governing_council_transition, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(igc_trans_be_t4, sotu_2004_bush_iraqi_governing_council_transition, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(igc_trans_be_t8, sotu_2004_bush_iraqi_governing_council_transition, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(igc_trans_be_t12, sotu_2004_bush_iraqi_governing_council_transition, base_extractiveness, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2004_bush_iraqi_governing_council_transition, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_2004_bush_iraqi_governing_council_transition, iraqi_sectarian_violence_escalation).
narrative_ontology:affects_constraint(sotu_2004_bush_iraqi_governing_council_transition, us_occupation_cost_externalization).
narrative_ontology:affects_constraint(sotu_2004_bush_iraqi_governing_council_transition, iraqi_institutional_capacity_deficit).

% DUAL FORMULATION NOTE:
% The constitutional transition constraint decomposes into three structurally distinct constraints: (1) the transition process itself (nominal authority transfer, drafting mechanism) with ε ≈ 0.35 early, rising to ε ≈ 0.65 by June 2004; (2) the underlying sectarian mobilization (community-level violence driven by state collapse and external interference) with ε ≈ 0.75 throughout; (3) the institutional capacity deficit (gap between nominal authority and actual security capability) with ε ≈ 0.80. The present story models the integrated constraint that bundles all three, tracking how the transition process interacts with sectarian violence and capacity deficit over the 12-month interval. The affects_constraints edges link to the component constraints, which have different ε values and different temporal trajectories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_2004_bush_iraqi_governing_council_transition, moderate, 0.75).
constraint_indexing:directionality_override(sotu_2004_bush_iraqi_governing_council_transition, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
