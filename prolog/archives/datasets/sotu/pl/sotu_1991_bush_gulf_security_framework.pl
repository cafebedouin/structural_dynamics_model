% ============================================================================
% CONSTRAINT STORY: sotu_1991_bush_gulf_security_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1991_bush_gulf_security_framework, []).

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
 *   constraint_id: sotu_1991_bush_gulf_security_framework
 *   human_readable: U.S. Naval Presence and Joint Exercise Framework in the Persian Gulf (1991-Present)
 *   domain: military/geopolitical/regional_security
 *
 * SUMMARY:
 *   The U.S. regional security architecture in the Persian Gulf, formally
 *   codified in the 1991 Bush administration's commitment to forward-deployed
 *   naval presence and joint military exercises with Gulf Cooperation Council
 *   states, represents a durable institutional arrangement that binds
 *   American strategic interests (oil access, geopolitical leverage,
 *   counterterrorism) to regional stability without the domestic political
 *   cost of permanent ground force basing. The constraint exhibits the full
 *   structural profile of a Tangled Rope: it solves a genuine coordination
 *   problem (regional states cannot unilaterally deter Iran and rival powers;
 *   the U.S. cannot pursue Persian Gulf interests without allied cooperation)
 *   while simultaneously enabling extraction (U.S. gains disproportionate
 *   regional influence; Gulf states become dependent on U.S. guarantees and
 *   vendor-locked into U.S. defense technology; the U.S. Navy's budgetary
 *   claims on readiness allocation increase at the expense of other strategic
 *   theaters like the Indo-Pacific). The framework's sustainability depends
 *   on maintaining the suppression mechanism — the credible threat from Iran,
 *   regional rivals, and the asymmetry of military capability — that locks
 *   regional states into dependency despite their nominal institutional
 *   power.
 *
 * KEY AGENTS:
 *   - United States Strategic Interests (institutional/arbitrage): Primary beneficiary — captures oil access guarantees, geopolitical leverage over OPEC, deterrence signal, alliance management bandwidth
 *   - Regional Autonomous Security Capacity (powerless/trapped): Primary victim — locked in permanent dependency, foregoes independent defense development, cedes strategic autonomy
 *   - Gulf State Sovereigns (organized/constrained): Secondary actor — officially allies but structurally dependent; benefit from security guarantee but constrained from independent capacity development
 *   - U.S. Military Readiness Allocation (moderate/constrained): Secondary victim — Gulf presence drains resources from Pacific theater and modern strategic competition; locked in by political commitments
 *   - Defense Contractors (institutional/arbitrage): Beneficiary — vendor lock-in ensures recurring procurement relationships and technology licensing revenue
 *   - Alternative Multilateral Arrangements (powerful/mobile): Emerging actor — Saudi Arabia, UAE, Qatar building OPEC+, diversified partnerships (France, Russia, China), Israel normalization to reduce exclusive U.S. dependency
 *   - Cold War Naval Doctrine Institutions (institutional/arbitrage): Beneficiary via inertia — CENTCOM structure, 5th Fleet, carrier battle group schedules sustained through institutional momentum despite original Cold War justification atrophying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1991_bush_gulf_security_framework, 0.52).
domain_priors:suppression_score(sotu_1991_bush_gulf_security_framework, 0.45).
domain_priors:theater_ratio(sotu_1991_bush_gulf_security_framework, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1991_bush_gulf_security_framework, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1991_bush_gulf_security_framework, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(sotu_1991_bush_gulf_security_framework, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1991_bush_gulf_security_framework, tangled_rope).
narrative_ontology:human_readable(sotu_1991_bush_gulf_security_framework, "U.S. Naval Presence and Joint Exercise Framework in the Persian Gulf (1991-Present)").
narrative_ontology:topic_domain(sotu_1991_bush_gulf_security_framework, "military/geopolitical/regional_security").

domain_priors:requires_active_enforcement(sotu_1991_bush_gulf_security_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1991_bush_gulf_security_framework, united_states_strategic_interests).
narrative_ontology:constraint_beneficiary(sotu_1991_bush_gulf_security_framework, gulf_state_sovereigns).
narrative_ontology:constraint_beneficiary(sotu_1991_bush_gulf_security_framework, defense_contractors).
narrative_ontology:constraint_victim(sotu_1991_bush_gulf_security_framework, u_s_military_readiness_allocation).
narrative_ontology:constraint_victim(sotu_1991_bush_gulf_security_framework, regional_autonomous_security_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIONAL AUTONOMOUS SECURITY CAPACITY (SNARE) — Gulf states cannot exit the framework without losing security guarantee against Iran, Iraq, and internal threats. Structural dependency is locked in by the asymmetry of military capability and the credible alternative threat from regional rivals. High extraction: regional states are trapped in permanent dependency, bear opportunity costs (cannot develop independent defense capacity), and surrender strategic autonomy in exchange for security umbrellas maintained at U.S. discretion.
constraint_indexing:constraint_classification(sotu_1991_bush_gulf_security_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: U.S. STRATEGIC INTERESTS (ROPE) — Experiences the constraint as coordination: maintaining naval presence and joint exercises solves the collective action problem of regional stability. Benefits from forward deployment (rapid response capability, deterrence signal, alliance management) outweigh costs when measured from the strategic perspective. Low extraction from this viewpoint — the framework distributes benefits toward U.S. interests and away from the U.S. military readiness budget (costs are externalized).
constraint_indexing:constraint_classification(sotu_1991_bush_gulf_security_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: GULF STATE MILITARIES AND SECURITY ESTABLISHMENTS (TANGLED ROPE) — Organized institutional actors experience the constraint as both coordination and extraction. Genuine coordination function: joint exercises build interoperability, provide training access to advanced NATO/U.S. doctrine, and create early-warning integration. Simultaneous asymmetric extraction: training dependency prevents independent doctrine development, exercises are scheduled at U.S. convenience, and Gulf militaries are locked into vendor relationships with U.S. defense contractors. Active enforcement required: maintaining the joint exercise schedule and technological coupling demands continuous institutional commitment.
constraint_indexing:constraint_classification(sotu_1991_bush_gulf_security_framework, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ALTERNATIVE MULTILATERAL SECURITY ARRANGEMENTS (SCAFFOLD) — Powerful actors (Saudi Arabia, UAE, Qatar) are building alternatives to exclusive U.S. dependency: OPEC+ coordination, diversified defense partnerships (France, UK, Russia, China), and recent normalization with Israel creating new security geometry. These agents have exit mobility and see the U.S. framework as increasingly optional. The constraint has a sunset logic: as regional actors accumulate alternative security sources, the exclusive U.S. guarantee's value declines. Theater ratio declining as actual alternatives materialize. This is a Scaffold from the perspective of actors with enough power to exit.
constraint_indexing:constraint_classification(sotu_1991_bush_gulf_security_framework, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR NAVAL PRESENCE DOCTRINE (PITON) — The 5th Fleet and joint exercise protocols originated in Cold War deterrence against Soviet expansion. Post-Cold War, the functional justification shifted to counterterrorism and power projection, but the institutional framework (carrier battle groups, CENTCOM command structure, 35-year exercise schedule) persists through inertia. Theater ratio: much activity is performative — maintaining force posture for signaling purposes rather than operational necessity. The original coordination function (Soviet deterrence) has atrophied, but the constraint remains because institutional actors benefit from budget allocation and career structures built around the framework.
constraint_indexing:constraint_classification(sotu_1991_bush_gulf_security_framework, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: U.S. MILITARY READINESS ALLOCATION (TANGLED ROPE) — The U.S. Navy experiences the constraint as both coordination and extraction. Genuine coordination: joint exercises build interoperability with allied militaries, forward-deployed presence enables rapid response to humanitarian crises and regional conflicts. Simultaneous extraction from readiness perspective: maintaining 5th Fleet presence drains readiness allocation from Pacific theater (Indo-Pacific Strategy), ties down carrier battle groups in low-intensity patrol duties, and locks resources into legacy Cold War force structure rather than emerging strategic competition with China. Constrained exit: the Navy cannot withdraw from the Gulf without losing regional influence and contradicting U.S. strategic commitments, but increasing budget to accommodate both Gulf and Pacific presence is politically constrained.
constraint_indexing:constraint_classification(sotu_1991_bush_gulf_security_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the Persian Gulf's geography (8.5% of global oil production, chokepoint for 20% of global trade, surrounded by rival states) creates structural constraints on security autonomy. No regional actor can unilaterally control the Gulf; no external power can ignore it. The forward-deployed naval framework appears as a natural law consequence of geography and resource dependency. However, this naturalizes what is actually a contingent post-WWII institutional choice. Alternative framings (withdrawal, multilateral mechanisms, energy diversification) are structurally possible but politically constrained. The engine's false-summit detector will flag this as naturalization of a constructed arrangement.
constraint_indexing:constraint_classification(sotu_1991_bush_gulf_security_framework, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1991_bush_gulf_security_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1991_bush_gulf_security_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1991_bush_gulf_security_framework, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1991_bush_gulf_security_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1991_bush_gulf_security_framework, TR),
    TR >= 0.70.

:- end_tests(sotu_1991_bush_gulf_security_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts through locked-in dependency (regional states cannot unilaterally develop deterrence), opportunity costs (foregone independent defense capacity development), and vendor lock-in (technology dependencies). However, extractiveness is not at snare levels (0.66+) because genuine coordination benefits exist: regional states do gain credible deterrence, and the U.S. does face collective action problems that joint frameworks solve. The 0.38→0.52 trajectory reflects a trend: initial post-Cold War cooperation (1991-2000) had stronger coordination justification (Iraq containment, Kuwait protection). As threats diversified (terrorism, drone proliferation) and the Iran threat waxed and waned, the framework's functional justification weakened — more activity became performative theater. Suppression (0.45): Moderate. Regional states face real barriers to exit (Iran threat, military capability asymmetry, geographic vulnerability) but also have accumulated alternative options (OPEC coordination, diversified partnerships, normalization with Israel, domestic naval development). Suppression is credible but not total. Theater ratio (0.58): Moderate-high and rising. Joint exercises increasingly serve signaling and relationship-maintenance functions rather than filling genuine interoperability gaps. The 0.42→0.58 trajectory reflects the piton pattern: original function (Cold War Soviet deterrence) has attenuated; new rationale (counterterrorism, power projection) is less compelling; institutional persistence through inertia accounts for continued activity.
 *
 * PERSPECTIVAL GAP:
 *   The gap manifests across multiple dimensions: (a) Time horizon: from immediate tactical perspective (joint exercise coordination), the framework is rope-like; from civilizational perspective (geopolitical structure), it appears as mountain or piton depending on whether one naturalizes or decomposes. (b) Power asymmetry: powerless regional capacity sees snare; organized regional militaries see tangled rope; powerful regional states see scaffold. (c) Extraction direction: U.S. strategic interests see rope (benefits); U.S. military readiness sees tangled rope (costs); regional states see snare from security dependency perspective but rope from defense contractor access perspective. (d) Functional decay: original Cold War function (Soviet deterrence) has vanished; intermediate function (Iraq containment) is intermittent; current function (general presence/influence) is increasingly performative. This functional decay is captured in rising theater ratio.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each agent is derived from their structural position relative to the extraction flow: (1) Regional Autonomous Capacity is trapped (no exit options) and bears costs → high d → maximum experienced extraction. (2) U.S. Strategic Interests are beneficiaries with arbitrage options (can exit by withdrawing, can arbitrage between regional security and Pacific strategy) → low d → benefits flow toward this agent. (3) Gulf State Militaries are organized but constrained (can develop alternatives but at high cost) → moderate d → mixed experience (coordination + extraction). (4) U.S. Military Readiness is constrained and victim → moderate-high d → significant cost extraction. (5) Defense Contractors are beneficiaries with arbitrage options → low d → benefits concentrated. The beneficiary/victim declarations feed directly into these d values. The piton and mountain perspectives are derived from institutional inertia and analytical vantage points, not from d computation — they emerge from power atom + time horizon combinations.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by acknowledging that it is genuinely hybrid — not pure extraction disguised as coordination, and not pure coordination with incidental extraction. The coordination is real: joint exercises do build interoperability, naval presence does deter regional rivals, allied security arrangements do solve collective action problems. The extraction is real: regional states are locked in dependent positions, U.S. military resources are locked in Gulf commitment at expense of strategic alternatives, defense contractors extract vendor-lock rent. The mandatrophy is resolved not by choosing one type but by accepting that from different structural positions, the same constraint appears as different types, and all classifications are accurate descriptions of what that position's agent experiences. The false-summit mountain perspective is particularly clarifying: it tempts policymakers to treat the framework as immutable (geographic necessity) when in fact it is a contingent institutional choice that could be restructured (withdrawal, diversified partnerships, energy transition) at political cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oil_dependency_counterfactual,
    'If global energy transitions reduce oil''s strategic importance, does the security framework collapse or do alternative justifications sustain it?',
    'Longitudinal tracking of stated rationales for Gulf presence; correlation between oil market volatility and force structure decisions; analysis of post-2050 strategy documents under net-zero energy scenarios',
    'If framework collapses: extractiveness declines sharply as primary coordination justification (oil access) disappears. If sustained: indicates the framework''s real function is geopolitical leverage and deterrence, not energy security — reframes the constraint as Snare for regional autonomy rather than Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oil_dependency_counterfactual, empirical, 'Whether energy transition triggers framework collapse or alternative justification').

omega_variable(
    regional_military_capacity_substitution,
    'Can Gulf state militaries actually develop independent deterrence capacity, or do structural factors (technology gap, population ratios, geography) make U.S. dependency irreversible?',
    'Analysis of defense procurement diversity, indigenous weapons development programs, military training self-sufficiency; comparison with non-aligned states that achieved military independence (Brazil, India, South Korea)',
    'If substitution possible: regional states have genuine exit mobility (Scaffold from their perspective). If impossible: states are structurally trapped despite organized power (Snare with organized agents — triggers Dynamic Coalition extension). If partially possible: tangled-rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_military_capacity_substitution, empirical, 'Whether regional military capacity substitution is structurally feasible').

omega_variable(
    joint_exercise_function_vs_theater,
    'What fraction of joint exercise activity is genuine interoperability training versus performative signaling and relationship management?',
    'Content analysis of exercise objectives and post-exercise reports; comparison of exercise parameters (scale, duration, complexity) with documented training requirements; surveys of participating military officers on exercise utility',
    'If theater > 0.70: piton classification gains support — framework sustained by institutional inertia despite declining functional need. If theater < 0.40: tangled-rope classification confirmed — exercises serve genuine coordination. Theater ratio is a direct measurement of this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(joint_exercise_function_vs_theater, empirical, 'Ratio of exercise training function to performative signaling').

omega_variable(
    extraction_direction_ambiguity,
    'Does the U.S. extract more from regional states through dependency lock-in, or do regional states extract security guarantees from U.S. strategic interests in oil/leverage?',
    'Cost-benefit analysis: measurable economic transfer flows (military aid, technology licensing), opportunity costs (foregone independent capacity development), geopolitical gains (regional influence, deterrence effects). Who benefits more in net terms?',
    'If U.S. net beneficiary: framework is pure Snare for regional states (Perspective 1 analysis). If regional states net beneficiary: framework is more cooperative than suggested (Perspective 2 upgrade to mutual coordination). If roughly balanced: Tangled Rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_direction_ambiguity, empirical, 'Net direction of extraction flow between U.S. and regional states').

omega_variable(
    iran_threat_credibility_trajectory,
    'As Iran''s threat level varies (post-JCPOA escalation, potential future de-escalation), does the security framework''s suppression mechanism strengthen or weaken? Are regional states responding to credible threat or manufactured dependency?',
    'Historical correlation analysis: force posture changes with threat level assessments; exit behavior testing — when Iran de-escalates, do regional states express reduced dependency or reaffirm commitment to U.S. framework?',
    'If suppression tracks Iran threat: framework''s lock-in is conditionally dependent on threat perception (could weaken if de-escalation succeeds). If suppression persists despite threat reduction: indicates genuine structural dependency (true Snare). This directly measures whether suppression is structural or manufactured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iran_threat_credibility_trajectory, empirical, 'Whether suppression tracks Iran threat or persists independently').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1991_bush_gulf_security_framework, 0, 33).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gulf_sec_tr_t0, sotu_1991_bush_gulf_security_framework, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gulf_sec_tr_t15, sotu_1991_bush_gulf_security_framework, theater_ratio, 15, 0.54).
narrative_ontology:measurement(gulf_sec_tr_t33, sotu_1991_bush_gulf_security_framework, theater_ratio, 33, 0.58).

% Extraction over time
narrative_ontology:measurement(gulf_sec_be_t0, sotu_1991_bush_gulf_security_framework, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gulf_sec_be_t15, sotu_1991_bush_gulf_security_framework, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(gulf_sec_be_t33, sotu_1991_bush_gulf_security_framework, base_extractiveness, 33, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1991_bush_gulf_security_framework, resource_allocation).
narrative_ontology:affects_constraint(sotu_1991_bush_gulf_security_framework, opec_petrodollar_regime).
narrative_ontology:affects_constraint(sotu_1991_bush_gulf_security_framework, iran_deterrence_asymmetry).
narrative_ontology:affects_constraint(sotu_1991_bush_gulf_security_framework, defense_technology_licensing_dependency).

% DUAL FORMULATION NOTE:
% The 1991 Bush framework decomposes into multiple structurally distinct constraints: (1) naval presence commitment (coordination dominance, theater rising), (2) joint exercise protocol (institutional inertia, piton mechanics), (3) defense technology transfer (vendor lock-in, pure extraction from regional autonomy perspective). This story aggregates the family of related constraints into a single framework analysis. Downstream constraints inherit the suppression and extraction dynamics established here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1991_bush_gulf_security_framework, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
