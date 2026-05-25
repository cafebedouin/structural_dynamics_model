% ============================================================================
% CONSTRAINT STORY: northeast_asian_wmd_proliferation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_northeast_asian_wmd_proliferation, []).

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
 *   constraint_id: northeast_asian_wmd_proliferation
 *   human_readable: Northeast Asian WMD Proliferation Constraint
 *   domain: geopolitical/security/strategic_weapons
 *
 * SUMMARY:
 *   Northeast Asian WMD proliferation creates a structural tension between
 *   the legal architecture of the Nuclear Nonproliferation Treaty and the
 *   strategic incentives driving weapons development in a region of great
 *   power competition and historical conflict. The constraint operates
 *   through multiple overlapping mechanisms: formal legal prohibition on
 *   signatory states, sanctions and economic pressure on proliferators,
 *   technological monopoly maintained by advanced states, and US security
 *   guarantee substitution for independent deterrence. North Korea's weapons
 *   program, China's strategic modernization, Japan and South Korea's latent
 *   capability and recurring debates about independent deterrence, and
 *   Taiwan's extreme vulnerability create a tightly coupled system where any
 *   major actor's policy shift triggers cascading regional responses. The
 *   constraint is characterized by high suppression (sanctions, treaty
 *   obligations, alliance dependencies create barriers to exit for
 *   non-nuclear states), moderate-to-high extractiveness (the hegemon
 *   captures strategic advantage; proliferators absorb sanctions cost;
 *   non-nuclear allies are locked into dependency), and increasingly
 *   performative diplomacy (Six-Party Talks, summits, and UN resolutions
 *   signal commitment without enforcing outcomes).
 *
 * KEY AGENTS:
 *   - North Korea: Regional proliferator (organized/constrained) — develops WMD capability as regime survival guarantee; experiences constraint through sanctions and isolation; retains some autonomy through Chinese support
 *   - South Korea: Trapped non-nuclear state (powerless/trapped) — bound by NPT and US alliance; existential exposure to North Korean capability; cannot legally pursue independent deterrence despite security incentives
 *   - Japan: Trapped non-nuclear state (powerless/trapped) — NPT signatory with latent capability; dependent on US security guarantee; faces domestic/international pressure against independent deterrence
 *   - Taiwan: Trapped non-nuclear state without treaty status (powerless/trapped) — no formal NPT status but de facto bound by international pressure; existentially threatened; no viable deterrence option outside US protection
 *   - United States: Alliance hegemon (institutional/arbitrage) — benefits from extended deterrence framework; maintains technological monopoly; provides security substitution for non-nuclear allies; manages regional competition with China
 *   - China: Rising regional power (powerful/mobile) — selective participation in nonproliferation regime; maintains strategic ambiguity on enforcement; benefits from constraints on regional competitors; manages North Korean alliance
 *   - International nonproliferation regime (IAEA, NPT): Institutional actor (institutional/constrained) — tasked with verification and enforcement; lacks access, resources, and political backing; maintains symbolic verification despite structural inability to prevent proliferation
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks framing proliferation as inevitable security dilemma inherent to anarchy rather than contingent outcome of hegemon-managed order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(northeast_asian_wmd_proliferation, 0.68).
domain_priors:suppression_score(northeast_asian_wmd_proliferation, 0.75).
domain_priors:theater_ratio(northeast_asian_wmd_proliferation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(northeast_asian_wmd_proliferation, extractiveness, 0.68).
narrative_ontology:constraint_metric(northeast_asian_wmd_proliferation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(northeast_asian_wmd_proliferation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(northeast_asian_wmd_proliferation, snare).
narrative_ontology:human_readable(northeast_asian_wmd_proliferation, "Northeast Asian WMD Proliferation Constraint").
narrative_ontology:topic_domain(northeast_asian_wmd_proliferation, "geopolitical/security/strategic_weapons").

domain_priors:requires_active_enforcement(northeast_asian_wmd_proliferation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(northeast_asian_wmd_proliferation, regional_hegemon_us_alliance).
narrative_ontology:constraint_victim(northeast_asian_wmd_proliferation, nuclear_free_states).
narrative_ontology:constraint_victim(northeast_asian_wmd_proliferation, civilian_populations).
narrative_ontology:constraint_victim(northeast_asian_wmd_proliferation, nonproliferation_regime).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED NON-NUCLEAR STATE (SNARE) — South Korea, Japan, and Taiwan face existential pressure within a region where WMD proliferation is advancing. Their formal commitment to NPT creates a structural lock: they cannot legally pursue nuclear weapons even as North Korea, China, and Russia acquire advanced arsenals. Exit options are effectively zero — treaty withdrawal would trigger sanctions and isolation. Suppression is maximal: the constraint operates through legal prohibition, alliance dependency, and existential threat simultation.
constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL PROLIFERATOR (TANGLED ROPE) — North Korea experiences a genuine coordination problem (regional security through demonstrated deterrence) alongside extraction (international sanctions, resource diversion, regime isolation). The constraint has a coordination function (demonstrating capability to guarantee regime survival through deterrence) and an asymmetric extraction component (sanctions flow from the constraint itself). Constrained exit: North Korea could theoretically abandon the program, but at catastrophic regime survival cost.
constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ALLIANCE HEGEMON—US POSITION (ROPE) — The US benefits from the proliferation constraint as coordination: extended deterrence through alliance commitment, regional basing, and technological monopoly on advanced WMD systems. The constraint coordinates US security interests (Japan/South Korea as forward bases, military-industrial complex procurement, strategic dominance) with its stated nonproliferation goals. Arbitrage exit: the US could shift to tolerating proliferation without existential cost, but chooses to maintain the regime because it subsidizes US regional hegemony.
constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NONPROLIFERATION REGIME (SNARE) — The NPT and IAEA face extraction: they are tasked with preventing proliferation but lack enforcement tools, inspection access, or verification authority over significant players. North Korea withdrew from the NPT; Iran remains in technical compliance while advancing capability; Israel operates outside the treaty. The regime is maintained through performative inspection and diplomatic rhetoric while structural proliferation proceeds. Constrained exit: regime members cannot unilaterally abandon the treaty without signaling collapse of the entire architecture.
constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL VERIFICATION THEATER (PITON) — Six-Party Talks, UN resolutions, IAEA inspections, and diplomatic summits are largely performative: they signal commitment to nonproliferation without enforcing outcomes. North Korea has repeatedly violated agreements; verification mechanisms have minimal access; sanctions are episodically enforced and partially evaded. The ritual persists through institutional inertia (the apparatus exists, participants have career incentives) despite low functional effectiveness. Theater ratio is elevated because the visible diplomatic activity masks the structural inability to prevent proliferation.
constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: RISING REGIONAL POWER—CHINA (TANGLED ROPE) — China experiences genuine coordination (strategic stability in Asia, maintenance of sphere of influence influence, managed competition with the US) alongside extraction dynamics (constraints on Japanese/South Korean rearmament benefit China's relative position; constraints on US escalation reduce perceived regional threat). Mobile exit: China maintains sufficient autonomy to adjust its relationship to the constraint, but chooses engagement because the constraint's current form subsidizes its regional position.
constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER—REALIST FRAMING (MOUNTAIN) — From a civilizational/universal analytical frame, the constraint appears as an immutable feature of international anarchy: in the absence of a world government, every state rationally pursues weapons that guarantee survival. Proliferation is inherent to the anarchic system. The NPT is a facade masking this structural reality. However, the base metrics contradict the mountain classification—suppression is high but not total (enforcement is selective and incomplete); extractiveness is high but not immutable (states have negotiated restraint). The analytical observer's mountain is a false summit: it naturalizes a contingent hegemon-managed order as structural inevitability.
constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(northeast_asian_wmd_proliferation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(northeast_asian_wmd_proliferation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(northeast_asian_wmd_proliferation, TR),
    TR >= 0.70.

:- end_tests(northeast_asian_wmd_proliferation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint systematically extracts from non-nuclear states (forced dependent status, delayed deterrence capability, alliance subordination) while benefiting the hegemon (technological monopoly, regional basing, strategic dominance) and complicating proliferators (sanctions, isolation, resource diversion, though also providing deterrence legitimacy). The upward trend from 0.45 to 0.68 reflects increasing cost of compliance as North Korean capability advances, China modernizes, and regional allies face growing pressure to consider independent deterrence. Suppression (0.75): High. Multiple overlapping suppression mechanisms: formal treaty prohibition for signatories, sanctions on proliferators, export controls on dual-use technology, alliance dependency that makes exit extremely costly, and existential threat simulation (each actor's weapons decision triggers perceived threats to others). Barriers to exit are near-total for non-nuclear states and substantial for proliferators. Theater ratio (0.58): Moderate-to-high. Diplomatic theater (summits, Six-Party Talks, UN resolutions) is substantial but not dominant. Actual verification is selective and incomplete; sanctions are episodically enforced and partially evaded; compliance agreements are repeatedly violated. Theater is rising (0.42 to 0.58) as diplomatic activity increases while proliferation accelerates.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival disagreement. The trapped non-nuclear state sees a snare: legal prohibition without credible security substitution, faced with advancing regional threats. The proliferator sees tangled rope: genuine deterrence coordination need paired with extraction via sanctions. The hegemon sees rope: coordination of extended deterrence that subsidizes their regional position. The nonprofit regime sees snare: tasked with enforcement authority it lacks. The diplomatic theater sees piton: rituals persist despite low functional effect. The rising power sees tangled rope: strategic advantage through selective enforcement. The analytical observer risks the false summit: seeing proliferation as inevitable rather than contingent. This perspectival spread—the entire classification range from a single constraint—indicates that divergent material interests and structural positions generate fundamentally incompatible readings of the same security arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across actor positions. Non-nuclear states trapped by alliance and treaty show d ≈ 0.95 (full targets). North Korea as constrained proliferator shows d ≈ 0.65 (victim of sanctions but beneficiary of deterrence legitimacy). The US as hegemon shows d ≈ 0.10 (full beneficiary through maintained strategic advantage). The nonproliferation regime as enforcement body shows d ≈ 0.80 (victim of the constraint's impossible position—tasked with preventing proliferation without tools). China's strategic ambiguity produces d ≈ 0.45 (intermediate position—neither committed nor opposed). These differentiated d values map to f(d) producing χ values that range from negative (for hegemon) to highly positive (for trapped allies), revealing that effective extraction concentrates on agents least able to exit.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC CASE: This constraint reveals the mandatrophy resolution through structural decomposition. The false summit (analytical mountain) naturalizes what is actually a hegemon-managed order maintained through asymmetric extraction. The snare (trapped alliance members) is the structural reality for non-nuclear states: they are bound by law and alliance to forgo weapons development while facing advancing regional threats. The tangled rope (proliferators) reflects their genuine security coordination problem (deterrence through demonstrated capability) paired with extraction costs (sanctions, isolation). The rope (hegemon) is real: the constraint does coordinate extended deterrence, and this coordination genuinely benefits the US. The piton reflects degraded verification: the diplomatic apparatus persists but functions at decreasing effectiveness as technical capability advances. The classification range itself is the answer: the constraint operates as a presheaf over multiple actor positions, each experiencing a structurally valid but incompatible classification. Resolving the mandatrophy requires accepting that 'what is this constraint?' has no single answer—instead, the question must be 'for whom?' and 'at what time horizon?' As North Korean capability advances and US commitment credibility declines, trapped allies experience increasing extraction; as proliferation accelerates, the diplomatic theater becomes more performative; as Chinese regional power grows, the hegemon's ability to maintain the constraint degrades. The constraint is stable as long as the US maintains military-technological dominance and alliance commitment credibility. It becomes a snare if that dominance erodes. It becomes rope if regional security is genuinely achieved through mutual restraint. None of these are permanent states—they are contingent outcomes of ongoing geopolitical struggle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_vs_compliance_gap,
    'Does verification failure reflect the inherent impossibility of monitoring WMD programs (structural mountain) or the insufficient political will to enforce compliance (contingent snare)?',
    'Comparative analysis of IAEA inspection regimes in willing vs unwilling states; technical assessment of detection thresholds; analysis of resources allocated to verification vs political barriers to enforcement',
    'If structural: proliferation is inevitable and constraint is false summit. If contingent: constraint could be tightened through enforcement investment and political commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_vs_compliance_gap, empirical, 'Whether verification gap is structural or enforcement-driven').

omega_variable(
    security_dilemma_inescapability,
    'Can regional security be achieved through nonproliferation commitment without US security guarantees, or is the constraint entirely dependent on continued US hegemonic provision of deterrence?',
    'Counterfactual analysis of regional stability absent US presence; survey of regional threat perceptions; modeling of security outcomes under symmetric vs asymmetric proliferation scenarios',
    'If achievable: constraint is genuinely coordination-based (Rope/Tangled Rope). If dependent on hegemon: constraint is extraction via security provision (Snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_dilemma_inescapability, conceptual, 'Whether regional security is achievable without US security provision').

omega_variable(
    china_constraint_participation_stability,
    'Does China''s participation in the nonproliferation regime reflect alignment with regime goals or strategic ambiguity that permits selective enforcement to China''s advantage?',
    'Analysis of Chinese enforcement behavior (sanctions votes, export control compliance); comparison of Chinese positions on North Korean vs Iranian proliferation; assessment of Chinese technology transfers to regional actors',
    'If alignment: China is genuine regime participant. If ambiguity: China uses the constraint selectively to manage regional competition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(china_constraint_participation_stability, empirical, 'Whether China is aligned with or strategically ambiguous toward the regime').

omega_variable(
    alliance_dependent_trap_mechanism,
    'Does South Korea/Japan NPT commitment persist due to security commitment credibility or due to sunk alliance costs that create path dependency?',
    'Historical analysis of moments when NPT withdrawal was considered; survey of decision-making processes; assessment of exit costs if alliance relationship were severed',
    'If credibility-based: constraint functions as coordination (Rope). If path-dependent: constraint functions as a trap (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_dependent_trap_mechanism, empirical, 'Whether NPT commitment reflects credible security commitment or alliance path dependency').

omega_variable(
    theater_ratio_trajectory,
    'Is the diplomatic theater (Six-Party Talks, summits, resolutions) becoming more performative over time, or is it genuinely building verification capacity and enforcement infrastructure?',
    'Trend analysis of inspection access, enforcement actions, and verification outcomes; comparison of stated commitments vs observed compliance; resource allocation to verification bodies',
    'If more performative: constraint degrading toward pure piton. If building capacity: constraint could transition toward Tangled Rope or Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_trajectory, empirical, 'Whether diplomatic theater is increasing or verification capacity is growing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(northeast_asian_wmd_proliferation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nwmd_tr_t0, northeast_asian_wmd_proliferation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nwmd_tr_t10, northeast_asian_wmd_proliferation, theater_ratio, 10, 0.5).
narrative_ontology:measurement(nwmd_tr_t20, northeast_asian_wmd_proliferation, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(nwmd_be_t0, northeast_asian_wmd_proliferation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(nwmd_be_t10, northeast_asian_wmd_proliferation, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(nwmd_be_t20, northeast_asian_wmd_proliferation, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(northeast_asian_wmd_proliferation, enforcement_mechanism).
narrative_ontology:affects_constraint(northeast_asian_wmd_proliferation, east_china_sea_territorial_disputes).
narrative_ontology:affects_constraint(northeast_asian_wmd_proliferation, us_alliance_credibility_east_asia).
narrative_ontology:affects_constraint(northeast_asian_wmd_proliferation, north_korean_regime_survival).

% DUAL FORMULATION NOTE:
% The Northeast Asian WMD proliferation constraint is structurally decomposable into three related but distinct constraints: (1) North Korean deterrence capability development (ε ≈ 0.72, snare from non-nuclear ally perspective); (2) Regional deterrence substitution dynamics (ε ≈ 0.55, tangled rope from proliferator perspective); (3) International verification theater (ε ≈ 0.38, piton from regime perspective). These are linked through network causality: North Korean capability advances fuel Japanese/South Korean deterrence pressure, which activates verification theater responses. The single story presented here integrates all three through multi-perspective indexing rather than decomposing into separate stories, because the ε-invariance principle does not apply cleanly—the 'observable' (weapons capability vs deterrence dynamics vs diplomatic effectiveness) fundamentally changes the constraint's nature, but these are genuinely coupled in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(northeast_asian_wmd_proliferation, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
