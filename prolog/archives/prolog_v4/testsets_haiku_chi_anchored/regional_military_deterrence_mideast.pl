% ============================================================================
% CONSTRAINT STORY: regional_military_deterrence_mideast
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regional_military_deterrence_mideast, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: regional_military_deterrence_mideast
 *   human_readable: US/Israeli Military Deterrence Posture against Iran
 *   domain: geopolitical/military/regional_security
 *
 * SUMMARY:
 *   The US/Israeli military deterrence posture against Iran represents a
 *   complex geopolitical constraint that simultaneously functions as
 *   coordination mechanism, extraction apparatus, and performative signaling.
 *   The stated purpose is to deter Iranian aggression and regional
 *   destabilization; the structural effect is to lock regional actors into
 *   asymmetric military dependency, justify continuous arms procurement, and
 *   constrain diplomatic alternatives while generating reciprocal Iranian
 *   military development. The constraint exhibits hybrid Tangled Rope
 *   characteristics: it provides genuine security coordination (prevents some
 *   Iranian escalations) while extracting through military-industrial profit,
 *   alliance dependency lock-in, and civilian vulnerability asymmetry. The
 *   theater ratio (0.65) reflects significant performative content — carrier
 *   strike group deployments and naval positioning that signal resolve but
 *   rarely engage in actual combat. The extractiveness has increased from
 *   0.35 to 0.58 over the measurement interval, driven by accumulating arms
 *   sales, expanded basing agreements, and deepening Gulf state military
 *   dependency.
 *
 * KEY AGENTS:
 *   - US Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — captures carrier deployments, air defense sales, base maintenance contracts, technology refresh cycles
 *   - Israeli Security Establishment: Primary beneficiary (institutional/arbitrage) — gains force projection capability, intelligence sharing, strategic depth, missile defense partnerships
 *   - Gulf State Monarchies: Constrained beneficiaries (moderate/constrained) — benefit from Iranian threat reduction but locked into US alliance dependency and diplomatic constraints
 *   - Iranian Civilian Population: Primary victim (powerless/trapped) — faces military strike vulnerability, economic sanctions, asymmetric risk; cannot exit constraint
 *   - Regional Destabilization: Structural victim (powerless/trapped) — arms races, proxy escalation, miscalculation risks are extracted as costs by no single actor
 *   - Regional Peace/Neutrality Actors: Secondary victims (organized/constrained) — Iraq, Oman, non-aligned states face constrained diplomatic autonomy and regional militarization dependency
 *   - Analytical Observer: (analytical/analytical) — sees constraint as either legitimate deterrence (mountain/rope) or engineered extraction (snare/piton)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_military_deterrence_mideast, 0.58).
domain_priors:suppression_score(regional_military_deterrence_mideast, 0.68).
domain_priors:theater_ratio(regional_military_deterrence_mideast, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_military_deterrence_mideast, extractiveness, 0.58).
narrative_ontology:constraint_metric(regional_military_deterrence_mideast, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(regional_military_deterrence_mideast, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regional_military_deterrence_mideast, tangled_rope).
narrative_ontology:human_readable(regional_military_deterrence_mideast, "US/Israeli Military Deterrence Posture against Iran").
narrative_ontology:topic_domain(regional_military_deterrence_mideast, "geopolitical/military/regional_security").

domain_priors:requires_active_enforcement(regional_military_deterrence_mideast).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regional_military_deterrence_mideast, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(regional_military_deterrence_mideast, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(regional_military_deterrence_mideast, gulf_state_monarchies).
narrative_ontology:constraint_beneficiary(regional_military_deterrence_mideast, regional_us_military_bases).
narrative_ontology:constraint_victim(regional_military_deterrence_mideast, iranian_civilian_population).
narrative_ontology:constraint_victim(regional_military_deterrence_mideast, regional_destabilization).
narrative_ontology:constraint_victim(regional_military_deterrence_mideast, civilian_infrastructure_vulnerability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRANIAN CIVILIAN POPULATION (SNARE) — No exit from military posture targeting their nation. Faces asymmetric vulnerability (drone strikes, naval blockade threats, cyberattacks). Cannot negotiate or exit the constraint. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.73.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL DESTABILIZATION (SNARE) — The deterrence posture itself generates instability: arms races, miscalculation risks, proxy escalation. No actor owns 'regional stability' as a constituency; it is extracted as a cost. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.74.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: US MILITARY-INDUSTRIAL COMPLEX (ROPE) — Primary beneficiary. Deterrence posture justifies carrier deployments, air defense procurement, missile sales, and base maintenance contracts. Experience is coordination: maintaining regional balance requires continuous military presence and technology refresh. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Negative extraction = net beneficiary.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ISRAELI SECURITY ESTABLISHMENT (ROPE) — Primary beneficiary. Deterrence posture provides force projection capability, intelligence sharing, missile defense partnerships, and strategic depth. Experience is coordination: US presence enables Israeli operations without direct escalation responsibility. d≈0.10, f(d)≈-0.09, σ=0.9 → χ≈-0.05.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: GULF STATE MONARCHIES (TANGLED ROPE) — Partially constrained beneficiaries. Deterrence reduces Iranian threat but also locks them into US alliance dependency, limits independent diplomacy, and exposes them to Iranian retaliation risks. Coordination benefit (security) and extraction cost (reduced autonomy) are both real. d≈0.50, f(d)≈0.65, σ=0.9 → χ≈0.34. Mixed experience: coordination and moderate extraction.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: REGIONAL PEACE/NEUTRALITY COALITION (TANGLED ROPE) — Actors (Iraq's neutrality attempts, Oman mediation, non-aligned states) see deterrence as both coordination mechanism and extraction of their autonomy. Cannot exit regional militarization; benefit from stability but constrained from diplomatic alternatives. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.46.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: COLD WAR SECURITY ARCHITECTURE (PITON) — The deterrence posture relies on performative military presence theater: carrier strike groups, AWACS patrols, port calls that signal resolve but rarely escalate to actual conflict. The ritual persists through institutional inertia (regional command structure, base agreements, alliance habits) despite the original strategic logic (Soviet containment) being obsolete. theater_ratio=0.65 approaches piton gate (≥0.70); the structure is mostly performative signaling. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.04.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: POTENTIAL DIPLOMATIC SETTLEMENT ARCHITECTURE (SCAFFOLD) — If viewed as a temporary bridge to negotiated settlement (JCPOA model, or future nuclear agreement), the deterrence posture could be a scaffold with sunset: military presence backs diplomacy until nuclear constraints are verified and reduced trust requirements. d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.29. Analytical perspective: the constraint has coordination function (enables negotiation from strength) but extraction cost (military-industrial dependency, arms race incentives). Sunset logic requires treaty verification infrastructure and Iranian compliance monitoring — possible but currently unresolved.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_military_deterrence_mideast_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regional_military_deterrence_mideast, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regional_military_deterrence_mideast, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regional_military_deterrence_mideast, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regional_military_deterrence_mideast, TR),
    TR >= 0.70.

:- end_tests(regional_military_deterrence_mideast_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated but not maximum. The deterrence posture does provide genuine coordination benefit (Iranian missile launches are constrained, some escalation paths are deterred). However, significant extraction occurs through military procurement cycles, base expansion, alliance dependency lock-in, and the implicit threat posture against a civilian population. The intermediate value (0.58 vs 0.75 for pure snare) reflects that genuine deterrence function exists alongside extraction. Suppression (0.68): High. Suppressed alternatives include: diplomatic negotiation channels (deterrence dominance reduces negotiating incentive for Iran to come to table from Iran's perspective; suppresses alternative security architectures), non-military conflict resolution, Iranian retaliation options (suppressed by asymmetric capability), regional neutrality options (suppressed for Gulf states by alliance requirements). Theater ratio (0.65): Moderate-high. Significant theatrical content: carrier strike group deployments primarily signal resolve rather than engage; port calls and AWACS patrols are messaging exercises; force positioning reflects basing agreements rather than dynamic tactical response. However, theater is not dominant (would require >0.70 for piton gate) — real capability deployment and actual deterrent effect exist. The trajectory shows theater increasing over time (0.50→0.65) as the posture shifts from active response (2003-2012 Iraq operations) to steady-state presence signaling.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. The US military-industrial complex and Israeli security establishment see Rope (pure coordination): we are solving the collective security problem of preventing Iranian aggression and regional instability. The Iranian civilian population sees Snare: maximum extraction, no exit, asymmetric vulnerability. Gulf state monarchies see Tangled Rope: genuine security benefit mixed with alliance dependency and reduced diplomatic autonomy. The regional destabilization vector sees extraction without benefit. The Cold War security architecture sees itself as Piton (degraded ritual maintained by institutional inertia). A diplomatic settlement framework could see Scaffold (military presence backs negotiation with sunset through treaty verification). The analytical observer risks seeing Mountain (deterrence is immutable geopolitical law given relative capabilities and geography), but the increasing theater ratio and extractiveness trajectory suggest this is a false summit — the constraint is historically contingent, not naturally inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   US military-industrial complex: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can exit constraint anytime by reduced deployment. Israeli security establishment: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.09. Net beneficiary; can reduce deterrence reliance or pursue alternative partnerships. Gulf state monarchies: Partially constrained victims/beneficiaries → d≈0.50, f(d)≈0.65. Mixed: benefit from Iranian deterrence but constrained by US alliance requirements and reduced diplomatic autonomy. Iranian civilian population: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; cannot exit military strike vulnerability or sanctions regime. Regional destabilization: Victim + trapped → d≈0.95, f(d)≈1.42. Structural victim with no constituency to advocate for stability; extracted as cost of deterrence system. Regional peace actors: Organized victims + constrained → d≈0.55, f(d)≈0.75. Significant extraction from reduced diplomatic latitude but some organizational capacity to pursue alternatives (Oman mediation attempts). Cold War architecture: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification from theater gate, not from directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by differentiating legitimate deterrence coordination from extraction logic through the victim/beneficiary structure. If the constraint were pure coordination (Rope), there would be no identifiable victims — all parties would perceive the deterrence as solving a collective action problem. Instead, the constraint creates identifiable victims (Iranian civilians, regional destabilization, peace advocates) who bear costs with minimal say in the system. This is the diagnostic signature of Tangled Rope: both coordination function (Iranian military aggression IS deterred; Gulf states ARE more secure) AND asymmetric extraction (Iranian civilians bear vulnerability costs; Gulf states lose diplomatic autonomy; arms races accelerate) coexist. The theater ratio increase over time (0.50→0.65) indicates that performative content is crowding out functional content — the constraint is degrading toward Piton. However, the classification as Tangled Rope is justified by the continued presence of both coordination (deterrence does work) and extraction (asymmetric cost distribution). The omega variables highlight the key uncertainties that could shift classification: if deterrence fails empirically (arms race dominates deterrent effect), the classification would shift toward Snare; if diplomatic settlement architecture succeeds, the constraint would become Scaffold with real sunset logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_success_measurement,
    'Has the US/Israeli deterrence posture actually prevented Iranian aggression, or has absence of major conflict been overdetermined by other factors (economic sanctions, internal Iranian constraints, proxy channel preferences)?',
    'Counterfactual analysis: comparison of Iranian military posture/capability deployment under different deterrence scenarios; declassified intelligence on Iranian decision-making; proxy group activity patterns relative to US force posture changes',
    'If deterrence successful: constraint is primarily coordination mechanism (Rope from more perspectives). If overdetermined: constraint is primarily extraction/signaling (Snare, Piton from more perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_success_measurement, empirical, 'Whether deterrence actually prevents Iranian aggression or is theater').

omega_variable(
    proxy_channel_independence,
    'To what degree can Iran pursue regional objectives through proxies (Hezbollah, Houthis, militias) independent of US/Israeli deterrence posture? Is deterrence against state actors decoupled from proxy warfare effectiveness?',
    'Empirical analysis of proxy operations before/after deterrence escalations; assessment of proxy financial/logistical dependency on Iran; comparison of proxy capability deployments vs US force posture timeline',
    'If proxies independent: deterrence fails its stated mission (extraction without coordination benefit). If proxies constrained: deterrence has real coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_channel_independence, empirical, 'Whether proxies operate independently of US/Israeli deterrence').

omega_variable(
    arms_race_acceleration_mechanism,
    'Does the deterrence posture generate arms race acceleration (Iranian missile development, drone programs, naval expansion) that net increases regional vulnerability despite deterrent intent?',
    'Temporal correlation analysis: Iranian military capability timelines vs US/Israeli deployments; cost-benefit analysis of deterrence spending vs arms race mitigation; expert military assessment of net regional balance change',
    'If acceleration dominant: constraint is net-extractive (Snare, Piton perspective valid). If balance preserved: constraint is coordination (Rope perspective valid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arms_race_acceleration_mechanism, empirical, 'Whether deterrence accelerates arms race despite stated intent').

omega_variable(
    alliance_dependency_extraction,
    'For Gulf states, does dependence on US deterrence guarantee extraction through military sales, basing rights, and political leverage (alignment with US Israel policy, constraints on independent diplomacy)?',
    'Structural analysis of US-Gulf state security agreements; quantification of military sales tied to basing rights; tracking of Gulf diplomatic autonomy constraints; comparison with non-aligned or Chinese-partnered states'' policy latitude',
    'If extraction dominant: Gulf states are Snare from their perspective (not Tangled Rope). If autonomy preserved: Tangled Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alliance_dependency_extraction, empirical, 'Whether Gulf state alliance creates extraction dependency').

omega_variable(
    nuclear_escalation_constraint_validity,
    'Does the deterrence posture actually constrain Iranian nuclear weapons development, or do nuclear programs proceed on independent technological/political timeline regardless of military posture?',
    'Intelligence assessment of Iranian nuclear program decision points and drivers; correlation of nuclear progress with deterrence escalations vs sanctions vs internal politics; comparison of nuclear timelines under different US policy regimes',
    'If deterrence constrains: Rope/Tangled Rope classification justified. If nuclear program independent: deterrence is theater (Piton); extraction without coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_escalation_constraint_validity, empirical, 'Whether deterrence actually constrains nuclear development').

omega_variable(
    civilian_vulnerability_asymmetry,
    'Is the civilian vulnerability asymmetry (Iranian civilians face military strike risk; US civilians face minimal regional risk) inherent to geography and capability, or is it structurally engineered by basing choices and doctrine?',
    'Doctrinal analysis of US force positioning and strike doctrine; assessment of Iranian retaliation capability under different scenarios; geographic vulnerability modeling; comparison with symmetric deterrence architectures',
    'If engineered: constraint is Snare with conscious design. If geographic: constraint is less extractive, more like Mountain (immutable geography).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_vulnerability_asymmetry, conceptual, 'Whether civilian vulnerability asymmetry is structural or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regional_military_deterrence_mideast, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rmd_tr_t0, regional_military_deterrence_mideast, theater_ratio, 0, 0.5).
narrative_ontology:measurement(rmd_tr_t10, regional_military_deterrence_mideast, theater_ratio, 10, 0.58).
narrative_ontology:measurement(rmd_tr_t20, regional_military_deterrence_mideast, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(rmd_be_t0, regional_military_deterrence_mideast, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rmd_be_t10, regional_military_deterrence_mideast, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rmd_be_t20, regional_military_deterrence_mideast, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regional_military_deterrence_mideast, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(regional_military_deterrence_mideast, 0.42).
narrative_ontology:affects_constraint(regional_military_deterrence_mideast, gulf_state_strategic_dependency).
narrative_ontology:affects_constraint(regional_military_deterrence_mideast, iranian_military_development_trajectory).
narrative_ontology:affects_constraint(regional_military_deterrence_mideast, regional_proxy_warfare_escalation).
narrative_ontology:affects_constraint(regional_military_deterrence_mideast, middle_east_arms_market_dynamics).

% DUAL FORMULATION NOTE:
% The deterrence posture decomposes into multiple structurally distinct constraints: (1) deterrence-as-coordination (prevention of Iranian military aggression), (2) deterrence-as-extraction (military procurement, alliance dependency, civilian vulnerability), (3) deterrence-as-theater (performative signaling with limited functional content). This story treats them as a unified tangled rope with sub-components. Alternative decomposition would separate the coordination function (Rope, ε≈0.20) from the extraction mechanism (Snare, ε≈0.65) as distinct constraints linked by causal dependency. The integrated treatment is justified by the fact that the coordination benefit is structurally inseparable from the extraction mechanism — deterrence through military presence necessarily creates both coordination and extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regional_military_deterrence_mideast, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
