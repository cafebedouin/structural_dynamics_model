% ============================================================================
% CONSTRAINT STORY: us_indo_pacific_force_posture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_indo_pacific_force_posture, []).

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
 *   constraint_id: us_indo_pacific_force_posture
 *   human_readable: US Indo-Pacific Force Posture and Regional Coordination
 *   domain: geopolitical/military/strategic_coordination
 *
 * SUMMARY:
 *   The US Indo-Pacific force posture represents a multilayered constraint on
 *   regional autonomy and development that simultaneously functions as a
 *   genuine coordination mechanism for shared security problems. Forward
 *   military deployment, basing access, military-to-military partnerships,
 *   and alliance structures create both deterrent effects and extraction
 *   mechanisms across different regional actors. Small island nations are
 *   structurally trapped by geographic vulnerability and security dependence;
 *   ASEAN as a collective experiences mixed coordination and subordination;
 *   the US strategic establishment benefits from forward access and deterrent
 *   credibility; regional development states see force posture as temporary
 *   scaffolding enabling growth; China experiences the posture as
 *   containment; Cold War-era basing infrastructure persists through
 *   institutional inertia; neutrality-seeking states face suppressed
 *   alternatives. The constraint's extractiveness has increased over the
 *   measurement interval as US strategic posture has shifted from balancing
 *   to containment, and as Chinese military capability growth has intensified
 *   competition for regional influence. Theater ratio has risen as much
 *   military activity has become performative demonstration of commitment
 *   rather than response to immediate threats.
 *
 * KEY AGENTS:
 *   - Small Island Nations: Primary victims (powerless/trapped) — geographic vulnerability creates structural dependency on US security guarantees; cannot pursue independent foreign policy without existential risk
 *   - ASEAN Institutional Body: Organized agents (organized/constrained) — benefit from regional coordination functions but constrained by subordination to great power competition; active enforcement through security partnerships
 *   - US Strategic Establishment: Primary beneficiary (institutional/arbitrage) — captures forward basing access, alliance interoperability, regional influence; benefits from deterrent credibility; has exit options through global repositioning
 *   - Regional Development States: Secondary beneficiary (moderate/mobile) — benefit from security umbrella enabling economic growth; see force posture as temporary scaffold with intended sunset as development matures
 *   - China: Powerful target (powerful/constrained) — experiences posture as containment mechanism; simultaneously benefits from coordination on non-zero-sum functions (freedom of navigation, disaster response)
 *   - Cold War Basing Infrastructure: Institutional actor (institutional/arbitrage) — persists through path dependence; maintains force structure that original strategic justification (Soviet containment) no longer applies
 *   - Neutrality-Seeking States: Trapped actors (moderate/constrained) — Vietnam, Thailand, Indonesia attempt non-aligned status but face suppressed alternatives through military partnership pressure; cannot achieve true independence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_indo_pacific_force_posture, 0.58).
domain_priors:suppression_score(us_indo_pacific_force_posture, 0.65).
domain_priors:theater_ratio(us_indo_pacific_force_posture, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_indo_pacific_force_posture, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_indo_pacific_force_posture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_indo_pacific_force_posture, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_indo_pacific_force_posture, tangled_rope).
narrative_ontology:human_readable(us_indo_pacific_force_posture, "US Indo-Pacific Force Posture and Regional Coordination").
narrative_ontology:topic_domain(us_indo_pacific_force_posture, "geopolitical/military/strategic_coordination").

domain_priors:requires_active_enforcement(us_indo_pacific_force_posture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_indo_pacific_force_posture, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(us_indo_pacific_force_posture, us_strategic_hegemony).
narrative_ontology:constraint_beneficiary(us_indo_pacific_force_posture, allied_security_guarantees).
narrative_ontology:constraint_victim(us_indo_pacific_force_posture, regional_autonomy).
narrative_ontology:constraint_victim(us_indo_pacific_force_posture, development_resources).
narrative_ontology:constraint_victim(us_indo_pacific_force_posture, peaceful_dispute_resolution).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL ISLAND NATIONS (SNARE) — Caught between dependency on US security guarantees and inability to pursue independent foreign policy. Geographic vulnerability and limited military capacity create structural trap. Cannot exit without facing existential security risk or economic isolation. Maximum extraction experienced.
constraint_indexing:constraint_classification(us_indo_pacific_force_posture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ASEAN COORDINATION MECHANISM (TANGLED ROPE) — Genuine coordination function: shared maritime security, freedom of navigation protocols, multilateral dispute mechanisms. Simultaneously experiences extraction: force posture constrains policy autonomy, subordinates regional solutions to great power competition, raises military spending benchmarks. Active enforcement through security partnerships and base access. Mixed benefit and cost structure.
constraint_indexing:constraint_classification(us_indo_pacific_force_posture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US STRATEGIC ESTABLISHMENT (ROPE) — Benefits from force posture through forward basing access, partner interoperability, regional influence, and deterrent credibility. Experiences constraint as coordination mechanism: sustaining alliances requires compatible force structures and communication protocols. Net beneficiary with significant arbitrage options (repositioning globally, adjusting force mix, changing allied commitment levels).
constraint_indexing:constraint_classification(us_indo_pacific_force_posture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL DEVELOPMENT STATES (SCAFFOLD) — Benefit from security umbrella enabling economic growth. See force posture as temporary stabilization mechanism with intended sunset: as development accelerates and regional institutions mature, security guarantees should become less necessary. Theater ratio moderate because effectiveness is mixed — genuine deterrence alongside performative readiness. Sunset logic: mature economies graduate from security dependence.
constraint_indexing:constraint_classification(us_indo_pacific_force_posture, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: CHINA (TANGLED ROPE) — Experiences constraint as containment mechanism: force posture is explicitly configured to limit Chinese regional influence. Simultaneously, force posture coordinates with China on non-zero-sum functions: freedom of navigation, disaster response, commercial shipping protection. Active enforcement through military-to-military protocols and rules of engagement. Powerful but constrained by the force posture's structural logic.
constraint_indexing:constraint_classification(us_indo_pacific_force_posture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: COLD WAR INSTITUTIONAL INERTIA (PITON) — Many bases and force posture elements persist through institutional continuity rather than current strategic logic. Guam buildup, Japan basing, Okinawan rotation cycles reflect Cold War planning that outlasted its strategic rationale. Theater ratio elevated because much current activity is performative maintenance of historically-justified positions. The function (Soviet containment) has atrophied; the structure (bases, rotation schedules, allied commitments) persists.
constraint_indexing:constraint_classification(us_indo_pacific_force_posture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: NEUTRALITY-SEEKING STATES (SNARE) — Vietnam, Thailand, Indonesia attempt non-aligned positions but face pressure to take sides through military-to-military relationships, arms sales, base access negotiations. Force posture's mere presence constrains policy autonomy. Suppressed alternatives: true neutrality, equidistant diplomacy, autonomous defense. High suppression through security partnerships that embed alignment assumptions.
constraint_indexing:constraint_classification(us_indo_pacific_force_posture, snare,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / GEOGRAPHIC DETERMINISM (MOUNTAIN) — From civilizational scale, some force posture is dictated by geographic reality: US power projection requires forward bases in archipelagic zones; geographic advantage accrues to powers controlling sea lanes. This perspective risks naturalizing as 'geographic inevitability' what is actually a contingent institutional choice (e.g., maintaining current base structure rather than exploring alternative deterrence architectures). Engine's false summit detector applies here.
constraint_indexing:constraint_classification(us_indo_pacific_force_posture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_indo_pacific_force_posture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_indo_pacific_force_posture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_indo_pacific_force_posture, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_indo_pacific_force_posture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_indo_pacific_force_posture, TR),
    TR >= 0.70.

:- end_tests(us_indo_pacific_force_posture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial but not maximal. The force posture generates significant asymmetric benefit for the US strategic establishment (forward access, deterrent credibility, alliance management) while constraining regional autonomy and forcing alignment choices. However, extraction is not complete because genuine coordination functions exist (freedom of navigation, disaster response protocols) and some regional actors derive real security benefits. The increase from 0.35 to 0.58 over the measurement interval reflects US strategic pivot from balancing to containment as Chinese capabilities grew. Suppression (0.65): High. Structural barriers to independent action include: geographic vulnerability, military technology asymmetry, alliance lock-in effects, defense spending benchmarks embedded in partnerships, security dependency. Neutral alternatives (true non-alignment, autonomous defense, equidistant diplomacy) are suppressed through implicit cost of isolation. Theater ratio (0.55): Moderate. Force posture has genuine deterrent function (Freedom of Navigation Operations, interoperability training, deterrent signaling) but significant performative content (allied reassurance exercises, commitment demonstrations, strategic communication theater).
 *
 * PERSPECTIVAL GAP:
 *   ASEAN and China experience the constraint as tangled rope from opposite positions: ASEAN sees regional subordination with coordination benefits; China sees containment with coordination opportunities. Small island nations see snare (no exit). US sees rope (beneficial coordination). Development states see scaffold (temporary). Cold War residue sees piton (inertial). The gap is maximal between powerless trapped agents and institutional beneficiaries — they live in different structural realities within the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from each agent's structural position relative to the force posture constraint. Small island nations have no exit (trapped) and bear costs (victims): d approaches 1.0, experiencing maximum χ. US beneficiaries have arbitrage exit and benefits (beneficiaries): d approaches 0.0, experiencing negative or minimal χ. ASEAN faces constrained exit (partnership lock-in) with mixed benefits and costs: d ≈ 0.55. China is powerful but constrained (no exit from containment): d ≈ 0.65. Development states have moderate mobility and benefit from temporary arrangement: d ≈ 0.45. The directionality structure reveals why different agents classify the same constraint so differently — it is not disagreement about facts but different structural positions in the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the tangled rope classification is robust across multiple victim and beneficiary configurations. US benefits (forward access, deterrent credibility, alliance management) are genuine and asymmetric. Regional victims (constrained autonomy, forced alignment, development resource diversion) are real structural costs. Active enforcement occurs through security partnerships and base access conditionality. The classification resists reduction to pure extraction (snare) because coordination functions are non-trivial (freedom of navigation, disaster response, interoperability) and some regional actors derive real security benefits. It resists reduction to pure coordination (rope) because the asymmetry is stark: beneficiary can exit via repositioning; victims cannot exit without security degradation. The snare perspective from trapped island nations is correct for their position but not universal. The piton observation (Cold War residue) is valid but does not dominate current strategic logic. Mandatrophy resolves by accepting that multiple types are true from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_vs_provocation_boundary,
    'At what force posture threshold does deterrence logic transition to provocation logic? Is current posture stabilizing or destabilizing?',
    'Regional stability metrics: incident rates, military exercises frequency, arms race dynamics before/after force posture shifts; cross-national perception surveys of threat and deterrence effectiveness',
    'If deterrence dominant: constraint is more Rope (coordination). If provocation effects exceed deterrence: constraint is more Snare (extraction from regional stability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_provocation_boundary, empirical, 'Deterrence vs provocation balance in current force posture').

omega_variable(
    regional_institutional_maturity_timeline,
    'What level of institutional development (ASEAN maturity, intra-regional dispute resolution, autonomous defense capability) would justify force posture reduction without security degradation?',
    'Comparative institutional analysis of ASEAN vs African Union, ECOWAS; assessment of current dispute resolution effectiveness; regional defense capability metrics vs external threat profiles',
    'If timeline is achievable (10-20 years): scaffold perspective is structurally sound, sunset is real. If institutional development perpetually deferred: scaffold is aspirational framing for a persistent snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_institutional_maturity_timeline, empirical, 'Institutional development required for force posture sunset').

omega_variable(
    base_economic_sovereignty_cost,
    'What is the actual development resource cost of hosting forward bases and military infrastructure? How much does it constrain economic autonomy?',
    'Cost accounting for environmental remediation, land use opportunity costs, defense spending benchmarks driven by alliance commitments, strategic infrastructure constraints',
    'If cost is modest: extraction metric should be lower. If cost is substantial: extraction and suppression metrics should be higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(base_economic_sovereignty_cost, empirical, 'Economic sovereignty cost of military basing').

omega_variable(
    china_escalation_lock_in,
    'Does current force posture prevent Chinese regional hegemony or lock both powers into escalatory dynamics that neither can exit without losing credibility?',
    'Game-theoretic analysis of strategic stability; historical comparison to Cold War arms control models; assessment of off-ramps and negotiated boundaries',
    'If preventive: constraint is coordination (Rope). If lock-in: constraint is mutual entrapment (Snare for both powers, despite different power levels).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_escalation_lock_in, conceptual, 'Whether force posture prevents hegemony or locks in escalation').

omega_variable(
    basing_infrastructure_sunk_cost_fallacy,
    'How much of current force posture persists due to sunk cost in infrastructure (bases, supply chains, training pipelines) rather than current strategic logic?',
    'Cost-benefit analysis of maintaining vs closing specific bases; comparison of justifications given at base opening vs current strategic documents; assessment of alternative force structures with equivalent capability but different geography',
    'If sunk cost is dominant: piton classification is accurate. If current logic justifies posture: snare/tangled rope classifications are more appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(basing_infrastructure_sunk_cost_fallacy, empirical, 'Sunk cost factors in basing persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_indo_pacific_force_posture, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usipfp_tr_t0, us_indo_pacific_force_posture, theater_ratio, 0, 0.4).
narrative_ontology:measurement(usipfp_tr_t10, us_indo_pacific_force_posture, theater_ratio, 10, 0.5).
narrative_ontology:measurement(usipfp_tr_t20, us_indo_pacific_force_posture, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(usipfp_be_t0, us_indo_pacific_force_posture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usipfp_be_t10, us_indo_pacific_force_posture, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(usipfp_be_t20, us_indo_pacific_force_posture, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_indo_pacific_force_posture, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_indo_pacific_force_posture, 0.12).
narrative_ontology:affects_constraint(us_indo_pacific_force_posture, us_china_strategic_competition).
narrative_ontology:affects_constraint(us_indo_pacific_force_posture, asean_strategic_autonomy).
narrative_ontology:affects_constraint(us_indo_pacific_force_posture, south_china_sea_dispute_resolution).
narrative_ontology:affects_constraint(us_indo_pacific_force_posture, regional_military_spending_arms_race).

% DUAL FORMULATION NOTE:
% The US Indo-Pacific force posture is a master constraint that structures multiple downstream constraints. Strategic competition with China (imposed containment), ASEAN autonomy (structural subordination), South China Sea disputes (enforced equilibrium), and regional arms race dynamics (competitive force buildup) are all influenced by force posture decisions. Network links establish causal dependency: force posture changes cascade through downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_indo_pacific_force_posture, organized, 0.55).
constraint_indexing:directionality_override(us_indo_pacific_force_posture, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
