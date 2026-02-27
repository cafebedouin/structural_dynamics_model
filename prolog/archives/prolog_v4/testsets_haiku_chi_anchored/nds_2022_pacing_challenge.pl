% ============================================================================
% CONSTRAINT STORY: nds_2022_pacing_challenge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nds_2022_pacing_challenge, []).

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
 *   constraint_id: nds_2022_pacing_challenge
 *   human_readable: US National Defense Strategy 2022: Pacing Challenge Doctrine
 *   domain: geopolitical/defense_strategy
 *
 * SUMMARY:
 *   The 2022 US National Defense Strategy establishes a geopolitical
 *   constraint centered on 'integrated deterrence' against China (the 'pacing
 *   challenge') and Russia (an 'acute threat'). This doctrine functions as a
 *   hybrid coordination-extraction mechanism: it coordinates security
 *   arrangements among Indo-Pacific allies while simultaneously extracting
 *   resources from competing domestic priorities and constraining space for
 *   global arms control negotiations. The NDS exhibits all characteristics of
 *   a tangled rope—it solves a genuine coordination problem (collective
 *   deterrence against Chinese military modernization) while imposing
 *   asymmetric costs on fiscal sustainability, non-aligned states, and
 *   domestic social priorities. The theater ratio (0.65) reflects that
 *   strategic threat assessment contains performative elements: the 'pacing
 *   challenge' framing drives procurement cycles and alliance restructuring
 *   that may exceed empirical threat timelines, and NATO burden-sharing
 *   targets operate as cost-signaling (Piton theater) rather than
 *   force-generation necessity. The constraint is strongest for powerless
 *   agents (global arms control regime, competing domestic priorities) who
 *   have no exit option from NDS-determined strategic posture. It is least
 *   extractive for the military-industrial complex (beneficiary with
 *   arbitrage exit), which experiences the doctrine as beneficial
 *   coordination. For regional allies, the constraint is tangled: genuine
 *   security benefits coexist with forced alignment costs and China trade
 *   interdependence. For the non-aligned movement, NDS appears as temporary
 *   coercion with an exit path (multipolarity, strategic autonomy), placing
 *   them in the scaffold perspective. The false summit (mountain perspective)
 *   occurs when analysts naturalize the doctrine as inevitable great power
 *   competition rather than recognizing it as a contingent strategic choice.
 *
 * KEY AGENTS:
 *   - US Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — NDS mandates sustained acquisition spending and force modernization; net beneficiary from defense budget escalation
 *   - Indo-Pacific Regional Allies (Japan, South Korea, Australia): Mixed (powerful/constrained) — benefit from security umbrella and technology transfer; constrained by China economic interdependence and forced alignment choices
 *   - Global Arms Control Regime: Primary victim (powerless/trapped) — constrained by escalatory doctrine; no exit from NDS-driven strategic competition; bears cost of degraded verification infrastructure
 *   - US Fiscal Sustainability: Secondary victim (moderate/trapped) — defense budgets locked in by doctrine; constrained by threat perception; no political pathway to reallocation without strategic vulnerability perception
 *   - Competing Domestic Priorities (Healthcare, Infrastructure, Education): Structural victim (powerless/trapped) — displaced by defense spending priorities; no constituency powerful enough to exit NDS-driven escalation
 *   - Non-Aligned Movement & Global South: Organized agents (organized/mobile) — experience NDS as temporary coercion to bifurcate alignment; perceive exit path through multipolarity and strategic autonomy; constrained but not trapped
 *   - NATO Burden-Sharing Architecture: Institutional actor (institutional/arbitrage) — maintains performative cost-signaling (2% GDP targets); degraded institution sustained by inertia rather than force necessity (Piton classification)
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing doctrine as immutable law of great power competition; false summit detector identifies contingent strategic choice as non-natural
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nds_2022_pacing_challenge, 0.52).
domain_priors:suppression_score(nds_2022_pacing_challenge, 0.68).
domain_priors:theater_ratio(nds_2022_pacing_challenge, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nds_2022_pacing_challenge, extractiveness, 0.52).
narrative_ontology:constraint_metric(nds_2022_pacing_challenge, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nds_2022_pacing_challenge, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nds_2022_pacing_challenge, tangled_rope).
narrative_ontology:human_readable(nds_2022_pacing_challenge, "US National Defense Strategy 2022: Pacing Challenge Doctrine").
narrative_ontology:topic_domain(nds_2022_pacing_challenge, "geopolitical/defense_strategy").

domain_priors:requires_active_enforcement(nds_2022_pacing_challenge).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nds_2022_pacing_challenge, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(nds_2022_pacing_challenge, indo_pacific_regional_allies).
narrative_ontology:constraint_beneficiary(nds_2022_pacing_challenge, defense_technology_sectors).
narrative_ontology:constraint_victim(nds_2022_pacing_challenge, fiscal_sustainability_us_budget).
narrative_ontology:constraint_victim(nds_2022_pacing_challenge, global_arms_control_regime).
narrative_ontology:constraint_victim(nds_2022_pacing_challenge, competing_domestic_priorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL ARMS CONTROL REGIME (SNARE) — Cannot exit the escalatory logic; bears extraction cost of constrained negotiation space and arms race acceleration. NPT signatory states face pressure to align with US containment posture or face secondary sanctions. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.70.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: US FISCAL SUSTAINABILITY (SNARE) — Defense budgets locked into escalatory targeting by NDS doctrine; trapped by geopolitical threat perception; no political pathway to exit without strategic vulnerability perception. d≈0.88, f(d)≈1.33, σ=1.0 → χ≈0.58.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INDO-PACIFIC REGIONAL ALLIES (TANGLED ROPE) — Benefit from US security umbrella and technology transfer but constrained by China interdependence (trade, supply chains). Coordination function: collective deterrence. Asymmetric extraction: must accept US military presence and alignment costs. d≈0.65, f(d)≈1.02, σ=1.1 → χ≈0.55.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: US MILITARY-INDUSTRIAL COMPLEX (ROPE) — Primary beneficiary. NDS doctrine mandates sustained defense spending, technology development, and force posture. Experiences constraint as coordination: setting strategic direction for 5-20 year platform development. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.06. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPETING DOMESTIC PRIORITIES (SNARE) — Structurally displaced by defense budgeting priorities. No constituency powerful enough to exit NDS-driven defense spending escalation. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.59.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: NON-ALIGNED MOVEMENT & GLOBAL SOUTH (SCAFFOLD) — Organized actors (India, Indonesia, African Union, BRICS) experiencing NDS as temporary but reversible coercion to align with US-China bifurcation. See exit path: multipolarity, economic sovereignty, strategic autonomy frameworks. d≈0.35, f(d)≈0.28, σ=1.1 → χ≈0.16. Low effective extraction due to agent organization and perceived impermanence of bipolar framing.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: NATO BURDEN-SHARING ARCHITECTURE (PITON) — Theater ratio 0.65: NATO spending mandates (2% GDP) are performative cost-signaling more than force generation. Actual deterrent value contested. Ritual persistence through institutional inertia (post-Cold War organizational structure) despite reduced functional necessity. d≈0.12, f(d)≈-0.04, σ=1.0 → χ≈-0.02.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / HEGEMONIC STABILITY (FALSE SUMMIT) — Risks naturalizing NDS doctrine as an immutable law of great power competition. Hegemonic stability theory suggests US containment of rising powers is structurally inevitable and cost-independent. However, base properties (ε=0.52, suppression=0.68, theater=0.65) contradict mountain classification — the doctrine is a contingent strategic choice, not a law of nature. Engine detects false summit.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nds_2022_pacing_challenge_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nds_2022_pacing_challenge, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nds_2022_pacing_challenge, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nds_2022_pacing_challenge, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nds_2022_pacing_challenge, TR),
    TR >= 0.70.

:- end_tests(nds_2022_pacing_challenge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.52): Moderate-high. The NDS doctrine extracts measurable costs from fiscal sustainability, arms control verification, and competing domestic priorities while generating real coordination benefits for regional allies and defense sectors. The extraction is not maximal (ε ≥ 0.66) because genuine strategic coordination solves a real deterrence problem—allies freely join the security arrangement, and the military-industrial complex benefits align with national defense objectives. However, extraction increases over the interval (0.38→0.52) as threat inflation accelerates spending and constrains budget negotiations. Suppression (0.68): High. Significant structural barriers to exit include: (1) geopolitical lock-in (allies cannot exit without strategic vulnerability), (2) domestic political costs (no elected official can advocate alliance dissolution without career risk), (3) institutional momentum (military planning horizons 15-20 years), (4) threat perception monopoly (intelligence assessments classified from public scrutiny), and (5) arms race dynamics (containment justifies Chinese modernization, which justifies further containment). Theater Ratio (0.65): Moderate-high. Strategic threat assessment contains performative elements: the 'pacing challenge' framing (language selection) drives procurement cycles that may exceed empirical timelines; NATO 2% targets function as cost-signaling rather than force requirement validation; congressional testimony on China threat exhibits threat inflation dynamics; media messaging emphasizes urgency over uncertainty. However, theater is not dominant (>0.70) because real military capability development occurs and actual strategic choices follow the doctrine—it is not pure ritual like Piton.
 *
 * PERSPECTIVAL GAP:
 *   The NDS doctrine demonstrates stark perspectival divergence. The military-industrial complex sees pure coordination (Rope)—the doctrine aligns procurement to strategic direction with net beneficiary positioning. Regional allies see tangled coordination-extraction (Tangled Rope)—genuine security benefits constrained by China interdependence and forced alignment costs. The non-aligned movement sees temporary coercion (Scaffold)—the bipolar framing is real but reversible through multipolarity trajectories. NATO sees degraded ritual (Piton)—burden-sharing targets persist through institutional inertia with contested functional necessity. Global arms control sees pure extraction (Snare)—constrained negotiation space with no exit option. Competing domestic priorities see pure extraction (Snare)—displaced by defense spending with no political constituency to resist. The analytical observer risks false summit (Mountain)—naturalizing doctrine as inevitable rather than contingent. The perspectival gaps reflect differential structural positions: agents with exit options see coordination; agents trapped without alternatives see extraction. This gap is the core diagnostic signature of a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Military-industrial complex: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Regional allies: Mixed beneficiary/victim + constrained → d≈0.65, f(d)≈1.02. Significant extraction balanced against security benefits; constrained exit elevates d. Global arms control regime: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction—abstract collective with no exit and no constituency. US fiscal sustainability: Victim + trapped → d≈0.88, f(d)≈1.33. High extraction; trapped by political costs of reallocation. Competing domestic priorities: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction—structural displacement with no political pathway. Non-aligned coalitions: Victim + mobile → d≈0.35, f(d)≈0.28. Lower extraction due to perceived reversibility (multipolarity exit path) and agent organization. NATO: Beneficiary/institutional artifact + arbitrage → d≈0.12, f(d)≈-0.04. Slight beneficiary (institutional continuity); piton classification from theater gate, not directionality. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit—observer perspective risks naturalizing contingent choice.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY GATE (ε > 0.46, mandatrophy_resolved required): The NDS doctrine satisfies the tangled rope gate (beneficiaries + victims + active enforcement all present) and crosses the high-extraction threshold (ε=0.52 > 0.46). MANDATROPHY ISSUE: The constraint risks being mislabeled as pure extraction (Snare) when viewed from the perspective of fiscal sustainability or competing domestic priorities. The mandatrophy resolves by establishing that genuine coordination functions coexist with asymmetric extraction: (1) Regional allies receive real security benefits and accept constrained exit voluntarily (coordination), (2) the military-industrial complex and allied governments genuinely solve a deterrence problem (coordination), (3) yet these coordination benefits are distributed asymmetrically—global arms control and domestic priorities bear costs disproportionate to benefits (extraction). The doctrine is neither pure coordination (Rope) nor pure extraction (Snare); it is a hybrid where the coordination function for some agents (beneficiaries) extracts from other agents (victims). This is the defining signature of tangled rope. The false summit (analytical mountain) occurs when strategists naturalize the doctrine as inevitable great power competition, obscuring that it is a contingent institutional choice that could be restructured (e.g., through détente, multilateral framework, strategic ambiguity revision). The mandatrophy resolution confirms: the constraint is real coordination for beneficiaries + real extraction for victims, making tangled rope the correct classification, not false summit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    china_actual_military_parity_timeline,
    'What is the actual timeline for Chinese military parity with US force projection capability across the Indo-Pacific, and how sensitive is NDS doctrine validity to this timeline?',
    'Longitudinal analysis of military capability metrics (carrier strike group equivalents, hypersonic missile accuracy, cyber warfare capacity); comparison against DoD intelligence assessments and Chinese capability declarations; scenario modeling of technology development trajectories',
    'If timeline < 10 years: NDS containment rationale is empirically justified. If timeline > 20 years: NDS exhibits precautionary overreach, and doctrine is better classified as institutional extraction (Snare from more perspectives). If timeline indeterminate: doctrine becomes self-justifying threat assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_actual_military_parity_timeline, empirical, 'Timeline and certainty of Chinese military parity in Indo-Pacific projection').

omega_variable(
    alliance_cohesion_under_strain,
    'Can the Indo-Pacific alliance architecture sustain simultaneous China containment and economic interdependence, or does NDS doctrine force allies into an unstable bifurcation?',
    'Analysis of Japanese, South Korean, Australian, and ASEAN economic and military dependency on China vs. US security guarantees; stress-testing alliance stability under trade disruption scenarios and nuclear signaling events',
    'If alliance cohesion maintained: NDS represents genuine coordination benefit (Rope/Scaffold from more perspectives). If bifurcation becomes unstable: NDS becomes pure extraction (Snare) for regional allies facing forced alignment choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_cohesion_under_strain, empirical, 'Sustainability of Indo-Pacific alliance architecture under NDS strain').

omega_variable(
    fiscal_multiplier_defense_spending,
    'Does sustained NDS-driven defense spending yield positive fiscal multiplier effects sufficient to offset opportunity costs to domestic priorities, or is the extraction net-negative for US economy?',
    'Econometric analysis of defense spending multipliers; opportunity cost calculation (GDP forgone from displaced healthcare, infrastructure, education investment); long-term productivity and competitiveness impact modeling',
    'If multiplier > 1.0 and opportunity costs < 0.3% GDP annually: fiscal sustainability perspective improves (less snare-like). If multiplier < 1.0 or opportunity costs > 0.8% GDP: domestic priority extraction is structurally severe, strengthening snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_multiplier_defense_spending, empirical, 'Net fiscal impact of NDS defense spending escalation').

omega_variable(
    arms_control_regime_substitution,
    'Are new arms control frameworks emerging that provide equivalent strategic stability to pre-2022 NPT/MIRV verification regimes, or is NDS doctrine eroding non-recoverable verification infrastructure?',
    'Inventory of active arms control agreements; assessment of verification capacity under each framework; scenario modeling of crisis stability under degraded transparency (e.g., unverified hypersonic deployment)',
    'If new frameworks emerge with equivalent confidence intervals: arms control perspective improves (less trapped). If regime collapses without substitution: global constraint classification approaches pure snare (maximum d→1.0).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(arms_control_regime_substitution, conceptual, 'Whether alternative arms control regimes can substitute for degraded NPT verification').

omega_variable(
    multipolarity_irreversibility,
    'Is the shift to multipolarity (as perceived by non-aligned coalitions) reversible by US containment strategy, or does NDS doctrine accelerate irreversible decoupling?',
    'Analysis of multipolarity indicators (reserve currency alternatives, trade linkages outside dollar system, technology decoupling); comparative analysis with 1970s détente pathway and assessment of whether NDS strategy could replicate that re-integration',
    'If reversible: scaffold perspective''s sunset clause is real (temporary coercion with exit path). If irreversible: multipolarity becomes a mountain-like structural shift, and NDS doctrine is misaligned with underlying constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multipolarity_irreversibility, conceptual, 'Reversibility of multipolarity trajectory under continued US containment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nds_2022_pacing_challenge, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nds22_tr_t0, nds_2022_pacing_challenge, theater_ratio, 0, 0.48).
narrative_ontology:measurement(nds22_tr_t2, nds_2022_pacing_challenge, theater_ratio, 2, 0.58).
narrative_ontology:measurement(nds22_tr_t4, nds_2022_pacing_challenge, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(nds22_be_t0, nds_2022_pacing_challenge, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nds22_be_t2, nds_2022_pacing_challenge, base_extractiveness, 2, 0.47).
narrative_ontology:measurement(nds22_be_t4, nds_2022_pacing_challenge, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nds_2022_pacing_challenge, enforcement_mechanism).
narrative_ontology:affects_constraint(nds_2022_pacing_challenge, indo_pacific_arms_competition).
narrative_ontology:affects_constraint(nds_2022_pacing_challenge, technology_export_controls_regime).
narrative_ontology:affects_constraint(nds_2022_pacing_challenge, alliance_burden_sharing_fragmentation).
narrative_ontology:affects_constraint(nds_2022_pacing_challenge, global_arms_control_verification_degradation).

% DUAL FORMULATION NOTE:
% The NDS pacing challenge doctrine is upstream of several subsidiary constraints: it establishes the strategic framework that drives arms competition dynamics (downstream), technology export control intensification (downstream), alliance burden-sharing negotiations (downstream), and arms control regime erosion (downstream). Each downstream constraint has its own ε value reflecting specific empirical status; the NDS represents the coordinating strategic choice that activates them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nds_2022_pacing_challenge, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
