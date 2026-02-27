% ============================================================================
% CONSTRAINT STORY: doomsday_clock_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_framework, []).

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
 *   constraint_id: doomsday_clock_framework
 *   human_readable: Global Catastrophic Risk Management Framework
 *   domain: geopolitical/existential_risk
 *
 * SUMMARY:
 *   The Global Catastrophic Risk Management Framework represents humanity's
 *   primary institutional apparatus for reducing existential threats: nuclear
 *   non-proliferation treaties, climate agreements, biosafety norms, and AI
 *   governance structures. This constraint exhibits the archetypal Tangled
 *   Rope structure because it combines genuine coordination function
 *   (preventing mutual annihilation, setting shared emission baselines) with
 *   asymmetric extraction (nuclear weapons states retain deterrence, fossil
 *   fuel exporters delay transition, developing nations bear compliance
 *   costs). The framework's theater ratio has increased from 0.42 (1974,
 *   post-OPEC confidence) to 0.64 (2024, after repeated unfulfilled climate
 *   pledges and nuclear modernization), indicating that institutional
 *   activity is increasingly performative — annual risk assessments by the
 *   Bulletin of Atomic Scientists, UN climate conferences, and
 *   nonproliferation reviews increasingly serve ceremonial functions rather
 *   than changing behavior. The extractiveness has risen from 0.35 to 0.52 as
 *   the framework has matured: initial coordination gains have plateaued
 *   while the institutional apparatus itself becomes a mechanism for locking
 *   in incumbent advantages (nuclear weapons legality for existing states,
 *   grandfathered industrial emissions, sovereign immunity from enforcement).
 *   The constraint faces a mandatrophy crisis: the framework cannot be
 *   classified as pure coordination (Rope) because enforcement is asymmetric
 *   and victims are trapped; cannot be classified as pure extraction (Snare)
 *   because genuine risk reduction does occur; and cannot be classified as a
 *   temporary bridge (Scaffold) because no sunset clause exists and
 *   beneficiaries block exit mechanisms. The false summit test reveals that
 *   attempts to naturalize this framework as an inevitable law of physics —
 *   'mutual deterrence is the only rational response to nuclear weapons,'
 *   'carbon physics cannot be negotiated away' — are ideological
 *   constructions masking contingent institutional power arrangements.
 *
 * KEY AGENTS:
 *   - Nuclear Weapons States (US, Russia, China, UK, France): Primary beneficiaries (institutional/arbitrage) — retain legal nuclear arsenals, veto power over treaty amendments, security guarantees to non-nuclear allies at no cost. Exit mechanism: can simply decline further treaties (though reputational/sanctions costs apply).
 *   - Fossil Fuel Exporters (OPEC, Russia, major coal producers): Primary beneficiaries (institutional/arbitrage) — experience climate framework as coordination setting targets so distant (2050 net-zero, 2100 for Paris 1.5°C) that they can continue extraction profitably for 25+ years. Loopholes and carbon offsetting preserve business models. Exit: high cost due to stranded asset risk but not structurally impossible.
 *   - Global Catastrophic Risk Mitigation (abstract commons): Primary victim (powerless/trapped) — cannot exit existence of risk, cannot organize to demand different institutional design, cannot defect without universal defection. Framework's enforcement mechanisms targeted asymmetrically away from beneficiaries.
 *   - Developing Nations and Future Populations: Secondary victims (moderate/trapped) — must comply with nonproliferation and climate constraints that restrict development (no nuclear energy, costly renewables) while bearing disproportionate climate impacts from incumbent nations' historical emissions. Limited exit: can pursue alternative coalitions (BRICS, G77) but structural asymmetry persists.
 *   - Responsible State Actors: Mixed position (organized/constrained) — benefit from nonproliferation norms that prevent symmetric threats (rivals don't get nukes) but constrained by compliance requirements and deterrence risks. Can exit through treaty withdrawal (high cost) or treaty non-ratification (lower cost).
 *   - International Risk Assessment Institutions (UN, IPCC, Bulletin of Atomic Scientists): Piton perspective (institutional/arbitrage) — maintain elaborate assessment rituals and prestige frameworks despite declining functional power. Theater ratio 0.64 indicates ~64% of activity is performative confidence-building rather than risk reduction. Continue because alternatives haven't fully delegitimized them.
 *   - Transnational Climate and Security Networks (cities, corporations, civil society): Organized beneficiaries with mobile exit (organized/mobile) — benefit from coordination signals (Paris Agreement legitimacy, ESG capital flows) that enable capital flows and policy implementation. Can shift jurisdiction or sector if terms change. Strategic position enables some voice without full veto power.
 *   - Analytical Observer: Risks naturalizing contingent arrangements (analytical/analytical) — sees nuclear deterrence and climate physics as immutable laws, missing that the institutional framework's design choices (which states keep arsenals, which nations pay climate costs, how enforcement is asymmetric) are political, not physical.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_framework, 0.52).
domain_priors:suppression_score(doomsday_clock_framework, 0.68).
domain_priors:theater_ratio(doomsday_clock_framework, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_framework, extractiveness, 0.52).
narrative_ontology:constraint_metric(doomsday_clock_framework, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(doomsday_clock_framework, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_framework, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_framework, "Global Catastrophic Risk Management Framework").
narrative_ontology:topic_domain(doomsday_clock_framework, "geopolitical/existential_risk").

domain_priors:requires_active_enforcement(doomsday_clock_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_framework, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(doomsday_clock_framework, established_industrial_powers).
narrative_ontology:constraint_beneficiary(doomsday_clock_framework, fossil_fuel_exporters).
narrative_ontology:constraint_victim(doomsday_clock_framework, global_catastrophic_risk_mitigation).
narrative_ontology:constraint_victim(doomsday_clock_framework, developing_nations).
narrative_ontology:constraint_victim(doomsday_clock_framework, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GCR MITIGATION (SNARE) — Structural inability to exit existential risk coordination. Global catastrophic risk mitigation cannot opt out of the framework that is supposedly protecting it. All humans trapped in shared risk. Extraction mechanism: framework imposes costs of inaction and compliance asymmetrically. d≈0.94, f(d)≈1.40, σ=1.2 → χ≈0.88.
constraint_indexing:constraint_classification(doomsday_clock_framework, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATIONS/FUTURE GENERATIONS (SNARE) — Structurally trapped. Must comply with climate/nonproliferation constraints that restrict development pathways while bearing disproportionate climate impacts. No genuine exit option. Career risks and capital flows penalize defection. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.85.
constraint_indexing:constraint_classification(doomsday_clock_framework, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: RESPONSIBLE STATE ACTORS (TANGLED ROPE) — Constrained by sovereignty concerns and deterrence logic, but also benefit from mutual non-proliferation and arms control norms that reduce overall risk. Experience genuine coordination function (arms control prevents mutual annihilation) alongside extraction (nonproliferation restricts legitimate energy/military options). d≈0.58, f(d)≈0.68, σ=1.1 → χ≈0.38.
constraint_indexing:constraint_classification(doomsday_clock_framework, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NUCLEAR WEAPONS STATES (ROPE) — Primary beneficiaries. Framework legitimates nuclear deterrence, locks in strategic advantage, prevents symmetric proliferation. Security guarantees to allies without surrendering weapons. Compliance is selective (own arsenals protected, rivals constrained). Experiences framework as coordination: setting global rules that benefit the rule-setters. d≈0.12, f(d)≈0.05, σ=1.2 → χ≈0.03. Near-zero extraction because departure costs are minimal — they can defect with minimal institutional penalty.
constraint_indexing:constraint_classification(doomsday_clock_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FOSSIL FUEL EXPORTERS (ROPE) — Experience climate framework as coordination mechanism: setting shared emission targets that preserve incumbent business models through exemptions, loopholes, and delayed enforcement. Selective compliance (committed to net-zero by 2050, not 2030). Framework benefits incumbents through delay. d≈0.08, f(d)≈-0.05, σ=0.9 → χ≈-0.02. Negative effective extraction = strong beneficiary position.
constraint_indexing:constraint_classification(doomsday_clock_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: RISK ASSESSMENT INSTITUTIONS (PITON) — UN bodies, IPCC, Bulletin of Atomic Scientists maintain elaborate assessment rituals (Doomsday Clock, annual risk reviews, consensus statements) with declining functional power. Theater ratio 0.64 indicates that ~64% of institutional activity is performative confidence-building rather than actual risk mitigation. Frameworks persist through ceremonial prestige despite decaying predictive accuracy and enforcement mechanisms. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.01.
constraint_indexing:constraint_classification(doomsday_clock_framework, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: TRANSNATIONAL NETWORKS (TANGLED ROPE) — Organized subnational actors (cities, corporations, civil society) experience the framework as both enabling and constraining. Benefit from global coordination signals (Paris Agreement legitimacy, ESG capital flows) but also bear compliance costs and risk being used as extractive labor for institutional performance. Mobile exit option: can shift jurisdiction or sector allegiance. d≈0.52, f(d)≈0.65, σ=1.1 → χ≈0.37.
constraint_indexing:constraint_classification(doomsday_clock_framework, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (FALSE SUMMIT CANDIDATE) — From a universal/civilizational view, mutual nuclear deterrence and climate physics appear as natural laws: nuclear weapons create irreversible MAD logic, greenhouse gas physics is immutable. Framework appears as humanity's only rational response. However, base properties (ε=0.52, suppression=0.68, theater=0.64) contradict the mountain gate (ε must be ≤0.25). This is a FALSE SUMMIT: the observer is naturalizing contingent geopolitical power arrangements as laws of physics. The 'inevitability' of the framework is ideological, not structural.
constraint_indexing:constraint_classification(doomsday_clock_framework, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(doomsday_clock_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(doomsday_clock_framework, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(doomsday_clock_framework, TR),
    TR >= 0.70.

:- end_tests(doomsday_clock_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The framework extracts compliance costs asymmetrically — developing nations must forgo nuclear energy and bear climate transition costs, while incumbent states keep arsenals and industrial advantage. However, extraction is not maximal (0.70+) because genuine risk reduction does occur: actual nuclear wars have been avoided (counterfactual: without deterrence norms, more proliferation), and emission baselines do constrain some behavior. The extractiveness has risen over 50 years as institutional maturity has allowed beneficiaries to lock in advantages. Suppression (0.68): High. Significant barriers to exit and defection include: (1) reputational/sanctions costs for non-compliance, (2) capital flight and financial exclusion for defectors, (3) technical dependencies (enriched uranium supply, fossil fuel infrastructure), (4) information asymmetry (beneficiaries control media narratives about framework legitimacy), and (5) path dependence (institutional inertia). But suppression is not maximal (0.90+) because some states do pursue exit strategies (Iran nuclear program despite NPT, Saudi Arabia pursuing renewables despite OPEC), and transnational networks provide alternative coordination pathways. Theater ratio (0.64): Moderate-high. The framework's institutional activity is increasingly performative: Doomsday Clock moves by 30 seconds each year despite no measurable change in actual policy; UN climate conferences conclude with non-binding commitments; nuclear modernization proceeds despite nonproliferation rhetoric; carbon accounting methodologies are gamed through offsetting schemes. Theater has increased as enforcement capacity has declined and institutional activity has become substitute for behavioral change. Beneficiary-victim gap (0.52 - 0.08 = 0.44): Large perspectival gap between institutional beneficiaries' rope perception and powerless commons' snare perception indicates the framework has matured from genuine coordination (initial NPT, Paris Agreement optimism) to extraction mechanism (current lock-in of incumbent advantage). Mandatrophy: Framework cannot resolve because it is genuinely hybrid — removing extraction mechanisms would destroy coordination function (if nuclear states can't lock in deterrence advantage, they abandon the treaty entirely), but acknowledging extraction would delegitimize the framework's moral authority (if developing nations call it extraction, cooperation collapses).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence: nuclear weapons states see Rope (coordination enabling mutual security), while developing nations see Snare (extraction of compliance). Fossil fuel exporters see Rope (coordination setting achievable targets), while climate victims see Snare (extraction of development opportunity). International institutions see Piton (degraded but prestigious ritual), while organized transnational networks see Tangled Rope (mixed coordination and constraint). The analytical observer risks seeing Mountain (immutable laws of physics and deterrence logic) when the structure is actually contingent power arrangements. The perspectival gap is NOT a measurement problem — it reflects that the framework genuinely IS a coordination mechanism for beneficiaries (extraction prevents their mutual defection) and genuinely IS an extraction mechanism for victims (compliance costs are imposed asymmetrically). The gap reveals the framework's hybrid nature: it solves the beneficiaries' collective action problem at the cost of imposing asymmetric burdens on the powerless commons.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapons states: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.05. Net beneficiary position. Their exit costs are moderate (reputational/diplomatic, not existential). Fossil fuel exporters: Beneficiary + arbitrage → d≈0.12, f(d)≈0.02. Near-zero effective extraction because they have significant exit options (alternative investment flows, climate skepticism campaigns). Developing nations/future populations: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Exit costs include development restrictions, capital flows conditional on compliance, and inability to opt out of climate/nuclear risk. Global catastrophic risk commons: Victim + trapped → d≈0.94, f(d)≈1.40. Cannot exit or organize. Responsible state actors: Mixed (organized/constrained) → d≈0.58, f(d)≈0.68. Moderate extraction because they benefit from non-proliferation norms (prevent symmetric threats) but constrained by deterrence reciprocity and treaty obligations. International institutions: Piton classification from theater gate (0.64 ≥ 0.70 fails, but still high), beneficiary + arbitrage → d≈0.10, f(d)≈-0.03. Transnational networks: Organized + mobile → d≈0.52, f(d)≈0.65. Moderate extraction with exit optionality. Analytical observer: analytical → d≈0.73, f(d)≈1.15. False summit classification reveals observer is naturalizing contingency.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY NOT RESOLVED. This constraint exhibits the archetype mandatrophy: it appears to demand mutually incompatible classifications depending on whether you prioritize beneficiary coordination function or victim extraction reality. Resolution would require one of three impossible outcomes: (1) Beneficiaries accept reclassification as Snare extractors, delegitimizing the framework and causing defection (NPT collapses if nuclear states admit they're using it for advantage lock-in). (2) Framework mechanisms become transparent and symmetrical, destroying the coordination function for beneficiaries (if developing nations can truly exit, nuclear states lose leverage and may withdraw). (3) Victims organize sufficiently to become 'organized' agents with real power, transforming to Tangled Rope with negotiating capacity — but this would require unprecedented global coordination at the very moment the framework is fragmenting. The mandatrophy is structural: the framework MUST maintain performative ambiguity (presenting itself as pure coordination for beneficiaries while imposing asymmetric costs on victims) to function. Full resolution would require institutional redesign with genuine symmetry and enforcement — a different constraint altogether. Current institutional maintenance strategy: increase theater ratio (more conferences, more rituals, more prestige) to maintain legitimacy narrative despite declining enforcement capacity and widening perspectival gap between beneficiaries' perception and victims' reality. This is the classic piton dynamic — ritual persistence substituting for functional repair.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_stability_assumption,
    'Is nuclear deterrence genuinely stable (fewer wars due to MAD) or does it create hidden costs that offset its stabilizing effects?',
    'Historical correlation analysis of war frequency pre/post-nuclear era; counterfactual modeling of non-nuclear geopolitical dynamics; analysis of proxy conflicts as hidden war costs',
    'If stable: framework is justified coordination mechanism (Rope from nuclear powers'' perspective is correct). If unstable or costs are hidden: framework is extraction mechanism masking structural fragility (Snare classification strengthens).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_stability_assumption, empirical, 'Whether nuclear deterrence reduces or masks conflicts').

omega_variable(
    climate_enforcement_credibility,
    'Do climate agreements have meaningful enforcement mechanisms, or are they purely aspirational commitments with theatrical compliance theater?',
    'Tracking of NDC (Nationally Determined Contributions) fulfillment rates; analysis of penalty mechanisms for non-compliance; comparison of stated vs actual emission trajectories; investigation of carbon accounting methodologies for gaming potential',
    'If enforceable: framework has real coordination function (Rope/Tangled Rope valid). If theatrical: framework is Piton (degraded ritual) or Snare (asymmetric enforcement targets developing nations while protecting incumbents).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_enforcement_credibility, empirical, 'Whether climate agreements have real enforcement power').

omega_variable(
    institutional_capture_extent,
    'To what degree has the GCR framework been captured by weapons-state and fossil-fuel interests, rendering it a mechanism for locking in incumbent advantage rather than reducing catastrophic risk?',
    'Analysis of treaty revision processes; historical tracking of which proposals succeeded/failed and whose interests they served; examination of exemptions and loopholes by beneficiary group; investigation of campaign financing for pro-incumbent climate/nuclear policies',
    'If capture is minimal: framework is legitimate coordination (Rope/Tangled Rope). If capture is extensive: framework is Snare masquerading as Rope — beneficiaries benefit from ''reduction'' frame while costs are imposed on victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_extent, empirical, 'Degree of institutional capture by incumbent powers').

omega_variable(
    alternative_coordination_possibility,
    'Could global catastrophic risks be managed through alternative institutional structures (distributed AI safety research, supranational courts, decentralized monitoring) that have lower extraction overhead?',
    'Comparative institutional analysis; pilot studies of alternative governance models; technical feasibility assessment for distributed verification systems; game-theoretic analysis of equilibrium stability under different institutional rules',
    'If alternatives are feasible: current framework is not inevitable (false summit exposed). If alternatives fail empirically or theoretically: current framework''s dominance reflects structural necessity, not just power (framework moves toward Mountain or justified Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_possibility, conceptual, 'Feasibility of alternative GCR governance structures').

omega_variable(
    systemic_risk_correlation,
    'Do nuclear proliferation constraints and climate constraints operate independently, or does controlling one increase risk in the other (e.g., restricting nuclear energy increases fossil fuel dependence)?',
    'Network analysis of constraint interdependencies; modeling of substitution effects; historical case studies of policy trade-offs; energy systems analysis linking nuclear/fossil/renewable pathways',
    'If independent: two separate constraints, each needs independent analysis. If correlated: framework creates hidden systemic risk that offset-calculations miss. Framework''s effective catastrophic risk reduction may be lower than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_risk_correlation, empirical, 'Whether GCR constraint dimensions are independent or interdependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_framework, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doomsday_tr_t0, doomsday_clock_framework, theater_ratio, 0, 0.42).
narrative_ontology:measurement(doomsday_tr_t25, doomsday_clock_framework, theater_ratio, 25, 0.54).
narrative_ontology:measurement(doomsday_tr_t50, doomsday_clock_framework, theater_ratio, 50, 0.64).

% Extraction over time
narrative_ontology:measurement(doomsday_be_t0, doomsday_clock_framework, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(doomsday_be_t25, doomsday_clock_framework, base_extractiveness, 25, 0.44).
narrative_ontology:measurement(doomsday_be_t50, doomsday_clock_framework, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_framework, global_infrastructure).
narrative_ontology:affects_constraint(doomsday_clock_framework, nuclear_deterrence_stability).
narrative_ontology:affects_constraint(doomsday_clock_framework, carbon_lock_in_mechanism).
narrative_ontology:affects_constraint(doomsday_clock_framework, international_enforcement_asymmetry).
narrative_ontology:affects_constraint(doomsday_clock_framework, developing_nation_debt_constraint).

% DUAL FORMULATION NOTE:
% The doomsday_clock_framework decomposes into multiple structurally distinct constraints: (1) nuclear_deterrence_stability (ε≈0.15, Mountain or Rope depending on whether MAD is actually stabilizing), (2) climate_enforcement_credibility (ε≈0.48, Piton — performative assessment with declining functional capacity), (3) nonproliferation_lock_in (ε≈0.45, Tangled Rope — genuine coordination for weapons states, extraction for developing nations). These share institutional apparatus but have distinct failure modes and remediation pathways. The aggregate framework's ε≈0.52 represents the composite extraction across all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_framework, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
