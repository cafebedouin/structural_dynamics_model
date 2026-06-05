% ============================================================================
% CONSTRAINT STORY: nuclear_site_safety_norms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_site_safety_norms, []).

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
 *   constraint_id: nuclear_site_safety_norms
 *   human_readable: International Nuclear Site Non-Proliferation and Safety Norms
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   The international system of nuclear site safety norms, formalized through
 *   the Non-Proliferation Treaty (1968), IAEA safeguards, and customary
 *   geopolitical conventions against military attacks on civilian nuclear
 *   infrastructure, represents a hybrid constraint combining genuine
 *   coordination needs (preventing proliferation, maintaining energy
 *   stability) with asymmetric extraction (enforcement burden concentrated on
 *   non-aligned states, military-armed states exempt from equivalent
 *   scrutiny). The constraint has historically functioned as a Tangled Rope:
 *   it solves the collective action problem of preventing an arms race in
 *   weapons-usable fissile material while simultaneously extracting
 *   enforcement costs disproportionately from weaker states. However, recent
 *   events (Russia's 2022 attacks on Ukrainian nuclear sites, particularly
 *   Zaporizhzhia) have demonstrated the fragility of the norm when
 *   geopolitical incentives overcome it. From the perspective of civilian
 *   populations and militarily vulnerable nuclear operators, the constraint
 *   appears more as a Snare: an apparent protection that offers no real exit
 *   when the norm fails. The theater ratio (0.58) reflects that the IAEA
 *   inspection regime, while substantive, lacks coercive enforcement teeth —
 *   it functions largely through reputation and the threat of sanctions,
 *   which have proven inadequate against determined violators. The increasing
 *   theater ratio over the interval (0.35 in 1970 → 0.58 in 2022) indicates
 *   that the performative component has grown as the enforcement system has
 *   encountered repeated challenges without effective response.
 *
 * KEY AGENTS:
 *   - Civilian Populations Near Nuclear Sites: Powerless/trapped (no exit) — bear catastrophic risk with no veto over siting decisions
 *   - Non-Armed Nuclear Operators (Ukraine, Japan, etc.): Moderate/constrained — militarily vulnerable, unable to deter attacks despite norm; bear security investment burden
 *   - IAEA and International Monitoring Regime: Organized/constrained — enforces asymmetric safeguards that solve proliferation coordination but extract compliance costs from non-armed states
 *   - Nuclear-Armed States and Energy-Secure Industrialized Nations: Institutional/arbitrage — benefit from norm (protects their sites, ensures energy stability), have military or market leverage to enforce it
 *   - Climate-Energy Transition Coalition: Organized/mobile — sees nuclear as temporary climate solution; invested in renewable alternatives that would reduce enforcement burden
 *   - Treaty Enforcement Apparatus (NPT, UN, diplomatic system): Institutional/arbitrage — maintains rituals and reputational enforcement; lacks credible deterrence
 *   - Analytical Observer: Analytical/analytical — risks naturalizing geopolitical norms as if they were physical laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_site_safety_norms, 0.38).
domain_priors:suppression_score(nuclear_site_safety_norms, 0.52).
domain_priors:theater_ratio(nuclear_site_safety_norms, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_site_safety_norms, extractiveness, 0.38).
narrative_ontology:constraint_metric(nuclear_site_safety_norms, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(nuclear_site_safety_norms, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_site_safety_norms, tangled_rope).
narrative_ontology:human_readable(nuclear_site_safety_norms, "International Nuclear Site Non-Proliferation and Safety Norms").
narrative_ontology:topic_domain(nuclear_site_safety_norms, "geopolitical/technological").

domain_priors:requires_active_enforcement(nuclear_site_safety_norms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_site_safety_norms, civilian_population_proximity).
narrative_ontology:constraint_beneficiary(nuclear_site_safety_norms, global_environmental_commons).
narrative_ontology:constraint_beneficiary(nuclear_site_safety_norms, industrial_state_energy_infrastructure).
narrative_ontology:constraint_victim(nuclear_site_safety_norms, militarily_vulnerable_operators).
narrative_ontology:constraint_victim(nuclear_site_safety_norms, non_aligned_nations).
narrative_ontology:constraint_victim(nuclear_site_safety_norms, enforcement_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — Residents proximate to nuclear infrastructure cannot exit the constraint. They are subject to catastrophic risk (Fukushima, Chernobyl) yet have no meaningful veto over siting decisions or operational safety. The norm creates an appearance of protection but offers no escape route if norms fail. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.64.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-ALIGNED NUCLEAR OPERATORS (SNARE) — Nations without nuclear weapons (e.g., Ukraine, Japan) operate reactors but are militarily vulnerable. The norm prohibits attacks but provides no enforcement against a determined aggressor (Russia's 2022 attack on Zaporizhzhia demonstrated norm failure). These operators bear extraction: they must invest in security, maintain expensive redundancy, and cannot deter attacks through retaliation. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.50.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IAEA AND MONITORING REGIME (TANGLED ROPE) — The international monitoring system (IAEA inspections, safeguards) solves a genuine coordination problem: verification that civil nuclear programs remain non-military. But the same system generates asymmetric extraction: weaker states are subject to intrusive inspections while nuclear-armed states face limited scrutiny. The regime requires active enforcement (inspection protocols, reporting mandates, sanctions threats) and has both coordination benefits (preventing proliferation) and extraction costs (sovereignty erosion for inspected states). d≈0.42, f(d)≈0.42, σ=1.1 → χ≈0.19.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: NUCLEAR-ARMED STATES / ENERGY EXPORTERS (ROPE) — Nations with nuclear weapons and those with secure energy infrastructure benefit from the norm's coordination function: it protects their own sites, ensures global energy stability, and preserves their structural advantage (military deterrence or energy market control). They experience the constraint as enabling coordination rather than extraction. The norm protects their interests and they have arbitrage options (sanctions threats, military deterrence, market leverage). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.04. Net beneficiary.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CLIMATE-ENERGY TRANSITION (SCAFFOLD) — The nuclear norm serves a temporary coordination function in climate mitigation (nuclear as low-carbon baseload). However, as renewable energy (solar, wind, storage) becomes cost-competitive, the enforcement burden of the norm declines. The sunset clause is implicit: nuclear's role in decarbonization is real but temporary. As transition technologies mature (2030–2050), the constraint's extraction mechanism weakens because alternatives emerge. d≈0.45, f(d)≈0.50, σ=1.1 → χ≈0.22.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TREATY ENFORCEMENT (PITON) — The Non-Proliferation Treaty (NPT) and related agreements constitute a degraded constraint: their primary enforcement mechanism is reputational and conditional (threat of sanctions), which has proven ineffective against determined violators (Iran's enrichment program, North Korea's weapons development, Russia's 2022 attacks on Ukrainian reactors). The rituals (IAEA inspections, UN declarations, diplomatic summits) persist despite weak enforcement. theater_ratio=0.58 indicates moderate performative content — inspections are substantive but lack coercive teeth. The apparatus functions through inertia and reputation rather than credible deterrence.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, reactor safety constraints appear inherent to nuclear physics: containment requirements, cooling demands, and decay heat management are irreducible physical facts. This perspective risks naturalizing the contingent geopolitical norms (treaties, inspections, deterrence) as if they were laws of physics. The engine will detect this as a false summit: the structural data (ε=0.38, suppression=0.52, theater=0.58) reveals that most of the constraint is institutional, not physical. True physical limits on reactor safety are much lower (ε≤0.10); the higher ε here reflects enforcement burden and geopolitical extraction.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_site_safety_norms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nuclear_site_safety_norms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nuclear_site_safety_norms, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(nuclear_site_safety_norms, TR),
    TR >= 0.70.

:- end_tests(nuclear_site_safety_norms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint redistributes costs toward non-armed operators and enforcement agencies while concentrating benefits among nuclear-armed states and energy-secure industrialized nations. The extraction is real but limited by: (1) the legitimate coordination benefit of nonproliferation; (2) the multilateral structure that prevents any single extractor from capturing all surplus; (3) the availability of energy alternatives that allow some escape. Rising from 0.18 (1970) to 0.38 (2022) reflects increasing enforcement burden as violations accumulate without strong response. Suppression (0.52): Moderate-high. Multiple barriers prevent exit: civil nuclear programs cannot easily switch to renewable-only (baseload dependency); military-vulnerable states cannot unilaterally withdraw from NPT without severe diplomatic costs; civilian populations have no veto; the geopolitical norm creates peer pressure against independent nuclear deterrence. These are coercive but not total — some states have developed nuclear weapons despite the regime (India, Pakistan, North Korea), and some have surrendered arsenals (South Africa, Ukraine). Theater ratio (0.58): Moderate. IAEA inspections are substantive verification activities, but they lack enforcement mechanism — the agency can report violations but cannot punish them without UN Security Council action (which nuclear-armed states can veto). Diplomatic summits and treaty reviews are largely performative. The theater has increased as enforcement gaps have widened without closing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full range of DR types across a narrow set of base metrics, revealing how structural position (military capability, geographic proximity, institutional affiliation) generates radically different classification experiences. Civilian populations see a Snare: apparent protection that evaporates when norms fail (Fukushima, Zaporizhzhia). Non-armed operators see a Snare: they bear enforcement burden and security cost without deterrence capability. The IAEA sees a Tangled Rope: solving proliferation coordination while extracting compliance costs asymmetrically. Nuclear-armed states and energy-secure nations see a Rope: coordination of mutual protection and energy stability. The climate transition movement sees a Scaffold: nuclear is useful but temporary. The treaty enforcement system sees itself as a Piton: performing its role through diminishing credibility. The analytical observer risks seeing a Mountain: naturalizing geopolitical norms as inherent to nuclear physics rather than contingent institutional arrangements. These are not different measurements of the same thing — they are genuinely different structural relationships to a single label.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations: Victims (no coordination benefit) + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — they cannot exit or organize resistance. Non-armed operators: Victims (bear enforcement cost) + constrained → d≈0.88, f(d)≈1.32. High extraction — they have some options (non-proliferation treaties, security partnerships) but cannot exit nuclear energy easily. IAEA: Both beneficiary (receives institutional legitimacy and funding) and victim (tasked with enforcement burden it cannot fully discharge) + constrained → d≈0.42, f(d)≈0.42. Blended; organizational benefits from coordination role but lacks coercive power. Nuclear-armed states: Beneficiaries (norm protects their sites and military advantage) + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary — they can threaten to withdraw from the system or veto enforcement. Climate coalition: Both beneficiary (nuclear helps decarbonization) and victim (renewable alternatives are emerging) + mobile → d≈0.45, f(d)≈0.50. Blended but with exit paths as technology matures. Treaty system: Institutional beneficiary (maintains legitimacy and influence) + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary in terms of institutional survival, victim in terms of credibility.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by exposing how a single institutional system (the NPT and IAEA regime) creates different constraint types for different structural positions. The mandatrophy question is not 'what is the true type?' but 'who experiences what type and why?' The constraint is genuinely a Tangled Rope at the system level (coordination + asymmetric extraction) while simultaneously being a Snare for powerless civilians and constrained non-armed operators. The recent failures (Ukraine 2022) reveal that the system's classification depends on fragile geopolitical conditions: when deterrence is absent or overwhelmed, the Rope collapses into a Snare for vulnerable parties while remaining a Rope for protected parties. The scaffold perspective (renewable energy transition) is empirically testable: if renewables can substitute nuclear by 2050, the constraint's extraction mechanism weakens. If not, nuclear remains permanently necessary and the tangled rope classification persists. The piton perspective (degraded enforcement) reflects that the treaty system's effectiveness has declined over time: early NPT (1970s) functioned relatively well; post-Iraq, post-Iran, post-North Korea, the enforcement apparatus has become increasingly theatrical. The false summit risk is the analytical observer who naturalizes the constraint as inherent to nuclear physics rather than recognizing it as a contingent geopolitical arrangement. The true physical constraint (reactor safety from thermal/mechanical limits) has ε≈0.05–0.10 (mountain level); the additional ε=0.28–0.38 reflects institutional, geopolitical, and economic factors that are empirically changeable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    norm_collapse_threshold,
    'At what point does norm violation (attacks on nuclear sites) become normalized rather than exceptional, collapsing the constraint?',
    'Empirical tracking of nuclear-site attacks; correlation with retaliatory capacity of targeted nations; assessment of whether subsequent attacks follow first violation',
    'If threshold crossed: constraint shifts from Tangled Rope to Snare globally (extraction without coordination benefit). If threshold holds: norm shows resilience despite violations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_collapse_threshold, empirical, 'Threshold for norm collapse via repeated violation').

omega_variable(
    enforcement_mechanism_adequacy,
    'Does the IAEA/NPT enforcement system actually prevent proliferation, or does it merely redistribute risk to non-aligned states while nuclear-armed states expand arsenals?',
    'Time-series analysis of global weapon-usable fissile material inventory; comparison of enforcement intensity on non-armed vs armed states; detection of asymmetric enforcement patterns',
    'If preventing proliferation: constraint is genuine tangled rope with real coordination benefit despite asymmetry. If merely redistributing risk: constraint is extraction mechanism (snare or worse) disguised as coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_adequacy, empirical, 'Whether IAEA enforcement actually prevents proliferation or merely redistributes enforcement burden').

omega_variable(
    deterrence_vs_institutional_norm,
    'Is the constraint maintained by military deterrence (implicit: attack a reactor and face retaliation) or by institutional/cultural norms (implicit: attacking civilians is taboo)?',
    'Analysis of attack patterns; identification of which states face credible deterrence vs which rely on norm internalization; survey of military doctrine statements on reactor targeting',
    'If deterrence-based: constraint is fragile (dependent on military balance) and asymmetric (armed states enforce through threat). If norm-based: constraint is more stable but contingent on cultural maintenance. Mixed: different regions operate under different mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_institutional_norm, conceptual, 'Whether constraint is maintained by deterrence or institutional norm').

omega_variable(
    renewable_substitution_timeline,
    'Can renewable energy and storage systems actually replace nuclear baseload by 2050, validating the scaffold sunset clause?',
    'Grid modeling studies for complete decarbonization; cost trajectory analysis for renewables + storage vs nuclear; empirical deployment rates for competing technologies',
    'If yes: scaffold perspective confirmed — nuclear safety constraints become optional as technology alternatives mature. If no: nuclear remains permanently necessary, validating tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_substitution_timeline, empirical, 'Whether renewable energy can substitute for nuclear baseload by 2050').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_site_safety_norms, 1970, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1970, nuclear_site_safety_norms, theater_ratio, 1970, 0.35).
narrative_ontology:measurement(nucl_tr_t1995, nuclear_site_safety_norms, theater_ratio, 1995, 0.48).
narrative_ontology:measurement(nucl_tr_t2022, nuclear_site_safety_norms, theater_ratio, 2022, 0.58).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1970, nuclear_site_safety_norms, base_extractiveness, 1970, 0.18).
narrative_ontology:measurement(nucl_be_t1995, nuclear_site_safety_norms, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(nucl_be_t2022, nuclear_site_safety_norms, base_extractiveness, 2022, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_site_safety_norms, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_site_safety_norms, nuclear_proliferation_incentives).
narrative_ontology:affects_constraint(nuclear_site_safety_norms, energy_infrastructure_vulnerability).
narrative_ontology:affects_constraint(nuclear_site_safety_norms, international_treaty_compliance).

% DUAL FORMULATION NOTE:
% The nuclear safety norm decomposes into two structurally distinct constraints: (1) Physical safety limits on reactor operation (ε≈0.08, Mountain) — thermal-hydraulic and containment limits from nuclear physics. (2) Geopolitical enforcement norms protecting sites from military attack (ε≈0.38, Tangled Rope) — the constraint story here addresses the second. These are linked via network.affects_constraints: the physical safety constraint influences operator design decisions, which the geopolitical norm must account for in enforcement (inspections, security assessments). The geopolitical constraint influences which nations operate reactors and under what conditions, affecting proliferation incentives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_site_safety_norms, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
