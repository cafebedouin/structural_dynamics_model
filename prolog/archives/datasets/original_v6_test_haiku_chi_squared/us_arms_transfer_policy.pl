% ============================================================================
% CONSTRAINT STORY: us_arms_transfer_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_arms_transfer_policy, []).

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
 *   constraint_id: us_arms_transfer_policy
 *   human_readable: US Arms Transfer Policy
 *   domain: political/military
 *
 * SUMMARY:
 *   The US Arms Transfer Policy creates a structural tension between
 *   legitimate alliance security coordination and the extraction of market
 *   rents, geopolitical leverage, and strategic technology control. The
 *   constraint governs weapon sales to foreign governments through three
 *   parallel mechanisms: Foreign Military Sales (government-to-government),
 *   Direct Commercial Sales (manufacturer-to-buyer), and Military Assistance
 *   Programs (grants/loans). These mechanisms involve the US State
 *   Department, Defense Department, and Congress as enforcers; US defense
 *   contractors as beneficiaries; recipient governments as both beneficiaries
 *   and constrained actors; and civilian populations in recipient nations as
 *   victims. The policy exhibits classic tangled-rope characteristics: a
 *   genuine coordination function (alliance deterrence, burden-sharing) is
 *   layered with significant asymmetric extraction (market access, technology
 *   control, geopolitical leverage). Over the 50-year interval,
 *   extractiveness has increased from 0.38 to 0.58 as conditionality has
 *   expanded, theater has increased from 0.42 to 0.68 as certification
 *   requirements have become more performative, and the policy has drifted
 *   toward snare from multiple perspectives.
 *
 * KEY AGENTS:
 *   - US Defense Industrial Base: Primary beneficiary (institutional/arbitrage) — secures markets, maintains production capacity, controls technology diffusion
 *   - US State Department: Institutional enforcer (institutional/arbitrage) — manages diplomatic leverage and conditionality; benefits from policy flexibility
 *   - US Congress: Organized overseer (organized/constrained) — formally constrains major transfers but constrained by information asymmetry and executive delegation
 *   - Recipient Nation Governments: Powerful constrained actors (powerful/constrained) — benefit from alliance and weapons access; constrained by US conditionality and asymmetric leverage
 *   - Recipient Civilian Populations: Primary victims (powerless/trapped) — bear costs of conflicts enabled by transfers; cannot exit participation in recipient government decisions
 *   - International Human Rights Advocates: Moderate mobile observers (moderate/mobile) — push conditionality frameworks but have limited enforcement power
 *   - International Humanitarian Law Apparatus: Institutional piton (institutional/arbitrage) — maintains formal verification theater with limited functional enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_arms_transfer_policy, 0.58).
domain_priors:suppression_score(us_arms_transfer_policy, 0.62).
domain_priors:theater_ratio(us_arms_transfer_policy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_arms_transfer_policy, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_arms_transfer_policy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_arms_transfer_policy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_arms_transfer_policy, tangled_rope).
narrative_ontology:human_readable(us_arms_transfer_policy, "US Arms Transfer Policy").
narrative_ontology:topic_domain(us_arms_transfer_policy, "political/military").

domain_priors:requires_active_enforcement(us_arms_transfer_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_arms_transfer_policy, us_defense_industrial_base).
narrative_ontology:constraint_beneficiary(us_arms_transfer_policy, us_state_department).
narrative_ontology:constraint_beneficiary(us_arms_transfer_policy, allied_governments).
narrative_ontology:constraint_victim(us_arms_transfer_policy, recipient_civilian_populations).
narrative_ontology:constraint_victim(us_arms_transfer_policy, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RECIPIENT CIVILIAN POPULATIONS (SNARE) — Cannot exit participation in recipient governments' conflicts enabled by US arms. Suppression high: no voting power in US policy, limited influence over recipient government procurement. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.73.
constraint_indexing:constraint_classification(us_arms_transfer_policy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RECIPIENT NATION GOVERNMENT (TANGLED_ROPE) — Benefits from US arms access and alliance relationship; constrained by US conditionality, Congressional oversight, and end-use monitoring. Suppression moderate: asymmetric relationship with US, limited ability to redirect transfers. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.39.
constraint_indexing:constraint_classification(us_arms_transfer_policy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: US DEFENSE INDUSTRIAL BASE (ROPE) — Primary beneficiary. Arms transfers secure markets, sustain production capacity, and maintain technological dominance. Experiences constraint as coordination: export control regime enables market access while preventing technology diffusion to competitors. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05.
constraint_indexing:constraint_classification(us_arms_transfer_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONGRESSIONAL OVERSIGHT BODIES (TANGLED_ROPE) — Organized actors with formal review authority over major transfers. Constrained by time pressure, classified information access, and executive branch expertise. Suppression moderate: ability to block transfers is real but executive can work around via agreements, reprogramming, and emergency provisions. d≈0.48, f(d)≈0.62, σ=1.2 → χ≈0.44.
constraint_indexing:constraint_classification(us_arms_transfer_policy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL HUMAN RIGHTS ADVOCATES (SCAFFOLD) — Mobile actors with exit options (advocacy, sanctions, litigation, media pressure). See arms transfer conditionality frameworks as temporary coordination mechanisms with sunset clauses. Theater moderate-high: conditionality certification is partly performative but creates real audit trails and veto points. d≈0.62, f(d)≈0.92, σ=1.2 → χ≈0.42.
constraint_indexing:constraint_classification(us_arms_transfer_policy, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL HUMANITARIAN LAW APPARATUS (PITON) — Geneva Conventions, ICCPR, Arms Trade Treaty create formal verification claims. Theater ratio high (0.68): certification procedures, end-use monitoring, and weapons-type restrictions are largely performative. Actual enforcement is limited by state sovereignty, resource constraints, and political will. Inertial maintenance: frameworks persist through legitimacy theater rather than functional verification. d≈0.12, f(d)≈0.05, σ=1.2 → χ≈0.04.
constraint_indexing:constraint_classification(us_arms_transfer_policy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / REALPOLITIK VIEW (MOUNTAIN) — From civilizational view, state arms transfers are inherent to international anarchy: states cannot exit self-help logic; arms are tools of deterrence and alliance management, not optional. This perspective sees the policy as an immutable feature of state competition. However, base properties (ε=0.58, suppression=0.62) reveal this as a false summit: extraction and suppression exist precisely because alternatives exist (arms control treaties, weapons restrictions, transparency regimes) and are actively suppressed by beneficiaries.
constraint_indexing:constraint_classification(us_arms_transfer_policy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_arms_transfer_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_arms_transfer_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_arms_transfer_policy, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_arms_transfer_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_arms_transfer_policy, TR),
    TR >= 0.70.

:- end_tests(us_arms_transfer_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts through multiple mechanisms: (1) recipient governments pay above-market prices for weapons, (2) technology sales enrich US contractors, (3) conditionality creates political leverage, (4) donor prestige and alliance dependence benefit US geopolitically. The increase from 0.38 to 0.58 reflects that extraction mechanisms have been layered on top of the coordination function over time — early transfers (1970s-1980s) focused on deterrence; later transfers (2000s-2020s) increasingly focused on market access and leverage. Suppression (0.62): High and persistent. Recipient governments have limited bargaining power; civilian populations have no voice in procurement; Congressional oversight is constrained by information asymmetry; alternative suppliers (Russia, China) create BATNA pressure but not exit options for many recipients. Theater ratio (0.68): High and rising. Certification procedures (human rights, counter-terrorism, end-use monitoring) are substantially performative — compliance documentation often follows transfers rather than preceding them, audit trails are limited, enforcement is selective based on geopolitical priorities, and violations rarely result in transfer halts.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates stark perspectival divergence. US defense contractors see pure coordination (Rope, d≈0.08, χ≈-0.05) — the policy solves a legitimate market access problem. Recipient governments see mixed coordination and extraction (Tangled Rope, d≈0.55, χ≈0.39) — they benefit from deterrence but suffer leverage asymmetry. Congressional overseers see organized but constrained power (Tangled Rope, d≈0.48, χ≈0.44) — they have formal veto authority but lack execution capacity. International human rights advocates see a temporary scaffold (Scaffold, d≈0.62, χ≈0.42) — conditionality is improvable, sunset is possible through arms control. The international humanitarian law apparatus sees a degraded ritual (Piton, d≈0.12, χ≈0.04) — certification theater with minimal functional verification. Recipient civilian populations see pure extraction (Snare, d≈0.92, χ≈0.73) — they bear costs, cannot exit, have no voice. The realpolitik analytical observer risks seeing an immutable natural law (Mountain) — states must transfer arms to maintain alliances — but the structural data reveals this as a false summit: the extraction and suppression components exist because alternatives (arms control, multilateral verification, technology sharing) are actively suppressed by beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   US defense industrial base: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; policy exists to solve their market access problem. Recipient governments: Victim + constrained → d≈0.55, f(d)≈0.75. Asymmetric relationship; constrained by US conditionality and leverage. Congressional bodies: Organized + constrained → d≈0.48, f(d)≈0.62. Formal authority but constrained by executive information advantage. Human rights advocates: Mobile + moderate → d≈0.62, f(d)≈0.92. Can exit advocacy platforms but face suppression of alternatives. International humanitarian law system: Institutional + arbitrage → d≈0.12, f(d)≈0.05. Piton classification from theater gate; low effective extraction. Recipient civilians: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — cannot exit, no voice, bear full cost. Analytical observer naturalizing as mountain: d≈0.72, f(d)≈1.15 — reveals false summit when checked against base properties showing suppression of alternative regimes.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The policy avoids mandatrophy collapse by legitimately containing a coordination function (alliance deterrence, burden-sharing) within an extraction mechanism (market access, leverage, technology control). The tangled rope classification is NOT a failure to distinguish coordination from extraction — it is accurate recognition that BOTH are present. The mandatrophy is resolved by measuring the suppression of alternative mechanisms: if recipient governments could freely choose suppliers (international market competition), if civilian populations could vote on transfers (democratic accountability), if multilateral verification could replace unilateral end-use monitoring (transparent enforcement), the policy would appear purely extractive (Snare). The constraint's tangled rope status depends on suppression of these alternatives. As suppression declines (arms control agreements, multilateral verification capacity, recipient diversification), the policy would reclassify toward Rope or Scaffold. The rising theater ratio (0.42→0.68) indicates the extraction mechanism is increasingly disguised as verification ritual rather than openly exercised as leverage — this is Goodhart drift, where certification theater substitutes for functional enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditionality_enforcement_efficacy,
    'Do US end-use monitoring conditions and human rights certifications actually constrain recipient behavior, or are they performative theater that enables transfers without real enforcement?',
    'Comparative analysis of certified vs uncertified transfers; longitudinal tracking of human rights violations by certified recipients; audit of monitoring compliance documentation',
    'If efficacious: constraint is legitimate tangled rope with real coordination function. If performative: constraint is snare disguised as rope; conditionality is extraction mechanism (certification covers political transfers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_enforcement_efficacy, empirical, 'Whether human rights conditions functionally constrain transfers').

omega_variable(
    alliance_deterrence_necessity,
    'Are specific arms transfers necessary for alliance security and deterrence, or could comparable security outcomes be achieved with arms control and joint defense without unilateral transfers?',
    'Counterfactual analysis: correlation between US transfers and military balance; game-theoretic modeling of deterrence with vs without unilateral access; historical cases of security maintenance without transfers',
    'If necessary: transfers are coordination (security public good). If contingent: transfers are rent-seeking extraction dressed in security language (beneficiary preference, not structural necessity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alliance_deterrence_necessity, conceptual, 'Whether transfers are structurally necessary for deterrence').

omega_variable(
    alternative_verification_regime_feasibility,
    'Could international weapons inspection, transparency registries, and multilateral arms control achieve equivalent or superior verification compared to unilateral US end-use monitoring?',
    'Analysis of existing multilateral verification regimes (IAEA, OPCW, ATT); comparison of detection rates and compliance; technical feasibility assessment of international inspection infrastructure',
    'If feasible: current regime is extraction mechanism with suppressed alternatives. If infeasible: piton classification (theater masks real coordination problem).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_verification_regime_feasibility, empirical, 'Feasibility of multilateral verification alternatives').

omega_variable(
    technology_transfer_containment_viability,
    'Does unilateral US control actually contain military technology diffusion to competitors, or do transfers to allies create secondary proliferation pathways that are equally effective at technology spread?',
    'Tracking of weapon system variants and licensed production; forensic analysis of technology flow through allied production; analysis of US technology availability from non-US sources',
    'If effective containment: beneficiary''s security interest (tech control) is real. If ineffective: transfer policy is pure extraction with false coordination justification (tech will diffuse anyway).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_containment_viability, empirical, 'Whether US unilateral control effectively contains technology diffusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_arms_transfer_policy, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usatp_tr_t0, us_arms_transfer_policy, theater_ratio, 0, 0.42).
narrative_ontology:measurement(usatp_tr_t25, us_arms_transfer_policy, theater_ratio, 25, 0.55).
narrative_ontology:measurement(usatp_tr_t50, us_arms_transfer_policy, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(usatp_be_t0, us_arms_transfer_policy, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(usatp_be_t25, us_arms_transfer_policy, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(usatp_be_t50, us_arms_transfer_policy, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_arms_transfer_policy, enforcement_mechanism).
narrative_ontology:affects_constraint(us_arms_transfer_policy, international_weapons_proliferation).
narrative_ontology:affects_constraint(us_arms_transfer_policy, regional_military_balance).
narrative_ontology:affects_constraint(us_arms_transfer_policy, human_rights_enforcement_regimes).

% DUAL FORMULATION NOTE:
% Arms transfer policy decomposes into separate structural constraints: (1) technology containment via unilateral control (ε≈0.35, Rope/Tangled Rope), (2) alliance deterrence via burden-sharing (ε≈0.25, Rope), (3) market extraction via conditionality (ε≈0.65, Snare/Tangled Rope). Current story treats the unified policy at ε=0.58; decomposition into technical and diplomatic components would refine classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_arms_transfer_policy, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
