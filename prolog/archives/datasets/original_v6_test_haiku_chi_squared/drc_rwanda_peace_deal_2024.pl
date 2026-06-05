% ============================================================================
% CONSTRAINT STORY: drc_rwanda_peace_deal_2024
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_drc_rwanda_peace_deal_2024, []).

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
 *   constraint_id: drc_rwanda_peace_deal_2024
 *   human_readable: US-Brokered DRC-Rwanda De-escalation Framework (2024)
 *   domain: geopolitical/regional_conflict
 *
 * SUMMARY:
 *   The 2024 US-brokered DRC-Rwanda de-escalation framework represents a
 *   geopolitical constraint that simultaneously functions as conflict
 *   coordination mechanism, extractive apparatus, and diplomatic theater. The
 *   United States positions the framework as a solution to regional
 *   instability driven by Rwandan support for M23 insurgency in eastern DRC,
 *   promising stabilization, civilian protection, and inclusive governance.
 *   Structurally, the framework creates asymmetric burdens: eastern DRC
 *   civilians face movement restrictions and displacement; armed groups face
 *   demobilization pressures; Rwanda gains recognition of security concerns
 *   and mineral trade formalization; the United States gains regional
 *   hegemonic positioning without proportional costs. The constraint exhibits
 *   classic Tangled Rope properties: it solves a coordination problem
 *   (de-escalation requires monitored military agreements and territorial
 *   frameworks) while simultaneously extracting compliance from those with
 *   fewest exit options (eastern DRC civilians, artisanal miners, powerless
 *   armed group members). Theater ratio has increased from 0.38 (genuine
 *   demobilization monitoring) to 0.64 (ritual diplomatic summits outweigh
 *   enforcement) over the twelve-month interval, indicating Goodhart
 *   drift—performative compliance replacing real de-escalation function.
 *
 * KEY AGENTS:
 *   - Eastern DRC Civilian Population: Primary victim (powerless/trapped) — faces movement restrictions, displacement, and armed group control with no exit option
 *   - United States Diplomatic Apparatus: Primary beneficiary (institutional/arbitrage) — gains regional hegemonic positioning and mineral supply chain influence without proportional enforcement burden
 *   - Rwanda Government and Military: Secondary beneficiary/victim hybrid (powerful/mobile) — gains legitimacy for mineral trade and security concerns but constrained by framework restrictions; maintains exit option via continued M23 support
 *   - DRC Government / FARDC: Secondary beneficiary (organized/constrained) — gains international military support and state consolidation narrative; constrained by monitoring and force requirements
 *   - M23 Insurgency and Armed Militias: Secondary victim/beneficiary (moderate/constrained) — face demobilization pressure but gain diplomatic recognition and negotiating legitimacy
 *   - Artisanal Mining Communities: Victim (powerless/trapped) — loses informal market access through conflict minerals certification and supply chain formalization
 *   - International Community (SADC, AU, UN): Performative monitor (institutional/arbitrage) — maintains ritual compliance role with minimal enforcement capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(drc_rwanda_peace_deal_2024, 0.52).
domain_priors:suppression_score(drc_rwanda_peace_deal_2024, 0.68).
domain_priors:theater_ratio(drc_rwanda_peace_deal_2024, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(drc_rwanda_peace_deal_2024, extractiveness, 0.52).
narrative_ontology:constraint_metric(drc_rwanda_peace_deal_2024, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(drc_rwanda_peace_deal_2024, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(drc_rwanda_peace_deal_2024, tangled_rope).
narrative_ontology:human_readable(drc_rwanda_peace_deal_2024, "US-Brokered DRC-Rwanda De-escalation Framework (2024)").
narrative_ontology:topic_domain(drc_rwanda_peace_deal_2024, "geopolitical/regional_conflict").

domain_priors:requires_active_enforcement(drc_rwanda_peace_deal_2024).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(drc_rwanda_peace_deal_2024, united_states_diplomatic_interests).
narrative_ontology:constraint_beneficiary(drc_rwanda_peace_deal_2024, rwanda_mineral_access).
narrative_ontology:constraint_beneficiary(drc_rwanda_peace_deal_2024, drc_conflict_reduction_apparatus).
narrative_ontology:constraint_victim(drc_rwanda_peace_deal_2024, eastern_drc_civilian_population).
narrative_ontology:constraint_victim(drc_rwanda_peace_deal_2024, armed_group_militias).
narrative_ontology:constraint_victim(drc_rwanda_peace_deal_2024, m23_insurgency).
narrative_ontology:constraint_victim(drc_rwanda_peace_deal_2024, artisanal_mining_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EASTERN DRC CIVILIANS (SNARE) — Trapped in conflict zone with no exit options. The de-escalation framework imposes movement restrictions, curfews, and displacement protocols with no meaningful civilian input. Cannot exit the territory; must comply with armed group and state enforcement. d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.57.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ARMED GROUP MILITIAS (TANGLED ROPE) — Constrained by framework's military demobilization requirements and weapons collection protocols, but benefit from diplomatic legitimacy and negotiating power. The framework coordinates territorial control agreements (coordination function) while extracting compliance through threat of sanctions (enforcement). d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.51.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DRC GOVERNMENT / FARDC (ROPE) — Benefits from framework as coordination mechanism for military operations, international support, and state consolidation narrative. Constrained by compliance monitoring and regional force requirements, but gains diplomatic legitimacy and foreign military aid. d≈0.45, f(d)≈0.50, σ=0.9 → χ≈0.23.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: UNITED STATES DIPLOMATIC INTERESTS (ROPE) — Primary beneficiary. Captures coordination function: stabilizing strategic region, preventing Chinese mineral monopoly, maintaining hegemonic positioning in African geopolitics. Can exit framework easily (arbitrage to other African priorities). d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.04. Net beneficiary from coordination without bearing costs.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RWANDA GOVERNMENT (TANGLED ROPE) — Powerful actor with dual relationship. Benefits from framework's recognition of security concerns (coordination) and mineral trade access (extraction via supply chain control). Can exit through continued M23 support or cross-border operations (mobile exit). d≈0.35, f(d)≈0.35, σ=0.9 → χ≈0.27. Asymmetric: framework restricts Rwandan cross-border operations while legitimizing mineral extraction pathways.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: INTERNATIONAL COMMUNITY (PITON) — SADC, AU, UN maintain performative compliance monitoring role with minimal enforcement capacity. Framework persists through institutional ritual (diplomatic summits, compliance reports) despite limited functional verification of demobilization or weapons collection. theater_ratio=0.64 indicates substantial performative activity. d≈0.15, f(d)≈0.08, σ=1.0 → χ≈0.03.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ARTISANAL MINING COMMUNITIES (SNARE) — Trapped between formal and informal mineral supply chains. Framework formalizes trade routes, reducing informal revenue; creates conflict minerals certification barriers that block artisanal miners from market access. Cannot exit (livelihoods depend on mining); cannot organize (informal sector status). d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.60.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SCAFFOLD) — Views framework as temporary coordination mechanism with uncertain sunset. De-escalation requires sustained enforcement (generational timeline), but success conditions are ambiguous. If enforced: coordination (Rope). If enforcement decays: reverts to Snare. theater_ratio suggests 36% genuine coordination (demobilization monitoring) and 64% performative ritual (diplomatic theater). Sunset contingent on: mineral supply stability, regional power balance shift, or hegemonic disinterest.
constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(drc_rwanda_peace_deal_2024_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(drc_rwanda_peace_deal_2024, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(drc_rwanda_peace_deal_2024, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(drc_rwanda_peace_deal_2024, TR),
    TR >= 0.70.

:- end_tests(drc_rwanda_peace_deal_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The framework extracts compliance from eastern DRC civilians (movement restrictions, curfews, displacement protocols) and artisanal miners (market access barriers) without meaningful compensation or input. Extraction is not total because Rwanda and M23 also face real demobilization constraints, and the DRC government gains military support. However, the asymmetry is substantial: powerless actors bear costs without benefits. Suppression (0.68): High. Movement restrictions, armed group control, lack of alternative livelihood pathways, and limited civilian participation in negotiation structures all constrain alternatives. Civilian grievance mechanisms are absent; exit from the conflict zone is militarily dangerous. Theater ratio (0.64): Moderate-high and rising. Initial framework (T=0) included genuine demobilization monitoring and weapons collection targets. By T=12 months, international summits and compliance reports have become increasingly performative—verification mechanisms lack capacity, enforcement is selective, and diplomatic theater (press statements, summit declarations) substitutes for ground-level verification. The rise from 0.38 to 0.64 indicates Goodhart drift: organizational activity (diplomatic events) replacing functional outcomes (actual demobilization).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same framework produces radically different classifications depending on observer position. The United States sees coordination (Rope)—they frame the agreement as solving the regional conflict problem through diplomatic mechanisms. Rwanda sees mixed benefits and constraints (Tangled Rope)—they gain security legitimacy and mineral trade formalization while constrained by cross-border operation restrictions. The DRC government sees coordination with support (Rope)—military aid, international backing, and state consolidation. But eastern DRC civilians see extraction and control (Snare)—they are subjected to movement restrictions, displacement, and armed group enforcement with no exit and no voice. Artisanal miners see supply chain extraction (Snare)—formalization requirements block informal market access. Armed militias see constrained negotiation (Tangled Rope)—demobilization pressure coupled with diplomatic legitimacy. The international community sees ritual compliance (Piton)—summits, reports, monitoring frameworks persist despite limited enforcement capacity. The perspectival gap reveals that the framework's 'de-escalation' framing naturalizes what is structurally an extraction mechanism for powerless actors, legitimized through coordination language.
 *
 * DIRECTIONALITY LOGIC:
 *   Eastern DRC civilians: Victim + trapped → d≈0.92, f(d)≈1.40, σ=0.8 → high extraction. No exit options; all movement constraints imposed externally. Artisanal miners: Victim + trapped → d≈0.95, f(d)≈1.42, σ=0.8 → maximum extraction. Supply chain formalization blocks informal access; no alternative livelihoods; cannot organize. United States: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08, σ=1.2 → net beneficiary. Gains geopolitical positioning; can exit to other African priorities; no enforcement cost. Rwanda: Beneficiary + mobile → d≈0.35, f(d)≈0.35, σ=0.9 → moderate extraction despite powerful status. Gains legitimacy and mineral trade access; can exit via continued M23 support; constrained by monitoring. DRC Government: Mixed beneficiary + constrained → d≈0.45, f(d)≈0.50, σ=0.9 → low-moderate extraction. Gains military support; constrained by force requirements and compliance monitoring; cannot easily exit (depends on US support). M23/Armed groups: Victim + constrained → d≈0.68, f(d)≈1.05, σ=0.9 → moderate-high extraction. Face demobilization pressure; gain diplomatic legitimacy; constrained exit (reintegration vs alternative operations). International community: Observer/monitor + arbitrage → d≈0.15, f(d)≈0.08, σ=1.0 → minimal extraction; institutional theater substitutes for enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY AT ε=0.52: The constraint exhibits genuine Tangled Rope properties (beneficiaries + victims + asymmetric enforcement + coordination function), but mandatrophy remains unresolved because the boundary between coordination and extraction is unstable. PRIMARY MANDATROPHY: Is this framework's primary function de-escalation and conflict resolution (Rope/Scaffold), or geopolitical mineral supply chain stabilization (extraction/Snare)? The rising theater_ratio (0.38→0.64) suggests degradation from genuine coordination toward performative cover for extraction. SECONDARY MANDATROPHY: Do eastern DRC civilians benefit from de-escalation (coordination) despite movement restrictions, or do they experience the restrictions as primary constraint (extraction)? The framework's civilian protection rhetoric conflicts with ground-level enforcement patterns. RESOLUTION CRITERIA: Mandatrophy resolves if (a) enforcement mechanisms demonstrate sustained credibility over 24+ months, showing genuine demobilization and civilian protection gains, shifting classification toward Scaffold/Rope, OR (b) theater_ratio stabilizes and enforcement decays, revealing framework as performative Piton masking underlying Snare dynamics. Current status (12 months, ε=0.52, theater=0.64): Framework has crossed the Goodhart threshold—ritual activity now equals or exceeds functional outcomes—but enforcement mechanisms remain sufficiently visible to prevent classification collapse. Without intervention, trajectory points toward Piton (degraded theater) or Snare (extraction mask). Declared as ACTIVE UNRESOLVED because mandate ambiguity persists and the framework's sunset conditions are undefined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_credibility,
    'Will the United States and international community sustain enforcement pressure if Rwanda or M23 violate the framework?',
    'Behavioral tracking: sanctions enforcement, military presence continuation, financial incentive alignment over 2-5 year horizon',
    'If sustained enforcement: Tangled Rope classification persists (extraction with coordination). If enforcement decays: collapses to Snare or pure Piton (theater without function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_credibility, preference, 'Whether enforcement mechanisms have credible backing').

omega_variable(
    mineral_extraction_pathway_legitimacy,
    'Does the framework legitimize existing mineral extraction patterns (Rwanda''s supply chain control) or genuinely reshape supply governance toward inclusive formalization?',
    'Supply chain analysis: tracking of formal vs informal mineral exports; cost/access changes for artisanal miners; labor standards in formalized chains',
    'If legitimizes existing patterns: extraction mechanism (Snare victims expand to include artisanal miners). If reshapes toward inclusion: coordination mechanism (Rope classification strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mineral_extraction_pathway_legitimacy, empirical, 'Whether mineral supply chain is genuinely reformed or extraction legitimized').

omega_variable(
    m23_reintegration_sincerity,
    'Is M23 demobilization and reintegration into FARDC genuine power-sharing coordination, or a cover for Rwandan-backed militia consolidation?',
    'Institutional analysis: FARDC command structure changes, M23 officer placement, operational independence; cross-border troop movements; mineral revenue allocation',
    'If genuine: classification shifts toward Rope/Scaffold (coordination with real sunset). If cover for consolidation: Snare classification confirmed (armed group victims suffer extraction masked as de-escalation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m23_reintegration_sincerity, empirical, 'Whether M23 reintegration represents genuine power-sharing').

omega_variable(
    us_geopolitical_exit_scenario,
    'Under what conditions would the United States withdraw diplomatic and financial enforcement pressure?',
    'Geopolitical modeling: US Africa strategy shifts, China-Africa relations evolution, competing regional interventions, cost-benefit analysis of continued engagement',
    'If exit conditions unclear: framework remains unstable (high mandatrophy). If exit conditions identified: scaffold sunset becomes concrete (transition to Piton or Snare upon exit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(us_geopolitical_exit_scenario, preference, 'Conditions for US disengagement and framework abandonment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(drc_rwanda_peace_deal_2024, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(drc_rwa_tr_t0, drc_rwanda_peace_deal_2024, theater_ratio, 0, 0.38).
narrative_ontology:measurement(drc_rwa_tr_t6, drc_rwanda_peace_deal_2024, theater_ratio, 6, 0.51).
narrative_ontology:measurement(drc_rwa_tr_t12, drc_rwanda_peace_deal_2024, theater_ratio, 12, 0.64).

% Extraction over time
narrative_ontology:measurement(drc_rwa_be_t0, drc_rwanda_peace_deal_2024, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(drc_rwa_be_t6, drc_rwanda_peace_deal_2024, base_extractiveness, 6, 0.43).
narrative_ontology:measurement(drc_rwa_be_t12, drc_rwanda_peace_deal_2024, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(drc_rwanda_peace_deal_2024, enforcement_mechanism).
narrative_ontology:affects_constraint(drc_rwanda_peace_deal_2024, rwandan_m23_proxy_control).
narrative_ontology:affects_constraint(drc_rwanda_peace_deal_2024, drc_mineral_supply_chain).
narrative_ontology:affects_constraint(drc_rwanda_peace_deal_2024, east_african_regional_stability).
narrative_ontology:affects_constraint(drc_rwanda_peace_deal_2024, conflict_minerals_certification).

% DUAL FORMULATION NOTE:
% The DRC-Rwanda peace deal is downstream of existing minerals competition (drc_mineral_supply_chain) and Rwandan strategic interests (rwandan_m23_proxy_control), but represents a distinct constraint with its own ε reflecting de-escalation framework's asymmetric burden distribution. The framework functionally integrates mineral supply governance, requiring analysis of downstream constraints (conflict_minerals_certification) to capture full extraction pathway.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(drc_rwanda_peace_deal_2024, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
