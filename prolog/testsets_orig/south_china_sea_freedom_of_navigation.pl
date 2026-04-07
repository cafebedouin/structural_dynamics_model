% ============================================================================
% CONSTRAINT STORY: south_china_sea_freedom_of_navigation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_south_china_sea_freedom_of_navigation, []).

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
 *   constraint_id: south_china_sea_freedom_of_navigation
 *   human_readable: South China Sea Freedom of Navigation Constraint
 *   domain: geopolitical/maritime_law
 *
 * SUMMARY:
 *   The South China Sea freedom of navigation constraint represents a
 *   geopolitical intersection between international maritime law, great-power
 *   competition, and regional sovereignty claims. The core structural
 *   tension: China's Nine-Dash Line claim and militarization of artificial
 *   islands assert regional dominance and restrict navigational freedom,
 *   while extra-regional powers conduct freedom of navigation operations
 *   (FONOPs) to assert UNCLOS compliance and prevent monopolistic control.
 *   The constraint simultaneously coordinates (stabilizes regional power
 *   distribution, provides predictable escalation norms, clarifies
 *   sphere-of-influence boundaries) and extracts (raises shipping costs,
 *   restricts smaller states' sovereignty, creates military escalation risk).
 *   The extractiveness has increased over the measurement interval (0.35 →
 *   0.58) as Chinese military capabilities expanded and claims hardened.
 *   Theater ratio (0.58) reflects substantial performative content in
 *   diplomatic protocols, ASEAN Code of Conduct discussions, and UNCLOS
 *   adherence rhetoric, alongside genuine functional coordination through
 *   tacit deconfliction and incident-avoidance norms.
 *
 * KEY AGENTS:
 *   - China: Primary beneficiary (institutional/arbitrage) — establishes regional sphere of influence through military presence and administrative assertions; highest arbitrage options for unilateral escalation or de-escalation
 *   - Commercial Shipping Operators: Primary victims (powerless/trapped) — must navigate contested waters with no exit option; bear full cost of geopolitical tension through rerouting, insurance, interdiction risk
 *   - ASEAN Regional Powers (Vietnam, Philippines, Malaysia, Indonesia): Secondary victims constrained (moderate/constrained) — face military asymmetry but coordinate through coalition to maintain ambiguity preventing monopolistic control
 *   - Extra-Regional Powers (US, Japan, Europe): Tertiary actors (institutional/constrained) — strategically constrained by international law commitments but benefit from status quo preventing Chinese hegemony; conduct FONOPs to assert norms
 *   - International Legal Regime (UNCLOS, arbitration mechanisms): Institutional scaffold (organized/constrained) — provides coordination framework with sunset logic as norms mature
 *   - Smaller Claimant States (Philippines, Vietnam): Structural victims (moderate/constrained) — experience acute extraction from military asymmetry and sovereignty denial despite nominal claimant status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(south_china_sea_freedom_of_navigation, 0.58).
domain_priors:suppression_score(south_china_sea_freedom_of_navigation, 0.65).
domain_priors:theater_ratio(south_china_sea_freedom_of_navigation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(south_china_sea_freedom_of_navigation, extractiveness, 0.58).
narrative_ontology:constraint_metric(south_china_sea_freedom_of_navigation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(south_china_sea_freedom_of_navigation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(south_china_sea_freedom_of_navigation, tangled_rope).
narrative_ontology:human_readable(south_china_sea_freedom_of_navigation, "South China Sea Freedom of Navigation Constraint").
narrative_ontology:topic_domain(south_china_sea_freedom_of_navigation, "geopolitical/maritime_law").

domain_priors:requires_active_enforcement(south_china_sea_freedom_of_navigation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(south_china_sea_freedom_of_navigation, claimant_coastal_states).
narrative_ontology:constraint_beneficiary(south_china_sea_freedom_of_navigation, china_regional_dominance).
narrative_ontology:constraint_victim(south_china_sea_freedom_of_navigation, third_party_maritime_operators).
narrative_ontology:constraint_victim(south_china_sea_freedom_of_navigation, international_law_adherence).
narrative_ontology:constraint_victim(south_china_sea_freedom_of_navigation, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMERCIAL SHIPPING (SNARE) — Trapped in contested waters with no exit option. Must navigate through zones claimed by multiple states; face harassment, interdiction, rerouting costs, and insurance premiums. Bear full extraction cost of geopolitical tension with zero coordination benefit. Maximum suppression via military presence and administrative barriers.
constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ASEAN REGIONAL POWERS (TANGLED ROPE) — Constrained by geographic position and economic dependence on sea trade, but also coordinate through ASEAN mechanisms and benefit from maintaining ambiguity that prevents unilateral control. Both extracted from (limited navigation sovereignty, military escalation risk) and coordinators of regional stability (shared interest in avoiding conflict). Significant suppression but real agency through coalition formation.
constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CHINA INSTITUTIONAL FRAMEWORK (ROPE) — From China's strategic perspective, the constraint is coordination of regional dominance through 'Nine-Dash Line' claims and artificial island militarization. China experiences the constraint as solving a coordination problem: establishing unambiguous sphere of influence that clarifies regional power distribution. Net beneficiary with high arbitrage options (can escalate or de-escalate unilaterally). Low experienced extraction — the constraint subsidizes China's position.
constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EXTRA-REGIONAL POWERS (TANGLED ROPE) — Constrained by international law commitments to freedom of navigation and UNCLOS adherence, but also benefit from the status quo of contested claims that prevent any single power from monopolizing the region. Conduct freedom of navigation operations (FONOPs) that assert coordination around international norms while extracting from regional powers through deliberate incitement of escalation risk. Mixed extraction and coordination — both constrained and beneficiary simultaneously.
constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL REGIME (SCAFFOLD) — UNCLOS, arbitration mechanisms, and multilateral maritime codes represent coordination scaffolding designed to reduce extraction and establish rule-of-law baseline. Suppression is high (enforcement weak against powerful states) but sunset logic is built in: successful legal precedent (2016 arbitration tribunal) and norm diffusion suggest the scaffold is transitioning from external enforcement to internalized compliance. Classified as Scaffold because suppression declines as norms mature.
constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: SMALLER CLAIMANT STATES (SNARE) — Constrained by military asymmetry (cannot escalate against China), economic dependence on sea lanes, and domestic political pressure to assert sovereignty claims. Experience both coordination pressure (need regional stability) and extraction (sovereignty violation, resource denial, military intimidation). High suppression from military superiority creates effective snare despite nominal status as claimants.
constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: INTERNATIONAL NORMS THEATER (PITON) — The elaborate framework of UNCLOS, ASEAN Code of Conduct discussions, joint development zone proposals, and diplomatic protocols is substantially performative. These mechanisms persist despite minimal enforcement against great powers and without resolving underlying territorial claims. Theater persists through institutional inertia (alternatives haven't fully replaced it) and because maintaining the appearance of rules is less costly than naked power assertion. Piton classification reflects degraded norm function.
constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint coordinates multiple functions: it stabilizes regional power distribution, enables economic activity, enforces (imperfectly) international norms, and allows great-power competition within a framework that prevents direct military conflict. This is genuine coordination with asymmetric extraction layered on top. The analytical view captures both functions simultaneously — not a single-type system but a hybrid with measurable coordination benefit and measurable extraction.
constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(south_china_sea_freedom_of_navigation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(south_china_sea_freedom_of_navigation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(south_china_sea_freedom_of_navigation, TR),
    TR >= 0.70.

:- end_tests(south_china_sea_freedom_of_navigation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint imposes measurable costs on merchant shipping (rerouting, insurance, delays), creates sovereignty denial for smaller claimants, and generates military escalation risk for all regional actors. But extractiveness is not as high as pure monopoly would be (0.75+) because the constraint has not eliminated international navigation rights entirely — UNCLOS norms still govern, FONOPs still occur, alternative routes exist. The 16-year trajectory from 0.35 to 0.58 reflects cumulative effect of artificial island militarization, increased administrative harassment, and hardened territorial claims. Suppression (0.65): High. Military asymmetry prevents smaller states from resisting (trapped in military inferiority). International community cannot enforce compliance against great powers (trapped in power politics). Commercial shipping has limited choice (trapped in geographic necessity). But suppression is not total (0.90+) because escape routes exist, international legal frameworks persist, and ASEAN coalition maintains negotiating capacity. Theater ratio (0.58): Moderate-high. UNCLOS mechanisms, ASEAN Code of Conduct discussions, and diplomatic protocols involve substantial performative content (norms asserted but not enforced against great powers), but real functional coordination occurs through deconfliction mechanisms, incident-avoidance norms, and tacit sphere-of-influence acceptance.
 *
 * PERSPECTIVAL GAP:
 *   Maximum gap between China's rope (net beneficiary, coordination achieved, low suppression experienced) and commercial shipping's snare (victim, trapped, high suppression). ASEAN perspectives occupy middle ground showing real agency but constrained extraction. Extra-regional powers occupy paradoxical position as beneficiaries constrained — they benefit from current structure but cannot assert this benefit without violating legal norms they've internalized. International legal regime appears as performative (piton) from within the constraint but maintains scaffolding potential (sunset logic) from outside. Smaller claimants show acute snare-like dynamics despite nominal claimant status because military asymmetry overrides nominal sovereignty.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary-victim mapping is unambiguous: China benefits (regional dominance, sphere of influence establishment); merchants, smaller states, and international norm adherence bear costs. Directionality overrides not needed — the power/exit combination naturally produces the perspectival gap. China's arbitrage options and institutional power produce near-zero d; commercial shipping's trapped status and powerlessness produce maximum d; ASEAN's organized power and coalition capacity produce moderate d; extra-regional powers' institutional status but legal constraint produce moderate d. The f(d) sigmoid converts these to the observed chi values.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint avoids mandatrophy collapse because it is genuinely hybrid. The tangled rope classification at the analytical level is the correct meta-category — it captures that coordination (regional power distribution clarified, escalation norms established, predictable sphere of influence) and extraction (navigational restriction, sovereignty denial, military intimidation) are both structurally real. Snare classification from merchant shipping perspective and rope classification from China perspective are both correct at their observational positions; the tangled rope is the integrating view that shows why both are true simultaneously. The constraint is not mislabeled as pure coordination (rope) nor mislabeled as pure extraction (snare) — both functions are present and measurable. Mandatrophy is resolved by recognizing the multi-perspective structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intended_vs_actual_navigation_impact,
    'Do contested claims actually restrict freedom of navigation materially, or do they primarily impose reputational/legal costs with minimal operational impact?',
    'Quantitative analysis of shipping reroutes, speed changes, insurance premium escalation, and piracy/interdiction incidents correlated with contested zone proximity',
    'If material restriction: snare classification dominates from merchant shipping perspective. If primarily legal/reputational: tangled rope classification more accurate — coordination benefits (route predictability, escalation norms) may offset extraction costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intended_vs_actual_navigation_impact, empirical, 'Actual operational impact of contested claims on shipping').

omega_variable(
    china_constraint_versus_hegemonic_imposition,
    'Is the constraint a structural feature of the region''s geopolitics or a deliberate hegemonic strategy that could be reversed with Chinese policy change?',
    'Historical analysis of Chinese claims evolution; comparative study of Chinese behavior in other maritime zones; assessment of domestic political constraints on retreat from Nine-Dash Line',
    'If structural/irreversible: mountain or snare from most perspectives. If strategic/reversible: tangled rope and scaffold perspectives validated — constraint has sunset potential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_constraint_versus_hegemonic_imposition, conceptual, 'Whether constraint is structural feature or hegemonic choice').

omega_variable(
    coordination_benefit_quantification,
    'Does the constraint''s coordination function produce measurable benefit (reduced conflict, predictable routing, liability clarity) that exceeds extraction costs?',
    'Comparison of accident/incident rates, insurance costs, and shipping delays in SCS vs open ocean; measurement of rules-based behavior compliance; diplomatic incident frequency over time',
    'If benefits exceed costs: rope or tangled rope with higher χ validated. If extraction exceeds benefits: snare classification for merchant shipping and weaker states confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_benefit_quantification, empirical, 'Net benefit accounting for coordination vs extraction').

omega_variable(
    asean_coalition_sustainability,
    'Can ASEAN maintain coalition constraints on claimant behavior indefinitely, or will bilateral accommodation with China fragmentthe coalition?',
    'Tracking of ASEAN joint statements over time; bilateral vs multilateral trade and military cooperation patterns; domestic political alignment of ASEAN members with China vs US',
    'If coalition sustains: organized perspective''s tangled rope holds; moderate suppression and real agency. If fragmented: snare classification spreads to weaker states; suppression increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asean_coalition_sustainability, conceptual, 'Long-term sustainability of ASEAN coalition constraints').

omega_variable(
    legal_precedent_enforcement_gap,
    'Will the 2016 arbitration tribunal precedent and international legal norms eventually enforce compliance, or will great-power politics override legal mechanisms permanently?',
    'Tracking of Chinese compliance with tribunal findings; monitoring of third-party enforcement actions (sanctions, military response, diplomatic isolation); assessment of whether arbitration tribunal rulings generate binding obligations',
    'If legal precedent enforces: scaffold sunset logic validates — constraints transition from external enforcement to internalized compliance. If overridden: legal regime remains piton (performative) and underlying snare structure persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legal_precedent_enforcement_gap, preference, 'Whether international legal mechanisms can enforce against great powers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(south_china_sea_freedom_of_navigation, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scs_fon_tr_t0, south_china_sea_freedom_of_navigation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(scs_fon_tr_t8, south_china_sea_freedom_of_navigation, theater_ratio, 8, 0.52).
narrative_ontology:measurement(scs_fon_tr_t16, south_china_sea_freedom_of_navigation, theater_ratio, 16, 0.58).

% Extraction over time
narrative_ontology:measurement(scs_fon_be_t0, south_china_sea_freedom_of_navigation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(scs_fon_be_t8, south_china_sea_freedom_of_navigation, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(scs_fon_be_t16, south_china_sea_freedom_of_navigation, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(south_china_sea_freedom_of_navigation, resource_allocation).
narrative_ontology:affects_constraint(south_china_sea_freedom_of_navigation, semiconductor_supply_chain_dependency).
narrative_ontology:affects_constraint(south_china_sea_freedom_of_navigation, us_china_military_competition).
narrative_ontology:affects_constraint(south_china_sea_freedom_of_navigation, asean_coalition_dynamics).

% DUAL FORMULATION NOTE:
% The South China Sea freedom of navigation constraint is upstream of multiple downstream constraints: semiconductor supply depends on stable SCS shipping; US-China military competition centers on SCS escalation dynamics; ASEAN coalition formation is partially driven by SCS coordination needs. The high extractiveness (0.58) reflects accumulated effect of multiple reinforcing mechanisms (military presence, administrative barriers, legal ambiguity), while the theater ratio suggests room for normative intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(south_china_sea_freedom_of_navigation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
