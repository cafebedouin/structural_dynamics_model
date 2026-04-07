% ============================================================================
% CONSTRAINT STORY: preah_vihear_territorial_claim
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preah_vihear_territorial_claim, []).

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
 *   constraint_id: preah_vihear_territorial_claim
 *   human_readable: Preah Vihear Territorial Claim and Border Dispute Resolution
 *   domain: geopolitical/territorial_sovereignty
 *
 * SUMMARY:
 *   The Preah Vihear territorial dispute between Thailand and Cambodia
 *   represents a structurally mixed constraint that exhibits properties of
 *   both coordination (international law precedent setting, border dispute
 *   resolution) and extraction (nationalist mobilization, military resource
 *   capture, population displacement). The constraint originated as a
 *   coordination problem (colonial boundary demarcation ambiguity requiring
 *   clarification) but has evolved into an extraction mechanism where
 *   nationalist factions in both states weaponize the unresolved claim for
 *   domestic political consolidation and regional power positioning. The ICJ
 *   ruling in 2008 provided legal clarity but failed to resolve the
 *   underlying extraction dynamics — military escalations continued in
 *   2008-2011, and the constraint persists despite formal adjudication. This
 *   pattern indicates that the legal victory satisfied the coordination
 *   function (international law precedent) but did not eliminate the
 *   extraction function (nationalist coalition resource capture). The theater
 *   ratio (0.68) reflects that dispute resolution institutions (ICJ, ASEAN
 *   mediation, bilateral negotiations) conduct extensive performative
 *   activity (ceremonial rulings, negotiation theater) while underlying
 *   conflict prevention capacity remains low — escalations occur despite
 *   institutional machinery, suggesting the machinery is maintenance ritual
 *   rather than functional prevention.
 *
 * KEY AGENTS:
 *   - Border Populations: Primary victim (powerless/trapped) — face displacement, restricted movement, violence exposure with minimal exit options; bear extraction costs without benefit
 *   - Thailand Nationalist Military Coalition: Organized extractor (organized/constrained) — weaponizes territorial claim for domestic mobilization, military budget justification, identity cohesion; sustains suppression of negotiation pathways
 *   - Cambodian Nationalist Coalition: Powerful beneficiary (powerful/mobile) — benefits from ICJ validation and restored national dignity; also entrapped in escalation dynamics and nationalist mobilization trap
 *   - Regional Stability Mechanisms: Institutional victim (institutional/constrained) — benefit from dispute resolution frameworks (coordination) but are exploited by nationalist rhetoric and escalation cycles
 *   - International Law Precedent System: Institutional beneficiary (institutional/arbitrage) — benefits from clear ICJ territorial adjudication setting precedent; exits through precedent acceptance
 *   - International Dispute Resolution Theater: Institutional actor (institutional/arbitrage) — maintains performative mediation, ceremonial negotiation, institutional legitimacy theater with degraded actual prevention capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preah_vihear_territorial_claim, 0.58).
domain_priors:suppression_score(preah_vihear_territorial_claim, 0.72).
domain_priors:theater_ratio(preah_vihear_territorial_claim, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preah_vihear_territorial_claim, extractiveness, 0.58).
narrative_ontology:constraint_metric(preah_vihear_territorial_claim, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(preah_vihear_territorial_claim, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preah_vihear_territorial_claim, tangled_rope).
narrative_ontology:human_readable(preah_vihear_territorial_claim, "Preah Vihear Territorial Claim and Border Dispute Resolution").
narrative_ontology:topic_domain(preah_vihear_territorial_claim, "geopolitical/territorial_sovereignty").

domain_priors:requires_active_enforcement(preah_vihear_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preah_vihear_territorial_claim, cambodia_nationalist_coalition).
narrative_ontology:constraint_beneficiary(preah_vihear_territorial_claim, international_law_precedent_supporters).
narrative_ontology:constraint_victim(preah_vihear_territorial_claim, thailand_security_interests).
narrative_ontology:constraint_victim(preah_vihear_territorial_claim, regional_stability_mechanism).
narrative_ontology:constraint_victim(preah_vihear_territorial_claim, border_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BORDER POPULATIONS (SNARE) — Residents of contested zones face intermittent conflict, restricted movement, and forced displacement with minimal exit options. The territorial claim creates physical barriers and legal uncertainty that trap these populations. They bear extraction (security costs, livelihood disruption) while benefiting minimally from the dispute's resolution.
constraint_indexing:constraint_classification(preah_vihear_territorial_claim, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL STABILITY MECHANISM (TANGLED ROPE) — ASEAN and regional institutions benefit from border dispute resolution frameworks (coordination function) while simultaneously being exploited by nationalist rhetoric and military posturing that undermines their authority. The constraint coordinates through international law precedent while extracting through escalation cycles and institutional capture.
constraint_indexing:constraint_classification(preah_vihear_territorial_claim, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERNATIONAL LAW SUPPORTERS (ROPE) — ICJ rulings and international law jurisprudence benefit from clear territorial adjudication. This perspective sees the constraint as pure coordination: clarifying precedent enables predictable dispute resolution globally. Benefits accrue through institutional/academic channels with exit via precedent acceptance.
constraint_indexing:constraint_classification(preah_vihear_territorial_claim, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THAILAND NATIONALIST MILITARY (SNARE) — Organized military and nationalist factions weaponize the territorial claim for domestic political consolidation and regional power positioning. They experience the constraint as extraction opportunity (mobilization resource, budget justification, identity cohesion) rather than cost. High suppression of negotiation pathways. Classified as Snare (not beneficiary) because the organized coalition sustains the extraction mechanism through active enforcement against peace initiatives.
constraint_indexing:constraint_classification(preah_vihear_territorial_claim, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: CAMBODIAN NATIONALIST COALITION (TANGLED ROPE) — Powerful nationalist actors (government, civil society, military factions) benefit from ICJ validation and restored national dignity while also using the claim as a mobilization tool that entraps Cambodia in escalation dynamics and diverts resources from development. The constraint provides both coordination (legitimacy through law) and extraction (national resources committed to military positioning).
constraint_indexing:constraint_classification(preah_vihear_territorial_claim, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL DISPUTE RESOLUTION THEATER (PITON) — The ICJ process, bilateral negotiations, and UN mechanisms maintain high performative content while actual conflict prevention capacity has degraded. The institutional machinery persists through inertia (precedent value, legitimacy theater) despite oscillating between ceremonial peak and military flare-up. Theater ratio reflects that dispute resolution institutions stage conflict de-escalation while underlying nationalist drives remain unaddressed.
constraint_indexing:constraint_classification(preah_vihear_territorial_claim, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preah_vihear_territorial_claim_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(preah_vihear_territorial_claim, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(preah_vihear_territorial_claim, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(preah_vihear_territorial_claim, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(preah_vihear_territorial_claim, TR),
    TR >= 0.70.

:- end_tests(preah_vihear_territorial_claim_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts substantial resources from border populations and regional stability institutions through escalation cycles, military positioning, and resource diversion. However, the extraction is not maximal (0.70+) because the ICJ ruling created partial resolution — legal title was clarified, reducing some uncertainty. The extractiveness has increased over the measurement interval (0.35 → 0.58) as nationalist mobilization has intensified despite legal resolution, indicating that the extraction mechanism is self-reinforcing. Suppression (0.72): High. Nationalist rhetoric, military threats, and state control of media create substantial barriers to negotiation and cross-border cooperation. Border populations face legal and physical barriers to exit. However, suppression is not absolute (0.90+) because some diplomatic channels remain open and international pressure persists. Theater ratio (0.68): Moderate-high. International dispute resolution institutions conduct extensive ceremonial activity (ICJ proceedings, bilateral negotiations, ASEAN summits) with degraded actual conflict prevention. The ratio has increased over time (0.55 → 0.68) as institutions have accumulated more performative activity without corresponding prevention capacity improvements. The theater is not extreme (0.90+) because underlying nationalist drives remain real — institutions are not pure theater, but their functional component has atrophied relative to their ceremonial component.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival disagreement. The Cambodian nationalist coalition sees Rope — international law coordination that validates their sovereignty claim. The international law system sees pure Rope — clear territorial precedent. The regional stability mechanisms see Tangled Rope — they coordinate through dispute resolution while being exploited by nationalist escalation. The Thai nationalist military sees Rope (from their extraction benefit perspective) or Snare (from a Thai civilian perspective constrained by military dominance). Border populations see Snare — pure extraction with no coordination benefit. The international dispute resolution system sees Piton — its own processes have become performative ritual. The gap reveals that classification depends almost entirely on which agent's structural position you occupy: beneficiaries (Cambodia nationalist, international law) see coordination; victims (border populations, regional institutions) see extraction; institutional actors (dispute resolution theater) see degraded ritual.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: (1) Cambodian nationalist coalition receives benefits from ICJ validation and regional status enhancement (low d via beneficiary status), but also faces military constraints due to organized Thai opposition, moderating their arbitrage options toward constrained (higher d). (2) Thai nationalist military faces constraints from international pressure and resource limitations on indefinite military positioning, but benefits from the unresolved status which justifies ongoing military spending and nationalist mobilization (mixed d around 0.50). (3) Border populations have no benefits and face trapped status (high d approaching 1.0). (4) Regional institutions should benefit from successful dispute resolution (low d) but are instead being exploited by nationalist pressures (d rises toward 0.60). (5) International law system benefits cleanly from precedent (very low d, around 0.05). The d values reflect that extractiveness flows from nationalist actors toward border populations and regional stability, with international law beneficiaries gaining clean coordination value.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is UNRESOLVED. The constraint initially was legitimately Rope (dispute resolution coordination) but has evolved into Tangled Rope (mixed coordination and extraction) without fully transitioning to Snare (pure extraction without coordination). The ICJ ruling satisfied the coordination function (legal precedent) but failed to eliminate nationalist extraction. The question 'is this coordination or extraction?' depends on the timescale: at immediate timeframe (the ICJ decision), Rope classification holds. At biographical timeframe (ongoing military positioning and nationalist mobilization), Tangled Rope classification holds. At generational timeframe (accumulating theater ratio, unresolved underlying nationalist drives), degradation toward Piton becomes evident. The mandatrophy persists because the constraint contains genuine coordination (dispute resolution infrastructure has real function) AND genuine extraction (nationalist resource capture is structurally embedded). Neither pure type captures the mechanism. The analysis resolves mandatrophy by showing that Tangled Rope is the correct classification: beneficiaries exist (Cambodian validation, international law precedent), victims exist (border populations, regional stability), active enforcement is required (military positioning, nationalist mobilization), and both coordination and extraction functions are structurally present and active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nationalist_coalition_critical_mass,
    'Do organized nationalist factions in either state exceed critical mass thresholds that enable coalition military action independent of state institutional control?',
    'Analysis of military command structure, state capacity for civilian control, prior instances of military override of political negotiation; comparison to other states with nationalist military capture',
    'If yes: both states transition from state-as-extractor to organized-coalition-as-extractor, changing exit options from arbitrage to constrained/trapped, raising chi for powerless agents. If no: state institutional actors retain exit capacity and can arbitrage toward negotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nationalist_coalition_critical_mass, empirical, 'Whether nationalist military factions exceed critical mass for independent action').

omega_variable(
    icj_ruling_credibility_vs_enforcement,
    'Does ICJ territorial adjudication derive its extractive power from perceived legitimacy (international law precedent) or from enforcement mechanisms (military capacity to defend territory)?',
    'Historical comparison: ICJ rulings that were accepted without military enforcement (coordination function) vs rulings that required military deployment or escalation threat. Attribution analysis of compliance drivers.',
    'If legitimacy-driven: the constraint is substantively Rope (international law as coordination standard). If enforcement-driven: the constraint remains Tangled Rope or Snare (ICJ provides legal cover for military extraction). Classification depends entirely on which mechanism is primary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(icj_ruling_credibility_vs_enforcement, conceptual, 'Whether ICJ ruling power derives from legitimacy or enforcement capacity').

omega_variable(
    border_population_agency_and_exit,
    'Do border populations possess genuine constrained exit (high-cost relocation, economic disruption, family separation) or are they truly trapped (legal barriers, physical barriers, no viable alternatives)?',
    'Empirical study of migration patterns, relocation costs, legal status of cross-border movement, employment opportunities outside contested zone. Distinction between structural trapping vs high-cost constraint.',
    'If constrained: border population perspective shifts to Tangled Rope (some extraction, some agency). If trapped: snare classification holds (no exit). Exit classification directly drives d value and experienced extractiveness (chi).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(border_population_agency_and_exit, empirical, 'Whether border populations are trapped or constrained').

omega_variable(
    regional_institution_capture_depth,
    'Have ASEAN and regional dispute resolution mechanisms been substantially captured by nationalist state actors, or do they retain sufficient autonomy to function as genuine coordination infrastructure?',
    'Process tracing of regional institution decisions: comparison of negotiation outcomes when nationalist pressures are low vs high; analysis of institutional autonomy during prior regional disputes; staff independence metrics.',
    'If captured: regional mechanisms transition from beneficiary (Rope perspective) to victim (become part of the tangled extraction). If autonomous: regional institutions retain Rope classification and can mediate toward coordination outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_institution_capture_depth, empirical, 'Depth of regional institution capture by nationalist state actors').

omega_variable(
    oscillation_driver_exogenous_vs_endogenous,
    'Do escalation-de-escalation cycles reflect external shocks (global politics, economic cycles) or are they driven by the inherent extraction logic of the territorial claim mechanism itself (military budget justification, periodic nationalist mobilization)?',
    'Time-series analysis of conflict intensity correlated with external events vs internal political cycles; comparison to other territorial disputes with different underlying mechanisms; counterfactual analysis of what de-escalation would require.',
    'If exogenous: constraint may be amenable to structural change through external interventions. If endogenous: the extraction mechanism is self-sustaining and requires addressing nationalist coalition incentives directly. This drives forecast of constraint persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oscillation_driver_exogenous_vs_endogenous, empirical, 'Whether oscillations are driven by external shocks or internal extraction logic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preah_vihear_territorial_claim, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pvh_tr_t0, preah_vihear_territorial_claim, theater_ratio, 0, 0.55).
narrative_ontology:measurement(pvh_tr_t10, preah_vihear_territorial_claim, theater_ratio, 10, 0.65).
narrative_ontology:measurement(pvh_tr_t20, preah_vihear_territorial_claim, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(pvh_be_t0, preah_vihear_territorial_claim, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pvh_be_t10, preah_vihear_territorial_claim, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(pvh_be_t20, preah_vihear_territorial_claim, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preah_vihear_territorial_claim, enforcement_mechanism).
narrative_ontology:affects_constraint(preah_vihear_territorial_claim, asean_institutional_capture).
narrative_ontology:affects_constraint(preah_vihear_territorial_claim, regional_military_spending_spiral).

% DUAL FORMULATION NOTE:
% Preah Vihear territorial claim is upstream of regional military dynamics and ASEAN institutional capture. The extraction mechanism (nationalist mobilization, military resource justification) flows downstream to affect military spending competition and institutional autonomy loss in neighboring disputes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preah_vihear_territorial_claim, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
