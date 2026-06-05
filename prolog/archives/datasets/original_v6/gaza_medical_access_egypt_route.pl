% ============================================================================
% CONSTRAINT STORY: gaza_medical_access_egypt_route
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gaza_medical_access_egypt_route, []).

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
 *   constraint_id: gaza_medical_access_egypt_route
 *   human_readable: Gaza Medical Access via Egypt Rafah Route
 *   domain: humanitarian/geopolitical
 *
 * SUMMARY:
 *   Medical access from Gaza to Egypt via the Rafah crossing represents a
 *   structural constraint where territorial closure and gatekeeping by
 *   multiple state actors creates a pure extraction mechanism targeting
 *   patients requiring external medical care. The constraint exhibits the
 *   classic snare architecture: the victim (Gaza patient) has no exit
 *   options; the gatekeepers (Egyptian state, Israeli security establishment)
 *   control a monopoly resource and extract compliance through survival
 *   threat. The constraint is reinforced by theater: humanitarian protocols,
 *   coordination meetings, and international negotiation create the
 *   appearance of functional medical referral systems while systematically
 *   deprioritizing and delaying cases. The theater_ratio (0.55) reflects that
 *   significant time is spent on administrative vetting, security screening,
 *   and political signaling rather than on actual medical triage and case
 *   prioritization. The extractiveness has accumulated over 15 years from
 *   0.42 (initial post-2007 closure) to 0.68 (current state), indicating
 *   increasing sophistication of the extraction mechanism and tightening of
 *   the gate. The suppression (0.78) is correspondingly high: patients face
 *   territorial confinement, no alternative routes, economic barriers to
 *   private passage, political risk in applying, and medical urgency that
 *   eliminates negotiating power.
 *
 * KEY AGENTS:
 *   - Gaza Patient: Primary victim (powerless/trapped) — no exit options; life-or-death dependence on gatekeepers; zero negotiating power
 *   - Palestinian Health Authority: Secondary victim/manager (moderate/constrained) — constrained by gatekeeper dependence but retains partial control over triage; mixed extraction and coordination role
 *   - Egyptian State: Primary beneficiary (institutional/arbitrage) — controls sole passage; experiences as coordination mechanism (border management, security vetting); maintains arbitrage options to modulate flow
 *   - Israeli Security Establishment: Institutional beneficiary (powerful/mobile) — controls northern gate; experiences as security coordination with asymmetric extraction function; politically constrained but structurally powerful
 *   - International Humanitarian System: Organized mediator (organized/constrained) — perceives as temporary scaffold with negotiable protocols; has agency through funding and pressure but constrained by state sovereignty
 *   - Gaza Civil Society / Patient Advocacy: Organized resistance (organized/constrained) — emerging countervailing power through documentation and international campaigns; transitioning from powerless to organized but still structurally trapped
 *   - Oslo Accords Framework: Institutional legacy (institutional/arbitrage) — provides legal framework for two-gate system; maintained through diplomatic theater despite functional degradation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gaza_medical_access_egypt_route, 0.68).
domain_priors:suppression_score(gaza_medical_access_egypt_route, 0.78).
domain_priors:theater_ratio(gaza_medical_access_egypt_route, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gaza_medical_access_egypt_route, extractiveness, 0.68).
narrative_ontology:constraint_metric(gaza_medical_access_egypt_route, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gaza_medical_access_egypt_route, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gaza_medical_access_egypt_route, snare).
narrative_ontology:human_readable(gaza_medical_access_egypt_route, "Gaza Medical Access via Egypt Rafah Route").
narrative_ontology:topic_domain(gaza_medical_access_egypt_route, "humanitarian/geopolitical").

domain_priors:requires_active_enforcement(gaza_medical_access_egypt_route).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gaza_medical_access_egypt_route, egyptian_state_apparatus).
narrative_ontology:constraint_beneficiary(gaza_medical_access_egypt_route, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(gaza_medical_access_egypt_route, international_intermediaries).
narrative_ontology:constraint_victim(gaza_medical_access_egypt_route, gazans_requiring_medical_care).
narrative_ontology:constraint_victim(gaza_medical_access_egypt_route, palestinian_patient_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GAZA PATIENT (SNARE) — Trapped by medical emergency and territorial closure. No alternative route exists; Egypt controls the sole passage. Life-or-death dependence creates maximum suppression. The patient's exit options are zero — they cannot walk around the constraint or substitute an alternative. Pure extraction: gatekeepers extract compliance through survival threat.
constraint_indexing:constraint_classification(gaza_medical_access_egypt_route, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PALESTINIAN HEALTH AUTHORITY (TANGLED ROPE) — Constrained by resource scarcity and political subordination; also benefits from retained partial control over triage and case selection. Mixed coordination (directing patients, organizing medical capacity) and extraction (dependence on gatekeeper approval). Significant exit costs but not absolute — can organize internal emergency care, reducing but not eliminating dependency.
constraint_indexing:constraint_classification(gaza_medical_access_egypt_route, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EGYPTIAN STATE (ROPE) — Controls the Rafah crossing; experiences the constraint as coordination mechanism for border management, refugee control, and security screening. Maintains arbitrage options: can modulate flow based on political signals, extract concessions from donor states, or redirect patients to other crossings. Net beneficiary with genuine exit optionality — the constraint serves coordination (who crosses, when, with what vetting) alongside extraction.
constraint_indexing:constraint_classification(gaza_medical_access_egypt_route, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTERNATIONAL HUMANITARIAN SYSTEM (SCAFFOLD) — UN agencies, ICRC, NGOs perceive the Rafah route as a temporary coordination framework with negotiable terms. Organized enough to negotiate protocols, secure funding, and pressure for opening. Sees a sunset: permanent resolution requires territorial access or alternative medical infrastructure. High suppression tolerated because the coalition has agency and an exit pathway (political settlement opening alternative routes or internal capacity). Effective extraction dampened by coalition organization.
constraint_indexing:constraint_classification(gaza_medical_access_egypt_route, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ISRAELI SECURITY STATE (TANGLED ROPE) — Controls northern access to Gaza; experiences the constraint as security coordination (vetting of medical cases against intelligence) with asymmetric extraction (Israeli institutions benefit from disruption of Palestinian health capacity). Mixed genuine coordination function (preventing weapon smuggling via medical channels) and asymmetric extraction (Palestinian medical systems degraded relative to Israeli). Mobile exit options but politically constrained — cannot publicly abandon medical screening without security cost.
constraint_indexing:constraint_classification(gaza_medical_access_egypt_route, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: GAZA CIVIL SOCIETY (SNARE → ROPE transition) — Organized but structurally constrained by the two-gate system (Israel + Egypt). Cannot exit the constraint but can organize resistance, documentation, and international pressure. Classification sits between snare and rope: trapped by gates, but organized enough to extract limited concessions. The coalition's emergence and measurement of mortality delays creates countervailing power, moving the classification toward rope over time.
constraint_indexing:constraint_classification(gaza_medical_access_egypt_route, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: OSLO ACCORDS FRAMEWORK (PITON) — The Rafah crossing and two-gate system were institutionalized under Oslo Accords structures. The crossing persists through institutional inertia and legal precedent, not because it functions well. Theater ratio high (endless negotiations over opening protocols, humanitarian pauses, NGO access) despite low functional medical benefit. The framework is degraded — it was designed for a transitional arrangement that never transitioned. Maintained through diplomatic theater and legal citation rather than because the structure solves the coordination problem.
constraint_indexing:constraint_classification(gaza_medical_access_egypt_route, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SNARE) — From a civilizational global scope, the Rafah bottleneck is a structural extraction mechanism embedded in the territorial partition regime. Medical access is weaponized as a compliance lever. The constraint exists to extract compliance (patient acceptance of political subordination) and degrade adversary capacity (Palestinian institutional resilience). This perspective classifies consistently as snare across all metrics.
constraint_indexing:constraint_classification(gaza_medical_access_egypt_route, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gaza_medical_access_egypt_route_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gaza_medical_access_egypt_route, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gaza_medical_access_egypt_route, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gaza_medical_access_egypt_route, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gaza_medical_access_egypt_route, TR),
    TR >= 0.70.

:- end_tests(gaza_medical_access_egypt_route_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and accumulating. The gate controls access to life-saving medical care through monopoly power. Initial extractiveness (0.42) reflected a functional humanitarian system with some throughput; current extractiveness (0.68) reflects systematic gating where approval rates depend on political signaling rather than medical urgency. The increase reflects institutional learning — gatekeepers have refined extraction mechanisms through experience. Suppression (0.78): Very high. Patients face absolute territorial confinement (no land borders except Egypt), economic barriers (transport, accommodation costs), political risk (security vetting used as political leverage), and medical urgency eliminating negotiating power. Alternative exit options (air travel, private medical tourism) are available only to wealthy Palestinians, creating class stratification of medical access. Theater ratio (0.55): Moderate-high. Significant portion of the constraint's operation is performative: humanitarian coordination meetings, protocols, and exceptions create appearance of functional systems while maintaining extraction. The theater is not as high as pure bureaucratic performance (piton) because medical urgency creates genuine time pressure — theater is embedded in actual gatekeeping, not separate from it.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates dramatic perspectival divergence. The trapped patient sees pure snare — no coordination benefit, only extraction through survival threat. The Palestinian health authority sees tangled rope — genuine medical coordination mixed with forced dependence. The Egyptian state sees rope or even coordination benefit — the constraint solves a real security and border management problem while providing leverage. The Israeli security establishment also sees coordination with benefit — genuine counter-smuggling function mixed with strategic capacity degradation. The international humanitarian system sees a scaffold — a temporary coordination problem with a sunset via political resolution or alternative infrastructure. Gaza civil society sees snare with emerging countervailing power — same extraction as the trapped patient but with organized resistance creating incipient alternatives. The analytical observer sees structural snare — the constraint exists as a pure extraction mechanism embedded in the territorial partition regime, with other classifications reflecting beneficiary or organizational perspectives rather than structural reality. The gap between the beneficiary perspectives (rope/coordination) and victim perspectives (snare/extraction) is maximal — the same institutional arrangement appears as manageable coordination from the gate-controlling position and as life-or-death extraction from the gated position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position within the extraction flow. The Gaza patient, trapped with no exit, derives d ≈ 0.95 (full target): maximum experienced extraction because they cannot negotiate or substitute. The Palestinian health authority, constrained but retaining triage authority, derives d ≈ 0.55 (mixed): partial victim role but also partial coordination function. Egyptian state, institutional beneficiary with arbitrage options, derives d ≈ 0.10 (near-beneficiary): low effective extraction because they control the gate and can exit by opening it. Israeli security establishment, powerful with mobile options but politically constrained, derives d ≈ 0.35 (constrained beneficiary): moderate effective extraction because the security role is real but secondary to Egyptian gatekeeping. International humanitarian system, organized but dependent on state cooperation, derives d ≈ 0.45 (mixed): moderate extraction because they have agency but face structural limits. Gaza civil society, organized but territorially trapped, derives d ≈ 0.62 (victim with emerging countervailing power): extractiveness experienced but declining as organization increases. Analytical observer, global scope, derives d ≈ 0.72 (external observer with no direct position): sees the extraction mechanism clearly from outside.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH STRUCTURAL DIFFERENTIATION: The mandatrophy (Is this coordination or extraction?) is resolved by recognizing that the constraint serves both functions simultaneously but asymmetrically. Egyptian state genuinely coordinates border management through the Rafah gate — this is a real, necessary function. Palestinian patients are genuinely extracted through the same gate — their compliance is coerced through survival threat. Both readings are structurally accurate; the constraint is a tangled rope from Egypt's perspective (real coordination + asymmetric benefit) and a snare from the patient's perspective (pure extraction). The snare classification for the victim is primary because extraction is the dominant mechanism from the patient's structural position (no exit, no alternatives, death threat). The constraint would remain snare even if the coordination function were fully removed — the extraction mechanism is independent of whether gatekeeping serves a real security function. This distinction (snare even if coordination genuinely necessary) is critical: it prevents the common move of naturalizing extraction as 'necessary coordination' simply because the coordination is real. The mandatrophy resolution: this IS a snare (extraction primary), AND it simultaneously IS tangled_rope (mixed coordination/extraction) from the gatekeeper's perspective. Both are true. The engine classifies as snare because the victim's structural reality determines the constraint type when perspectives diverge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    humanitarian_exception_gaming,
    'Are humanitarian exceptions (medical referrals) genuine coordination mechanisms or extraction cover stories used to justify gatekeeping while appearing benevolent?',
    'Track correlation between approval rates and political events; compare approval rates to medical urgency scores; analyze whether denial rates reflect security rationale or collective punishment',
    'If exceptions genuine: classification softens toward rope from powerful/organized perspectives. If cover story: confirms snare classification — apparent coordination is theater masking extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_exception_gaming, empirical, 'Whether humanitarian exceptions are genuine coordination or extraction cover').

omega_variable(
    death_rate_causation_attribution,
    'How much of the Gaza medical mortality excess is attributable to the Rafah route constraint versus internal infrastructure destruction and resource scarcity?',
    'Comparative analysis: mortality rates in pre-2007 Gaza with open Egyptian crossing vs post-2007; modeling of medical death attribution to access barriers vs capacity loss; counterfactual analysis of survival with unrestricted movement',
    'If Rafah constraint accounts for >40% of excess mortality: strengthens snare classification (extraction mechanism is primary). If <20%: suggests internal destruction is primary, and Rafah becomes secondary constraint affecting already-degraded system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(death_rate_causation_attribution, empirical, 'Attribution of medical deaths to access constraint vs infrastructure loss').

omega_variable(
    alternative_medical_infrastructure_viability,
    'Could Palestinian medical capacity meet internal demand without external referrals if supply chains and electricity were stable?',
    'Analysis of pre-2008 internal hospital capacity and patient outcomes; modeling of current demand vs capacity under historical infrastructure conditions; assessment of permanent vs temporary capacity losses',
    'If viable: Rafah constraint is overlay on deeper problem (infrastructure). If not viable: external access is structurally necessary, making extraction through gating mechanism more severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_medical_infrastructure_viability, empirical, 'Viability of internal Palestinian medical self-sufficiency').

omega_variable(
    egyptian_state_interest_decomposition,
    'Does Egypt maintain the Rafah constraint primarily for security coordination (genuine border management) or for extraction benefit (leverage over Palestinian/Israeli politics)?',
    'Interviews with Egyptian state officials; analysis of crossing protocols against comparable international borders; assessment of whether protocols target genuinely dangerous items or reflect political gatekeeping',
    'If security-primary: Egypt''s perspective as institutional beneficiary with rope classification is accurate. If extraction-primary: Egypt''s role shifts toward collusion in snare mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(egyptian_state_interest_decomposition, empirical, 'Whether Egyptian gatekeeping serves security coordination or political extraction').

omega_variable(
    organized_patient_advocacy_power_threshold,
    'At what level of international pressure and civil society organization does Gaza patient advocacy shift from powerless/trapped to organized/constrained in the constraint structure?',
    'Track advocacy group capacity, documentation projects, legal filings, and media impact; measure correlation between organized campaigns and crossing opening/approval rate changes; identify inflection points where advocacy demonstrates countervailing power',
    'If threshold crossed: collective action by Gaza civil society can reclassify as rope from organized perspective. If threshold unreached: powerless agent remains structurally trapped regardless of organization attempts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organized_patient_advocacy_power_threshold, empirical, 'Threshold for patient advocacy organization to generate countervailing power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gaza_medical_access_egypt_route, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gma_tr_t0, gaza_medical_access_egypt_route, theater_ratio, 0, 0.38).
narrative_ontology:measurement(gma_tr_t5, gaza_medical_access_egypt_route, theater_ratio, 5, 0.47).
narrative_ontology:measurement(gma_tr_t10, gaza_medical_access_egypt_route, theater_ratio, 10, 0.55).
narrative_ontology:measurement(gma_tr_t15, gaza_medical_access_egypt_route, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(gma_be_t0, gaza_medical_access_egypt_route, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gma_be_t5, gaza_medical_access_egypt_route, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(gma_be_t10, gaza_medical_access_egypt_route, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(gma_be_t15, gaza_medical_access_egypt_route, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gaza_medical_access_egypt_route, enforcement_mechanism).
narrative_ontology:affects_constraint(gaza_medical_access_egypt_route, gaza_internal_medical_infrastructure).
narrative_ontology:affects_constraint(gaza_medical_access_egypt_route, palestinian_health_system_capacity).
narrative_ontology:affects_constraint(gaza_medical_access_egypt_route, israel_security_screening).

% DUAL FORMULATION NOTE:
% The Rafah medical access constraint is downstream of the territorial partition regime (Palestine confinement) and upstream of internal medical capacity degradation (infrastructure destruction). It can be decomposed into security coordination (Israeli/Egyptian vetting for weapons/explosives) and political extraction (gatekeeping used to enforce compliance). These are structurally distinct mechanisms with different ε values but are currently integrated into single constraint. Full analysis would separate security_coordination_gate (lower ε, genuine coordination function) from political_compliance_extraction (higher ε, pure extraction). Current story treats integrated mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gaza_medical_access_egypt_route, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
