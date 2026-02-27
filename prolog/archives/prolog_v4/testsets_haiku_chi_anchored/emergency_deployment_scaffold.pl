% ============================================================================
% CONSTRAINT STORY: emergency_deployment_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emergency_deployment_scaffold, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: emergency_deployment_scaffold
 *   human_readable: The Tactical Crossing: Emergency Bridge Infrastructure as Temporary Coordination
 *   domain: technological/political
 *
 * SUMMARY:
 *   The tactical crossing represents the deployment of emergency
 *   military-grade bridge infrastructure following the structural failure of
 *   a legacy crossing mechanism. The constraint exhibits textbook scaffold
 *   properties: temporary coordination solution with explicit sunset clause
 *   and declining theater ratio as institutional confidence in the permanent
 *   replacement builds. The bridge solves a genuine collective action problem
 *   (geography requires crossing) while remaining explicitly temporary. The
 *   challenge is distinguishing true scaffold — where the sunset is credible
 *   and enforced — from hidden piton, where the emergency measure persists
 *   indefinitely through institutional inertia. The constraint's
 *   theater_ratio (0.55 at endpoint) reflects that emergency deployment has
 *   lower performative content than traditional infrastructure projects:
 *   decisions are made on speed and functional adequacy, not ceremony. As
 *   time advances and permanent replacement approaches, theater should
 *   decline further (approaching 0.35 baseline). If theater remains elevated
 *   or increases, the constraint is degrading toward piton classification.
 *
 * KEY AGENTS:
 *   - Military Logistics Command: Powerful institutional actor (powerful/mobile) — primary beneficiary; coordinates rapid deployment; has high exit mobility (can reroute)
 *   - Civilian Population: Moderate organizational level (moderate/constrained) — primary beneficiary; trapped by geography; constrained by lack of alternatives; benefits from restored connectivity
 *   - Regional Reconstruction Authority: Institutional actor (institutional/constrained) — manages temporary deployment AND controls permanent replacement timeline; owns the sunset mechanism
 *   - Legacy Bridge Regime: Institutional failure (institutional/arbitrage) — degraded piton that has structurally failed; represents the constraint this scaffold is replacing
 *   - Civil Society Coalition: Organized actors (organized/mobile) — merchants, civic groups, humanitarian organizations; high exit mobility; evaluate bridge on functional grounds
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing temporary measure as immutable; must recognize contingency of the crossing problem's solution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emergency_deployment_scaffold, 0.28).
domain_priors:suppression_score(emergency_deployment_scaffold, 0.42).
domain_priors:theater_ratio(emergency_deployment_scaffold, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emergency_deployment_scaffold, extractiveness, 0.28).
narrative_ontology:constraint_metric(emergency_deployment_scaffold, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(emergency_deployment_scaffold, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emergency_deployment_scaffold, scaffold).
narrative_ontology:human_readable(emergency_deployment_scaffold, "The Tactical Crossing: Emergency Bridge Infrastructure as Temporary Coordination").
narrative_ontology:topic_domain(emergency_deployment_scaffold, "technological/political").

domain_priors:requires_active_enforcement(emergency_deployment_scaffold).
narrative_ontology:has_sunset_clause(emergency_deployment_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emergency_deployment_scaffold, civilian_population_transit).
narrative_ontology:constraint_beneficiary(emergency_deployment_scaffold, military_logistics_command).
narrative_ontology:constraint_beneficiary(emergency_deployment_scaffold, regional_reconstruction_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MILITARY LOGISTICS COMMAND (ROPE) — Pure coordination benefit. The bridge solves the collective action problem of maintaining supply lines and troop mobility without imposing extraction costs. Exit options are mobile (can reroute if necessary). d≈0.48, f(d)≈0.60, σ=0.8 → χ≈0.13. Low effective extraction; coordination mechanism is transparent.
constraint_indexing:constraint_classification(emergency_deployment_scaffold, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: CIVILIAN POPULATION (SCAFFOLD) — Constrained by lack of alternative crossing mechanisms but benefits from restored connectivity. The bridge is understood as temporary emergency measure with explicit sunset horizon (12-24 months for permanent replacement). Exit options are constrained (cannot bypass crossing without extreme cost). d≈0.65, f(d)≈1.00, σ=0.8 → χ≈0.22. Moderate extraction justified by emergency coordination necessity; sunset clause legitimizes temporary asymmetry.
constraint_indexing:constraint_classification(emergency_deployment_scaffold, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: REGIONAL RECONSTRUCTION AUTHORITY (SCAFFOLD) — Institutional actor managing transition from emergency to permanent infrastructure. Coordinates bridge deployment while engineering permanent replacement. Exit options are constrained by infrastructure dependencies but agency is high (controls the sunset timeline). d≈0.50, f(d)≈0.65, σ=0.9 → χ≈0.16. Low-moderate effective extraction; institutional perspective owns the sunset mechanism.
constraint_indexing:constraint_classification(emergency_deployment_scaffold, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: LEGACY INFRASTRUCTURE REGIME (PITON) — The collapsed bridge represents a degraded piton that once coordinated regional mobility. The old regime persists through regulatory inertia (permits, land rights, maintenance contracts) even though structural failure has eliminated functional capacity. theater_ratio ≈ 0.65 (performative maintenance of defunct system). The emergency scaffold temporarily replaces lost function while the institutional inertia that maintained the piton dissipates. d≈0.05, f(d)≈-0.12, σ=0.9 → χ≈-0.02. The piton perspective sees the scaffold as its own replacement.
constraint_indexing:constraint_classification(emergency_deployment_scaffold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the crossing problem is structurally immutable: geography requires bridging. The bridge itself is a contingent solution, but the necessity of traversing the obstacle is a law-like constraint. However, the structural data (ε=0.28, suppression=0.42, theater=0.55) contradicts full mountain classification — the constraint exhibits contingent institutional choices (deployment speed, temporary vs permanent, cost allocation), revealing this as a false summit. The true constraint is temporary scaffolding, not inherent law.
constraint_indexing:constraint_classification(emergency_deployment_scaffold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: CIVIL SOCIETY COALITION (ROPE) — Organized groups (civic associations, merchant guilds, humanitarian organizations) benefit from bridge as pure coordination good. High exit mobility (can advocate for alternative solutions, can mobilize public opinion). Theater ratio from this perspective is low (0.35) — civil society evaluates bridge on functional merit (does it work?) rather than performative criteria. d≈0.40, f(d)≈0.40, σ=0.8 → χ≈0.11. Rope classification reflects genuine coordination without meaningful extraction.
constraint_indexing:constraint_classification(emergency_deployment_scaffold, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_deployment_scaffold_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(emergency_deployment_scaffold, TR),
    TR >= 0.70.

:- end_tests(emergency_deployment_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The emergency bridge imposes modest extraction relative to coordination benefit. Civilian population must accept temporary solution with uncertain timeline, but benefits from restored mobility. Military logistics gains coordination benefit with minimal extraction cost. The extractiveness is justified by emergency necessity and legitimized by explicit sunset clause. Theater ratio (0.55): Moderate-low. Emergency infrastructure deployment has lower performative content than traditional procurement (decisions based on speed and functionality rather than ceremony, permits, and ritual approval). Theater increases slightly over the interval as institutional processes normalize and permanent replacement planning generates procedural overhead, but remains below levels typical of peacetime infrastructure projects. Suppression (0.42): Moderate. Civilian population has constrained exit options (cannot bypass the crossing without extreme cost or danger) but is not trapped. Alternative routes exist (longer, more expensive, riskier). Suppression is justified by emergency conditions and explicitly understood as temporary. Claimed type: SCAFFOLD. Requires `requires_active_enforcement: true` (bridge maintenance requires active command) and `has_sunset_clause: true` (permanent replacement committed with target timeline).
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap is between the organizational levels and their relationship to the sunset clause. Military logistics (powerful/mobile) sees pure coordination (Rope) with low extraction. Civilian population (moderate/constrained) sees the same structure as Scaffold — temporary coordination with a time limit that constrains their exit but justifies suppression. Reconstruction authority (institutional) owns the sunset mechanism and experiences it as legitimate temporary deployment. The legacy piton perspective reveals the constraint's role: replacing a degraded coordination mechanism that failed. The analytical observer risks seeing the crossing as an immutable natural law (Mountain) — but the bridge's very existence demonstrates contingency. The civil society coalition sees Rope because they have exit mobility (can advocate, organize, pressure for alternative solutions). The perspectival gap measures the asymmetry in exit options and time horizons.
 *
 * DIRECTIONALITY LOGIC:
 *   Military logistics: Beneficiary + mobile → d≈0.48, f(d)≈0.60. Net beneficiary with genuine exit mobility. Civilian population: Beneficiary + constrained → d≈0.65, f(d)≈1.00. Constrained beneficiary; moderate extraction justified by emergency and sunset. Reconstruction authority: Institutional + constrained → d≈0.50, f(d)≈0.65. Institutional beneficiary with agency (controls sunset). Legacy piton: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Degraded predecessor; no longer extracting value. Civil society: Organized + mobile → d≈0.40, f(d)≈0.40. Organized beneficiaries with high exit mobility. Analytical observer: analytical → d≈0.72, f(d)≈1.15. The mountain classification is false — the bridge is contingent, not immutable.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy because the scaffold classification is justified by explicit structural features: (1) genuine coordination function (bridge solves crossing problem), (2) temporary deployment with credible sunset clause (permanent replacement commitment), (3) moderate extraction justified by emergency necessity and time-limited status, (4) declining theater ratio as institutional confidence builds. The constraint is at risk of mandatrophy drift if: (a) the sunset clause becomes indefinitely extended (indicating degradation to piton), (b) the civilian population begins experiencing the bridge as purely extractive rather than necessary coordination (would require reclassification to tangled_rope or snare), or (c) the permanent replacement becomes impossible, forcing indefinite dependency. The omega variables flag these risks. The analytical observer's mountain perspective is explicitly a false summit — the constraint is not immutable but contingent on institutional choices about permanent infrastructure investment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    permanent_replacement_timeline,
    'What constitutes a credible commitment to the sunset clause? Does the reconstruction authority have enforceable timelines for permanent bridge completion?',
    'Contractual review of reconstruction contracts; tracking of milestone achievement rates; comparison to historical infrastructure project timelines in the region',
    'If timeline is credible and enforced: scaffold classification holds, suppression remains moderate. If timeline is indefinitely extended: constraint degrades to piton (emergency measures become permanent through institutional inertia).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(permanent_replacement_timeline, empirical, 'Credibility and enforceability of permanent bridge sunset clause').

omega_variable(
    beneficiary_extraction_asymmetry,
    'Are military logistics and civilian transit extracting differential value from the emergency bridge, with military logistics capturing disproportionate benefit relative to cost contribution?',
    'Comparative analysis of traffic volume (military vs civilian), maintenance cost allocation, tolls/fees (if any), and resource commitment to bridge integrity',
    'If asymmetric military benefit: scaffold degrades toward tangled_rope (military extraction disguised as temporary coordination). If symmetric: pure scaffold coordination holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_extraction_asymmetry, empirical, 'Whether military logistics extracts disproportionate value from emergency bridge').

omega_variable(
    legacy_piton_causality,
    'Was the bridge collapse itself driven by institutional degradation (piton maintenance failure) or by exogenous shock (flood, attack, natural disaster)? Does the cause affect whether the emergency scaffold is truly temporary?',
    'Forensic engineering analysis of collapse cause; historical maintenance records of the collapsed bridge; comparison to regional infrastructure maintenance funding patterns',
    'If collapse caused by piton degradation: permanent replacement is strongly incentivized, scaffold sunset is credible. If exogenous shock: permanent bridge could be indefinitely delayed if temporary solution works adequately, risking indefinite scaffold-to-piton drift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legacy_piton_causality, empirical, 'Whether bridge collapse resulted from institutional piton degradation or exogenous shock').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emergency_deployment_scaffold, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ebridg_tr_t0, emergency_deployment_scaffold, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ebridg_tr_t6, emergency_deployment_scaffold, theater_ratio, 6, 0.48).
narrative_ontology:measurement(ebridg_tr_t12, emergency_deployment_scaffold, theater_ratio, 12, 0.55).

% Extraction over time
narrative_ontology:measurement(ebridg_be_t0, emergency_deployment_scaffold, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ebridg_be_t6, emergency_deployment_scaffold, base_extractiveness, 6, 0.24).
narrative_ontology:measurement(ebridg_be_t12, emergency_deployment_scaffold, base_extractiveness, 12, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emergency_deployment_scaffold, global_infrastructure).
narrative_ontology:affects_constraint(emergency_deployment_scaffold, legacy_bridge_piton).
narrative_ontology:affects_constraint(emergency_deployment_scaffold, permanent_infrastructure_replacement).
narrative_ontology:affects_constraint(emergency_deployment_scaffold, regional_logistics_network).

% DUAL FORMULATION NOTE:
% The emergency bridge scaffold is downstream of the legacy bridge piton (degraded predecessor) and upstream of the permanent infrastructure replacement. The legacy bridge represents institutional inertia maintaining a failed system; the emergency scaffold is the transition mechanism; the permanent bridge will be the new coordination constraint. These three form a constraint family spanning degradation, emergency response, and institutional renewal.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
