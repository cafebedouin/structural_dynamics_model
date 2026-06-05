% ============================================================================
% CONSTRAINT STORY: eu_irgc_terrorist_designation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_irgc_terrorist_designation, []).

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
 *   constraint_id: eu_irgc_terrorist_designation
 *   human_readable: EU Terrorist Designation of Iran's IRGC
 *   domain: geopolitical/sanctions/counterterrorism
 *
 * SUMMARY:
 *   The EU's potential (or actual) terrorist designation of Iran's Islamic
 *   Revolutionary Guard Corps exemplifies how a geopolitical constraint can
 *   simultaneously function as coordination mechanism, extraction apparatus,
 *   performative ritual, and immutable law of statecraft depending on the
 *   observer's structural position. The designation creates coordination
 *   among US-allied intelligence networks while extracting from the Iranian
 *   economy, European business, and (rhetorically) international law. The
 *   constraint's core tension: it is presented as a technical
 *   counterterrorism measure but functions as a geopolitical sanction regime
 *   that requires continuous institutional enforcement and faces contestation
 *   from both Iranian counter-positioning and European diplomatic divergence.
 *   Theater has increased over time as the performative content (designation
 *   review cycles, legal procedures, compliance theater) has grown relative
 *   to operational effectiveness (IRGC continues operations, proxy networks
 *   adapt, regional influence persists). The constraint exhibits all six DR
 *   types from different perspectives, making it a diagnostic exemplar for
 *   how geopolitical structures appear differently depending on whether one
 *   is measuring from beneficiary (US-allied coalition), victim (Iranian
 *   economy), observer of institutional degradation (EU diplomatic
 *   establishment), or analytical distance (civilizational/realist view). The
 *   mandatrophy is NOT resolved: the constraint is fundamentally contested
 *   between a Rope reading (coordination benefit) and a Snare reading (pure
 *   extraction); the middle Tangled Rope classification reflects the genuine
 *   ambiguity.
 *
 * KEY AGENTS:
 *   - US-allied intelligence coalition (institutional/arbitrage): Primary beneficiary — coordination function enables joint operations, sanctions enforcement, strategic pressure without direct military conflict
 *   - Iranian civilian economy (powerless/trapped): Primary victim — designation cascades through banking, trade, shipping; no exit mechanism; bears extraction through economic isolation and secondary sanctions
 *   - Iranian state security apparatus (organized/constrained): Secondary victim/strategic actor — constrained but not powerless; retains capacity to negotiate, build counter-coalitions, adapt operational networks
 *   - European business/shipping interests (powerful/mobile): Secondary victim — mobile but constrained by compliance costs, reputational risk, forfeited trade opportunities
 *   - EU diplomatic coalition (organized/mobile): Organized beneficiary-with-reservation — experiences designation as temporary containment pending nuclear negotiations; sees sunset clause as implicit
 *   - International counterterrorism regime (institutional/arbitrage): Institutional observer — maintains designation ritual; sees own regime as partially degraded (piton characteristics)
 *   - Analytical observer (analytical/analytical): Civilizational view — risks naturalizing contingent sanction system as immutable law of geopolitics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_irgc_terrorist_designation, 0.58).
domain_priors:suppression_score(eu_irgc_terrorist_designation, 0.72).
domain_priors:theater_ratio(eu_irgc_terrorist_designation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_irgc_terrorist_designation, extractiveness, 0.58).
narrative_ontology:constraint_metric(eu_irgc_terrorist_designation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(eu_irgc_terrorist_designation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_irgc_terrorist_designation, tangled_rope).
narrative_ontology:human_readable(eu_irgc_terrorist_designation, "EU Terrorist Designation of Iran's IRGC").
narrative_ontology:topic_domain(eu_irgc_terrorist_designation, "geopolitical/sanctions/counterterrorism").

domain_priors:requires_active_enforcement(eu_irgc_terrorist_designation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_irgc_terrorist_designation, us_allied_intelligence_networks).
narrative_ontology:constraint_beneficiary(eu_irgc_terrorist_designation, gulf_state_security_interests).
narrative_ontology:constraint_beneficiary(eu_irgc_terrorist_designation, eu_counterterrorism_operations).
narrative_ontology:constraint_victim(eu_irgc_terrorist_designation, iranian_state_capacity).
narrative_ontology:constraint_victim(eu_irgc_terrorist_designation, eu_iran_trade_relations).
narrative_ontology:constraint_victim(eu_irgc_terrorist_designation, european_shipping_industry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRANIAN CIVILIAN ECONOMY (SNARE) — Trapped by designation cascades affecting banking, shipping, and trade. Secondary sanctions freeze access to SWIFT, rupture insurance markets, and isolate value chains. No exit mechanism; bears extraction through economic strangulation. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.97. High effective extraction.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IRANIAN STATE SECURITY (TANGLED ROPE) — Constrained by designation but retains capacity to negotiate, build counter-coalitions (Russia, China), and develop alternative financial networks. Designation creates coordination problem (how to respond?) while extracting through isolation. Requires active enforcement via intelligence sharing among allies. d≈0.68, f(d)≈1.05, σ=1.1 → χ≈0.67. Mixed coordination/extraction.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US-ALLIED SECURITY COALITION (ROPE) — Benefits from coordination function: designation creates legitimate pretext for intelligence sharing, joint operations against proxy networks, and strategic pressure without direct military conflict. d≈0.10, f(d)≈-0.05, σ=1.2 → χ≈-0.06. Net beneficiary through institutional arbitrage.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EUROPEAN BUSINESS & SHIPPING (TANGLED ROPE) — Mobile but constrained: companies can exit Iran market but face reputational/legal sanctions if non-compliant. Coordination benefit (predictable legal framework) mixed with extraction (forfeited trade opportunities, secondary sanctions compliance costs). d≈0.52, f(d)≈0.68, σ=1.0 → χ≈0.39. Moderate extraction from extraction mechanism.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: EU DIPLOMATIC COALITION (SCAFFOLD) — Organized agents (France, Germany, some EU states) experience the designation as temporary containment pending nuclear negotiations or geopolitical recalibration. Sunset clause implicit: if JCPOA returns to viability or regional settlement occurs, designation can be reversed. theater ≤ 0.70, χ ≤ 0.30. d≈0.38, f(d)≈0.38, σ=1.1 → χ≈0.15. Low extraction because exit path exists through diplomacy.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: INTERNATIONAL COUNTERTERRORISM REGIME (PITON) — Theater ≥ 0.70. The designation ritual (UNSC resolutions, EU listing procedures, designation review cycles) is substantially performative. The core function (preventing IRGC operational capability) is not achieved through designation alone — IRGC continues operations, adapts to sanctions, maintains proxy networks. Theater_ratio=0.68 approaches the piton gate. Designation persists through institutional inertia and legal compliance theater, not because it effectively degrades IRGC operational capacity.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / REALIST VIEW (MOUNTAIN) — From a civilizational perspective, designation is an immutable feature of great-power competition: all major powers maintain lists of adversary organizations; designation is inherent to statecraft. However, ε=0.58, suppression=0.72, and theater=0.68 contradict the mountain classification. This appears as a false summit — the observer risks naturalizing a contingent institutional arrangement (modern sanctions regimes, terrorist designation systems) as a law of statecraft.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_irgc_terrorist_designation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_irgc_terrorist_designation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_irgc_terrorist_designation, TR),
    TR >= 0.70.

:- end_tests(eu_irgc_terrorist_designation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from the Iranian economy through banking isolation, shipping impediments, and trade rupture, but not with the total severity of a pure snare (which would be 0.75+). Extraction is constrained by: (1) Iranian counter-measures (alternative payments, cryptocurrency, informal trade), (2) EU diplomatic hedging (some states maintain engagement channels), and (3) China/Russia providing partial circumvention. The 0.58 score reflects that extraction is significant but incomplete. Suppression (0.72): High. Barriers to exit are substantial: Iranian government cannot easily de-list the IRGC without appearing to capitulate; IRGC is embedded in Iranian state apparatus; proxy networks face operational constraints even if not disabled. However, suppression is not total (0.90+) because: (1) IRGC continues operations at reduced efficiency, (2) proxy networks maintain functional autonomy, (3) Iran has strategic alternatives (Russia, China partnerships). Theater ratio (0.68): High. The designation process itself is theatrical: EU listing procedures, UNSC discussions, designation review cycles follow legal ritual while IRGC's core operational capability (projecting power through proxies) persists. The procedural theater has increased as the operational effectiveness has plateaued. The theater reflects Goodhart drift: the metric (designation status) has replaced the goal (degrading IRGC operational capacity).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is the core diagnostic feature. The US-allied coalition sees Rope: designation is pure coordination benefit, enabling intelligence sharing and strategic pressure without direct military conflict, while providing legal/institutional cover. The Iranian economy sees Snare: no exit mechanism, extraction through cascading sanctions. The Iranian state security apparatus sees Tangled Rope: constrained by isolation but retains negotiation capacity and strategic alternatives; designation creates coordination problem (how to respond?) alongside extraction. European business sees Tangled Rope: coordination benefit (predictable legal framework for compliance) mixed with extraction (forfeited trade). The EU diplomatic establishment sees Scaffold: temporary containment pending nuclear settlement or geopolitical recalibration; implicit sunset clause. The international counterterrorism regime sees Piton: the designation ritual persists through institutional inertia despite modest operational effectiveness. The civilizational analytical observer risks a false summit (Mountain): naturalizing the sanction system as immutable feature of great-power competition. The true perspectival structure: beneficiaries (US-allied) experience rope; victims (Iranian economy) experience snare; institutional actors (EU, international regime) experience degraded tangles and rituals; the analytical view risks naturalizing contingency. No single type is 'correct' — the constraint IS the presheaf of these disagreements.
 *
 * DIRECTIONALITY LOGIC:
 *   US-allied intelligence coalition: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.03. Net beneficiary; low directionality because they have exit options (can de-escalate) but actively maintain the constraint. Iranian economy: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction; trapped by state-level designation. Iranian state security: Victim + constrained (organized power) → d≈0.68, f(d)≈1.05. High extraction but organized agents retain negotiation capacity and strategic alternatives. European business: Mixed + mobile → d≈0.52, f(d)≈0.68. Symmetric because companies can exit but face legal/reputational constraints; mobile exit option reduces d from pure victim status. EU diplomatic coalition: Organized + mobile → d≈0.38, f(d)≈0.38. Low extraction because coalition has agency, diplomatic channels, and sees a path forward through negotiation. International regime: Institutional + arbitrage → d≈0.10, f(d)≈-0.02. Piton classification comes from theater gate, not from directionality. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification appears but is a false summit (naturalization of contingency).
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The constraint does not cleanly resolve into a single classification because the beneficiary (US-allied coalition) sees Rope but the victim (Iranian economy) sees Snare. The Tangled Rope classification emerges from the fact that BOTH are structurally true: the constraint does solve a coordination problem (how to pressure Iran without direct military conflict) AND extracts from victims (Iranian economy, Iranian state capacity). The mandatrophy question: 'Is this coordination that has extraction overhead, or extraction that is justified as coordination?' cannot be definitively answered from structural data alone. Empirical resolution would require: (1) counterfactual analysis (would the coordination function exist without the extraction?), (2) comparison to alternative coordination mechanisms (could the same intelligence sharing occur without designation?), (3) Iranian perspective on whether 'being on the terrorist list' actually changes their strategic options vs. alternative isolation mechanisms. Current status: The constraint is genuine Tangled Rope because both the coordination function (intelligence sharing, strategic pressure) and the extraction (economic isolation, state capacity reduction) are structurally necessary to the mechanism. If the coordination function were removed, the constraint would collapse to pure Snare. If the extraction were removed, the constraint would weaken to pure Rope. The hybrid is not accidental; it is the central feature. The mandatrophy_resolved flag is false because the theoretical question 'Could we achieve the coordination benefit without the extraction?' remains open empirically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irgc_functionality_threshold,
    'At what operational capacity does IRGC designation become counterproductive (driving radicalization, creating martyr narrative, strengthening proxy cohesion)?',
    'Longitudinal analysis of IRGC proxy network resilience, recruitment rates, and operational tempo pre/post-designation; qualitative assessment of Iranian domestic framing',
    'If threshold crossed: designation shifts from constrained containment (tangled_rope) to pure extraction (snare). If threshold not crossed: tangled_rope/scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irgc_functionality_threshold, empirical, 'IRGC operational capacity and counterproductivity threshold').

omega_variable(
    eu_nuclear_diplomacy_path,
    'Will EU diplomatic efforts toward Iran nuclear settlement provide a viable exit path for the designation within 5-10 years?',
    'Tracking of JCPOA renegotiation milestones, EU-Iran direct engagement, and domestic political shifts in Iran; assessment of whether designation is explicitly tied to nuclear settlement',
    'If viable exit path: scaffold perspective confirmed, sunset clause is real structural feature. If no path: designation becomes permanent fixture (piton or snare, not scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_nuclear_diplomacy_path, empirical, 'Viability of EU diplomatic path to designation reversal').

omega_variable(
    secondary_sanctions_cascade,
    'Does EU designation trigger secondary sanctions on non-US companies, or does it remain primarily symbolic within EU jurisdiction?',
    'Historical analysis of actual secondary sanction enforcement; comparison to US IRGC designation enforcement; tracking of third-country compliance patterns',
    'If cascades (de facto secondary): extraction to Iranian economy is severe (snare classification strengthened). If symbolic: extraction is moderate (tangled_rope holds).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secondary_sanctions_cascade, empirical, 'Secondary sanctions cascade from EU designation').

omega_variable(
    proxy_network_independence,
    'Are Iranian proxy networks (Hezbollah, PMF, Houthis) functionally dependent on IRGC command-and-control, or operationally autonomous despite ideological alignment?',
    'Intelligence analysis of proxy network command structures; correlation between IRGC operational constraints and proxy network activity; assessment of ideological vs. tactical cohesion',
    'If dependent: designation constrains proxy ecosystem (moderate extraction benefit to allies). If autonomous: designation has minimal direct operational effect (coordination function oversells, extraction understates).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proxy_network_independence, empirical, 'Operational autonomy of Iranian proxy networks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_irgc_terrorist_designation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_irgc_tr_t0, eu_irgc_terrorist_designation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(eu_irgc_tr_t10, eu_irgc_terrorist_designation, theater_ratio, 10, 0.62).
narrative_ontology:measurement(eu_irgc_tr_t20, eu_irgc_terrorist_designation, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(eu_irgc_be_t0, eu_irgc_terrorist_designation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(eu_irgc_be_t10, eu_irgc_terrorist_designation, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(eu_irgc_be_t20, eu_irgc_terrorist_designation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_irgc_terrorist_designation, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_irgc_terrorist_designation, iran_nuclear_program_constraint).
narrative_ontology:affects_constraint(eu_irgc_terrorist_designation, gulf_state_proxy_networks).
narrative_ontology:affects_constraint(eu_irgc_terrorist_designation, eu_sanctions_regime_coherence).

% DUAL FORMULATION NOTE:
% The IRGC designation is downstream of the broader Iran nuclear/geopolitical constraint (which has different ε and structure) but represents a distinct enforcement mechanism with its own temporal dynamics and perspectival structure. The upstream nuclear constraint (ε≈0.45, uncertain Mountain vs Snare) affects designation through the JCPOA negotiation status; the designation feeds forward into proxy network constraints and EU sanctions regime coherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_irgc_terrorist_designation, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
