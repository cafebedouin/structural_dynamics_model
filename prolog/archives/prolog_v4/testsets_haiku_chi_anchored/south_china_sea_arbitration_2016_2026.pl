% ============================================================================
% CONSTRAINT STORY: south_china_sea_arbitration_2016_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_south_china_sea_arbitration_2016_2026, []).

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
 *   constraint_id: south_china_sea_arbitration_2016_2026
 *   human_readable: The 2016 South China Sea Arbitral Award (2016-2026)
 *   domain: geopolitical/legal
 *
 * SUMMARY:
 *   The 2016 South China Sea Arbitral Award represents a critical juncture in
 *   international law enforcement: a binding award that China rejected,
 *   creating a structural constraint with radically divergent interpretations
 *   depending on the observer's position. The award is a tangled hybrid of
 *   coordination and extraction — it provides a legal framework
 *   (coordination) while simultaneously creating asymmetric enforcement
 *   burdens and exclusion for coastal communities (extraction). The
 *   constraint exhibits high theater ratio (0.68) because most post-award
 *   activity is performative: diplomatic statements about 'rules-based
 *   order,' legal filings asserting 'freedom of navigation,' and media
 *   coverage emphasizing 'international law' — while operational enforcement
 *   remains minimal and contested. The extractiveness (0.58) reflects that
 *   China captures benefits of non-compliance (continued effective control,
 *   domestic legitimacy) without bearing proportional enforcement costs,
 *   while claimant states and fishing communities bear costs without gaining
 *   proportional benefits. The suppression (0.72) is high because enforcement
 *   mechanisms are weak: China faces reputational costs but not military
 *   deterrence; fishing communities face de facto exclusion enforced by coast
 *   guard presence; the international legal order faces systematic
 *   undermining without effective remedy.
 *
 * KEY AGENTS:
 *   - China: Powerful state (powerful/constrained) — nominally constrained by award but de facto unconstrained by enforcement; primary defector from the constraint; benefits from non-compliance
 *   - Philippines and Claimant Coalition: Organized mid-powers (organized/constrained) — primary beneficiaries of the award's legitimacy but constrained by dependence on external enforcement (US, Japan, Australia) and inability to compel compliance
 *   - Coastal Fishing Communities: Powerless agents (powerless/trapped) — nominally protected by expanded EEZ rights but trapped by Chinese coast guard enforcement and lack of enforcement support; actual losers
 *   - International Law System: Abstract collective good (powerless/trapped) — cannot enforce itself; bears costs of systematic undermining without remedy
 *   - UNCLOS Institutional Framework: Institutional actor (institutional/arbitrage) — benefits from demonstrated functionality of arbitration mechanism; low exposure because tribunal did its job (judgment rendered)
 *   - US/Japan/Australia/Quad Coalition: Powerful institutional actors (organized/constrained) — building enforcement capacity through naval operations and alliance mechanisms; represent the scaffold perspective (sunset via maturation of enforcement)
 *   - Diplomatic/Legal Performance System: Institutional maintenance mechanism (institutional/arbitrage) — perpetuates theater through statements, briefs, and symbolic operations; benefits from continued award visibility regardless of enforcement outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(south_china_sea_arbitration_2016_2026, 0.58).
domain_priors:suppression_score(south_china_sea_arbitration_2016_2026, 0.72).
domain_priors:theater_ratio(south_china_sea_arbitration_2016_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(south_china_sea_arbitration_2016_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(south_china_sea_arbitration_2016_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(south_china_sea_arbitration_2016_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(south_china_sea_arbitration_2016_2026, tangled_rope).
narrative_ontology:human_readable(south_china_sea_arbitration_2016_2026, "The 2016 South China Sea Arbitral Award (2016-2026)").
narrative_ontology:topic_domain(south_china_sea_arbitration_2016_2026, "geopolitical/legal").

domain_priors:requires_active_enforcement(south_china_sea_arbitration_2016_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(south_china_sea_arbitration_2016_2026, philippines).
narrative_ontology:constraint_beneficiary(south_china_sea_arbitration_2016_2026, claimant_states).
narrative_ontology:constraint_beneficiary(south_china_sea_arbitration_2016_2026, international_law_system).
narrative_ontology:constraint_victim(south_china_sea_arbitration_2016_2026, china).
narrative_ontology:constraint_victim(south_china_sea_arbitration_2016_2026, fishermen_communities).
narrative_ontology:constraint_victim(south_china_sea_arbitration_2016_2026, disputed_sea_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FISHING COMMUNITIES (SNARE) — Filipino, Vietnamese, and regional fishing communities are trapped in the constraint: nominally protected by the award (EEZ access), but enforcement is minimal and Chinese coast guard presence creates de facto exclusion. No exit option; bears costs of restricted access. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.73.
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INTERNATIONAL LEGAL ORDER (SNARE) — The award is extracted from by both China (non-compliance) and claimants (selective enforcement). The abstract good of predictable law-based order bears costs without enforcement capacity. d≈0.94, f(d)≈1.40, σ=1.0 → χ≈0.82.
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: PHILIPPINES & CLAIMANT COALITION (TANGLED ROPE) — Organized agents benefit from the award (legitimacy, legal standing, EEZ rights) AND extracted from by enforcement costs, dependence on external support (US, Japan), and ongoing Chinese pressure. Coordination function: unity among claimant states vs extraction: resource asymmetry forces continued deference to dominant powers. d≈0.58, f(d)≈0.73, σ=0.9 → χ≈0.38.
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: UNCLOS INSTITUTIONAL FRAMEWORK (ROPE) — The award affirmed the coordination function of UNCLOS: dispute resolution via binding arbitration. UNCLOS institutions (tribunal, registry) experience the award as successful coordination — legal clarity benefits all rule-of-law actors. d≈0.12, f(d)≈0.08, σ=1.0 → χ≈0.05.
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CHINA (TANGLED ROPE) — Constrained by the award (legitimacy costs, international pressure) but benefits from coordination function if it chose engagement (disputes resolved predictably rather than through military escalation). However, enforcement against China is minimal — the constraint actually provides de facto veto: non-compliance incurs reputational cost but no enforcement cost. d≈0.48, f(d)≈0.65, σ=1.0 → χ≈0.38. Hybrid extraction and coordination: the award functions as coordination for compliers, extraction for non-compliers (China can ignore without enforcement).
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL ENFORCEMENT COALITION (SCAFFOLD) — Organized actors (ASEAN, Japan, US, Australia, EU) are building enforcement capacity through naval presence, treaty commitments (Quad, AUKUS), and legal support networks. The award has a sunset: as enforcement capacity grows (Freedom of Navigation operations, rules-based order strengthening), the extractive gap narrows. Theater ratio reflects: current enforcement is more legal performance (statements) than operational reality. d≈0.35, f(d)≈0.38, σ=1.2 → χ≈0.15. Low effective extraction because coalition sees clear exit path: institutionalize enforcement.
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: HISTORICAL DISPUTE MECHANISM (PITON) — From a civilizational/historical lens, the award is a degraded instance of a once-functional territorial dispute resolution system (Treaty of Westphalia logic: external adjudication of sovereignty claims). That system persisted through the Cold War but has atrophied — enforcement capacity for arbitral awards is now primarily theater (statements, legal briefs, symbolic operations). Theater ratio 0.68 reflects: most activity is performative (diplomatic statements, legal filings, media coverage) rather than enforcement operations. The award's actual material impact is limited relative to the symbolic weight invested in it.
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN?) — The analytical view risks naturalizing the award as an immutable feature of international law: 'arbitral awards are binding and disputes resolved via law.' But structural data contradicts mountain classification: ε=0.58 (moderate-high extraction), suppression=0.72 (significant coercion), theater=0.68 (highly performative). The award is contingent on US enforcement capacity and strategic interest, not a law of nature. False summit detector will flag this perspective.
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(south_china_sea_arbitration_2016_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(south_china_sea_arbitration_2016_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(south_china_sea_arbitration_2016_2026, TR),
    TR >= 0.70.

:- end_tests(south_china_sea_arbitration_2016_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The award creates asymmetric gains: China gains from non-compliance (effective control without legitimacy cost), claimant states and fishing communities lose access without compensation or enforcement support. The award is not a pure Snare (would require χ > 0.66) because it did provide legitimate standing and EEZ recognition to claimants — that is a real coordination benefit. But China's ability to reject it without enforcement creates extraction: the rule-of-law framework is hijacked by powerful defection. Suppression (0.72): High. Enforcement is suppressed by: (1) lack of enforcement mechanism (arbitral awards have no enforcement agency), (2) cost asymmetry (China can maintain presence at lower operational cost than allied enforcement), (3) domestic political constraints (compliance would damage Chinese regime legitimacy), (4) geographic factors (SCS is proximate to China, distant from external enforcers). Theater ratio (0.68): High and rising. In year 0 (2016), theater was ~0.55: genuine surprise and shock that award was rendered. By 2026, theater has risen to 0.68: activity is now dominated by statements (Quad statements, ASEAN statements, Chinese statements), legal operations (filing amicus briefs, asserting positions), and symbolic naval operations (FON operations broadcast to international media) rather than actual enforcement (China's effective control unchanged, fishing communities still excluded, dispute unresolved). The growth in theater ratio reflects Goodhart's law: as the award became visible, metrics (diplomatic statements, press releases) substituted for outcome (actual compliance, fishing access restored).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence from identical structural data. Fishing communities see a Snare: nominally protected but actually excluded, with no enforcement support or exit option. The international law system sees a Snare: undermined systematically without enforcement remedy. China sees a Tangled Rope: nominally constrained but benefits from coordination avoidance (if compliant, disputes resolved via law; if non-compliant, gains effective control). Claimants see a Tangled Rope: coordination function (legal standing) plus extraction (enforcement costs, dependence on external support). The Quad coalition sees a Scaffold: enforcement is under construction (freedom of navigation ops, alliance-building), with a clear sunset path (mature enforcement capacity in 10-20 years). UNCLOS sees a Rope: the arbitration mechanism worked and remains available for future disputes. The historical/civilizational view risks a Mountain (international law is inherent to state system) but structural data contradicts this — the award is contingent on US enforcement capacity and strategic interest. The perspectival gap is extreme: from the fishing community's view, the award is a pure Snare; from UNCLOS's view, it's a successful Rope. Both are structurally correct from their positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Fishing communities: Victim + trapped → d≈0.92, f(d)≈1.38. Maximally extracted. International law system: Victim + trapped (abstract, cannot defend itself) → d≈0.94, f(d)≈1.40. Maximally extracted. Philippines/claimants: Mixed (beneficiary of legal standing + victim of enforcement burden) + constrained → d≈0.58, f(d)≈0.73. Moderate-high extraction. China: Nominally victim but de facto beneficiary (non-compliance without cost) + constrained → d≈0.48, f(d)≈0.65. Moderate extraction. UNCLOS institutions: Beneficiary + arbitrage → d≈0.12, f(d)≈0.08. Net beneficiary (tribunal succeeded). Quad coalition: Organized beneficiary (building enforcement capacity) + constrained (must maintain operations) → d≈0.35, f(d)≈0.38. Low-moderate extraction. Historical mechanism: Institutional + arbitrage → d≈0.08, f(d)≈-0.02. Near beneficiary (piton classification comes from theater gate, not directionality).
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is confirmed by the mandatrophy test: (1) Genuine coordination function present: the award established legal standing for claimants and provided a framework for dispute resolution — this is coordination. (2) Asymmetric extraction present: China bears no enforcement cost, claimants bear enforcement burden, fishing communities bear access loss — this is extraction. (3) Active enforcement required: the award cannot self-execute; it requires external enforcement (Quad operations, allied support) — confirmed. The Snare perspective (fishing communities, international law system) is also valid: from their position, the constraint appears as pure extraction because they cannot exit and their position has not improved. The mandatrophy is resolved by recognizing that Tangled Rope and Snare are the same constraint from different structural positions. The classification hierarchy: from China's view, it's a Tangled Rope (constrained but benefiting from coordination avoidance); from claimants' view, it's a Tangled Rope (coordination function + enforcement burden); from fishing communities' view, it's a Snare (trapped, no benefit). No type is 'wrong' — the perspectival presheaf IS the full answer. The false summit (analytical/civilizational/mountain) is correctly identified as a false summit: the award is not an immutable law of nature but a contingent institutional arrangement depending on US enforcement capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_threshold,
    'What enforcement capacity (freedom of navigation operations, allied naval presence, economic sanctions) is required to make the award functionally binding rather than purely symbolic?',
    'Comparative analysis of compliance rates under different enforcement regimes; modeling of military/economic deterrence thresholds; historical comparison with other arbitral awards',
    'If low threshold (current level): award is binding via reputational cost. If high threshold: award requires major power commitment to enforce. Classification shifts from Snare/Tangled Rope toward Scaffold (with longer sunset).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_threshold, empirical, 'Enforcement capacity required for functional compliance').

omega_variable(
    china_compliance_pathway,
    'Does China have a credible pathway to partial compliance (recognize EEZ, abandon nine-dash line) without losing domestic political legitimacy?',
    'Analysis of Chinese internal politics; modeling of nationalist constraints; comparison with historical precedents (Vietnam''s accommodation, Indonesia''s position); tracking of Chinese rhetoric evolution',
    'If pathway exists: constraint can transition from Snare toward Rope/Tangled Rope as coordination mechanism. If no pathway: constraint remains extractive (Snare) because compliance requires regime-level change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(china_compliance_pathway, conceptual, 'Whether China can accommodate the award domestically').

omega_variable(
    fishing_community_substitution,
    'Can displaced fishing communities substitute lost SCS access with alternative fishing grounds or alternative livelihoods without major economic disruption?',
    'Economic analysis of alternative fishing zones, mariculture viability, port redevelopment; household surveys of fishing-dependent communities; cost-benefit modeling of livelihood transitions',
    'If substitution possible: constraint harm is mitigated (victims transition to constrained status). If not: constraint remains Snare for fishing communities (trapped without alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fishing_community_substitution, empirical, 'Livelihood substitution feasibility for displaced fishing communities').

omega_variable(
    quad_enforcement_durability,
    'Is the emerging Quad/AUKUS enforcement coalition durable across US administrations and geopolitical shifts, or is it contingent on current US strategic focus?',
    'Institutional analysis of alliance structures; modeling of dependency on US commitment; tracking of allied capability investments; historical comparison with Cold War security commitments',
    'If durable: enforcement coalition matures, scaffold sunset becomes real (10-20 year horizon). If contingent: enforcement remains theater, constraint stays Snare/Piton for long term.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quad_enforcement_durability, conceptual, 'Durability of enforcement coalition across administration changes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(south_china_sea_arbitration_2016_2026, 2016, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scs_arb_tr_t0, south_china_sea_arbitration_2016_2026, theater_ratio, 0, 0.55).
narrative_ontology:measurement(scs_arb_tr_t5, south_china_sea_arbitration_2016_2026, theater_ratio, 5, 0.62).
narrative_ontology:measurement(scs_arb_tr_t10, south_china_sea_arbitration_2016_2026, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(scs_arb_be_t0, south_china_sea_arbitration_2016_2026, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(scs_arb_be_t5, south_china_sea_arbitration_2016_2026, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(scs_arb_be_t10, south_china_sea_arbitration_2016_2026, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(south_china_sea_arbitration_2016_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(south_china_sea_arbitration_2016_2026, south_china_sea_freedom_of_navigation).
narrative_ontology:affects_constraint(south_china_sea_arbitration_2016_2026, asean_unity_constraint).
narrative_ontology:affects_constraint(south_china_sea_arbitration_2016_2026, us_pivot_to_asia_commitment).

% DUAL FORMULATION NOTE:
% The arbitral award itself is a distinct constraint from the broader SCS dispute. The award provides the formal legal framework (Rope-like functionality for institutional actors), but enforcement of the award creates a separate, extractive constraint (Snare/Tangled Rope for victims and claimants). The 2016 award is upstream of the FON operational constraint and ASEAN cohesion constraint — it establishes legitimacy that those constraints depend on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(south_china_sea_arbitration_2016_2026, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
