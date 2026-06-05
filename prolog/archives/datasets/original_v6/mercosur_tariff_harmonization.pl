% ============================================================================
% CONSTRAINT STORY: mercosur_tariff_harmonization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mercosur_tariff_harmonization, []).

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
 *   constraint_id: mercosur_tariff_harmonization
 *   human_readable: Mercosur Tariff Harmonization Constraint
 *   domain: trade_policy/regional_integration
 *
 * SUMMARY:
 *   Mercosur tariff harmonization represents a structural commitment among
 *   four South American states (Brazil, Argentina, Paraguay, Uruguay) to
 *   maintain a common external tariff (CET) and restrict intra-regional
 *   tariff variance since 1995. The constraint exhibits mixed coordination
 *   and extraction characteristics: it solves a real collective action
 *   problem (preventing tariff competition that would undermine regional
 *   protection) while distributing costs asymmetrically across consumers,
 *   smaller member states, and liberalization-oriented sectors. The
 *   constraint's theater has increased over the interval (1995–2009) as
 *   actual enforcement has diverged from official harmonization claims —
 *   numerous bilateral carve-outs, exemptions, and informal tariff
 *   adjustments have proliferated while the harmonization apparatus maintains
 *   its formal structure. The constraint demonstrates how regional trade
 *   governance can simultaneously generate genuine coordination benefits (for
 *   protected industries and bureaucratic institutions) and severe extraction
 *   (for dispersed consumers and constrained smaller states).
 *
 * KEY AGENTS:
 *   - Protected Domestic Industries: Primary beneficiary (powerful/mobile) — garner tariff protection preventing competition; benefit from mutual commitment of member states to high external tariffs
 *   - Consumers: Primary victim (powerless/trapped) — face artificially elevated prices and no exit mechanism; broadly dispersed, lack organization for political voice
 *   - Mercosur Bureaucracy: Secondary beneficiary (institutional/arbitrage) — derives institutional legitimacy, budgets, and enforcement authority from maintaining harmonization apparatus
 *   - Smaller Member States (Paraguay, Uruguay): Secondary victim (organized/constrained) — constrained by larger member state bargaining power; accept higher external tariffs than preferred in exchange for regional market access
 *   - Export-Oriented Sectors (Agriculture, Services): Organized victim-beneficiary (organized/mobile) — bear extraction through higher input costs but benefit from regional market access and see liberalization pressure as creating sunset
 *   - Analytical Observer: Structural analyst (analytical/analytical) — identifies genuine coordination function alongside asymmetric extraction, resolving classification to tangled rope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mercosur_tariff_harmonization, 0.52).
domain_priors:suppression_score(mercosur_tariff_harmonization, 0.48).
domain_priors:theater_ratio(mercosur_tariff_harmonization, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mercosur_tariff_harmonization, extractiveness, 0.52).
narrative_ontology:constraint_metric(mercosur_tariff_harmonization, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(mercosur_tariff_harmonization, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mercosur_tariff_harmonization, tangled_rope).
narrative_ontology:human_readable(mercosur_tariff_harmonization, "Mercosur Tariff Harmonization Constraint").
narrative_ontology:topic_domain(mercosur_tariff_harmonization, "trade_policy/regional_integration").

domain_priors:requires_active_enforcement(mercosur_tariff_harmonization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mercosur_tariff_harmonization, protected_domestic_industries).
narrative_ontology:constraint_beneficiary(mercosur_tariff_harmonization, mercosur_bureaucracy).
narrative_ontology:constraint_victim(mercosur_tariff_harmonization, consumers).
narrative_ontology:constraint_victim(mercosur_tariff_harmonization, export_oriented_sectors).
narrative_ontology:constraint_victim(mercosur_tariff_harmonization, smaller_member_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSUMERS (SNARE) — Trapped within tariff harmonization with no exit mechanism. Tariff floors are enforced across all member states; domestic consumers cannot opt out or access lower-priced imports. Face extraction through artificially elevated prices while bearing no benefit from coordination. Maximum experienced extraction — dispersed, disorganized group with no voice in negotiations.
constraint_indexing:constraint_classification(mercosur_tariff_harmonization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PROTECTED DOMESTIC INDUSTRIES (TANGLED ROPE) — Primary beneficiary but faces real constraints from harmonization requirements. Benefits from tariff floors that protect from competition, but coordination function is genuine — harmonization prevents member states from undercutting each other's tariff protection, enabling mutual commitment to high external tariffs. Industry faces real costs of enforcing tariff uniformity (compliance monitoring, prevention of smuggling). Moderate extraction on consumer victims balanced by coordination benefit for industries themselves.
constraint_indexing:constraint_classification(mercosur_tariff_harmonization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: SMALLER MEMBER STATES (TANGLED ROPE) — Constrained by asymmetric bargaining power but also benefit from regional trade access. Smaller economies (Paraguay, Uruguay) face pressure to accept harmonized tariffs set by larger members (Brazil, Argentina), but gain access to larger market without individual negotiating capacity. Experience mixed extraction (forced into higher tariffs than preferred) and coordination benefit (regional market integration). Exit options (bilateral deals, MERCOSUR withdrawal) carry high political and economic costs.
constraint_indexing:constraint_classification(mercosur_tariff_harmonization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MERCOSUR BUREAUCRACY (ROPE) — Experiences constraint as pure coordination mechanism: their institutional role is to enforce harmonization norms and resolve disputes. Bureaucratic agents have arbitrage options (enforcement discretion, interpretation variance, exemption management). Net beneficiary of the constraint structure — budgets, authority, and career pathways depend on maintaining the harmonization apparatus. Low experienced extraction; stable coordination apparatus that generates stable funding and institutional capacity.
constraint_indexing:constraint_classification(mercosur_tariff_harmonization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: EXPORT-ORIENTED SECTORS / SERVICES LIBERALIZERS (SCAFFOLD) — Organized interests (agricultural exporters, service providers, tech companies) push for tariff harmonization on THEIR inputs/services while resisting harmonization on their outputs. See the current tariff architecture as temporary — external pressure from WTO and bilateral free trade agreements creates a sunset logic: harmonized tariffs will eventually be disrupted by broader trade liberalization agreements. Experience constraint as temporary scaffolding blocking optimal trade patterns. Exit pathway visible through multilateral liberalization pressure. Organized coalition can apply pressure to modify terms; not trapped.
constraint_indexing:constraint_classification(mercosur_tariff_harmonization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY MERCOSUR INSTITUTIONAL IDENTITY (PITON) — From the institutional longevity perspective, tariff harmonization is a vestigial commitment that persists through path dependence and institutional inertia rather than active coordination function. The original 1990s vision of deep customs union integration has not materialized (numerous exemptions exist, bilateral tariff deals undermine uniformity, common tariff code enforcement is weak). The harmonization apparatus persists because: (a) no member state has incentive to formally abandon it (defection costs are visible; cost of maintaining the fiction is diffuse), (b) the theater of harmonization (coordinated tariff announcements, dispute resolution processes) maintains the institutional legitimacy of MERCOSUR itself. Theater ratio reflects that much harmonization activity is performative — exceptions, exemptions, and bilateral carve-outs undermine the uniformity that harmonization claims to enforce.
constraint_indexing:constraint_classification(mercosur_tariff_harmonization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Full structural view from civilizational/global scope reveals genuine coordination function (mutual commitment to external tariff protection, prevention of competitive undercutting) alongside asymmetric extraction (larger member dominance, consumer surplus transfer, constraint on smaller-state autonomy). The constraint persists because it solves a real coordination problem (commitment device) while distributing costs asymmetrically. Neither pure coordination nor pure extraction — the definitional case for tangled rope. Engine classification of this perspective validates the claimed type.
constraint_indexing:constraint_classification(mercosur_tariff_harmonization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mercosur_tariff_harmonization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mercosur_tariff_harmonization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mercosur_tariff_harmonization, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mercosur_tariff_harmonization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mercosur_tariff_harmonization, TR),
    TR >= 0.70.

:- end_tests(mercosur_tariff_harmonization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint transfers consumer surplus to protected industries and provides tariff revenue, but not as severe as pure extraction because genuine coordination benefits exist for member states (mutual commitment credibility, prevention of tariff undercutting). The measurement trajectory shows rising extractiveness from 0.42 (1995, immediately post-implementation) to 0.52 (2009), reflecting layered extraction: initial harmonization was partly efficiency-enhancing (regional integration benefit), but over time, the architecture has accumulated exemptions and bilateral carve-outs that preserve extraction while reducing coordination function. Suppression (0.48): Moderate. Consumers have theoretical alternatives (parallel imports, regional shopping, advocate for liberalization) but face significant barriers (transport costs, formal enforcement, lack of political organization). Smaller states have exit option (bilateral deals, potential MERCOSUR withdrawal) but carry high diplomatic and economic costs. Theater ratio (0.55): Moderate-rising. Harmonization apparatus maintains formal uniformity claims (CET schedules, dispute resolution processes) while actual tariff variance is substantial through exemptions (approximately 900 exemptions to CET as of 2009), bilateral tariff adjustments, and informal non-enforcement. Theater has risen over interval as the gap between claimed and actual harmonization has widened — the constraint maintains institutional legitimacy through performative activity (tariff announcement ceremonies, dispute resolution theater) while substance erodes.
 *
 * PERSPECTIVAL GAP:
 *   Gap between consumer perspective (Snare) and industry perspective (Rope) reflects fundamental disagreement on whether the constraint coordinates mutual benefit or enforces asymmetric extraction. Consumer gap to larger analytical view: consumers see mountain-like immutability of tariffs, but organized pressure (services liberalization, agricultural export interests, external trade pressure) reveals tariffs as contingent institutional arrangements, not natural law. Gap between smaller-state perception of constraint and larger-state perception: larger states (Brazil, Argentina) experience harmonization as coordination mechanism maintaining their protection; smaller states experience it as asymmetric constraint on their preferred tariff autonomy. Gap between bureaucratic perception (Rope) and actual enforcement data (Piton features): institution perceives itself as executing coordination, but rising exemptions and declining theater-to-substance ratio suggest institutional degradation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective derive from their structural position relative to extraction flow. Consumers: trapped exit + victim status → d ≈ 0.95 (maximum target), f(d) ≈ 1.42, experience high chi. Protected industries: mobile exit + beneficiary status → d ≈ 0.20 (strong beneficiary), f(d) ≈ 0.02, experience low/negative chi (net benefit). Smaller states: constrained exit + victim status → d ≈ 0.70 (moderate target), f(d) ≈ 1.00, experience moderate chi. Mercosur bureaucracy: arbitrage exit + beneficiary status → d ≈ 0.10 (institutional beneficiary), f(d) ≈ -0.10, net benefit (negative chi). Export-oriented sectors: mobile exit + mixed status → d ≈ 0.55 (symmetric), f(d) ≈ 0.75, moderate experienced extraction. Scope modifier σ(regional) = 0.9 reduces chi across all perspectives. Effective extraction chi = ε × f(d) × σ(S): ranges from χ ≈ -0.05 (industries) to χ ≈ 0.65 (consumers), validating tangled rope classification (0.40 ≤ χ ≤ 0.90 at institutional perspective).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy collapse by maintaining genuine coordination function alongside extraction. The defining feature preventing misclassification: protected industries genuinely need the coordination commitment (harmonization prevents member states from undercutting each other's tariffs, solving a prisoner's dilemma of tariff races). This is not theater — the commitment mechanism is real. Simultaneously, the extraction is not incidental coordination cost; it is asymmetric, concentrated, and borne by powerless dispersed actors. The constraint exhibits all tangled rope gates: (a) base_extraction ≥ 0.30 (0.52), (b) suppression ≥ 0.40 (0.48), (c) coordination function present (mutual tariff commitment), (d) asymmetric extraction present (consumers bear costs, industries reap benefits), (e) requires active enforcement (CET administration, dispute resolution, exemption monitoring). If the constraint were pure extraction (Snare), the coordination function would vanish — but organized industries genuinely need the commitment device. If it were pure coordination (Rope), the asymmetry would vanish — but distribution is asymmetric (d values differ sharply). The theater ratio trajectory (rising from 0.38 to 0.55) indicates potential degradation toward Piton over longer timescales if exemptions continue to undermine actual harmonization; current measurement does not yet justify piton classification (theater < 0.70), but omega variables document this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harmonization_enforcement_credibility,
    'How credibly does Mercosur actually enforce uniform tariffs, or is the constraint substantially theatrical?',
    'Empirical audit of tariff code enforcement actions, successful disputes resolved vs disputes that stalled, actual tariff variance across member states vs officially declared uniformity, incidence of bilateral carve-outs and exemptions',
    'If enforcement is strong: extraction mechanism is tightly coupled to harmonization apparatus, suppression ≈ 0.48. If enforcement is weak: suppression should be lower (exit is easier through non-enforcement), theater_ratio should be higher, classify as piton not tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harmonization_enforcement_credibility, empirical, 'Credibility of Mercosur tariff enforcement vs theater').

omega_variable(
    harmonization_necessity_vs_preference,
    'Do member states genuinely commit to harmonized tariffs because of coordination need, or do they maintain harmonization facades while negotiating bilateral/trilateral deals to achieve preferred tariff structures?',
    'Content analysis of Mercosur Council minutes and official tariff schedules vs revealed behavior in actual trade flows, bilateral agreement negotiations, informal tariff adjustments; interviews with trade policy officials on stated vs actual preferences',
    'If genuine commitment: coordination function is real, classification stable. If preference misalignment: the constraint is theater sustained by institutional path dependence, classify as piton, extractiveness should be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harmonization_necessity_vs_preference, empirical, 'Whether harmonization reflects genuine member commitment or institutional theater').

omega_variable(
    consumer_exit_cost_magnitude,
    'What is the actual economic magnitude of consumer harm from tariff harmonization floors, and how does it compare to perceived distribution in member states?',
    'CGE modeling of consumer surplus loss under current tariff structure vs counterfactual unilateral liberalization; price comparison studies for harmonized vs non-harmonized commodity categories; welfare distribution analysis by income quintile',
    'If consumer loss is large and concentrated in low-income groups: suppression is justified at ≈0.48, snare classification for consumer perspective is accurate. If loss is diffuse or moderate: suppression overestimated, should be lower, some consumers have exit options (parallel imports, regional shopping).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_exit_cost_magnitude, empirical, 'Magnitude of consumer welfare loss from tariff harmonization').

omega_variable(
    smaller_state_bargaining_asymmetry,
    'Is Paraguay/Uruguay genuinely constrained by harmonization norms, or do they accept harmonization voluntarily because access to larger markets outweighs tariff costs?',
    'Counterfactual analysis: modeling outcomes if smaller states withdraw from tariff harmonization (bilateral tariff negotiation dynamics); diplomatic cable analysis on how tariff decisions are negotiated; survey of trade policymakers in smaller states on perceived bargaining power and constraints',
    'If constrained (extracted from): exit options lower, classification remains tangled_rope. If voluntary: exit options higher (mobile), should classify as rope, smaller state power should be powerful or organized, not constrained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(smaller_state_bargaining_asymmetry, empirical, 'Whether smaller states are constrained by harmonization or voluntarily accept it').

omega_variable(
    external_trade_liberalization_sunset,
    'Is the scaffold classification valid? Will multilateral or bilateral free trade pressure actually force tariff restructuring, or is the harmonization constraint durable across generations?',
    'Projection of regional trade agreement negotiation dynamics over 10-20 year horizon; monitoring of WTO dispute outcomes; tracking of bilateral FTA negotiations involving Mercosur members; trend analysis of external tariff rates and convergence pressures',
    'If liberalization pressure is strong and structural: scaffold classification is valid, sunset is real, extractiveness ≈0.30 (temporary structure). If pressure is weak or absorbed through exemptions: sunset does not materialize, constraint is durable, classify as snare or tangled_rope indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(external_trade_liberalization_sunset, empirical, 'Whether external trade liberalization creates actual sunset for harmonization constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mercosur_tariff_harmonization, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(merc_tr_t0, mercosur_tariff_harmonization, theater_ratio, 0, 0.38).
narrative_ontology:measurement(merc_tr_t7, mercosur_tariff_harmonization, theater_ratio, 7, 0.48).
narrative_ontology:measurement(merc_tr_t14, mercosur_tariff_harmonization, theater_ratio, 14, 0.55).

% Extraction over time
narrative_ontology:measurement(merc_be_t0, mercosur_tariff_harmonization, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(merc_be_t7, mercosur_tariff_harmonization, base_extractiveness, 7, 0.47).
narrative_ontology:measurement(merc_be_t14, mercosur_tariff_harmonization, base_extractiveness, 14, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mercosur_tariff_harmonization, resource_allocation).
narrative_ontology:affects_constraint(mercosur_tariff_harmonization, regional_trade_integration_enforcement).
narrative_ontology:affects_constraint(mercosur_tariff_harmonization, consumer_welfare_in_customs_unions).

% DUAL FORMULATION NOTE:
% Mercosur tariff harmonization is upstream of broader regional integration enforcement (common market formation, labor mobility, regulatory harmonization). The tariff constraint establishes the precedent and institutional infrastructure for coordination mechanisms; downstream constraints inherit both its genuine coordination logic and its extractive asymmetries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mercosur_tariff_harmonization, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
