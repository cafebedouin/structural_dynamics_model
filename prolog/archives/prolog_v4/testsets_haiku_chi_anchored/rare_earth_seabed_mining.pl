% ============================================================================
% CONSTRAINT STORY: rare_earth_seabed_mining
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_seabed_mining, []).

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
 *   constraint_id: rare_earth_seabed_mining
 *   human_readable: Deep-Sea Rare Earth Mining and Global Supply Chain Control
 *   domain: economic/technological/environmental
 *
 * SUMMARY:
 *   Deep-sea rare earth mining, exemplified by Japan's test extraction near
 *   Minamitorishima Island, represents a constraint that simultaneously
 *   solves a genuine coordination problem (diversifying rare earth supply
 *   away from Chinese monopsony) and creates severe extraction from powerless
 *   ecological systems. The constraint operates at multiple institutional
 *   levels: Japan gains strategic independence; competing coastal states lose
 *   supply chain leverage; deep-sea benthic communities have no exit option;
 *   the International Seabed Authority represents a temporary regulatory
 *   scaffold with explicit sunset logic pending ocean impact science
 *   maturation. Theater ratio has risen from 0.25 (decade 1: genuine
 *   exploratory science framing) to 0.52 (decade 2: environmental compliance
 *   documentation that often substitutes for meaningful mitigation).
 *   Extractiveness has risen from 0.32 to 0.58 as the economic value of
 *   exclusive seabed mining rights becomes apparent and competing mining
 *   contractors scale operations. This progression reveals mandate drift: the
 *   constraint began as a coordination response to supply scarcity but is
 *   accumulating extractive mechanisms (licensing monopolies, coastal state
 *   exclusion, environmental externality pricing into corporate profits).
 *
 * KEY AGENTS:
 *   - Japan's Strategic Resource Independence: Institutional beneficiary (institutional/arbitrage) — achieves energy security and electronics independence from China's rare earth dominance
 *   - Deep-Sea Benthic Ecosystems: Primary victim (powerless/trapped) — abyssal communities have no exit from sediment extraction; face permanent or multi-century disturbance
 *   - Competing Coastal States (China, Indonesia, Philippines): Secondary beneficiary/victim (powerful/mobile) — lose leverage in rare earth markets but can potentially organize regional responses or develop alternative sources
 *   - Rare Earth Technology Manufacturers: Tertiary beneficiary (institutional/arbitrage) — gain stable diversified supply; captured into Japan's supply chain control rather than China's
 *   - Deep-Sea Mining Contractors: Tertiary beneficiary (powerful/mobile) — capture licensing rents and extraction premiums; have high exit option (can relocate to other seabed zones)
 *   - International Seabed Authority: Organized coordinator (organized/constrained) — creates provisional regulatory framework with explicit 10-15 year sunset pending scientific maturity; has leverage through licensing authority but limited enforcement capacity
 *   - Traditional Land-Based Mining Regions: Degraded institutional victim (institutional/constrained) — Mountain Pass, Bayan Obo, Lynas face market displacement and environmental remediation cost absorption; low power to organize opposition
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing technological/economic scarcity as immutable physical constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_seabed_mining, 0.58).
domain_priors:suppression_score(rare_earth_seabed_mining, 0.62).
domain_priors:theater_ratio(rare_earth_seabed_mining, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_seabed_mining, extractiveness, 0.58).
narrative_ontology:constraint_metric(rare_earth_seabed_mining, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rare_earth_seabed_mining, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_seabed_mining, tangled_rope).
narrative_ontology:human_readable(rare_earth_seabed_mining, "Deep-Sea Rare Earth Mining and Global Supply Chain Control").
narrative_ontology:topic_domain(rare_earth_seabed_mining, "economic/technological/environmental").

domain_priors:requires_active_enforcement(rare_earth_seabed_mining).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_seabed_mining, japan_strategic_resource_independence).
narrative_ontology:constraint_beneficiary(rare_earth_seabed_mining, rare_earth_technology_manufacturers).
narrative_ontology:constraint_beneficiary(rare_earth_seabed_mining, deep_sea_mining_contractors).
narrative_ontology:constraint_victim(rare_earth_seabed_mining, deep_sea_ecosystems).
narrative_ontology:constraint_victim(rare_earth_seabed_mining, competing_coastal_states).
narrative_ontology:constraint_victim(rare_earth_seabed_mining, long_term_ocean_chemistry_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEEP-SEA BENTHIC ECOLOGY (SNARE) — Abyssal organisms have no exit from sediment extraction. Cannot organize or resist. High suppression: minimal alternative habitat, dispersal-limited fauna. d≈0.98, f(d)≈1.40, σ=1.2 → χ≈0.98. Pure extraction from ecological substrate.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMPETING COASTAL STATES (TANGLED ROPE) — Face extraction of global rare earth supply chain autonomy. Can organize regionally (ASEAN frameworks) but lack unilateral exit. Benefit from rare earth access but lose leverage if Japan's seabed source succeeds. d≈0.70, f(d)≈1.05, σ=1.0 → χ≈0.61. Mixed: coordination through trade frameworks; extraction through supply concentration.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: JAPAN'S STRATEGIC RESOURCE INDEPENDENCE (ROPE) — Solves collective action problem of diversifying rare earth supply away from monopsonic Chinese control. Seabed mining represents genuine coordination benefit: enables all electronics/renewable manufacturers to access stable supply. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary; constraint perceived as pure coordination.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL SEABED AUTHORITY (SCAFFOLD) — ISA polymetallic nodule framework and emerging deep-sea mining codes represent temporary enforcement structures with explicit sunset logic: rules are framed as provisional pending ocean impact science maturation (10-15 years). Organized actors (environmental NGOs, scientific advisory bodies, coastal state coalitions) see the constraint as temporary coordination enabling research while protecting the commons. d≈0.45, f(d)≈0.50, σ=1.1 → χ≈0.32. Low effective extraction because organized agents have policy leverage and sunset pathways.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL LAND-BASED MINING (PITON) — Mountain Pass (USA), Bayan Obo (China), Lynas (Australia) represent degraded extractive systems. Theater ratio 0.55: environmental remediation obligations are largely unfunded, monitoring is sparse, and the industry persists through sunk capital inertia despite poor ecosystem performance. Seabed mining threatens their market position but they lack the coordination or political power to organize effective opposition. d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.55. Moderate extraction; institutional inertia maintaining degraded systems.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a geological/chemical perspective, rare earth concentration in accessible ore (land deposits) is a fixed thermodynamic constraint: crustal abundance of rare earths is ~150 ppm but economically extractable ore grades are ~0.1% in the best terrestrial deposits. Deep-sea polymetallic crusts (~5000 ppm for cobalt, ~800 ppm for rare earths) appear to violate this only by shifting the cost structure. This perspective risks naturalizing a contingent technological/economic boundary as an immutable physical limit. accessibility_collapse candidate: 0.80 (not quite mountain). However, structural data (ε=0.58, suppression=0.62) contradicts mountain gate (ε ≤ 0.25), revealing this as a false summit.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_seabed_mining_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rare_earth_seabed_mining, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_seabed_mining, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rare_earth_seabed_mining, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rare_earth_seabed_mining, TR),
    TR >= 0.70.

:- end_tests(rare_earth_seabed_mining_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint has genuine coordination value (solving rare earth supply concentration) but increasingly captures extractive rents through exclusive licensing, coastal state exclusion from seabed resources within their EEZ-adjacent zones, and environmental externality pricing into corporate profits. Initial extractiveness (0.32) reflected primary supply-diversification function. Current extractiveness (0.58) reflects layered extraction: licensing monopolies, resource concentration in Japanese/allied hands, and cost externalization to deep-sea systems. Suppression (0.62): High. Competing coastal states cannot unilaterally exit — they are structurally dependent on either seabed supply or alternative sources, both of which are being concentrated. Deep-sea ecosystems have zero escape options. Environmental regulations (ISA codes) exist but enforcement is weak and dispute resolution is slow. Suppression is not total because organized actors (environmental coalitions, ISA, coastal state networks) have some policy leverage, but leverage is constrained by capital costs of alternative mining and China's existing refining dominance. Theater ratio (0.48): Moderate. Environmental baseline studies and ISA compliance documentation serve real scientific functions but are also performative — they often substitute for mandatory mitigation and track compliance with weak regulations. Theater has increased as the scale of operations has expanded and environmental stakes have become visible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the range from rope to snare depending on agent position. Japan's strategic independence view sees a rope — genuine supply coordination problem being solved. The ISA and environmental coalitions see a scaffold — temporary regulatory challenge with scientific resolution pathway. Traditional land-based mining sees a piton — their degraded systems persist through inertia while being displaced by seabed alternatives. Competing coastal states see tangled rope — both benefit from supply diversification and lose leverage through it; they are constrained but not powerless. Deep-sea ecosystems see a snare — pure extraction with no coordination benefit and no exit option. The civilizational analytical observer risks seeing a mountain (naturalized scarcity) but the structural data (ε=0.58, suppression=0.62) contradicts this: the constraint is extractive enough to fail the mountain gate. The perspectival gap reveals that the 'natural scarcity' framing conceals a contingent institutional arrangement: rare earth scarcity is real as a problem of monopolized refining capacity and concentrated processing infrastructure, not as a fundamental physical constraint. Seabed mining shifts the extraction point but does not eliminate the underlying institutional concentration problem.
 *
 * DIRECTIONALITY LOGIC:
 *   Japan's strategic independence: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Deep-sea benthic ecosystems: Victim + trapped → d≈0.98, f(d)≈1.40. Maximum extraction. Competing coastal states: Mixed (both beneficiary via supply diversification and victim via supply chain leverage loss) + mobile → d≈0.70, f(d)≈1.05. Significant extraction but with exit optionality through regional coordination. Rare earth manufacturers: Beneficiary + mobile → d≈0.18, f(d)≈0.00. Slight beneficiary. ISA and environmental coalitions: Organized + constrained → d≈0.45, f(d)≈0.50. Low effective extraction; organized actors have policy leverage. Traditional land-based mining: Victim (market displacement) + constrained (capital-locked) → d≈0.65, f(d)≈0.95. Moderate-high extraction; institutional inertia forces continued operation despite displacement. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival but contradicted by structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY MEMBER: Deep-sea rare earth mining is downstream of two upstream constraints: (1) China's rare earth refining monopoly (higher empirical confidence, established 1990s-2020s) and (2) rare earth demand from renewable energy transition (very high confidence, physical). The mandatrophy here is resolved by showing that seabed mining solves the upstream monopoly problem (coordination) while creating new extraction from ocean systems (snare-like). The constraint is tangled rope at the structural level: genuine coordination function (supply diversification) coupled with asymmetric extraction (costs to ecosystems and competing states, benefits to Japan and contractors). The theater ratio trajectory (0.25 → 0.52) documents mandate drift: environmental compliance documentation increasingly substitutes for mitigation, a signature of piton degradation. The framework detects this through rising theater_ratio coupled with stable/rising extractiveness — if the coordination function were strengthening, theater would fall. Instead, theater rises while extraction strengthens, indicating the constraint is drifting from rope toward snare. The scaffold perspective (ISA regulation) is credible only if ISA enforcement legitimacy holds (omega variable: enforcement_legitimacy). If ISA enforcement fails, the scaffold collapses and the constraint becomes pure snare with regulatory theater. Current status: Tangled Rope, trending toward Snare if mandatrophy conditions deteriorate (ecosystem recovery time >> 100 years, enforceability fails, rare earth substitution stalls).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    seabed_ecosystem_recovery_timeline,
    'How long do abyssal sediment ecosystems require to recover from nodule/crust extraction, and does recovery occur at meaningful ecological timescales?',
    'Long-term benthic monitoring post-extraction; comparison with natural disturbance baselines (seismic events, turbidity currents); genetic diversity recovery in endemic fauna',
    'If recovery > 100 years: extraction is functionally permanent for biological communities. If recovery < 50 years: moderate short-term damage narrative becomes viable. Recovery timeline directly determines whether snare perspective (powerless/trapped) or scaffold perspective (temporary with sunset) is structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(seabed_ecosystem_recovery_timeline, empirical, 'Deep-sea ecosystem recovery timescale post-mining').

omega_variable(
    rare_earth_supply_substitution_feasibility,
    'Can technological substitution or circular economy strategies reduce rare earth demand faster than seabed supply ramps up, potentially rendering deep-sea mining unnecessary?',
    'Rare earth intensity trends in renewable energy and electronics; material substitution breakthroughs (e.g., rare-earth-free permanent magnets); recycling recovery rates for electronics/EV batteries',
    'If substitution > 40% demand reduction possible: seabed mining is extraction capturing transitional rents, not solving supply constraint (snare). If substitution < 15%: mining is genuine coordination response to real scarcity (rope). This resolves whether the constraint is about resource necessity or monopoly rent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rare_earth_supply_substitution_feasibility, empirical, 'Feasibility of rare earth demand reduction through substitution').

omega_variable(
    ocean_chemistry_cascade_thresholds,
    'Does large-scale seabed mining trigger tipping points in ocean chemistry (pH buffering capacity, oxygen minimum zones, calcium carbonate saturation) that are irreversible on human timescales?',
    'Biogeochemical modeling of sediment pore-water chemistry disturbance; coupled ocean circulation simulations; paleoceanographic analogs for natural sediment disturbance events',
    'If yes: constraint is effectively a snare (universal scope, irreversible harm). If no: harm is localized/reversible, and scaffold logic applies (temporary impact, recoverable). This is the civilization-timescale test that separates natural law constraints from extractive systems.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ocean_chemistry_cascade_thresholds, empirical, 'Irreversibility of ocean chemistry impacts from deep-sea mining').

omega_variable(
    china_monopoly_permanence,
    'Is China''s rare earth processing monopoly (95% global refining capacity) a persistent structural advantage or a transitional artifact of past investment?',
    'Capital cost analysis for rare earth refineries in other jurisdictions; China''s own processing cost trajectory and environmental constraint impacts; competing refinery project timelines (US, Japan, EU)',
    'If monopoly structural: Japan''s seabed mining solves genuine supply bottleneck (rope coordination). If monopoly transient: seabed mining is preemptive rent extraction (snare toward Japan''s competitors). Determines whether beneficiary group''s framing is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_monopoly_permanence, empirical, 'Permanence of China''s rare earth refining monopoly').

omega_variable(
    international_seabed_authority_enforcement_legitimacy,
    'Will ISA deep-sea mining codes be enforced against powerful state/corporate actors, or does the scaffold sunset logic depend on a governance capacity that ISA demonstrably lacks?',
    'Historical ISA enforcement patterns on nodule mining violations; state compliance rates with ISA provisional rules; fiscal capacity of ISA relative to dispute resolution needs',
    'If enforcement real: scaffold is structural, sunset logic is credible. If enforcement weak: ISA codes are performative (piton-like), and the constraint is actually a snare with regulatory theater. This determines whether organized actors (ISA, environmental coalitions) have genuine leverage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_seabed_authority_enforcement_legitimacy, conceptual, 'Legitimacy and enforceability of ISA deep-sea mining codes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_seabed_mining, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(resm_tr_t0, rare_earth_seabed_mining, theater_ratio, 0, 0.25).
narrative_ontology:measurement(resm_tr_t10, rare_earth_seabed_mining, theater_ratio, 10, 0.48).
narrative_ontology:measurement(resm_tr_t20, rare_earth_seabed_mining, theater_ratio, 20, 0.52).

% Extraction over time
narrative_ontology:measurement(resm_be_t0, rare_earth_seabed_mining, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(resm_be_t10, rare_earth_seabed_mining, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(resm_be_t20, rare_earth_seabed_mining, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_seabed_mining, resource_allocation).
narrative_ontology:affects_constraint(rare_earth_seabed_mining, china_rare_earth_refining_monopoly).
narrative_ontology:affects_constraint(rare_earth_seabed_mining, renewable_energy_supply_chain_criticality).
narrative_ontology:affects_constraint(rare_earth_seabed_mining, international_seabed_authority_authority).

% DUAL FORMULATION NOTE:
% Deep-sea rare earth mining is structurally downstream of China's refining monopoly (higher ε, established) and rare earth demand from renewable transition (very high confidence). However, these are distinct constraints: the monopoly has ε ≈ 0.45 (rope/tangled_rope depending on perspective); the demand is ε ≈ 0.05 (rope: genuine coordination); the seabed mining constraint has ε = 0.58 (tangled_rope: coordination + extraction). Seabed mining is the mechanism by which the monopoly constraint is addressed, but it introduces new extraction mechanisms (ecosystem harm, coastal state leverage loss) not present in the upstream constraints. The network link captures this causal dependency and institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rare_earth_seabed_mining, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
