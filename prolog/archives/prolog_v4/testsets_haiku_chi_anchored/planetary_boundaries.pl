% ============================================================================
% CONSTRAINT STORY: planetary_boundaries
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_planetary_boundaries, []).

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
 *   constraint_id: planetary_boundaries
 *   human_readable: Planetary Boundaries Framework
 *   domain: environmental/economic
 *
 * SUMMARY:
 *   The Planetary Boundaries framework, proposed by Rockström et al. (2009),
 *   defines a 'safe operating space for humanity' across nine critical Earth
 *   system processes: climate change, biosphere integrity, land-system
 *   change, freshwater use, phosphorus and nitrogen cycles, ocean
 *   acidification, chemical pollution, ozone depletion, and atmospheric
 *   aerosol loading. The framework creates a structural tension between
 *   wealthy economies that benefit from present consumption patterns and all
 *   other actors who bear the costs of boundary transgression. This
 *   constraint exhibits a perspectival chasm: high-income economies and
 *   extractive industries experience boundaries as a coordination problem
 *   (rope/scaffold) solvable through technology and market mechanisms; future
 *   generations, global south communities, and ecosystem integrity experience
 *   boundaries as extraction (snare) with no exit pathway. The theater ratio
 *   (0.62) reflects the gap between governance performance (COP pledges,
 *   net-zero commitments, ESG reporting) and actual decarbonization outcomes
 *   — ambitious targets are announced while structural emissions patterns
 *   persist. The constraint's extractiveness (0.58) has risen from 0.35 to
 *   its current level as boundary transgression has accelerated despite
 *   growing awareness, suggesting that the governance theater is decoupling
 *   from functional outcomes.
 *
 * KEY AGENTS:
 *   - Future Generations: Primary powerless victim (powerless/trapped) — will inherit boundary-transgressed systems with no agency in policy choices
 *   - Global South Populations: Secondary victim (moderate/constrained) — bear disproportionate environmental costs while locked into colonial extraction patterns
 *   - High-Income Economies: Primary beneficiary (institutional/arbitrage) — accumulated wealth through high-emission development, now positioned to fund transition without fundamental consumption reduction
 *   - Extractive Industries: Organized beneficiary (organized/arbitrage) — capture boundary framework as market opportunity through carbon pricing, green credentials, and sustainability premium markets
 *   - Developing Nations: Constrained player (powerful/constrained) — navigate between boundary compliance and development rights, structurally disadvantaged by framework design
 *   - Environmental NGO Coalition: Organized advocate (organized/constrained) — push for genuine boundary compliance through sunset logic: renewable transitions, circular economy, regenerative systems
 *   - International Governance Bodies: Institutional theater maintainer (institutional/arbitrage) — manage performative compliance (COP meetings, pledges, agreements) while lacking enforcement mechanisms
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes hybrid coordination-extraction structure and identifies key ambiguities in boundary equity and enforcement credibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(planetary_boundaries, 0.58).
domain_priors:suppression_score(planetary_boundaries, 0.65).
domain_priors:theater_ratio(planetary_boundaries, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(planetary_boundaries, extractiveness, 0.58).
narrative_ontology:constraint_metric(planetary_boundaries, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(planetary_boundaries, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(planetary_boundaries, tangled_rope).
narrative_ontology:human_readable(planetary_boundaries, "Planetary Boundaries Framework").
narrative_ontology:topic_domain(planetary_boundaries, "environmental/economic").

domain_priors:requires_active_enforcement(planetary_boundaries).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(planetary_boundaries, high_consumption_economies).
narrative_ontology:constraint_beneficiary(planetary_boundaries, extractive_industries).
narrative_ontology:constraint_victim(planetary_boundaries, future_generations).
narrative_ontology:constraint_victim(planetary_boundaries, global_south_populations).
narrative_ontology:constraint_victim(planetary_boundaries, ecosystem_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Trapped by current overshoot dynamics with no voice in present policy. Bears full cost of cumulative boundary transgression. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.99.
constraint_indexing:constraint_classification(planetary_boundaries, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GLOBAL SOUTH POPULATIONS (SNARE) — Constrained by resource dependency and limited access to alternative economies. Bear disproportionate extraction through environmental degradation while having limited agency in boundary-setting governance. d≈0.90, f(d)≈1.30, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(planetary_boundaries, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: DEVELOPING NATIONS (TANGLED ROPE) — Constrained by debt, trade requirements, and technology access. Faces boundary constraints as coordination problem (need for sustainable development pathways) but also extracted from through colonial-era boundary definitions that lock in current inequities. d≈0.62, f(d)≈0.82, σ=1.0 → χ≈0.48.
constraint_indexing:constraint_classification(planetary_boundaries, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HIGH-INCOME ECONOMIES (ROPE) — Primary beneficiaries. Experience boundaries as coordination challenge solved through technological substitution and efficiency gains. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(planetary_boundaries, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EXTRACTIVE INDUSTRIES (ROPE) — Organize around the boundaries as coordination frame enabling market capture. Benefit from carbon pricing, offsetting, and 'sustainable resource' premium pricing. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(planetary_boundaries, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ENVIRONMENTAL NGO COALITION (SCAFFOLD) — Organized advocates treating boundaries as temporary coordination problem with technology and policy pathways to compliance. See sunset logic: renewable transitions, circular economy norms, and regenerative agriculture are building exit routes. d≈0.45, f(d)≈0.52, σ=1.2 → χ≈0.36.
constraint_indexing:constraint_classification(planetary_boundaries, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL GOVERNANCE BODIES (PITON) — UN frameworks, IPCC, and multilateral agreements maintain the boundary theater (COP meetings, net-zero pledges, corporate ESG metrics) while structural extraction persists. theater_ratio=0.62 reflects performative commitments disconnected from enforcement mechanisms. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(planetary_boundaries, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — Views boundaries as hybrid coordination (legitimate limit-setting to preserve habitability) AND extraction mechanism (unequal burden distribution, historical responsibility obscured by present-tense framing, technology-solutionism delaying redistribution). d≈0.60, f(d)≈0.78, σ=1.2 → χ≈0.58.
constraint_indexing:constraint_classification(planetary_boundaries, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(planetary_boundaries_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(planetary_boundaries, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(planetary_boundaries, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(planetary_boundaries, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(planetary_boundaries, TR),
    TR >= 0.70.

:- end_tests(planetary_boundaries_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The framework enables persistent extraction despite increasing boundary awareness. High-income economies benefit from carbon-intensive development while pledging future decarbonization; extractive industries capture 'sustainable resource' premium pricing; developing nations face constrained development pathways. The extractiveness has increased from 0.35 to 0.58 as gap between pledge and outcome has widened. Suppression (0.65): High. Significant barriers include: technology requirements and capital costs lock out poorer actors; alternative development pathways (degrowth, redistribution) are politically suppressed; narrative emphasis on individual responsibility masks structural drivers; carbon accounting methodologies obscure Scope 3 emissions and outsourced impacts. Theater ratio (0.62): Moderate-high. Performative components include: net-zero pledges without enforcement; carbon offset markets enabling emission accounting fiction; ESG reporting disconnected from actual impact; COP meetings producing aspirational commitments that don't bind behavior. Legitimate components remain: scientific boundary-setting itself is functional, and some renewable transitions demonstrate real decarbonization. The theater ratio has increased from 0.42 to 0.62 as governance discourse has expanded while structural outcomes have stagnated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival divergence. Future generations and global south see a snare (trapped, high extraction, no exit). Developing nations see tangled rope (mixed coordination and extraction). High-income economies and extractive industries see rope (legitimate coordination challenge). Environmental advocates see scaffold (temporary problem with technological sunset). Governance bodies see piton (performative ritual disconnected from function). The analytical observer sees tangled rope with elevated extraction due to equity gaps. The gap is not measurement error — it reflects genuine structural position differences. An agent with arbitrage exit (wealthy economy) experiences different effective constraints than an agent with trapped exit (future generations). The framework legitimizes this divergence by treating boundaries as neutral science, obscuring how boundary allocation embeds historical inequities and concentrated decision-making power.
 *
 * DIRECTIONALITY LOGIC:
 *   High-income economies: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries. Can fund transitions, access premium markets for green technology, delay contraction through offsets. Extractive industries: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Capture boundary frame as market opportunity (carbon pricing, green credentials, sustainability premiums). Developing nations: Victim + constrained → d≈0.62, f(d)≈0.82. Trapped between boundary limits and development aspiration; cannot exit without permission/subsidy from wealthy economies. Global south populations: Victim + constrained → d≈0.90, f(d)≈1.30. Highest extraction — bear environmental costs of both their own development AND outsourced impacts from wealthy economies. Future generations: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — no voice in present policy, will inherit transgressed system. Environmental NGO coalition: Organized + constrained → d≈0.45, f(d)≈0.52. Has agency and advocates for pathways, but constrained by political economy. International governance: Beneficiary + arbitrage (institutional perspective) → d≈0.05, f(d)≈-0.12. Theater maintenance serves institutional interests even if not benefiting directly.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY: The planetary boundaries framework is presented as neutral science establishing natural limits, but the indexical classification reveals embedded extraction and power asymmetries. The framework sets boundaries based on Earth system science (legitimate coordination framing — 'safe operating space for humanity') but does NOT specify how boundaries should be allocated across nations or actors. This omission enables extraction: wealthy nations interpret boundaries as AVERAGE limits, positioning themselves as compliant while outsourcing impacts; poorer nations face STRICT interpretation as limits on their own activities. The analytical observer (perspective 8) correctly identifies this as tangled rope with elevated extraction (d≈0.60), not pure rope. The mandatrophy resolution is: (1) acknowledge that boundary-setting is coordination (rope/scaffold elements are real — biophysical limits do exist and require coordination), BUT (2) recognize that boundary ALLOCATION is a distribution mechanism with extractive elements (snare/tangled_rope), and (3) make allocation design explicit rather than hidden in 'neutral science' framing. The current framework satisfies mandatrophy only if interpreted as implicit support for per-capita equity in boundary allocation — but this is not the framework's stated position, making the current instantiation a mandatrophy FAILURE. A compliant version would either: (a) explicitly allocate boundaries with historical responsibility weighting (moving classification toward scaffold for developing nations), or (b) acknowledge and formalize the inequitable allocation (moving to snare classification for powerless actors but with transparent extraction structures rather than naturalized science framing).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_equity_threshold,
    'What historical baseline defines ''fair'' boundary allocation — pre-industrial emissions, equal per-capita current allocation, or responsibility-weighted based on cumulative historical emissions?',
    'Analysis of competing allocation frameworks (per-capita equity, cumulative responsibility, capability-based distribution); examination of which framework is embedded in major climate agreements',
    'If pre-industrial baseline: current high-income economies are in massive overshoot and should face rapid contraction (snare classification stronger). If equal per-capita: developing nations must be subsidized or face extraction (tangled_rope confirmed). If responsibility-weighted: historical emitters fund transition for others (scaffold pathway possible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_equity_threshold, preference, 'How boundary allocations should account for historical responsibility').

omega_variable(
    technology_substitution_feasibility,
    'Can renewable transitions, precision agriculture, and circular economy systems actually decarbonize at the scale and speed required to achieve boundary compliance without absolute contraction of material throughput?',
    'Empirical analysis of renewable energy scaling rates, land-use requirements for green agriculture, material recycling recovery rates; comparison with boundary transgression timelines',
    'If feasible: scaffold pathway is real, high-income economies genuinely experience as coordination problem. If infeasible: high-income economies are trapped in extraction by biophysical limits, classification shifts toward snare for powerless actors and rope-to-snare for wealthy beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_substitution_feasibility, empirical, 'Whether technology can achieve decarbonization without material contraction').

omega_variable(
    enforcement_mechanism_credibility,
    'Do current climate governance mechanisms (COP commitments, national NDCs, corporate pledges, carbon markets) actually constrain behavior or primarily serve as performative legitimation?',
    'Tracking of emission trajectories against pledge timelines; analysis of carbon market effectiveness, offset accounting accuracy, and compliance enforcement rates',
    'If credible: boundaries function as genuine coordination with enforcement (move toward rope classification). If performative: boundaries function as theater masking extraction (piton classification strengthens, snare confirmed for powerless agents).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_credibility, empirical, 'Whether governance mechanisms actually enforce boundary compliance').

omega_variable(
    ecosystem_tipping_point_irreversibility,
    'Are some boundary transgressions (Amazon dieback, ice sheet collapse, coral system collapse) point-of-no-return even if emissions subsequently decline, or are there restoration pathways?',
    'Paleoclimate analysis of recovery timescales after past excursions; modeling of system resilience and hysteresis in Earth system processes',
    'If irreversible: boundaries are immutable natural law (mountain classification), and transgression represents permanent extraction from future generations. If reversible: extraction is temporary (scaffold classification valid), and restoration represents exit pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_tipping_point_irreversibility, empirical, 'Whether ecosystem collapse from boundary transgression is reversible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(planetary_boundaries, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pb_tr_t0, planetary_boundaries, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pb_tr_t10, planetary_boundaries, theater_ratio, 10, 0.55).
narrative_ontology:measurement(pb_tr_t20, planetary_boundaries, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(pb_be_t0, planetary_boundaries, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pb_be_t10, planetary_boundaries, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(pb_be_t20, planetary_boundaries, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(planetary_boundaries, global_infrastructure).
narrative_ontology:affects_constraint(planetary_boundaries, carbon_budget_governance).
narrative_ontology:affects_constraint(planetary_boundaries, biodiversity_tipping_point).
narrative_ontology:affects_constraint(planetary_boundaries, agricultural_land_system).
narrative_ontology:affects_constraint(planetary_boundaries, freshwater_allocation).
narrative_ontology:affects_constraint(planetary_boundaries, nitrogen_phosphorus_cycles).
narrative_ontology:affects_constraint(planetary_boundaries, ocean_acidification).
narrative_ontology:affects_constraint(planetary_boundaries, chemical_accumulation).
narrative_ontology:affects_constraint(planetary_boundaries, ozone_layer_depletion).
narrative_ontology:affects_constraint(planetary_boundaries, aerosol_atmospheric_loading).

% DUAL FORMULATION NOTE:
% Planetary boundaries decompose into two structural claims: (1) Earth system boundaries are real biophysical limits (ε≈0.05-0.15, near-mountain), (2) Global allocation of boundary space is an extraction mechanism with embedded historical inequities (ε≈0.58, tangled_rope). The framework conflates these as a single 'safe operating space' claim. The network represents actual causal dependencies among the nine boundary processes, but the constraint story here captures the GOVERNANCE structure (how boundaries are allocated and enforced), not the Earth system coupling. Each individual boundary process (carbon, biodiversity, nitrogen, etc.) would have its own story with lower ε values reflecting biophysical necessity; the planetary_boundaries story captures the meta-level extraction that occurs through unequal allocation and performative governance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(planetary_boundaries, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
