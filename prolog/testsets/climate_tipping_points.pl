% ============================================================================
% CONSTRAINT STORY: climate_tipping_points
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_tipping_points, []).

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
 *   constraint_id: climate_tipping_points
 *   human_readable: Climate Tipping Points as Irreversible Constraint
 *   domain: climate_physics/geopolitical_economy
 *
 * SUMMARY:
 *   Climate tipping points represent an asymmetric constraint where
 *   high-emission actors (wealthy nations, incumbent energy sectors) extract
 *   career/economic benefits during the accumulation phase (1950-2025) while
 *   locking in irreversible costs for vulnerable populations and future
 *   generations. The constraint exhibits snare properties at the
 *   civilizational scale: once a tipping point is crossed (Atlantic
 *   overturning weakens, Amazon transitions from carbon sink to source,
 *   Arctic permafrost enters positive feedback), the climate system becomes
 *   locked into a new state regardless of subsequent mitigation. Suppression
 *   is structural and extreme (physics permits no negotiation). Theater is
 *   moderate-to-high: climate policy apparatus maintains elaborate reporting
 *   and target-setting while the central constraint (preventing threshold
 *   crossing) remains largely unverified in real time. The extractiveness has
 *   increased over the measurement interval (0.42 to 0.68) as awareness of
 *   tipping point proximity has grown without corresponding policy
 *   acceleration — the constraint has become more transparent but extraction
 *   has hardened rather than resolved. The constraint family decomposes into
 *   distinct physical and political components: (1) the thermodynamic tipping
 *   point dynamics (mountain-adjacent but policy-dependent), (2) the
 *   institutional policy response (piton with high theater), (3) the
 *   distributional extraction between emitters and victims (snare for
 *   powerless, tangled_rope for organized vulnerable nations, rope for
 *   high-emission producers).
 *
 * KEY AGENTS:
 *   - Vulnerable populations (island nations, equatorial regions, subsistence communities): Primary victims (powerless/trapped) — face irreversible habitat loss, resource collapse, forced migration with zero exit capacity
 *   - Future generations: Primary victims (powerless/trapped) — structurally nonexistent at time of emissions; inherit locked-in climate state
 *   - Climate-vulnerable nations: Secondary victims (organized/constrained) — AOSIS, LDC groups have bargaining power but remain constrained by others' emissions; some exit via adaptation funding
 *   - High-emission producers: Primary beneficiaries (institutional/arbitrage) — capture economic benefits during accumulation phase; have exit capacity via energy transition and green tech leadership
 *   - Incumbent energy sectors: Secondary beneficiaries (institutional/arbitrage) — extract rents during transition window; can arbitrage into renewables or disappear
 *   - Climate policy apparatus: Institutional actor (institutional/arbitrage) — maintains governance structures; theater ratio high, verification of tipping-point-prevention function low
 *   - Analytical observer: Civilizational frame (analytical/analytical) — risks naturalizing contingent political failure as immutable physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_tipping_points, 0.68).
domain_priors:suppression_score(climate_tipping_points, 0.72).
domain_priors:theater_ratio(climate_tipping_points, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_tipping_points, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_tipping_points, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_tipping_points, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_tipping_points, snare).
narrative_ontology:human_readable(climate_tipping_points, "Climate Tipping Points as Irreversible Constraint").
narrative_ontology:topic_domain(climate_tipping_points, "climate_physics/geopolitical_economy").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_tipping_points, high_emission_producers).
narrative_ontology:constraint_beneficiary(climate_tipping_points, incumbent_energy_sectors).
narrative_ontology:constraint_victim(climate_tipping_points, vulnerable_populations).
narrative_ontology:constraint_victim(climate_tipping_points, future_generations).
narrative_ontology:constraint_victim(climate_tipping_points, ecosystems_at_threshold).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE POPULATIONS (SNARE) — Island nations, equatorial regions, and subsistence communities face irreversible climate shifts with zero exit capacity. Tipping points lock in extraction (loss of habitability, resource collapse, forced migration) without consent or compensation. Maximum suppression: climate physics brooks no negotiation; adaptive capacity is zero for those already at margins.
constraint_indexing:constraint_classification(climate_tipping_points, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE GENERATIONS (SNARE) — Cannot exit a climate state determined by current emissions. Tipping point lock-in passes irreversible costs forward. Exit options are structurally nonexistent — future agents inherit the constraint without participation in its creation.
constraint_indexing:constraint_classification(climate_tipping_points, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CLIMATE-VULNERABLE NATIONS (TANGLED ROPE) — Organized coalitions (Alliance of Small Island States, Least Developed Countries Group) have some bargaining power and exit options (adaptation funding, loss-and-damage mechanisms) but remain structurally constrained by tipping point trajectories they did not cause. Hybrid: genuine coordination mechanisms exist (Paris Agreement) alongside asymmetric extraction (rich emitters offset rich emitters' carbon, poor nations adapt or perish).
constraint_indexing:constraint_classification(climate_tipping_points, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-EMISSION PRODUCERS (ROPE) — Institutional actors (fossil fuel states, manufacturing hubs, developed economies) experience tipping point constraint as coordination problem: stabilizing atmospheric carbon requires collective action. From their position, the constraint solves the free-rider problem (individual carbon reduction costs, but global climate goods are nonexcludable). They have exit options via arbitrage — shift to renewables, capture green energy markets, offset through carbon finance. Extraction runs toward them during transition: first-mover disadvantage on decarbonization can be offset by leadership in green tech.
constraint_indexing:constraint_classification(climate_tipping_points, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CLIMATE POLICY APPARATUS (PITON) — International climate governance (UNFCCC, IPCC, national climate agencies) maintains high theater ratio: annual climate conferences, emissions reporting, carbon accounting. Functional verification of tipping point prevention is impossible in real time (feedback loops are decadal-scale). The apparatus persists through institutional inertia and reputational maintenance despite low verification of actual tipping point avoidance. Theater: elaborate measurement protocols, nationally-determined contributions, carbon markets. Function: unclear whether bureaucratic activity reduces tipping point risk or merely performs mitigation.
constraint_indexing:constraint_classification(climate_tipping_points, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PHYSICAL TIPPING POINT DYNAMICS (MOUNTAIN) — From the analytical/civilizational frame, tipping points represent physical irreversibilities: once a bifurcation is crossed (Atlantic Meridional Overturning Circulation weakens past threshold, Amazon shifts from sink to source, permafrost carbon release exceeds feedback threshold), the system cannot return to prior state without external forcing exceeding observational capacity. This appears as a natural law: thermodynamics of coupled atmosphere-ocean-biosphere systems. However, engine's false summit detector will flag this — the 'immutability' derives from current policy ineffectiveness and distributional lock-in, not from physics alone. Physics is immutable only if human actions remain constant. The mountain classification naturalizes contingent political failure.
constraint_indexing:constraint_classification(climate_tipping_points, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_tipping_points_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_tipping_points, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_tipping_points, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_tipping_points, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_tipping_points, TR),
    TR >= 0.70.

:- end_tests(climate_tipping_points_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The primary extraction mechanism is temporal: high-emission actors (primarily wealthy nations, 1950-2025) internalized economic benefits of fossil fuels while externalizing climate costs onto future periods and vulnerable geographies. The extraction is asymmetric across power (wealthy/organized vs powerless/unorganized), time (present/past vs future), and space (high-latitude/developed vs equatorial/developing). The 0.68 value reflects that extraction is severe (widespread impact, irreversibility) but not maximal—policy mechanisms and mitigation pathways exist in principle, even if political will is lacking. Suppression (0.72): Very high. Suppression mechanisms are multifaceted: (1) physical (climate physics permits no appeals, negotiations, or equity considerations), (2) epistemic (tipping point uncertainty creates plausible deniability—thresholds are diffuse, transitions stochastic, feedback timescales defy real-time verification), (3) institutional (fossil fuel interests fund climate doubt; policy moves slower than threshold proximity), (4) structural (mitigation requires global coordination with uneven costs; collective action problem is maximally hard). Theater ratio (0.58): Moderate-high. Climate policy maintains substantial performative elements: annual conferences, nationally-determined contributions with weak enforcement, carbon accounting with methodological ambiguity, emissions trading systems that may permit offsets rather than reductions. But theater is not dominant—the constraint's physical reality (ice sheet acceleration, atmospheric CO₂ concentration, ocean heat content) provides constant external verification that bypasses institutional narrative. Theater is declining as evidence becomes undeniable, but policy theater persists because enforcement theater is easier than actual decarbonization.
 *
 * PERSPECTIVAL GAP:
 *   The deepest perspectival gap is between high-emission producers' Rope (coordination problem with shared benefits) and vulnerable populations' Snare (irreversible extraction with zero agency). Both observe the same physical constraint (tipping points), but their structural position within the emissions/climate system determines whether they experience the constraint as enabling (Rope) or catastrophic (Snare). The climate policy apparatus sees its own theater rising while function stagnates (Piton), creating an internal tension between legitimacy maintenance and physical reality. The analytical observer risks the false summit of naturalization—treating contingent political/economic arrangements (emissions-intensive development, delayed policy response, ineffective enforcement) as immutable features of tipping point physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the extraction flow. High-emission producers and incumbent energy sectors have low d values (beneficiaries with arbitrage exit options): d≈0.10-0.15, yielding negative or near-zero χ. They experience the constraint as enabling (coordination solving the free-rider problem, market opportunities in green transition). Vulnerable populations and future generations have high d values (victims with trapped exit options): d≈0.90-1.00, yielding maximum χ. They experience the constraint as maximally extractive—irreversible costs they did not incur. Organized vulnerable nations have moderate-high d values (victims with constrained exit options): d≈0.65-0.75, yielding significant but not maximal χ. Policy apparatus has intermediate d (institutional beneficiary during enforcement, but increasingly constrained by physical reality): d≈0.35-0.45, yielding moderate χ. The analytical observer occupies d≈0.70 (seeing asymmetry across all positions) yielding χ that flags the snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tipping points are not generically 'coordination problems' or 'pure extraction'—they are structurally hybrid, with different agents experiencing the same constraint as fitting different types. The extraction (benefits to high-emitters, costs to vulnerable populations) is real and severe. The coordination (global collective action problem in mitigation) is also real. The constraint is a Snare for those who bear costs without agency (vulnerable populations, future generations). It is a Tangled Rope for those with organized power but constrained exit (vulnerable nations bargaining in climate negotiations). It is a Rope for those with low initial costs and exit capacity (wealthy emitters arbitraging into green tech). The false summit (Mountain classification from physics-only framing) naturalizes the political failure to mitigate—physics per se does not mandate tipping point crossing; policy does. The mandatrophy is resolved by recognizing that the constraint's classification depends on which agent and which time horizon is indexed. At the powerless/generational scale, it is Snare. At the institutional/biographical scale with arbitrage options, it is Rope. The constraint's six-type heterogeneity is not ambiguity; it is the accurate structural picture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tipping_point_threshold_uncertainty,
    'Are empirical tipping point thresholds deterministic bifurcations or stochastic transitions spanning decades?',
    'Paleoclimate reconstruction of past transitions; high-resolution climate model ensembles with structural uncertainty quantification; real-time threshold crossing indicators (deep ocean temperature, ice sheet acceleration, vegetation greenness indices)',
    'If deterministic sharp threshold: constraint is nearly irreversible mountain (ε→0.25, suppression→1.0). If stochastic diffuse transition: constraint permits mitigation within wider window (ε→0.45, suppression→0.55, snare→tangled_rope). Current uncertainty spans this range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tipping_point_threshold_uncertainty, empirical, 'Whether tipping points are sharp deterministic thresholds or gradual stochastic transitions').

omega_variable(
    carbon_cycle_reversibility,
    'Can atmospheric CO₂ removal at scales 10-50 GtCO₂/year be deployed before tipping points are crossed, and if so, can it reverse crossing of already-crossed thresholds?',
    'Scaling analysis of carbon capture technologies (DAC, enhanced weathering, biochar production); cost trajectory modeling; deployment rate constraints from energy/land requirements; paleoclimate evidence on reversal timescales for AMOC, Amazon, permafrost systems',
    'If reversibility feasible: snare becomes tangled_rope (some exit capacity emerges for organized agents with resources). If reversal infeasible: snare classification hardens, tipping point becomes closer to mountain (immutable by policy).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carbon_cycle_reversibility, empirical, 'Whether carbon removal at scale can prevent or reverse tipping point crossing').

omega_variable(
    distributional_responsibility_ambiguity,
    'To what degree do historical cumulative emissions vs. current annual emissions vs. per-capita consumption determine moral responsibility for tipping point lock-in?',
    'Responsibility accounting frameworks (consumption-based vs production-based vs cumulative attribution); analysis of how different allocations shift beneficiary/victim classifications across nations and generations',
    'If historical accumulation dominates: wealthy nations are primary extractors (high-emission-producer snare beneficiary status hardened). If current emissions dominate: middle-income rapidly industrializing nations become primary victims. Shifts which agents experience constraint as rope vs snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_responsibility_ambiguity, preference, 'How to allocate responsibility for emissions driving tipping points').

omega_variable(
    adaptation_sufficiency_boundary,
    'Is there a temperature rise magnitude beyond which adaptation becomes structurally impossible for organized societies and ecosystems?',
    'Scaling analysis of adaptation costs vs GDP for different warming scenarios; threshold analysis of ecosystem collapse points (coral reef extinction, permafrost thaw, crop failure cascades); modeling of resource scarcity (water, arable land) as functions of temperature',
    'If adaptation possible at 2-3°C: constraint becomes tangled_rope for most victims (costly but survivable coordination). If adaptation impossible beyond 1.5°C: constraint hardens to snare for vast populations (extraction becomes existential).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_sufficiency_boundary, empirical, 'Whether adaptation remains possible beyond certain warming thresholds').

omega_variable(
    policy_enforcement_credibility,
    'Can climate policy commitments (Paris Agreement NDCs, net-zero pledges) be enforced at sufficient scales and speeds to prevent tipping points, or do they function primarily as theater?',
    'Gap analysis between stated NDCs and emissions trajectories; tracking of greenwashing vs real decarbonization in corporate/national commitments; modeling of whether current policy stringency puts trajectory on pace for tipping point avoidance',
    'If enforcement credible: piton classification is optimistic (theater declining, function increasing). If enforcement fails: piton is correct diagnosis (institutional apparatus persists as theater while tipping points advance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(policy_enforcement_credibility, empirical, 'Whether climate policy commitments can actually enforce tipping point avoidance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_tipping_points, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tip_tr_t0, climate_tipping_points, theater_ratio, 0, 0.4).
narrative_ontology:measurement(clim_tip_tr_t15, climate_tipping_points, theater_ratio, 15, 0.52).
narrative_ontology:measurement(clim_tip_tr_t30, climate_tipping_points, theater_ratio, 30, 0.58).
narrative_ontology:measurement(clim_tip_tr_t45, climate_tipping_points, theater_ratio, 45, 0.61).

% Extraction over time
narrative_ontology:measurement(clim_tip_be_t0, climate_tipping_points, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_tip_be_t15, climate_tipping_points, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(clim_tip_be_t30, climate_tipping_points, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(clim_tip_be_t45, climate_tipping_points, base_extractiveness, 45, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_tipping_points, global_infrastructure).
narrative_ontology:affects_constraint(climate_tipping_points, fossil_fuel_lock_in).
narrative_ontology:affects_constraint(climate_tipping_points, international_emissions_accountability).
narrative_ontology:affects_constraint(climate_tipping_points, adaptive_capacity_threshold).

% DUAL FORMULATION NOTE:
% Climate tipping points decompose into three structurally distinct constraints: (1) physical tipping point dynamics (mountain-adjacent, determined by paleoclimate bifurcation theory), (2) institutional policy response (piton, theater-driven), (3) distributional extraction (snare/tangled_rope). Each has its own ε and perspectives. The three are linked via network.affects_constraints to capture causal dependency: policy failure to prevent tipping points (piton institutional inertia) preserves fossil fuel lock-in (snare beneficiary status), which shapes international accountability negotiations (tangled_rope between emitters and vulnerable nations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_tipping_points, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
