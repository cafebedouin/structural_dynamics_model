% ============================================================================
% CONSTRAINT STORY: moores_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_moores_law, []).

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
 *   constraint_id: moores_law
 *   human_readable: Moore's Law as an Industrial Convention
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Moore's Law, Gordon Moore's 1965 observation that the number of
 *   transistors on a microchip doubles approximately every two years, has
 *   evolved from an empirical regularity into an industry coordination
 *   mechanism and extraction regime. The constraint exhibits structural
 *   features of a tangled rope: it serves a genuine coordination function
 *   (synchronized investment, standardized roadmaps, predictable ecosystem
 *   planning) while simultaneously extracting costs from rivals, smaller
 *   competitors, and physical materials. From the perspective of dominant
 *   semiconductor manufacturers (TSMC, Samsung, Intel), Moore's Law is a
 *   coordination tool that benefits them through capital scale and
 *   first-mover advantage. From the perspective of smaller manufacturers and
 *   materials science, it is a snare: a pacing requirement that cannot be
 *   escaped without market death, enforced by customer expectations and
 *   analyst projections. The theater ratio has risen from 0.15 (1965-2005,
 *   when the doubling was largely descriptive) to 0.58 (2020+), reflecting
 *   that firms now announce Moore's Law pacing regardless of actual process
 *   node improvements, creating a performative aspect where the cultural
 *   narrative outpaces physical progress.
 *
 * KEY AGENTS:
 *   - Dominant Manufacturers (TSMC, Samsung, Intel): Primary beneficiary (institutional/arbitrage) — capture competitive advantage from process leadership; benefit from industry-wide coordination around Moore's Law roadmaps
 *   - Smaller Semiconductor Firms: Primary victim (powerless/trapped) — locked into Moore's Law pacing by market expectations; cannot opt out without losing relevance; R&D costs prohibitive
 *   - Materials Science and Physics: Primary victim (powerless/trapped) — physical constraints cannot exit; bears cost of accelerating toward quantum tunneling and heat limits
 *   - Systems Integrators and ODMs: Secondary actor (moderate/constrained) — constrained by Moore's Law pacing but benefit from synchronized ecosystem; partial agency through alternative workload distribution
 *   - Technology Ecosystem (software, application developers): Secondary beneficiary (institutional/arbitrage) — benefits from predictable performance improvement; can arbitrage between generations
 *   - Analyst Community and Industry Culture: Institutional enforcer (institutional/arbitrage) — maintains Moore's Law narrative; projects adherence as market expectation; benefits from the coordination role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(moores_law, 0.52).
domain_priors:suppression_score(moores_law, 0.48).
domain_priors:theater_ratio(moores_law, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(moores_law, extractiveness, 0.52).
narrative_ontology:constraint_metric(moores_law, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(moores_law, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(moores_law, tangled_rope).
narrative_ontology:human_readable(moores_law, "Moore's Law as an Industrial Convention").
narrative_ontology:topic_domain(moores_law, "technological/economic").

domain_priors:requires_active_enforcement(moores_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(moores_law, semiconductor_manufacturers).
narrative_ontology:constraint_beneficiary(moores_law, technology_product_ecosystems).
narrative_ontology:constraint_victim(moores_law, rival_chip_design_methodologies).
narrative_ontology:constraint_victim(moores_law, manufacturing_cost_sustainability).
narrative_ontology:constraint_victim(moores_law, materials_science_constraints).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATERIALS SCIENCE CONSTRAINTS (SNARE) — Physical limits on transistor density, power dissipation, and quantum tunneling are absolute. The materials science floor cannot exit the Moore's Law timetable; it bears the cost of accelerating toward fundamental barriers. No alternative pathway; extraction enforced by physics.
constraint_indexing:constraint_classification(moores_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALLER MANUFACTURERS (SNARE) — Locked into Moore's Law pacing by market expectations and ecosystem demands. Cannot opt out without losing market share. R&D costs for next-node development are prohibitive; trapped in an escalating extraction cycle where only capital-rich firms survive.
constraint_indexing:constraint_classification(moores_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SYSTEMS INTEGRATORS AND ODMS (TANGLED ROPE) — Constrained by Moore's Law pacing but also benefit from predictable roadmaps and ecosystem standardization. Coordination function (shared technology roadmaps) coexists with extraction (forced investment in new designs every product cycle). Moderate agency but structural dependence.
constraint_indexing:constraint_classification(moores_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DOMINANT MANUFACTURERS (ROPE) — Primary beneficiary. Moore's Law coordinates industry expectations, justifies capital expenditure, and creates competitive advantage for firms with process leadership. Benefits from the predictability and the coordination function; experiences low extraction cost due to capital scale and arbitrage options.
constraint_indexing:constraint_classification(moores_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PRODUCT ECOSYSTEM (ROPE) — Software, application design, and consumer expectations are coordinated around Moore's Law pacing. Benefits from predictable performance improvement curves; experiences the constraint as enabling coordination rather than extractive. Can arbitrage between chip generations and optimize for cost/performance tradeoffs.
constraint_indexing:constraint_classification(moores_law, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MOORE'S LAW AS CULTURAL ARTIFACT (PITON) — The prediction itself has become performative. The original observation (an empirical regularity from 1965-2005) is now a self-fulfilling prophecy maintained by industry culture and analyst expectations. Theater ratio high: firms announce adherence to Moore's Law pacing regardless of physical progress, creating a disconnect between the performative claim and underlying physics. The ritual persists through institutional inertia and narrative momentum.
constraint_indexing:constraint_classification(moores_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL LIMITS (MOUNTAIN) — From a universal/civilizational perspective, some transistor density doubling is inherent to manufacturing progress: physical laws set hard limits on scaling, and the gap between optimistic pacing and actual capability is structural. This perspective risks naturalizing contingent industrial coordination as an immutable law. The false summit detector should flag this perspective as naturalization.
constraint_indexing:constraint_classification(moores_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moores_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(moores_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moores_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(moores_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(moores_law, TR),
    TR >= 0.70.

:- end_tests(moores_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The constraint creates asymmetric costs: dominant manufacturers invest heavily but recoup benefits through market share and pricing power. Smaller firms face existential pressure. Materials science faces physical barriers with no escape. The extractiveness has increased from ~0.25 (1965-2005, when doubling was largely descriptive) to 0.52 (2020+) as the pacing has become prescriptive and the physical bottlenecks have tightened. Suppression (0.48): Moderate. Firms cannot easily exit Moore's Law commitments without losing investor confidence and market position. But suppression is not total — some firms have chosen specialty markets (power efficiency, cost optimization) that decouple from Moore's Law pacing. The trend toward alternative architectures (AI accelerators, specialized processors) is gradually reducing suppression for some segments. Theater ratio (0.58): Elevated and rising. From 1965-2005, Moore's Law was primarily descriptive. Since 2015, firms announce adherence to Moore's Law roadmaps while actual node transitions slip or show smaller increments. The performative aspect intensified with EUV delays, chiplet strategies, and process maturation. Theater has become the dominant mechanism for maintaining the convention as physical scaling slows.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival inversion. Dominant manufacturers perceive Moore's Law as a coordination benefit (Rope) — it synchronizes supply chains, justifies capital expenditure, and creates competitive advantage. Smaller manufacturers perceive it as a snare (Snare) — they are locked into a pacing requirement they cannot afford. Materials science perceives it as a fundamental limit (Mountain or Snare, depending on whether the physicist believes the scaling will continue). The analyst community perceives it as a cultural performance (Piton) — the narrative persists through institutional momentum even as the underlying physics slows. Systems integrators perceive it as mixed coordination and constraint (Tangled Rope) — the roadmap enables planning but forces constant redesign. The product ecosystem perceives it as enabling (Rope) — predictable performance curves enable long-term software planning. The civilizational analytical observer risks naturalizing this as an immutable law (false summit in Mountain classification).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from each agent's position in the extraction flow. Dominant manufacturers are beneficiaries with arbitrage options (exit: migrate to specialty markets or alternative architectures, but low incentive). Their d approaches 0.0 (full beneficiary); f(d) is negative, producing negative effective extraction — they experience the constraint as coordination benefit. Smaller manufacturers are victims with constrained options (exit: specialty markets, but costly; exit: exit industry). Their d approaches 0.95 (near-full target); f(d) is 1.42, producing high experienced extraction. Materials science is powerless and trapped (exit: none). Their d approaches 1.0; f(d) is 1.42. Systems integrators are moderate agents with some mobility (exit: workload distribution, alternative architectures). Their d ~0.55; f(d) ~0.75, producing moderate experienced extraction. The analytical observer's d ~0.73 (observer position); f(d) ~1.15, producing observer-level chi. Scope modifier σ(S) = 1.2 (global) amplifies these values.
 *
 * MANDATROPHY ANALYSIS:
 *   Moore's Law resolves the mandatrophy by revealing the constraint's hybrid nature. It appears as pure extraction (Snare) to powerless agents locked into a pacing timetable they cannot escape. It appears as coordination (Rope) to dominant beneficiaries who use it to synchronize ecosystems. It appears as mixed (Tangled Rope) to moderate agents constrained but not broken by it. The Piton classification (theater ratio 0.58) indicates the performative aspect is rising — the cultural narrative now carries more weight than the underlying physics. The false summit (Mountain classification from the analytical observer) reveals the risk of naturalizing a contingent industry convention as a law of nature. The mandatrophy is resolved by recognizing that Moore's Law is NOT a single constraint but a network of three structurally distinct constraints: (1) the physical doubling rate of transistor density (base_extractiveness ~0.25-0.30, Mountain or Rope depending on whether it's inevitable or coordinated), (2) the industry convention of pacing technology releases every 2 years (base_extractiveness ~0.50, Tangled Rope or Snare depending on perspective), and (3) the performance expectation set by analyst projections and marketing (base_extractiveness ~0.45, Piton with high theater). The current story models the constraint at the industry convention level (level 2), which is the most policy-relevant and the locus of actual extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_scaling_limit_timeline,
    'What timeline before physical barriers (quantum tunneling, power dissipation, heat management) halt transistor density doubling?',
    'Empirical research on sub-nanometer scaling limits, power wall studies, and quantum mechanical tunneling probability curves. Longitudinal tracking of actual die density improvement vs Moore''s Law prediction.',
    'If <5 years: Moore''s Law is already breaking down and snare extraction is accelerating. If 10+ years: the convention has decades of structural validity remaining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_scaling_limit_timeline, empirical, 'Timeline to physical scaling barriers').

omega_variable(
    alternative_computing_paradigms_adoption,
    'Do alternative computing approaches (quantum, neuromorphic, optical, analog) substitute for traditional CMOS scaling and reduce Moore''s Law enforcement power?',
    'Market adoption rates of alternative architectures; correlation between alternative-platform share and competitive pressure on CMOS roadmaps; analysis of workload migration to alternatives vs traditional scaling.',
    'If alternatives gain >20% market share: Moore''s Law shifts from Mountain/Snare to Scaffold or Piton (sunset clause becomes real). If alternatives remain niche: Moore''s Law extraction mechanism persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_computing_paradigms_adoption, empirical, 'Whether alternative paradigms reduce Moore''s Law enforcement').

omega_variable(
    industry_coordination_vs_physical_inevitability,
    'Is the observed doubling from 1965-2005 a description of physical inevitability, an industry coordination mechanism, or both in different periods?',
    'Historical analysis of when Moore''s Law transitions from descriptive observation to prescriptive industry target; examination of R&D allocation changes and marketing emphasis; comparison with semiconductor physics papers.',
    'If coordination mechanism: the constraint is structurally tangled_rope or snare depending on perspective (current classification holds). If physical inevitability: mountain classification is legitimate. If both: the constraint''s ε depends on which period is evaluated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(industry_coordination_vs_physical_inevitability, conceptual, 'Whether Moore''s Law is physical fact or industry convention').

omega_variable(
    capital_concentration_causality,
    'Does Moore''s Law enforcement drive capital concentration in semiconductor manufacturing, or does capital concentration drive adherence to Moore''s Law?',
    'Causal analysis of firm consolidation timelines relative to Moore''s Law announcements; examination of R&D spending curves; comparison of smaller-firm survival rates before/after each node transition.',
    'If Moore''s Law drives concentration: the snare victim classification for smaller manufacturers is correct. If concentration drives Moore''s Law: the causality points to an industry choice to maintain the convention despite alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_concentration_causality, empirical, 'Direction of causality between Moore''s Law and capital concentration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(moores_law, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(moores_tr_t0, moores_law, theater_ratio, 0, 0.15).
narrative_ontology:measurement(moores_tr_t15, moores_law, theater_ratio, 15, 0.38).
narrative_ontology:measurement(moores_tr_t30, moores_law, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(moores_be_t0, moores_law, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(moores_be_t15, moores_law, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(moores_be_t30, moores_law, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(moores_law, global_infrastructure).
narrative_ontology:affects_constraint(moores_law, semiconductor_supply_chain_consolidation).
narrative_ontology:affects_constraint(moores_law, rare_earth_element_concentration).
narrative_ontology:affects_constraint(moores_law, chip_design_complexity_scaling).
narrative_ontology:affects_constraint(moores_law, power_consumption_limits).

% DUAL FORMULATION NOTE:
% Moore's Law as an industrial convention (base_extractiveness 0.52) is downstream of Moore's Law as a physical doubling rate (base_extractiveness ~0.25). These are structurally distinct constraints with different ε values. The physical doubling is increasingly Mountain-like; the industry convention is increasingly Tangled Rope or Piton. Constraint family linking enables separate analysis of the descriptive claim (physics) vs the prescriptive norm (industry).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(moores_law, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
