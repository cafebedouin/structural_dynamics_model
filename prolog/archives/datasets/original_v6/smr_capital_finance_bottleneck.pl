% ============================================================================
% CONSTRAINT STORY: smr_capital_finance_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_smr_capital_finance_bottleneck, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: smr_capital_finance_bottleneck
 *   human_readable: SMR Capital Finance Bottleneck
 *   domain: energy/finance/infrastructure
 *
 * SUMMARY:
 *   The SMR capital finance bottleneck represents a structural asymmetry
 *   between the technical maturity of small modular reactor designs and the
 *   capital/regulatory infrastructure required to deploy them at scale. This
 *   constraint exhibits coordination benefits (regional utilities genuinely
 *   need distributed generation) alongside extraction mechanisms (incumbent
 *   utilities capture gatekeeping rents through financing control and
 *   regulatory complexity). The bottleneck is neither purely a market failure
 *   (venture capital exists but views SMR risk as unsuitable) nor purely an
 *   incumbent conspiracy, but a hybrid that serves incumbent interests while
 *   obstructing deployment. The extractiveness trajectory shows accumulation:
 *   early-stage SMRs (2015-2018) faced capital constraints as a bottleneck
 *   but treated them as solvable via technology maturation and cost
 *   reduction. By 2024, the constraint has become extractive — capital
 *   requirements have *increased* due to regulatory scope-creep and
 *   supply-chain localization mandates, creating a deepening gap between
 *   venture funding and deployment needs. Theater ratio has risen as
 *   regulatory approval processes have elaborated without proportional
 *   verification gains, creating the appearance of risk management while
 *   gatekeeping remains the primary function.
 *
 * KEY AGENTS:
 *   - SMR Startups (powerless/trapped): Developers lacking capital access and proven-plant references; face dilution and regulatory delays with no alternative pathways
 *   - Regional Utilities and Municipals (moderate/constrained): Genuine beneficiaries of distributed generation but face incumbent utility gatekeeping and financing constraints
 *   - Incumbent Nuclear Utilities (institutional/arbitrage): Primary beneficiaries; control capital access and regulatory relationships that maintain their centrality to energy infrastructure
 *   - Large Reactor Manufacturers (institutional/arbitrage): Secondary beneficiaries; benefit from continued large-reactor focus and licensing complexity that favors incumbent supply chains
 *   - International Climate Finance (organized/constrained): Coalition actors building alternative financing (blended finance, concessional capital, climate funds) designed to bypass incumbent control
 *   - Nuclear Regulators (institutional/arbitrage): Maintain performative review infrastructure with high theater ratio; real safety gatekeeping mixed with regulatory inertia
 *   - Decarbonization Transition (powerless/trapped): Abstract collective victim; deployment delays undermine climate targets with no self-correction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(smr_capital_finance_bottleneck, 0.58).
domain_priors:suppression_score(smr_capital_finance_bottleneck, 0.68).
domain_priors:theater_ratio(smr_capital_finance_bottleneck, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(smr_capital_finance_bottleneck, extractiveness, 0.58).
narrative_ontology:constraint_metric(smr_capital_finance_bottleneck, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(smr_capital_finance_bottleneck, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(smr_capital_finance_bottleneck, tangled_rope).
narrative_ontology:human_readable(smr_capital_finance_bottleneck, "SMR Capital Finance Bottleneck").
narrative_ontology:topic_domain(smr_capital_finance_bottleneck, "energy/finance/infrastructure").

domain_priors:requires_active_enforcement(smr_capital_finance_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(smr_capital_finance_bottleneck, incumbent_nuclear_utilities).
narrative_ontology:constraint_beneficiary(smr_capital_finance_bottleneck, large_reactor_manufacturers).
narrative_ontology:constraint_beneficiary(smr_capital_finance_bottleneck, traditional_utility_financing_structures).
narrative_ontology:constraint_victim(smr_capital_finance_bottleneck, smr_developers).
narrative_ontology:constraint_victim(smr_capital_finance_bottleneck, distributed_energy_deployment).
narrative_ontology:constraint_victim(smr_capital_finance_bottleneck, decarbonization_pace).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMR STARTUP DEVELOPER (SNARE) — Trapped by capital requirements exceeding venture funding thresholds and inability to finance projects until operational. No exit: without utility-scale deployment capital, the developer cannot build prototypes that prove feasibility; without proven feasibility, they cannot access capital. Experiences full extraction: forced to accept dilutive financing terms, regulatory delays, and market-gatekeeping by incumbent utilities who control grid access and public acceptance.
constraint_indexing:constraint_classification(smr_capital_finance_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL UTILITIES / MUNICIPALS (TANGLED ROPE) — Genuinely benefit from SMR coordination: distributed generation reduces transmission bottlenecks and improves resilience. But highly constrained by regulatory framework, balance-sheet financing requirements, and incumbent utility gatekeeping. Faces moderate extraction through forced reliance on large-reactor financing structures and incumbent-controlled supply chains. Some agency — can lobby for regulatory change — but costs are high.
constraint_indexing:constraint_classification(smr_capital_finance_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT NUCLEAR UTILITIES (ROPE) — Primary beneficiaries. Coordinate capital access through established relationships with project finance, regulatory predictability, and utility-scale financing infrastructure. The constraint (high capital requirements, regulatory complexity) maintains their gatekeeping function and enables them to extract rents from new entrants. Arbitrage exit: can deploy capital globally, shift between technology bases, or lobby for regulatory changes. Net beneficiary.
constraint_indexing:constraint_classification(smr_capital_finance_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL CLIMATE TRANSITION COALITION (SCAFFOLD) — Organized actors (IEA, national climate targets, international finance institutions) see SMR deployment as a temporary coordination problem with a sunset: innovative financing structures (blended finance, concessional capital, risk guarantees, technology-specific development banks) are designed to bridge the capital gap. The constraint is temporary — alternative financing pathways (government backing, international climate funds) are explicitly designed to replace traditional utility-controlled capital gatekeeping. Sunset estimated at 10-15 years as SMR technology matures and regulatory frameworks harmonize.
constraint_indexing:constraint_classification(smr_capital_finance_bottleneck, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY APPROVAL THEATER (PITON) — Nuclear regulatory frameworks are highly conservative by design (safety requirement) but have accumulated substantial performative overhead: licensing timelines, standardization demands, and review layers that far exceed technical necessity in many cases. Theater ratio is high because much of the regulatory burden reflects institutional inertia and risk aversion rather than actual safety gaps. Regulatory bodies maintain these review processes through institutional entrenchment, not because they verify novel SMR designs at rates proportional to cost. The piton classification reflects degraded functional verification relative to procedural elaboration.
constraint_indexing:constraint_classification(smr_capital_finance_bottleneck, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / THERMODYNAMIC SCALE LIMITS (MOUNTAIN) — From a civilizational perspective, some capital concentration in energy infrastructure is inherent: high-consequence safety systems require significant redundancy, regulatory oversight, and supply-chain certification costs. These create economic thresholds that cannot be fully eliminated. However, the extent of the bottleneck is not set by thermodynamics — it reflects institutional choices (financing structure, regulatory risk allocation, incumbent gatekeeping) that are contingent, not natural. The engine will identify this as a false summit: naturalizing institutional arrangements as immutable laws.
constraint_indexing:constraint_classification(smr_capital_finance_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(smr_capital_finance_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(smr_capital_finance_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(smr_capital_finance_bottleneck, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(smr_capital_finance_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(smr_capital_finance_bottleneck, TR),
    TR >= 0.70.

:- end_tests(smr_capital_finance_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The capital gap has grown from a technical/economic problem (early 2010s) to a structural gatekeeping mechanism by 2024. SMR developers face dilutive financing (equity stakes 30-50% above market rates), regulatory delays (licensing 3-5x longer than large reactors despite comparable safety risk), and market access constraints imposed by incumbent utilities. The extraction is not maximal (0.72+) because alternative financing pathways are maturing (climate funds, green banks, blended finance) and some SMR projects (NuScale in Idaho, UK SMR in Rolls-Royce) are reaching government backing. Suppression (0.68): High. Barriers to capital access include: venture investors' risk aversion to nuclear, regulatory complexity creating 10+ year licensing timelines, incumbent utility gatekeeping through grid access and public acceptance control, balance-sheet financing requirements excluding startups, and concentrated financing expertise in large-project space. Suppression is not total (0.85+) because some government-backed pathways exist and several SMR projects have achieved financing. Theater ratio (0.64): Moderate-high. Nuclear regulatory process includes genuine safety verification but also substantial performative overlay: regulatory review layers that far exceed technical necessity, standardization demands that reflect risk aversion rather than safety requirement, licensing timelines driven by institutional capacity rather than technical complexity. Theater has increased over interval because regulatory frameworks have not streamlined despite accumulated SMR design experience.
 *
 * PERSPECTIVAL GAP:
 *   Incumbent utility sees rope (coordination); SMR startup sees snare (extraction). Regional utility sees tangled rope (mixed benefits and costs). Climate coalition sees scaffold (temporary with sunset). Regulator sees piton (degraded function, maintained by inertia). Analytical observer risks false summit (mountain) — naturalizing incumbent financing control as immutable capital requirement. The gap reveals that the same financing structure solves a genuine coordination problem (matching capital to large infrastructure projects) while extracting from entrants who cannot access the coordinating mechanism. This is the signature of tangled rope: real coordination function paired with asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent utilities benefit from the constraint (low d, negative chi) — gatekeeping rents subsidize their financing costs. SMR developers bear costs (high d, high chi) — trapped without alternative capital and regulatory pathways. The directionality pipeline derives d from: (1) structural position (beneficiary vs. victim), (2) power level (institutional vs. powerless), (3) exit options (arbitrage vs. trapped). Incumbents have arbitrage (can deploy capital elsewhere, lobby regulatory change, shift technology preferences); SMR startups have trapped (no alternative financing, regulatory credibility, or market access). Organized climate coalition has constrained exit (can pressure governments but cannot unilaterally bypass incumbent gatekeeping). The suppression metric (0.68) is raw structural property — independent of observational perspective — reflecting actual barriers to alternative capital access: risk aversion, regulatory complexity, incumbent control of grid and public acceptance.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint is legitimately tangled rope (not pure snare mislabeled as rope). Genuine coordination exists: the financing structure does efficiently channel capital to large-scale nuclear projects and solves real problems of capital concentration in high-consequence infrastructure. But the coordination function is asymmetrically distributed — it benefits incumbents and excludes entrants through mechanisms (regulatory complexity, venture risk aversion, incumbent gatekeeping) that are not inherent to capital efficiency but contingent on incumbent control. The mandatrophy resolves by showing that both the rope (coordination) and snare (extraction) perspectives are structurally real. The analytical observer should NOT flatten this into either 'the financing system is just efficient coordination' (false simplification) or 'the financing system is pure gatekeeping' (false maximization). The constraint's actual structure is hybrid: efficient for those inside the mechanism, extractive for those excluded from it. Climate finance alternatives (scaffold perspective) offer a real exit pathway by creating parallel financing mechanisms that bypass incumbent gatekeeping, lowering the theater ratio through more transparent allocation and standardized risk frameworks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    smr_capital_threshold_necessity,
    'How much of the capital requirement gap is inherent to SMR physics/safety vs. contingent on incumbent utility financing models?',
    'Comparative cost analysis: SMRs financed via public balance sheets (China, Russia) vs. private venture capital (US, UK). Decomposition of licensing, supply-chain certification, and balance-sheet requirements into necessity categories.',
    'If >70% inherent: constraint approaches mountain classification. If <50% inherent: capital gap is primarily institutional gatekeeping (snare deepens). Mid-range triggers scaffold pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smr_capital_threshold_necessity, empirical, 'Necessity vs. contingency of capital threshold').

omega_variable(
    regulatory_learning_curve_sufficiency,
    'Will SMR regulatory frameworks achieve learning curve cost reductions (licensing streamlining, standardized design approval) at rates comparable to solar/wind prior technology scaling, or will nuclear conservatism prevent learning?',
    'Cost and timeline data for first vs. fifth SMR licensing; comparison of licensing cost per unit to solar/wind scaling curves; forward modeling of regulatory efficiency gains.',
    'If learning curve mature in 10 years: scaffold sunset is real, financing bottleneck dissolves. If learning curve stalls: permanent regulatory drag sustains extraction mechanism and extends snare classification indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_learning_curve_sufficiency, empirical, 'Whether nuclear regulatory learning occurs at sufficient pace').

omega_variable(
    alternative_financing_ecosystem_viability,
    'Can climate finance institutions (green banks, concessional capital, government guarantees) genuinely substitute for private utility capital, or does SMR deployment ultimately require incumbent utility balance sheets?',
    'Historical comparison to other energy transitions (renewable subsidies, grid infrastructure); technical analysis of SMR''s technical/regulatory demands vs. capital structure innovations that have solved comparable infrastructure bottlenecks.',
    'If viable alternatives exist: scaffold classification confirmed, sunset real. If alternatives insufficient: bottleneck is structural, snare persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_financing_ecosystem_viability, empirical, 'Viability of alternative financing to substitute for incumbent utility capital').

omega_variable(
    smr_deployment_pace_counterfactual,
    'What would SMR deployment pace be absent the capital bottleneck? (Counterfactual necessary to measure extraction magnitude.)',
    'Expert elicitation on technical readiness timelines; comparison to analogous technologies (advanced batteries, offshore wind) that faced similar financing constraints; modeling of deployment under hypothetical alternative financing structures.',
    'If counterfactual pace significantly faster: extraction is real and measurable (snare/tangled rope confirmed). If pace similar: bottleneck reflects demand constraints or technical immaturity, not extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(smr_deployment_pace_counterfactual, conceptual, 'Counterfactual deployment pace absent capital bottleneck').

omega_variable(
    incumbent_utility_gatekeeping_intentionality,
    'Is incumbent utility gatekeeping intentional extraction strategy or rational risk aversion to novel technology?',
    'Documentary evidence: utility internal strategy documents, regulatory filings, licensing objections. Behavioral analysis: do utilities gate SMRs more aggressively than equivalent novel LNG or fossil gas technologies? Game-theoretic analysis: what are utilities'' true payoffs from SMR deployment vs. non-deployment?',
    'If intentional extraction: snare deepens, organized resistance likely. If rational risk aversion: constraint is coordination problem (tangled rope), negotiation pathways exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_utility_gatekeeping_intentionality, empirical, 'Whether incumbent gatekeeping is intentional extraction or rational risk aversion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(smr_capital_finance_bottleneck, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smr_cap_tr_t0, smr_capital_finance_bottleneck, theater_ratio, 0, 0.52).
narrative_ontology:measurement(smr_cap_tr_t5, smr_capital_finance_bottleneck, theater_ratio, 5, 0.6).
narrative_ontology:measurement(smr_cap_tr_t10, smr_capital_finance_bottleneck, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(smr_cap_be_t0, smr_capital_finance_bottleneck, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(smr_cap_be_t5, smr_capital_finance_bottleneck, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(smr_cap_be_t10, smr_capital_finance_bottleneck, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(smr_capital_finance_bottleneck, resource_allocation).
narrative_ontology:boltzmann_floor_override(smr_capital_finance_bottleneck, 0.18).
narrative_ontology:affects_constraint(smr_capital_finance_bottleneck, nuclear_regulatory_complexity).
narrative_ontology:affects_constraint(smr_capital_finance_bottleneck, utility_scale_grid_infrastructure).
narrative_ontology:affects_constraint(smr_capital_finance_bottleneck, climate_finance_institutional_maturity).

% DUAL FORMULATION NOTE:
% SMR capital bottleneck decomposes into three structurally distinct constraints: (1) capital_concentration_requirement (inherent to high-consequence infrastructure, lower epsilon), (2) incumbent_utility_gatekeeping (contingent institutional arrangement, higher epsilon), and (3) regulatory_licensing_complexity (mixed natural requirement and performative inertia, epsilon 0.35-0.45). This story aggregates all three; decomposition into separate stories may improve analytical precision by isolating extractiveness drivers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(smr_capital_finance_bottleneck, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
