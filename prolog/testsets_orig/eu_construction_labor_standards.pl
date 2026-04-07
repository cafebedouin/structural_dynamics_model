% ============================================================================
% CONSTRAINT STORY: eu_construction_labor_standards
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_construction_labor_standards, []).

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
 *   constraint_id: eu_construction_labor_standards
 *   human_readable: EU Construction Labor Standards and Wage Extraction
 *   domain: labor_regulation/international_coordination
 *
 * SUMMARY:
 *   EU construction labor standards represent a structural hybrid between
 *   legitimate cross-border labor coordination and extractive wage
 *   suppression of migrant workers. The constraint operates through a formal
 *   framework (EU labor mobility directives, Posted Workers Directive,
 *   minimum wage harmonization targets) that genuinely solves the
 *   coordination problem of skill matching and labor supply across unequal
 *   development zones. Simultaneously, the constraint enables asymmetric
 *   extraction: enforcement gaps between member states, visa restrictions
 *   that trap migrant workers, deliberate regulatory arbitrage through the
 *   PWD loophole, and structural wage suppression relative to native workers
 *   doing identical work. The theater ratio (0.58) reflects that compliance
 *   theater is moderate but increasing — EU standards are publicly enforced
 *   in Western member states while systematically under-enforced in Eastern
 *   states, creating the appearance of uniform labor protection alongside
 *   structural differentiation. The extractiveness trajectory (0.35→0.55 over
 *   the interval) shows accumulation: as the constraint matured post-2004,
 *   beneficiaries (Western firms, unions) increasingly exploited enforcement
 *   gaps and PWD loopholes, while the formal standard remained unchanged.
 *
 * KEY AGENTS:
 *   - Migrant Construction Workers: Primary victims (powerless/trapped) — trapped by visa restrictions, debt, family dependency, and language barriers; face wage suppression and non-enforcement of standards in actual workplace conditions
 *   - Eastern European Labor Supply: Secondary victims (moderate/constrained) — constrained by EU mobility rules and skill certification requirements; subject to regulatory arbitrage via enforcement differential
 *   - Western European Construction Firms: Primary beneficiaries (institutional/arbitrage) — capture differential wages between posted workers and native workers; use PWD to arbitrage enforcement gaps; maintain market position through standards that prevent wage competition
 *   - Unionized Labor Organizations: Secondary beneficiaries (organized/constrained) — defend unionized workers' wage floors through standards; constrained by EU political process but retain significant influence
 *   - EU Regulatory Agencies: Theater maintainers (institutional/arbitrage) — conduct compliance ritual while enforcement varies 4x across member states; maintain appearance of uniform standards
 *   - Analytical Observer: Cross-position analyst (analytical/analytical) — observes simultaneous coordination function (labor market matching across unequal zones) and extraction mechanism (enforcement arbitrage, wage suppression via differential application)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_construction_labor_standards, 0.55).
domain_priors:suppression_score(eu_construction_labor_standards, 0.65).
domain_priors:theater_ratio(eu_construction_labor_standards, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_construction_labor_standards, extractiveness, 0.55).
narrative_ontology:constraint_metric(eu_construction_labor_standards, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(eu_construction_labor_standards, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_construction_labor_standards, tangled_rope).
narrative_ontology:human_readable(eu_construction_labor_standards, "EU Construction Labor Standards and Wage Extraction").
narrative_ontology:topic_domain(eu_construction_labor_standards, "labor_regulation/international_coordination").

domain_priors:requires_active_enforcement(eu_construction_labor_standards).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_construction_labor_standards, western_european_construction_firms).
narrative_ontology:constraint_beneficiary(eu_construction_labor_standards, unionized_labor_organizations).
narrative_ontology:constraint_beneficiary(eu_construction_labor_standards, regulatory_agencies).
narrative_ontology:constraint_victim(eu_construction_labor_standards, migrant_construction_workers).
narrative_ontology:constraint_victim(eu_construction_labor_standards, eastern_european_labor_supply).
narrative_ontology:constraint_victim(eu_construction_labor_standards, labor_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MIGRANT CONSTRUCTION WORKER (SNARE) — Trapped by visa restrictions, debt-financed migration, language barriers, and family dependency on remittances. Cannot exit without losing both income and immigration status. Faces wage suppression relative to native workers, despite identical work. Zero degrees of freedom structurally; maximum experienced extraction.
constraint_indexing:constraint_classification(eu_construction_labor_standards, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: EASTERN EUROPEAN LABOR SUPPLY (TANGLED ROPE) — Constrained by EU mobility rules, skill certification requirements, and regional income differentials. Participates in coordination mechanism (EU labor mobility framework) that legitimately solves skill matching and wage harmonization problems. Also subject to asymmetric extraction via wage depression, deliberate under-enforcement of standards in Eastern member states, and regulatory arbitrage. Mixed experience: genuine coordination benefit alongside significant extraction.
constraint_indexing:constraint_classification(eu_construction_labor_standards, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: WESTERN EUROPEAN CONSTRUCTION FIRMS (ROPE) — Net beneficiaries with arbitrage options. Standards create cost floors that prevent wage competition from low-cost migration, protecting domestic labor costs and profit margins. Firms see the constraint as coordination: the standards allow simultaneous access to lower-wage labor while preventing price wars. They can arbitrage by posting workers from Eastern subsidiaries (Posted Workers Directive creates loophole), capturing differential wages.
constraint_indexing:constraint_classification(eu_construction_labor_standards, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: UNIONIZED LABOR ORGANIZATIONS (ROPE) — Organized beneficiaries with constrained exit (cannot exit EU labor coordination without forfeiting political influence). Standards protect unionized workers' wage floors and prevent undercutting by migrant labor. The constraint solves the collective action problem of wage suppression through immigration. Unions experience the framework as coordination mechanism, not extraction — it serves their interests.
constraint_indexing:constraint_classification(eu_construction_labor_standards, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EU REGULATORY AGENCIES (PITON) — The enforcement apparatus (labor inspectorates, social security oversight) is substantially performative. Standards exist on paper; enforcement varies dramatically across member states (Western enforcement ~80%, Eastern enforcement ~20%). Inspectorates conduct routine audits that firms anticipate and manage through temporary compliance. Theater ratio high because the regulatory ritual persists despite known low functional enforcement. Agencies maintain the appearance of standards without achieving equal application across EU space.
constraint_indexing:constraint_classification(eu_construction_labor_standards, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Cross-position analysis reveals the constraint simultaneously coordinates legitimate labor market matching (solves the skill mismatch and wage equalization problem across unequal development zones) and extracts from migrant workers via regulatory arbitrage, enforcement gaps, and structural immobility. The constraint is NOT a mountain (immutable feature of labor economics) nor pure coordination. It is a hybrid that requires active enforcement to suppress the arbitrage mechanisms that beneficiaries use to capture differential rents.
constraint_indexing:constraint_classification(eu_construction_labor_standards, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_construction_labor_standards_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_construction_labor_standards, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_construction_labor_standards, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_construction_labor_standards, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_construction_labor_standards, TR),
    TR >= 0.70.

:- end_tests(eu_construction_labor_standards_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high, reflecting genuine extraction alongside coordination. The constraint captures differential wages through (a) structural wage suppression of migrants relative to native workers for identical work, (b) enforcement gap exploitation (firms invest in compliance only in high-inspection member states), (c) Posted Workers Directive loophole that allows posting without full labor standard application. But extractiveness is not maximal (≤0.66 snare threshold) because the coordination function is real — the standard does enable labor mobility that would not occur without wage harmonization framework. Suppression (0.65): Moderate-high. Visa restrictions, family dependency on remittances, debt-financed migration, language barriers, and lack of citizenship all create barriers to exit. But suppression is not total — some workers accumulate skills, acquire citizenship, and exit. Theater ratio (0.58): Moderate. Compliance ritual exists but is not the dominant extraction mechanism. Standards are genuinely enforced in Western member states (~80% of firms comply with documented audits). Eastern enforcement (~20%) creates appearance of uniform standards while actual compliance varies. The gap is structural and visible, not purely theatrical — firms and workers alike know enforcement varies by jurisdiction. Claimed type (Tangled Rope) reflects both coordination (beneficiaries + active enforcement gate) and extraction (victims + asymmetric gains).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. Western firms see Rope (coordination mechanism that enables profitable access to skill-matched labor while protecting native wage floors). Migrants see Snare (trapped in low-wage work with no exit). Unions see Rope (protection against wage undercutting). Eastern labor supply sees Tangled Rope (both benefit from EU mobility and suffer from enforcement differential). Regulatory agencies see Piton (performative compliance ritual maintained through inertia). The analytical observer sees Tangled Rope (simultaneous coordination and extraction). The perspectival gap reveals that 'the same labor standard' is experienced as coordination by beneficiaries, as enforcement theater by agencies, and as structural trap by powerless victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from power + exit + beneficiary/victim status. Migrant workers (powerless/trapped/victim) derive d ≈ 0.95, producing maximum f(d) ≈ 1.42, maximum chi. Western firms (institutional/arbitrage/beneficiary) derive d ≈ 0.10, producing f(d) ≈ -0.01, negative chi (they experience subsidy, not extraction). Eastern labor supply (moderate/constrained/victim) derives d ≈ 0.65, producing f(d) ≈ 1.00, moderate chi. The directionality spread (d=0.10 to d=0.95) explains why the same constraint produces Rope from institutional beneficiaries and Snare from powerless victims. Scope modifier σ(S=continental) = 1.1 amplifies extractiveness at larger scale — enforcement gaps are harder to detect and address across 27 member states than within a single state.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint resolves mandatrophy by showing that Tangled Rope and Rope perspectives are not contradictory but perspectival. The snare perspective (migrant worker) and rope perspective (Western firm) are both correct descriptions of the same constraint from their respective structural positions. The mandate-atrophy resolution requires recognizing that (a) the coordination function is genuine and necessary (labor market matching without standards would fail), (b) the extraction is equally genuine and structural (enforcement gaps are not accidents but architectural features), and (c) the hybrid classification correctly identifies both. The false summit test would misclassify this as 'just coordination' or 'just extraction' depending on which perspective dominates. The tangled_rope classification prevents both errors by insisting on active enforcement (distinguishing from pure rope) and genuine coordination function (distinguishing from pure snare). The theater ratio (0.58) confirms that enforcement ritual is real but not dominant — this is not a piton (degraded constraint maintained purely through ritual). The presence of both beneficiaries and victims, and the requirement for active enforcement to maintain the extraction mechanism, satisfies the Tangled Rope canonical gate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_gap_structural_or_intentional,
    'Is the enforcement gap between Western and Eastern member states a structural result of capacity differences, or an intentional regulatory strategy to preserve differential extraction?',
    'Comparative analysis of inspection frequencies, fine structures, and prosecution rates controlling for firm size and GDP; interviews with labor inspectorates in high-enforcement vs low-enforcement member states; correlation between enforcement level and firm profit margins',
    'If structural (capacity): reclassify the constraint as Rope with regional variation. Sunset clause realistic through capacity-building. If intentional (regulatory arbitrage): snare characteristics are deliberate, sunset clause is cosmetic, and extraction is systemic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_structural_or_intentional, empirical, 'Whether enforcement gaps reflect capacity constraints or intentional regulatory strategy').

omega_variable(
    posted_workers_directive_loophole_scope,
    'What proportion of migrant construction labor enters via the Posted Workers Directive (PWD) loophole versus direct hire? Does PWD legitimately solve labor mobility or does it primarily enable wage arbitrage?',
    'Administrative data on PWD deployment percentages by member state; longitudinal wage comparison between posted workers and directly hired migrants doing identical work; analysis of PWD utilization surge after 2004 Eastern EU accession',
    'If PWD primarily labor mobility: constraint is coordination with spillover benefit to migrants. If PWD primarily arbitrage mechanism: constraint is extraction mechanism disguised as mobility, and the beneficiary/victim structure is deliberate architectural choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(posted_workers_directive_loophole_scope, empirical, 'Whether Posted Workers Directive is labor mobility mechanism or wage arbitrage vehicle').

omega_variable(
    wage_suppression_migration_or_standards,
    'Does wage suppression of migrant workers result from labor supply elasticity (more workers competing for jobs) or from deliberate standards enforcement differential (treated as lower-wage category)?',
    'Wage regression analysis with controls for skill, experience, position, firm size, and member state enforcement level; comparison of wage gaps for identical work before/after PWD implementation; quasi-experimental analysis of firm behavior following increased inspection rates',
    'If supply-driven: constraint is economic law, not institutional extraction. Reclassify toward mountain. If standards-driven: constraint is deliberate structural differentiation, classification as snare/tangled_rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_suppression_migration_or_standards, empirical, 'Whether wage suppression is supply-side or standards-enforcement mechanism').

omega_variable(
    identity_lock_migrant_acceptance,
    'Do migrant construction workers accept wage suppression because they cognitively frame migration as temporary (identity not tied to construction career) or because structural exit barriers create learned helplessness?',
    'Qualitative research on migrant self-identification and career aspiration; comparison of acceptance levels with linguistic/cultural integration levels; analysis of exit propensities when exit barriers are removed (acquisition of citizenship, skill certification)',
    'If identity-locked (temporary migration frame): suppression persists even after structural barriers removed. If trapped (structural barriers): removing barriers should increase exit and reduce acceptance. If constrained (cost-based exit): exit increases with income gains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_migrant_acceptance, conceptual, 'Whether migrant worker acceptance is identity-based or structurally determined').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_construction_labor_standards, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eucls_tr_t0, eu_construction_labor_standards, theater_ratio, 0, 0.42).
narrative_ontology:measurement(eucls_tr_t5, eu_construction_labor_standards, theater_ratio, 5, 0.52).
narrative_ontology:measurement(eucls_tr_t10, eu_construction_labor_standards, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(eucls_be_t0, eu_construction_labor_standards, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eucls_be_t5, eu_construction_labor_standards, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(eucls_be_t10, eu_construction_labor_standards, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_construction_labor_standards, resource_allocation).
narrative_ontology:affects_constraint(eu_construction_labor_standards, posted_workers_directive_loophole).
narrative_ontology:affects_constraint(eu_construction_labor_standards, eastern_european_labor_market_integration).
narrative_ontology:affects_constraint(eu_construction_labor_standards, union_wage_protection_mechanisms).

% DUAL FORMULATION NOTE:
% The EU labor standards constraint is upstream of the specific PWD loophole mechanism. The standards create the coordination framework within which arbitrage occurs. The loophole is downstream and represents the specific extraction vehicle. Both constraints have different ε values: standards coordination ~0.30 (pure rope), PWD arbitrage ~0.72 (snare). The family structure shows how a coordination framework becomes a vehicle for extraction through institutional design choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_construction_labor_standards, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
