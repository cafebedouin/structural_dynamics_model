% ============================================================================
% CONSTRAINT STORY: sotu_1967_johnson_department_consolidation_labor_commerce
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1967_johnson_department_consolidation_labor_commerce, []).

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
 *   constraint_id: sotu_1967_johnson_department_consolidation_labor_commerce
 *   human_readable: Commerce-Labor Department Consolidation Proposal (Johnson, 1967)
 *   domain: governance/administrative_organization
 *
 * SUMMARY:
 *   President Johnson's 1967 proposal to consolidate the Department of
 *   Commerce with the Department of Labor into a unified Cabinet department
 *   was framed as an efficiency measure: eliminating duplicate administrative
 *   functions, reducing bureaucratic redundancy, and improving policy
 *   coordination between business and labor interests. However, the proposal
 *   structurally reorganized the power relationship between labor and capital
 *   within the federal executive. The constraint exhibits high perspectival
 *   variance: business and efficiency advocates see genuine coordination
 *   (Rope), labor advocates see institutional capture (Snare), displaced
 *   staff see forced reorganization (Tangled Rope), and administrative
 *   reformers see a reversible experiment (Scaffold). The theater ratio
 *   increases over the consolidation period, reflecting that the efficiency
 *   narrative carries performative weight but obscures the underlying power
 *   reorganization. The extractiveness measurement shows an initial low value
 *   (22%, reflecting genuine efficiency benefits in the proposal) rising to
 *   42% as implementation reveals labor policy subordination costs and
 *   administrative staff job losses. This trajectory is diagnostic of tangled
 *   rope — real coordination function (efficiency) overlaid with asymmetric
 *   extraction (labor demotion, staff displacement).
 *
 * KEY AGENTS:
 *   - Labor Movement and Worker Advocates: Primary victim (powerless/trapped) — loses dedicated institutional voice; labor policy subordinated to commerce-oriented hierarchy
 *   - Displaced Administrative Staff: Secondary victim (powerless/trapped) — faces job redundancy within consolidated structure; affected by reorganization with minimal negotiating power
 *   - Business Community and Employers: Primary beneficiary (institutional/arbitrage) — benefits from streamlined policy coordination; labor and commerce interests unified under business-friendly leadership
 *   - Taxpayers and Budget Efficiency Coalition: Secondary beneficiary (institutional/arbitrage) — benefits from reduced overhead and administrative redundancy
 *   - Congressional Labor Committee: Organized actor (organized/constrained) — loses jurisdictional autonomy over labor policy; constrained by executive initiative but retains legislative oversight authority
 *   - Department Specialization Principle: Institutional principle (institutional/arbitrage) — organizational doctrine that policy domains warrant dedicated institutions; maintained through historical precedent but functionally degraded by recurrent consolidations
 *   - Administrative Reform Coalition: Organized agent (organized/constrained) — supports consolidation as efficiency measure with measurable outcomes and potential sunset if ineffective
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the opposition between labor and business as immutable, ignoring that institutional separation reflects political choice rather than natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1967_johnson_department_consolidation_labor_commerce, 0.38).
domain_priors:suppression_score(sotu_1967_johnson_department_consolidation_labor_commerce, 0.48).
domain_priors:theater_ratio(sotu_1967_johnson_department_consolidation_labor_commerce, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1967_johnson_department_consolidation_labor_commerce, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1967_johnson_department_consolidation_labor_commerce, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1967_johnson_department_consolidation_labor_commerce, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1967_johnson_department_consolidation_labor_commerce, tangled_rope).
narrative_ontology:human_readable(sotu_1967_johnson_department_consolidation_labor_commerce, "Commerce-Labor Department Consolidation Proposal (Johnson, 1967)").
narrative_ontology:topic_domain(sotu_1967_johnson_department_consolidation_labor_commerce, "governance/administrative_organization").

domain_priors:requires_active_enforcement(sotu_1967_johnson_department_consolidation_labor_commerce).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1967_johnson_department_consolidation_labor_commerce, taxpayers).
narrative_ontology:constraint_beneficiary(sotu_1967_johnson_department_consolidation_labor_commerce, business_interests).
narrative_ontology:constraint_beneficiary(sotu_1967_johnson_department_consolidation_labor_commerce, executive_branch_efficiency).
narrative_ontology:constraint_victim(sotu_1967_johnson_department_consolidation_labor_commerce, labor_advocacy_capacity).
narrative_ontology:constraint_victim(sotu_1967_johnson_department_consolidation_labor_commerce, administrative_staff_job_security).
narrative_ontology:constraint_victim(sotu_1967_johnson_department_consolidation_labor_commerce, institutional_specialization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED ADMINISTRATIVE STAFF (TANGLED ROPE) — Cannot exit the consolidation; faces job loss or reassignment. Trapped within the bureaucratic restructuring. Both benefits (potential career continuity, merger efficiencies) and costs (job redundancy, institutional knowledge loss) are externally imposed. High suppression — no negotiation power with Cabinet-level decision.
constraint_indexing:constraint_classification(sotu_1967_johnson_department_consolidation_labor_commerce, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LABOR MOVEMENT AND WORKER ADVOCATES (SNARE) — Faces structural demotion within consolidated hierarchy. Labor interests lose dedicated institutional voice; labor policy becomes subordinate to commerce-oriented priorities. Cannot exit or veto the consolidation. Suppression is high — organized labor has limited veto power against executive initiative. Pure extraction: worker interests bear costs of institutional capture without corresponding coordination benefits.
constraint_indexing:constraint_classification(sotu_1967_johnson_department_consolidation_labor_commerce, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: BUSINESS COMMUNITY AND EMPLOYERS (ROPE) — Benefits from streamlined policy coordination; labor and commerce issues consolidated under business-friendly hierarchy. Can arbitrage across departments or influence consolidation terms. Experiences constraint as coordination mechanism: unified policy framework reduces inter-agency friction. Net beneficiary — extraction runs toward this actor.
constraint_indexing:constraint_classification(sotu_1967_johnson_department_consolidation_labor_commerce, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TAXPAYERS AND BUDGET EFFICIENCY COALITION (ROPE) — Benefits from reduced administrative overhead and eliminated redundancy. Experiences consolidation as genuine coordination: streamlined bureaucracy produces real savings. Can arbitrage toward or away from the proposal through electoral pressure. Low extraction — the coordination function is primary.
constraint_indexing:constraint_classification(sotu_1967_johnson_department_consolidation_labor_commerce, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONGRESSIONAL LABOR COMMITTEE (TANGLED ROPE) — Loses jurisdictional autonomy and appropriations authority over labor policy if consolidated. Constrained by executive initiative and political feasibility. Both benefits (streamlined legislative oversight, reduced inter-committee friction) and costs (diminished committee influence, labor interests buried in larger department). Moderate extraction — organized but facing institutional entropy.
constraint_indexing:constraint_classification(sotu_1967_johnson_department_consolidation_labor_commerce, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DEPARTMENT SPECIALIZATION PRINCIPLE (PITON) — Organizational principle that policy domains warrant dedicated institutional focus. Theater ratio high: the principle is invoked but implementation is fragile. Historical pattern shows consolidations often fail operationally (labor interests re-absorbed by commerce hierarchy); departments re-split within 10-20 years. Maintained through inertia and rhetorical appeal but functionally degraded.
constraint_indexing:constraint_classification(sotu_1967_johnson_department_consolidation_labor_commerce, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ADMINISTRATIVE REFORM COALITION (SCAFFOLD) — Sees consolidation as temporary coordination mechanism with explicit sunset: efficiency gains can be measured and evaluated; if consolidation fails to reduce overhead or increases labor policy capture, re-separation is possible. Organized actors (budget hawks, reformers) see this as an experiment with defined success metrics and exit clause. Theater ratio moderate — reform agenda is performative but includes real measurement protocols.
constraint_indexing:constraint_classification(sotu_1967_johnson_department_consolidation_labor_commerce, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a public administration theory perspective, separation of labor and commerce policy reflects an irreducible structural fact: worker interests and business interests have opposing priorities; merging institutions representing opposed interests into a single hierarchy necessarily subordinates one to the other. This perspective classifies the attempt to consolidate as defying a natural organizing principle. However, the structural data (identifiable beneficiaries, victims, and extraction mechanisms) contradicts the mountain classification — the engine will detect this as a false summit.
constraint_indexing:constraint_classification(sotu_1967_johnson_department_consolidation_labor_commerce, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1967_johnson_department_consolidation_labor_commerce_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1967_johnson_department_consolidation_labor_commerce, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1967_johnson_department_consolidation_labor_commerce, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1967_johnson_department_consolidation_labor_commerce, TR),
    TR >= 0.70.

:- end_tests(sotu_1967_johnson_department_consolidation_labor_commerce_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The proposal offers genuine efficiency benefits (eliminated redundant positions, streamlined policy coordination) valued at ~0.22 baseline extractiveness. However, the reorganization creates asymmetric costs: labor policy subordination within a commerce-oriented hierarchy extracts from worker advocates (adding ~0.15); staff displacement extracts from administrative personnel (adding ~0.08). The net effect is moderate extractiveness, not pure efficiency. Suppression (0.48): Moderate-high. Labor movement has organizational capacity to resist but limited veto power against executive initiative in 1967 context (unions are strong but executive autonomy on bureaucratic structure is high). Administrative staff have minimal power to negotiate consolidation terms. Theater ratio (0.62): Moderate-high. The efficiency narrative is substantively real (overhead reduction is achievable) but obscures the power reorganization. Over the consolidation period, theater rises as the gap widens between the 'streamlining' rhetoric and the 'labor demotion' reality. This is characteristic of institutional reorganization: efficiency claims are genuine but incomplete — they are true about administrative overhead but silent about policy hierarchy shifts.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a deep perspectival gap between institutional actors with opposed interests. Business sees a coordination mechanism solving the legitimate problem of inter-departmental friction. Labor sees an extraction mechanism that subordinates their interests to business-oriented priorities. The snare classification from labor's perspective and the rope classification from business's perspective are not measurement disagreements but structural differences in how each agent experiences the consolidation. From labor's perspective, the constraint forces a choice: accept institutional demotion or exit (not really an exit — labor advocacy doesn't disappear, it just loses its dedicated Cabinet seat). From business's perspective, the constraint solves a coordination problem: unified labor and commerce policy reduces inter-agency turf wars. Both perspectives are accurate to their structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Labor advocates and displaced staff are victims with trapped exit (d ≈ 0.92), experiencing high effective extraction despite moderate base extractiveness. Business and taxpayers are beneficiaries with arbitrage exit (d ≈ 0.05), experiencing low or negative effective extraction. The Congressional Labor Committee is an organized victim with constrained exit (d ≈ 0.55), experiencing moderate extraction. The consolidation creates directionality asymmetry: those with power to shape the consolidation (business, executive branch) benefit; those with power to resist (organized labor, Congress, staff unions) face constrained or trapped dynamics and bear net extraction costs. This is the core tangled rope signature: genuine coordination function (efficiency) paired with asymmetric extraction (labor demotion).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves by distinguishing genuine coordination function (efficiency, reduced redundancy) from asymmetric extraction (labor demotion, staff displacement). The tangled rope classification captures both: the constraint is not pure extraction (snare) because efficiency gains are real; it is not pure coordination (rope) because labor interests bear concentrated costs. The mandatrophy arises from Johnson's proposal framing, which emphasizes efficiency while remaining silent about power reorganization. The analytical observer viewing this from a 'natural law' perspective (that labor and business interests are inherently opposed and consolidation necessarily subordinates one) risks naturalizing what is actually a political choice: the consolidation COULD have been structured differently (e.g., with co-equal labor and business leadership, or with explicit labor policy protections). The false summit is the framing that consolidation is a neutral efficiency measure; the structural reality is that it is a power reorganization with efficiency benefits as a secondary feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_capture_threshold,
    'At what point does labor policy subordination within a business-oriented consolidated department constitute functional capture vs. legitimate inter-departmental coordination?',
    'Post-consolidation labor policy outcomes: wage standard enforcement, occupational safety regulations, union recognition enforcement, compared to pre-consolidation baseline and counterfactual (if Labor had remained independent)',
    'If capture occurs within 3 years: snare classification confirmed for labor advocates. If labor policy remains independent: tangled rope classification more accurate, suggesting constraint is reversible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_capture_threshold, empirical, 'Threshold for labor policy capture within consolidated department').

omega_variable(
    administrative_efficiency_realization,
    'Do proposed overhead reductions (eliminated duplicate positions, streamlined policy coordination) materialize at claimed levels?',
    'Budget analysis of actual staffing reductions vs. projected; measurement of inter-departmental coordination delays pre- vs. post-consolidation; cost accounting for transition friction and lost specialized expertise',
    'If efficiencies materialize: rope classification for taxpayers confirmed. If overhead reduction fails: entire consolidation is theater (piton classification), and extraction mechanism is political positioning rather than structural benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(administrative_efficiency_realization, empirical, 'Whether projected administrative efficiencies materialize post-consolidation').

omega_variable(
    institutional_separation_viability,
    'If the consolidation proves dysfunctional, can labor policy be re-separated into its own department, or does institutional merger create path dependency?',
    'Historical comparison with previous consolidations (e.g., Commerce created 1903, Interior/Labor separation patterns); analysis of whether bureaucratic re-separation has been attempted and reversed in US administrative history',
    'If re-separation is feasible: scaffold sunset is real, and the constraint is temporary. If path-dependent: institutional lock-in occurs, and labor advocates face permanent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_separation_viability, empirical, 'Whether labor-commerce consolidation can be reversed if dysfunctional').

omega_variable(
    labor_political_power_asymmetry,
    'Does the organizational structure (consolidated department with business-friendly leadership) structurally limit labor''s political input relative to business input?',
    'Analysis of labor movement''s ability to influence consolidated department policy; comparison of labor policy outcomes under consolidation vs. independent Labor Department; lobbying access and political capital required to shape policy',
    'If asymmetry is structural (not fixable by leadership choice): snare classification is more accurate than tangled rope. If asymmetry is contingent on specific leadership: tangled rope is appropriate, and policy outcomes depend on political choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_political_power_asymmetry, conceptual, 'Whether consolidation structurally limits labor''s political influence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1967_johnson_department_consolidation_labor_commerce, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu67_tr_t0, sotu_1967_johnson_department_consolidation_labor_commerce, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sotu67_tr_t3, sotu_1967_johnson_department_consolidation_labor_commerce, theater_ratio, 3, 0.58).
narrative_ontology:measurement(sotu67_tr_t6, sotu_1967_johnson_department_consolidation_labor_commerce, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(sotu67_be_t0, sotu_1967_johnson_department_consolidation_labor_commerce, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sotu67_be_t3, sotu_1967_johnson_department_consolidation_labor_commerce, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(sotu67_be_t6, sotu_1967_johnson_department_consolidation_labor_commerce, base_extractiveness, 6, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1967_johnson_department_consolidation_labor_commerce, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1967_johnson_department_consolidation_labor_commerce, labor_representation_federal_hierarchy).
narrative_ontology:affects_constraint(sotu_1967_johnson_department_consolidation_labor_commerce, business_labor_policy_integration).

% DUAL FORMULATION NOTE:
% The consolidation proposal creates two structurally distinct constraints: (1) administrative efficiency coordination (genuine collaborative problem-solving); (2) labor policy subordination within a consolidated hierarchy (power asymmetry). This story models both as tangled rope. If decomposed into separate stories, efficiency coordination would be rope (ε ≈ 0.12) and labor subordination would be snare (ε ≈ 0.65). The story represents them as a single hybrid because the consolidation decision is architecturally unified — the efficiency gains are inseparable from the power reorganization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1967_johnson_department_consolidation_labor_commerce, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
