% ============================================================================
% CONSTRAINT STORY: sotu_2005_bush_no_child_left_behind_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2005_bush_no_child_left_behind_enforcement, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_2005_bush_no_child_left_behind_enforcement
 *   human_readable: NCLB Federal Accountability Mandate: Standardized Testing and Achievement Gap Closure
 *   domain: education/federal_policy/accountability
 *
 * SUMMARY:
 *   The No Child Left Behind Act (2001) represents a federal mandate to close
 *   achievement gaps through standardized testing and accountability
 *   mechanisms. Schools are required to demonstrate adequate yearly progress
 *   (AYP) toward closing racial and socioeconomic achievement gaps; failure
 *   to meet targets triggers escalating sanctions including loss of federal
 *   funding and state takeover. This constraint exhibits a classic Tangled
 *   Rope structure: it possesses a genuine coordination function (making
 *   achievement disparities visible, creating federal leverage for equity
 *   pressure, forcing resource allocation transparency) while simultaneously
 *   extracting through compliance burden, curriculum narrowing, high-stakes
 *   testing theater, and shifting responsibility for poverty-driven gaps onto
 *   schools rather than addressing structural inequality. The extractiveness
 *   trajectory shows accumulation: the constraint began moderately extractive
 *   (2002, measured at 0.32) but escalated as states set higher testing
 *   standards and schools faced genuine sanction threats (2005, peaked at
 *   0.62). The theater ratio increased in parallel as high-stakes testing
 *   drove curricular narrowing and test-prep focus, especially in
 *   under-resourced schools where the testing ritual became increasingly
 *   disconnected from actual pedagogical improvement. The constraint's
 *   mandatrophy centers on whether the genuine coordination benefit (federal
 *   leverage for equity) can justify the extraction costs (pedagogical
 *   autonomy loss, resource misallocation to test prep, teacher flight from
 *   low-performing schools), or whether it naturalizes as a Snare
 *   masquerading as equity policy.
 *
 * KEY AGENTS:
 *   - Federal Department of Education: Primary beneficiary (institutional/arbitrage) — gains policy authority expansion, leverages Title I funding to enforce federal standards, benefits from accountability data infrastructure
 *   - Standardized Testing Industry: Primary beneficiary (institutional/arbitrage) — direct revenue increase from expanded testing contracts, assessment design services, data systems
 *   - Under-Resourced Schools: Primary victim (powerless/trapped) — mandated compliance without proportional resource increases; face federal takeover and funding loss threats
 *   - Minority Student Populations: Primary victim (moderate/constrained) — targeted for gap closure but benefit contingent on district capacity; experience test-driven curriculum narrowing; face increased evaluation pressure
 *   - Teacher Workforce: Secondary victim (organized/constrained) — loss of pedagogical autonomy, high-stakes evaluation pressure, increased accountability burden; attrition pressure in low-performing schools
 *   - Affluent Suburban Districts: Secondary beneficiary (powerful/mobile) — already meet benchmarks; experience NCLB as low-burden ritual attestation; gain competitive advantage from resources devoted to advanced placement and enrichment
 *   - Classroom Pedagogical Autonomy: Victim (abstract, powerless/trapped) — curriculum narrows toward tested subjects and test-prep; project-based learning, critical thinking, and exploratory instruction are squeezed
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent funding mechanisms as immutable constraints on achievable equity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2005_bush_no_child_left_behind_enforcement, 0.58).
domain_priors:suppression_score(sotu_2005_bush_no_child_left_behind_enforcement, 0.62).
domain_priors:theater_ratio(sotu_2005_bush_no_child_left_behind_enforcement, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2005_bush_no_child_left_behind_enforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_2005_bush_no_child_left_behind_enforcement, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sotu_2005_bush_no_child_left_behind_enforcement, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2005_bush_no_child_left_behind_enforcement, tangled_rope).
narrative_ontology:human_readable(sotu_2005_bush_no_child_left_behind_enforcement, "NCLB Federal Accountability Mandate: Standardized Testing and Achievement Gap Closure").
narrative_ontology:topic_domain(sotu_2005_bush_no_child_left_behind_enforcement, "education/federal_policy/accountability").

domain_priors:requires_active_enforcement(sotu_2005_bush_no_child_left_behind_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2005_bush_no_child_left_behind_enforcement, federal_department_education).
narrative_ontology:constraint_beneficiary(sotu_2005_bush_no_child_left_behind_enforcement, standardized_testing_industry).
narrative_ontology:constraint_beneficiary(sotu_2005_bush_no_child_left_behind_enforcement, high_performing_affluent_districts).
narrative_ontology:constraint_beneficiary(sotu_2005_bush_no_child_left_behind_enforcement, measurement_professionals).
narrative_ontology:constraint_victim(sotu_2005_bush_no_child_left_behind_enforcement, low_performing_under_resourced_schools).
narrative_ontology:constraint_victim(sotu_2005_bush_no_child_left_behind_enforcement, minority_student_populations).
narrative_ontology:constraint_victim(sotu_2005_bush_no_child_left_behind_enforcement, classroom_pedagogical_autonomy).
narrative_ontology:constraint_victim(sotu_2005_bush_no_child_left_behind_enforcement, educational_innovation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDER-RESOURCED SCHOOL DISTRICT (SNARE) — Mandated to close achievement gaps but lacks the capital resources (qualified teachers, modern facilities, technology) to meet federal benchmarks. Exit costs are total: refusing to participate triggers federal takeover, funding withdrawal, and school closure. The compliance burden falls heaviest on already-constrained districts with minimal alternative revenue sources. Maximum experienced extraction — bear compliance costs with minimal agency.
constraint_indexing:constraint_classification(sotu_2005_bush_no_child_left_behind_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MINORITY STUDENT POPULATIONS (SNARE) — Targeted by NCLB's achievement gap mandate but benefit is contingent on district capacity. In under-resourced schools, the mandate creates pressure for resource reallocation but without actual resource increase — becomes zero-sum game. Exit costs: students cannot leave district schools without significant mobility (private school tuition, relocation, charter availability). Theater of 'closing gaps' without providing means. High suppression from limited school choice and resource constraints.
constraint_indexing:constraint_classification(sotu_2005_bush_no_child_left_behind_enforcement, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PUBLIC SCHOOL ADMINISTRATION (TANGLED ROPE) — Genuine coordination function: standardized benchmarks enable inter-district comparison, force resource visibility, and create federal levers for equity pressure. But also significant extraction: compliance bureaucracy, high-stakes testing overhead, curriculum narrowing to test prep, and state takeover threat. Mixed experience — some coordination benefit (external mandate creates political cover for equity spending) alongside high enforcement costs and loss of curricular autonomy. Constrained exit (rejecting federal funds loses entire federal budget stream).
constraint_indexing:constraint_classification(sotu_2005_bush_no_child_left_behind_enforcement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STANDARDIZED TESTING INDUSTRY (ROPE) — Benefits directly from NCLB mandate: increased testing volume, contract expansion, data services revenue. Experiences constraint as pure coordination: designing assessments that compare schools enables the entire accountability logic. High arbitrage (can exit by not contracting, but profit incentives align perfectly with mandate). Low experienced extraction — extraction runs toward this actor. Rope classification derived from positive directionality despite high institutional power.
constraint_indexing:constraint_classification(sotu_2005_bush_no_child_left_behind_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FEDERAL DEPARTMENT OF EDUCATION (TANGLED ROPE) — Genuine coordination function: NCLB's Title I funding increases provide federal leverage to enforce equity through accountability metrics; creates pressure on states to address disparities. But also embedded extraction: federal overreach into local education, standardized testing theater masking inequality, and shifting responsibility for poverty-related achievement gaps onto schools rather than addressing resource distribution. Net beneficiary through institutional expansion and policy control, but also captures some victim status through political backlash. Arbitrage exit (can modify policy framework), but career institutional interests align with continuation.
constraint_indexing:constraint_classification(sotu_2005_bush_no_child_left_behind_enforcement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: AFFLUENT SUBURBAN DISTRICT (PITON) — Experiences NCLB as largely performative ritual: schools already meet benchmarks, so compliance is box-checking. Testing overhead is minimal relative to institutional capacity. High theater ratio (passing tests is expected, testing ritual is ceremonial attestation of existing success rather than mechanism for improvement). Low actual extraction because the constraint's mechanisms don't apply forcefully to already-high-performing districts. Mobile exit (can emphasize advanced placement, private testing, curriculum enrichment that bypass the standardized test as the meaningful signal). Piton classification derived from theater gate and minimal functional impact.
constraint_indexing:constraint_classification(sotu_2005_bush_no_child_left_behind_enforcement, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: TEACHER WORKFORCE (TANGLED ROPE) — Organized through unions and professional associations. NCLB creates genuine coordination function: transparent accountability pressure can force resource allocation toward under-resourced classrooms, which unions support. But high extraction: high-stakes testing narrows pedagogy, limits professional autonomy, increases performance evaluation pressure, creates disincentives for teaching in low-performing schools. Constrained exit (cannot simply refuse to teach under NCLB mandate; can exit profession through attrition). Mixed experience — some benefit (potential for equity-driven resource reallocation) alongside significant autonomy costs and evaluation pressure.
constraint_indexing:constraint_classification(sotu_2005_bush_no_child_left_behind_enforcement, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a long-term sociological perspective, some achievement gap is inherent to unequal resource distribution: schools funded by local property taxes will always have disparities correlated with housing segregation and wealth distribution. Achievement gap is a natural law of local-funding-dependent education systems. This perspective sees NCLB's mandate as addressing an impossible constraint — trying to close gaps while maintaining the structural mechanisms that produce gaps. However, the structural data (identifiable beneficiaries, active enforcement, significant extraction) contradicts mountain classification — the engine will flag this as false summit, revealing naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(sotu_2005_bush_no_child_left_behind_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2005_bush_no_child_left_behind_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2005_bush_no_child_left_behind_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2005_bush_no_child_left_behind_enforcement, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2005_bush_no_child_left_behind_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2005_bush_no_child_left_behind_enforcement, TR),
    TR >= 0.70.

:- end_tests(sotu_2005_bush_no_child_left_behind_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint began with moderate extraction (0.32 in 2002) as schools interpreted NCLB as a coordination challenge requiring better measurement and resource reallocation. By 2005-2006, extractiveness had risen to 0.62 as sanction threats became credible, schools realized gap-closure benchmarks were unachievable without structural resource increases, and compliance burden consumed increasing shares of school budgets. The peak represents the point at which the constraint's cost-benefit ratio for under-resourced schools shifted from mixed (some benefit from transparency, some burden from compliance) to predominantly extractive (high burden with minimal plausible benefit). The slight decrease to 0.58 by 2009 reflects modest federal flexibility additions and growing union resistance, but the constraint remained highly extractive. Suppression (0.62): High. Schools have limited exit options: refusing federal funding triggers takeover authority activation; cannot simply opt out of state accountability systems. Teachers cannot exit without leaving profession. Students cannot exit without private school access or inter-district transfers. The suppression reflects real structural barriers (funding dependency, regulatory authority), not manufactured scarcity. Theater ratio (0.68): High and rising. By 2005-2009, high-stakes testing became substantially theatrical in under-resourced schools: testing ritual consumed class time without improving underlying pedagogical capacity; curriculum narrowed to tested subjects and test-prep strategies that bore diminishing relationship to actual learning outcomes; bubble-sheet drilling replaced conceptual learning. The high theater reflects Goodhart's Law: teaching to the test became an end in itself, disconnected from the coordination function (measuring actual achievement gaps). Affluent schools had lower theater (testing was a valid signal of existing excellence); under-resourced schools had high theater (testing became a compliance ritual masking continued inequality).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is the core diagnostic signal: beneficiaries (testing industry, federal officials) see coordination and legitimate equity enforcement; victims (under-resourced schools, teachers, minority students) see mandated gaps with insufficient means. The magnitude of this gap (beneficiary rope vs. victim snare from near-identical ε) reveals that the constraint is not a natural law or pure coordination failure, but a policy choice with distributed costs and concentrated benefits. The divergence shows up most starkly in the transition from the immediate/biographical timeframe (where under-resourced schools see snare) to the generational frame (where analytical observers risk seeing mountain — treating local funding inequality as inevitable). The perspectival gap analysis exposes that risk: what looks natural at civilizational scale is a choice at biographical scale.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from agents' structural position and exit costs. The testing industry has positive directionality (low d, ~0.10): beneficiary with arbitrage exit, experiencing negative effective extraction (constraint subsidizes their growth). Federal education officials have moderate-positive directionality (d~0.25): beneficiary with policy arbitrage (can adjust rules), but some victim status through political backlash from implementation failures. Under-resourced schools have high directionality (d~0.90): victims with trapped exit (cannot refuse federal funds without losing entire Title I stream), experiencing maximum effective extraction. Teachers have moderate-high directionality (d~0.65): victims with constrained exit (can leave profession but high cost), experiencing significant extraction from evaluation pressure and autonomy loss. The directionality gap between beneficiaries (testing industry, federal officials) and victims (under-resourced schools, teachers) is the engine's primary input for computing perspectival chi values. No overrides are needed; the structural data produces differentiated directionality automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED: The extractiveness (0.58) falls in the contested zone where mandatrophy is irreducible. The core ambiguity: NCLB genuinely has a coordination function (making disparities visible, forcing transparency, creating federal leverage for equity pressure), but the extraction mechanism (unachievable gap-closure benchmarks without proportional resource increase, curriculum narrowing, teacher flight) undermines whether the coordination benefit justifies the cost. The constraint cannot be classified as pure Rope (the extraction is too high, the burden too unequal) or pure Snare (the coordination function and federal equity pressure are real, not theater). It is genuinely Tangled Rope. But the core mandatrophy question remains: At what point does the extraction mechanism (especially teacher flight and curriculum narrowing with permanent pedagogical consequences) outweigh the coordination benefit? This depends on empirical resolution of the omegas: if federal funding was adequate, if gap closure was achievable, and if teacher quality is preserved in low-performing schools, NCLB is a mixed-cost coordination mechanism (Tangled Rope is appropriate). If federal funding was inadequate, gap closure was structurally impossible, and teacher flight occurred, NCLB is a mandate without means (reclassify as Snare). The 2026 retrospective suggests the omegas resolve toward the snare direction: federal funding was insufficient, teacher attrition in low-performing schools was documented, and gap closure did not occur. But from 2005 perspective (the SOTU year of prompt), that resolution was still uncertain. The mandatrophy_resolved flag is set to false because the constraint's classification legitimately depends on empirical outcomes not yet determined in 2005.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gap_closure_possibility_structural,
    'Can achievement gaps be structurally closed through school-based accountability without addressing funding disparities, residential segregation, and poverty-related resource gaps?',
    'Longitudinal analysis of achievement gap trends in jurisdictions with NCLB accountability vs. jurisdictions with additional resource equalization mechanisms (school finance reform, pre-K expansion, housing integration). Comparison of gap closure rates where accountability exists with and without commensurate resource transfers.',
    'If gaps can be closed through school accountability alone: NCLB is genuine coordination mechanism (Rope/Tangled Rope from more perspectives). If structural barriers prevent closure: NCLB becomes theater masking inequality (Piton, Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gap_closure_possibility_structural, empirical, 'Whether achievement gaps are closable through school accountability without broader structural resource redistribution').

omega_variable(
    high_stakes_testing_pedagogical_cost,
    'Does high-stakes testing-driven curriculum narrowing permanently damage pedagogical capacity in under-resourced schools, making it harder to return to broader curricula even if accountability pressure were removed?',
    'Analysis of schools exiting NCLB sanctions: do curricula expand or remain narrowed? Comparison of pedagogical practices (critical thinking, writing intensity, project-based learning) in high-stakes vs. low-stakes testing contexts. Teacher attrition rates and subject specialization losses in constrained schools.',
    'If damage is temporary: constraint is reversible (Scaffold/Tangled Rope). If damage is permanent: extraction mechanism includes path-dependence trap (Snare/Piton upgrade).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(high_stakes_testing_pedagogical_cost, empirical, 'Whether curriculum narrowing from high-stakes testing creates irreversible pedagogical capacity loss').

omega_variable(
    testing_industry_revenue_incentive,
    'Does the standardized testing industry profit from failing schools remaining in the accountability system longer, creating perverse incentive against gap closure?',
    'Financial analysis of testing industry revenue models; investigation of contract duration, renewal rates for high-performing vs. low-performing districts; analysis of whether incentive structures reward gap closure or testing volume.',
    'If revenue depends on testing volume regardless of outcomes: structural extraction is embedded in measurement apparatus (Snare/Piton). If revenue aligns with gap closure: genuine coordination (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(testing_industry_revenue_incentive, empirical, 'Whether testing industry revenue model creates perverse incentives against achievement gap closure').

omega_variable(
    teacher_quality_flight_mechanism,
    'Does NCLB accountability pressure systematically push experienced teachers out of low-performing schools, worsening the very gaps the policy aims to close?',
    'Longitudinal teacher mobility analysis: tracking teacher movements pre- and post-NCLB, comparing attrition rates in high-accountability schools vs. low-accountability schools, analyzing whether experienced teachers disproportionately exit low-performing schools under accountability pressure.',
    'If teacher flight occurs: NCLB has extractive perverse effect undermining its own mandate (Snare). If teacher distribution is stable: accountability mechanism is functional (Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(teacher_quality_flight_mechanism, empirical, 'Whether NCLB accountability pressure causes experienced teacher flight from low-performing schools').

omega_variable(
    federal_funding_adequacy,
    'Did federal Title I funding increases under NCLB actually provide sufficient resources to close achievement gaps, or was the mandate under-resourced from inception?',
    'Analysis of Title I funding per-pupil in gap-target schools vs. actual costs of achieving benchmarks (teacher salaries for qualified staff, technology, remediation programs, reduced class sizes). Comparison of compliance costs across districts of different base resource levels.',
    'If funding was adequate: constraint is coordination mechanism with extraction overlay (Tangled Rope). If funding was insufficient: constraint is mandate without means (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_funding_adequacy, empirical, 'Whether federal Title I funding increases provided adequate resources to achieve NCLB benchmarks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2005_bush_no_child_left_behind_enforcement, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nclb_tr_t0, sotu_2005_bush_no_child_left_behind_enforcement, theater_ratio, 0, 0.38).
narrative_ontology:measurement(nclb_tr_t3, sotu_2005_bush_no_child_left_behind_enforcement, theater_ratio, 3, 0.52).
narrative_ontology:measurement(nclb_tr_t6, sotu_2005_bush_no_child_left_behind_enforcement, theater_ratio, 6, 0.68).
narrative_ontology:measurement(nclb_tr_t9, sotu_2005_bush_no_child_left_behind_enforcement, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(nclb_be_t0, sotu_2005_bush_no_child_left_behind_enforcement, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(nclb_be_t3, sotu_2005_bush_no_child_left_behind_enforcement, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(nclb_be_t6, sotu_2005_bush_no_child_left_behind_enforcement, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(nclb_be_t9, sotu_2005_bush_no_child_left_behind_enforcement, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2005_bush_no_child_left_behind_enforcement, resource_allocation).
narrative_ontology:affects_constraint(sotu_2005_bush_no_child_left_behind_enforcement, standardized_testing_curriculum_narrowing).
narrative_ontology:affects_constraint(sotu_2005_bush_no_child_left_behind_enforcement, teacher_attrition_low_performing_schools).
narrative_ontology:affects_constraint(sotu_2005_bush_no_child_left_behind_enforcement, local_funding_equity_disparity).

% DUAL FORMULATION NOTE:
% NCLB's coordination function (resource transparency, federal equity leverage) is structurally distinct from its extraction mechanism (unachievable benchmarks, curriculum narrowing, teacher flight). These could be modeled as separate constraints: 'NCLB-as-transparency-mechanism' (lower ε, more rope) vs 'NCLB-as-accountability-theater' (higher ε, more snare). The unified story treats them as one constraint because NCLB's architecture embeds both — the coordination and extraction are structurally inseparable. Downstream constraints (curriculum narrowing, teacher attrition) are causal effects of the extraction mechanism, not separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
