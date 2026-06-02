% ============================================================================
% CONSTRAINT STORY: uk_postgraduate_visa_points_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_postgraduate_visa_points_system, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: uk_postgraduate_visa_points_system
 *   human_readable: UK Postgraduate Visa Points System
 *   domain: immigration/labor_policy
 *
 * SUMMARY:
 *   The UK postgraduate visa points system represents a hybrid constraint
 *   combining genuine labor market coordination with asymmetric extraction.
 *   Since 2022, the system allocates postgraduate visa slots by points
 *   accumulated through: degree classification, institution tier, field of
 *   study (higher points for shortage occupations), and salary expectations.
 *   The constraint exhibits structural tension between its coordinating
 *   function (directing international talent toward labor shortages) and its
 *   extracting function (imposing disproportionate barriers on students in
 *   non-shortage fields). Students in shortage occupations like computer
 *   science, engineering, and healthcare can accumulate points relatively
 *   easily, while students in humanities, social sciences, and
 *   non-prioritized STEM face barriers that push them toward higher salaries,
 *   sponsorship dependency, or exit from the UK labor market entirely. The
 *   constraint's extractiveness has increased over the interval as
 *   institutions have responded to visa barriers by raising postgraduate
 *   fees, and as the government has tightened salary thresholds for points.
 *   The theater ratio reflects that the points-based framing suggests
 *   objective technical allocation while actual filtering is driven by policy
 *   discretion over the shortage occupations list and salary thresholds.
 *
 * KEY AGENTS:
 *   - Postgraduate Students in Non-Shortage Fields: Primary victims (powerless/trapped) — bear disproportionate extraction through visa barriers with no alternative pathways
 *   - International Students from Middle-Income Countries: Secondary victims (moderate/constrained) — face genuine constraints on visa access but have some alternative pathways (family sponsorship, post-study work, doctoral routes)
 *   - UK Higher Education Institutions: Primary beneficiaries (institutional/arbitrage) — capture postgraduate fees amplified by visa barriers and institutional ranking benefits from international enrollment
 *   - Employers in Shortage Occupations: Beneficiaries (institutional/arbitrage) — receive pre-filtered, motivated, educated candidates without domestic recruitment costs
 *   - UK Government: Beneficiary (institutional/arbitrage) — gains visa fee revenue and labor market control; allocates talent to prioritized sectors
 *   - Government Labor Market Planning: Organized actor (organized/constrained) — sees points system as temporary mechanism with sunset clause; maintains it pending domestic workforce reskilling
 *   - Immigration Administration System: Institutional actor (institutional/arbitrage) — maintains points system through administrative continuity; performs theater of objectivity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_postgraduate_visa_points_system, 0.58).
domain_priors:suppression_score(uk_postgraduate_visa_points_system, 0.65).
domain_priors:theater_ratio(uk_postgraduate_visa_points_system, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_postgraduate_visa_points_system, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_postgraduate_visa_points_system, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(uk_postgraduate_visa_points_system, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_postgraduate_visa_points_system, tangled_rope).
narrative_ontology:human_readable(uk_postgraduate_visa_points_system, "UK Postgraduate Visa Points System").
narrative_ontology:topic_domain(uk_postgraduate_visa_points_system, "immigration/labor_policy").

domain_priors:requires_active_enforcement(uk_postgraduate_visa_points_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_postgraduate_visa_points_system, uk_higher_education_institutions).
narrative_ontology:constraint_beneficiary(uk_postgraduate_visa_points_system, employers_in_shortage_occupations).
narrative_ontology:constraint_beneficiary(uk_postgraduate_visa_points_system, uk_government_revenue).
narrative_ontology:constraint_victim(uk_postgraduate_visa_points_system, postgraduate_students_from_non_shortage_fields).
narrative_ontology:constraint_victim(uk_postgraduate_visa_points_system, international_students_middle_income_countries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POSTGRADUATE STUDENT IN NON-SHORTAGE FIELD (SNARE) — Trapped by visa point requirements designed for shortage occupations. Student cannot modify their field of study retroactively, cannot accumulate points through alternative pathways, and faces escalating visa costs. Zero exit options: remain in non-shortage field and fail to accumulate points, or abandon credentials already earned. Maximum experienced extraction — the constraint explicitly targets this agent and provides no legitimate alternative.
constraint_indexing:constraint_classification(uk_postgraduate_visa_points_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNATIONAL STUDENT FROM MIDDLE-INCOME COUNTRY (TANGLED ROPE) — Constrained but not trapped. Faces genuine coordination problem: UK needs to allocate limited immigration slots and has legitimate interest in attracting talent to shortage occupations. Also benefits from UK education access, institutional reputation, and visa pathway clarity. The system constrains exit (high visa costs, points barriers) but provides some legitimate alternatives (STEM routes, salary thresholds, family sponsorship paths). Significant extraction but with genuine coordination function.
constraint_indexing:constraint_classification(uk_postgraduate_visa_points_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UK HIGHER EDUCATION INSTITUTION (ROPE) — Benefits from postgraduate enrollment fees and institutional rankings sustained by international student populations. Experiences the constraint as coordination: the visa points system channels students toward shortage fields (where employment is easier), which improves graduate employment metrics and institutional prestige. Net beneficiary — the system creates reliable pipeline of motivated international talent filtered by economic demand signals. Active enforcement supports institutional interests.
constraint_indexing:constraint_classification(uk_postgraduate_visa_points_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EMPLOYERS IN SHORTAGE OCCUPATIONS (ROPE) — Direct beneficiaries. The points system creates reliable supply of pre-vetted, educated candidates in occupations facing labor shortages (healthcare, engineering, data science). The 'coordination' function is real: employers have legitimate interest in migrant workers, students have employment pathway clarity. Lower extraction experienced because the constraint aligns employer and student interests in shortage fields — both parties benefit from completion.
constraint_indexing:constraint_classification(uk_postgraduate_visa_points_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: GOVERNMENT LABOR MARKET PLANNING (SCAFFOLD) — Sees the points system as temporary mechanism for managing labor market transitions. The sunset logic: as UK reskills domestic workforce in shortage occupations (through expanded vocational training, apprenticeships), reliance on postgraduate international migration should decline. The constraint has built-in sunset: intended to be temporary until labor market realignment. Government maintains points system through periodic review and adjustment of shortage occupations list.
constraint_indexing:constraint_classification(uk_postgraduate_visa_points_system, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: IMMIGRATION ADMINISTRATION SYSTEM (PITON) — The points-based system is institutional inertia: inherited from earlier point-based immigration frameworks designed for work migration, adapted to postgraduate visas without fundamental functional redesign. The theater is maintaining the fiction that 'points-based' means objective, non-discretionary selection when in practice the shortage occupations list is discretionary policy. The system persists through institutional continuity (it works technically) rather than structural necessity, but actual filtering is driven by salary thresholds and employer sponsorship, not by points accumulation. Theater ratio reflects that points are visible administrative overhead while actual allocation decisions are made elsewhere.
constraint_indexing:constraint_classification(uk_postgraduate_visa_points_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the system is a genuine hybrid: coordinating labor allocation (legitimate function) while extracting rents from international students (asymmetric extraction). The coordination function is real — UK genuinely faces labor shortages in identified occupations. The extraction is real — students in non-shortage fields bear disproportionate cost. Tangled rope classification reflects both functions operating simultaneously: not a snare (extraction is not the primary function), not pure rope (extraction is substantial), but genuine hybrid that requires active enforcement of the points metric to maintain.
constraint_indexing:constraint_classification(uk_postgraduate_visa_points_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_postgraduate_visa_points_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_postgraduate_visa_points_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_postgraduate_visa_points_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_postgraduate_visa_points_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_postgraduate_visa_points_system, TR),
    TR >= 0.70.

:- end_tests(uk_postgraduate_visa_points_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting asymmetric extraction from non-shortage-field students while genuine coordination function for shortage fields exists. The value increased from 0.35 to 0.58 over six years as institutions raised fees in response to visa barriers and government tightened salary thresholds. This is not pure extraction (Snare territory ≥0.66) because the system does coordinate legitimate labor demand, beneficiaries genuinely solve a coordination problem, and some students benefit from clarity about pathways. But extraction is substantial and asymmetric. Suppression (0.65): High. Barriers include: visa point requirements with limited accumulation pathways, rising salary thresholds, institutional visa sponsorship gatekeeping, fee escalation, and limited alternative routes. These are not total barriers (some students do navigate the system) but represent significant suppression of exit options for non-shortage-field graduates. Theater ratio (0.48): Moderate. The points-based framing creates appearance of technical objectivity, but the shortage occupations list is discretionary policy, salary thresholds are policy-set rather than market-derived, and institution tier rankings are institutional-hierarchy maintenance rather than labor market signals. This is not piton-level theater (would need ≥0.70) because the system does perform real filtering and allocation function — the theater is overlay on genuine coordination, not replacement for it.
 *
 * PERSPECTIVAL GAP:
 *   The powerless student in a non-shortage field perceives a Snare (high extraction, no exit, trapped by retroactive field choice). The moderate student from middle-income country perceives Tangled Rope (genuine constraints but some alternatives, mixed benefits and costs). The UK university perceives Rope (coordination mechanism channeling talent to measurable employment outcomes, enhanced institutional metrics). The shortage-field employer perceives Rope (supply of pre-filtered candidates matching labor demand). The government labor market planning perceives Scaffold (temporary mechanism with sunset as domestic workforce transitions). The immigration administration perceives Piton (inherited points-based system maintained through institutional inertia, performative objectivity). The analytical observer perceives Tangled Rope (genuine coordination function cannot be cleanly separated from asymmetric extraction; both are structural features). The perspectival gaps are wide because the constraint genuinely serves different functions for different agents: coordination for shortage fields, extraction for non-shortage fields, revenue for institutions, labor supply for employers, labor market control for government.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each agent is determined by their structural position relative to the points system. Non-shortage-field students are pure targets (d≈0.95): they accumulate points slowly, face barriers without benefiting from coordination function, experience maximal extraction. Middle-income international students are mixed victims and constrained agents (d≈0.70): some alternatives exist but costly, face asymmetric barriers. Universities and employers are beneficiaries with arbitrage capacity (d≈0.05): they solve coordination problems while capturing rents. Government is institutional beneficiary (d≈0.10): gains both revenue and labor market control. The government labor market planning actor has lower d (0.30-0.40) because their structural role is managing transition, not capturing extraction — they theoretically stand to lose benefit as domestic workforce reskills. The directionality derivation chain: beneficiary status + arbitrage/mobile exit → low d → negative f(d) for universities/employers; victim status + trapped/constrained exit → high d → high f(d) for students. This produces the perspectival gap: same constraint yields rope for beneficiaries, tangled rope for middle agents, snare for victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the system genuinely performs both coordination and extraction simultaneously. The coordination function (directing talent to labor shortages) is not fakery — UK genuinely faces shortages in identified occupations, and the system does allocate students toward these areas. The extraction is not secondary or incidental — the system systematically advantages shortage fields while disadvantaging non-shortage fields through points barriers that don't reflect labor demand signals but rather policy priorities. The tangled_rope classification prevents the false resolution of 'it's really a snare, the coordination function is fake' or 'it's really a rope, the extraction is just distributional fairness.' Both are structural features. The constraint requires active enforcement (points calculation, salary verification, institution tier maintenance) and exhibits both asymmetric extraction and coordination function. The theater ratio (0.48) reflects that the points-based framing creates appearance of technical objectivity that obscures policy discretion, but not so completely as to constitute piton-level degradation. The system works technically (it allocates visa slots), which distinguishes it from piton (where the system persists despite failing functionally). The mandatrophy resolution: this is tangled rope from the analytical perspective because the institutional coordination function and the asymmetric extraction cannot be separated — they are two dimensions of the same policy mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shortage_occupation_list_discretion,
    'Is the shortage occupations list determined by genuine labor market signals or by policy discretion disguised as technical metrics?',
    'Comparison of occupations on shortage list vs. actual vacancy rates, wage premiums, and employer vacancy duration data; analysis of how list changed across political cycles',
    'If technically determined: system coordinates labor supply to genuine needs (stronger rope function). If discretionary: list becomes policy tool for labor control, extractiveness increases (stronger snare function for non-list fields).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(shortage_occupation_list_discretion, empirical, 'Whether shortage list reflects genuine labor market signals or policy discretion').

omega_variable(
    salary_threshold_extraction,
    'Do salary thresholds for points accumulation primarily filter for capability or primarily exclude lower-income international students?',
    'Longitudinal analysis of graduate salaries by field and visa category; correlation between threshold levels and student rejection rates by country of origin; comparison with wage distribution for domestic graduates in same fields',
    'If primarily capability filter: points system is proportionate coordination mechanism. If primarily exclusionary: threshold functions as proxy for wealth-screening, amplifying extractiveness for students from middle-income countries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(salary_threshold_extraction, empirical, 'Whether salary thresholds filter for capability or exclude by income').

omega_variable(
    labor_market_sunset_credibility,
    'Is the government labor market planning sunset clause (domestic reskilling will reduce need for international postgraduates) operationally credible or aspirational?',
    'Track government investment in domestic STEM/shortage-field training; measure year-over-year change in domestic pipeline to shortage occupations; assess whether shortage occupation list actually narrows as domestic supply increases',
    'If credible: scaffold perspective valid, system has genuine sunset mechanism. If aspirational: scaffold is rhetorical, system is de facto permanent snare for non-shortage students, extractiveness should be reclassified upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_sunset_credibility, empirical, 'Whether domestic reskilling will reduce international postgraduate visa dependence').

omega_variable(
    institutional_revenue_dependence,
    'How much does institutional extraction revenue depend on international postgraduate student fees, and what proportion of that depends on non-shortage-field students?',
    'Financial analysis of university revenue by international postgraduate cohort; disaggregation by field of study; assessment of whether institutions would maintain non-shortage fields without international student revenue',
    'If high dependence: universities have structural interest in maintaining non-shortage-field enrollment despite visa barriers. Coordination function (matching students to labor demand) is subordinate to extraction function (capturing fees). Extractiveness should be reclassified upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_revenue_dependence, empirical, 'Institutional financial dependence on international postgraduate fees').

omega_variable(
    alternative_pathway_sufficiency,
    'Do alternative visa pathways (family sponsorship, work migration post-graduation, doctoral routes) constitute genuine alternatives or rhetorical exits?',
    'Analysis of visa approval rates for non-shortage-field graduates attempting family sponsorship or post-study work routes; assessment of salary and sponsorship requirements; comparison with formal shortage-field pathways',
    'If genuine alternatives: tangled_rope classification is correct, moderate extraction experienced. If rhetorical: alternatives are merely costlier or lower-probability routes, increasing experienced extraction and pushing toward snare classification for many students.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_pathway_sufficiency, empirical, 'Whether alternative visa pathways are genuine or rhetorical alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_postgraduate_visa_points_system, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukpgvisa_tr_t0, uk_postgraduate_visa_points_system, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ukpgvisa_tr_t3, uk_postgraduate_visa_points_system, theater_ratio, 3, 0.45).
narrative_ontology:measurement(ukpgvisa_tr_t6, uk_postgraduate_visa_points_system, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(ukpgvisa_be_t0, uk_postgraduate_visa_points_system, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ukpgvisa_be_t3, uk_postgraduate_visa_points_system, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(ukpgvisa_be_t6, uk_postgraduate_visa_points_system, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_postgraduate_visa_points_system, resource_allocation).
narrative_ontology:boltzmann_floor_override(uk_postgraduate_visa_points_system, 0.18).
narrative_ontology:affects_constraint(uk_postgraduate_visa_points_system, uk_higher_education_international_student_dependence).
narrative_ontology:affects_constraint(uk_postgraduate_visa_points_system, shortage_occupation_definition_mechanism).

% DUAL FORMULATION NOTE:
% The postgraduate visa points system is downstream of broader UK immigration policy framework but represents a distinct constraint with its own extractiveness trajectory. Upstream constraints (immigration skill tiers, salary threshold mechanisms) have their own ε values; this constraint models the hybrid coordination-extraction function specific to postgraduate education pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
