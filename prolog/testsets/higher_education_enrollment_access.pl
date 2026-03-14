% ============================================================================
% CONSTRAINT STORY: higher_education_enrollment_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_higher_education_enrollment_access, []).

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
 *   constraint_id: higher_education_enrollment_access
 *   human_readable: Higher Education Enrollment Access Constraint
 *   domain: education/economic_mobility
 *
 * SUMMARY:
 *   Higher education enrollment access operates as a mixed
 *   coordination-extraction mechanism spanning 50+ years (1970s-present).
 *   Universities coordinate access to credential and social mobility (genuine
 *   function) while simultaneously extracting through tuition cost-shifting,
 *   selectivity gatekeeping, and debt-financed financing. The constraint
 *   exhibits all six DR types because different agents experience
 *   fundamentally different structural relationships to it: low-income
 *   students experience it as immutable extraction (snare); regional public
 *   universities experience it as mixed coordination-extraction with
 *   institutional incentives misaligned (tangled rope); elite institutions
 *   experience it as pure coordination (rope); organized advocates experience
 *   it as a temporary problem with policy solutions (scaffold); standardized
 *   testing sees itself as degraded gatekeeping ritual (piton); and the
 *   analytical observer risks naturalizing institutional barriers as
 *   immutable educational scarcity (false mountain). The extractiveness has
 *   risen from 0.35 to 0.58 over 50 years, driven by cost-shifting from
 *   public to students (rising tuition, declining state funding) and
 *   increasing role of student debt as a financing mechanism. Theater ratio
 *   rising (0.40 to 0.55) reflects growing performative elements:
 *   standardized tests, rankings-driven selectivity, and credentialing
 *   inflation that decouples from labor market needs.
 *
 * KEY AGENTS:
 *   - Low-income students: Primary victims (powerless/trapped) — structurally excluded or debt-loaded; no exit options
 *   - First-generation applicants: Primary victims (powerless/identity_locked) — face information asymmetries, may internalize exclusion as personal inadequacy
 *   - Regional public universities: Mixed status (moderate/constrained) — genuinely provide access but increasingly incentivized toward selectivity and revenue extraction
 *   - Elite research universities: Primary beneficiaries (institutional/arbitrage) — capture prestige and funding through selective enrollment; experience constraint as coordination
 *   - Student debt processors (lenders, servicers): Secondary beneficiaries (institutional/arbitrage) — extract through debt servicing fees, interest, and loan origination
 *   - Credential gatekeepers (employers, professional licensing): Beneficiaries (institutional/arbitrage) — benefit from degree scarcity signaling and exclusivity maintenance
 *   - Student debt reform coalition: Organized agents (organized/mobile) — see sunset through policy change; building alternative pathways (debt cancellation, free public higher ed)
 *   - Standardized testing industry: Institutional (institutional/arbitrage) — maintains gatekeeping function despite degraded validity; beneficiary from enrollment filtering
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(higher_education_enrollment_access, 0.58).
domain_priors:suppression_score(higher_education_enrollment_access, 0.62).
domain_priors:theater_ratio(higher_education_enrollment_access, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(higher_education_enrollment_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(higher_education_enrollment_access, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(higher_education_enrollment_access, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(higher_education_enrollment_access, tangled_rope).
narrative_ontology:human_readable(higher_education_enrollment_access, "Higher Education Enrollment Access Constraint").
narrative_ontology:topic_domain(higher_education_enrollment_access, "education/economic_mobility").

domain_priors:requires_active_enforcement(higher_education_enrollment_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(higher_education_enrollment_access, elite_institutions).
narrative_ontology:constraint_beneficiary(higher_education_enrollment_access, credential_gatekeepers).
narrative_ontology:constraint_beneficiary(higher_education_enrollment_access, student_debt_processors).
narrative_ontology:constraint_victim(higher_education_enrollment_access, low_income_students).
narrative_ontology:constraint_victim(higher_education_enrollment_access, first_generation_applicants).
narrative_ontology:constraint_victim(higher_education_enrollment_access, geographic_periphery_applicants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME STUDENT (SNARE) — Structurally trapped by cost barriers (tuition, housing, opportunity cost of not working), information asymmetries (first-generation students lack family knowledge of application processes), and geographic disadvantage (limited quality secondary schools in low-income areas). No genuine exit option; bears full extraction. The constraint appears as immutable: 'higher education costs what it costs; if you can't afford it, you're not college material.'
constraint_indexing:constraint_classification(higher_education_enrollment_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL PUBLIC UNIVERSITY (TANGLED ROPE) — Provides genuine coordination function (access to higher education, workforce development, social mobility channel) but simultaneously extracts through tuition cost-shifting, performance metrics that incentivize selectivity over access, and reliance on contingent labor that prevents genuine student support. Can exit state control through privatization but faces reputational and regulatory costs. Mixed beneficiary-victim status: genuinely serves students but increasingly treats enrollment as revenue extraction.
constraint_indexing:constraint_classification(higher_education_enrollment_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ELITE RESEARCH UNIVERSITY (ROPE) — Experiences enrollment access constraint as pure coordination problem: they solve it (accept qualified students, provide scholarships) while capturing benefits (prestige, tuition revenue, research funding tied to enrolled cohorts). Low perceived extraction because benefits flow toward them; they see themselves as solving the access problem, not creating barriers. High arbitrage capability: can shift between enrollment models, funding sources, international student ratios without structural consequence.
constraint_indexing:constraint_classification(higher_education_enrollment_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STUDENT DEBT REFORM COALITION (SCAFFOLD) — Organized advocates (NAACP, CUNY students, progressive legislators) see enrollment access as a solvable coordination problem with a sunset: debt cancellation, tuition-free public higher education, and income-based repayment create alternative pathways that bypass the extraction mechanism. Temporary suppression is tolerated because the coalition has agency and perceives an exit date through policy change. Sunset logic: if federal policy shifts toward free public higher education (generational timescale), the debt-based extraction mechanism loses force.
constraint_indexing:constraint_classification(higher_education_enrollment_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: STANDARDIZED TESTING APPARATUS (PITON) — SAT/ACT testing creates performative gatekeeping function with degraded utility: test scores correlate primarily with family income and test prep access, not with college success. The apparatus persists through institutional inertia (universities anchor hiring/admissions on scores; test companies lobby; guidance counselors default to it) despite low functional validity. Theater ratio high because the ritual of taking standardized tests feels consequential but actually measures wealth proxy, not academic readiness. Many institutions have moved test-optional, indicating recognition that the function has atrophied.
constraint_indexing:constraint_classification(higher_education_enrollment_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, some selectivity in higher education access appears immutable: capacity constraints (universities have finite seats), quality variation (not all institutions are equivalent), and effort requirements (completing degrees requires sustained work). This view naturalizes the current constraint as an inherent property of educational scarcity. However, structural data contradicts the mountain classification: the specific barriers (cost, information asymmetry, standardized test gatekeeping) are institutional choices, not natural laws. The constraint is contingent, not immutable — revealed as false summit by the tangled rope classification.
constraint_indexing:constraint_classification(higher_education_enrollment_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(higher_education_enrollment_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(higher_education_enrollment_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(higher_education_enrollment_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(higher_education_enrollment_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(higher_education_enrollment_access, TR),
    TR >= 0.70.

:- end_tests(higher_education_enrollment_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over interval. The constraint exhibits asymmetric extraction: low-income students bear costs (tuition, debt, opportunity cost) while elite institutions and debt processors capture benefits (tuition revenue, prestige, interest revenue). The value reflects that extraction is substantial but not total — genuine access exists through debt financing (extractive but functional), and some scholarship funding creates mobility paths. Rising trajectory (0.35 → 0.58) reflects 50-year cost-shift from public to students via declining state support and rising tuition. Suppression (0.62): High. Multiple barriers operate simultaneously: cost barriers (tuition + living expenses), information asymmetries (first-generation students lack application knowledge), geographic barriers (unequal secondary school quality by region/income), and potentially internalized identity barriers ('not college material'). Suppression is not total because some pathways exist (community college transfers, second-chance admissions), but barriers are significant. Theater ratio (0.55): Moderate-high and rising (0.40 → 0.55). The constraint has increasingly performative elements: standardized testing correlates with family income more than college success; university rankings drive selectivity that doesn't correlate with learning outcomes; credential inflation where bachelor's degree is now baseline despite unchanged job skill requirements. Core coordination function (access to higher education, workforce development) remains genuine, but theater has grown as selectivity and branding have become institutional priorities.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and reveals the constraint's hybrid nature. Low-income students experience immutable extraction (snare) — barriers feel absolute from inside trapped position. Regional public universities experience genuine mixed function (tangled rope) — they provide access while simultaneously being incentivized toward cost-shifting and selectivity that reduces access. Elite institutions experience pure coordination (rope) — they solve 'how to select and educate the best students' while capturing all the benefits of that solution. Open-science coalition of advocates experiences solvable temporary problem (scaffold) — policy can redirect funding, cancel debt, and create tuition-free pathways. The analytical observer at civilizational scale risks seeing natural scarcity (false mountain) — 'only so many elite slots available' — that naturalizes institutional choices as inevitable. The standardized testing apparatus experiences itself as degraded ritual (piton) — admissions staff often admit it doesn't predict success but default to it due to inertia. No single perspective is wrong; the perspectival presheaf IS the full description of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position: power level, exit options, and relationship to extraction flow. Low-income students have d ≈ 0.95 (full target, trapped, powerless) → maximum experienced extraction. First-generation applicants may have d ≈ 0.88-0.95 depending on whether barriers are purely structural (trapped) or partially internalized (identity_locked) — identity_locked agents can theoretically perceive barrier mutability but cannot exercise exit from within their framing. Regional public universities have d ≈ 0.50-0.55 (mixed: constrained exit, both beneficiary and victim status) → moderate experienced extraction. Elite institutions have d ≈ 0.10-0.20 (beneficiary, institutional power, arbitrage exit) → low or negative experienced extraction. Reform coalition has d ≈ 0.35-0.45 (organized agents, mobile exit through policy change) → moderate experienced extraction. The derived directionality explains why elite universities don't perceive the constraint as problematic (low d → low χ) while low-income students perceive it as crushing (high d → high χ) despite identical base extractiveness (0.58).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint avoids mandatrophy by recognizing that all six types are legitimate perspectival readings. The apparent contradiction ('is this tangled rope or snare?') is resolved by observing that it IS both—it is snare from the powerless agent's structural position and tangled rope from the institutional perspective. The false mountain perspective (analytical observer naturalizing barriers as scarcity) is flagged as pseudo-classification: the engine's false summit detector identifies it as unfounded because the structural data shows institutional choices (cost-shifting, selectivity incentives) rather than immutable scarcity. The coordination function (enabling higher education access) is genuine and persists across all perspectives. The extraction layer (cost-shifting, debt financing, credential gatekeeping) is also genuine and asymmetric. They coexist in a hybrid that is structurally snare for trapped agents, tangled rope for constrained institutional actors, and rope for beneficiaries. The mandatrophy is resolved by the indexical tuple: power + exit + time + scope fully determine what the constraint IS from each position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_barrier_vs_selectivity_mechanism,
    'Does the enrollment constraint operate primarily through cost barriers or through selectivity gatekeeping?',
    'Comparative analysis: tuition-free public higher education systems (Germany, Scandinavia) with equivalent selectivity vs cost-gated systems; measurement of enrollment patterns when cost barriers are removed but selectivity maintained',
    'If cost-primary: constraint is extractive (snare). If selectivity-primary: constraint is coordination (rope/mountain). Current mixed operation suggests tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_barrier_vs_selectivity_mechanism, empirical, 'Whether cost or selectivity is the dominant barrier mechanism').

omega_variable(
    information_asymmetry_persistence,
    'Do information asymmetries (first-generation students lack application knowledge) persist or dissolve with improved counseling and transparency?',
    'Measurement of enrollment rates for first-generation students before/after intervention programs (GEAR UP, College Advising Corps); statistical control for family income',
    'If persistent after information provision: barrier is structural (cost + selection). If dissolves: barrier is information-asymmetry-driven and solvable through transparency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_persistence, empirical, 'Whether information asymmetries are the binding constraint').

omega_variable(
    debt_mechanism_sustainability,
    'Is student debt a necessary financing mechanism or a revenue extraction layer that could be replaced by public funding?',
    'International comparison of higher education funding models; historical analysis of U.S. shift from public to private cost-bearing (1970s-present); fiscal modeling of tuition-free scenarios',
    'If necessary: constraint reflects real resource scarcity (mountain/rope). If replaceable: current debt mechanism is institutional choice enabling extraction (snare/tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_mechanism_sustainability, empirical, 'Whether student debt financing is structurally necessary').

omega_variable(
    identity_lock_in_meritocratic_framing,
    'Do low-income and first-generation students internalize the ''not college material'' framing, making exit psychologically impossible despite structural mobility?',
    'Longitudinal psychometric study tracking self-efficacy beliefs, educational aspirations, and internalized stigma among control vs treatment groups (enhanced support/visibility of pathways)',
    'If identity-locked: suppression mechanism is partially internalized; constraint persists even after cost barriers fall. If not identity-locked: suppression is purely structural (cost + gatekeeping).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_meritocratic_framing, empirical, 'Whether exclusion is internalized as identity or experienced as external barrier').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(higher_education_enrollment_access, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heea_tr_t0, higher_education_enrollment_access, theater_ratio, 0, 0.4).
narrative_ontology:measurement(heea_tr_t25, higher_education_enrollment_access, theater_ratio, 25, 0.48).
narrative_ontology:measurement(heea_tr_t50, higher_education_enrollment_access, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(heea_be_t0, higher_education_enrollment_access, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(heea_be_t25, higher_education_enrollment_access, base_extractiveness, 25, 0.5).
narrative_ontology:measurement(heea_be_t50, higher_education_enrollment_access, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(higher_education_enrollment_access, resource_allocation).
narrative_ontology:affects_constraint(higher_education_enrollment_access, student_debt_accumulation).
narrative_ontology:affects_constraint(higher_education_enrollment_access, credential_inflation_wage_premium).
narrative_ontology:affects_constraint(higher_education_enrollment_access, labor_market_screening_signaling).

% DUAL FORMULATION NOTE:
% Higher education enrollment access decomposes into three structurally distinct constraints: (1) enrollment_access_coordination (ε ≈ 0.25, Rope) — the genuine function of providing educational access; (2) cost_barrier_extraction (ε ≈ 0.65, Snare) — tuition and financing mechanisms; (3) selectivity_gatekeeping (ε ≈ 0.40, Tangled Rope) — institutional ranking competition driving exclusivity. The unified story uses tangled_rope to capture the hybrid, but decomposition into three stories would allow separate analysis of each mechanism's extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(higher_education_enrollment_access, moderate, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
