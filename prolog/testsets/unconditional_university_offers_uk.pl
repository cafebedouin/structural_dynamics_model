% ============================================================================
% CONSTRAINT STORY: unconditional_university_offers_uk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_university_offers_uk, []).

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
 *   constraint_id: unconditional_university_offers_uk
 *   human_readable: Use of Unconditional Offers in UK University Admissions
 *   domain: economic/social
 *
 * SUMMARY:
 *   Following the 2015 removal of student number caps, UK universities
 *   entered a more competitive recruitment market. Research-intensive
 *   institutions began issuing unconditional offers to secure enrollment
 *   pipelines, particularly targeting students from state schools,
 *   lower-income backgrounds, and geographic regions without strong
 *   university presence. This constraint exemplifies the tangled rope
 *   pattern: universities solved a real coordination problem (enrollment
 *   uncertainty in a liberalized market) while simultaneously creating
 *   extractive mechanisms that harm disadvantaged students through
 *   information asymmetry and misaligned incentives. The unconditional offer
 *   functionally replaced the A-level conditional system, which had performed
 *   coordination work by signaling mutual expectations. The new system
 *   generates benefits (universities gain enrollment security; some
 *   lower-income students gain university access) while imposing costs (many
 *   enrolled students experience poor outcomes; the admissions signal
 *   degrades; institutional theater replaces genuine quality signaling).
 *
 * KEY AGENTS:
 *   - Research-Intensive Universities: Primary beneficiary (institutional/arbitrage) — secure enrollment numbers and revenue in competitive market
 *   - State School Applicants: Primary victim (powerless/trapped) — targeted by aggressive unconditional offers; lack information to evaluate quality; bear cost of poor outcomes
 *   - Lower-Income Students: Secondary victim (moderate/constrained) — face information asymmetry; benefit from some access but enrolled in mismatched institutions
 *   - Teaching-Focused Universities: Secondary beneficiary (organized/constrained) — participate in extraction but face regulatory pressure limiting sustainability
 *   - Office of Students / Regulatory Bodies: Analytical observer (organized/mobile) — identify market failure and attempt intervention through metrics and enforcement
 *   - Admissions System (Structural): Piton actor (institutional/arbitrage) — prior conditional system degraded; unconditional offer theater persists through institutional inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_university_offers_uk, 0.52).
domain_priors:suppression_score(unconditional_university_offers_uk, 0.58).
domain_priors:theater_ratio(unconditional_university_offers_uk, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_university_offers_uk, extractiveness, 0.52).
narrative_ontology:constraint_metric(unconditional_university_offers_uk, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(unconditional_university_offers_uk, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_university_offers_uk, tangled_rope).
narrative_ontology:human_readable(unconditional_university_offers_uk, "Use of Unconditional Offers in UK University Admissions").
narrative_ontology:topic_domain(unconditional_university_offers_uk, "economic/social").

domain_priors:requires_active_enforcement(unconditional_university_offers_uk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_university_offers_uk, research_intensive_universities).
narrative_ontology:constraint_beneficiary(unconditional_university_offers_uk, elite_institution_prestige).
narrative_ontology:constraint_victim(unconditional_university_offers_uk, state_school_applicants).
narrative_ontology:constraint_victim(unconditional_university_offers_uk, lower_income_students).
narrative_ontology:constraint_victim(unconditional_university_offers_uk, admissions_system_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE SCHOOL APPLICANT (SNARE) — Students from schools without institutional connections face maximum extraction. Unconditional offers target these students aggressively; they are unable to resist, lack information about the true selectivity of institutions, and bear the cost of low-quality degree outcomes. No exit once enrolled.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOWER-INCOME STUDENT (TANGLED ROPE) — Benefits from some access to higher education and financial support infrastructure. Constrained by limited geographic mobility and information access. Experiences extraction through enrollment in lower-tier institutions via unconditional offers, but also coordinates via league tables and institutional reputation signals. Mixed benefits and costs.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RESEARCH-INTENSIVE UNIVERSITY (ROPE) — Primary beneficiary. Uses unconditional offers to secure student recruitment pipelines and guarantee enrollment numbers. Experiences the constraint as coordination: early commitment secures revenue, reduces recruitment uncertainty. Net beneficiary with full exit options.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TEACHING-FOCUSED UNIVERSITY (SCAFFOLD) — Uses unconditional offers strategically but faces sunset pressure. Regulatory scrutiny (Office of Students TEF metrics, teaching quality standards) increasingly penalizes poor outcomes. Teaching-quality focus requires genuine student-institution fit, making unconditional offers less sustainable. Sees constraint as temporary, with exit path through market repositioning.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ADMISSIONS SYSTEM (PITON) — The prior A-level conditional offer system performed a real coordination function: students knew their admission depended on exam performance; universities knew their intake quality. Unconditional offers degraded this function — the signal has atrophied, replaced by theater: universities claim to identify 'high-potential' students but lack reliable criteria. The system persists through institutional inertia despite low functionality.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / REGULATORY PERSPECTIVE (TANGLED ROPE) — The Office of Students and policy analysts see genuine coordination failure requiring intervention. Unconditional offers solve universities' enrollment security problem but create information asymmetry and misalignment of incentives. The emerging regulatory frameworks (student outcome metrics, teaching quality metrics) attempt to restore functionality through enforcement. This perspective combines both coordination gap analysis and extractive structure identification.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_university_offers_uk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unconditional_university_offers_uk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unconditional_university_offers_uk, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_university_offers_uk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unconditional_university_offers_uk, TR),
    TR >= 0.70.

:- end_tests(unconditional_university_offers_uk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Universities extract enrollment security and revenue through the mechanism of unconditional offers. The extraction is not absolute (some lower-income students genuinely benefit from expanded access) but is substantial (many experience poor degree outcomes; information asymmetry enables mismatching). The value reflects the genuine coordination benefit (universities' market problem solved) balanced against the extraction harm (student misalignment costs). The metric shows measurable increase from 0.28 in 2015 (post-cap removal, before widespread unconditional offer adoption) to 0.52 by 2021 (peak adoption period), indicating accelerating extraction as universities optimized the mechanism. Suppression (0.58): Moderate-high. Significant barriers to resistance include information asymmetry (state school students lack league table literacy), geographic isolation (limited awareness of alternatives), financial pressure (lower-income students cannot easily reject offers), and lack of institutional advocacy. However, suppression is not total — some students do research institutions carefully, and regulatory pressure is beginning to reduce it. Theater ratio (0.64): Moderately high. Unconditional offers function as theater: universities claim to identify 'high-potential' students who will thrive, but the offers are actually enrollment-securing mechanisms with weak predictive basis. The admissions committees perform assessment rituals without genuine quality signal transmission. The theater has increased over the interval as universities refined the mechanism and regulatory scrutiny forced justification narratives.
 *
 * PERSPECTIVAL GAP:
 *   The research-intensive university sees Rope (solving genuine coordination problem of enrollment uncertainty). The teaching-focused university sees Scaffold (temporary solution with regulatory sunset). The state school applicant sees Snare (pure extraction with no exit). The analytical observer sees Tangled Rope (hybrid mechanism requiring intervention). The admissions system sees Piton (prior conditional system degraded, replaced by theater). The perspectival gaps reflect genuine differences in structural position: beneficiaries with exit options experience coordination; trapped victims experience extraction; regulatory observers see the full tangled structure requiring enforcement. The piton classification of the admissions system is the key diagnostic insight — it reveals that the prior A-level conditional system performed real work (alignment of expectations, quality signaling), while unconditional offers replaced function with theater.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality from beneficiary/victim declarations and exit options. Research-intensive universities are beneficiaries with arbitrage options (can walk away, choose enrollment strategies) — they derive low d → negative χ. State school applicants are victims with trapped exit (once enrolled, cannot easily leave) — they derive high d → high χ. The tangled rope classification emerges from the combination: the constraint simultaneously solves a coordination problem (universities' market uncertainty) and creates extraction (student misalignment). Teaching-focused universities occupy an intermediate position: they benefit from the revenue but face regulatory penalties for poor outcomes, making their exit options constrained rather than free. This differentiation in exit options is critical to the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED via tangled rope specification. The unconditional offer constraint initially appears to be pure extraction (Snare: universities extracting enrollment security; students bearing cost of misalignment). However, the constraint genuinely includes a coordination component: universities solved a real problem (post-cap-removal market uncertainty), and some lower-income students gained access who would not have under the prior conditional system. The mandatrophy resolution requires identifying both components simultaneously — this is the tangled rope signature. The extraction is not hidden or incidental; it coexists with genuine coordination benefits. The regulatory response (Office of Students metrics, teaching quality frameworks) reflects this recognition: the intervention does not attempt to eliminate unconditional offers entirely (acknowledging the coordination function) but rather to align institutional incentives through outcome accountability (constraining the extraction component). This hybrid response confirms the tangled rope diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditional_vs_unconditional_outcome_gap,
    'How much of the poor degree outcomes for unconditional offer recipients is attributable to the offer itself versus underlying student capability or institutional quality?',
    'Longitudinal matching analysis: compare outcomes of students who received unconditional vs conditional offers from the same institution, controlling for entry grades and demographic factors',
    'If outcome gap largely attributable to offer: extraction mechanism confirmed (adverse selection). If outcome gap reflects institutional quality: extraction is less severe, more structural (enrollment strategy).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditional_vs_unconditional_outcome_gap, empirical, 'Attribution of outcome disparities to unconditional offers versus institutional factors').

omega_variable(
    information_asymmetry_measurability,
    'Can students from disadvantaged backgrounds actually identify low-quality offers, or is the information asymmetry functionally irreducible?',
    'Survey research on applicant decision-making; analysis of league table awareness and comprehension among state school versus independent school applicants; behavioral economics testing of offer evaluation',
    'If students can identify poor offers: market self-correction possible, constraint is extractive but not immutable. If asymmetry is irreducible: suppression value may be higher, snare classification stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_measurability, empirical, 'Whether information asymmetries can be overcome or are functionally irreducible').

omega_variable(
    regulatory_effectiveness_ceiling,
    'Can regulatory intervention (Office of Students, TEF, OfS metrics) actually eliminate unconditional offer extraction, or will universities find institutional workarounds?',
    'Tracking of unconditional offer volumes post-OfS regulation; analysis of alternative enrollment-securing mechanisms (foundation years, partnership pathways, clearing strategies); interviews with university admissions staff',
    'If regulation effective: constraint is scaffold (sunset real). If universities circumvent: constraint may degrade to piton (regulatory theater). If circumvention reaches parity with original extraction: snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_effectiveness_ceiling, empirical, 'Whether regulatory frameworks can eliminate or will merely redirect unconditional offer extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_university_offers_uk, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uuo_tr_t0, unconditional_university_offers_uk, theater_ratio, 0, 0.35).
narrative_ontology:measurement(uuo_tr_t3, unconditional_university_offers_uk, theater_ratio, 3, 0.5).
narrative_ontology:measurement(uuo_tr_t6, unconditional_university_offers_uk, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(uuo_be_t0, unconditional_university_offers_uk, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(uuo_be_t3, unconditional_university_offers_uk, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(uuo_be_t6, unconditional_university_offers_uk, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_university_offers_uk, resource_allocation).
narrative_ontology:affects_constraint(unconditional_university_offers_uk, a_level_grading_inflation).
narrative_ontology:affects_constraint(unconditional_university_offers_uk, university_league_table_rankings).
narrative_ontology:affects_constraint(unconditional_university_offers_uk, student_debt_burden_uk).

% DUAL FORMULATION NOTE:
% The unconditional offer constraint is downstream of the 2015 student number cap removal, which created market competition, and upstream of student debt burden (higher enrollment of lower-income students in mismatched institutions increases default risk) and league table dynamics (institutions optimize for enrollment at cost of educational quality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_university_offers_uk, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
