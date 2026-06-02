% ============================================================================
% CONSTRAINT STORY: international_student_economic_impact
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_international_student_economic_impact, []).

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
 *   constraint_id: international_student_economic_impact
 *   human_readable: International Student Economic Impact and Institutional Extraction
 *   domain: higher_education/economic_policy
 *
 * SUMMARY:
 *   International student enrollment in higher education creates a structural
 *   constraint where host institutions extract significant revenue from
 *   students facing barriers to credential portability and geographic
 *   mobility. The constraint is a hybrid of genuine coordination
 *   (institutions need tuition revenue to maintain research and subsidize
 *   domestic students) and asymmetric extraction (international students bear
 *   disproportionate costs while bearing constraints they didn't create). The
 *   classification varies dramatically by perspective: international students
 *   experience a snare (trapped by visa and credential barriers);
 *   institutions experience rope (coordination mechanism); the reform
 *   movement sees a temporary problem with a sunset (scaffold); the soft
 *   power narrative is performative and degraded (piton); and the analytical
 *   observer risks naturalizing contingent policy choices as inevitable
 *   features of education itself. Base extractiveness has increased over the
 *   measurement interval (0.35 → 0.58) as institutions have become more
 *   dependent on international tuition and tuition differentials have
 *   widened. Theater ratio has also increased (0.48 → 0.65), indicating that
 *   the coordination rationale ('we need this revenue') is increasingly
 *   accompanied by performative rhetoric ('international education enriches
 *   campus culture') that obscures the extraction mechanism.
 *
 * KEY AGENTS:
 *   - International Students: Primary victims (powerless/trapped) — face visa constraints, credential recognition barriers, geographic mobility tax, discrimination; bear full cost of tuition premium and unable to exit
 *   - Host Institution Administration: Primary beneficiary (institutional/arbitrage) — captures international tuition revenue differential; can adjust enrollment mix; experiences constraint as coordination
 *   - Domestic Students and Families: Secondary victims (moderate/constrained) — benefit from institutions' economies of scale but face reduced access and quality dilution
 *   - Local Service Sector: Secondary beneficiary (organized/arbitrage) — benefits from international student spending; can serve alternative customer bases
 *   - Education Reform Advocates: Organized coalition (organized/constrained) — see the constraint as temporary problem with sunset mechanisms; building credential portability and online alternatives
 *   - Government Officials/Narrativizers: Institutional actors (institutional/arbitrage) — maintain soft power framing; benefit from enrollment as proxy for influence; see own rhetoric as degraded
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy choices as inevitable limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(international_student_economic_impact, 0.58).
domain_priors:suppression_score(international_student_economic_impact, 0.52).
domain_priors:theater_ratio(international_student_economic_impact, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(international_student_economic_impact, extractiveness, 0.58).
narrative_ontology:constraint_metric(international_student_economic_impact, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(international_student_economic_impact, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(international_student_economic_impact, tangled_rope).
narrative_ontology:human_readable(international_student_economic_impact, "International Student Economic Impact and Institutional Extraction").
narrative_ontology:topic_domain(international_student_economic_impact, "higher_education/economic_policy").

domain_priors:requires_active_enforcement(international_student_economic_impact).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(international_student_economic_impact, host_institution_administration).
narrative_ontology:constraint_beneficiary(international_student_economic_impact, domestic_student_savings).
narrative_ontology:constraint_beneficiary(international_student_economic_impact, local_service_sector).
narrative_ontology:constraint_victim(international_student_economic_impact, international_students).
narrative_ontology:constraint_victim(international_student_economic_impact, domestic_student_access).
narrative_ontology:constraint_victim(international_student_economic_impact, academic_quality_standards).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERNATIONAL STUDENT (SNARE) — Trapped by visa constraints, language barriers, credential recognition issues, and sunk costs. Cannot exit without losing tuition investment and educational credentials. Bears full extraction through inflated tuition, predatory housing, credential undervaluation, and discrimination in employment. No alternatives for legitimate pathway to credential in host country.
constraint_indexing:constraint_classification(international_student_economic_impact, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMESTIC STUDENTS (TANGLED ROPE) — Benefit from institutions' economies of scale and subsidized research enabled by international student revenue, but face reduced access and quality dilution from over-enrollment. Constrained by geographic attachment and credential requirements. Experience both genuine coordination (shared campus resources) and extraction (tuition cross-subsidization).
constraint_indexing:constraint_classification(international_student_economic_impact, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HOST INSTITUTION ADMINISTRATION (ROPE) — Primary beneficiary. Experiences constraint as coordination mechanism: tuition revenue funds research, enables domestic student scholarships, subsidizes operational costs. Can exit by admitting different student mix (arbitrage option). Net beneficiary with high institutional agency. Classification reflects genuine coordination function alongside asymmetric extraction.
constraint_indexing:constraint_classification(international_student_economic_impact, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LOCAL SERVICE SECTOR (ROPE) — Secondary beneficiary (organized retailers, landlords, transport services). Experiences genuine coordination: international student spending sustains local economy. Can exit by other customer bases (arbitrage). Extraction is limited because service providers compete and international students have some mobility between providers.
constraint_indexing:constraint_classification(international_student_economic_impact, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: EDUCATION REFORM MOVEMENT (SCAFFOLD) — Organized advocates see the constraint as a temporary coordination failure with sunset mechanisms: credential portability agreements, international tuition regulation, and distributed education models (online degrees, branch campuses in origin countries) are building alternatives. These reforms have explicit sunset logic — as they mature, the reliance on geographic mobility diminishes. Constrained by institutional resistance but sees clear exit pathway.
constraint_indexing:constraint_classification(international_student_economic_impact, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SOFT POWER NARRATIVE (PITON) — Government officials and administrators invoke international education as soft power and cultural exchange, but the actual function has degraded: institutions maximize revenue extraction rather than cultural understanding; students are priced out of genuine cultural participation; propaganda purposes are abandoned in favor of pure revenue. The narrative persists through institutional inertia and occasional rhetorical invocation, but the real coordination function (cultural diplomacy) has atrophied. Theater ratio reflects this performative maintenance.
constraint_indexing:constraint_classification(international_student_economic_impact, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks framing the constraint as inevitable: 'Education requires geographic mobility,' 'credential verification requires centralized institutions,' 'tuition variation reflects real cost differences.' These framings naturalize contingent institutional arrangements. The constraint appears unchangeable from within the framing, but structural decomposition reveals this is a false summit — the arrangement is contingent on visa policy, credential monopolies, and institutional revenue models, all of which are chosen.
constraint_indexing:constraint_classification(international_student_economic_impact, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(international_student_economic_impact_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(international_student_economic_impact, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(international_student_economic_impact, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(international_student_economic_impact, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(international_student_economic_impact, TR),
    TR >= 0.70.

:- end_tests(international_student_economic_impact_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting genuine asymmetry in tuition and experience cost, but not maximal because institutions do provide legitimate educational services. The international student premium is partially real (operating costs, subsidies) and partially extractive (revenue maximization beyond cost). Suppression (0.52): Moderate-high. International students face real structural barriers (visa policy, credential non-recognition, language requirements) that limit exit options, but these barriers are policy-created, not inevitable. Theater ratio (0.65): Indicates significant performative content — institutions invoke 'diversity' and 'cultural enrichment' to justify enrollment that is primarily motivated by revenue. The theater has increased over time as the rhetorical justification has become more elaborate relative to the actual coordination function. Claimed type (tangled_rope): Institution genuinely coordinates some resources through tuition revenue while asymmetrically extracting from trapped agents. Both functions are real, both require active enforcement (visa compliance, credential barriers), and both produce measurable costs and benefits.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The international student's snare is a fundamentally different structural reality from the institution's rope. Both are correct from their respective positions, but they imply contradictory answers to 'is this exploitation or coordination?' The resolution is that it is both: genuine coordination (institutions need the revenue) coupled with asymmetric extraction (students bear the costs). The tangled rope classification captures this hybrid. The scaffold perspective adds a crucial diagnostic: the constraint's extractiveness horizon is finite and policy-determined. As credential portability and online education mature, the geographic mobility tax disappears. This timeline is empirical and measurable, not philosophical.
 *
 * DIRECTIONALITY LOGIC:
 *   International students face maximum directionality (d approaching 1.0) because they are trapped victims with no exit options. Institutions face minimum directionality (d approaching 0.0) because they are beneficiaries with arbitrage options. Domestic students face intermediate directionality (d ≈ 0.5-0.6) because they experience both coordination benefits (scale economics) and extraction costs (reduced access). The local service sector faces low directionality (d ≈ 0.2-0.3) because they benefit and have arbitrage alternatives. Reform advocates face constrained mobility (d ≈ 0.55) because they have partial exit options (work on alternatives) but are constrained by institutional resistance. The piton perspective uses arbitrage (d ≈ 0.0) because the narrativizer can exit by changing rhetoric without institutional consequence.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED via perspectival decomposition. The constraint is neither pure rope (coordination) nor pure snare (extraction), but tangled rope with a measurable sunset. The mandatrophy dissolves when we recognize: (1) institutions genuinely coordinate resources (rope function is real), (2) international students genuinely experience extraction (snare function is real), and (3) the extraction mechanism is contingent on policy choices that are being actively disrupted (scaffold function is real). The soft power narrative (piton) is performative, confirming that the coordination rationale has become theater — the real function is revenue, which is extraction. The analytical observer's mountain (inevitable feature of education) is false: the mobility tax is policy-created and policy-removable. The constraint resolves the mandatrophy by showing that hybrid classification is correct when the coordination and extraction mechanisms are genuinely coupled, and the perspectival gaps are real differences in structural position, not differences in observation quality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_access_causality,
    'Does international student enrollment actually reduce domestic student access, or do declining domestic applications and reduced public funding precede international recruitment?',
    'Longitudinal analysis of domestic application rates, public funding trends, and international enrollment timing; counterfactual institutional behavior if international tuition revenues were unavailable',
    'If international enrollment causes reduced domestic access: extraction is primary (snare/tangled_rope confirmed). If reduced funding causes increased international recruitment: extraction is secondary to public disinvestment (scaffold/rope classification strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_access_causality, empirical, 'Causal relationship between international and domestic student access').

omega_variable(
    credential_portability_timeline,
    'How long before credential portability agreements and online education models reduce the geographic mobility tax sufficiently to break the extraction mechanism?',
    'Tracking adoption rates of bilateral degree recognition, employer acceptance of online credentials, and wage differentials for geographically portable vs immobile credentials',
    'If portability matures within 10 years: scaffold sunset is realistic, constraining extractiveness horizon. If adoption stalls beyond 20 years: scaffold perspective is aspirational, extraction may entrench.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credential_portability_timeline, empirical, 'Timeline for credential portability maturation').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression of international student alternatives primarily structural (visa/credential barriers) or internalized (students internalize perception of geographic mobility tax as inevitable)?',
    'Post-graduation analysis: do students who exit the system (return to origin country) perceive the suppression as structural or inevitable? Do they encourage others to pursue the same path despite barriers?',
    'If structural: barrier removal changes behavior. If internalized: students may perpetuate extraction themselves even after barriers are removed (identity-locked mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in student perception').

omega_variable(
    tuition_cross_subsidization_magnitude,
    'What proportion of domestic student scholarships and research funding is genuinely subsidized by international student tuition premium, versus the institution''s administrative choice to allocate revenue that way?',
    'Institutional cost accounting and counterfactual budgeting: if international tuition were subject to same rate as domestic tuition, how would the institution reallocate remaining revenue?',
    'If cross-subsidy is necessary: rope coordination is genuine (institutions must extract from international students to serve domestic ones). If discretionary: the coordination function is theater, and the constraint is snare for international students.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tuition_cross_subsidization_magnitude, empirical, 'Magnitude and necessity of tuition cross-subsidization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(international_student_economic_impact, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(intl_stud_tr_t0, international_student_economic_impact, theater_ratio, 0, 0.48).
narrative_ontology:measurement(intl_stud_tr_t5, international_student_economic_impact, theater_ratio, 5, 0.58).
narrative_ontology:measurement(intl_stud_tr_t10, international_student_economic_impact, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(intl_stud_be_t0, international_student_economic_impact, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(intl_stud_be_t5, international_student_economic_impact, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(intl_stud_be_t10, international_student_economic_impact, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(international_student_economic_impact, resource_allocation).
narrative_ontology:boltzmann_floor_override(international_student_economic_impact, 0.18).
narrative_ontology:affects_constraint(international_student_economic_impact, credential_portability_barriers).
narrative_ontology:affects_constraint(international_student_economic_impact, public_higher_education_disinvestment).
narrative_ontology:affects_constraint(international_student_economic_impact, visa_regime_restrictions).

% DUAL FORMULATION NOTE:
% International student economic impact is downstream of public funding decline and visa policy, but represents a distinct structural constraint. The upstream constraints (disinvestment, visa restrictions) enable the extraction mechanism; the international student constraint has its own extractiveness reflecting the tuition asymmetry and geographic mobility tax. Decompose into separate stories if analyzing credential portability barriers independently (which have different ε reflecting the pure policy dimension).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(international_student_economic_impact, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
