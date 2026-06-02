% ============================================================================
% CONSTRAINT STORY: legacy_admission_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legacy_admission_gatekeeping, []).

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
 *   constraint_id: legacy_admission_gatekeeping
 *   human_readable: Legacy Admission Gatekeeping in Elite Universities
 *   domain: education/institutional_access
 *
 * SUMMARY:
 *   Legacy admission preferences in elite US universities create a hybrid
 *   constraint that simultaneously coordinates fundraising and alumni
 *   relationships while extracting opportunity cost from qualified non-legacy
 *   applicants. The constraint operates at the intersection of credential
 *   markets, philanthropic incentive structures, and institutional identity
 *   maintenance. From 2000-2020, base extractiveness rose from 0.42 to 0.58
 *   as competition intensified (global applicant pool expanded while legacy
 *   slots remained constant), theater ratio increased from 0.52 to 0.64 as
 *   universities deployed stronger merit narratives while maintaining legacy
 *   practices, and suppression requirement climbed from 0.62 to 0.68 as
 *   non-legacy applicants required higher test scores and credentials to
 *   compete for reduced slots. The constraint exhibits all six DR types from
 *   different perspectives: pure extraction (powerless non-legacy applicants
 *   facing snare), genuine coordination (university fundraising and alumni
 *   relationships), temporary institutional arrangements solvable by policy
 *   (scaffold view from access-equity coalitions), degraded ritualism (piton
 *   view of merit ideology), mixed coordination-extraction (tangled rope view
 *   from institutional administration), and false naturalization of
 *   contingent choices (mountain view from analytical observers). The
 *   critical empirical questions are whether legacy preferences actually
 *   cause marginal donation flows (causal omega) or merely select for
 *   students already from giving families (spurious correlation), and whether
 *   'institutional culture' is genuinely maintained by legacy composition or
 *   is a post-hoc rationalization that could be rebuilt post-elimination.
 *
 * KEY AGENTS:
 *   - Qualified Non-Legacy Applicants: Powerless/trapped (national scope) — excluded from credential premium without exit options; experience snare classification with maximum chi. Includes majority of applicant pool at elite institutions.
 *   - First-Generation and Low-Income Applicants: Moderate/constrained (national scope) — face layered material and identity barriers; experience snare with moderate chi from constrained exit options. Estimated 15-20% of incoming classes pre-policy reform.
 *   - Elite University Administration: Moderate/mobile (national scope) — coordinate fundraising and alumni relations while restricting access; experience tangled rope with genuine but asymmetric coordination function. Primary institutional agent maintaining constraint.
 *   - Wealthy Alumni Families and Donor Base: Institutional/arbitrage (global scope) — benefit from preference system and institutional fundraising; experience rope with negative or low chi. Source of $500M-$1B annual major gift flows (estimated 40-50% legacy-connected).
 *   - Access-Equity Coalition: Organized/constrained (national scope) — student activists, policy advocates, equity-focused administrators building alternative institutional logics; experience scaffold with sunset pathway. Growing power post-2020 (Harvard/Yale policy changes, federal scrutiny).
 *   - Merit Ideology and Ranking Theater: Institutional/constrained (global scope) — U.S. News selectivity metrics, institutional branding, merit narrative performance; experience piton as performative ritual maintaining contradiction between stated and actual selection logic.
 *   - Analytical Observer: Analytical/analytical (universal scope) — risks naturalizing contingent institutional choices as structural inevitabilities; experience mountain that engine flags as false summit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legacy_admission_gatekeeping, 0.58).
domain_priors:suppression_score(legacy_admission_gatekeeping, 0.68).
domain_priors:theater_ratio(legacy_admission_gatekeeping, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legacy_admission_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(legacy_admission_gatekeeping, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legacy_admission_gatekeeping, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legacy_admission_gatekeeping, tangled_rope).
narrative_ontology:human_readable(legacy_admission_gatekeeping, "Legacy Admission Gatekeeping in Elite Universities").
narrative_ontology:topic_domain(legacy_admission_gatekeeping, "education/institutional_access").

domain_priors:requires_active_enforcement(legacy_admission_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legacy_admission_gatekeeping, wealthy_alumni_families).
narrative_ontology:constraint_beneficiary(legacy_admission_gatekeeping, elite_university_administration).
narrative_ontology:constraint_beneficiary(legacy_admission_gatekeeping, donor_base).
narrative_ontology:constraint_victim(legacy_admission_gatekeeping, qualified_non_legacy_applicants).
narrative_ontology:constraint_victim(legacy_admission_gatekeeping, first_generation_applicants).
narrative_ontology:constraint_victim(legacy_admission_gatekeeping, low_income_qualified_students).
narrative_ontology:constraint_victim(legacy_admission_gatekeeping, meritocratic_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: QUALIFIED NON-LEGACY APPLICANT (SNARE) — Cannot exit the credential market; faces systematic disadvantage with no alternative pathway to equivalent status. The applicant is fully trapped by credential architecture and resource barriers. Experiences pure extraction: opportunity cost of exclusion, wage premium from elite credential unavailable, network effects concentrated in excluded cohort. Maximum chi from powerless + trapped + national scope.
constraint_indexing:constraint_classification(legacy_admission_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIRST-GENERATION / LOW-INCOME APPLICANTS (SNARE) — Face material barriers (test prep costs, college counseling access, application fees) plus identity barriers (unfamiliar with institutional culture, impostor syndrome from legacy-coded environment). Constrained exit options — can pursue regional/non-elite universities but at permanent earnings and network penalty. High experienced extraction.
constraint_indexing:constraint_classification(legacy_admission_gatekeeping, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE UNIVERSITY ADMINISTRATION (TANGLED ROPE) — Experiences genuine coordination function: legacy preferences maintain alumni relationships, facilitate major gift fundraising ($1B+ endowment flows), sustain institutional culture and traditions. Simultaneously extracts from qualified non-legacy candidates by restricting access to credential premium. Active enforcement required (selective admission policy enforcement, donation cultivation). Benefits exceed costs for institution — asymmetric extraction toward donors.
constraint_indexing:constraint_classification(legacy_admission_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: WEALTHY ALUMNI FAMILIES / DONOR BASE (ROPE) — Net beneficiaries experiencing pure coordination: legacy preference signals their continued status and investment in institutional perpetuation. Donation flows sustained by preference system ($1.2B annual major gifts at top 20 institutions estimated 40-50% from legacy families or their networks). Arbitrage exit option — can invest elsewhere if preference disappears, but preference system locks in advantage. Low to negative experienced extraction.
constraint_indexing:constraint_classification(legacy_admission_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ACCESS-EQUITY COALITION (SCAFFOLD) — Student-led and policy advocacy organizations (Students for Fair Admissions, affirmative action opponents, equity-focused administrators) see legacy preferences as a temporary institutional arrangement with an identifiable sunset pathway: federal policy intervention (Department of Education, Congressional action), institutional policy change (elimination of legacy consideration), or market pressure (demographic shifts reducing legacy applicant pool). Constraint has a clear exit mechanism and organized agents with agency.
constraint_indexing:constraint_classification(legacy_admission_gatekeeping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MERIT-BASED IDEOLOGY / INSTITUTIONAL BRAND (PITON) — Elite universities publicly commit to merit-based selection and diversity while maintaining legacy preferences. The merit narrative persists through institutional inertia and theatrical performance: U.S. News rankings reward 'selectivity' metrics that legacy preferences inflate (by admitting low-SAT legacy applicants, overall SAT average drops less than it would under pure merit, maintaining rank appearance). Theater ratio high because the merit story is maintained despite contradicting practice. Piton classification: degraded but persistent due to ranking theater.
constraint_indexing:constraint_classification(legacy_admission_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, wealth-based stratification may appear immutable: markets naturally concentrate resources, institutional prestige naturally clusters, meritocratic sorting always requires some selectivity gatekeeping. This perspective risks naturalizing contingent institutional choices as structural inevitabilities. Engine false summit detection will identify this as naturalization of social arrangements.
constraint_indexing:constraint_classification(legacy_admission_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legacy_admission_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legacy_admission_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legacy_admission_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legacy_admission_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legacy_admission_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(legacy_admission_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Legacy preferences extract opportunity cost from non-legacy applicants through credential scarcity — elite degree premium (lifetime earnings difference ~$300K+, network effects in prestige sectors). However, the extraction is not maximal (snare level ~0.66+) because universities do admit substantial non-legacy cohorts and genuine merit consideration exists alongside legacy consideration. The constraint is hybrid: real coordination value in fundraising and alumni relations exists alongside pure extraction. Suppression (0.68): High. Multiple suppression mechanisms operate: material barriers (test prep costs, application fees, college counseling access concentration in wealthy schools); cultural barriers (unfamiliarity with elite institutional culture, legacy-coded expectations); normative barriers (belief in meritocratic legitimacy despite evidence of legacy preference); structural barriers (credential market logic where exclusion produces permanent earnings/network penalty). Suppression is not total (applicants can pursue non-elite credentials) but substantial enough to make exit costly. Theater ratio (0.64): Moderate-high. Elite universities explicitly commit to merit-based selection and diversity while maintaining legacy preferences. The merit narrative is substantially performative: published diversity statistics highlight race/ethnicity/first-generation while leaving legacy preference rates obscured; U.S. News rankings reward selectivity metrics that legacy preferences inflate (lower-credential legacy admits dilute SAT averages less than rejecting all below-cutoff applicants); institutional marketing emphasizes 'most rigorous selection process' while legacy consideration remains. The theater is not total (some genuine merit consideration occurs) but substantial enough to signal institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence from a single structural phenomenon. Non-legacy applicants (d≈0.95) experience snare — pure extraction, no coordination benefit visible to them. Alumni donors (d≈0.15) experience rope — coordination mechanism (institutional relationship maintenance) with minimal extraction. Administration (d≈0.55) experiences tangled rope — legitimate coordination function alongside asymmetric extraction, but benefits exceed costs from their institutional position. Access-equity coalition (d≈0.55 organized) experiences scaffold — same structural contradiction but with perceived agency and sunset pathway. Merit ideology (institutional/arbitrage) experiences piton — ritual performance persisting through inertia despite degraded function. Analytical observer risks mountain — naturalizing wealth stratification as inevitable feature of high-stakes credentials. The perspectival gap is structurally rooted: the same constraint produces genuinely different experienced chi values and classification outcomes depending on the observer's structural position, exit options, and power level. No single classification captures the phenomenon; the presheaf over all positions is necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) computation drives chi = ε × f(d) × σ(S). Non-legacy applicants: victim status + trapped exit → high d (~0.95) → f(d) ≈ 1.42 → maximum chi. First-generation/low-income: victim status + constrained exit → high d (~0.82) → f(d) ≈ 1.20 → high chi. Administration: beneficiary + mobile exit → moderate d (~0.55) → f(d) ≈ 0.75 → moderate chi. Alumni/donors: beneficiary + arbitrage exit → low d (~0.15) → f(d) ≈ -0.01 → negative/near-zero chi (they experience low/negative extraction). Access-equity coalition: victim status + constrained exit → high d (~0.80) → but organized power atom → moderate d adjustment (~0.55) → f(d) ≈ 0.75 → moderate chi. National scope σ(S) = 1.0 (no scope amplification), producing final chi values in range 0.0-0.82 across perspectives. The perspectival gap emerges from directionality differences, not from ε or σ(S) differences.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint exhibits genuine coordination properties (fundraising, alumni relations, institutional culture continuity) bundled with pure extraction (credential opportunity cost for non-legacy applicants). The tangled rope classification correctly captures this hybrid: 0.40 ≤ χ ≤ 0.90 (satisfied at ~0.60), base extraction ε ≥ 0.30 (satisfied at 0.58), suppression ≥ 0.40 (satisfied at 0.68), requires active enforcement true. The coordination function is real and substantial ($500M-1B annual gift flows), preventing pure snare classification. The extraction is asymmetric and structurally maintained by enforcement (selective admission decisions based on legacy status). However, the critical mandatrophy question is whether the 'coordination function' is genuine or a rationalization of pure extraction. The omegas address this: if legacy preferences do NOT causally drive major gifts (omega: donation_causality_ambiguity), then the 'coordination' is spurious and the constraint should reclassify toward snare. If institutional culture CANNOT be maintained post-elimination of legacy preferences (omega: identity_lock_institutional_culture), then the coordination is real and tangled rope is appropriate. The classification is stable under the assumption that both coordination functions are genuine. If either is empirically falsified, reclassification is warranted. The constraint demonstrates that high-ε tangled rope classifications require empirical validation of claimed coordination functions, not merely assumption of institutional claims about coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    merit_versus_wealth_measurement,
    'How do we measure ''merit'' independent of existing wealth-correlated proxies (standardized test scores, extracurricular access, college prep resources)?',
    'Comparative longitudinal analysis: matched cohorts of admitted students (legacy vs non-legacy) tracked for academic performance, graduation outcomes, advanced degree attainment, and career achievement. If non-legacy cohort outperforms legacy cohort at equivalent test scores, legacy preferences are selection error, not coordination.',
    'If legacy cohort underperforms: legacy preferences are pure extraction (not coordination, reclassifies toward snare). If performance equivalent: preferences reflect real coordination value (institutional culture/relationship maintenance) justifying tangled rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(merit_versus_wealth_measurement, empirical, 'Whether legacy status predicts genuine merit or is orthogonal to performance').

omega_variable(
    donation_causality_ambiguity,
    'Do legacy preferences cause major gift flows, or do wealthy families with tradition of giving happen to have legacy-eligible children?',
    'Institutional policy experiments: universities that eliminate legacy preferences and track subsequent major gift patterns. Donor survey data on preference elasticity of giving. Natural experiments from policy changes (Harvard/Yale legacy elimination outcomes).',
    'If causal: legacy preferences generate $500M-$1B annually in marginal donation value (true coordination function justifying tangled rope). If spurious: preferences are pure extraction masquerading as coordination (reclassifies toward snare). If partially causal: proportional tangled rope vs snare mixture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(donation_causality_ambiguity, empirical, 'Whether legacy preferences drive donations or reflect existing donor characteristics').

omega_variable(
    identity_lock_institutional_culture,
    'To what extent does ''institutional culture'' and ''tradition'' of elite universities depend on legacy composition, versus being cultural myths that rationalize wealth-based gatekeeping?',
    'Historical institutional analysis of culture narratives before and after policy changes. Ethnographic/interview data with legacy and non-legacy students on experienced institutional culture and belonging. Institutional change processes at universities that eliminated legacy preferences.',
    'If culture genuinely depends on legacy continuity: preferences serve real coordination function (justify tangled rope, institutional identity-coordination boltzmann type). If culture is post-hoc rationalization: preferences are pure extraction dressed in tradition-speak (reclassify toward snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_institutional_culture, conceptual, 'Whether institutional culture requires legacy composition or is reconstructible post-elimination').

omega_variable(
    suppression_mechanism_internalization,
    'How much of the suppression experienced by non-legacy applicants is structural (material barriers) versus internalized (belief in meritocratic legitimacy of the system)?',
    'Post-admission ethnography: tracking non-legacy admits'' sense of belonging, impostor syndrome, and perceived legitimacy versus admitted legacy students with similar credentials. Panel studies on first-generation student self-perception of deservingness.',
    'If highly internalized: suppression is maintained without enforcement infrastructure; constraint persists post-elimination through normative capture. If structural: enforcement can be removed and suppression decreases. Affects whether constraint is truly tangled_rope or partially piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized components of admission suppression').

omega_variable(
    false_summit_naturalization,
    'Is wealth stratification in credential access a fundamental property of high-stakes credentials, or a contingent institutional choice by elite universities?',
    'Comparative institutional analysis: credential systems with different gatekeeping logics (public universities, credential guilds, government-issued credentials, international systems). Historical analysis of when/why elite US universities adopted legacy preferences (20th century formalization from 19th century informal practice).',
    'If fundamental: mountain classification correct (extraction inherent to credentialing). If contingent: mountain is false summit — natural-law framing naturalizes institutional choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether wealth stratification in credentials is structural or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legacy_admission_gatekeeping, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legacy_tr_t0, legacy_admission_gatekeeping, theater_ratio, 0, 0.52).
narrative_ontology:measurement(legacy_tr_t10, legacy_admission_gatekeeping, theater_ratio, 10, 0.58).
narrative_ontology:measurement(legacy_tr_t20, legacy_admission_gatekeeping, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(legacy_be_t0, legacy_admission_gatekeeping, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legacy_be_t10, legacy_admission_gatekeeping, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(legacy_be_t20, legacy_admission_gatekeeping, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legacy_su_t0, legacy_admission_gatekeeping, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(legacy_su_t10, legacy_admission_gatekeeping, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(legacy_su_t20, legacy_admission_gatekeeping, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legacy_admission_gatekeeping, identity_coordination).
narrative_ontology:affects_constraint(legacy_admission_gatekeeping, credential_premium_wage_gap).
narrative_ontology:affects_constraint(legacy_admission_gatekeeping, intergenerational_wealth_concentration).
narrative_ontology:affects_constraint(legacy_admission_gatekeeping, first_generation_student_dropout_risk).

% DUAL FORMULATION NOTE:
% Legacy admission gatekeeping operates in a constraint family with three upstream structural constraints: the credentialing wage premium (why the gate matters), intergenerational wealth concentration (what funds alumni giving), and downstream first-generation dropout risk (downstream victim cohort experiencing identity and resource barriers in admitted cohort). Each story has distinct ε values reflecting different structural mechanisms. Legacy gatekeeping sits between credential value and family wealth concentration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legacy_admission_gatekeeping, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
