% ============================================================================
% CONSTRAINT STORY: uk_graduate_visa_salary_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_graduate_visa_salary_threshold, []).

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
 *   constraint_id: uk_graduate_visa_salary_threshold
 *   human_readable: UK Graduate Visa Minimum Salary Threshold
 *   domain: economic/political
 *
 * SUMMARY:
 *   The UK Graduate Visa minimum salary threshold (£38,000 in 2024, indexed
 *   to Average Weekly Earnings) creates a structural extraction mechanism
 *   wrapped in the language of labor market protection. International
 *   graduates invest £15,000-30,000 in UK tuition, obtain a two-year
 *   post-study work visa, and then face a binary constraint: secure
 *   employment above the indexed threshold within two years or return home.
 *   The constraint exhibits all six DR types depending on observer position.
 *   Domestic workers and the Treasury see coordination (protecting labor
 *   market). Employers in talent-dependent sectors see mixed extraction and
 *   coordination. Graduates see pure extraction (trapped by visa dependency
 *   and salary floors). Universities see a degraded institutional ritual
 *   (theater). Global talent networks see a temporary self-limiting
 *   constraint as competing countries offer better terms. The analytical
 *   observer risks naturalizing visa sovereignty as immutable law when the
 *   structural mechanics are contingent policy choices. The theater ratio
 *   (0.45) reflects moderate performance: government rhetoric emphasizes
 *   'attracting global talent' and 'protecting UK competitiveness' while the
 *   actual mechanism constrains both — extracting value from graduates
 *   through visa dependency and limiting employer access to talent.
 *
 * KEY AGENTS:
 *   - International Graduates: Primary victim (powerless/trapped) — invest in UK education, then face binary salary constraint with no renegotiation capacity
 *   - UK Domestic Workers: Primary beneficiary (institutional/arbitrage) — protected from low-wage competition; can exit politically if threshold proves economically damaging
 *   - UK Treasury: Primary beneficiary (institutional/arbitrage) — reduces long-term visa processing costs and net fiscal burden of graduate visa holders
 *   - UK Employers (Tech/Science/Healthcare): Secondary victim (moderate/constrained) — face talent shortage when graduates cannot extend visas; cannot easily exit regulatory framework
 *   - UK Higher Education Institutions: Institutional actor (organized/constrained) — rely on international tuition revenue; maintain theater of 'graduate employment support' without changing constraints
 *   - Global Talent Arbitrage Networks: Powerful actors (powerful/mobile) — arbitrage visa differences; exit path is relocation to competing countries with better graduate visa terms
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing visa sovereignty as immutable when constraints are contingent policy choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_graduate_visa_salary_threshold, 0.58).
domain_priors:suppression_score(uk_graduate_visa_salary_threshold, 0.68).
domain_priors:theater_ratio(uk_graduate_visa_salary_threshold, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_graduate_visa_salary_threshold, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_graduate_visa_salary_threshold, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(uk_graduate_visa_salary_threshold, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_graduate_visa_salary_threshold, snare).
narrative_ontology:human_readable(uk_graduate_visa_salary_threshold, "UK Graduate Visa Minimum Salary Threshold").
narrative_ontology:topic_domain(uk_graduate_visa_salary_threshold, "economic/political").

domain_priors:requires_active_enforcement(uk_graduate_visa_salary_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_graduate_visa_salary_threshold, uk_domestic_workers).
narrative_ontology:constraint_beneficiary(uk_graduate_visa_salary_threshold, uk_treasury).
narrative_ontology:constraint_victim(uk_graduate_visa_salary_threshold, international_graduates).
narrative_ontology:constraint_victim(uk_graduate_visa_salary_threshold, uk_employers_skill_dependent_sectors).
narrative_ontology:constraint_victim(uk_graduate_visa_salary_threshold, uk_higher_education_revenue).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERNATIONAL GRADUATE (SNARE) — Trapped by visa status dependence. After investing £15,000-30,000 in UK tuition, the graduate faces a two-year work visa that can only be extended if salary reaches £38,000+ threshold (2024 rate, indexed annually). Cannot negotiate salary downward to secure employment; cannot easily exit to other countries without losing UK credential value. Maximum suppression of alternatives — either meet threshold or return home. Experiences pure extraction: visa constraints extract value (foregone wages during visa hunt, restricted job mobility, employer leverage).
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UK DOMESTIC WORKERS / TREASURY (ROPE) — Benefits from coordination function: salary threshold protects domestic labor market by pricing out low-wage competition and redirects graduates toward high-skill roles. Treasury benefits from reduced long-term visa processing costs and net fiscal burden reduction. The constraint appears as coordination: 'we are solving a collective action problem (brain drain, wage depression) together.' Arbitrage exit allows these agents to adjust threshold politically if fiscal calculation changes.
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: UK EMPLOYERS IN SKILL-DEPENDENT SECTORS (TANGLED ROPE) — Mixed experience. Benefit from coordination: visa rules protect against low-wage undercutting in retail/hospitality. But suffer extraction: tech, science, healthcare sectors face genuine talent shortage when graduates cannot secure visa extension. Cannot easily exit (relocation costs, regulatory burden in other countries), so constrained by the threshold. Benefits from some roles (wage pressure relief) while extracted from others (talent constraint). Suppression of alternatives is high — graduate must stay employed or leave UK entirely.
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: UK HIGHER EDUCATION INSTITUTIONS (PITON) — Theater ratio elevated. Universities lobby politicians for graduate visa access (claiming talent and revenue benefits) but have diminished functional capacity to guarantee employment for graduates at threshold salary. The constraint persists through institutional inertia: universities maintain recruitment campaigns targeting international students despite knowing many cannot extend visas. Performative advocacy for 'international competitiveness' masks declining functional influence on graduate outcomes. Suppression is moderate (institutions can invest in career services, employers can hire) but theater is high (institutional theater of addressing 'graduate employment' without changing fundamental constraints).
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GLOBAL TALENT ARBITRAGE NETWORKS (SCAFFOLD) — Graduate visa threshold creates temporary coordination problem with visible sunset: competing countries (Canada, Australia, UAE, post-Brexit EU expansion) are building alternative pathways. UK graduates increasingly arbitrage: study in UK at lower cost than US but work in Canada or Germany at higher salary without UK threshold constraint. The scaffold sees the UK threshold as temporary friction, not immutable extraction. Exit path is explicit: 'move to country with better graduate visa terms.' This perspective sees the constraint as a self-limiting policy mechanism that will decay as rival nations offer better terms. Sunset clause is implicit — 5-10 years before threshold becomes ineffective due to talent drain.
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — Risk of naturalizing: 'visa sovereignty is immutable; states must protect labor markets; this is inherent to nation-state power.' But the structural data contradicts the mountain classification: extractiveness (0.58), suppression (0.68), and theater (0.45) place this squarely in Snare/Tangled Rope territory. The constraint is contingent (salary threshold is a policy choice, not a law of physics), reversible (threshold can be lowered or abolished), and subject to external pressure (rival countries' competing policies). The false summit reveals that 'border control is natural law' is naturalization of a political extraction mechanism.
constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_graduate_visa_salary_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_graduate_visa_salary_threshold, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_graduate_visa_salary_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_graduate_visa_salary_threshold, TR),
    TR >= 0.70.

:- end_tests(uk_graduate_visa_salary_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The salary threshold extracts value from graduates through: (1) visa dependency leverage enabling employer wage suppression, (2) foregone earnings during visa-eligible job search, (3) restricted job mobility (must maintain threshold salary or lose visa), (4) opportunity cost of returning home. The extractiveness has increased over the interval as the threshold has been tightened (from £20,800 in 2016 to £38,000 by 2024), reflecting deliberate policy ratcheting. Suppression (0.68): High. International graduates face severe suppression of alternatives: cannot negotiate salary downward without losing visa extension, cannot easily exit to other countries (credential value concentration in UK), cannot appeal to visa authority (threshold is immutable by design), limited information about threshold trajectory before enrolling in UK institution. The visa dependency creates asymmetric bargaining power favoring employers. Theater ratio (0.45): Moderate-low. The constraint has less performative content than most extractive mechanisms because the salary floor is transparent and monitored. However, government rhetoric ('attracting global talent,' 'world-leading universities') creates theater around the stated purpose — protecting competitiveness — when the mechanism actively constrains both graduate opportunity and employer talent access.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. Beneficiaries experience minimal or negative extraction (coordination benefit); victims experience maximum extraction (trapped without alternatives). The gap reveals the constraint's structural asymmetry: the beneficiaries' 'collective action problem solution' is the victims' structural entrapment. This is the hallmark of a pure Snare — one group's benefit is extracted directly from another group with no reciprocal benefit. The Tangled Rope and Rope perspectives exist but are contextual (employer mix of benefit/extraction, university institutional ritual). The scaffold perspective is aspirational — it represents the constraint's structural vulnerability (competing countries' superior terms create sunset logic) rather than current lived experience. The analytical observer's false summit is diagnostic of naturalization: once you see the constraint as contingent policy rather than immutable law, the entire perspectival structure collapses toward Snare/Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   International graduates' directionality (d) is derived from: victim status (yes — salary constraint extraction), exit options (trapped — credential value is UK-specific, relocation costs are high), power level (powerless — no negotiating capacity). Structural derivation yields d ≈0.95, f(d) ≈ 1.42, experienced χ ≈ 0.82 (before scope adjustment). Domestic workers' directionality is derived from: beneficiary status (yes — wage protection), exit options (arbitrage — can politically lobby), power level (institutional). Structural derivation yields d ≈0.15, f(d) ≈ -0.01, experienced χ ≈ 0 or negative (compensation flow toward this group). Employers' directionality is mixed: some benefit (wage suppression in low-skill roles), some extracted (talent shortage in high-skill roles). Average d ≈0.55, f(d) ≈ 0.75, moderate χ. No directionality overrides are needed — the structural derivation captures the real dynamics. The beneficiary/victim declarations drive the automatic pipeline: graduates are victims (high d), domestic workers are beneficiaries (low d), employers are mixed (intermediate d).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (ε = 0.58 > 0.46): This constraint requires mandatrophy analysis to prevent mislabeling as 'unfortunate but necessary labor market policy' when it is a pure extraction mechanism. The question is: 'Does the salary threshold solve a genuine coordination problem or simply extract value under the guise of one?' Analysis reveals: (1) Coordination function is real but weak — some wage suppression occurs and some domestic workers benefit. (2) Extraction function is severe and direct — graduates face binary constraint that is orthogonal to actual skill level or labor market need (many £35k graduates are genuinely needed by UK employers). (3) Asymmetry is structural — beneficiaries can exit politically (arbitrage), victims cannot exit at all (trapped). This is not a Rope hiding in Snare clothing; it is a Snare with a genuine but secondary coordination component. The Tangled Rope classification for moderate agents (employers) is correct because they experience both coordination (wage protection) and extraction (talent constraint) in substantial measures. The overall constraint is a Snare with Tangled Rope effects for specific sectors. Mandatrophy resolution confirms that the 'labor market protection' framing is not false but is a partial truth that obscures the primary extraction mechanism targeting international graduates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_wage_inflation_spiral,
    'Does the salary threshold itself drive wage inflation in UK tech/science sectors, creating a self-reinforcing extraction mechanism?',
    'Comparison of wage growth rates in high-threshold sectors (tech: £38k+) vs low-threshold sectors (retail/hospitality: no threshold for short-term work visa); cross-national analysis of wage levels in equivalent sectors in countries with lower graduate visa thresholds',
    'If yes: threshold becomes a rent-seeking mechanism for UK employers and domestic workers (extraction amplified). If no: threshold operates as labor market protection without distortion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_wage_inflation_spiral, empirical, 'Whether salary threshold drives wage inflation in target sectors').

omega_variable(
    brain_drain_counterfactual,
    'What fraction of UK-educated graduates would remain in UK labor market at salary ≥£38k if visa constraints were removed?',
    'Survey data from recent graduates; comparison with historical pre-threshold visa policies; analysis of graduate destination data before/after threshold changes',
    'If retention rate is >80%: threshold is extractive (removes genuine opportunities without achieving stated labor market protection). If <40%: threshold is necessity (many graduates leave regardless).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(brain_drain_counterfactual, empirical, 'Counterfactual retention rate without salary threshold').

omega_variable(
    employer_bargaining_position_capture,
    'Do UK employers leverage visa dependency to suppress graduate salaries below market clearing rates for international graduates?',
    'Comparison of starting salaries for international vs domestic graduates in same role/field; analysis of employer statements in grad recruitment; exit interviews revealing visa pressure in salary negotiations',
    'If evidence of bargaining position capture: suppression mechanism (0.68) is confirmed. If market salaries align: suppression is overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(employer_bargaining_position_capture, empirical, 'Employer leverage over international graduate salaries due to visa dependency').

omega_variable(
    alternative_visa_route_sufficiency,
    'Are Skilled Worker Visa routes (Tier 2 successor) providing sufficient alternative pathways for graduates, or are skill-level misclassifications creating barriers?',
    'Data on Skilled Worker visa grant rates for graduate-level roles; comparison of time-to-visa approval between Graduate Visa and Skilled Worker routes; employer data on visa sponsorship burden',
    'If alternatives insufficient: Graduate Visa extraction is unchecked (Snare confirmed). If alternatives viable: extraction is partial (Tangled Rope more accurate).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_visa_route_sufficiency, empirical, 'Sufficiency of alternative visa routes for graduate-level work').

omega_variable(
    global_talent_flow_redirect,
    'Are top-quartile UK-educated graduates now redirecting to Canada/Australia/UAE on visa completion, rather than extending UK work visas?',
    'Tracking data from UK alumni networks; visa grant statistics for competing countries; survey of recent UK graduates on post-study work destination choice',
    'If strong redirect: scaffold sunset perspective confirmed (constraint self-limiting). If minimal redirect: constraint persists despite rival competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_talent_flow_redirect, empirical, 'Redirect of UK-educated graduates to higher-paying visa jurisdictions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_graduate_visa_salary_threshold, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ugvst_tr_t0, uk_graduate_visa_salary_threshold, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ugvst_tr_t3, uk_graduate_visa_salary_threshold, theater_ratio, 3, 0.41).
narrative_ontology:measurement(ugvst_tr_t6, uk_graduate_visa_salary_threshold, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(ugvst_be_t0, uk_graduate_visa_salary_threshold, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ugvst_be_t3, uk_graduate_visa_salary_threshold, base_extractiveness, 3, 0.47).
narrative_ontology:measurement(ugvst_be_t6, uk_graduate_visa_salary_threshold, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_graduate_visa_salary_threshold, resource_allocation).
narrative_ontology:affects_constraint(uk_graduate_visa_salary_threshold, uk_higher_education_international_enrollment).
narrative_ontology:affects_constraint(uk_graduate_visa_salary_threshold, uk_talent_drain_to_competing_jurisdictions).

% DUAL FORMULATION NOTE:
% The UK graduate visa salary threshold is part of a constraint family centered on UK post-study work policy. The threshold itself (this story) operates at ε=0.58 (Snare/Tangled Rope boundary); it is upstream of two related constraints: (1) international enrollment demand (affected by known visa restrictions, ε ≈ 0.45, Tangled Rope), (2) talent drain to Canada/Australia/UAE (affected by threshold-driven relative disadvantage, ε ≈ 0.50, Tangled Rope). The family exhibits lifecycle dynamics: as threshold tightens, enrollment demand declines (negative feedback), talent drain accelerates (positive feedback), and competitive advantage shift occurs toward rival jurisdictions. Network decomposition prevents conflating 'the visa policy' (multiple constraints with different ε values, different beneficiary/victim sets, different failure modes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
