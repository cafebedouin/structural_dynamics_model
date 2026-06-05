% ============================================================================
% CONSTRAINT STORY: college_admissions_market
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_college_admissions_market, []).

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
 *   constraint_id: college_admissions_market
 *   human_readable: The US Elite College Admissions Market
 *   domain: social/economic
 *
 * SUMMARY:
 *   The US elite college admissions market is a high-stakes, many-to-one
 *   matching mechanism where institutional scarcity (limited seats at
 *   prestigious institutions) meets unlimited demand (all students seeking
 *   admission). The constraint functions simultaneously as a coordination
 *   mechanism (matching qualified students with appropriate institutions) and
 *   an extraction apparatus (concentrating educational opportunity and
 *   credential value among those with financial capacity for test prep,
 *   counseling, and application signaling). The system exhibits a
 *   perspectival cascade: lower-income applicants experience it as a snare
 *   (trapped by cost barriers); public school counselors experience it as
 *   tangled rope (mandated labor with constrained resources); elite
 *   institutions experience it as rope (prestige competition that benefits
 *   them); the test prep industry experiences it as rope (pure benefit
 *   through fees); reform advocates see it as scaffold (temporary, with
 *   sunset as alternatives mature); legacy admissions persist as piton
 *   (performative ritual); and the analytical observer risks naturalizing it
 *   as a mountain (inevitable scarcity sorting) when it is actually
 *   contingent institutional design. The theater ratio (0.65) reflects
 *   substantial performative content: the holistic review process, while
 *   framed as individualized assessment, operates within constraints that
 *   privilege resource-signaling capacity. Essays, extracurriculars, and
 *   demonstrated interest require time, money, and cultural capital to
 *   perform convincingly. The extractiveness (0.58) reflects that much of the
 *   system's function—filtering applicants efficiently—is genuine
 *   coordination, but the mechanism redistributes educational opportunity in
 *   ways that correlate with family wealth rather than merit, constituting
 *   extraction layered onto coordination.
 *
 * KEY AGENTS:
 *   - Lower-Income Applicants: Primary victim (powerless/trapped) — face cost barriers to test prep ($2,000-5,000), counseling ($5,000-15,000), coaching ($1,500-3,000), and application fees ($150-300); lack access to institutional knowledge about admissions processes
 *   - Elite Institutions (Harvard, Yale, Stanford, etc.): Primary beneficiary (institutional/arbitrage) — benefit from large applicant pools enabling yield management, prestige metrics, and tuition leverage; can adjust test requirements and financial aid policies strategically
 *   - Test Prep and Counseling Industry: Secondary beneficiary (powerful/arbitrage) — captures $20+ billion annual market revenue through tutoring, test administration, essay coaching, and application management platforms
 *   - Public School Counselors: Secondary victim (moderate/constrained) — bound by mandated college reporting, high student-to-counselor ratios (482:1), underfunded departments; expected to provide guidance without resources
 *   - Wealthy Families: Primary beneficiary (powerful/arbitrage) — afford full-service admissions consulting ($10,000-50,000+), test prep, and multiple application attempts; capture disproportionate share of elite seats
 *   - Test-Optional Reform Coalition: Organized advocate (organized/constrained) — FairTest, Common Application, nonprofit counseling networks building alternative pathways; constrained by institutional resistance but seeing sunset horizon
 *   - Lower Public and Regional Universities: Secondary actor (institutional/constrained) — lose potential students and tuition revenue to prestige competition; constrained by need to compete using same ranking metrics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(college_admissions_market, 0.58).
domain_priors:suppression_score(college_admissions_market, 0.68).
domain_priors:theater_ratio(college_admissions_market, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(college_admissions_market, extractiveness, 0.58).
narrative_ontology:constraint_metric(college_admissions_market, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(college_admissions_market, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(college_admissions_market, tangled_rope).
narrative_ontology:human_readable(college_admissions_market, "The US Elite College Admissions Market").
narrative_ontology:topic_domain(college_admissions_market, "social/economic").

domain_priors:requires_active_enforcement(college_admissions_market).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(college_admissions_market, elite_institutions).
narrative_ontology:constraint_beneficiary(college_admissions_market, wealthy_families).
narrative_ontology:constraint_beneficiary(college_admissions_market, test_prep_industry).
narrative_ontology:constraint_victim(college_admissions_market, lower_income_applicants).
narrative_ontology:constraint_victim(college_admissions_market, public_education_system).
narrative_ontology:constraint_victim(college_admissions_market, school_counselor_workforce).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOWER-INCOME APPLICANT (SNARE) — Trapped within a system where test prep costs $2,000-5,000, college counselors charge $5,000-15,000, essay coaching services $1,500-3,000, and application fees total $150-300. No realistic exit without accepting severe educational opportunity cost. The constraint extracts through gatekeeping: access to elite credentials is conditioned on financial capacity to pay for signals rather than merit alone. Maximum experienced extraction due to trapped exit and powerless position.
constraint_indexing:constraint_classification(college_admissions_market, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC SCHOOL COUNSELOR (TANGLED ROPE) — Constrained by rising student-to-counselor ratios (national average 482:1), mandated college-reporting requirements, and underfunded guidance departments. Benefits from the constraint through professional legitimacy and institutional expectations (schools must provide college guidance), but extraction runs through mandatory reporting labor, credential standardization, and competitive pressure to demonstrate college placement rates. Neither fully beneficiary nor fully victim — the system both employs and constrains them.
constraint_indexing:constraint_classification(college_admissions_market, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE INSTITUTION (ROPE) — Benefits from the market structure: standardized test scores enable fast filtering, applicant volume validates institutional prestige, tuition leverage through selective admission and yield management, and ability to arbitrage between full-pay and need-blind admissions. Experiences constraint as coordination mechanism for prestige competition. Can arbitrage by adjusting test requirements, creating early decision tracks, or modifying financial aid policies — has real exit options through institutional strategy.
constraint_indexing:constraint_classification(college_admissions_market, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TEST PREP & COUNSELING INDUSTRY (ROPE) — Net beneficiary. Extraction flows toward this sector through tutoring fees, coaching services, SAT/ACT test administration revenue, essay coaching, and application management platforms. High arbitrage capacity: can pivot to test-optional markets, offer scholarship programs for visibility, or rebrand services as 'holistic admissions support.' Power and arbitrage options position this as a pure coordinator that benefits from high-stakes uncertainty.
constraint_indexing:constraint_classification(college_admissions_market, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TEST-OPTIONAL REFORM COALITION (SCAFFOLD) — Organized advocates (FairTest, Common Application, nonprofit counseling networks, social mobility organizations) are building structural alternatives to high-stakes testing: test-optional policies, free application fee waivers, holistic review guidelines, and direct institutional aid for low-income recruitment. Low effective extraction because the coalition has agency and sees a sunset: as test-optional norms spread and direct financial aid becomes more transparent, the traditional SAT/ACT gating mechanism loses mechanical force. Sunset horizon: 15-25 years for norms to mature across all tiers.
constraint_indexing:constraint_classification(college_admissions_market, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY ADMISSIONS (PITON) — Performative gatekeeping with attenuated function. Originally served coordination function (alumni network reinforcement, institutional identity), but primary effect now is inertial extraction: legacy preference correlates with parental wealth and whiteness, concentrating seats without explicit merit justification. Institutions maintain legacy policies through tradition despite mounting reputational cost and declining functional benefit. Theater ratio high: the ritual persists as institutional theater (alumni relations, development office justification) rather than coordination. Some institutions have eliminated legacy policies, revealing it as degraded rather than immutable.
constraint_indexing:constraint_classification(college_admissions_market, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of college matching via selective admission is inherent to a system where institutional capacity is limited and demand exceeds supply. Scarce seats require some gatekeeping mechanism; the specific mechanism (test scores, essays, extracurriculars) is contingent, but the need for filtering is structural. However, the base properties contradict this — the extractiveness (0.58), suppression (0.68), and active enforcement requirement indicate this is not a natural law but rather a contingent institutional arrangement. Engine will flag this as a false summit.
constraint_indexing:constraint_classification(college_admissions_market, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(college_admissions_market_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(college_admissions_market, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(college_admissions_market, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(college_admissions_market, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(college_admissions_market, TR),
    TR >= 0.70.

:- end_tests(college_admissions_market_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The system does perform genuine coordination (matching students with institutions, signaling academic preparation), but significant extraction occurs through: (1) information asymmetry (wealthy families access expensive counselors; poor families do not), (2) cost gatekeeping (test prep and application services are priced beyond lower-income reach), (3) signal-cost mismatch (essays require writing skills and time that correlate with family resources, not merit alone), (4) prestige extraction (elite institutions capture credential value without proportional quality differential). The extractiveness increased from 0.35 to 0.58 over the measurement interval as test prep costs rose, counselor-to-student ratios declined, and the system added more costly signals (demonstrated interest, optional essays, video responses). Suppression (0.68): High. Multiple barriers prevent exit or alternative pathways: (1) elite credentials are narrowly concentrated (top 50 institutions hold 70% of prestige value), (2) employers and graduate programs screen by institution, (3) student debt burden makes any college attendance path sticky, (4) public school counselors are mandated to guide students toward four-year institutions, (5) test-optional policies are recent and incomplete (most competitive institutions still heavily weight standardized tests). Theater ratio (0.65): High and increasing. Substantial performative content: holistic review is framed as individualized but operates within resource-constrained institutions using formulaic scoring; essays assess writing quality (correlated with tutoring access), not merit; extracurricular engagement correlates with family ability to fund activities; legacy admissions is pure performance (no merit link); demonstrated interest theater favors students whose families can afford travel and campus visits; the Common Application and its 650-word personal statement are performance artifacts, not merit signals. The theater ratio increased from 0.40 to 0.65 as institutions added more optional-but-expected signals (videos, supplements, demonstrated interest).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a maximum perspectival gap. Lower-income applicants see a snare (trapped, no exit, pure extraction). Counselors see tangled rope (mandated work with constrained resources, but professional legitimacy). Institutions see rope (beneficial coordination enabling prestige competition). Industry sees rope (pure benefit stream). Reform coalition sees scaffold with sunset (alternative pathways being built). Legacy system sees piton (performative theater without functional merit). Analytical observer risks seeing mountain (inevitable scarcity matching) but structural data shows this is false naturalization. The gap exists because the same mechanism (selective admission via high-cost signals) functions as fair filtering for resource-rich agents, as entrapment for resource-poor agents, and as performance ritual for institutional legitimacy. No single classification captures all experiences — the presheaf over the observation site is the complete answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives d from the agent's structural position relative to the extraction flow. Lower-income applicants are trapped victims (d ≈ 0.95) — high power of f(d), maximum experienced extraction. Public school counselors are constrained extractees who also benefit from institutional expectations (d ≈ 0.55) — moderate f(d), mixed experienced extraction. Elite institutions are arbitrage beneficiaries (d ≈ 0.10) — low/negative f(d), negative experienced extraction (system subsidizes them). Test prep industry is arbitrage beneficiaries (d ≈ 0.05) — very low/negative f(d), negative experienced extraction (pure benefit). Wealthy families are arbitrage beneficiaries (d ≈ 0.08) — negative f(d). Reform coalition is constrained organizers with exit options (d ≈ 0.40) — moderate f(d), but low effective extraction because organized agents have leverage. The directionality derivation confirms the tangled_rope classification: multiple beneficiaries (institutions, industry, wealthy), multiple victims (lower-income, counselors, public system), active enforcement (institutional test requirements, application processes), and both coordination function (matching) and asymmetric extraction (resource gatekeeping).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through perspectival composition. The temptation is to classify this as pure extraction (snare) — the system clearly extracts through gatekeeping and cost barriers. But this collapses the coordination function: the system does match students with institutions, does provide signals of academic preparation, does create incentives for achievement, and does enable genuine opportunity creation for some. Similarly, classifying as pure coordination (rope) collapses the extraction function: the prestige gatekeeping is real, the cost barriers are real, and the resource-correlation is real. The tangled_rope classification resolves this by insisting both functions exist simultaneously: the system is a genuine coordination mechanism for the market (matching) AND a genuine extraction mechanism for the opportunity structure (prestige gatekeeping, cost redistribution). This forces the analytical framework to hold both truths: (1) the system solves a real matching problem (coordination), and (2) the system solves it through mechanisms that disproportionately benefit the wealthy and powerful (extraction). The test-optional scaffold perspective shows that the extraction function is not immutable — alternative mechanisms (GPA, application fee waivers, holistic review without test scores) can solve the matching problem with lower extraction, but institutions resist because they benefit from the current high-extraction design. This confirms that the extraction is structural choice, not natural necessity. Mandatrophy is resolved by the perspectival cascade: no single type is correct; the system is correctly classified as tangled_rope when analyzed holistically, but that classification only makes sense when paired with the snare experience of trapped victims, the piton degradation of legacy systems, and the scaffold possibility of reform alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    socioeconomic_signal_validity,
    'Do standardized test scores and essay quality genuinely correlate with college success metrics (graduation, GPA, earnings), or do they primarily signal family resources and test-taking preparation access?',
    'Regression analysis controlling for family income: correlation of test score with college success vs. income with college success; comparison of predictive power within socioeconomic strata',
    'If genuine merit signal: admissions system is coordination (lower snare classification). If primarily resource signal: system is extraction (higher snare/tangled rope classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(socioeconomic_signal_validity, empirical, 'Whether test scores predict success independent of family resources').

omega_variable(
    elite_institutional_necessity,
    'What fraction of lower-income students'' lifetime earnings advantage from elite college attendance would persist under alternative admissions methods (GPA-only, lottery from qualified applicants, test-optional with holistic review)?',
    'Quasi-experimental studies comparing earnings outcomes across admissions policy changes; regression discontinuity at admissions cutoffs; longitudinal tracking of test-optional vs test-required cohorts',
    'If advantage persists (>80%): the extractive gatekeeping is real, not artifact of selection. If advantage minimal (<20%): much of the gatekeeping is performative theater, not genuine opportunity creation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_institutional_necessity, empirical, 'Causal contribution of elite institution attendance to earnings').

omega_variable(
    counselor_labor_substitution,
    'Could public school college guidance be effectively replaced or supplemented by low-cost digital platforms and peer mentoring without increasing educational inequality?',
    'Pilot programs comparing outcomes of digital guidance, peer mentoring, and traditional counseling for equivalent applicant cohorts; cost-per-placement analysis; equity outcome measures',
    'If effective substitutes exist: counselor constraint is primarily labor extraction (victims classification strengthened). If substitutes fail for certain populations: counselor labor is essential coordination (victims classification weakens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counselor_labor_substitution, empirical, 'Whether digital/peer alternatives to counselor labor are effective').

omega_variable(
    prestige_signal_replaceability,
    'If institutions adopted transparent, low-cost admissions (GPA, test-optional, financial-aid-first), would employers and graduate programs still use elite institution attendance as a screening signal, or would the signal lose value?',
    'Employer hiring preference studies pre/post admissions policy changes; graduate program admission criterion analysis; market signaling studies',
    'If prestige signal persists: elite institutions can arbitrage indefinitely (rope perspective confirmed). If signal degrades: prestige competition becomes zero-sum (scaffold sunset accelerates).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prestige_signal_replaceability, preference, 'Whether prestige signal remains after low-cost admissions adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(college_admissions_market, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cadm_tr_t0, college_admissions_market, theater_ratio, 0, 0.4).
narrative_ontology:measurement(cadm_tr_t15, college_admissions_market, theater_ratio, 15, 0.55).
narrative_ontology:measurement(cadm_tr_t30, college_admissions_market, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(cadm_be_t0, college_admissions_market, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cadm_be_t15, college_admissions_market, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(cadm_be_t30, college_admissions_market, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(college_admissions_market, resource_allocation).
narrative_ontology:affects_constraint(college_admissions_market, student_debt_accumulation).
narrative_ontology:affects_constraint(college_admissions_market, intergenerational_wealth_transfer).
narrative_ontology:affects_constraint(college_admissions_market, k12_school_funding_inequality).

% DUAL FORMULATION NOTE:
% The college admissions market decomposes into multiple structurally distinct constraints: (1) the matching problem (how to allocate scarce elite seats among qualified applicants) — could be solved with low extractiveness via lottery or GPA-only, suggesting this is contingent institutional design, not natural law; (2) the prestige signaling market (whether elite institution attendance causally produces earnings advantage or merely signals pre-existing family resources) — this is upstream and empirically contested; (3) the test prep cost barrier (whether high-cost signals are necessary for admissions gatekeeping) — downstream from the matching problem, generates most of the extractive burden on lower-income applicants. The current story treats the full system as tangled_rope (0.58 extractiveness). A decomposition story focusing on prestige-signal validity would have much higher extractiveness (0.70+, snare) if earnings advantage is purely signaling. A decomposition story focusing on GPA-only matching would have extractiveness near 0.05 (rope). The current story unifies these by treating the institutional design as a choice that bundles coordination with extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(college_admissions_market, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
