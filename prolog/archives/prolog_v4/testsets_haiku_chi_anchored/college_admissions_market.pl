% ============================================================================
% CONSTRAINT STORY: college_admissions_market
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The US elite college admissions market functions as a high-stakes,
 *   many-to-one matching mechanism with dual structural functions: genuine
 *   coordination (colleges curating peer effects through selective
 *   enrollment) and extractive rent-seeking (students and families competing
 *   for positional goods via test scores, credentials, and status signals).
 *   Over the past 40 years, the system has shifted toward higher
 *   extractiveness and theater as competition intensified, test prep
 *   professionalized, and college prestige became increasingly concentrated
 *   among a shrinking set of elite institutions. The constraint exhibits both
 *   coordination benefits (peer networks, intellectual diversity at selective
 *   institutions) and extraction costs (income-based suppression through test
 *   prep barriers, information asymmetry, psychological pressure, and
 *   systemic inequality reproduction). The market's many-to-one structure
 *   creates acute powerlessness for low-income applicants: they compete
 *   against vastly better-resourced cohorts with no meaningful exit option
 *   except tier-down to less prestigious (and lower-earning) institutions.
 *   The theater ratio (0.64) reflects that significant portions of the
 *   admissions process involve performative activities (essays demonstrating
 *   'fit,' demonstrated interest through campus visits, extracurricular
 *   resume padding) that correlate weakly with academic or intellectual
 *   potential but strongly with family social capital and ability to signal
 *   status.
 *
 * KEY AGENTS:
 *   - Low-income applicants: Primary victims (powerless/trapped) — bear costs of test prep access barriers, information asymmetry, application fees; no exit except tier-down
 *   - First-generation students: Primary victims (powerless/trapped) — navigate opaque institutional norms without family models; bear costs of cultural capital gaps
 *   - High-socioeconomic applicants: Mixed actor (moderate/constrained) — benefit from test prep access, legacy preference, social capital signaling; also constrained by intense peer competition and psychological pressure
 *   - Elite institutions: Primary beneficiary (institutional/arbitrage) — benefit from prestige capital, alumni network effects, and selectivity branding; coordination function is genuine but extraction mechanism is leveraged
 *   - Test prep industry: Organized beneficiary (organized/arbitrage) — profit from inequality by selling access to test prep resources; extraction revenue stream
 *   - Public K-12 system: Organized secondary actor (organized/constrained) — benefit from college placement rates (reputation funding); bear costs of test-teaching, resource concentration, inequality amplification
 *   - Analytical observer: Structural analyst (analytical/analytical) — sees both genuine coordination and genuine extraction; risks either naturalizing market inequality or underestimating peer-effect benefits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(college_admissions_market, 0.58).
domain_priors:suppression_score(college_admissions_market, 0.68).
domain_priors:theater_ratio(college_admissions_market, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(college_admissions_market, extractiveness, 0.58).
narrative_ontology:constraint_metric(college_admissions_market, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(college_admissions_market, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(college_admissions_market, tangled_rope).
narrative_ontology:human_readable(college_admissions_market, "The US Elite College Admissions Market").
narrative_ontology:topic_domain(college_admissions_market, "social/economic").

domain_priors:requires_active_enforcement(college_admissions_market).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(college_admissions_market, elite_institutions).
narrative_ontology:constraint_beneficiary(college_admissions_market, test_prep_industry).
narrative_ontology:constraint_beneficiary(college_admissions_market, high_socioeconomic_applicants).
narrative_ontology:constraint_victim(college_admissions_market, low_income_applicants).
narrative_ontology:constraint_victim(college_admissions_market, first_generation_students).
narrative_ontology:constraint_victim(college_admissions_market, epistemic_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME APPLICANT (SNARE) — Trapped by information asymmetry, SAT prep costs ($3K-$15K), application fees (70+ schools × $90), and lack of institutional guidance. Cannot exit without foreclosing elite pathway. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(college_admissions_market, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIRST-GENERATION STUDENT (SNARE) — Bears costs of navigating opaque institutional norms (essay writing, demonstrated interest, extracurricular signal) without family models or social capital. No meaningful exit option except tier-down. d≈0.90, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(college_admissions_market, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-SOCIOECONOMIC APPLICANT (TANGLED ROPE) — Benefits from test prep access, legacy preference, athletic recruitment, and social capital signaling (unpaid internships, travel). Also constrained by intense competition and psychological pressure. Coordination function: selective admissions curate peer cohorts. d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.29.
constraint_indexing:constraint_classification(college_admissions_market, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ELITE INSTITUTION (ROPE) — Experiences admissions as coordination: generates peer effects, alumni networks, and institutional prestige through selective curation. Extraction (ε=0.58) benefits the institution through prestige capital and alumni giving. d≈0.10, f(d)≈-0.05, σ=1.0 → χ≈-0.03. Net beneficiary.
constraint_indexing:constraint_classification(college_admissions_market, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TEST PREP INDUSTRY (ROPE) — Organized market actor (Kaplan, Princeton Review, boutique tutors). Experiences admissions market as coordination problem needing profitable solution. Extraction (ε=0.58) becomes revenue stream. d≈0.08, f(d)≈-0.04, σ=1.0 → χ≈-0.02. Net beneficiary through service provision.
constraint_indexing:constraint_classification(college_admissions_market, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PUBLIC K-12 SYSTEM (TANGLED ROPE) — Organized actor with mixed incentives. Benefits from college placement rates (reputation, funding) but bears costs of teaching to test, resource concentration in test-prep-rich schools, and inequality amplification. d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(college_admissions_market, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global perspective, elite college admissions exhibits genuine coordination function (peer effects, network formation) AND extractive mechanism (rent-seeking via signaling, status competition, inequality reproduction). Both functions are structural. d≈0.68, f(d)≈1.00, σ=1.2 → χ≈0.70. The constraint's true classification.
constraint_indexing:constraint_classification(college_admissions_market, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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

:- end_tests(college_admissions_market_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The system transfers significant resources and opportunity from low-income to high-income families through test prep costs ($3K-$15K), application fees, and implicit capital requirements. However, extractiveness is not as severe as pure predatory extraction (0.70+) because elite institutions do provide genuine peer-effect coordination benefits: admitted cohorts have measurable positive impacts on peer outcomes (earnings, network access, intellectual development). The 0.58 value reflects a 45-55 split between coordination and extraction. The value has increased from 0.35 (40 years ago) as test-prep professionalization, application inflation (7+ apps per student), and prestige concentration have amplified the extraction component. Suppression (0.68): High. Significant barriers include: SAT/ACT test prep costs and time requirements, application fee barriers (70+ applications × $90), geographic/information asymmetry (rural students, immigrant families), lack of institutional guidance in under-resourced schools, essay-writing requirements that correlate with family writing culture, and publication bias (elite institutions dominate media coverage, signaling that other paths are inferior). These barriers are structural rather than accidental — they emerge from institutional design choices (test reliance, application fees, subjective essays) and market dynamics (prestige concentration). However, suppression is not total — some low-income students do access test prep through nonprofits, some institutions offer fee waivers, some states have strong public universities offering alternatives. Theater ratio (0.64): Moderate-high. The admissions process involves significant performative activity: essays assessing 'fit' rather than intellectual capability, demonstrated interest through campus visits (privilege those with geographic proximity), extracurricular activities as proxies for student quality (advantage wealthier families with resource access), legacy preferences (entirely symbolic), and recruitment athletics (combine performance signaling with institutional branding). The theater has increased as competition has intensified — institutions use admissions rituals to reinforce prestige positioning rather than to optimize peer matching.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits acute perspectival divergence driven by structural position. Low-income applicants experience pure extraction (Snare): they bear all costs with minimal coordination benefits. Elite institutions experience net coordination (Rope): they genuinely benefit from peer curation even as they extract rent. The test prep industry experiences profitable coordination (Rope): it solves a real problem (preparation for high-stakes test) while extracting fees. High-socioeconomic applicants experience mixed extraction (Tangled Rope): they benefit from coordination (peer networks) but also bear costs of competition intensity and psychological pressure. The analytical observer sees the true structure (Tangled Rope): both coordination and extraction are real, both are structural. The perspectival gap between low-income applicants (Snare) and elite institutions (Rope) reveals the core extraction mechanism: what appears to institutions as legitimate peer-curation appears to low-income students as a filter designed to exclude them.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income applicants: Victims + trapped → d≈0.92. f(d)≈1.40. Maximum extraction — no meaningful exit except tier-down. First-generation students: Victims + trapped → d≈0.90, f(d)≈1.38. Near-maximum extraction — cultural capital asymmetry creates information trap. High-socioeconomic applicants: Beneficiaries + constrained → d≈0.45, f(d)≈0.50. Moderate extraction — benefit from coordination (peer networks) and from resource access, but constrained by intense peer competition. Elite institutions: Beneficiaries + arbitrage → d≈0.10, f(d)≈-0.05. Net beneficiary — experience admissions as coordination problem with prestige solution. Test prep industry: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.04. Net beneficiary — extract profit from solving access inequality (creating moral hazard). Public K-12 system: Mixed + constrained → d≈0.65, f(d)≈0.95. Significant extraction — benefits from college placement metrics but bears costs of inequality amplification. Analytical observer: Structural + analytical → d≈0.68, f(d)≈1.00, σ=1.2 → χ≈0.70. The constraint's true classification emerges from the analytical view.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via structural decomposition. The constraint's high extractiveness (0.58) combined with genuine coordination function creates a true Tangled Rope classification, not a mislabeled Snare. The mandatrophy is resolved by: (1) Identifying the genuine coordination function: elite institutions do produce peer-effect benefits measured in graduate outcomes and network access. (2) Identifying the genuine extraction mechanism: test-prep costs, application fees, information asymmetry, and systemic inequality all operate as zero-sum competition for positional goods. (3) Recognizing these are NOT substitutes — both operate simultaneously. Low-income students are extracted from AND denied coordination benefits. High-income students extract via resource advantage AND receive coordination benefits. (4) The indexical classification captures this: Powerless agents see Snare (extraction without coordination), institutional agents see Rope (coordination with arbitrage), analytical observer sees Tangled Rope (both genuine). This is not ambiguity — it is accurate perspectival variation revealing the constraint's true structure. If the system were pure Snare, we would expect NO institutional agents to perceive coordination value; they do, which falsifies the Snare hypothesis. If the system were pure Rope, we would expect powerless agents to perceive some coordination benefit; they do not, which falsifies the Rope hypothesis. The Tangled Rope classification is mandatrophy-resolving because it explains the perspectival divergence via structural position, not via measurement ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    signal_vs_skill_decomposition,
    'How much of the admissions market''s extractiveness comes from genuine peer-effect coordination (colleges matching students for intellectual complementarity) versus pure signaling extraction (credentials as zero-sum status games)?',
    'Long-term outcome analysis: correlation between admitted cohort composition and graduate earnings/productivity; comparison of peer effects from ''random'' groupings vs actual admissions selections; causal inference from natural experiments (policy changes, merit aid eligibility thresholds)',
    'If peer effects are real: ε should be 0.30-0.40 (mostly coordination). If signaling dominates: ε should be 0.65-0.75 (mostly extraction). Current value (0.58) assumes 50-50 split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signal_vs_skill_decomposition, empirical, 'Decomposition of coordination function from extraction mechanism').

omega_variable(
    test_prep_capacity_effect,
    'Does the 30-year expansion of test prep availability and quality reduce or amplify suppression for low-income applicants?',
    'Time-series analysis of SAT score gaps between high-income and low-income students; regression discontinuity at test prep price thresholds; comparison of gap reduction vs gap stability across cohorts with increasing test prep market penetration',
    'If test prep accessibility improves outcomes: suppression should decline (toward 0.50). If test prep creates an arms race: suppression should remain high or increase (0.70+). Current assumption (0.68) reflects partial accessibility but persistent inequality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(test_prep_capacity_effect, empirical, 'Effect of test prep market expansion on income-based suppression').

omega_variable(
    legacy_preference_counterfactual,
    'If legacy preferences and athletic recruitment were eliminated, how much would the admission process shift toward merit signaling vs coordination?',
    'Analysis of institutions that eliminated legacy preferences (Amherst, Stanford, etc.); comparison of peer composition changes; measurement of whether admissions criteria shifted toward standardized metrics (pure signaling) or toward institutional mission signals (coordination)',
    'If shift toward merit metrics: extractiveness increases (more pure signaling). If mission diversity maintained: extractiveness stable or decreases (signaling is moderated by coordination goals). Current value (0.58) assumes partial persistence of coordination despite preferences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legacy_preference_counterfactual, empirical, 'Counterfactual impact of eliminating legacy and athletic preferences').

omega_variable(
    international_student_arbitrage_scope,
    'Does the expansion of international admissions represent genuine peer-effect coordination (international diversity) or pure revenue extraction (higher tuition from wealthier international families)?',
    'Institutional financial data: proportion of international students paying full sticker vs receiving aid; comparison of admitted international cohort quality metrics vs domestic cohort; analysis of whether international diversity is prioritized in admissions rubrics or is a side effect of revenue optimization',
    'If coordination dominates: international admissions signal genuine diversification goals. If extraction dominates: ε increases (0.65+) for international students specifically, revealing segmented market extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_student_arbitrage_scope, empirical, 'Whether international student enrollment is coordination or extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(college_admissions_market, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cam_tr_t0, college_admissions_market, theater_ratio, 0, 0.45).
narrative_ontology:measurement(cam_tr_t20, college_admissions_market, theater_ratio, 20, 0.55).
narrative_ontology:measurement(cam_tr_t40, college_admissions_market, theater_ratio, 40, 0.64).

% Extraction over time
narrative_ontology:measurement(cam_be_t0, college_admissions_market, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cam_be_t20, college_admissions_market, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(cam_be_t40, college_admissions_market, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(college_admissions_market, resource_allocation).
narrative_ontology:affects_constraint(college_admissions_market, student_debt_trap).
narrative_ontology:affects_constraint(college_admissions_market, educational_inequality).
narrative_ontology:affects_constraint(college_admissions_market, prestige_concentration).

% DUAL FORMULATION NOTE:
% The college admissions market decomposes into several structurally distinct constraints. The admissions-as-matching mechanism (this story, ε=0.58, Tangled Rope) is downstream of prestige-concentration dynamics (ε>0.70, likely Snare at system level) and upstream of student-debt accumulation (ε=0.65, Snare). These are linked: admissions market incentivizes expensive college choices; expensive choices require debt; debt trap emerges. The ε values differ because the observables are different: admissions extractiveness measures match asymmetry and suppression; debt-trap extractiveness measures financial predation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(college_admissions_market, organized, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
