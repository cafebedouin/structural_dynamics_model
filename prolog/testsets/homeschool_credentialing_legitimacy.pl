% ============================================================================
% CONSTRAINT STORY: homeschool_credentialing_legitimacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homeschool_credentialing_legitimacy, []).

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
 *   constraint_id: homeschool_credentialing_legitimacy
 *   human_readable: Homeschool Credentialing Legitimacy
 *   domain: education/credentialing/labor_market
 *
 * SUMMARY:
 *   The homeschool credentialing legitimacy constraint represents a
 *   structural tension between educational autonomy (homeschooling parents'
 *   right to design curriculum) and labor market access (requiring
 *   institutional credentials to signal competence). The constraint has
 *   intensified as homeschooling has grown and as college/employer
 *   credentialing standards have narrowed. Parents designing high-quality
 *   education face institutional gatekeeping: transcripts from accredited
 *   institutions are treated as the sole reliable signal, forcing
 *   homeschoolers into costly workarounds (standardized testing, portfolio
 *   development, third-party evaluation services) or credential invisibility.
 *   The constraint exhibits properties of both coordination (colleges and
 *   employers need signals to match talent with opportunities) and extraction
 *   (institutional gatekeepers maintain requirements that preserve their
 *   monopoly on legitimacy regardless of actual competency signaling value).
 *   The theater ratio reflects that institutional credentialing requirements
 *   are increasingly performative: colleges and employers often screen by
 *   credential type (institution name, transcript format) rather than actual
 *   learning outcomes, while alternative assessment methods (portfolios,
 *   demonstrated competencies, project-based learning) receive institutional
 *   dismissal despite comparable or superior predictive validity. The
 *   constraint has grown more extractive over the measurement interval as
 *   homeschooling has expanded (making the credential gap more salient to
 *   parents and students) while institutional willingness to evaluate
 *   non-standard credentials has stalled.
 *
 * KEY AGENTS:
 *   - Homeschooled Students: Primary victims (powerless/trapped) — face institutional credential invisibility affecting college/employment access; have no exit pathway that doesn't require accepting institutional legitimacy hierarchy
 *   - Homeschooling Parents: Mixed position (moderate/constrained) — coordinate education provision while constrained by credentialing barriers; some benefit from credential gaps enabling unmonitored education
 *   - Credentialing Bodies (Colleges, Professional Boards): Primary beneficiaries (institutional/arbitrage) — control legitimacy standards and credential pathways; extract monopoly rent through credential gatekeeping
 *   - Institutional Schools: Secondary beneficiary (institutional/arbitrage) — maintain competitive advantage through credential monopoly; resistant to portfolio-based alternatives that would eliminate gatekeeping
 *   - Homeschool Advocacy Coalition: Organized challengers (organized/constrained) — build alternative credentialing pathways (portfolios, competency assessments) but constrained by lack of institutional recognition
 *   - Selective Colleges: Powerful constrained actor (powerful/constrained) — maintain institutional credentialing requirement as liability-reduction and admissions-screening mechanism, but have capacity to modify requirements
 *   - Analytical Observer: Sees institutional naturalization (analytical/analytical) — risks treating credentialing hierarchy as inevitable feature of education rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homeschool_credentialing_legitimacy, 0.58).
domain_priors:suppression_score(homeschool_credentialing_legitimacy, 0.65).
domain_priors:theater_ratio(homeschool_credentialing_legitimacy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homeschool_credentialing_legitimacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(homeschool_credentialing_legitimacy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(homeschool_credentialing_legitimacy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homeschool_credentialing_legitimacy, tangled_rope).
narrative_ontology:human_readable(homeschool_credentialing_legitimacy, "Homeschool Credentialing Legitimacy").
narrative_ontology:topic_domain(homeschool_credentialing_legitimacy, "education/credentialing/labor_market").

domain_priors:requires_active_enforcement(homeschool_credentialing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homeschool_credentialing_legitimacy, institutional_educators).
narrative_ontology:constraint_beneficiary(homeschool_credentialing_legitimacy, credentialing_bodies).
narrative_ontology:constraint_victim(homeschool_credentialing_legitimacy, homeschooled_students).
narrative_ontology:constraint_victim(homeschool_credentialing_legitimacy, homeschooling_parents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOMESCHOOLED STUDENT (SNARE) — Trapped without meaningful alternatives. Student faces institutional gatekeeping: colleges and employers demand transcripts from accredited institutions, standardized test scores as proxies for legitimacy, or expensive third-party evaluations. The constraint extracts through credential denial — years of education labor are invisible to external parties. Suppression is structural: no regulatory pathway exists for homeschool self-credentialing at scale. Student cannot exit without accepting the legitimacy hierarchy.
constraint_indexing:constraint_classification(homeschool_credentialing_legitimacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HOMESCHOOLING PARENT (TANGLED ROPE) — Constrained by resource barriers and institutional requirements, but also coordinates learning autonomy and family educational values. Parent must navigate dual burdens: designing curriculum (coordination function) while securing recognition (extraction mechanism). Can exit through returning to institutional school or accepting lower-credential pathway, but at significant cost. Partial beneficiary of system's legitimacy ceiling — some parents exploit the credential gap to extract from their children (unaccredited = unmonitored).
constraint_indexing:constraint_classification(homeschool_credentialing_legitimacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIALING BODY (ROPE) — Institutional beneficiary with full arbitrage. Controls the legitimacy standard and the pathway for credential recognition. Experiences the constraint as pure coordination: issuing transcripts and certifications performs a market function (labor market matching, college admission, employer screening). Net positive position — extraction flows toward this agent through monopoly rent. Can arbitrage into related services: standardized testing, third-party portfolio evaluation, credential verification.
constraint_indexing:constraint_classification(homeschool_credentialing_legitimacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HOMESCHOOL ADVOCACY COALITION (TANGLED ROPE) — Organized agents (state homeschool associations, portfolio-based credentialing platforms) see genuine coordination function (creating alternative assessment pathways) and real extraction (required to duplicate institutional systems to gain any legitimacy traction). Coalition has constrained exits: can build parallel credentials, but these lack market recognition without expensive lobbying or institutional partnerships. Extraction is visible but not maximal — coalition has voice and can partially exit through alternative credential frameworks.
constraint_indexing:constraint_classification(homeschool_credentialing_legitimacy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL SCHOOLING SYSTEM (PITON) — The legitimacy requirement for institutional credentials is partially performative. Schools maintain accreditation processes that often measure input compliance (teacher credentials, course hours, facility standards) rather than actual learning outcomes. For homeschooled students, the lack of these inputs is treated as disqualifying despite evidence that homeschool learning is comparable or superior on many metrics. The system maintains the requirement through institutional inertia: 'we've always required transcripts from accredited institutions.' Theater ratio is high because the requirement persists despite being increasingly mismatched to actual credentialing goals.
constraint_indexing:constraint_classification(homeschool_credentialing_legitimacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ELITE SELECTIVE COLLEGES (TANGLED ROPE) — Powerful agents constrained by liability and liability-aversion. Schools maintain the institutional credentialing requirement as a coordination mechanism (simplifies admissions screening) and a risk-reduction mechanism (homeschool transcripts create evaluation burden; institutional transcripts provide standardized risk signals). Powerful position with arbitrage-adjacent exit options (could evaluate homeschool portfolios at scale if motivated). Extraction mechanism: maintaining the requirement extracts effort and cost from homeschoolers while colleges avoid expansion of admissions evaluation burden. Some genuine coordination value (standardized transcripts do aid admissions matching) alongside visible extraction (barrier maintenance).
constraint_indexing:constraint_classification(homeschool_credentialing_legitimacy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — FALSE NATURALIZATION (MOUNTAIN) — From a civilizational view, the legitimacy constraint might appear immutable: educational institutions require credentials from other educational institutions; this is a natural law of bureaucratic legitimacy. However, the structural data contradicts this. The constraint is not a natural law but a contingent institutional arrangement maintained through power asymmetry, risk aversion, and incumbent protection. The 'naturalness' is an artifact of institutional inertia, not a law of educational epistemology.
constraint_indexing:constraint_classification(homeschool_credentialing_legitimacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homeschool_credentialing_legitimacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(homeschool_credentialing_legitimacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(homeschool_credentialing_legitimacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(homeschool_credentialing_legitimacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(homeschool_credentialing_legitimacy, TR),
    TR >= 0.70.

:- end_tests(homeschool_credentialing_legitimacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through credential gatekeeping: years of homeschool educational labor are invisible to labor markets unless repackaged through institutional frameworks (standardized tests, third-party portfolio services, institutional partnerships). The extraction is not total because some pathways exist (direct employment, trade paths, entrepreneurship) that bypass credentials entirely, and some colleges have begun accepting non-standard credentials. The value reflects that the gatekeeping is real and costs are significant (time, money, opportunity cost), but the extraction is not absolute. Suppression (0.65): High. Multiple barriers suppress alternative credentialing: (1) Institutional path-dependency: colleges and employers default to institutional credentials because evaluation costs for non-standard credentials are high; (2) Regulatory barriers: in many jurisdictions, teacher credentialing, course requirements, and assessment standards are mandated for accredited institutions but not optional for homeschools, creating asymmetric information burden; (3) Risk aversion: colleges perceive institutional transcripts as reducing evaluation burden and liability risk; (4) Network effects: as institutional credentials become more standardized, they acquire greater legitimacy, making alternatives harder to establish. Theater ratio (0.68): Moderately high. Institutional credential requirements are increasingly performative. Colleges and employers screen by credential type (institutional name) rather than learning outcomes. Accreditation standards for institutions often measure input compliance (facility standards, teacher qualifications, course hour requirements) rather than actual student learning, yet these inputs are treated as guarantees of quality. Portfolio-based assessment and demonstrated competencies are dismissed despite evidence of comparable or superior predictive validity. The performative content increased over the measurement interval as institutional credentialing became more standardized and risk-averse institutions more reluctant to evaluate non-standard formats.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits sharp perspectival gap between beneficiaries and victims. Institutional credentialing bodies classify the constraint as Rope (pure coordination — credentials facilitate labor market matching and college admissions). Homeschooled students classify it as Snare (pure extraction — credential denial blocks access without reciprocal benefit). Homeschooling parents and colleges occupy intermediate positions (Tangled Rope) where genuine coordination (admissions screening, credibility signaling) coexists with visible extraction (requirement maintenance despite alternative pathways). The analytical observer at civilizational timescale risks misclassifying this as Mountain (institutional credentials are inherent to education systems), but structural data reveals the classification as false: the institutional credential requirement is contingent on power asymmetries (colleges control labor market access) and incumbent protection (institutional schools maintain competitive advantage through gatekeeping), not on immutable educational epistemology. The gap between the naive mountain classification and the actual tangled-rope structure is the gap that reveals the constraint's extractive content.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) reflects their structural relationship to the credential extraction flow. Homeschooled students experience maximum extraction (d ≈ 0.95 → high f(d)) because they are trapped victims bearing full costs of credential gatekeeping without meaningful exit options. Homeschooling parents are moderate victims with some exit optionality (d ≈ 0.70 → moderate f(d)); they can return to institutional school or accept lower-credential pathways, but these come at significant cost. Institutional credentialing bodies and schools experience minimal or negative extraction (d ≈ 0.05–0.15 → low/negative f(d)) because they are beneficiaries: the credential monopoly extracts toward them. Selective colleges, despite being powerful, experience moderate extraction (d ≈ 0.55) because they are partially constrained by liability concerns and admissions evaluation burden, making them less pure beneficiaries than credential monopolists. The homeschool advocacy coalition experiences moderate extraction (d ≈ 0.60) because they have organized voice but constrained exit: they can build alternatives but lack institutional recognition to scale them. The analytical observer at civilizational scope would measure d ≈ 0.72 if naively treating the institutional credentialing requirement as natural law, but the structural data contradicts this measurement.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the homeschool credentialing constraint is not a false positive (genuine coordination problem with collateral extraction) but a real hybrid. The beneficiary claim ('institutional credentials facilitate educational signaling') is true — labor markets do need signals about competency. The extraction claim ('institutional gatekeeping preserves credential monopoly regardless of alternative credibility') is also true — the institutional requirement persists even where alternative credentialing is demonstrated to be reliable. The constraint resolves mandatrophy when both truths are recognized: the requirement performs genuine coordination function (reducing information asymmetry about student competency) AND maintains extractive gatekeeping (preserving institutional competitive advantage by preventing credential substitution). The tangled-rope classification captures this hybrid: both coordination and extraction are real, both are structural, neither can be eliminated without losing the other entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    homeschool_learning_equivalence,
    'Are learning outcomes from homeschooling genuinely equivalent to institutional schooling, or does the credential gap reflect real quality differences?',
    'Longitudinal outcome data: college graduation rates, earnings, professional achievement, and assessments of foundational skills (literacy, numeracy, critical thinking) for homeschooled vs institutionally schooled cohorts controlling for parental education and SES',
    'If equivalent: credential gap is pure rent-seeking extraction (classify as Snare/Piton). If homeschool outcomes lag: gap reflects legitimate signaling function (reclassify toward Rope). If homeschool outcomes exceed: constraint is perverse misallocation of credibility (strongly Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homeschool_learning_equivalence, empirical, 'Whether homeschool learning outcomes match institutional schooling').

omega_variable(
    portfolio_verification_feasibility,
    'Can portfolio-based credentialing (student work samples, demonstrated competencies, third-party assessments) provide reliable labor market signals without institutional gatekeeping?',
    'Experimental or quasi-experimental evaluation: track labor market outcomes for homeschoolers with strong portfolios vs weak institutional transcripts; survey employer hiring practices on portfolio acceptance; assess cost and reliability of portfolio-based screening at scale',
    'If feasible and cost-effective: alternative credentialing paths are structurally possible, confirming that the institutional requirement is not natural law (Scaffold sunset toward Rope). If not feasible: constraint may reflect genuine information asymmetry costs (Rope reclassification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(portfolio_verification_feasibility, empirical, 'Whether portfolio-based credentialing can reliably signal competence').

omega_variable(
    selective_college_risk_aversion_driver,
    'Is the institutional credential requirement primarily driven by liability risk aversion, admissions screening burden reduction, or genuine quality concerns?',
    'Analysis of institutional credentialing policies: interviews with admissions leadership; historical evolution of policies (when did homeschool restrictions appear and why); comparative analysis of admission standards (do colleges apply same rigor to international transcripts, transfer credits from low-selectivity institutions, or test-optional pathways)',
    'If risk aversion dominant: colleges could lower barriers through risk mitigation (portfolio insurance, test bundles); constraint is Tangled Rope with exit options. If burden reduction: constraint will persist unless portfolio tools reduce screening cost. If quality concern genuine: constraint may represent legitimate Rope with signaling value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_college_risk_aversion_driver, empirical, 'Primary driver of institutional credentialing requirements').

omega_variable(
    homeschool_regulation_variation_effect,
    'Does the legitimacy constraint differ meaningfully across jurisdictions with different regulatory frameworks (lightly regulated vs. portfolio-mandated vs. test-mandated states)?',
    'Cross-state policy analysis: credentialing acceptance by colleges and employers for homeschooled students from high-regulation vs low-regulation states; labor market outcomes by regulatory environment; student pathway diversity (college, trade, direct employment) by jurisdiction',
    'If variation exists: constraint is jurisdictionally contingent, not universal (theater ratio lower in high-regulation states, suggesting coordination function stronger where verification is delegated to state authorities). If no variation: constraint is cross-jurisdictionally robust, suggesting powerful institutional coordination or entrenchment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(homeschool_regulation_variation_effect, empirical, 'Whether credentialing legitimacy varies by regulatory environment').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of homeschool credentialing primarily structural (institutional barriers, regulatory requirements) or internalized (homeschooling parents and students accepting the legitimacy hierarchy as natural)?',
    'Analysis of homeschool community narratives: survey homeschooling parents on perceived vs actual barriers to credential recognition; track whether parents seek alternative pathways or accept credential invisibility; measure willingness to invest in portfolio-building or institutional partnerships as proxy for internalizing vs resisting the constraint',
    'If internalized: constraint has higher effective suppression than structural measures suggest; students carry suppression even if institutional barriers were removed. If structural: removing barriers (regulatory change, portfolio acceptance) would materially reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Degree to which suppression is internalized vs structural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homeschool_credentialing_legitimacy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hscl_tr_t0, homeschool_credentialing_legitimacy, theater_ratio, 0, 0.52).
narrative_ontology:measurement(hscl_tr_t5, homeschool_credentialing_legitimacy, theater_ratio, 5, 0.62).
narrative_ontology:measurement(hscl_tr_t10, homeschool_credentialing_legitimacy, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(hscl_be_t0, homeschool_credentialing_legitimacy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hscl_be_t5, homeschool_credentialing_legitimacy, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(hscl_be_t10, homeschool_credentialing_legitimacy, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homeschool_credentialing_legitimacy, identity_coordination).
narrative_ontology:boltzmann_floor_override(homeschool_credentialing_legitimacy, 0.1).
narrative_ontology:affects_constraint(homeschool_credentialing_legitimacy, educational_accreditation_gatekeeping).
narrative_ontology:affects_constraint(homeschool_credentialing_legitimacy, professional_licensing_transparency).
narrative_ontology:affects_constraint(homeschool_credentialing_legitimacy, portfolio_based_hiring_acceptance).

% DUAL FORMULATION NOTE:
% The homeschool credentialing legitimacy constraint is downstream of broader educational accreditation and professional licensing systems but has its own distinct structural properties. Upstream constraints (accreditation standards, professional board requirements) define what counts as 'legitimate' credentialing; the homeschool constraint is the instantiation of these standards in the labor market gatekeeping that excludes non-institutional paths. Decomposition is appropriate because the ε value differs: educational accreditation (upstream) may be lower-extractiveness coordination; the homeschool gatekeeping (this story) has higher extractiveness due to the power asymmetry and incumbent protection mechanisms specific to the educational labor market.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homeschool_credentialing_legitimacy, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
