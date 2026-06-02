% ============================================================================
% CONSTRAINT STORY: information_asymmetry_edtech
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_asymmetry_edtech, []).

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
 *   constraint_id: information_asymmetry_edtech
 *   human_readable: Information Asymmetry in Educational Technology Markets
 *   domain: education/technology/economics
 *
 * SUMMARY:
 *   Educational technology markets exhibit a structural information asymmetry
 *   where vendors possess superior knowledge of system capabilities,
 *   limitations, security risks, and pedagogical effectiveness, while
 *   institutional buyers and students must make adoption decisions under
 *   radical uncertainty. This asymmetry is enforced through contractual data
 *   lock-in, switching cost accumulation, and systematic opacity regarding
 *   system performance metrics. Over the 15-year interval, both
 *   extractiveness and theater ratio have risen as market concentration
 *   increases, vendor contracts grow more restrictive, and compliance systems
 *   become more dependent on vendor-provided metrics. The constraint
 *   demonstrates how a single structural phenomenon — vendor knowledge
 *   advantage — appears as pure extraction (snare) to trapped parties, mixed
 *   coordination-extraction (tangled rope) to constrained parties, pure
 *   coordination (rope) to institutional beneficiaries, a solvable problem
 *   (scaffold) to organized coalitions, and a degraded compliance ritual
 *   (piton) to accreditation systems. The analytical observer risks
 *   naturalizing this as an immutable feature of technology markets
 *   (mountain), but the structural data reveals it as a false summit enforced
 *   by contingent business model choices and institutional capture.
 *
 * KEY AGENTS:
 *   - EdTech Vendors: Primary beneficiary (institutional/arbitrage) — capture user attention data, lock institutions into upgrade cycles, extract switching costs and contractual penalties
 *   - Institutional Buyers: Primary victim (powerless/trapped) — locked into vendor ecosystems after investment in curriculum redesign, staff training, data integration; cannot access independent performance data
 *   - Students: Primary victim (powerless/trapped) — no choice in platform adoption; data captured and monetized; trapped by institutional mandate and age-based power asymmetry
 *   - Educators: Secondary victim (moderate/constrained) — gain coordination benefits but constrained by administrative mandates; limited autonomy over pedagogical tool selection
 *   - Open Education Movement: Organized coalition (organized/constrained) — OER advocates, open-source projects, institutional consortia building alternative pathways with sunset logic
 *   - Accreditation and Compliance Systems: Institutional actor (institutional/arbitrage) — maintains performative dependence on vendor metrics; benefits from convenience of standardized measurement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent market properties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_asymmetry_edtech, 0.58).
domain_priors:suppression_score(information_asymmetry_edtech, 0.62).
domain_priors:theater_ratio(information_asymmetry_edtech, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_asymmetry_edtech, extractiveness, 0.58).
narrative_ontology:constraint_metric(information_asymmetry_edtech, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(information_asymmetry_edtech, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_asymmetry_edtech, snare).
narrative_ontology:human_readable(information_asymmetry_edtech, "Information Asymmetry in Educational Technology Markets").
narrative_ontology:topic_domain(information_asymmetry_edtech, "education/technology/economics").

domain_priors:requires_active_enforcement(information_asymmetry_edtech).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(information_asymmetry_edtech, edtech_vendors).
narrative_ontology:constraint_victim(information_asymmetry_edtech, institutional_buyers).
narrative_ontology:constraint_victim(information_asymmetry_edtech, students).
narrative_ontology:constraint_victim(information_asymmetry_edtech, educators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INSTITUTIONAL BUYER (SNARE) — School districts and universities face vendor lock-in after adoption. Switching costs are prohibitive: curriculum redesign, staff retraining, data migration complexity, and contractual penalties. Trapped by upfront investment and network effects. Cannot access performance data to evaluate whether the system actually improves learning outcomes. Maximum experienced extraction — forced to continue payments and adoption despite degraded or ineffective systems.
constraint_indexing:constraint_classification(information_asymmetry_edtech, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE STUDENT (SNARE) — No choice in platform adoption; required to use vendor systems as condition of enrollment. Data from learning activities is captured by vendors for profiling and monetization. Cannot exit without withdrawing from institution. Trapped by institutional mandate and age-based power asymmetry (especially for K-12). Full extraction: no compensation, no transparency, no recourse.
constraint_indexing:constraint_classification(information_asymmetry_edtech, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: THE EDUCATOR (TANGLED ROPE) — Mixed experience: gains coordination benefits (automated grading, content libraries, student progress tracking) but also bears extraction. Constrained by administrative mandates and platform dependencies. Limited autonomy over pedagogical choices — forced to adapt teaching to system affordances rather than selecting tools for pedagogical goals. Some agency through professional communities advocating for open-source alternatives, but switching costs and institutional inertia are high.
constraint_indexing:constraint_classification(information_asymmetry_edtech, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EDTECH VENDOR (ROPE) — Benefits from information asymmetry as a pure coordination mechanism from vendor perspective: aggregating learning data, standardizing assessment formats, providing infrastructure that would be expensive for institutions to replicate independently. Vendor sees the constraint as solving a genuine coordination problem: how do you coordinate millions of educational interactions across institutional boundaries? The extraction is a side effect of solving this coordination problem, not its primary purpose. High exit capacity through arbitrage — can shift features, pivot market segments, or exit entirely.
constraint_indexing:constraint_classification(information_asymmetry_edtech, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN EDUCATION MOVEMENT (SCAFFOLD) — Organized coalition (OER advocates, open-source software communities, institutional consortia) sees information asymmetry as a temporary coordination failure with a sunset: open learning records, portable learning data standards (Learning Record Store, xAPI), and open-source alternatives (Moodle, Canvas community forks) are building pathways to exit vendor lock-in. Sunset timeline: 15-25 years as data portability regulations and institutional capacity for interoperability mature. Low effective extraction from this perspective because organized agents see agency and exit paths.
constraint_indexing:constraint_classification(information_asymmetry_edtech, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COMPLIANCE AND ACCREDITATION SYSTEM (PITON) — Educational accreditation bodies and accountability systems rely on standardized metrics (test scores, course completion rates, retention) that edtech vendors conveniently provide. This creates a performative feedback loop: vendors optimize for metrics that appear in accreditation reports, institutions adopt vendors to meet compliance requirements, accreditation bodies treat vendor-provided metrics as legitimate outcomes measures. The system persists through institutional inertia and bureaucratic convenience, not because it produces good pedagogical outcomes. Theater ratio is high: substantial portion of institutional adoption is driven by compliance theater, not evidence of learning effectiveness.
constraint_indexing:constraint_classification(information_asymmetry_edtech, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some information asymmetry in complex technical systems is inherent: vendors necessarily know their systems better than external users, and this gap cannot be fully closed without surrendering system complexity and proprietary advantage. This perspective sees asymmetry as an immutable property of technology markets. However, structural data reveals this as a false summit: the measured asymmetry (extractiveness 0.58, suppression 0.62) reflects contingent institutional choices (data lock-in, contractual opacity, limited transparency) rather than inherent technical limits. Full transparency and data portability are technically feasible; their absence is enforced by business model design.
constraint_indexing:constraint_classification(information_asymmetry_edtech, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_asymmetry_edtech_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(information_asymmetry_edtech, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_asymmetry_edtech, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(information_asymmetry_edtech, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(information_asymmetry_edtech, TR),
    TR >= 0.70.

:- end_tests(information_asymmetry_edtech_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Vendors capture substantial surplus through data monopoly, switching cost lock-in, and contractual opacity. However, not at the maximum (0.72+) because institutions retain some negotiating power through competitive alternatives, and open-source platforms provide partial escape routes. The measured value reflects the current market structure with imperfect competition and partial transparency rather than absolute information monopoly. The trajectory from 0.35 to 0.58 shows increasing extraction as market consolidation advances and data becomes more valuable. Suppression (0.62): High. Barriers to exit include sunk costs in curriculum redesign, staff retraining, data integration, contractual penalties, and lack of transparent alternatives. Switching costs are real and substantial. However, not at maximum (0.80+) because some institutions have successfully migrated to open-source platforms, and transparency regulations are beginning to reduce opacity barriers. The trajectory reflects increasing enforcement as contracts tighten and data lock-in deepens. Theater ratio (0.68): Moderately high. Substantial portion of institutional adoption is driven by compliance and accreditation theater (meeting standardized metrics, demonstrating quantified outcomes) rather than genuine pedagogical selection. However, not at the piton-defining threshold (0.70+) because coordination functions are real: vendors do solve legitimate problems (infrastructure, assessment aggregation, progress tracking). The theater derives from the mismatch between institutional compliance requirements and pedagogical effectiveness rather than pure institutional inertia. The trajectory shows theater increasing as accreditation systems become more dependent on vendor-provided metrics.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival divergence. The vendor sees rope — solving a genuine coordination problem of how to manage millions of educational interactions across institutional boundaries. The institutional buyer sees snare — locked in by switching costs and contract penalties, unable to access data needed to evaluate effectiveness. The student sees snare — mandatory participation in data collection with no meaningful consent or exit. The educator sees tangled rope — gains coordination benefits but constrained by administrative mandate and pedagogical autonomy loss. The open education coalition sees scaffold — a solvable temporary problem with exit pathways through open standards and open-source alternatives. The accreditation system sees piton — a degraded ritual maintained through institutional inertia and bureaucratic convenience. The civilizational observer risks seeing mountain — taking the vendor's coordination-problem framing as natural law. The perspectival gaps reveal the true structure: the constraint is snare from the perspectives of those with no exit capacity, tangled rope from those with some agency, rope from those who benefit, and scaffold from those who have organized alternatives. The false summit is the mountain classification, which naturalizes what is actually enforced by business model and institutional design.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position and exit capacity. Vendors with arbitrage exit and beneficiary status get low d (0.15-0.20) producing negative or near-zero chi — they experience the constraint as coordination benefit, not extraction. Institutional buyers who are trapped victims get high d (0.90+), producing maximum chi — they bear the full extraction. Students who are trapped victims get the same high d, experiencing maximum extraction relative to their power. Educators with moderate power and constrained exit get middle-range d (0.55-0.65), producing moderate chi. Organized coalitions with constrained exit but organization power get lower d (0.50-0.55) because their agency reduces the experienced extraction. The analytical observer's d is set at the canonical value for analytical power (0.73), which produces the mountain classification at civilizational scope — but this classification is a false summit, indicating that the analytical position is itself partially captured by naturalizing institutional arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through the perspectival presheaf: the structural phenomenon is not simply 'information asymmetry' but rather six distinct structural relationships to the same underlying data disparity. The vendor's rope classification is not wrong — vendors genuinely solve coordination problems. The buyer's snare classification is not wrong — buyers are genuinely extracted from. These are not competing claims about a single constraint; they are structural facts about different positions within the constraint system. The mandatrophy resolves when we recognize that 'the' constraint does not have a single type — it has a type-function that maps observation position to classification. The false summit classification for the analytical observer is the key diagnostic: it reveals that the mountain perspective is a naturalizing error, not a legitimate classification. The constraint is socially constructed (enforced by business model and institutional choices), not a law of nature. This becomes visible precisely through the perspectival gap analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogical_effectiveness_measurement,
    'Can learning outcome improvements from edtech adoption be reliably measured and attributed to the system, or do confounding factors (selection bias, Hawthorne effect, measurement gaming) make true causality unknowable?',
    'Randomized controlled trials with blinded institutions; longitudinal comparison of learning outcomes in institutions that adopted vs. rejected specific systems; analysis of outcome metric design incentives within vendor contracts',
    'If measurable: institutions can verify claims and renegotiate contracts based on performance. If unmeasurable: information asymmetry is irreducible, and the snare classification is structural. If measurement is gamed: the asymmetry persists despite apparent transparency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_effectiveness_measurement, empirical, 'Whether pedagogical effectiveness can be reliably measured and attributed').

omega_variable(
    data_portability_technical_feasibility,
    'Is the difficulty of migrating learning data between platforms primarily technical (inherent interoperability complexity) or economic (vendor control of data formats and migration APIs)?',
    'Analysis of data schema complexity; testing of xAPI and LRS implementations for cross-platform portability; comparison of migration effort between systems with open vs. proprietary data formats',
    'If technical: lock-in is partially irreducible, and suppression reflects genuine coordination cost. If economic: lock-in is enforced choice, and suppression is pure extraction. If hybrid: decompose into separate constraints (technical coordination + economic enforcement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_portability_technical_feasibility, empirical, 'Technical vs. economic sources of vendor lock-in').

omega_variable(
    market_concentration_and_exit_capacity,
    'How many viable alternative systems exist for a given educational context, and what is the actual cost of migration relative to institutional budgets?',
    'Market concentration analysis (HHI indices by educational segment); survey of migration costs across institution types; tracking of adoption churn rates and reasons for switching',
    'If few alternatives exist: trapped classification is justified (exit is materially impossible). If many alternatives exist but switching costs are high: constrained classification is more accurate. If both metrics change: decompose into separate time-varying constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_concentration_and_exit_capacity, empirical, 'Market alternatives and switching cost barriers').

omega_variable(
    false_summit_natural_law_vs_constructed,
    'Is information asymmetry in edtech inherent to complex technology markets (natural law) or the result of vendor business model choices and institutional capture of accreditation systems (constructed constraint)?',
    'Comparative analysis of transparency levels across vendors with different business models (proprietary, open-source, nonprofit); historical analysis of privacy regulations and their effects on vendor data collection practices; evaluation of whether technical systems designed for transparency (open-source, vendor-neutral platforms) eliminate the asymmetry',
    'If natural law: mountain classification is correct; vendors cannot credibly commit to transparency without losing competitive advantage. If constructed: false summit detector should reclassify to snare or tangled_rope; the constraint is enforced by business model and institutional design choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_vs_constructed, conceptual, 'Inherent technical property vs. enforced institutional arrangement').

omega_variable(
    student_data_monetization_consent,
    'Do institutional consent structures genuinely capture student agency over data use, or do they constitute identity-locked consent where students cannot meaningfully withdraw agreement without exiting education?',
    'Analysis of consent opt-out capacity: can students refuse data use without education penalty? Comparison of educational outcomes for students who opt out vs. consent; evaluation of whether institutions offer meaningful alternatives for students who refuse vendor platforms',
    'If meaningful opt-out: exit_options should be constrained, not trapped. If no meaningful opt-out: identity_locked classification may apply (students trapped by identity as learner). If identity_locked: suppression mechanisms include internalized compliance and epistemic closure about alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(student_data_monetization_consent, empirical, 'Validity of consent structures for student data monetization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_asymmetry_edtech, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infoas_tr_t0, information_asymmetry_edtech, theater_ratio, 0, 0.48).
narrative_ontology:measurement(infoas_tr_t8, information_asymmetry_edtech, theater_ratio, 8, 0.61).
narrative_ontology:measurement(infoas_tr_t15, information_asymmetry_edtech, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(infoas_be_t0, information_asymmetry_edtech, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(infoas_be_t8, information_asymmetry_edtech, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(infoas_be_t15, information_asymmetry_edtech, base_extractiveness, 15, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(infoas_su_t0, information_asymmetry_edtech, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(infoas_su_t8, information_asymmetry_edtech, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(infoas_su_t15, information_asymmetry_edtech, suppression_requirement, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_asymmetry_edtech, resource_allocation).
narrative_ontology:affects_constraint(information_asymmetry_edtech, student_data_monetization).
narrative_ontology:affects_constraint(information_asymmetry_edtech, educational_institutional_lock_in).
narrative_ontology:affects_constraint(information_asymmetry_edtech, pedagogy_vendor_dependency).

% DUAL FORMULATION NOTE:
% Information asymmetry in edtech is upstream of three decomposed constraints: (1) student_data_monetization focuses on the extraction of behavioral data for profit; (2) educational_institutional_lock_in focuses on switching costs and contractual barriers; (3) pedagogy_vendor_dependency focuses on how vendor design choices constrain pedagogical autonomy. Each has its own epsilon and beneficiary/victim structure. The information asymmetry story models the coordination-vs-extraction dynamic at the market level; the downstream stories model specific extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
