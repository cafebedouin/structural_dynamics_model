% ============================================================================
% CONSTRAINT STORY: medical_liability_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_medical_liability_doctrine, []).

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
 *   constraint_id: medical_liability_doctrine
 *   human_readable: Medical Liability Doctrine and Defensive Medicine
 *   domain: healthcare/law/economic
 *
 * SUMMARY:
 *   Medical liability doctrine comprises the legal framework governing
 *   physician accountability for patient harm, combined with the
 *   institutional machinery of malpractice insurance and litigation. This
 *   constraint exhibits a core structural tension: it purports to serve
 *   coordination (establishing standards of care, compensating injured
 *   patients, deterring negligence) but operationally extracts from both
 *   physicians and patients while benefiting insurers and litigation
 *   attorneys. The doctrine forces physicians into defensive medicine —
 *   ordering tests and procedures primarily to demonstrate adherence to
 *   liability protection rather than for clinical benefit — thereby
 *   increasing costs for patients without improving outcomes. Simultaneously,
 *   the doctrine fails to reliably compensate genuinely injured patients:
 *   litigation is expensive, outcomes are unpredictable, and settlement
 *   incentives often favor insurers over both patients and physicians. The
 *   theater_ratio (0.68) reflects the performative dimension: much of the
 *   litigation process (discovery, expert reports, jury procedures) produces
 *   the appearance of rigorous truth-seeking without correlating strongly to
 *   case quality or evidence strength. Over the measurement interval (years
 *   0-15), theater has increased as defensive medicine has become more
 *   elaborate and litigation procedures more complex, while extractiveness
 *   has increased as costs to physicians and patients have mounted and
 *   coordination benefits have failed to materialize proportionally.
 *
 * KEY AGENTS:
 *   - Injured Patients: Primary victims (powerless/trapped) — trapped in liability system, bear costs of defensive medicine and litigation delays, receive unpredictable compensation
 *   - Practicing Physicians: Secondary victims (moderate/constrained) — forced into defensive medicine, pay high insurance premiums, constrained by liability exposure but can theoretically exit profession
 *   - Malpractice Insurers: Primary beneficiaries (institutional/arbitrage) — capture steady premium revenue, control risk pools, experience constraint as pure coordination mechanism
 *   - Litigation Bar: Secondary beneficiaries (institutional/arbitrage) — derive entire practice domain from liability doctrine, experience as coordination mechanism enabling predictable cases and fee structures
 *   - State Licensing Boards: Institutional observers (institutional/constrained) — ostensibly enforce medical standards but largely redundant with liability doctrine; maintain performative role through inertia
 *   - Medical Association Coalition: Organized agents (organized/constrained) — advocate for reform, create practice guidelines, experience mixed extraction and coordination
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent liability assignment choices as inevitable features of medical practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(medical_liability_doctrine, 0.58).
domain_priors:suppression_score(medical_liability_doctrine, 0.62).
domain_priors:theater_ratio(medical_liability_doctrine, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(medical_liability_doctrine, extractiveness, 0.58).
narrative_ontology:constraint_metric(medical_liability_doctrine, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(medical_liability_doctrine, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(medical_liability_doctrine, tangled_rope).
narrative_ontology:human_readable(medical_liability_doctrine, "Medical Liability Doctrine and Defensive Medicine").
narrative_ontology:topic_domain(medical_liability_doctrine, "healthcare/law/economic").

domain_priors:requires_active_enforcement(medical_liability_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(medical_liability_doctrine, malpractice_insurers).
narrative_ontology:constraint_beneficiary(medical_liability_doctrine, litigation_attorneys).
narrative_ontology:constraint_victim(medical_liability_doctrine, patients).
narrative_ontology:constraint_victim(medical_liability_doctrine, physicians).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INJURED PATIENT (SNARE) — Trapped within the liability system: must navigate complex legal procedures, faces barriers to compensation despite genuine injury, bears costs of defensive medicine through higher premiums and restricted treatment options. No meaningful exit — cannot opt out of the medical liability regime. Maximum extraction from a patient perspective.
constraint_indexing:constraint_classification(medical_liability_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRACTICING PHYSICIAN (TANGLED ROPE) — Constrained by malpractice liability exposure, forced to practice defensive medicine, bears significant costs (time, expense, psychological burden). But also benefits from the liability system: provides insurance coverage, establishes standard-of-care protocols that protect from extreme liability, offers procedural fairness mechanisms. High suppression (fear of lawsuit) but genuine coordination function (standards protect collective practice). Constrained exit — leaving medicine is costly but theoretically possible.
constraint_indexing:constraint_classification(medical_liability_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MALPRACTICE INSURER (ROPE) — Primary beneficiary. Experiences the constraint as coordination: the liability doctrine creates predictable risk pools, enables pricing, and generates steady premium revenue. Can arbitrage between markets, adjust underwriting criteria, exit specific segments. Low experienced extraction — the constraint solves the coordination problem of risk management. Net flow of value toward this agent.
constraint_indexing:constraint_classification(medical_liability_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LITIGATION BAR (ROPE) — Secondary beneficiary. Experiences the constraint as a coordination mechanism that reliably generates cases, establishes fee structures (contingency basis), and creates predictable legal procedures. Can arbitrage between jurisdictions, pursue high-value cases, exit specific practice areas. The liability doctrine creates their entire practice domain. Net flow of value toward this agent.
constraint_indexing:constraint_classification(medical_liability_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE LICENSING BOARD (PITON) — Ostensibly enforces medical standards but largely performs theater: licensing boards rarely discipline physicians for defensive medicine or for outcomes within the standard-of-care envelope. The board's function (protecting public) has been displaced by insurance and litigation mechanisms. Maintains performative role through institutional inertia — exists because historical precedent created it, but actual enforcement is degraded and often redundant with liability doctrine itself.
constraint_indexing:constraint_classification(medical_liability_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MEDICAL ASSOCIATION COALITION (TANGLED ROPE) — Organized physicians and professional groups advocate for liability reform, create practice guidelines that coordinate defensive medicine, establish mutual aid mechanisms. Constrained (must operate within existing doctrine) but organized (sufficient power to shape standards and norms). Experiences both extraction (forced to accept high malpractice costs) and coordination benefit (unified standard-of-care definitions protect members collectively). Moderate power and constrained exit — can negotiate within the system but cannot dismantle it.
constraint_indexing:constraint_classification(medical_liability_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, medical liability appears as an inevitable feature of any system where physicians make decisions under uncertainty: risk of harm is intrinsic to medical practice, accountability is a natural law of complex systems, and some liability mechanism must exist. The doctrine appears immutable. However, the structural data contradicts this — the theater_ratio (0.68), the suppression asymmetry, and the beneficiary/victim distribution reveal this as a false summit. The 'inevitable' framing naturalizes what are contingent liability assignment choices.
constraint_indexing:constraint_classification(medical_liability_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(medical_liability_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(medical_liability_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(medical_liability_doctrine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(medical_liability_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(medical_liability_doctrine, TR),
    TR >= 0.70.

:- end_tests(medical_liability_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The doctrine extracts substantially from physicians (defensive medicine costs, insurance premiums, litigation exposure) and patients (higher healthcare costs, restricted treatment options, unpredictable compensation when injured). But the extraction is not as severe as pure snare (0.70+) because: (a) genuine coordination function exists (standards of care do guide practice), (b) some compensation reaches injured patients (though inefficiently), (c) the regime has created institutions (medical associations, insurance pools) that partially mitigate extraction. The 0.58 value reflects significant extraction layered onto coordination. Suppression (0.62): Moderate-high. Barriers to exit include: specialized licensing that cannot be easily abandoned, sunk education costs, professional identity fusion, regulatory barriers to practice alternatives, and fear of litigation exposure. Physicians cannot realistically shift costs to alternative systems; patients cannot opt out. High suppression but not total — some physicians do exit, some patients seek alternative medicine. Theater ratio (0.68): High and increasing. Performative elements include: (a) expert testimony that often reflects litigation attorney advocacy rather than evidence, (b) jury procedures that create appearance of deliberation without ensuring verdict quality correlates to case strength, (c) defensive medicine that produces documentation (tests, imaging, consultations) signaling adherence to liability protection rather than clinical necessity, (d) settlement negotiations that optimize for cost containment rather than accuracy or justice. The increasing theater over the interval reflects that as defensive medicine has become more elaborate, the documentation and procedural elements have become more theatrical.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range from snare (injured patient, powerless) through tangled rope (physicians, medical associations) to rope (insurers, attorneys) to piton (licensing boards) with an analytical false summit at the universal scope. The perspectival gap reveals structural asymmetry: agents with arbitrage options (insurers, attorneys) perceive coordination and benefit; agents with constrained or trapped options (physicians, patients) perceive extraction and suffer. The gap is NOT merely perceptual disagreement — it is structural. Injured patients literally have no exit and literally bear costs (higher insurance premiums, restricted treatments). Insurers literally have exit options (withdraw from market, adjust underwriting) and literally benefit (steady premiums, risk pools). The classification differences are not competing interpretations of identical facts — they are true descriptions of different structural positions relative to the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from structural position: beneficiary status, exit options, and power level. Malpractice insurers hold institutional power with arbitrage exit — they derive low d (around 0.10-0.20) from beneficiary status, producing low or negative f(d), resulting in low experienced extraction chi. They perceive the constraint as pure coordination. Injured patients hold powerless status with trapped exit — they derive high d (around 0.90-0.95) from victim status, producing maximum f(d), resulting in high experienced extraction chi. They perceive the constraint as snare. Physicians hold moderate power with constrained exit — they derive moderate-high d (around 0.60-0.70) from mixed victim/beneficiary status (they benefit from standardized care guidelines but suffer from liability exposure), producing moderate f(d), resulting in moderate extraction chi perceived as tangled rope. The organized medical association holds organized power with constrained exit — they derive moderate d (around 0.45-0.55), producing mixed coordination and extraction, classifying as tangled rope from their perspective. The analytical observer derives d from observational position (around 0.72-0.73) and risks false summit classification by naturalizing contingent arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's mandatrophy resides in the gap between coordination framing (standards of care, accountability, patient compensation) and extraction reality (defensive medicine costs, unpredictable litigation outcomes, asymmetric beneficiary distribution). The doctrine claims to coordinate — to establish what constitutes acceptable medical practice and to compensate those harmed by deviation. This claim is genuine; the coordination function is real: physicians do benefit from standardized care definitions, and some injured patients do receive compensation. But the extraction component (theater, suppression, selective beneficiary distribution) has grown faster than the coordination benefit, as measured by the increasing theater_ratio and extractiveness over the interval. The mandatrophy is resolved by recognizing that the doctrine is legitimately tangled rope from multiple perspectives, not a pure coordination mechanism corrupted by misuse. The constraint contains both functions: genuine coordination (standards) and genuine extraction (insurance rent-seeking, litigation attorney gains, defensive medicine costs). The mediation between them is not a policy fix — it is a structural choice about how much extraction to tolerate to maintain the coordination benefit. The analysis shows that extraction is not incidental waste; it is the profitable output that attracts and sustains the institutional beneficiaries (insurers, attorneys). Reducing extraction would require deliberately reducing beneficiary returns, which means the beneficiaries would withdraw, which means the doctrine would lose institutional support. The mandatrophy is thus irresolvable within the current institutional structure: you cannot have low-cost coordination while maintaining the incentives that sustain the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    standard_of_care_definition_ambiguity,
    'Is the ''standard of care'' used in liability doctrine functionally defined by what physicians actually do (defensive practices) or by normative best practices?',
    'Longitudinal analysis of expert testimony: what practices are cited as ''standard'' in cases over time. Correlation between prevalence of defensive practice and expert consensus on medical evidence.',
    'If defined by actual practice: defensive medicine becomes legally privileged (circular extraction — liability forces behavior that then defines acceptable behavior). If defined by evidence: defensive practices should be distinguishable from standard care and penalized, breaking the extraction cycle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(standard_of_care_definition_ambiguity, empirical, 'Whether standard of care is defined by actual practice or evidence-based norms').

omega_variable(
    insurer_moral_hazard_mechanism,
    'Do malpractice insurers have incentives to reduce frivolous suits, or do they systematically prefer settlement of marginal claims to contain litigation costs?',
    'Analysis of insurer settlement patterns vs jury outcomes; comparison of claims insurers settle vs those taken to trial; study of insurer rate structures and how they incentivize physician behavior.',
    'If insurers favor settlement: the liability system is extracting from physicians and patients alike (both pay; both have little control). If insurers aggressively defend: the system functions as intended (weeds out frivolous claims, preserves valid ones). Directly affects chi calculation for physician and patient perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurer_moral_hazard_mechanism, empirical, 'Insurer settlement incentives and their effect on physician extraction').

omega_variable(
    defensive_medicine_quantification,
    'What proportion of current medical spending is defensive medicine (tests/procedures ordered primarily for liability protection, not clinical benefit)?',
    'Meta-analysis of studies estimating defensive medicine prevalence and cost; physician surveys on test ordering rationale; comparison of practice patterns in low-liability vs high-liability jurisdictions.',
    'If defensive medicine > 20% of costs: tangled_rope classification confirmed (significant extraction hidden in medical spending). If < 5%: the extraction component is minimal and coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defensive_medicine_quantification, empirical, 'Proportion of medical spending driven by liability protection').

omega_variable(
    liability_regime_alternatives_feasibility,
    'Are alternative liability regimes (no-fault systems, contractual liability caps, enterprise liability) feasible replacements, or do they produce different but equally severe extraction mechanisms?',
    'Comparative study of countries/states with alternative regimes (Sweden, New Zealand no-fault systems; contract medicine in selected sectors); analysis of physician and patient outcomes under each regime.',
    'If alternatives are feasible and produce lower extraction: the current doctrine is contingent extraction, not natural law. If all regimes produce equivalent extraction: the constraint is closer to a mountain (liability-driven coordination cost is inherent). Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_regime_alternatives_feasibility, empirical, 'Feasibility and comparative extraction costs of alternative liability regimes').

omega_variable(
    plaintiff_win_rate_asymmetry,
    'Do plaintiffs with strong evidence of harm win at higher rates than those with weaker cases, or does win rate fail to correlate with evidence strength?',
    'Case-level analysis: match expert assessments of evidence strength against jury outcomes; study verdict predictability from case characteristics.',
    'If win rate correlates with evidence: the system functions as truth-seeking mechanism (rope). If win rate is random or driven by attorney quality/resources: the system is pure extraction mechanism (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(plaintiff_win_rate_asymmetry, empirical, 'Correlation between evidence strength and plaintiff win rates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(medical_liability_doctrine, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(medlia_tr_t0, medical_liability_doctrine, theater_ratio, 0, 0.52).
narrative_ontology:measurement(medlia_tr_t5, medical_liability_doctrine, theater_ratio, 5, 0.6).
narrative_ontology:measurement(medlia_tr_t10, medical_liability_doctrine, theater_ratio, 10, 0.68).
narrative_ontology:measurement(medlia_tr_t15, medical_liability_doctrine, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(medlia_be_t0, medical_liability_doctrine, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(medlia_be_t5, medical_liability_doctrine, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(medlia_be_t10, medical_liability_doctrine, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(medlia_be_t15, medical_liability_doctrine, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(medical_liability_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(medical_liability_doctrine, defensive_medicine_escalation).
narrative_ontology:affects_constraint(medical_liability_doctrine, healthcare_cost_inflation).
narrative_ontology:affects_constraint(medical_liability_doctrine, pharmaceutical_litigation_risk).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
