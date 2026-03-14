% ============================================================================
% CONSTRAINT STORY: credential_portability_barrier
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credential_portability_barrier, []).

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
 *   constraint_id: credential_portability_barrier
 *   human_readable: Credential Portability Barrier Across Jurisdictions and Professions
 *   domain: professional/economic/regulatory
 *
 * SUMMARY:
 *   Credential portability barriers are the enforcement mechanisms by which
 *   professional licensing bodies restrict labor mobility and maintain supply
 *   scarcity, creating rents for incumbent professionals while extracting
 *   opportunity costs from workers seeking geographic or career mobility. The
 *   constraint exhibits a hybrid structure: it contains genuine coordination
 *   functions (standardizing competence criteria, assuring public safety)
 *   alongside asymmetric extraction (supply restriction, geographic
 *   rent-seeking, career path dependency). The barrier operates through both
 *   formal regulatory mechanisms (state licensing boards requiring
 *   re-certification across state lines, credential evaluation agencies
 *   requiring equivalency testing for international credentials) and informal
 *   institutional maintenance (professional associations lobbying against
 *   reciprocity agreements, licensing exams that test jurisdiction-specific
 *   knowledge rather than competence). The theater_ratio has increased over
 *   the interval as the institutional maintenance burden has grown — more
 *   compliance documentation, more administrative gatekeeping, more
 *   performative evaluation — while the actual competence verification
 *   function remains relatively constant. This drift suggests Piton-level
 *   degradation: the system persists through institutional inertia rather
 *   than because it effectively prevents incompetence.
 *
 * KEY AGENTS:
 *   - Mobile Workers: Primary victims (powerless/trapped) — licensed professionals seeking geographic mobility face complete credential revalidation or career interruption
 *   - Career Switchers: Secondary victims (moderate/constrained) — individuals with relevant credentials in adjacent fields face re-licensing requirements despite overlapping competence
 *   - International Credential Holders: Primary victims (powerless/trapped) — professionals trained outside destination country face recognition barriers and bridge program requirements
 *   - State Licensing Boards: Primary beneficiary (institutional/arbitrage) — maintain monopolistic credential control; coordinate professional standards; benefit from restricted supply
 *   - Professional Associations/Trade Guilds: Primary beneficiary (powerful/arbitrage) — maintain supply scarcity supporting wage premiums and licensing fee revenue
 *   - Regulatory Jurisdictions: Beneficiary (institutional/arbitrage) — credential restrictions enable geographic market segmentation and local monopoly protection
 *   - Mutual Recognition Coalition: Organized agents (organized/mobile) — trade frameworks, reciprocity councils, credential evaluation services building portability infrastructure with generational sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes both genuine coordination (competence standardization) and extraction (supply restriction)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credential_portability_barrier, 0.52).
domain_priors:suppression_score(credential_portability_barrier, 0.58).
domain_priors:theater_ratio(credential_portability_barrier, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credential_portability_barrier, extractiveness, 0.52).
narrative_ontology:constraint_metric(credential_portability_barrier, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(credential_portability_barrier, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credential_portability_barrier, tangled_rope).
narrative_ontology:human_readable(credential_portability_barrier, "Credential Portability Barrier Across Jurisdictions and Professions").
narrative_ontology:topic_domain(credential_portability_barrier, "professional/economic/regulatory").

domain_priors:requires_active_enforcement(credential_portability_barrier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credential_portability_barrier, incumbent_licensed_professionals).
narrative_ontology:constraint_beneficiary(credential_portability_barrier, credentialing_bodies).
narrative_ontology:constraint_beneficiary(credential_portability_barrier, regulatory_jurisdictions).
narrative_ontology:constraint_victim(credential_portability_barrier, mobile_workers).
narrative_ontology:constraint_victim(credential_portability_barrier, career_switchers).
narrative_ontology:constraint_victim(credential_portability_barrier, labor_market_efficiency).
narrative_ontology:constraint_victim(credential_portability_barrier, underutilized_credential_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOBILE WORKER (SNARE) — Licensed professional (nurse, electrician, accountant) with credential valid in home state faces complete re-certification or re-licensing requirements to practice in another state. Trapped: cannot practice across state lines despite equivalent training and proven competence. No exit from the constraint without abandoning geographic mobility or income during re-licensing. Maximum extraction experienced — bears full cost of credential non-recognition.
constraint_indexing:constraint_classification(credential_portability_barrier, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERNATIONAL CREDENTIAL HOLDER (SNARE) — Professional trained and credentialed in another country (physician, engineer, architect) seeking to work in destination country faces recognition barriers, equivalency examinations, bridge programs, or complete re-credentialing. Trapped by: non-recognition of foreign credentials, lack of reciprocity agreements, credential evaluation bureaucracy. Highest extraction — skill and knowledge rendered economically unusable despite objective competence. No exit option except credential surrender or geographic restriction.
constraint_indexing:constraint_classification(credential_portability_barrier, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CAREER SWITCHER (TANGLED ROPE) — Individual with legitimate credentials in one profession seeking to transition to adjacent field (software developer to data scientist, nurse to health administrator). Faces credential re-requirements despite overlapping knowledge base. Constrained: can switch (exit exists but costly — re-training, re-certification, income loss), but the barrier is high. Also benefits from credential system that enables specialization and standardization. Mixed extraction and coordination.
constraint_indexing:constraint_classification(credential_portability_barrier, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE LICENSING BOARD (ROPE) — Coordinates standardization within jurisdiction and coordinates public safety assurance. The barrier serves the genuine coordination function of ensuring competence standards. Also benefits from control over credential recognition (can restrict labor supply, manage professional population density). Net beneficiary — constraint enforces monopolistic control. Arbitrage exit: can recognize credentials from reciprocal jurisdictions when politically advantageous, creating local rent-extraction through selective portability.
constraint_indexing:constraint_classification(credential_portability_barrier, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: PROFESSIONAL ASSOCIATION / TRADE GUILD (ROPE) — Coordinates credentialing standards and professional norms; also maintains supply restriction for members. Benefits from credential non-portability — scarcity supports wage premiums and licensing fees. Can arbitrage by lobbying for reciprocity agreements that favor their members. Experiences constraint as pure coordination benefit (maintains professional standard) with extraction as side effect.
constraint_indexing:constraint_classification(credential_portability_barrier, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: MUTUAL RECOGNITION COALITION (SCAFFOLD) — Organized agents (international trade agreements, professional reciprocity councils, credential evaluation services) building mechanisms to enable portability: mutual recognition agreements, standardized equivalency testing, harmonized training standards. See the barrier as temporary — as frameworks mature, portability increases. Low experienced extraction because organized agents have agency and see exit paths (bilateral agreements, reciprocity frameworks). Theater ratio declining as portable frameworks mature.
constraint_indexing:constraint_classification(credential_portability_barrier, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: VESTIGIAL GUILD SYSTEM (PITON) — Historical craft guilds controlled training and market access; modern licensing systems inherited this structure. The original coordination function (ensuring competence in pre-industrial craft contexts) is substantially degraded. Modern licensing persists through institutional inertia — examining boards maintain credential requirements and non-recognition not because they prevent incompetence (training standards are established through education, not licensing) but because the institutional apparatus exists and benefits incumbents. Theater ratio high (0.75+): most of the enforcement activity is performative boundary-maintenance rather than genuine competence-verification.
constraint_indexing:constraint_classification(credential_portability_barrier, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both genuine coordination function (standardization, competence assurance, public safety) AND asymmetric extraction (supply restriction, geographic rent-extraction, barriers to mobility). The constraint solves real problems (ensuring practitioners meet baseline competence) while extracting from those seeking to exercise competence across boundaries. Hybrid structure with both components essential to the constraint's persistence.
constraint_indexing:constraint_classification(credential_portability_barrier, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credential_portability_barrier_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credential_portability_barrier, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credential_portability_barrier, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credential_portability_barrier, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credential_portability_barrier, TR),
    TR >= 0.70.

:- end_tests(credential_portability_barrier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significantly from mobile workers and international credential holders through delayed income during re-licensing, re-examination costs, and career opportunity loss. However, extraction is not absolute (0.70+) because some legitimate competence standardization occurs and because portability is increasing through bilateral agreements. The extractiveness reflects both the genuine coordination overhead and the additional rent-seeking layered atop it. Suppression (0.58): Moderate-high. Barriers to exit include: regulatory prohibition on cross-state practice, credential evaluation bureaucracies, re-examination requirements, re-training mandates, lack of reciprocity agreements. However, suppression is not total (0.75+) because some jurisdictions recognize reciprocal credentials, equivalency pathways exist, and international frameworks are emerging. Theater ratio (0.64): Moderate-high and increasing. Licensing examinations test jurisdiction-specific knowledge (e.g., state-specific healthcare regulations) rather than competence; licensing boards maintain administrative procedures that appear to verify competence but largely maintain boundaries. The theater has increased as the institutional apparatus has grown without corresponding increases in actual competence differentiation. This drift drives the Piton classification at the civilizational perspective.
 *
 * PERSPECTIVAL GAP:
 *   The original constraint demonstrates stark perspectival disagreement. The trapped international credential holder sees pure extraction (Snare) — their competence is rendered economically unusable despite objective qualification. The state licensing board sees pure coordination (Rope) — they are solving the legitimate problem of standardizing professional competence across practitioners. The mutual recognition coalition sees a temporary coordination failure with a sunset (Scaffold) — bilateral agreements and harmonized standards are building alternative pathways. The vestigial guild system sees its own degraded ritual (Piton) — the licensing apparatus persists through inertia, maintaining boundaries that no longer serve genuine competence verification. The career switcher sees mixed extraction and coordination (Tangled Rope) — the barrier both ensures standards and restricts mobility. The analytical observer sees both functions: genuine coordination necessity AND extractive supply restriction. The perspectival gap reveals that the constraint's justification ('ensuring competence') and its mechanism ('restricting labor supply') operate simultaneously — one cannot reduce the constraint to purely coordination or purely extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerless trapped agents (international credential holders, interstate movers) experience maximum directionality — they are full targets of the extraction mechanism with no exit options. Their d approaches 1.0, producing high effective extractiveness chi. Moderate constrained agents (career switchers) experience moderate directionality — they have exit options (accept career restriction, invest in re-credentialing) but these are costly, d ≈ 0.55-0.65. Institutional beneficiaries (licensing boards, professional associations) experience low directionality — they are beneficiaries with arbitrage options, d ≈ 0.10-0.20, producing low or negative chi from their perspective. Organized agents in the mutual recognition coalition experience mobile exit options reducing their experienced extraction despite the constraint's apparent severity — d ≈ 0.45 due to real agency and visible sunset pathway. The analytical observer's d ≈ 0.72 reflects position as external analyst seeing the full asymmetric structure.
 *
 * MANDATROPHY ANALYSIS:
 *   HYBRID STRUCTURE WITH REAL COORDINATION: This constraint resolves the mandatrophy by identifying a genuine, necessary coordination function (competence standardization) that is inseparable from asymmetric extraction (supply restriction for rent maintenance). The constraint cannot be stripped of its extraction component without losing its coordination function — the scarcity that supports rents is the same scarcity that prevents incompetent practitioners from entering markets. However, the current implementation captures MORE extraction than the minimum necessary for coordination. The theater_ratio drift (0.48 → 0.64) reveals that institutional maintenance (administrative gatekeeping, exam bureaucracy) has increased faster than competence standardization requirements, indicating Goodhart decay: the constraint has drifted toward pure institutional maintenance. Mandatrophy resolution: The constraint is legitimately Tangled Rope (requires both coordination and extraction components), but is degrading toward Piton as theater overwhelms function. The analytical observer correctly classifies as Tangled Rope, not as Snare (would deny coordination function) or Rope (would deny extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_verification_threshold,
    'What portion of licensing requirements actually verify competence versus maintaining professional boundaries?',
    'Comparative analysis of competence standards across jurisdictions; correlation between licensing rigor and patient/client outcomes; examination of requirements that differ across regions for identical professions',
    'If >70% is genuine competence verification: constraint is primarily coordination (Rope from more perspectives). If <40% is competence-related: constraint is primarily extraction (Snare from more perspectives). Piton assessment depends on this threshold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_verification_threshold, empirical, 'Proportion of licensing requirements that serve genuine competence verification').

omega_variable(
    reciprocity_emergence_timeline,
    'What is the realistic timeline for mutual recognition agreements to create portability across major jurisdictions and professions?',
    'Historical analysis of reciprocity agreement adoption rates; modeling of political economy of recognizing foreign credentials; surveying of remaining structural barriers to mutual recognition',
    'If timeline < 10 years: Scaffold perspective is realistic, constraint has structural sunset. If timeline > 30 years: Scaffold is aspirational, constraint will persist as tangled rope or snare. Determines whether sunset clause is real or performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_emergence_timeline, empirical, 'Timeline for mutual recognition agreement adoption to create meaningful portability').

omega_variable(
    rent_extraction_magnitude,
    'How much of the wage differential between licensed and non-licensed practitioners in the same role is attributable to credential scarcity versus genuine competence differentiation?',
    'Econometric analysis of wage gaps controlling for actual competence measures; comparison of wage premium across jurisdictions with different credential portability; assessment of wage convergence in high-portability professions',
    'If differential is >60% attributable to scarcity: beneficiaries are capturing significant rents, extractiveness toward 0.60+. If differential is <30% scarcity-driven: most of the wage gap reflects competence, extractiveness toward 0.35-0.40.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_extraction_magnitude, empirical, 'Magnitude of wage premium attributable to credential scarcity versus competence').

omega_variable(
    skill_utilization_decay,
    'Do credential barriers measurably reduce labor market efficiency and economic output through credential-skills mismatch?',
    'Tracking underemployment rates among internationally-credentialed workers; measuring output losses from unfilled positions in credential-scarce regions; comparing GDP/capita across jurisdictions with different credential portability regimes',
    'If measurable efficiency loss is significant: constraint is classified higher in extractiveness across all perspectives. If efficiency loss is marginal: extraction claim is weakened, constraint may reclassify as lower extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_utilization_decay, empirical, 'Whether credential barriers create measurable labor market inefficiency losses').

omega_variable(
    public_safety_evidence,
    'Is there empirical evidence that credential non-portability improves public safety outcomes compared to systems with higher portability?',
    'Comparative patient/client safety outcomes across high-portability versus high-restriction jurisdictions; analysis of competence-related adverse events in professions with mandatory re-credentialing versus accepted reciprocity',
    'If portability correlates with worse outcomes: coordination function is real, constraint reclassifies toward Rope. If portability shows equivalent or better outcomes: ''safety'' justification is cover story, extractiveness interpretation strengthens, constraint reclassifies toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_safety_evidence, empirical, 'Whether credential non-portability produces demonstrable public safety benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credential_portability_barrier, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cred_port_tr_t0, credential_portability_barrier, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cred_port_tr_t10, credential_portability_barrier, theater_ratio, 10, 0.58).
narrative_ontology:measurement(cred_port_tr_t20, credential_portability_barrier, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(cred_port_be_t0, credential_portability_barrier, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cred_port_be_t10, credential_portability_barrier, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(cred_port_be_t20, credential_portability_barrier, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credential_portability_barrier, identity_coordination).
narrative_ontology:affects_constraint(credential_portability_barrier, professional_wage_premium_maintenance).
narrative_ontology:affects_constraint(credential_portability_barrier, geographic_labor_market_segmentation).
narrative_ontology:affects_constraint(credential_portability_barrier, international_skills_underutilization).

% DUAL FORMULATION NOTE:
% Credential portability barrier decomposes into three structurally distinct constraints along domain lines: (1) within-profession credential non-recognition (geographic/jurisdictional barriers) with ε ≈ 0.55; (2) across-profession credential non-transfer (career-switching barriers) with ε ≈ 0.40; (3) international credential recognition failures (cross-border barriers) with ε ≈ 0.65. Each has different beneficiary structures and suppression mechanisms. The present story covers the unified constraint; network links identify related structural mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(credential_portability_barrier, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
