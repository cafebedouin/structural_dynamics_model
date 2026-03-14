% ============================================================================
% CONSTRAINT STORY: credentialing_decoupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credentialing_decoupling, []).

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
 *   constraint_id: credentialing_decoupling
 *   human_readable: Credentialing Decoupling: Institutional Credentials vs. Functional Competence
 *   domain: institutional/labor_markets/education
 *
 * SUMMARY:
 *   Credentialing decoupling is the structural gap between institutional
 *   credentials (licenses, degrees, certifications) and actual job
 *   competence. Workers obtain credentials through credential institutions
 *   (universities, licensing boards, professional associations) at
 *   significant cost and time investment. These credentials are legally or
 *   industry-norm-required to practice in many fields. Yet empirical evidence
 *   shows weak correlation between credential possession and actual job
 *   performance in many domains. The constraint exhibits the full spectrum of
 *   indexical classifications depending on the observer's structural
 *   position: credentialing institutions experience pure coordination (Rope);
 *   gatekeeping professionals experience mixed coordination and extraction
 *   (Tangled Rope); non-credentialed competent workers experience pure
 *   extraction (Snare); alternative credentialing movements see a
 *   transitional problem with sunset logic (Scaffold); vestigial requirements
 *   appear as degraded institutional inertia (Piton); and the analytical
 *   observer risks naturalizing a contingent institutional arrangement as an
 *   immutable law (Mountain). The theater ratio has increased from 0.42 to
 *   0.68 over 30 years as credential requirements have proliferated while
 *   credential-competence correlation has declined — a diagnostic signature
 *   of Goodhart drift and institutional decay.
 *
 * KEY AGENTS:
 *   - Non-Credentialed Competent Workers: Primary victims (powerless/trapped) — functionally capable but legally barred; bear full extraction cost without proportional benefit
 *   - Career-Transitioning Workers: Secondary victims (moderate/constrained) — face high but surmountable costs; partially benefit from credential market access
 *   - Credentialing Institutions: Primary beneficiaries (institutional/arbitrage) — capture tuition/examination/renewal fees; experience constraint as pure coordination
 *   - Gatekeeping Professional Associations: Secondary beneficiaries (organized/constrained) — maintain licensing monopolies and income extraction; coordinate actual competence verification
 *   - Alternative Credentialing Movements: Organized challengers (organized/mobile) — bootcamps, portfolio-based hiring, skills certifications; represent alternative pathways with sunset logic
 *   - Vestigial Credential Requirements: Institutional inertia — licensing boards, occupational regulators maintaining requirements through path dependency, not empirical justification
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks treating credentialing as natural law rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credentialing_decoupling, 0.58).
domain_priors:suppression_score(credentialing_decoupling, 0.62).
domain_priors:theater_ratio(credentialing_decoupling, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credentialing_decoupling, extractiveness, 0.58).
narrative_ontology:constraint_metric(credentialing_decoupling, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(credentialing_decoupling, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credentialing_decoupling, tangled_rope).
narrative_ontology:human_readable(credentialing_decoupling, "Credentialing Decoupling: Institutional Credentials vs. Functional Competence").
narrative_ontology:topic_domain(credentialing_decoupling, "institutional/labor_markets/education").

domain_priors:requires_active_enforcement(credentialing_decoupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credentialing_decoupling, credentialing_institutions).
narrative_ontology:constraint_beneficiary(credentialing_decoupling, gatekeeping_professionals).
narrative_ontology:constraint_victim(credentialing_decoupling, non_credentialed_competent_workers).
narrative_ontology:constraint_victim(credentialing_decoupling, labor_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-CREDENTIALED COMPETENT WORKER (SNARE) — Functionally capable but legally barred from practicing without institutional credentials. Bears full cost of credentialing extraction without proportional benefit. No meaningful exit: credentials are required by law/industry norm; retraining costs prohibit arbitrage; trapped by structural legal barriers and employers' credential-signaling requirements.
constraint_indexing:constraint_classification(credentialing_decoupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CAREER-TRANSITIONING WORKER (TANGLED ROPE) — Faces high but surmountable costs to exit (credential expenses, time, opportunity cost). Benefits from credential-enabled market access and wage premium upon completion. Asymmetric extraction during transition period; real but costly exit option. Coordination function exists (credentials do signal baseline competence for complex domains) alongside extractive overhead (time, cost, gatekeeping markup).
constraint_indexing:constraint_classification(credentialing_decoupling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CREDENTIALING INSTITUTIONS (ROPE) — Experience the constraint as pure coordination mechanism: issuing credentials solves the information problem of verifying competence at scale. Revenue is a coordination incentive, not extraction. Can arbitrage credential standards across jurisdictions. Net beneficiary but perceives genuine coordination function.
constraint_indexing:constraint_classification(credentialing_decoupling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GATEKEEPING PROFESSIONAL ASSOCIATIONS (TANGLED ROPE) — Coordinate actual competence verification (genuine function) while maintaining licensing monopolies that extract economic rents. Organized agents with constrained exit (would face liability if competence verification collapsed). Dual function: legitimate standards + extractive gatekeeping. Benefits from credential enforcement; costs are diffuse (aggregate labor market inefficiency).
constraint_indexing:constraint_classification(credentialing_decoupling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE CREDENTIALING MOVEMENTS (SCAFFOLD) — Bootcamps, industry certifications, portfolio-based hiring, and skills-based credentials represent temporary coordination alternatives with sunset logic. Lower theater (skills tests replace rituals). As these mature, traditional credential extraction loses force. Organized agents see clear exit path; constraint is transitional.
constraint_indexing:constraint_classification(credentialing_decoupling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: VESTIGIAL CREDENTIAL REQUIREMENTS (PITON) — Many credentialing requirements persist through institutional inertia despite demonstrable decoupling from competence. Licensing for interior designers, cosmetologists, and many service trades shows negligible correlation with actual harm prevention or consumer satisfaction. The ritual persists (theater = 0.68) because organizations inherit credential requirements and lack incentive to eliminate them, not because they detect meaningful competence gaps. Degraded constraint maintained by path dependency.
constraint_indexing:constraint_classification(credentialing_decoupling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing credentialing as an immutable requirement of complex knowledge work. Some verification lag between knowledge and credentials is inevitable; some gatekeeping is necessary to prevent harm. However, the structural data reveals this as a false summit: the decoupling between credentials and actual competence is not a law of nature but a contingent institutional failure. Historical variation in credential-competence correlation, cross-jurisdictional differences, and the success of alternative credentialing systems all demonstrate that the current decoupling is changeable.
constraint_indexing:constraint_classification(credentialing_decoupling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credentialing_decoupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credentialing_decoupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credentialing_decoupling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credentialing_decoupling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credentialing_decoupling, TR),
    TR >= 0.70.

:- end_tests(credentialing_decoupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts from non-credentialed workers through legal barriers, time costs, and opportunity costs of credential acquisition. The extraction is not total because some agents can arbitrage (alternative certification; some employers hire on competence) and because credentials do provide genuine wage premium upon acquisition. The 30-year trend from 0.32 to 0.58 reflects increasing proliferation of credential requirements (credential creep) without corresponding increase in credential-competence correlation. Suppression (0.62): Significant. Barriers include legal prohibition on unlicensed practice, employer credential-signaling requirements, public perception that credentials equal competence, high switching costs (sunk credential investment discourages market entry by non-credentialed), and professional association resistance to alternative credentialing. Theater ratio (0.68): High and rising (0.42 to 0.68 over 30 years). Many credentials require minimal genuine competence assessment: cosmetology licensing requires 1000+ hours of study and examination that correlate weakly with consumer harm prevention; professional association renewals often require only fee payment; credential curricula diverge substantially from actual job tasks in dynamic fields. The rising theater reflects Goodhart drift: once credentials become the selection criterion, institutions optimize for credential attainment rather than competence, decoupling the two.
 *
 * PERSPECTIVAL GAP:
 *   The gap between Rope (credentialing institutions) and Snare (non-credentialed workers) is maximal. Institutions see coordination—their function is to signal competence, reduce information asymmetry, enable labor market matching. This is genuinely valuable. But victims see pure extraction—barriers to entry, forced credential acquisition, wage suppression for non-credentialed despite competence. Both perspectives are structurally correct from their positions. The Tangled Rope classification at the moderate/constrained level (career-transitioning workers) bridges this gap: the constraint has genuine coordination function AND genuine asymmetric extraction operating simultaneously. The Scaffold perspective reveals why the gap exists: the coordination function can be provided more efficiently (lower theater, lower cost) through alternative mechanisms, which means the current extraction is not minimal necessary overhead but contingent institutional markup. The Piton perspective (vestigial requirements) completes the diagnosis: many credential requirements have lost even their coordination function (they don't correlate with competence) yet persist through inertia, making the constraint pure theater masked as necessary gatekeeping.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to the extraction flow. Non-credentialed competent workers are victims (d ≈ 0.95) experiencing trapped exit; their structurally-derived d produces high f(d) ≈ 1.42, yielding high effective extraction chi. Career-transitioning workers are partial victims (d ≈ 0.70) with constrained exit; f(d) ≈ 1.00, moderate chi. Credentialing institutions are beneficiaries (d ≈ 0.10) with arbitrage exit; f(d) ≈ -0.01, near-zero or negative chi from their perspective. Gatekeeping professionals are organized beneficiaries (d ≈ 0.35) with constrained exit (liability if competence verification fails); f(d) ≈ 0.35, moderate positive chi but they experience mixed coordination and extraction. Alternative credentialing movements are organized agents (d ≈ 0.50) with mobile exit (can develop independent verification pathways); f(d) ≈ 0.65, moderate chi but they see sunset logic. The piton perspective (vestigial requirements) derives from institutional actors (d ≈ 0.15, institutional canonical) experiencing low extraction but high theater—the constraint persists through inertia, not extractive function.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint is robustly classified as Tangled Rope at the base level (claimed_type: tangled_rope). The mandatrophy is resolved by showing that credentialing decoupling meets all three Tangled Rope gates: (1) Genuine coordination function exists—credentials do provide information about competence and enable labor market matching; (2) Asymmetric extraction exists—non-credentialed workers bear disproportionate costs; (3) Active enforcement required—legal prohibition on unlicensed practice and professional association gatekeeping maintain the constraint. The false summit risk is in the analytical perspective, which might naturalize credentialing as an immutable requirement of complex knowledge work ('some verification is always necessary'). The structural data refutes this: the 30-year rise in theater_ratio (0.42 → 0.68) and extractiveness (0.32 → 0.58) without corresponding improvement in credential-competence correlation demonstrates that the current form is contingent, not natural law. Alternative credentialing movements show that the same coordination function is achievable with lower extraction—proving the decoupling is institutional choice, not physical law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_competence_correlation,
    'What is the empirical correlation between institutional credentials and actual job performance across domains?',
    'Meta-analysis of hiring outcomes, performance reviews, and task completion rates controlling for credentials vs. alternative signals (portfolio, test scores, apprenticeship hours)',
    'High correlation (r > 0.6): credential decoupling is overstated; classification shifts toward Rope from multiple perspectives. Low correlation (r < 0.3): decoupling is severe; classification shifts toward Snare for powerless agents. The correlation varies by domain — medical licensing shows high correlation; cosmetology licensing shows low correlation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_competence_correlation, empirical, 'Empirical correlation between credentials and job performance').

omega_variable(
    alternative_credentialing_viability,
    'Can portfolio-based hiring, skills-based certification, and apprenticeship models actually replace institutional credentialing for complex domains without loss of public safety or consumer protection?',
    'Comparative outcome analysis: jurisdictions with strong alternative credentialing (EU apprenticeships, tech bootcamp pipelines, portfolio-based hiring) vs. strict institutional credential requirements. Harm metrics: consumer complaints, professional misconduct, adverse outcomes.',
    'If viable: Scaffold perspective confirmed; sunset logic is structural. If not viable: Scaffold is aspirational; institutional credentials remain necessary extraction cost, not mere gatekeeping. Classification shifts toward Rope for credentialing institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credentialing_viability, empirical, 'Whether alternative credentialing systems can replace institutional credentials').

omega_variable(
    theater_vs_genuine_verification,
    'What proportion of institutional credentialing (examination, curriculum, credential renewal) represents genuine competence verification vs. performative theater?',
    'Analysis of credential renewal rates and criteria: if renewals require demonstrated new competence vs. mere fee payment; correlation between credential-renewal compliance and actual practitioner skill degradation; audit of examination content against job task requirements',
    'If theater > 0.75: credentialing is primarily extractive theater (Snare from competent non-credentialed workers). If theater < 0.35: credentialing is primarily functional verification (Rope). Current estimate (0.68) suggests substantial but not dominant theater component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_vs_genuine_verification, empirical, 'Proportion of credential process that is theater vs. genuine verification').

omega_variable(
    institutional_identity_lock,
    'To what degree do credentialing institutions and gatekeeping professionals have their institutional identity fused with the current credential system, preventing recognition of viable alternatives?',
    'Institutional resistance to alternative credentialing models; professional association statements on portfolio-based hiring and skills certification; career trajectory analysis of professionals trained in credential verification vs. alternative assessment methods',
    'If high identity lock: institutions cannot recognize decoupling because their self-concept depends on credential monopoly. The constraint persists through cognitive capture, not structural necessity. If low identity lock: institutions genuinely believe credentials are necessary; decoupling is less severe than measures suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_identity_lock, conceptual, 'Institutional identity fusion with current credentialing system').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credentialing_decoupling, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cred_tr_t0, credentialing_decoupling, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cred_tr_t10, credentialing_decoupling, theater_ratio, 10, 0.55).
narrative_ontology:measurement(cred_tr_t20, credentialing_decoupling, theater_ratio, 20, 0.68).
narrative_ontology:measurement(cred_tr_t30, credentialing_decoupling, theater_ratio, 30, 0.74).

% Extraction over time
narrative_ontology:measurement(cred_be_t0, credentialing_decoupling, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cred_be_t10, credentialing_decoupling, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(cred_be_t20, credentialing_decoupling, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cred_be_t30, credentialing_decoupling, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credentialing_decoupling, information_standard).
narrative_ontology:affects_constraint(credentialing_decoupling, occupational_licensing_rents).
narrative_ontology:affects_constraint(credentialing_decoupling, professional_gatekeeping_monopolies).

% DUAL FORMULATION NOTE:
% Credentialing decoupling decomposes into two structurally distinct claims: (1) the empirical decoupling between credential possession and job competence (measurement problem; this story), and (2) the economic extraction enabled by credential-based gatekeeping monopolies (rent-seeking problem; related story occupational_licensing_rents). Both share the same institutions but have different ε values and different resolution mechanisms. This story focuses on the information/coordination failure; the sibling story focuses on the economic monopoly failure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(credentialing_decoupling, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
