% ============================================================================
% CONSTRAINT STORY: institutional_credentialing_monopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_credentialing_monopoly, []).

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
 *   constraint_id: institutional_credentialing_monopoly
 *   human_readable: Institutional Credentialing Monopoly
 *   domain: institutional/labor/education
 *
 * SUMMARY:
 *   Institutional credentialing monopoly constrains labor market access by
 *   restricting entry pathways to those certified by educational institutions
 *   and professional licensing boards. This constraint exhibits the classic
 *   dual structure of tangled rope: genuine coordination (standardized
 *   training ensures professional quality, liability protection through
 *   certified competence) layered with asymmetric extraction (artificial
 *   scarcity of credentials enables tuition extraction, incumbent wage
 *   protection, and regulatory rent-seeking). The constraint operates through
 *   regulatory enforcement (licensing laws, employer hiring norms, social
 *   prestige attached to credentials) and escalates extractive intensity over
 *   time as alternative pathways emerge but struggle for recognition. Theater
 *   ratio (0.65) reflects that credentialing processes increasingly focus on
 *   revenue collection (degree inflation, credential stacking, continuing
 *   education requirements) rather than on actual competence verification —
 *   employers increasingly supplement credential-based hiring with direct
 *   skills assessments and portfolio review, indicating declining functional
 *   necessity of the institutional monopoly.
 *
 * KEY AGENTS:
 *   - Credential Seekers: Primary victims (powerless/trapped) — structurally dependent on institutional pathways; no exit options available
 *   - Skilled Practitioners Without Credentials: Secondary victims (moderate/constrained) — demonstrate competence outside institutional pathways but face high labor market barriers
 *   - Credentialing Institutions: Primary beneficiaries (institutional/arbitrage) — control supply of recognized credentials; capture tuition extraction and occupational prestige
 *   - Incumbent Professionals: Secondary beneficiaries (powerful/constrained) — benefit from artificial labor scarcity and quality standards; constrained by maintenance costs and credential stacking
 *   - Alternative Credentialing Coalition: Organized agents (organized/mobile) — bootcamp platforms, micro-credential providers, skill-based hiring systems building parallel pathways
 *   - Regulatory Licensing Framework: Institutional actor (institutional/arbitrage) — maintains monopoly through state enforcement; increasingly performative as actual coordination work shifts to employers
 *   - Labor Market Mobility: Victim (powerless/trapped) — structural inefficiency from credential-requirement filtering that excludes capable workers and locks workers into credential paths even when demonstrable competence exists
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_credentialing_monopoly, 0.58).
domain_priors:suppression_score(institutional_credentialing_monopoly, 0.68).
domain_priors:theater_ratio(institutional_credentialing_monopoly, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_credentialing_monopoly, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_credentialing_monopoly, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(institutional_credentialing_monopoly, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_credentialing_monopoly, tangled_rope).
narrative_ontology:human_readable(institutional_credentialing_monopoly, "Institutional Credentialing Monopoly").
narrative_ontology:topic_domain(institutional_credentialing_monopoly, "institutional/labor/education").

domain_priors:requires_active_enforcement(institutional_credentialing_monopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_credentialing_monopoly, credentialing_institutions).
narrative_ontology:constraint_beneficiary(institutional_credentialing_monopoly, incumbent_professionals).
narrative_ontology:constraint_victim(institutional_credentialing_monopoly, credential_seekers).
narrative_ontology:constraint_victim(institutional_credentialing_monopoly, alternative_pathway_workers).
narrative_ontology:constraint_victim(institutional_credentialing_monopoly, labor_market_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CREDENTIAL SEEKER (SNARE) — Faces irreversible structural dependency on institutional credentialing. Without recognized credentials, access to professional labor markets is effectively closed. Cannot exit because employers enforce credential requirements; cannot pursue alternatives because regulatory frameworks and social norms eliminate their value proposition. Trapped in a biographical-horizon extraction mechanism.
constraint_indexing:constraint_classification(institutional_credentialing_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SKILLED PRACTITIONER WITHOUT CREDENTIALS (TANGLED ROPE) — Demonstrates genuine competence outside institutional pathways but faces high barriers to labor market entry. The constraint coordinates professional standards (genuine function) while extracting economic rent through artificial scarcity. This practitioner can exit toward informal economy or geographic arbitrage, but at significant cost. Both coordination (maintains quality standards) and extraction (restricts labor supply) are structurally present.
constraint_indexing:constraint_classification(institutional_credentialing_monopoly, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIALING INSTITUTION (ROPE) — Experiences the constraint primarily as coordination mechanism: standardizing training, maintaining professional quality, managing liability through standardized curricula. Can exit the system through market competition or regulatory change; currently maintains monopoly through regulatory enforcement. Net beneficiary — receives tuition extraction flows, occupational prestige, institutional endowment growth.
constraint_indexing:constraint_classification(institutional_credentialing_monopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT PROFESSIONAL (TANGLED ROPE) — Coordinated through credentialing standards (genuine benefit: ensures peer quality, maintains professional reputation); extracted from through artificial labor scarcity (genuine cost: restricted competition limits wage negotiation, forces credential maintenance costs). This agent benefits from the coordination and the extraction simultaneously — the constraint gives both quality assurance and competitive protection.
constraint_indexing:constraint_classification(institutional_credentialing_monopoly, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE CREDENTIALING COALITION (SCAFFOLD) — Organized actors (bootcamps, online credential platforms, micro-credentials, employer-direct training) are building alternative pathways with lower extraction. These alternatives have genuine sunset logic: as they mature and demonstrate equal or superior outcomes (GitHub portfolios replacing transcripts, demonstrated competence replacing degree-holding), the traditional monopoly's extraction mechanism loses force. Theater ratio declining as outcomes-based credentialing matures.
constraint_indexing:constraint_classification(institutional_credentialing_monopoly, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY LICENSING FRAMEWORK (PITON) — Institutional mechanisms (state licensing boards, professional association gatekeeping, credential recognition laws) persist largely through inertia. Original function was to prevent fraud and incompetence (legitimate coordination). Current function is substantially performative: licensing exams correlate weakly with actual competence; renewal requirements function as revenue extraction rather than quality maintenance. The theater has increased as the real coordination work has been displaced to employer hiring and peer networks.
constraint_indexing:constraint_classification(institutional_credentialing_monopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, information asymmetry between employer and job seeker is an immutable feature of labor markets: employers cannot directly observe employee competence, so some signal (credential) is necessary. This perspective naturalizes the monopoly as an inevitable solution to Spence's signaling problem. However, structural data contradicts the mountain classification — the constraint contains contingent institutional choices (which credentials are recognized, who grants them, what enforces compliance) that are not immutable laws. False summit detection applies.
constraint_indexing:constraint_classification(institutional_credentialing_monopoly, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_credentialing_monopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_credentialing_monopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_credentialing_monopoly, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_credentialing_monopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_credentialing_monopoly, TR),
    TR >= 0.70.

:- end_tests(institutional_credentialing_monopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The credentialing monopoly extracts economic value through tuition collection (credential seekers must pay institutions), artificial labor scarcity (restricted supply of recognized credentials drives up incumbent wages and creates unemployment for credential-less workers), and rent-seeking (institutions maintain barriers to prevent alternative credentials from achieving parity). However, extraction is not maximal because genuine coordination function remains: institutional credentialing does maintain professional standards, reduce employer screening costs, and manage occupational liability. The 10-year trajectory (0.38 → 0.58) reflects increasing extraction relative to coordination as alternative credentials emerge and demonstrate effectiveness, making the monopoly increasingly a pure extraction mechanism rather than necessary coordination. Suppression (0.68): High. Multiple barriers prevent alternatives from functioning: regulatory requirements that recognize only institutional credentials, employer hiring norms that default to degree-holding, professional association gatekeeping, and social prestige attached to institutional affiliation. These barriers are not absolute (skilled workers can sometimes enter through portfolio-based hiring or geographic arbitrage) but substantial. Theater ratio (0.65): Moderate-high. Credentialing processes have become increasingly performative: licensing exams correlate weakly with actual job performance in many fields; continuing education requirements function as revenue extraction rather than competence maintenance; degree inflation has forced credential stacking (bachelor's → master's → additional certifications) despite no documented increase in job competence requirements. This indicates declining functional necessity of the institutional monopoly.
 *
 * PERSPECTIVAL GAP:
 *   The tangled rope classification emerges from the simultaneous presence of genuine coordination (professional standards, quality assurance, liability management) and asymmetric extraction (tuition, labor scarcity, regulatory gatekeeping). Every perspective except the analytical mountain-view acknowledges this duality. The key diagnostic signal is the perspectival gap between institutional and powerless perspectives: the same structural mechanism (credential requirement) is experienced as beneficial coordination by the institution (Rope) and as coercive extraction by the seeker (Snare). This gap reveals that the constraint's extractive force depends on power asymmetry and exit closure, not on the coordination function itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to extraction flows. Credential seekers are full targets (d ≈ 0.95): they must pay tuition, incur opportunity costs, and face restricted labor access without credentials. Their trapped exit status amplifies d toward maximum. Credentialing institutions are near-beneficiaries (d ≈ 0.05): they receive tuition flows, control credential supply, and maintain regulatory barriers. Their arbitrage exit (ability to maintain the monopoly through regulatory enforcement) reduces d toward minimum. Incumbent professionals have moderate d (≈ 0.50): they benefit from labor scarcity but bear credential maintenance costs and face wage restrictions from labor market distortion. Alternative credential advocates have moderate-low d (≈ 0.40): they face barriers to recognition but have mobile exit options (building new markets, geographic expansion, demonstrating parity). The skilled practitioners without credentials have high d (≈ 0.80): they bear exclusion costs but have some mobile exit options through portfolio-based hiring or geographic arbitrage.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH PERSPECTIVAL DIFFERENTIATION: The mandatrophy is resolved by showing that credentialing monopoly is genuinely tangled — it coordinates professional standards while extracting economic rent, and these two functions are structurally inseparable within the current institutional arrangement. The false summit (analytical mountain view) attempts to naturalize the monopoly as an inevitable solution to information asymmetry. However, the existence of functioning alternative credentials (bootcamp completion correlates with job performance; GitHub portfolios predict technical competence; employer-direct training eliminates information asymmetry entirely) demonstrates that the coordinate-by-monopoly framing is not immutable. The extraction can be decoupled from coordination through alternative institutional arrangements: certification bodies that don't control training, market-based credential recognition, employer-led standard-setting, or outcome-based verification. The mandatrophy is resolved by recognizing that the tangled rope structure is contingent, not necessary, and can be reorganized into lower-extraction alternatives (Scaffold represents the decomposed future state where coordination persists but extraction is reduced through market competition and regulatory pluralism).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_signaling_necessity,
    'Is institutional credentialing monopoly a necessary solution to information asymmetry in labor markets, or a contingent institutional arrangement that could be replaced by alternative signals?',
    'Empirical outcomes tracking: compare labor market efficiency (match quality, wage dispersion, mobility) across jurisdictions with varying credential strictness; measure competence-signal correlation for traditional vs alternative credentials',
    'If necessary: classify as mountain-proximate (information asymmetry floor). If contingent: classify as pure extraction mechanism exploiting asymmetry — snare from many more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_signaling_necessity, empirical, 'Whether credentialing monopoly is necessary or contingent').

omega_variable(
    alternative_credentialing_convergence,
    'Will alternative credentials (bootcamp completion, GitHub portfolio, employer certification, skill-based assessment) achieve parity with institutional degrees in labor market recognition within the next 10-15 years?',
    'Longitudinal hiring data: proportion of hires by credential type; wage premium correlation; employer-stated credential preference shifts; occupational licensing board recognition of alternative pathways',
    'If yes: scaffold sunset is real — monopoly is being displaced by market and technological change. If no: monopoly persists despite alternatives; extraction mechanism is structural rather than temporary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credentialing_convergence, empirical, 'Timeline for alternative credential parity').

omega_variable(
    regulatory_capture_extent,
    'To what degree are credentialing institutions actively lobbying to maintain regulatory barriers vs passively benefiting from incumbent advantage?',
    'Policy advocacy tracking; campaign finance analysis for credentialing institution donations to licensing board elections and legislative campaigns; FOIA disclosure of regulatory agency communications',
    'If active capture: extraction is intentionally maintained by institutions with power to prevent alternatives. If passive benefit: extraction persists through path dependence and individual institutional incentives without coordinated lobbying. Different mandatrophy implications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Extent of active regulatory capture by credentialing institutions').

omega_variable(
    competence_quality_correlation,
    'How strongly do institutional credentials actually correlate with on-the-job competence and performance across different occupational fields?',
    'Meta-analysis of credential-performance correlations; employer exit interview data on credential predictiveness; occupational fields where correlation is weak (predictive of licensing exams but not job performance)',
    'If strong correlation: credentialing maintains real coordination function despite extraction. If weak correlation: credentialing is primarily extraction mechanism with performative quality maintenance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_quality_correlation, empirical, 'Credential-competence correlation strength').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_credentialing_monopoly, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(icm_tr_t0, institutional_credentialing_monopoly, theater_ratio, 0, 0.52).
narrative_ontology:measurement(icm_tr_t5, institutional_credentialing_monopoly, theater_ratio, 5, 0.58).
narrative_ontology:measurement(icm_tr_t10, institutional_credentialing_monopoly, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(icm_be_t0, institutional_credentialing_monopoly, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(icm_be_t5, institutional_credentialing_monopoly, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(icm_be_t10, institutional_credentialing_monopoly, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_credentialing_monopoly, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(institutional_credentialing_monopoly, 0.12).
narrative_ontology:affects_constraint(institutional_credentialing_monopoly, occupational_licensing_rent_seeking).
narrative_ontology:affects_constraint(institutional_credentialing_monopoly, credential_inflation_cycle).
narrative_ontology:affects_constraint(institutional_credentialing_monopoly, employer_hiring_risk_aversion).

% DUAL FORMULATION NOTE:
% Institutional credentialing monopoly decomposes into three linked constraints with distinct ε values: (1) credential_supply_restriction (ε=0.72, Snare) — pure extraction through artificial scarcity; (2) quality_standard_coordination (ε=0.15, Rope) — genuine coordination of professional standards; (3) regulatory_enforcement_theater (ε=0.68, Piton) — performative licensing increasingly sustained by inertia. The tangled rope classification (ε=0.58) represents the integrated constraint where coordination and extraction are structurally entangled. Each sub-story has different beneficiaries and victims. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_credentialing_monopoly, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
