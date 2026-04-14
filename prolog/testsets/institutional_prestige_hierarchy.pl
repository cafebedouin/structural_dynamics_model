% ============================================================================
% CONSTRAINT STORY: institutional_prestige_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_prestige_hierarchy, []).

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
 *   constraint_id: institutional_prestige_hierarchy
 *   human_readable: Institutional Prestige Hierarchy as Extraction Mechanism
 *   domain: organizational/social/institutional
 *
 * SUMMARY:
 *   The institutional prestige hierarchy is a structural mechanism that
 *   coordinates labor market quality signaling while simultaneously
 *   extracting rents from non-credentialed workers and resource-constrained
 *   institutions. The hierarchy appears inevitable from a civilizational
 *   perspective (information asymmetry requires some signaling mechanism) but
 *   is revealed as contingent institutional arrangement when examined from
 *   the perspectives of trapped agents. The extractiveness has increased over
 *   the measurement interval (0.35 → 0.58) as credential requirements have
 *   become more stringent and as the wage penalties associated with non-elite
 *   credentials have widened. The theater ratio has also increased (0.52 →
 *   0.68) as accreditation and ranking systems have become more elaborate and
 *   performative. The constraint exhibits all six classification types from
 *   different perspectives, making it a diagnostic exemplar for how indexical
 *   classification reveals extractive mechanisms hidden within apparently
 *   neutral institutional arrangements.
 *
 * KEY AGENTS:
 *   - High-Prestige Institutions: Primary beneficiary (institutional/arbitrage) — capture elite student pools, research funding concentration, and labor market wage premiums for graduates. Complete mobility to arbitrage prestige into resources.
 *   - Elite Credential Holders: Secondary beneficiary (powerful/arbitrage) — experience prestige as pure coordination benefit; their credentials carry wage premiums and status. Complete mobility within prestige system.
 *   - Non-Credentialed Workers: Primary victim (powerless/trapped) — excluded from high-wage sectors; face lifetime wage penalties (15-40% earnings gap); no legitimate exit from prestige hierarchy. Trapped by credential requirements with no alternative signaling mechanisms.
 *   - Low-Prestige Institution Graduates: Secondary victim (moderate/constrained) — bear wage penalties and hiring discrimination; experience prestige as mixed coordination/extraction. Constrained exit: can change fields or re-credential at high cost.
 *   - Resource-Constrained Institutions: Tertiary victim (organized/constrained) — caught in prestige-chasing treadmill; cannot raise prestige without resources, cannot raise resources without prestige. Constrained by competitive pressure and funding structures.
 *   - Accreditation and Ranking System: Institutional actor (institutional/arbitrage) — maintains performative apparatus; benefits from institutional inertia; has mobility to modify standards but interests aligned with prestige maintenance.
 *   - Alternative Credential Movement: Organized agents (organized/constrained) — building exit pathways; have agency and can see sunset timeline. Constrained by incumbent prestige system's institutional power and employer coordination on credential standards.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_prestige_hierarchy, 0.58).
domain_priors:suppression_score(institutional_prestige_hierarchy, 0.62).
domain_priors:theater_ratio(institutional_prestige_hierarchy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_prestige_hierarchy, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_prestige_hierarchy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(institutional_prestige_hierarchy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_prestige_hierarchy, tangled_rope).
narrative_ontology:human_readable(institutional_prestige_hierarchy, "Institutional Prestige Hierarchy as Extraction Mechanism").
narrative_ontology:topic_domain(institutional_prestige_hierarchy, "organizational/social/institutional").

domain_priors:requires_active_enforcement(institutional_prestige_hierarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_prestige_hierarchy, high_prestige_institutions).
narrative_ontology:constraint_beneficiary(institutional_prestige_hierarchy, elite_credential_holders).
narrative_ontology:constraint_victim(institutional_prestige_hierarchy, low_prestige_institution_graduates).
narrative_ontology:constraint_victim(institutional_prestige_hierarchy, non_credentialed_workers).
narrative_ontology:constraint_victim(institutional_prestige_hierarchy, resource_constrained_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-CREDENTIALED WORKER (SNARE) — Faces structural barriers to labor market mobility; excluded from high-wage sectors requiring institutional credentials. No legitimate exit from the prestige hierarchy short of acquiring expensive credentials. Bears full extraction through wage penalty and opportunity costs with minimal alternative pathways.
constraint_indexing:constraint_classification(institutional_prestige_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-PRESTIGE INSTITUTION GRADUATE (TANGLED ROPE) — Experiences both coordination and extraction. The prestige system coordinates credentialing, risk assessment, and labor market matching (genuine coordination benefit). But the graduate also bears asymmetric extraction through wage penalties (15-40% lifetime earnings gap) and hiring discrimination. Constrained exit: can change fields or continue education at high cost, but cannot escape the credential mark.
constraint_indexing:constraint_classification(institutional_prestige_hierarchy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HIGH-PRESTIGE INSTITUTION (ROPE) — Experiences the hierarchy as coordination: institutional reputation enables quality signaling, preferential access to research funding, elite student recruitment, and faculty retention. Prestige is a coordination mechanism that solves the information problem ('How do we know this graduate is competent?'). Net beneficiary with complete mobility — can arbitrage prestige into funding, partnerships, and market share.
constraint_indexing:constraint_classification(institutional_prestige_hierarchy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RESOURCE-CONSTRAINED INSTITUTION (TANGLED ROPE) — Mid-tier institutions experience both coordination and extraction. The prestige hierarchy coordinates quality expectations and hiring standards (coordination benefit). But these institutions are also trapped in an extraction mechanism: they cannot raise prestige without resources, cannot raise resources without prestige, and competitive pressure forces them to pursue prestige-chasing signaling (low theater-ratio activities) rather than core educational function. Constrained exit: institutional mortality is slow and rare, but strategic agility is severely limited.
constraint_indexing:constraint_classification(institutional_prestige_hierarchy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ACCREDITATION AND RANKING SYSTEM (PITON) — The apparatus that maintains the prestige hierarchy is largely performative. Accreditation reviews assess theater: documentation, governance structures, strategic plans, and reputation metrics. These do not measure learning outcomes or institutional function reliably. Ranking systems (U.S. News, QS, Times Higher Education) are explicitly theater-driven, using reputation surveys and research output as proxies for educational quality. The system persists through institutional inertia — everyone knows it's nonsensical, but the alternative (no shared ranking system) appears worse. Theater ratio is high (0.68) because the accreditation and ranking apparatus is substantially about performing prestige rather than measuring it.
constraint_indexing:constraint_classification(institutional_prestige_hierarchy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPETENCY-BASED AND ALTERNATIVE CREDENTIAL MOVEMENT (SCAFFOLD) — Organized actors (industry skill certifications, online learning platforms, competency assessments, employer-led credentialing) are building parallel verification pathways that bypass institutional prestige. These alternatives are low-theater (direct skills assessment) and are gradually reducing the extraction capacity of the prestige hierarchy in technical fields. The scaffold has a sunset clause: as alternative credentials gain employer recognition and network value, the institutional prestige hierarchy loses its monopoly on signaling. Estimated sunset in technical fields: 15-25 years.
constraint_indexing:constraint_classification(institutional_prestige_hierarchy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some form of quality signaling mechanism is inherent to human institutions: buyers cannot assess product quality directly, so institutions emerge to solve the information problem. Prestige hierarchies appear inevitable as a solution to information asymmetry. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit, revealing that while *some* quality signaling is necessary, the *specific prestige hierarchy* (ranked institutions with persistent inequality, credential scarcity, lifetime wage penalties) is a contingent institutional arrangement, not a natural law.
constraint_indexing:constraint_classification(institutional_prestige_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_prestige_hierarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_prestige_hierarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_prestige_hierarchy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_prestige_hierarchy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_prestige_hierarchy, TR),
    TR >= 0.70.

:- end_tests(institutional_prestige_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The prestige hierarchy extracts from non-credentialed workers through wage penalties, from low-prestige graduates through lifetime earnings gaps and hiring discrimination, and from resource-constrained institutions through competitive funding pressure. The extraction is not total (some low-prestige graduates succeed, some institutions thrive), but it is substantial and structural. The measurement trajectory (0.35 → 0.58) reflects increasing credential requirements and widening wage gaps, suggesting the extraction mechanism has intensified. Suppression (0.62): High. Significant barriers to exit include: credential scarcity enforced by accreditation standards, employer coordination around prestige signals, high cost of acquiring elite credentials, social stigma attached to non-elite credentials, and lack of alternative signaling mechanisms with equivalent labor market value. However, suppression is not total — alternative pathways exist (bootcamps, certifications, employer-led training) and are gradually reducing barriers. Theater ratio (0.68): High. Accreditation systems assess documentation and governance structures rather than learning outcomes. Ranking systems rely on reputation surveys and research output proxies that correlate weakly with educational quality. Much of the prestige apparatus is about performing legitimacy rather than measuring it. The measurement trajectory (0.52 → 0.68) reflects increasing institutionalization of prestige theater as alternative verification systems emerge.
 *
 * PERSPECTIVAL GAP:
 *   Maximum. The beneficiary's rope (prestige solves information problem efficiently) conflicts with the victim's snare (prestige excludes from opportunity and extracts lifetime earnings). The moderate agent's tangled rope (prestige both enables and constrains) bridges these. The analytical observer's mountain (signaling is inherent) conflicts with the scaffold's sunset (alternatives are viable). The piton perspective (accreditation theater is nonsensical) reveals the machinery of prestige maintenance that the high-prestige institution's rope perspective takes for granted.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from beneficiary/victim declarations and exit options. High-prestige institutions as beneficiaries with arbitrage exit options derive low d (prestige flows toward them, negative or zero effective extraction). Non-credentialed workers as victims with trapped exit derive maximum d (prestige extractsfully from them). Low-prestige graduates and resource-constrained institutions occupy intermediate positions: victims with constrained exit, producing moderate-high d. The alternative credential movement as organized agents with constrained exit produce moderate d — they have agency but cannot fully escape incumbent system's lock-in. No overrides are necessary; the derived directionality values accurately reflect structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by demonstrating how the same institutional arrangement (prestige hierarchy) provides genuine coordination benefits while simultaneously extracting rents through artificial scarcity maintenance. The mandatrophy is resolved not by claiming one type is 'correct,' but by recognizing that the coordinate-and-extract function are structurally inseparable in prestige systems. The system coordinates labor market quality signaling (Rope function) and therefore suppresses alternative signaling mechanisms (Snare function). The analytical observer's temptation to call this a natural law (Mountain) is revealed as false summit: some signaling is necessary, but this particular hierarchy's severity and persistence derives from rent-seeking and path dependence, not from information asymmetry alone. The scaffold perspective's sunset logic is real — competency-based alternatives are materially reducing prestige hierarchy's extraction capacity — but the prestige system will likely persist in modified form because the coordination benefit is genuine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    signaling_necessity_threshold,
    'What level of quality signaling is genuinely necessary for functional labor markets versus what is extractive scarcity maintenance?',
    'Comparison of labor market outcomes across countries with different credential hierarchies; analysis of whether removing prestige tiers increases or decreases unemployment and skill mismatch',
    'If threshold is low: most prestige extraction is above the necessary coordination cost and should be classified as snare/extraction. If threshold is high: prestige hierarchy is serving a genuine coordination function and should be classified as rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signaling_necessity_threshold, empirical, 'Threshold distinguishing necessary signaling from extractive scarcity').

omega_variable(
    credential_substitution_feasibility,
    'Can competency-based credentials and industry certifications functionally replace institutional prestige hierarchies for labor market matching without loss of coordination quality?',
    'Longitudinal tracking of hiring outcomes for competency-credentialed vs institutionally-credentialed cohorts; analysis of whether employer skill requirements drive shift toward alternative credentials or prestige remains primary signal',
    'If feasible: scaffold sunset is real and extraction period is limited. If not feasible: alternative credentials become supplementary and prestige hierarchy remains dominant — classification stays tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_substitution_feasibility, empirical, 'Whether competency credentials can replace institutional prestige').

omega_variable(
    prestige_persistence_mechanism,
    'Is prestige hierarchy maintained by genuine coordination lock-in (employers must use prestige signals because others do) or by rent-seeking and status quo bias?',
    'Counterfactual analysis: would decouple institutional ranking systems in a subset of hiring markets produce coordination failure or improved matching? Historical analysis of moments when prestige hierarchies weakened or strengthened.',
    'If lock-in: prestige is structural coordination problem and suppression is justified. If rent-seeking: suppression is enforced artificial scarcity and extraction classification is dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prestige_persistence_mechanism, conceptual, 'Whether prestige is coordination lock-in or rent-seeking').

omega_variable(
    intergenerational_mobility_ceiling,
    'Does the prestige hierarchy reduce or maintain intergenerational social mobility compared to a no-hierarchy counterfactual?',
    'Comparative analysis of social mobility rates across prestige-driven vs non-prestige-driven education systems; analysis of whether credential scarcity reduces or increases relative inequality',
    'If reduces mobility: prestige hierarchy is pure extraction mechanism regardless of signaling function. If maintains/increases mobility: hierarchy serves real coordination benefit despite extractive elements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_mobility_ceiling, empirical, 'Prestige hierarchy impact on intergenerational mobility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_prestige_hierarchy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prestige_tr_t0, institutional_prestige_hierarchy, theater_ratio, 0, 0.52).
narrative_ontology:measurement(prestige_tr_t20, institutional_prestige_hierarchy, theater_ratio, 20, 0.62).
narrative_ontology:measurement(prestige_tr_t40, institutional_prestige_hierarchy, theater_ratio, 40, 0.68).
narrative_ontology:measurement(prestige_tr_t10, institutional_prestige_hierarchy, theater_ratio, 10, 0.57).

% Extraction over time
narrative_ontology:measurement(prestige_be_t0, institutional_prestige_hierarchy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prestige_be_t20, institutional_prestige_hierarchy, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(prestige_be_t40, institutional_prestige_hierarchy, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(prestige_be_t10, institutional_prestige_hierarchy, base_extractiveness, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_prestige_hierarchy, information_standard).
narrative_ontology:affects_constraint(institutional_prestige_hierarchy, wage_inequality).
narrative_ontology:affects_constraint(institutional_prestige_hierarchy, educational_access).
narrative_ontology:affects_constraint(institutional_prestige_hierarchy, credential_inflation).
narrative_ontology:affects_constraint(institutional_prestige_hierarchy, intergenerational_mobility).

% DUAL FORMULATION NOTE:
% The prestige hierarchy decomposes into multiple structurally distinct constraints: credential_inflation (ε≈0.72, pure extraction), educational_access (ε≈0.55, tangled rope with sunset), wage_inequality (ε≈0.60, snare for non-credentialed), and intergenerational_mobility (ε≈0.58, tangled rope). Each has its own measurement trajectory and perspectives. The institutional_prestige_hierarchy is the coordinating constraint that links these downstream effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
