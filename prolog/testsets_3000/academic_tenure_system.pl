% ============================================================================
% CONSTRAINT STORY: academic_tenure_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_tenure_system, []).

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
 *   constraint_id: academic_tenure_system
 *   human_readable: Academic Tenure System
 *   domain: economic/social
 *
 * SUMMARY:
 *   The academic tenure system is designed to protect academic freedom by
 *   guaranteeing permanent employment for scholars who meet disciplinary
 *   standards. However, it simultaneously extracts extraordinary effort from
 *   junior faculty during the probationary period (typically 6-7 years)
 *   through competition, uncertainty, and the threat of expulsion. The system
 *   is a canonical tangled rope: it provides genuine coordination (solving
 *   the commitment problem of how to protect inquiry from external pressure)
 *   while imposing asymmetric extraction (concentrating security among
 *   tenured insiders while externalizing employment risk to contingent
 *   workers). The constraint's evolution from 1970 to 2000 shows increasing
 *   extractiveness and theater as contingent labor expanded to buffer
 *   tenure-track positions, and as publication expectations escalated beyond
 *   the original meritocratic intent. The theater ratio increased as
 *   institutions maintained tenure-rank prestige (ritual) while actual
 *   protection eroded through alternative hiring modes.
 *
 * KEY AGENTS:
 *   - Junior faculty on tenure track: Primary victims (powerless/trapped) — face 6-7 year probation with minimal job security, required to publish, teach, and serve at high levels to achieve permanent employment
 *   - Tenured faculty: Primary beneficiaries (institutional/arbitrage) — secure permanent employment with protection from political/commercial pressure; captured early productivity surge from junior cohort
 *   - Research institutions: Secondary beneficiary (institutional/arbitrage) — gain prestige from tenure-track status, benefit from junior faculty hyperproductivity, use tenure to attract senior researchers
 *   - Adjuncts and contingent workers: Secondary victims (powerless/trapped) — comprise growing proportion of faculty; bear employment precarity that subsidizes tenure-track positions
 *   - Faculty unions and collective organizations: Organized mediators (organized/constrained) — advocate for tenure extensions and contingent worker protections; experience system as hybrid extraction/coordination
 *   - Star researchers with external funding: Mobile beneficiaries (powerful/mobile) — use portable reputation and funding to extract institutional resources while benefiting from tenure's protective framework
 *   - University administration: Institutional maintainers (institutional/constrained) — preserve tenure ritual while managing employment through contingent hiring; experience system as degraded (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_tenure_system, 0.58).
domain_priors:suppression_score(academic_tenure_system, 0.62).
domain_priors:theater_ratio(academic_tenure_system, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_tenure_system, extractiveness, 0.58).
narrative_ontology:constraint_metric(academic_tenure_system, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(academic_tenure_system, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_tenure_system, tangled_rope).
narrative_ontology:human_readable(academic_tenure_system, "Academic Tenure System").
narrative_ontology:topic_domain(academic_tenure_system, "economic/social").

domain_priors:requires_active_enforcement(academic_tenure_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_tenure_system, tenured_faculty).
narrative_ontology:constraint_beneficiary(academic_tenure_system, research_institutions).
narrative_ontology:constraint_victim(academic_tenure_system, junior_faculty).
narrative_ontology:constraint_victim(academic_tenure_system, adjuncts).
narrative_ontology:constraint_victim(academic_tenure_system, doctoral_students).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTINGENT JUNIOR FACULTY (SNARE) — Trapped in a 6-7 year probationary period with minimal income security, required to produce research, teaching, and service at suprahuman levels to secure permanent employment. Alternative exit routes (industry, administration) exist nominally but carry career stigma and substantial switching costs. The constraint extracts maximum effort while suppressing alternatives through status hierarchy and sunk investment in academic identity formation. From this position, the tenure system is pure extraction masked as opportunity.
constraint_indexing:constraint_classification(academic_tenure_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TENURED FACULTY & RESEARCH INSTITUTIONS (ROPE) — Experience the tenure system as protective coordination: job security enables long-term research planning, defense of unpopular inquiry, and institutional autonomy. Tenure serves a genuine coordination function — it solves the commitment problem of how to protect scholars from political pressure or funder whims. Tenured faculty with arbitrage exit options see this as near-pure coordination with minimal extraction directed at themselves. Institutions benefit from the prestige of tenure-track faculty and the productivity of competition-induced effort in the probationary phase.
constraint_indexing:constraint_classification(academic_tenure_system, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ORGANIZED FACULTY / UNION (TANGLED ROPE) — Collectively organized faculty see tenure as a hybrid: it provides coordination (job protection), but the system extracts and concentrates benefits among tenured insiders while externalizing costs to contingent workers. From the organized perspective, the constraint is both protective and extractive. Unions push for tenure extensions, job security for adjuncts, and cost-sharing of extraction. Their constrained exit (cannot exit collective sector entirely) and organized power produce a complex relationship with the system.
constraint_indexing:constraint_classification(academic_tenure_system, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STAR RESEARCHER (TANGLED ROPE) — High-performing academics with portable reputation and external funding can exit to industry labs, international positions, or industry roles with minimal cost. They experience tenure as coordination (protecting their freedom to pursue risky research) while using threat of exit to extract institutional resources (lab space, salary supplements, course releases). Their mobility allows them to arbitrage between the tenure system's protections and external offers. They benefit from coordination and exercise extraction power.
constraint_indexing:constraint_classification(academic_tenure_system, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC ADMINISTRATION (PITON) — Universities maintain tenure as formal policy (theater_ratio 0.65) while actually managing employment through increasing proportions of contingent labor, adjunct positions, and fixed-term appointments. The tenure ritual persists through legal obligation and prestige maintenance, but its functional protection has atrophied — most academics are now outside tenure-track, and tenure-track positions themselves require increasing hyperproductivity to secure. Administration sees the system as degraded but institutionally necessary to maintain research rankings and faculty recruitment narratives.
constraint_indexing:constraint_classification(academic_tenure_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ALTERNATIVE EMPLOYMENT ADVOCATES (SCAFFOLD) — Reform movements advocating for fixed-term contracts with genuine security, universal benefits, and equitable evaluation see the current tenure system as a temporary scaffolding that will be replaced by more equitable permanent arrangements. They recognize tenure's protective function but argue the extraction costs (dual-track labor markets, hyperproductivity expectations) are unsustainable and unethical. This perspective sees the system as having a sunset clause — as institutional alternatives mature (industry research labs, think tanks, renewable-term security with benefits), the tenure monopoly on academic freedom protection will erode.
constraint_indexing:constraint_classification(academic_tenure_system, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — A civilizational-scale analysis might frame tenure as an immutable response to the knowledge production problem: expertise production inherently requires long-term investment, freedom from short-term pressures, and protection from external interference. From this view, tenure-like protections are natural law — any system of knowledge production must solve the problem of how to protect inquiry from political/commercial pressure. However, the structural data contradicts this: tenure is a contingent institutional arrangement with clear beneficiaries and victims, not a law of nature. The mountain classification is a false summit — naturalizing what is actually a hybrid extraction-coordination mechanism.
constraint_indexing:constraint_classification(academic_tenure_system, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_tenure_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_tenure_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_tenure_system, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_tenure_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(academic_tenure_system, TR),
    TR >= 0.70.

:- end_tests(academic_tenure_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the magnitude of effort extracted during probation and the structural asymmetry between job security for insiders and precarity for outsiders. The value reflects that tenure's protective coordination function is real but comes at substantial cost to contingent workers and junior faculty. The trajectory shows increasing extractiveness as the proportion of contingent labor expanded (from ~25% of faculty in 1970 to ~70% by 2000) and publication expectations intensified. Suppression (0.62): Moderate-high. Junior faculty face significant barriers to exit: doctoral training is sunk investment in academic identity, institutional prestige hierarchies channel career ambitions into academic paths, alternative sectors (industry, administration) carry stigma as 'leaving the field,' and the academic labor market is nationally coordinated with limited position availability. Suppression is not total because some faculty do exit, but the cost is psychologically and economically significant. Theater ratio (0.65): Moderate-high. The system maintains substantial performative content: tenure review committees evaluate 'research excellence' through metrics (publication count, citation impact, grant funding) that correlate imperfectly with actual knowledge production; teaching evaluations are quantified but weakly predictive of student learning; service contributions are tracked but loosely evaluated. The theater has increased as evaluation bureaucracy expanded while actual autonomy in research direction decreased due to external funding dependencies. The core protective function (freedom from political pressure) remains genuine but increasingly marginal for most academic workers.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces the full range of classifications from different structural positions. Tenured faculty with arbitrage options see near-pure coordination (Rope) — they experience protection and autonomy. Junior faculty with no exit see extraction (Snare) — they bear the probationary burden. Organized faculty see the hybrid clearly (Tangled Rope) — they negotiate over who bears extraction costs. Star researchers see leverage (Tangled Rope) — they use exit threat to extract resources while benefiting from protection. Administration sees degradation (Piton) — the ritual persists but the function has atrophied through contingent hiring. Reformers see a temporary system being replaced (Scaffold) — they advocate for alternative employment models with different security mechanisms. The analytical view risks naturalizing (Mountain) — framing tenure as an inevitable response to knowledge production requirements — but the structural data reveals it as contingent: alternative institutions (corporate labs, international universities) produce knowledge under different employment arrangements, showing that the tenure-freedom link is not immutable. The perspectival gap reflects genuine structural differences: tenured insiders benefit from the system; trapped outsiders bear its costs; mobile actors exploit its asymmetries; organized actors negotiate its terms; administration maintains its ritual; reformers imagine its replacement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position: their power level, exit options, and relationship to the extraction flow. Tenured faculty and institutions (institutional power, arbitrage exit) experience low d → negative effective extraction — they are net beneficiaries. Junior faculty (powerless, trapped) experience high d → high effective extraction — the system extracts from them. Contingent workers (powerless, trapped) experience highest d — they are excluded from tenure's protection entirely. Organized faculty (organized power, constrained exit) experience moderate d — they benefit from protections while bearing some extraction costs through uncertainty and contingency. Star researchers (powerful, mobile) experience low d in the protection function (they benefit from freedom) but can raise d selectively when extracting resources (threatening to exit). The analytical observer (analytical, analytical) experiences high d observationally — they see the full structure including externalities — but this is an observer-relative property, not a structural one. The engine derives d automatically from beneficiary/victim declarations and exit capacity; the resulting χ values show why different agents classify the same constraint type differently.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The system resolves mandatrophy (preventing misclassification of coordination as pure extraction) through mandatory declaration of both beneficiaries (tenured faculty, institutions) and victims (junior faculty, adjuncts). The tangled_rope type requires: (1) at least one beneficiary (satisfied: tenured faculty), (2) at least one victim (satisfied: junior faculty and contingent workers), (3) requires_active_enforcement=true (satisfied: tenure committees actively adjudicate promotion decisions), and (4) a genuine coordination function alongside asymmetric extraction (satisfied: tenure protects academic freedom while extracting effort from the young). Without the victim and beneficiary declarations, a naive observer might classify this as pure Rope (coordination with minimal extraction) or miss the extraction entirely. The mandatrophy gates enforce that BOTH the protective coordination function AND the extraction asymmetry are acknowledged in the classification. The rope (pure coordination) perspective from tenured faculty is legitimate but incomplete; the snare (pure extraction) perspective from junior faculty is legitimate but incomplete; the tangled rope type integrates both. The false mountain perspective (naturalizing tenure as inherent to knowledge production) is exposed as a false summit by the structural decomposition of beneficiaries, victims, and enforcement mechanisms — revealing tenure as a contingent institutional choice, not a law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_magnitude_threshold,
    'At what publication/teaching/service burden level does the probationary period constitute extraction rather than legitimate competitive vetting?',
    'Comparative analysis of success rates, time-to-tenure, and burnout rates across disciplines and institution types; measurement of ''normal'' vs ''superhuman'' expectation baselines; documentation of cases where stated expectations exceeded achieved metrics by departing cohort',
    'If threshold is low (2-3 papers per year): many institutions are running extractive snares disguised as meritocratic tenure tracks. If threshold is high (8+ papers): institutional variation is extreme and classification depends on department.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_magnitude_threshold, empirical, 'At what performance burden does tenure track cross from vetting to extraction').

omega_variable(
    contingent_labor_substitution_effect,
    'Is the growth of adjunct and contingent labor a causal result of tenure protection (institutions offload risk onto contingent workers to fund tenure protections) or an independent trend?',
    'Historical comparison of tenure density vs contingent hiring patterns; regression of institutional tenure expenditure against contingent labor use; interviews with administrative decision-making on hiring strategy',
    'If causal: tenure system is structurally dependent on extraction from contingent workers, making it a true snare for that population. If independent: tenure and contingency are separable policy choices, and the snare structure exists but could be decoupled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contingent_labor_substitution_effect, empirical, 'Whether tenure growth causally drives contingent labor expansion').

omega_variable(
    freedom_protection_sufficiency,
    'Does tenure actually protect academic freedom, or has political/corporate pressure found alternate mechanisms that tenure does not prevent?',
    'Documentation of academic freedom violations (firings, forced resignations, coercive constraint of research) comparing tenure-protected vs contingent faculty; analysis of whether tenure violations occur through indirect mechanisms (non-renewal of research funding, administrative retaliation, harassment)',
    'If tenure does protect: the coordination function is real and the system is hybrid (tangled rope). If tenure is routinely circumvented: the coordination function is aspirational and the system is closer to pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_protection_sufficiency, empirical, 'Whether tenure actually protects academic freedom from pressure').

omega_variable(
    alternative_security_viability,
    'Can fixed-term renewable contracts with genuine income security and benefits provide equivalent academic freedom protection to permanent tenure?',
    'Case studies of non-tenure alternative employment models (international universities, research institutes, industry labs); comparison of research output, innovation, and freedom of inquiry across employment models; measurement of whether researchers feel equivalent freedom in renewable-term vs permanent-tenure positions',
    'If viable: the scaffold perspective is structurally correct and tenure system has genuine sunset — alternatives can replace it. If unviable: the system is locked in by fundamental requirements and alternatives are aspirational.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_security_viability, empirical, 'Whether alternative employment models can protect academic freedom as effectively as tenure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_tenure_system, 1970, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_tr_t0, academic_tenure_system, theater_ratio, 0, 0.45).
narrative_ontology:measurement(tenure_tr_t15, academic_tenure_system, theater_ratio, 15, 0.55).
narrative_ontology:measurement(tenure_tr_t30, academic_tenure_system, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(tenure_be_t0, academic_tenure_system, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tenure_be_t15, academic_tenure_system, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(tenure_be_t30, academic_tenure_system, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_tenure_system, enforcement_mechanism).
narrative_ontology:affects_constraint(academic_tenure_system, academic_publishing_peer_review).
narrative_ontology:affects_constraint(academic_tenure_system, doctoral_training_debt_pipeline).
narrative_ontology:affects_constraint(academic_tenure_system, research_funding_concentration).

% DUAL FORMULATION NOTE:
% The tenure system is upstream of publishing pressure (the tenure requirement drives publication expectations) and doctoral debt (the tenure track incentivizes lengthy PhD training as preparation). It is also coupled to research funding concentration (external funding increasingly determines research direction and job security, competing with tenure's protective mandate). Separate constraint stories for each downstream claim have their own ε values reflecting their distinct structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(academic_tenure_system, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
