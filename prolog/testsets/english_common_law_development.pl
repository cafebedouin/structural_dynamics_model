% ============================================================================
% CONSTRAINT STORY: english_common_law_development
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_english_common_law_development, []).

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
 *   constraint_id: english_common_law_development
 *   human_readable: English Common Law Development Constraint
 *   domain: legal/institutional
 *
 * SUMMARY:
 *   English common law developed from 12th-century royal courts into a
 *   self-reinforcing system of precedent, judicial interpretation, and
 *   institutional authority. Over 800+ years, the system exhibits classic
 *   tangled_rope dynamics: it genuinely coordinates legal interpretation and
 *   enables doctrinal evolution (coordination function), yet simultaneously
 *   extracts value toward established voices and suppresses alternative
 *   jurisprudential frameworks (extraction asymmetry). The constraint's
 *   extractiveness has increased modestly (0.28 to 0.38) as doctrinal
 *   complexity has grown, while theater ratio has increased more sharply
 *   (0.35 to 0.58) as the performative content of precedent adherence has
 *   risen relative to its constraining force. The system maintains legitimacy
 *   through the narrative of flexibility and gradual reform, yet structurally
 *   privileges voices that can command centuries of citation recognition. The
 *   credentialing pipeline (articled clerkship, bar examination, senior rank
 *   attainment) functions simultaneously as qualification signaling and as
 *   gatekeeping mechanism suppressing alternative interpretive traditions.
 *
 * KEY AGENTS:
 *   - Established Common Law Interpreters: Primary beneficiary (institutional/arbitrage) — senior judges, established practitioners, institutional legal bodies; benefit from precedent system's distribution of interpretive authority
 *   - Legal Innovation Capacity: Primary victim (powerless/trapped) — novel jurisprudential approaches, excluded interpretive frameworks, reformist legal theories; suppressed by precedent doctrine and credentialing barriers
 *   - Excluded Interpretive Voices: Secondary victim (powerless/trapped) — non-common-law traditions, marginalized legal scholars, practitioners lacking institutional affiliation; face structural barriers to recognition
 *   - Legal Reformers: Moderate agent (moderate/constrained) — reform-minded jurists seeking doctrinal change; benefit from common law's flexibility while facing suppression from institutional inertia
 *   - Legislative Reformer Coalition: Organized actors (organized/mobile) — parliamentary committees, law commissions, codification movements; see statutory reform as sunset mechanism replacing common law primacy
 *   - Precedent Ritual Performers: Institutional actors (institutional/arbitrage) — courts, bar associations, legal institutions; maintain and perform the stare decisis doctrine
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing path-dependence as immutable law; might miss that alternative legal systems prove precedent monopoly is contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(english_common_law_development, 0.38).
domain_priors:suppression_score(english_common_law_development, 0.42).
domain_priors:theater_ratio(english_common_law_development, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(english_common_law_development, extractiveness, 0.38).
narrative_ontology:constraint_metric(english_common_law_development, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(english_common_law_development, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(english_common_law_development, tangled_rope).
narrative_ontology:human_readable(english_common_law_development, "English Common Law Development Constraint").
narrative_ontology:topic_domain(english_common_law_development, "legal/institutional").

domain_priors:requires_active_enforcement(english_common_law_development).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(english_common_law_development, established_common_law_interpreters).
narrative_ontology:constraint_beneficiary(english_common_law_development, institutional_continuity_agents).
narrative_ontology:constraint_victim(english_common_law_development, legal_innovation_capacity).
narrative_ontology:constraint_victim(english_common_law_development, excluded_interpretive_voices).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED LEGAL VOICE (SNARE) — Non-established legal practitioners, marginalized traditions, and alternative jurisprudential frameworks face structural entrapment. The common law system requires decades of credentialing, institutional affiliation, and citation recognition to be heard authoritatively. No exit option exists short of abandoning legal practice entirely. Maximum experienced extraction: innovation capacity is suppressed; institutional inertia bears down without relief.
constraint_indexing:constraint_classification(english_common_law_development, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGAL REFORMER (TANGLED ROPE) — Reform-minded jurists benefit from common law's flexibility and evolutionary capacity (genuine coordination function), yet face suppression from precedent doctrine and institutional resistance. Constrained by career cost of radical departures, but possess some agency to reshape doctrine through careful argumentation. Mixed coordination-extraction hybrid: the system enables gradual change while suppressing rapid reform.
constraint_indexing:constraint_classification(english_common_law_development, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMON LAW ESTABLISHMENT (ROPE) — Senior judges, established practitioners, and institutional legal bodies experience common law as pure coordination: the system coordinates legal interpretation, enables predictable doctrine development, and provides arbitrage access to doctrinal reshaping. Net beneficiary — the constraint enables their authority and distributes recognition value toward established voices.
constraint_indexing:constraint_classification(english_common_law_development, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGISLATIVE REFORMER COALITION (SCAFFOLD) — Organized actors (parliamentary committees, law commission bodies, codification movements) see common law's doctrinal constraints as a temporary coordination failure solvable through explicit statutory reform. Legislative pathways create sunset mechanisms: codification establishes clearer rules, statutory amendment overrides judge-made doctrine, and written constitutions cap the common law's supremacy. Organized agents see exit pathways and can coordinate alternatives.
constraint_indexing:constraint_classification(english_common_law_development, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: PRECEDENT RITUAL (PITON) — The doctrine of stare decisis (binding precedent) persists largely as institutional theater: nominal adherence to precedent coexists with extensive techniques for doctrinal avoidance (distinguishing, limiting, overruling). The ritual of precedent provides legitimacy cover for judicial discretion while its actual constraining force has degraded over centuries. Theater ratio reflects the gap between the formal rule (precedent is binding) and operational reality (precedent is routinely reinterpreted).
constraint_indexing:constraint_classification(english_common_law_development, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, some doctrinal constraint is inherent to any legal system based on precedent: the past always constrains the future, and no legal system can operate with pure freedom. This perspective views common law's path-dependence as immutable. However, this naturalizes a contingent institutional choice (adherence to precedent norms) as inherent law. The engine's false summit detector will identify this as misclassification: path-dependence is real but not immutable — statutory law, civil law systems, and written constitutions all prove alternative structures are possible.
constraint_indexing:constraint_classification(english_common_law_development, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(english_common_law_development_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(english_common_law_development, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(english_common_law_development, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(english_common_law_development, TR),
    TR >= 0.70.

:- end_tests(english_common_law_development_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The common law system provides genuine coordination benefits (predictable doctrine, enabling incremental reform, facilitating complex legal reasoning) while extracting value toward established voices through precedent privilege, credentialing barriers, and centuries of accumulated interpretive authority. The extraction is not maximal (snare-level) because genuine flexibility exists — doctrinal change does occur, and alternative voices can eventually gain hearing — but barriers are substantial. Theater ratio (0.58): Moderate-high. The doctrine of stare decisis is performed more than enforced: judges routinely distinguish, limit, or reinterpret precedent while maintaining nominal adherence to the binding rule. The gap between formal doctrine (precedent is binding) and operational reality (precedent is routinely reframed) produces theater. Theater has increased over the interval as legal complexity has grown and judges have developed more sophisticated distinction/limitation techniques. Suppression (0.42): Moderate. Barriers to innovation include precedent doctrine, credentialing requirements, institutional gatekeeping through citation networks, and the cost of decades-long practice building. But suppression is not absolute — statutory reform is possible, novel arguments occasionally prevail, and legislative override exists. The powless/trapped perspective experiences higher suppression (0.55+), while organized reformers experience lower suppression (0.30-0.35).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how institutional power concentrates interpretive authority even within nominally flexible systems. The establishment's experience (Rope) is legitimate — common law does enable gradual doctrinal evolution and provides genuine coordination. But this legitimacy masks extraction: the system privileges voices that can command institutional affiliation, centuries of citation recognition, and credentialing credentials. The excluded voice's experience (Snare) is equally structural: without institutional backing, novel jurisprudential frameworks are systematically invisible to the system, not because they lack merit but because the system's gatekeeping mechanisms operate through citation privilege and credentialing requirements. The perspectival gap is not resolvable by claiming 'both are true' — it reveals that the system simultaneously coordinates (genuine function) and extracts (asymmetric benefit distribution).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural relationship to the constraint. Established interpreters are beneficiaries with arbitrage access (low d, negative f(d), experienced as pure coordination). Legal reformers are partial victims with constrained exits (moderate d, moderate f(d), experienced as mixed). Excluded voices are full victims with trapped exits (high d, high f(d), experienced as snare). Legislative coalitions are organized agents with mobile exits (moderate d, can coordinate around the constraint). The precedent system's directionality derives from institutional power and arbitrage: judges can reshape doctrine while claiming fidelity to precedent, giving them low-cost reinterpretation arbitrage. The powerless/trapped agent experiences directionality near 0.90 (nearly pure target), producing maximum f(d) and maximum experienced extractiveness. The institutional beneficiary experiences directionality near 0.10 (nearly pure beneficiary with arbitrage), producing near-zero or negative f(d).
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION REQUIRED: English common law should decompose into at least three distinct constraints with different ε values: (1) DOCTRINAL COORDINATION FUNCTION (ε≈0.15, Rope) — the genuine coordination mechanism of precedent-based legal interpretation; (2) INSTITUTIONAL INTERPRETIVE GATEKEEPING (ε≈0.55, Tangled Rope) — the extraction of recognition toward credentialed voices and established interpreters; (3) PRECEDENT RITUAL PERFORMANCE (ε≈0.12, Piton) — the theater of stare decisis adherence. This story currently conflates these three structurally distinct mechanisms. However, treating the combined system holistically as Tangled Rope (ε=0.38) is defensible if the extraction and coordination are genuinely coupled: the gatekeeping mechanism (requiring institutional affiliation, credentialing) is necessary to maintain the doctrinal coordination (judges need recognized authority to shape doctrine). The mandatrophy is resolved by this coupling logic: common law cannot coordinate without gatekeeping, and the gatekeeping is not parasitic on coordination but rather enables it. The extraction toward established voices is the price of doctrinal stability and incremental evolutionary capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_flexibility_threshold,
    'At what rate of doctrinal change does common law cease to coordinate and become pure extraction (privileging established voices through fake flexibility)?',
    'Empirical analysis of overruling rates by decade, citation patterns favoring established vs novel arguments, and speed of doctrinal evolution relative to social change pressures',
    'If flexibility rate > 5% per generation: common law is genuine rope/tangled rope. If rate < 1%: system is predominantly piton (theater of flexibility). Determines whether extracted voices are experiencing genuine coordination failure or performative openness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_flexibility_threshold, empirical, 'Rate threshold distinguishing doctrinal flexibility from performative rigidity').

omega_variable(
    alternative_jurisprudential_viability,
    'Can non-common-law jurisprudential frameworks (natural law, civil law, Islamic law, indigenous legal traditions) coexist within or alongside the common law system, or does the system require monopoly on interpretive authority?',
    'Historical analysis of legal pluralism in common law jurisdictions; examination of colonial-era resistance to common law imposition; contemporary multiculturalism and family law disputes; statutory frameworks recognizing alternative traditions',
    'If coexistence is possible: suppression is institutional preference rather than structural necessity (classification remains tangled_rope). If monopoly is required: common law''s extractiveness is higher than measured (reclassify toward snare). Determines whether suppression of excluded voices is contingent or inherent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_jurisprudential_viability, empirical, 'Whether alternative jurisprudential systems can coexist with common law').

omega_variable(
    credentialing_capture_mechanism,
    'Does the centuries-long credentialing pipeline (articled clerkship, bar examination, senior rank attainment) primarily serve gatekeeping/extraction or genuine qualification signaling?',
    'Correlation analysis between credentials and actual legal competence; examination of successful legal innovation by uncredentialed vs credentialed actors; cost-benefit analysis of extended training periods relative to legal knowledge requirements',
    'If primarily gatekeeping: suppression metric should be higher (0.50+), reclassifying toward snare from moderate perspectives. If genuinely signaling: credentialing is legitimate coordination cost, extraction metric decreases. Determines whether the powerless/trapped perspective reflects real structural barriers or credential-based discrimination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialing_capture_mechanism, empirical, 'Whether legal credentialing serves gatekeeping or qualification signaling').

omega_variable(
    institutional_inertia_vs_path_dependence,
    'Is common law''s resistance to reform inherent to precedent-based systems (path-dependence), or is it institutional inertia exploitable by organized reform movements (scaffold dynamics)?',
    'Comparative analysis of doctrinal change rates in common law vs civil law jurisdictions; examination of reform success rates when organized coalitions mobilize; historical case studies of major doctrinal shifts (tort law, contract law, constitutional interpretation)',
    'If inherent: scaffold sunset is aspirational (no real exit path). If institutional inertia: scaffold is valid, sunset is achievable through legislative reform. Determines whether legal reformers actually experience constrained agency (tangled_rope) or trapped agency (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_path_dependence, empirical, 'Distinction between path-dependence and exploitable institutional inertia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(english_common_law_development, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(engl_tr_t0, english_common_law_development, theater_ratio, 0, 0.35).
narrative_ontology:measurement(engl_tr_t200, english_common_law_development, theater_ratio, 200, 0.48).
narrative_ontology:measurement(engl_tr_t400, english_common_law_development, theater_ratio, 400, 0.58).

% Extraction over time
narrative_ontology:measurement(engl_be_t0, english_common_law_development, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(engl_be_t200, english_common_law_development, base_extractiveness, 200, 0.34).
narrative_ontology:measurement(engl_be_t400, english_common_law_development, base_extractiveness, 400, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(english_common_law_development, enforcement_mechanism).
narrative_ontology:affects_constraint(english_common_law_development, legal_precedent_path_dependence).
narrative_ontology:affects_constraint(english_common_law_development, professional_legal_credentialing).
narrative_ontology:affects_constraint(english_common_law_development, judicial_interpretive_authority).

% DUAL FORMULATION NOTE:
% English common law development should decompose into: (1) doctrinal_coordination_function (ε~0.15, Rope) — genuine coordination of legal interpretation; (2) institutional_gatekeeping (ε~0.55, Tangled Rope) — extraction toward credentialed voices; (3) precedent_ritual_performance (ε~0.12, Piton) — theater of stare decisis. This story treats the integrated system as Tangled Rope (ε=0.38) due to coupled coordination-extraction dynamics. Decomposition is optional if the coupling is verified as structural rather than contingent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(english_common_law_development, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
