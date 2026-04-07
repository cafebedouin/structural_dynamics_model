% ============================================================================
% CONSTRAINT STORY: zipfs_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zipfs_law, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: zipfs_law
 *   human_readable: Zipf's Law (The Power Law of Information)
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   Zipf's Law represents a mathematical regularity so fundamental to
 *   information systems that it appears in nearly every domain involving
 *   ranked, unordered data: word frequencies in natural language, city sizes,
 *   wealth distributions, web traffic patterns, biological diversity, and
 *   solar flare magnitudes. The law states that the frequency of an item is
 *   inversely proportional to its rank: if the most common item appears f
 *   times, the kth-ranked item appears approximately f/k times. This
 *   relationship emerges spontaneously in large decentralized systems without
 *   centralized enforcement, making it a candidate for classification as a
 *   natural law. The constraint is not imposed by any agent or institution;
 *   it arises from the combinatorial structure of how information systems
 *   scale. The task is to determine whether Zipf's Law qualifies as a
 *   Mountain (immutable natural law) or whether it is better understood as a
 *   collection of domain-specific statistical artifacts that manifest
 *   differently under different observational frames.
 *
 * KEY AGENTS:
 *   - Mathematical Universe: Source of the invariant structure — does not benefit or suffer
 *   - Rank Distribution Subjects: Individual items, words, events (powerless/trapped) — have no agency in their position within the frequency hierarchy
 *   - Information Systems Designers: Engineers, data scientists (institutional/arbitrage) — benefit from predictability of Zipfian structure; use it for optimization rather than resisting it
 *   - Statistical Modeling Communities: Researchers and predictive systems (organized/arbitrage) — leverage Zipfian regularity for forecasting and resource allocation
 *   - Platforms Attempting Diversity: Social media, search engines (institutional/constrained) — bear costs when attempting to enforce non-Zipfian distributions to improve content diversity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zipfs_law, 0.12).
domain_priors:suppression_score(zipfs_law, 0.03).
domain_priors:theater_ratio(zipfs_law, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zipfs_law, extractiveness, 0.12).
narrative_ontology:constraint_metric(zipfs_law, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(zipfs_law, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zipfs_law, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(zipfs_law, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zipfs_law, mountain).
narrative_ontology:human_readable(zipfs_law, "Zipf's Law (The Power Law of Information)").
narrative_ontology:topic_domain(zipfs_law, "technological/mathematical").

domain_priors:emerges_naturally(zipfs_law).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL OBSERVER (MOUNTAIN) — Zipf's Law as a universal statistical regularity emerging across all large, unranked datasets. Eigenvalue distributions, word frequencies, city populations, solar flares, and earthquake magnitudes all exhibit the same inverse power relationship. This is not enforced by any agent; it emerges from the combinatorial structure of information itself. Zero degrees of freedom — the relationship is invariant across observational frame, measurement basis, and historical period. Pure natural law of information systems.
constraint_indexing:constraint_classification(zipfs_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: RANK DISTRIBUTION SUBJECT (MOUNTAIN) — Any system attempting to generate a large corpus of items without explicit centralized ranking automatically produces Zipfian distributions. The constraint is inescapable: individual items cannot 'opt out' of rank position. The distribution emerges from decentralized production and is immutable without external intervention. Powerless agents (individual items, words, events) have no choice regarding their position in the frequency hierarchy.
constraint_indexing:constraint_classification(zipfs_law, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: INFORMATION SYSTEMS DESIGNER (MOUNTAIN) — Engineers, data scientists, and systems architects encounter Zipf's Law as an immutable property of large-scale information systems. Attempting to create 'flat' distributions (equal frequency across all ranks) is thermodynamically impossible without continuous external enforcement. The law persists across all technological implementations: natural language processing, internet traffic, database distributions, user activity logs. Zero degrees of freedom for system design.
constraint_indexing:constraint_classification(zipfs_law, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: PREDICTIVE ANALYTICS COALITION (MOUNTAIN) — Statistical modeling communities leverage Zipf's Law as a constraint they can rely on, not resist. The power law enables predictive capacity: knowing the rank of an item enables probability estimation. Organizations exploit Zipfian structure (caching strategies, search indexing, resource allocation) not by violating it but by accepting and optimizing within it. The constraint provides reliable structure, not oppressive extraction.
constraint_indexing:constraint_classification(zipfs_law, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zipfs_law_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(zipfs_law, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(zipfs_law, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(zipfs_law, ExtMetricName, E),
    domain_priors:suppression_score(zipfs_law, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(zipfs_law),
    narrative_ontology:constraint_metric(zipfs_law, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(zipfs_law, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(zipfs_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. Zipf's Law describes a statistical regularity that emerges from decentralized production, not an extractive mechanism. No agent is extracting value from any other agent; the distribution is a consequence of combinatorial structure. The value is slightly above zero only to account for cases where systems deliberately maintain Zipfian structure for optimization purposes, which might be seen as a weak form of exploitation of the tail (suppressing tail items to optimize head performance). Suppression (0.03): Negligible. The law is not maintained through coercion or alternative suppression. It emerges naturally from unforced production dynamics. The minimal value accounts for the fact that creating large unranked datasets inherently produces this distribution without suppression being necessary. Theater Ratio (0.15): Minimal. There is no performative element to Zipf's Law. The relationship is directly observable in empirical data. The small non-zero value reflects that scientific explanation and formalization of the law contain some narrative framing, but the underlying phenomenon is observable without theater.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify Zipf's Law identically as a Mountain because the constraint manifests the same way from every observational position. This uniformity across all indices is the strongest evidence for mountain classification. The mathematical observer sees pure structure; the powerless subject sees inescapable distribution; the systems designer sees immutable constraints on information architecture; the analytics coalition sees reliable exploitable regularity. None of these perspectives generate disagreement about the nature of the constraint — only about how to interpret or respond to it. The absence of perspectival gap (where beneficiaries and victims would perceive different constraint types) confirms that Zipf's Law is not an institutional artifact or extractive mechanism but a structural property of information systems themselves.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint exhibits uniform directionality across all observational contexts because it is an immutable natural law. No agent experiences structural directionality relative to Zipf's Law because the law is not a mechanism of extraction or coordination — it is a property of information itself. The mathematical observer derives zero directionality; the rank distribution subject experiences the constraint as structurally inescapable but not extractive (d ≈ 0.5, symmetric); institutional designers benefit from predictability (d ≈ 0.0, beneficiary); organized forecasters exploit the regularity (d ≈ 0.15, slight beneficiary). These are not meaningfully different structural positions — all are passive recipients of an immutable regularities. The absence of perspectival variation in directionality is diagnostic of mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through the uniform mountain classification across all perspectives. There is no temptation to label Zipf's Law as pure coordination (Rope) or as hidden extraction (Snare) because the structural data does not support either reading. No coordination benefit exists — the law is not enforced by cooperative agents solving collective action problems. No extraction exists — no agent bears costs while others benefit from Zipfian structure; rather, all agents experience it as a constraint they must adapt to. The mountain classification is robust to alternative observables (changing how rank is defined, using different frequency measures, sampling vs. enumeration) because the power-law relationship persists invariantly. The risk of false summit (naturalizing contingent institutional arrangements) is low here because the regularity is empirically demonstrable across biological, natural, and social systems independent of any institutional enforcement. Zipf's Law represents one of the clearest cases of a true mountain in the constraint corpus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_vs_domain_specific,
    'Is Zipf''s Law a true universal principle or a domain-specific statistical artifact of how humans and natural systems generate unranked datasets?',
    'Meta-analysis of power-law exponents across natural, social, and technological systems; investigation of whether deviation from Zipfian form corresponds to mechanisms that actively enforce alternative distributions',
    'If universal: mountain classification holds across all observables. If domain-specific: different constraint stories for different information domains (natural language, web traffic, biological networks) with potentially different ε values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_vs_domain_specific, empirical, 'Whether Zipf''s Law is truly universal or domain-specific').

omega_variable(
    measurement_basis_invariance,
    'Does changing the measurement basis (counting absolute frequency vs. log-frequency, using different rank definitions, sampling vs. complete enumeration) produce different conclusions about the strength of Zipfian structure?',
    'Systematic comparison of Zipf exponent estimates across different measurement methodologies applied to the same corpus',
    'If invariant: confirms mountain. If measurement-dependent: reveals that ''Zipf''s Law'' labels multiple distinct constraints that should be decomposed into separate stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_basis_invariance, empirical, 'Whether Zipfian structure is invariant to measurement basis').

omega_variable(
    enforceability_boundary,
    'Can institutions or systems actively enforce non-Zipfian distributions, and if so, what is the cost (in suppression, theater, or active enforcement) required to maintain deviation?',
    'Case studies of attempted anti-Zipfian distribution engineering (e.g., platform recommendation algorithms that enforce diversity, educational content curation that mandates even reach); measurement of resource expenditure and failure modes',
    'If enforceability requires suppression or theater: Zipf''s Law is a mountain and enforcement mechanisms are separate constraints. If enforcement is self-sustaining: some systems may genuinely escape Zipfian structure and should be modeled separately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforceability_boundary, empirical, 'Whether and at what cost systems can enforce non-Zipfian distributions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zipfs_law, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zipf_tr_t0, zipfs_law, theater_ratio, 0, 0.15).
narrative_ontology:measurement(zipf_tr_t50, zipfs_law, theater_ratio, 50, 0.15).
narrative_ontology:measurement(zipf_tr_t100, zipfs_law, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(zipf_be_t0, zipfs_law, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(zipf_be_t50, zipfs_law, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(zipf_be_t100, zipfs_law, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zipfs_law, information_standard).
narrative_ontology:affects_constraint(zipfs_law, pareto_principle).
narrative_ontology:affects_constraint(zipfs_law, long_tail_economics).
narrative_ontology:affects_constraint(zipfs_law, search_relevance_ranking).

% DUAL FORMULATION NOTE:
% Zipf's Law is a foundational constraint that affects multiple downstream constraints in information and economic systems. Pareto principle (80/20 rule) is a direct consequence of Zipfian distributions when applied to business contexts. Long-tail economics studies how technological systems extend Zipf's original domain. Search ranking algorithms operate within the constraints imposed by Zipfian word and link distributions. These are not separate constraints but applications of Zipf's Law to different domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
