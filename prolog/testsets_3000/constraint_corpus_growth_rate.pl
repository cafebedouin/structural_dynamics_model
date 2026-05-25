% ============================================================================
% CONSTRAINT STORY: constraint_corpus_growth_rate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constraint_corpus_growth_rate, []).

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
 *   constraint_id: constraint_corpus_growth_rate
 *   human_readable: Constraint Corpus Growth Rate Asymmetry
 *   domain: institutional/knowledge_governance
 *
 * SUMMARY:
 *   The constraint corpus growth rate reflects a structural tension between
 *   the need for comprehensive constraint representation across domains and
 *   the institutional capacity to author, validate, and integrate new
 *   constraint stories. The constraint operates through multiple mechanisms:
 *   (1) authorship bottleneck — expertise requirements and narrative
 *   complexity create high barrier to entry for new authors, (2) validation
 *   gate — centralized schema enforcement and expert review slow submission
 *   throughput, (3) type skew — institutional preference for well-established
 *   constraint types (Mountain, Rope, Piton) and gatekeeping friction against
 *   emerging types (interpersonal dynamics, cognitive capture,
 *   identity-locked exit mechanisms), and (4) theater accumulation — the
 *   review process becomes increasingly performative as automated validation
 *   handles most errors, yet the ritual persists through inertia. This
 *   constraint exhibits all six DR types from different structural positions,
 *   revealing how knowledge governance systems can simultaneously coordinate
 *   and extract.
 *
 * KEY AGENTS:
 *   - Constraint Authorship Power: Primary beneficiary (institutional/arbitrage) — controls schema interpretation, validation authority, and corpus narrative. Benefits from centralized control and from defining what counts as a valid constraint.
 *   - Corpus Representativeness: Primary victim (powerless/trapped) — abstract collective good that cannot exit the growth bottleneck. Underrepresented constraint types remain underexplored; cannot organize to demand authoring resources.
 *   - Authors and Discovery Communities: Secondary victim (moderate/constrained) — face high authorship overhead, validation delays, and expertise barriers. Also benefit from corpus as reference material and collaborative tooling.
 *   - Classification Gate Enforcers: Institutional beneficiary (institutional/arbitrage) — maintains schema consistency and prevents degradation. Controls narrative of what constitutes valid constraint knowledge.
 *   - Decentralized Authoring Coalition: Organized agents (organized/constrained) — building parallel authoring frameworks and distributed validation systems as exit from centralized bottleneck.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (schema complexity, review rituals) as immutable features of constraint knowledge governance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constraint_corpus_growth_rate, 0.52).
domain_priors:suppression_score(constraint_corpus_growth_rate, 0.48).
domain_priors:theater_ratio(constraint_corpus_growth_rate, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constraint_corpus_growth_rate, extractiveness, 0.52).
narrative_ontology:constraint_metric(constraint_corpus_growth_rate, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(constraint_corpus_growth_rate, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constraint_corpus_growth_rate, tangled_rope).
narrative_ontology:human_readable(constraint_corpus_growth_rate, "Constraint Corpus Growth Rate Asymmetry").
narrative_ontology:topic_domain(constraint_corpus_growth_rate, "institutional/knowledge_governance").

domain_priors:requires_active_enforcement(constraint_corpus_growth_rate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constraint_corpus_growth_rate, constraint_authorship_power).
narrative_ontology:constraint_beneficiary(constraint_corpus_growth_rate, classification_gate_enforcers).
narrative_ontology:constraint_victim(constraint_corpus_growth_rate, corpus_representativeness).
narrative_ontology:constraint_victim(constraint_corpus_growth_rate, constraint_discovery_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CORPUS REPRESENTATIVENESS — Cannot exit the growth constraint. Abstract collective need for balanced constraint coverage bears full cost of authorship bottleneck. Underrepresented constraint types (Tangled Rope, Scaffold, interpersonal dynamics) remain underexplored while well-covered types (Mountain, Rope, Piton) accumulate redundantly. No mechanism to exit this asymmetry.
constraint_indexing:constraint_classification(constraint_corpus_growth_rate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AUTHORS AND DISCOVERY COMMUNITIES — Constrained by authorship overhead, expertise requirements, and validation review bottlenecks. High barrier to entry for generating new constraint stories. Also benefits from existing corpus as reference material and from collaborative authoring frameworks. Mixed extraction and coordination — significant asymmetry in who can author vs. who benefits from breadth.
constraint_indexing:constraint_classification(constraint_corpus_growth_rate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CLASSIFICATION GATE ENFORCERS — Primary beneficiary. Controls the schema validation, the technical authoring tools, the curation standards, and the acceptance criteria for constraint stories. Benefits from centralized control of the corpus. Experiences the constraint as coordination: enforcing gates maintains consistency and prevents degraded submissions. Net beneficiary.
constraint_indexing:constraint_classification(constraint_corpus_growth_rate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZED AUTHORING COALITION — Organized agents (independent researchers, cross-disciplinary teams, domain experts outside traditional hierarchy) see the centralized bottleneck as a temporary coordination failure with a sunset. Distributed constraint discovery frameworks, autonomous authoring templates, and lightweight validation pipelines are being developed as alternatives. Sunset clause: adoption of decentralized validation tooling within 5-10 years would bypass centralized gates.
constraint_indexing:constraint_classification(constraint_corpus_growth_rate, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MANUAL REVIEW AND CURATION RITUAL — The per-story manual review process for schema compliance, metric accuracy, and perspectival completeness is largely performative. Automated validation catches many errors; humans catch edge cases and provide narrative feedback. But the theater persists through institutional inertia — the ritual demonstrates rigor and maintains authority, even though much of the review content could be automated. Theater ratio reflects how much of the review is performative signaling vs. functional error detection.
constraint_indexing:constraint_classification(constraint_corpus_growth_rate, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW — From a civilizational perspective, constraint corpus growth faces inherent limits: validation complexity increases with domain breadth, expertise requirements are inescapable, and schema coverage must expand to accommodate new constraint types. This perspective sees the growth bottleneck as an immutable feature of knowledge governance systems. However, structural data contradicts this naturalization — decentralized authoring, automated validation, and modular schema architectures are all technologically feasible alternatives that would shift the constraint from bottleneck to coordination.
constraint_indexing:constraint_classification(constraint_corpus_growth_rate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constraint_corpus_growth_rate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constraint_corpus_growth_rate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constraint_corpus_growth_rate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constraint_corpus_growth_rate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(constraint_corpus_growth_rate, TR),
    TR >= 0.70.

:- end_tests(constraint_corpus_growth_rate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. Measured as the asymmetry between authors' effort and their proportional voice in corpus design. Initial value (0.35) reflects coordination function — early stage benefited all participants in establishing shared frameworks. Current value (0.52) reflects accumulation of gating mechanisms: each new constraint type requires schema expansion, each schema expansion increases validation complexity, each complexity increase raises authorship barriers. Suppression (0.48): Moderate. Authors cannot easily exit the system — they must work within the established schema and submit to centralized review. However, alternatives are technically feasible (decentralized validation, modular schemas), so exit is constrained rather than impossible. The suppression is not psychological (not identity_locked) but structural (expertise barriers, review queues, schema friction). Theater ratio (0.58): Moderate-high and rising. The manual review ritual was initially functional (catching schema violations, ensuring metric accuracy, checking narrative logic). As automated validation improves, the theater content increases — reviews now spend effort on edge cases and narrative taste rather than functional error detection. The ritual persists partly through institutional inertia (demonstrating rigor and maintaining authority) rather than pure function.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between gate enforcers (who see coordination and consistency requirements) and authors (who see barriers and extraction). Gate enforcers' rope perspective emphasizes the need for shared validation standards and schema consistency — the constraint solves the collective action problem of preventing degraded submissions. Authors' snare and tangled rope perspectives emphasize the cost of expertise barriers, review delays, and schema friction — they bear the extraction without capturing proportional voice in system design. The decentralized coalition sees a scaffold — the bottleneck is temporary because distributed authoring and validation are technically feasible alternatives. The piton perspective (on the review ritual) exposes how the constraint's theater has accumulated over time: early reviews were functional error-catching, recent reviews are performative ritual-maintenance. The mountain perspective (natural law view) wrongly naturalizes institutional complexity as immutable — it confuses schema design choices with constraints of reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Classification gate enforcers occupy the beneficial end (d ≈ 0.15): they maintain consistency, control narrative, and benefit from centralized authority. Authors and communities occupy the constrained victim end (d ≈ 0.60): they face barriers, delays, and expertise requirements but also access the corpus. The corpus representativeness is a trapped victim (d ≈ 0.95): it cannot organize, exit, or demand resources. The analytical observer at civilizational scope risks naturalizing this arrangement (d ≈ 0.70 as observer) — treating institutional complexity as immutable rather than contingent. The sigmoid f(d) produces moderate-to-high chi because trapped and constrained victims experience the growth rate asymmetry acutely, while beneficiaries experience it as coordination. Decentralized coalition agents have exit paths (organized/constrained), lowering their experienced extraction relative to pure victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing that the apparent 'growth bottleneck' is actually a bundle of five distinct structural elements: (1) genuine coordination function (shared schema, consistency requirements) — legitimate rope, (2) authorship expertise requirement (some complexity is inherent) — justified constraint, (3) gatekeeping extraction (preference for certain types, friction against emerging types) — extractive asymmetry, (4) theater accumulation (performative review) — institutional inertia, and (5) technical alternatives (decentralized validation, modular schemas) — feasible sunset paths. The constraint is neither pure coordination nor pure extraction — it is a tangled rope where the coordination function (schema consistency) is real but the extraction mechanism (authorship barriers, type bias) is also structurally embedded. Resolving the mandatrophy requires disaggregating these elements: keep the coordination function (distributed validation can maintain consistency), reduce the extraction mechanism (simplify schema, distribute authoring authority), and implement the sunset clause (transition to decentralized frameworks over 5-10 years).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorship_expertise_requirement,
    'What proportion of corpus growth constraint is inherent domain expertise requirement vs. artificial gatekeeping?',
    'Comparative analysis of successful constraint story authoring by expertise level; tracking of author-expert dyad patterns; assessment of schema complexity vs. actual validation difficulty',
    'If inherent (>70%): growth rate reflects legitimate domain complexity. If artificial (<30%): schema and process can be simplified to enable broader authorship. Intermediate values indicate mixed constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorship_expertise_requirement, empirical, 'Expertise requirement inherent vs. gatekeeping-induced').

omega_variable(
    decentralized_validation_sufficiency,
    'Can distributed peer validation and automated schema checking replace centralized expert review while maintaining constraint story quality and consistency?',
    'Pilot decentralized authoring framework; comparison of error rates and classification consistency between centralized and distributed validation; user experience metrics for author friction',
    'If sufficient: scaffold sunset is real and achievable. If insufficient: central authority is necessary and the constraint is a justified institutional arrangement, not extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_validation_sufficiency, empirical, 'Decentralized validation technical feasibility').

omega_variable(
    constraint_type_discovery_bias,
    'Is corpus skew toward Mountain/Rope/Piton constraints driven by natural frequency of these types in reality or by author/gatekeeper preference for certain types?',
    'Survey of unexplored constraint domains; assessment of ease vs. incentive for authoring each type; comparison with theoretical prediction of type frequency under neutrality',
    'If preference-driven: growth asymmetry is extractive (beneficiaries prefer some types). If frequency-driven: asymmetry reflects reality and is not extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constraint_type_discovery_bias, empirical, 'Whether corpus skew reflects natural frequency or author/gatekeeper preference').

omega_variable(
    schema_complexity_scaling,
    'Does schema complexity increase faster than authoring tooling sophistication, creating a widening expertise gap?',
    'Tracking of schema feature count over time; measurement of tool usability metrics; correlation with author success rates and submission quality',
    'If gap widens: constraint acts as rising barrier (snare from author perspective). If gap narrows: tooling keeps pace and constraint acts as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(schema_complexity_scaling, empirical, 'Schema complexity vs. tooling sophistication trajectory').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constraint_corpus_growth_rate, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cgr_tr_t0, constraint_corpus_growth_rate, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cgr_tr_t3, constraint_corpus_growth_rate, theater_ratio, 3, 0.48).
narrative_ontology:measurement(cgr_tr_t6, constraint_corpus_growth_rate, theater_ratio, 6, 0.55).
narrative_ontology:measurement(cgr_tr_t9, constraint_corpus_growth_rate, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(cgr_be_t0, constraint_corpus_growth_rate, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cgr_be_t3, constraint_corpus_growth_rate, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(cgr_be_t6, constraint_corpus_growth_rate, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(cgr_be_t9, constraint_corpus_growth_rate, base_extractiveness, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constraint_corpus_growth_rate, information_standard).
narrative_ontology:affects_constraint(constraint_corpus_growth_rate, constraint_corpus_heterogeneity).
narrative_ontology:affects_constraint(constraint_corpus_growth_rate, constraint_knowledge_access_concentration).
narrative_ontology:affects_constraint(constraint_corpus_growth_rate, schema_complexity_accumulation).

% DUAL FORMULATION NOTE:
% The corpus growth rate constraint is part of a family of institutional friction constraints affecting knowledge governance. Upstream constraint: schema_complexity_accumulation (ε ≈ 0.38) — the accumulation of schema features over time creates the technical complexity that enables gatekeeping. Downstream constraints: constraint_corpus_heterogeneity (ε ≈ 0.55) — the skew toward well-established types produces bias in what constraints are discoverable and studied; constraint_knowledge_access_concentration (ε ≈ 0.62) — centralized authorship creates asymmetric voice in corpus narrative direction. Growth rate constraint links all three: complexity enables gatekeeping, gatekeeping creates skew, skew concentrates access.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constraint_corpus_growth_rate, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
