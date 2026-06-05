% ============================================================================
% CONSTRAINT STORY: zipfs_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   domain: technological/information_theory/linguistics
 *
 * SUMMARY:
 *   Zipf's Law is a mathematical regularity describing how frequency
 *   distributions rank-order across a vast domain: natural language word
 *   frequencies, city populations, firm sizes, website traffic, biological
 *   event frequencies, and seismic events. The pattern — where the k-th most
 *   frequent item appears roughly as often as the 1st item divided by k —
 *   emerges repeatedly in complex systems with no apparent coordination
 *   mechanism. The constraint is a structural property of information
 *   organization itself, not an imposed rule. Its universality and
 *   mathematical derivability from information-theoretic first principles
 *   (optimal coding under realistic constraints) mark it as a natural law:
 *   unchosen, unchangeable by institutional will, and accessible to any
 *   observer who examines ranked data. The constraint exhibits zero degrees
 *   of freedom for all indices — no agent can escape it, no observer
 *   perspective reframes it as contingent, no exit option avoids it. This is
 *   the canonical exemplar of a Mountain constraint.
 *
 * KEY AGENTS:
 *   - Information Theory Community: Analytical observers who verify the constraint's mathematical necessity
 *   - Language Communities: Speakers who experience the constraint as unchangeable linguistic structure
 *   - Technical Infrastructure: Network operators, search engines, database architects who must optimize around the power law
 *   - Natural Systems: Biological, geological, and social systems that generate Zipfian distributions through independent optimization
 *   - Empirical Scientists: Researchers across domains who observe the pattern's persistence and universality
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
narrative_ontology:constraint_metric(zipfs_law, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(zipfs_law, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zipfs_law, mountain).
narrative_ontology:human_readable(zipfs_law, "Zipf's Law (The Power Law of Information)").
narrative_ontology:topic_domain(zipfs_law, "technological/information_theory/linguistics").

domain_priors:emerges_naturally(zipfs_law).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL ANALYST (MOUNTAIN) — Zipf's Law emerges from fundamental information-theoretic principles: optimal coding, information entropy minimization, and the mathematics of rank-ordered distributions. Across all systems examined (natural language, city sizes, web traffic, biological organization), the power law appears as an inevitable consequence of how information organizes under selection pressure. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08. The constraint is perceived as a structural invariant of information systems themselves, not as an imposed rule.
constraint_indexing:constraint_classification(zipfs_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: LANGUAGE COMMUNITY (MOUNTAIN) — Speakers cannot escape Zipf's Law through collective decision or institutional reform. Attempts to equalize word frequency — artificial language projects (Esperanto), prescriptive grammar rules, controlled vocabularies — consistently fail when they conflict with the power law. Natural language evolution invariably reproduces the distribution. The constraint is unchangeable from within the language system itself. d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.07.
constraint_indexing:constraint_classification(zipfs_law, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: INFORMATION TECHNOLOGY INFRASTRUCTURE (MOUNTAIN) — Network operators, search engines, database architects, and content platforms encounter Zipf's Law as a fixed architectural constraint. Cache design, indexing strategies, query optimization, and resource allocation all must be built around the power law distribution. Attempts to flatten the distribution require energy and compute proportional to the deviation from the law — the constraint reasserts itself through thermodynamic efficiency. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.10. The infrastructure adapts to the law rather than resisting it.
constraint_indexing:constraint_classification(zipfs_law, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMPIRICAL NATURAL LAW OBSERVER (MOUNTAIN) — Zipf's Law exhibits the characteristic signature of a mountain constraint: universal observability across independent domains (natural language, city populations, firm sizes, web traffic, earthquake magnitudes, scientific publication counts), mathematical derivability from information-theoretic first principles, zero manipulation by local institutional actors, and persistence across centuries of observation. accessibility_collapse=0.91 (the law appears in virtually all sufficiently large ranked systems examined), resistance=0.08 (theoretical and empirical challenges are marginal refinements, not fundamental alternatives). d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.14, but the mountain gate fires on the NL profile metrics independently of χ.
constraint_indexing:constraint_classification(zipfs_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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
 *   Extractiveness (0.12): Minimal. Zipf's Law extracts nothing from any agent — it is a constraint on information organization, not a mechanism for redistributing resources. The low value reflects that the law is purely descriptive of structural inevitability, not coercive or extractive. Suppression (0.03): Minimal. No alternatives are suppressed because no alternatives are possible within realistic information systems. The law is not maintained against resistance; it emerges naturally. Theater ratio (0.15): Near-zero. Zipf's Law is not performative. Its verification is straightforward: rank any large dataset by frequency and the power law appears. No institutional ritual or theatrical maintenance is required. The small nonzero value reflects minor variations in how the exponent is measured across domains, but the core pattern is robust and unambiguous. Accessibility collapse (0.91): Very high. The law appears in virtually every examined system with sufficient sample size and proper boundary selection. Resistance (0.08): Very low. Theoretical and empirical challenges to the law exist (boundary effects, finite-size corrections, alternative exponent formulations) but none constitute fundamental resistance — they are refinements, not replacements.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on Mountain classification. The mathematical analyst sees inevitable information theory. The language community experiences unchangeable linguistic structure. The infrastructure community finds the law as an architectural constraint to optimize around, not against. The empirical observer catalogs its universal appearance. There is no gap — the constraint appears identically structurally necessary from every index. This uniformity is the hallmark of a true Mountain: no observer position generates an alternative reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Zipf's Law has no beneficiaries or victims. No agent benefits from the constraint; no agent bears a cost from it. The constraint is neutral with respect to power distribution — it appears equally in egalitarian and hierarchical information systems. Directionality derivation is not applicable to mountains. All agents experience the same structural inevitability regardless of power, time horizon, exit options, or scope. The constraint is observer-invariant in the full sense: d values would be identical if computed, but the computation is unnecessary because the mountain classification precedes beneficiary/victim analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   Zipf's Law resolves the mandatrophy by avoiding it entirely. The constraint is not a Snare misnamed as coordination, nor a Rope enhanced with hidden extraction. It is a pure structural invariant that makes no demands on any agent and cannot be manipulated or degraded by institutional redesign. The theater_ratio is low (0.15) because verification is direct observation, not institutional performance. The extractiveness is minimal (0.12) because nothing is extracted. The suppression is negligible (0.03) because no alternative is suppressed — the law is not maintained against resistance; it emerges from information-theoretic necessity. The constraint is not in danger of degrading toward a Piton (theatrical maintenance) because its function is not institutional but mathematical. Zipf's Law exemplifies how to distinguish true natural law constraints from contingent institutional arrangements: compare against the NL profile metrics. If accessibility_collapse≥0.85, resistance≤0.15, and emerges_naturally=true, the mountain classification is robust.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_coding_necessity,
    'Is Zipf''s Law an inevitable consequence of optimal information coding, or merely a frequent empirical pattern that systems gravitate toward but could theoretically escape?',
    'Information-theoretic derivation from first principles showing Zipf emerges from entropy minimization subject to realistic constraints; comparison with non-Zipfian information systems and analysis of their theoretical cost',
    'If necessary consequence of optimal coding: Mountain classification is correct (ε≤0.25). If contingent pattern: could degrade toward Rope or Scaffold if institutional redesign pressures were sufficiently large.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(optimal_coding_necessity, empirical, 'Whether Zipf''s Law is necessary from information theory or contingent on current system structure').

omega_variable(
    domain_boundary_definition,
    'What distinguishes systems that exhibit Zipf''s Law from those that violate it? Are the violations fundamental or measurement artifacts?',
    'Systematic survey of claimed non-Zipfian systems (e.g., biological body size distributions, company valuations in concentrated markets); verification of measurement methodology and system boundary selection',
    'If violations are genuine: accessibility_collapse should be lower (~0.75); law is contingent on domain selection. If violations are artifacts: accessibility_collapse≥0.85 is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_definition, empirical, 'Whether non-Zipfian systems represent true exceptions or measurement artifacts').

omega_variable(
    causality_direction,
    'Does Zipf''s Law govern information organization, or does information organization happen to produce Zipfian statistics as an epiphenomenon of unrelated optimization processes?',
    'Causal modeling from information theory; experimental manipulation of ranking systems to test whether power-law avoidance is possible under realistic optimization constraints',
    'If causal: Law is a true structural constraint (Mountain). If epiphenomenon of independent optimizations: Law might be better modeled as Rope (coordination without explicit extraction or control).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causality_direction, conceptual, 'Whether Zipf''s Law causes or merely describes information organization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zipfs_law, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zipf_tr_t0, zipfs_law, theater_ratio, 0, 0.12).
narrative_ontology:measurement(zipf_tr_t500, zipfs_law, theater_ratio, 500, 0.14).
narrative_ontology:measurement(zipf_tr_t1000, zipfs_law, theater_ratio, 1000, 0.15).

% Extraction over time
narrative_ontology:measurement(zipf_be_t0, zipfs_law, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(zipf_be_t500, zipfs_law, base_extractiveness, 500, 0.11).
narrative_ontology:measurement(zipf_be_t1000, zipfs_law, base_extractiveness, 1000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zipfs_law, information_standard).
narrative_ontology:affects_constraint(zipfs_law, information_entropy_bounds).
narrative_ontology:affects_constraint(zipfs_law, optimal_coding_limits).

% DUAL FORMULATION NOTE:
% Zipf's Law is upstream of many information-system constraints. Coding schemes, compression algorithms, and resource allocation mechanisms all must account for the power law. The constraint family includes: (1) Zipf's Law itself (the rank distribution invariant, ε≈0.12, Mountain), (2) optimal coding limits (information-theoretic derivation, ε≈0.08, Mountain), and (3) practical implementation of Zipfian-aware infrastructure (institutional choices in caching, indexing, database design, ε≈0.35, Rope). These are separate constraints with different ε values: the mathematical law is a Mountain; the practical deployment choices are Ropes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
