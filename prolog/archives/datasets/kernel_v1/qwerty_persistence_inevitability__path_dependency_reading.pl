% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__path_dependency_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence_inevitability__path_dependency_reading
 *   human_readable: QWERTY Persistence as Accident-Driven Path Dependency
 *   domain: technology_history/institutional_analysis
 *
 * SUMMARY:
 *   The QWERTY keyboard layout persists as the dominant global standard not
 *   because manufacturers engineered lock-in through cartel coordination, but
 *   because of accident-driven path dependency: Christopher Latham Sholes'
 *   typewriter won early market dominance, training accumulated in QWERTY,
 *   and network effects created a coordination equilibrium. Once the
 *   equilibrium crystallized, switching to an alternative layout (Dvorak,
 *   Colemak) became individually irrational despite potential aggregate
 *   efficiency gains. This reading treats QWERTY persistence as a natural law
 *   of coordination under increasing returns — a mountain rather than a
 *   manufactured snare. The constraint exhibits zero extraction (no
 *   beneficiary captures value from QWERTY dominance; manufacturers respond
 *   to demand, they don't engineer it), minimal suppression (the constraint
 *   operates through coordination equilibrium, not coercion), and low theater
 *   (the mechanism is straightforward: installed base + trained labor +
 *   compatibility requirements). This is ONE reading of a contested kernel:
 *   the strategic lock-in reading argues that manufacturers did intentionally
 *   reinforce QWERTY through deliberate partnerships and cartel
 *   standardization. The two readings disagree about whether lock-in is
 *   natural (path dependency) or constructed (strategic). This constraint
 *   story instantiates the path dependency reading cleanly and documents the
 *   sibling reading's core claims as omega variables.
 *
 * KEY AGENTS:
 *   - Christopher Latham Sholes / Remington: Historical contingency — Sholes' typewriter design happened to be mechanically successful and achieved early market dominance; no strategic intent required
 *   - Typists (accumulated human capital): Bear the switching cost of staying in QWERTY equilibrium; diffuse population with no coherent collective organization
 *   - Typewriter / keyboard manufacturers (collective): Respond to demand for trained typists and compatibility; no coordinated lock-in mechanism in path dependency reading (contrast with strategic lock-in sibling)
 *   - Alternative layout designers (Dvorak, et al.): Attempted to break the equilibrium but faced insurmountable coordination problems; no victim status because they could exit by specializing in niche domains
 *   - Analytical observer: Sees the entire mechanism as an inevitable consequence of network economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.08).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.02).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Persistence as Accident-Driven Path Dependency").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology_history/institutional_analysis").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, 'be6d48ad-e541-43a2-84b6-5982c0d669f4').
narrative_ontology:cs_kernel_codification('be6d48ad-e541-43a2-84b6-5982c0d669f4', distributed).
narrative_ontology:cs_authority_grounding('be6d48ad-e541-43a2-84b6-5982c0d669f4', distributed).
narrative_ontology:cs_reading_relation('be6d48ad-e541-43a2-84b6-5982c0d669f4', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('be6d48ad-e541-43a2-84b6-5982c0d669f4', foundational, lock_in_purely_accidental_no_strategic_intent).
narrative_ontology:cs_axiom_status(lock_in_purely_accidental_no_strategic_intent, holdable).
narrative_ontology:cs_axiom_grounding('be6d48ad-e541-43a2-84b6-5982c0d669f4', lock_in_purely_accidental_no_strategic_intent, empirically_contingent).
narrative_ontology:cs_axiom('be6d48ad-e541-43a2-84b6-5982c0d669f4', foundational, network_effects_irreversible_given_installed_base).
narrative_ontology:cs_axiom_status(network_effects_irreversible_given_installed_base, holdable).
narrative_ontology:cs_axiom_grounding('be6d48ad-e541-43a2-84b6-5982c0d669f4', network_effects_irreversible_given_installed_base, empirically_contingent).
narrative_ontology:cs_created_at('be6d48ad-e541-43a2-84b6-5982c0d669f4', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATH DEPENDENCY ANALYST (MOUNTAIN) — From a civilizational perspective examining technological lock-in mechanisms, QWERTY persistence appears as an immutable consequence of network effects and switching costs once initial equilibrium is reached. No agent deliberately maintains the constraint; it persists through accumulated contingency. The analyst sees the mechanism as a natural law of coordination under increasing returns.
constraint_indexing:constraint_classification(qwerty_persistence_inevitability__path_dependency_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 2: TYPIST (MOUNTAIN) — An individual typist at biographical time horizon experiences QWERTY as an unchangeable fact of their environment. Training in QWERTY is costly; switching to an alternative layout would require relearning muscle memory and abandoning compatibility with existing systems. The constraint appears as an immutable natural limit on their exit options — not because of strategic coercion, but because the coordination equilibrium has hardened around the historical accident.
constraint_indexing:constraint_classification(qwerty_persistence_inevitability__path_dependency_reading, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: TYPEWRITER MANUFACTURER (MOUNTAIN) — A manufacturer in the 1890s-1920s could theoretically choose an alternative layout, but the economics of compatibility create a hard constraint. Any manufacturer deviating from QWERTY would face reduced demand (trained typists, existing documents), incompatible supply chains, and market isolation. The constraint operates not through active enforcement by competitors but through the crystallized expectations embedded in installed base and human capital. The manufacturer experiences this as a natural limit on viable product design, not as intentional lock-in by others.
constraint_indexing:constraint_classification(qwerty_persistence_inevitability__path_dependency_reading, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: GENERATIONAL INDUSTRY COORDINATION (ROPE) — From a generational timescale, the industry collectively solved a real coordination problem: which keyboard layout should everyone standardize on? The fact that QWERTY was the solution is historically contingent (a specific mechanical typewriter won market dominance first), but the need for coordination is not contingent. From this perspective, QWERTY is pure coordination — the constraint solved a genuine collective action problem. No extraction, no suppression, no alternative paths explored actively.
constraint_indexing:constraint_classification(qwerty_persistence_inevitability__path_dependency_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: NATURAL LAW / NETWORK EFFECTS (MOUNTAIN) — From the perspective of formal network effects analysis and increasing returns economics, QWERTY persistence is a mathematical consequence of the coordination problem, not a specific institutional choice. The underlying mechanism — that abandoning a dominant equilibrium requires simultaneous coordination of producers and consumers — is a structural property of network goods. QWERTY is the canonical exemplar precisely because it demonstrates that technological lock-in can occur without strategic intent. The persistence is necessary given the initial conditions.
constraint_indexing:constraint_classification(qwerty_persistence_inevitability__path_dependency_reading, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(qwerty_persistence_inevitability__path_dependency_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qwerty_persistence_inevitability__path_dependency_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, ExtMetricName, E),
    domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. In the path dependency reading, no agent extracts surplus from QWERTY dominance. Manufacturers compete on quality and cost, not on lock-in rents. Typists receive training in QWERTY because that's what the market demands, not because they're being systematically overcharged. The efficiency loss from QWERTY (vs. Dvorak) is a diffuse dead weight, not extracted value. The low value reflects that this is pure coordination (rope-level) with negligible extraction overlay. Suppression (0.02): Negligible. The constraint operates through incentive alignment (compatible with existing systems), not through coercion or suppression of alternatives. Dvorak layout is not forbidden; it simply cannot achieve critical mass given the installed base and switching costs. No agent actively suppresses Dvorak — it fails due to coordination problems, not deliberate prevention. Theater ratio (0.15): Low. The mechanism is functionally transparent: early market dominance → accumulated training → network effects → coordination lock. There is no performative ritual maintaining QWERTY; the constraint is maintained by ordinary market operations. The slight theater (0.15 rather than 0.0) reflects that some framing of QWERTY as 'the standard' involves narrative reinforcement, but this is minimal compared to genuine theater-driven constraints.
 *
 * PERSPECTIVAL GAP:
 *   DIAGNOSTIC SIGNAL: This constraint is classified as mountain from all five perspectives despite differing power levels and exit options. Typists and manufacturers and analysts all see an unchangeable technical constraint. This uniformity is unusual — most constraints produce perspectival gaps precisely because different agents experience extraction differently. The uniformity here is evidence for the path dependency reading: the constraint operates symmetrically, not asymmetrically. If the strategic lock-in sibling reading were correct, we would expect manufacturers to experience rope (they benefit from lock-in) and typists to experience snare (they bear switching costs). The absence of that gap suggests path dependency (pure coordination) rather than strategic lock-in (mixed extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   This path dependency reading contains no beneficiary or victim set because the constraint operates symmetrically: all actors (manufacturers, typists, alternative designers) face the same coordination problem. Manufacturers face demand for QWERTY-compatible machines; they supply them. Typists need compatibility with documents and other workers; QWERTY provides it. Alternative designers cannot achieve critical mass; they exit gracefully into niche domains. No agent experiences directional extraction because the constraint solves a genuine coordination problem that all parties share. From all perspectives, QWERTY is experienced as an immutable constraint of the coordination landscape — a mountain — because the exit cost of coordinating on an alternative is prohibitive for everyone. The analytical observer sees this as natural law; the affected agents see it as unchangeable fact. This uniformity across perspectives is diagnostic: when all observers classify a constraint identically regardless of power or exit options, the constraint is typically a genuine mountain. Contrast with the strategic lock-in sibling reading, which would produce a perspectival gap: manufacturers would experience rope (beneficiary), typists would experience snare (victim), and the analytical observer would expose the extraction mechanism. The absence of that perspectival gap in this reading is evidence for the path dependency thesis.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY ISSUE: This constraint's extractiveness (0.08) is below the 0.46 threshold that triggers mandatrophy analysis. The path dependency reading classifies the constraint as a mountain consistently across all perspectives, indicating no hidden coordination-extraction ambiguity that would require resolving. The constraint is epistemically simple: it's a natural law of network coordination. The only irreducible uncertainty is WHETHER the natural law reading is correct (i.e., whether the strategic lock-in sibling reading is the true reading) — that ambiguity is documented in omega variables, not mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_alternative_dominance,
    'If Sholes & Glidden had failed commercially and a different manufacturer with Dvorak-compatible machines had achieved early dominance, would path dependency lock-in that alternative layout equally permanently?',
    'Historical counterfactual analysis using complexity theory; simulation of network effects under different initial conditions; analysis of keyboard layout changes in specialized domains (Dvorak adoption in niche communities, Colemak diffusion)',
    'If yes: QWERTY is purely accident-driven (this reading holds). If no: some layouts have inherent coordination advantages or manufacturability advantages (undermines pure path dependency thesis; moves closer to strategic lock-in reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_alternative_dominance, conceptual, 'Whether alternative early dominance would lock in an alternative layout').

omega_variable(
    intentional_standardization_vs_emergent_equilibrium,
    'Did manufacturers actively lobby for QWERTY standardization, or did standardization emerge from market forces selecting Sholes & Glidden dominance?',
    'Historical archival analysis: manufacturer correspondence, industry association records, patent licensing agreements; chronological mapping of when standardization language appears in manufacturer marketing vs internal communications',
    'If active lobbying: evidence favors strategic lock-in reading (sibling constraint). If emergent: evidence favors path dependency reading (this constraint). Most likely: mixed timeline with early emergence followed by later active preservation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intentional_standardization_vs_emergent_equilibrium, empirical, 'Degree of intentional coordination vs emergent market selection in QWERTY dominance').

omega_variable(
    diffuse_vs_concentrated_efficiency_loss,
    'Who bears the cost of QWERTY persistence — is the efficiency loss diffuse (every typist loses marginal productivity) or concentrated (specific communities or industries)?',
    'Productivity comparison: QWERTY vs Dvorak in controlled typing studies; measurement of switching costs by community (office workers, journalists, programmers); analysis of whether any coherent victim group exists that could organize resistance',
    'If diffuse: no victim set, no extraction mechanism, pure path dependency (this reading). If concentrated: identifiable victim group exists; potential for organized resistance; moves toward mixed coordination-extraction (tangled_rope or snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_vs_concentrated_efficiency_loss, empirical, 'Concentration vs diffusion of efficiency loss from QWERTY persistence').

omega_variable(
    natural_law_vs_constructed_inevitability,
    'Is QWERTY persistence a true natural law of coordination (logically necessary given network effects), or a contingent institutional outcome that appears inevitable in retrospect?',
    'Comparative analysis of lock-in across different technologies: examine cases where path-dependent lock-in was broken (e.g., VHS→DVD→streaming, floppy disk→USB, regional power standards → slow convergence); identify structural conditions that distinguish reversible from irreversible lock-in',
    'If true natural law: mountain classification holds; false summit detection should NOT trigger. If contingent: may indicate that the mountain classification naturalizes what is actually an institutional arrangement; false summit detection may apply if the strategic lock-in sibling reading demonstrates that manufacturers did intentionally reinforce QWERTY dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_inevitability, conceptual, 'Logical necessity vs contingent institutional construction of QWERTY inevitability').

omega_variable(
    reading_dominance_in_technology_studies,
    'Which reading (path dependency vs strategic lock-in) dominates contemporary technology studies and economics scholarship, and is that dominance empirically justified or paradigmatically enforced?',
    'Citation analysis and paradigm mapping: survey of textbooks, review articles, and foundational papers; analysis of which evidence scholars emphasize and which they dismiss; examination of whether strategic lock-in evidence is treated as equally credible as path dependency evidence',
    'If path dependency dominates justified by evidence: this reading reflects scholarly consensus. If strategic lock-in evidence is systematically discounted: indicates potential paradigm lock (institutional inertia in scholarship itself, parallel to QWERTY lock in technology). If reading dominance reflects author positions rather than evidence: reveals that the kernel contest itself is partly a dispute over how to interpret historical facts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_dominance_in_technology_studies, conceptual, 'Scholarly consensus vs paradigmatic enforcement in reading selection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 1870, 1970).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_pd_tr_t0, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qwerty_pd_tr_t10, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(qwerty_pd_tr_t20, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(qwerty_pd_be_t0, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(qwerty_pd_be_t10, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 10, 0.08).
narrative_ontology:measurement(qwerty_pd_be_t20, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 20, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability__strategic_lock_in_reading).

% DUAL FORMULATION NOTE:
% The QWERTY persistence kernel has two structurally distinct readings. PATH_DEPENDENCY_READING (this constraint): ε=0.08, no extraction, mountain classification — pure coordination under increasing returns. STRATEGIC_LOCK_IN_READING (sibling): ε=0.52, extraction by coordinated manufacturers, tangled_rope classification — mixed coordination and deliberate lock-in. These are not two views of one constraint; they are two distinct constraints sharing a kernel. The epsilon values differ by a factor of six. Authors must choose which reading to instantiate in each constraint story. Network link marks them as members of the same kernel dispute; compile both stories to fully represent the contested claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
