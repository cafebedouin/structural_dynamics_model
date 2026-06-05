% ============================================================================
% CONSTRAINT STORY: selection_pressure_architecture
% ============================================================================
% Version: 7.0-json (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-03-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_selection_pressure_architecture, []).

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
 *   constraint_id: selection_pressure_architecture
 *   human_readable: Selection Pressure as Architecture (Optimization Environments Select Against Verification)
 *   domain: epistemology/information_theory/institutional
 *
 * SUMMARY:
 *   Any environment optimized for signal propagation systematically selects
 *   for accessibility over truth-value, regardless of the intent or quality
 *   of individual actors within it. This is not a claim about bad actors or
 *   poor design — it is a structural property of optimization itself. When
 *   the objective function is not truth-value (and truth-value is notoriously
 *   difficult to formalize as an objective), the optimization process will
 *   find paths that maximize the actual objective while being orthogonal to
 *   or inversely correlated with epistemic accuracy. This applies to AI
 *   training pipelines (optimizing prediction accuracy over available data,
 *   not truth), social media platforms (optimizing engagement, not accuracy),
 *   prestige journalism (optimizing access and audience, not structural
 *   understanding), and academic publishing (optimizing citation count and
 *   novelty, not replication). The mountain claim is that this property is
 *   inherent to optimization, not an artifact of any particular
 *   implementation.
 *
 * KEY AGENTS:
 *   - Information consumer (powerless/trapped): receives outputs shaped by selection pressure; cannot alter the structural relationship between optimization and truth-value
 *   - Platform operator (institutional/constrained): designs optimization environments but faces the mathematical difficulty of formalizing truth-value as an objective; Goodhart pressure applies to any proxy
 *   - Reform coalition (organized/constrained): can shift which objective is optimized, but cannot eliminate the structural property that optimization selects for its objective
 *   - Epistemologist (analytical/analytical): sees the invariance across domains; the structural property is a theorem about optimization, not a contingent fact about platforms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(selection_pressure_architecture, 0.18).
domain_priors:suppression_score(selection_pressure_architecture, 0.03).
domain_priors:theater_ratio(selection_pressure_architecture, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(selection_pressure_architecture, extractiveness, 0.18).
narrative_ontology:constraint_metric(selection_pressure_architecture, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(selection_pressure_architecture, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(selection_pressure_architecture, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(selection_pressure_architecture, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(selection_pressure_architecture, mountain).
narrative_ontology:human_readable(selection_pressure_architecture, "Selection Pressure as Architecture (Optimization Environments Select Against Verification)").
narrative_ontology:topic_domain(selection_pressure_architecture, "epistemology/information_theory/institutional").

domain_priors:emerges_naturally(selection_pressure_architecture).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual receiving outputs of optimization environments (social media feeds, news, AI outputs). The selection pressure is invisible and structurally unalterable from this position. Classified mountain: no achievable action changes the structural relationship between optimization objectives and truth-value.
constraint_indexing:constraint_classification(selection_pressure_architecture, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Institutional actor designing or operating an optimization environment. Even an operator who wants to optimize for truth-value faces the mountain: truth is harder to measure than engagement, accuracy has longer feedback cycles than virality, and any proxy metric for truth is itself subject to Goodhart pressure. The constraint is mountain even for the operator because the difficulty is mathematical, not volitional.
constraint_indexing:constraint_classification(selection_pressure_architecture, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Analytical observer seeing the structural property of optimization itself. At universal scope, this is a theorem about optimization: any objective function that is not truth-value will select for non-truth-value. The constraint is invariant across all optimization domains — AI training, social media amplification, prestige journalism, academic incentives, political rhetoric.
constraint_indexing:constraint_classification(selection_pressure_architecture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Organized group attempting to redesign information environments for epistemic quality. Faces the mountain: can change WHICH objective is optimized for, but cannot change the structural property that optimization selects for its objective. Any new objective introduces new Goodhart vulnerabilities. Coalition can shift the problem, not eliminate it.
constraint_indexing:constraint_classification(selection_pressure_architecture, mountain,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(selection_pressure_architecture_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(selection_pressure_architecture, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(selection_pressure_architecture, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(selection_pressure_architecture, ExtMetricName, E),
    domain_priors:suppression_score(selection_pressure_architecture, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(selection_pressure_architecture),
    narrative_ontology:constraint_metric(selection_pressure_architecture, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(selection_pressure_architecture, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(selection_pressure_architecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.18 — below the mountain ceiling of 0.25. The constraint extracts from no specific agent; it is a structural property of environments. The 'cost' is borne diffusely by anyone relying on optimization-environment outputs for epistemic purposes, but this is analogous to how entropy 'costs' ordered systems — no agent extracts, no beneficiary exists. Suppression at 0.03 reflects that the constraint does not actively prevent alternatives; it is simply the case that optimization works this way. Theater ratio at 0.10 reflects that the selection pressure is genuine, not performative. Accessibility collapse at 0.92: the constraint is nearly invisible to those inside optimization environments — the outputs look authoritative precisely because they were selected for looking authoritative. Resistance at 0.08: extremely low resistance to the claim's truth; the mathematical structure of optimization is well-established.
 *
 * PERSPECTIVAL GAP:
 *   No perspectival gap — this is a uniform-type mountain. All four perspectives classify as mountain because the structural property of optimization is invariant across power positions, time horizons, and exit options. The platform operator might appear to have more agency (could redesign the objective function), but even redesign faces the same mountain: the new objective introduces new Goodhart vulnerabilities. The organized reform coalition can shift the locus of the problem but not eliminate it. The uniformity is itself diagnostic: when every perspective sees the same classification, the constraint is likely structural rather than positional.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary/victim declarations. Mountains do not have structural beneficiaries or victims — the constraint is a property of environments, not a relationship between agents. Downstream axes (feedback_suppression_tangled_rope, hyperstition_snare) instantiate the specific agent relationships where extraction occurs. The mountain establishes the structural condition; the downstream axes identify who benefits and who bears costs within specific institutional implementations.
 *
 * MANDATROPHY ANALYSIS:
 *   Mountain classification prevents two mandatrophy errors: (1) Treating the selection pressure as a snare implies an extracting agent who could be held accountable — but the constraint operates without any agent's intent. Removing Mark Zuckerberg does not change the mathematics of optimization. (2) Treating the selection pressure as a rope implies it could be redesigned to serve all parties — but the Goodhart problem means any truth-proxy objective will eventually be gamed. The mountain classification correctly identifies the constraint as structural rather than volitional, directing corrective attention to downstream institutional implementations rather than to the impossible task of 'fixing optimization.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_mountain_validity,
    'Is the selection pressure a true mountain (thermodynamic — any optimization environment must select for its objective over non-objectives) or a piton of current platform design choices that could be redesigned?',
    'Counterfactual platform analysis: if platforms were redesigned to optimize for truth-value (e.g., prediction markets, adversarial verification systems), does the selection-against-verification pressure persist in a different form (Goodhart on truth proxies), or does it genuinely resolve? If it persists, mountain confirmed. If it resolves, reclassify as piton or rope of current platform architecture.',
    'If piton: reclassifies to piton with theater_ratio ≥ 0.70 (the ''inevitability'' framing is itself theatrical). Downstream axes retain their classifications but lose the thermodynamic grounding — feedback suppression becomes a design failure rather than a structural inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_mountain_validity, empirical, 'Thermodynamic vs. design-contingent selection pressure').

omega_variable(
    omega_scope_invariance,
    'Does the selection pressure operate identically across all optimization domains (AI training, social media, journalism, academia, politics), or do domain-specific structures modulate it enough to break the universal claim?',
    'Cross-domain measurement: compare divergence rates between propagation success and ground-truth accuracy across AI training, social media, prestige journalism, and academic publishing. If divergence rates differ by more than one order of magnitude across domains, the universal mountain claim is too strong — decompose into domain-specific stories.',
    'If domain-specific: decompose into separate constraint stories (selection_pressure_ai_training, selection_pressure_social_media, etc.) linked by network.affects_constraints. The general principle becomes a commentary note, not a constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_scope_invariance, empirical, 'Cross-domain invariance of selection pressure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(selection_pressure_architecture, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(selection_pressure_architecture, feedback_suppression_tangled_rope).
narrative_ontology:affects_constraint(selection_pressure_architecture, hyperstition_snare).
narrative_ontology:affects_constraint(selection_pressure_architecture, bureaucratic_ego_rope).

% DUAL FORMULATION NOTE:
% This mountain is the structural source node for a three-axis constraint family. The general principle (optimization selects for its objective) generates specific institutional failure modes: feedback suppression (success ecology dismantles error-correction), hyperstition (propagation consumes verification), and bureaucratic ego (documentary consistency without experiential continuity). Each downstream axis has its own ε, its own beneficiary/victim structure, and its own classification. The mountain does not extract — the downstream axes do.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
