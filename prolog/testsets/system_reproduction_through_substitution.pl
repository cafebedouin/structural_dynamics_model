% ============================================================================
% CONSTRAINT STORY: system_reproduction_through_substitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_system_reproduction_through_substitution, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: system_reproduction_through_substitution
 *   human_readable: System Reproduction Through Substitution
 *   domain: organizational_ethics/systems_theory/moral_psychology
 *
 * SUMMARY:
 *   System reproduction through substitution describes the structural
 *   invariant that organizational patterns persist through individual
 *   replacement rather than individual persistence. When a CEO is replaced,
 *   the new occupant tends to reproduce the same strategic patterns despite
 *   different personality, values, or stated intentions. When a corrupt
 *   official is removed, their replacement often engages in similar corrupt
 *   practices despite entering the role with reformist commitments. When an
 *   abusive manager is fired, their successor may exhibit similar abusive
 *   behaviors despite different temperament. This is not a coordination
 *   problem or an extractive mechanism — it is a descriptive law of
 *   role-differentiated systems. The constraint operates through multiple
 *   structural channels: information asymmetries (the role-occupant has
 *   access to information others lack, shaping their perception of viable
 *   options), incentive structures (the role's reward and punishment
 *   mechanisms operate independently of occupant character), accountability
 *   asymmetries (the role's structural position determines what the occupant
 *   can be held accountable for), network effects (the role's position in
 *   communication and authority networks constrains available actions), and
 *   legitimacy requirements (the role's institutional position requires
 *   certain performances to maintain organizational coherence). These
 *   mechanisms are not contingent institutional arrangements that could be
 *   otherwise — they are emergent properties of functional differentiation in
 *   complex systems. The pattern recurs across all organizational types
 *   (corporate, governmental, non-profit, academic, military) and all
 *   cultural contexts, suggesting a structural invariant rather than a
 *   culturally-specific coordination norm.
 *
 * KEY AGENTS:
 *   - Individual Actor: Experiences immediate choices as free; cannot perceive structural reproduction at biographical timescale (powerless/trapped at immediate horizon)
 *   - Career Professional: Observes pattern persistence across colleagues but attributes to culture rather than structure (moderate/constrained at biographical horizon)
 *   - Institutional Leadership: Sees complete personnel turnover yet pattern recurrence; may naturalize as 'human nature' (institutional/arbitrage at generational horizon)
 *   - Reform Coalition: Organized effort to change outcomes through personnel replacement; discovers substitution mechanism through failure (organized/mobile at generational horizon)
 *   - Systems Theorist: Analytical observer recognizing structural invariant; sees substitution as emergent property of role differentiation (analytical/analytical at civilizational horizon)
 *   - Executive Successor: Incoming leader with reform mandate who discovers role constraints reproduce predecessor patterns (powerful/mobile at biographical horizon)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(system_reproduction_through_substitution, 0.08).
domain_priors:suppression_score(system_reproduction_through_substitution, 0.03).
domain_priors:theater_ratio(system_reproduction_through_substitution, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(system_reproduction_through_substitution, extractiveness, 0.08).
narrative_ontology:constraint_metric(system_reproduction_through_substitution, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(system_reproduction_through_substitution, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(system_reproduction_through_substitution, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(system_reproduction_through_substitution, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(system_reproduction_through_substitution, mountain).
narrative_ontology:human_readable(system_reproduction_through_substitution, "System Reproduction Through Substitution").
narrative_ontology:topic_domain(system_reproduction_through_substitution, "organizational_ethics/systems_theory/moral_psychology").

domain_priors:emerges_naturally(system_reproduction_through_substitution).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL ACTOR IMMEDIATE (MOUNTAIN) — From the immediate biographical perspective, the individual experiences their own choices as free and their role as contingent. They cannot see that their replacement would reproduce the same patterns. The structural invariance is invisible at this timescale — appears as personal agency rather than systemic constraint.
constraint_indexing:constraint_classification(system_reproduction_through_substitution, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CAREER PROFESSIONAL (MOUNTAIN) — Over a biographical timescale, the professional sees colleagues come and go but attributes pattern persistence to 'organizational culture' or 'industry norms' — still perceives it as mutable through reform efforts. The structural mechanism (role-position determines behavior more than individual character) remains obscured by the lived experience of choice.
constraint_indexing:constraint_classification(system_reproduction_through_substitution, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (MOUNTAIN) — Leadership observes complete personnel turnover across generational time yet sees the same institutional patterns recur. They may attribute this to 'human nature' or 'market forces' rather than recognizing the structural reproduction mechanism. Even with arbitrage exit options, they perceive the pattern as unchangeable because it transcends individual agency.
constraint_indexing:constraint_classification(system_reproduction_through_substitution, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (MOUNTAIN) — Organized reformers with generational perspective observe that replacing 'bad actors' with 'good people' fails to change outcomes. They recognize the pattern but may still frame it as a coordination problem solvable through better incentive design, missing that the substitution mechanism is a structural invariant — the system reproduces itself through role-position constraints that operate regardless of occupant character.
constraint_indexing:constraint_classification(system_reproduction_through_substitution, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From the analytical civilizational perspective, system reproduction through substitution is a structural invariant of complex organizations. The pattern is not a policy choice or cultural artifact but an emergent property of role-differentiated systems: positions shape behavior through information access, incentive structures, accountability asymmetries, and social network effects that operate independently of occupant identity. This is not extractive — it is a descriptive law of organizational dynamics.
constraint_indexing:constraint_classification(system_reproduction_through_substitution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: EXECUTIVE SUCCESSOR (MOUNTAIN) — The incoming executive with reform mandate discovers that their structural position constrains their choices in ways their predecessor's position constrained theirs. The 'fresh perspective' they brought dissipates as role demands, information flows, and stakeholder pressures reproduce the previous pattern. High power and mobility do not overcome the structural reproduction mechanism — they experience it as an unchangeable feature of the role itself.
constraint_indexing:constraint_classification(system_reproduction_through_substitution, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(system_reproduction_through_substitution_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(system_reproduction_through_substitution, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(system_reproduction_through_substitution, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(system_reproduction_through_substitution, ExtMetricName, E),
    domain_priors:suppression_score(system_reproduction_through_substitution, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(system_reproduction_through_substitution),
    narrative_ontology:constraint_metric(system_reproduction_through_substitution, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(system_reproduction_through_substitution, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(system_reproduction_through_substitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint describes a structural property of complex organizations, not an extraction mechanism. The minimal extractiveness reflects only the cognitive and emotional cost of recognizing that individual agency is more constrained than subjective experience suggests — the 'cost' of discovering that replacing people does not change outcomes. This is not extraction in the sense of asymmetric resource transfer but rather the epistemic cost of pattern recognition. Suppression (0.03): Minimal. There are no significant barriers to observing the substitution pattern — it is empirically accessible through longitudinal organizational studies, successor case studies, and cross-cultural comparison. The low suppression reflects that the pattern is not hidden by active enforcement but rather by the phenomenological gap between individual experience (choice feels free) and structural observation (patterns recur regardless of occupant). Accessibility collapse (0.92): Very high. Once the substitution mechanism is recognized, it is extremely difficult to unsee. The pattern is robust across organizational types, cultural contexts, and historical periods. Resistance (0.08): Very low. Attempts to overcome the substitution mechanism through personnel selection, training, or incentive design consistently fail because the mechanism operates at the structural level of role-position constraints, not at the individual level of character or motivation. Theater ratio (0.15): Low. Organizational responses to pattern recurrence (ethics training, leadership development, succession planning) have some genuine function (they may reduce variance in how the pattern manifests) but cannot eliminate the underlying substitution mechanism. The theater component reflects the gap between stated reform goals (changing outcomes through better people) and structural reality (outcomes are determined by role-position constraints).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits mountain classification from all perspectives because the substitution mechanism is a structural invariant, not a contingent institutional arrangement. The perspectival variation is in RECOGNITION rather than CLASSIFICATION. The individual actor at immediate timescale cannot perceive the pattern (experiences choices as free). The career professional at biographical timescale observes pattern persistence but misattributes cause (sees culture, not structure). The institutional leadership at generational timescale sees complete turnover yet pattern recurrence but may naturalize it (human nature, market forces). The reform coalition discovers the mechanism through failure (replacing people does not change outcomes). The analytical observer recognizes it as an emergent property of role differentiation. The executive successor experiences it directly (role constraints reproduce predecessor patterns despite different intentions). All perspectives converge on mountain classification because the constraint is genuinely unchangeable at the level of individual agency — it can only be addressed through structural redesign that eliminates or transforms the role-positions themselves, which is typically infeasible for functional organizations.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries or victims in the conventional sense because it is not an extraction mechanism. The substitution pattern operates symmetrically across all role-occupants regardless of their structural position. A CEO and a line worker both experience their choices as constrained by role-position, though the content of the constraints differs. The analytical observer's directionality (d ≈ 0.72) reflects the epistemic position of studying the pattern rather than experiencing it from within a role. There are no directionality overrides because there is no asymmetric extraction to model — the constraint describes a structural property of organizations, not a resource transfer mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN CERTIFICATION: This constraint is a genuine mountain, not a naturalized extraction mechanism. The substitution pattern is not a contingent institutional arrangement that serves extractive interests — it is an emergent property of functional differentiation in complex systems. The pattern recurs across all organizational types (corporate, governmental, non-profit, academic, military), all cultural contexts (Western, Eastern, indigenous), and all historical periods (ancient bureaucracies, medieval guilds, modern corporations), which is the signature of a structural invariant rather than a culturally-specific coordination norm or an extractive mechanism disguised as natural law. The constraint can be distinguished from false summits through three tests: (1) Cross-cultural invariance — the pattern appears in organizational forms that developed independently with no cultural contact. (2) Functional necessity — eliminating role differentiation eliminates the organization's capacity to perform its function (a hospital cannot operate without role-specialized positions; those positions will constrain occupant behavior regardless of who fills them). (3) Resistance to reform — thousands of years of organizational reform efforts across all cultures have failed to eliminate the substitution mechanism, suggesting it is a structural limit rather than a solvable coordination problem. The constraint is downstream of two extractive constraints (role_capture_through_cost_asymmetry and brilliance_as_structural_liability) but is itself non-extractive — those upstream constraints exploit the substitution mechanism but do not create it. The substitution mechanism would operate even in the absence of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(system_reproduction_through_substitution, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(system_reproduction_through_substitution, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of role_capture_through_cost_asymmetry (tangled_rope, ε=0.55) and brilliance_as_structural_liability (snare, ε=0.68). Those upstream constraints are extractive mechanisms that exploit the substitution pattern described here. The substitution mechanism itself is non-extractive (mountain, ε=0.08) — it is a structural invariant that extractive constraints leverage. The network relationship is: extractive role dynamics (upstream) exploit the substitution mechanism (this constraint) to persist across personnel changes. Eliminating the extractive dynamics would not eliminate the substitution mechanism, but eliminating the substitution mechanism (through complete elimination of role differentiation) would eliminate the extractive dynamics' persistence mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
