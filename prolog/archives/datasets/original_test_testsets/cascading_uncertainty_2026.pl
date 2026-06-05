% ============================================================================
% CONSTRAINT STORY: cascading_uncertainty_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cascading_uncertainty_2026, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cascading_uncertainty_2026
 *   human_readable: The Sovereignty Gap (Cascading Uncertainty)
 *   domain: political/social/geopolitical
 *
 * SUMMARY:
 *   As of February 2026, the simultaneous expiration of the New START treaty
 *   and the intensification of a domestic enforcement crisis have created a
 *   'sovereignty gap.' This gap is a structural condition where the erosion
 *   of both international arms control norms and domestic rule of law creates
 *   cascading uncertainty. The constraint is not a specific rule, but the
 *   *enforced absence of rules*, which fundamentally alters the risk calculus
 *   for all actors, from individuals to nation-states.
 *
 * KEY AGENTS:
 *   - Global Civil Society: Primary victim (powerless/trapped) — bears the cost of decreased global security and stability.
 *   - Geopolitical Revisionists: Primary beneficiary (institutional/arbitrage) — exploits the lack of rules to advance strategic goals.
 *   - Allied Nations: Secondary victim (organized/constrained) — security is degraded, but retains some policy agency.
 *   - Defense Contractors: Secondary beneficiary (organized/mobile) — profits from the resulting arms race and instability.
 *   - Analytical Observer: Sees the full structure of collapsed coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cascading_uncertainty_2026, 0.68).
domain_priors:suppression_score(cascading_uncertainty_2026, 0.8).
domain_priors:theater_ratio(cascading_uncertainty_2026, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cascading_uncertainty_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(cascading_uncertainty_2026, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(cascading_uncertainty_2026, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cascading_uncertainty_2026, tangled_rope).
narrative_ontology:human_readable(cascading_uncertainty_2026, "The Sovereignty Gap (Cascading Uncertainty)").
narrative_ontology:topic_domain(cascading_uncertainty_2026, "political/social/geopolitical").

domain_priors:requires_active_enforcement(cascading_uncertainty_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cascading_uncertainty_2026, geopolitical_revisionists).
narrative_ontology:constraint_beneficiary(cascading_uncertainty_2026, defense_contractors).
narrative_ontology:constraint_beneficiary(cascading_uncertainty_2026, authoritarian_regimes).
narrative_ontology:constraint_victim(cascading_uncertainty_2026, global_civil_society).
narrative_ontology:constraint_victim(cascading_uncertainty_2026, allied_nations_under_nuclear_umbrella).
narrative_ontology:constraint_victim(cascading_uncertainty_2026, domestic_rule_of_law).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL CIVIL SOCIETY (SNARE) — Trapped in a new, more dangerous global environment. The extraction of security, stability, and predictability is total, with no recourse or exit. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.16. This is a clear Snare.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GEOPOLITICAL REVISIONIST (ROPE) — Experiences the collapse of the rules-based order as liberation. The absence of treaties is a coordination good, enabling greater freedom of action to reshape the global order. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.098. The constraint is a net subsidy.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ALLIED NATION (TANGLED ROPE) — Experiences both the loss of a global coordination good (strategic stability) and the direct extraction of its own security. It is constrained by alliance structures and geography but retains some agency. d≈0.65, f(d)≈1.00, σ=0.9 → χ≈0.61. This falls squarely in the Tangled Rope category.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the full structure: the collapse of a prior coordination mechanism (arms control) has created a new reality defined by high, asymmetric extraction of security. The system has both coordination-failure and extraction components. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.94. This is a high-chi Tangled Rope, bordering on Snare.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: THE REALIST VIEW (MOUNTAIN) — This perspective naturalizes the constraint, viewing the collapse of rules as a return to the inevitable, anarchic state of great power competition. It frames a contingent political failure as a fixed law of geopolitics. The engine will identify this as a false summit, as the base properties (ε=0.68, suppression=0.80) are inconsistent with a Mountain classification.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cascading_uncertainty_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cascading_uncertainty_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cascading_uncertainty_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cascading_uncertainty_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cascading_uncertainty_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.68) is high because the constraint extracts the public good of 'security' and 'predictability' from the global commons and transfers strategic advantage to a few actors. Suppression (0.80) is very high, as the political will and trust required to build a new treaty regime (the alternative) have been systematically dismantled. Theater (0.35) is moderate; diplomatic rituals continue, but they are increasingly decoupled from the reality of unconstrained military competition.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For revisionist powers, the lack of constraints is a liberating coordination good (Rope). For the global populace and nations reliant on the old order, it is a coercive trap that extracts their security (Snare). For allies caught in the middle, it is a dysfunctional system with remnants of coordination but dominated by extraction (Tangled Rope). The 'Realist' perspective attempts to naturalize this situation as an immutable law of physics (Mountain), but the high extraction and suppression metrics reveal this to be a false summit—a political choice, not an inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Geopolitical revisionists are beneficiaries with arbitrage exit, yielding a low 'd' and negative effective extraction (χ < 0). Global civil society is a victim with trapped exit, yielding a high 'd' and extremely high positive χ, classifying as a Snare. Allied nations are victims but with constrained exit options, placing their derived 'd' and resulting χ in the Tangled Rope category. The analytical observer's canonical 'd' also results in a Tangled Rope classification, which serves as the system's overall claim.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves a key mandatrophy. A naive analysis might label the situation simply as 'anarchy' (a Mountain) or 'great power competition' (a neutral framing). The DR framework disambiguates this by quantifying the immense, asymmetric extraction of security from the powerless. It demonstrates that the 'freedom of action' for the powerful (a Rope from their perspective) is structurally identical to the coercive extraction experienced by the trapped (a Snare from theirs). The system is not neutral; it is a highly extractive Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    china_participation_calculus,
    'Is China''s refusal to join trilateral arms control a structural feature of its rise, or a contingent policy choice that could be reversed by diplomatic or economic pressure?',
    'Analysis of Chinese strategic documents; backchannel diplomatic engagement; observing reactions to US/Russian arsenal changes.',
    'If structural, the sovereignty gap is a long-term Mountain-like feature. If contingent, a new Rope or Tangled Rope (a new treaty) is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_participation_calculus, empirical, 'Whether China''s arms control stance is a fixed or flexible policy.').

omega_variable(
    domestic_crisis_linkage,
    'Is the domestic enforcement crisis an independent phenomenon, or is it causally linked to geopolitical instability (e.g., leaders manufacturing a crisis to consolidate power)?',
    'Correlational analysis between geopolitical tension indicators and domestic policy shifts; tracing funding and rhetoric from external actors.',
    'If linked, the constraint is a more coherent, intentional Snare. If independent, it''s a confluence of two separate system failures, making it a more chaotic Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_crisis_linkage, empirical, 'Causal link between the domestic and international crises.').

omega_variable(
    escalation_pathway_inevitability,
    'Does the absence of formal treaties inevitably lead to an arms race and conflict, or can informal norms and deterrence provide a stable (if risky) new equilibrium?',
    'Game-theoretic modeling of multi-polar deterrence; historical analysis of pre-treaty eras; monitoring of arsenal development and deployment postures.',
    'If inevitable, the Snare perspective is the most accurate long-term description. If a stable equilibrium is possible, the situation may evolve into a high-tension Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_pathway_inevitability, conceptual, 'Whether the absence of treaties guarantees conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cascading_uncertainty_2026, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(casc_tr_t0, cascading_uncertainty_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(casc_tr_t3, cascading_uncertainty_2026, theater_ratio, 3, 0.3).
narrative_ontology:measurement(casc_tr_t5, cascading_uncertainty_2026, theater_ratio, 5, 0.35).

% Extraction over time
narrative_ontology:measurement(casc_be_t0, cascading_uncertainty_2026, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(casc_be_t3, cascading_uncertainty_2026, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(casc_be_t5, cascading_uncertainty_2026, base_extractiveness, 5, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(cascading_uncertainty_2026, global_economic_stability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
