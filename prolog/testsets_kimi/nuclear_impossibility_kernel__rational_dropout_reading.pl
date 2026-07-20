% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__rational_dropout_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Rational Dropout Constraint
 *   domain: strategic/international_relations
 *
 * SUMMARY:
 *   The rational_dropout_reading of the nuclear_impossibility_kernel holds
 *   that nuclear weapons do not make war physically impossible, but impose a
 *   rational-choice constraint: the costs of nuclear war exceed any
 *   conceivable benefit, causing rational actors to drop it from active
 *   consideration. War remains in the reachable setâstrategic debates and
 *   war-gaming confirm its conceptual accessibilityâbut the cost-benefit
 *   calculus renders it a structurally dominated option. This reading differs
 *   from the structural_contraction_reading (which claims physical
 *   impossibility of victory) and the credibility_paradox_reading (which
 *   focuses on the performative tension between deterrence and use). The
 *   constraint is treated here as a feature of the strategic landscape: an
 *   emergent property of nuclear destructiveness interacting with rational
 *   agency, not an enforced coordination mechanism.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Primary subjects (institutional/powerful) â constrained by the cost structure but retain launch capability and doctrinal flexibility
 *   - civilian_populations: Ultimate stake (powerless/universal) â bear the cost that creates the constraint without agency in maintaining it
 *   - strategic_theorists: Analytical observers (analytical) â model and propagate the rational dropout logic through game-theoretic and strategic analysis
 *   - non_nuclear_weapon_states: Excluded observers (moderate/global) â live under the constraint's shadow without shaping its logic or enjoying its symmetrical stabilization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.06).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.08).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Rational Dropout Constraint").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic/international_relations").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, '151ac49a-b877-48b0-88d5-bf3f1c2152c3').
narrative_ontology:cs_kernel_codification('151ac49a-b877-48b0-88d5-bf3f1c2152c3', distributed).
narrative_ontology:cs_authority_grounding('151ac49a-b877-48b0-88d5-bf3f1c2152c3', expertise).
narrative_ontology:cs_interpretation_layer_present('151ac49a-b877-48b0-88d5-bf3f1c2152c3').
narrative_ontology:cs_reading_relation('151ac49a-b877-48b0-88d5-bf3f1c2152c3', nuclear_impossibility_kernel__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('151ac49a-b877-48b0-88d5-bf3f1c2152c3', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('151ac49a-b877-48b0-88d5-bf3f1c2152c3', foundational, victory_remains_structurally_possible).
narrative_ontology:cs_axiom_status(victory_remains_structurally_possible, holdable).
narrative_ontology:cs_axiom_grounding('151ac49a-b877-48b0-88d5-bf3f1c2152c3', victory_remains_structurally_possible, empirically_contingent).
narrative_ontology:cs_axiom('151ac49a-b877-48b0-88d5-bf3f1c2152c3', foundational, costs_exceed_all_conceivable_benefits).
narrative_ontology:cs_axiom_status(costs_exceed_all_conceivable_benefits, holdable).
narrative_ontology:cs_axiom_grounding('151ac49a-b877-48b0-88d5-bf3f1c2152c3', costs_exceed_all_conceivable_benefits, instrumental).
narrative_ontology:cs_reference_frame('151ac49a-b877-48b0-88d5-bf3f1c2152c3', rational_strategic_calculus).
narrative_ontology:cs_drift_state('151ac49a-b877-48b0-88d5-bf3f1c2152c3', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('151ac49a-b877-48b0-88d5-bf3f1c2152c3', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Does not solve a coordination problem through agreement; rather, it eliminates a dominated strategy from individual rational choice sets. The resulting peace is an emergent byproduct of unilateral cost-benefit dropout, not a negotiated equilibrium.
% TRANSFER_FUNCTION: No direct transfer between agents; the constraint removes nuclear first-strike and major war from the active option set of all rational strategic actors by rendering expected costs infinite relative to expected gains.
% ABSENT_VOICES: Non-nuclear weapon states, future generations, and abolitionist movements are present in discourse but excluded from the strategic logic that constitutes the constraint; they would argue the constraint is insufficient and the weapons themselves should be eliminated rather than managed by rational dropout.
% DISAPPEARANCE_RATIONALE: If the rational dropout constraint vanishedâif nuclear war became a cost-benefit rational optionâthe entire structure of great-power relations would reorganize. Deterrence doctrines would collapse, extended deterrence guarantees would dissolve, and alliance architectures built on the assumption of nuclear unusability would fracture.
% FOUNDING_PROBLEM: The endemic risk of major war between nuclear-armed powers, where traditional cost-benefit calculations fail because nuclear weapons invert the cost structure of conflict, making even 'victory' catastrophic.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by non-nuclear weapon states (who seek security without acquisition), international organizations monitoring non-proliferation and disarmament, and independent strategic studies scholarship outside nuclear weapons establishments; the live status is universally acknowledged even where prescriptions differ.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.06, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__rational_dropout_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is minimal (0.06) because the constraint does not extract resources from governed parties; it eliminates a dominated strategy from rational choice sets. Suppression is minimal (0.08) because the constraint persists without active enforcementâstates do not need to be coerced into avoiding nuclear war once the cost structure is understood. Theater ratio is negligible (0.02) because the constraint requires no performative maintenance to function; the cost-benefit logic is direct. Accessibility collapse is very high (0.92) because once nuclear destructiveness is comprehended, the alternative (rational nuclear war for gain) collapses as a live strategic option. Resistance is negligible (0.05) because no actor with launch authority has a sustained interest in reopening nuclear war as a rational policy option.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of nuclear command authority, the constraint feels like self-imposed rational judgment (low directionality). From the seat of civilian populations, it is an external structural fact they cannot influence (high directionality toward protection). The divergence is muted because the constraint applies symmetricallyâall nuclear states face the same cost structureâproducing near-identical computed types across nuclear-armed seats. The analytical observer seat sees the full structure: a mountain that looks like self-governance to those on top and like weather to those underneath.
 *
 * DIRECTIONALITY LOGIC:
 *   No explicit beneficiaries or victims are declared because the constraint operates symmetrically on all rational strategic actors. The cost is self-inflicted by the technology, not transferred between agents. Nuclear states are simultaneously subjects and objects of the constraint. Civilian populations are structurally protected (negative extraction) but have no agency in maintaining the constraint. Directionality is near-symmetric across nuclear-armed seats because power and exit profiles are structurally identical among them.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by distinguishing the rational dropout (mountain) from deterrence-as-coordination (rope or tangled rope). If the constraint required active enforcementâif states had to be continually pressured not to launchâit would be a snare or tangled rope. But the rational dropout reading claims that once costs are understood, no enforcement is necessary; the dominated strategy drops out of the rational choice set unilaterally. Mandatrophy would occur if the strategic community continued maintaining elaborate deterrence theorizing after the underlying cost structure had dissolved (e.g., if nuclear arsenals became non-destructive), but the current interval shows no such dissolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the rational dropout the correct reading of the nuclear impossibility kernel, or does the structural contraction reading (physical impossibility) or credibility paradox reading better capture the constraint?',
    'Comparative analysis of war-game outcomes and strategic stability models; whether any scenario exists in which nuclear war produces a meaningful ''victory'' for one party.',
    'If structural contraction is correct, the constraint is a stronger mountain with zero reachability; if credibility paradox is correct, the constraint depends on active performative maintenance and may classify as tangled rope or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contest between three kernel readings').

omega_variable(
    rationality_axiom_empirical_status,
    'Do nuclear-armed states actually behave as cost-benefit rational actors with respect to nuclear use, or is the rational dropout an idealization?',
    'Behavioral and organizational studies of nuclear command and control; crisis decision-making case studies (Cuban Missile Crisis, 1973 Middle East alert, etc.).',
    'If states systematically deviate from cost-benefit rationality, the rational dropout reading''s predictive power collapses and the constraint may be weaker than modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_axiom_empirical_status, empirical, 'Empirical validity of rational actor axiom').

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the rational dropout constraint a discovered feature of strategic reality under nuclear conditions, or a constructed doctrine that benefits existing nuclear powers by stabilizing the status quo?',
    'Historical analysis of deterrence theory emergence; examination of whether non-nuclear states experience the constraint symmetrically; false-summit mountain evaluation.',
    'If constructed, the mountain claim is a false summit and the constraint reclassifies as tangled rope serving nuclear-weapons-state interests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Natural law versus constructed stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nuc_rd_tr_t0, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(nuc_rd_tr_t10, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 10, 0.03).
narrative_ontology:measurement(nuc_rd_tr_t20, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(nuc_rd_tr_t30, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 30, 0.06).
narrative_ontology:measurement(nuc_rd_tr_t40, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 40, 0.04).
narrative_ontology:measurement(nuc_rd_tr_t50, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 50, 0.02).
narrative_ontology:measurement(nuc_rd_tr_t60, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 60, 0.02).
narrative_ontology:measurement(nuc_rd_tr_t70, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 70, 0.03).
narrative_ontology:measurement(nuc_rd_tr_t80, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 80, 0.02).

% Extraction over time
narrative_ontology:measurement(nuc_rd_be_t0, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(nuc_rd_be_t10, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 10, 0.08).
narrative_ontology:measurement(nuc_rd_be_t20, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(nuc_rd_be_t30, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 30, 0.1).
narrative_ontology:measurement(nuc_rd_be_t40, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement(nuc_rd_be_t50, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 50, 0.06).
narrative_ontology:measurement(nuc_rd_be_t60, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(nuc_rd_be_t70, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 70, 0.06).
narrative_ontology:measurement(nuc_rd_be_t80, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 80, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nuclear_impossibility_kernel__rational_dropout_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% The nuclear_impossibility_kernel decomposes into three structurally distinct constraints. The structural_contraction_reading treats war as physically impossible (stronger mountain). The rational_dropout_reading treats war as structurally possible but rationally dominated (mountain with reachable set preserved). The credibility_paradox_reading treats the constraint as a performative coordination problem. Each reading has different epsilon, different scope, and different classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
