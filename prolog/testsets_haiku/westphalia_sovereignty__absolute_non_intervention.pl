% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Absolute Non-Intervention Sovereignty Doctrine
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the Westphalia sovereignty
 *   kernel—the absolute non-intervention reading. The kernel itself is
 *   contested: states claiming full sovereignty based on territorial
 *   inviolability compete with readings that make sovereignty conditional on
 *   respect for atrocities (conditional_responsibility) and readings that
 *   treat sovereignty as a scalar property dependent on state capacity
 *   (graded_sovereignty). This story models the absolute reading: sovereignty
 *   is categorical, inviolability is per se, and internal conduct—no matter
 *   how atrocious—does not forfeit territorial protection. The constraint is
 *   CLAIMED as tangled_rope because it coordinates protection of interstate
 *   peace while simultaneously extracting protection for perpetrators of
 *   atrocities. The authored metrics reflect a trajectory of increasing
 *   theater: the founding coordination function (preventing religious wars)
 *   has substantially atrophied, but the doctrine is maintained through
 *   performative international affirmations and selective enforcement
 *   (intervention is still labeled 'exception-to-the-rule'). This reading's
 *   beneficiaries are state elites and authoritarian regimes; its victims are
 *   persecuted populations, minorities, and dissidents. The rival readings
 *   (conditional_responsibility, graded_sovereignty) would completely alter
 *   the victim set and beneficiary structure; they are different constraints,
 *   not different observations of this one.
 *
 * KEY AGENTS:
 *   - state_elites: institutional agenda-setters; structure the non-intervention doctrine to shield internal governance; maintain monopoly over use of violence
 *   - authoritarian_regimes: organized beneficiaries; depend directly on the doctrine to continue atrocities without external intervention
 *   - populations_under_atrocity: powerless victims; trapped by identity and geography; no voice in forums where doctrine is codified
 *   - persecuted_minorities: powerless victims; identity-locked; cannot renounce the status that makes them targets
 *   - internal_dissidents: powerless victims; constrained exit; face state repression with no external protection under the doctrine
 *   - western_democracies: institutional payers; endorse non-intervention officially while practicing selective intervention; constrained by the doctrine's legitimacy gap
 *   - humanitarian_ngos: excluded; document atrocities but legally barred from calling for intervention by the doctrine itself
 *   - UN_security_council: observer; formally adjudicates intervention legitimacy but applies the non-intervention doctrine as default rule
 *   - international_courts: observer; navigate the contradiction between non-intervention and universal atrocity jurisdiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.78).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.81).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.78).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Absolute Non-Intervention Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '6d9d8f77-0e6a-497a-94d8-d6653997dded').
narrative_ontology:cs_kernel_codification('6d9d8f77-0e6a-497a-94d8-d6653997dded', formalized).
narrative_ontology:cs_authority_grounding('6d9d8f77-0e6a-497a-94d8-d6653997dded', lineage).
narrative_ontology:cs_interpretation_layer_present('6d9d8f77-0e6a-497a-94d8-d6653997dded').
narrative_ontology:cs_reading_relation('6d9d8f77-0e6a-497a-94d8-d6653997dded', westphalia_sovereignty__conditional_responsibility, forecloses).
narrative_ontology:cs_reading_relation('6d9d8f77-0e6a-497a-94d8-d6653997dded', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('6d9d8f77-0e6a-497a-94d8-d6653997dded', foundational, sovereignty_categorical_inviolable).
narrative_ontology:cs_axiom_status(sovereignty_categorical_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('6d9d8f77-0e6a-497a-94d8-d6653997dded', sovereignty_categorical_inviolable, deontological).
narrative_ontology:cs_axiom('6d9d8f77-0e6a-497a-94d8-d6653997dded', foundational, non_intervention_absolute_regardless_conduct).
narrative_ontology:cs_axiom_status(non_intervention_absolute_regardless_conduct, holdable).
narrative_ontology:cs_axiom_grounding('6d9d8f77-0e6a-497a-94d8-d6653997dded', non_intervention_absolute_regardless_conduct, conventional).
narrative_ontology:cs_reference_frame('6d9d8f77-0e6a-497a-94d8-d6653997dded', sovereign_equality_inviolable_territory).
narrative_ontology:cs_drift_state('6d9d8f77-0e6a-497a-94d8-d6653997dded', post_humanitarian_exception_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6d9d8f77-0e6a-497a-94d8-d6653997dded', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, state_elites).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_atrocity).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, persecuted_minorities).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, internal_dissidents).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) and climbing: the doctrine transfers protection from vulnerable populations to state elites, and the extraction is asymmetric—it falls heaviest on the powerless. Suppression is also high (0.81): the doctrine actively prevents external actors from intervening, imposes legal sanctions on those who do, and excludes victim populations from international forums where the doctrine is affirmed. Theater is substantial (0.62) and rising: the founding coordination problem (preventing religious wars and conquest) has been solved for 350+ years, yet the doctrine is maintained through ritualized affirmations (UN General Assembly resolutions), selective application (Western states intervene and then appeal to 'humanitarian exception'), and doctrinal gymnastics (distinguishing 'intervention' from 'involvement'). The trajectory shows base extractiveness rising from 0.55 to 0.78 as more state regimes adopt internal repression under cover of the doctrine's protection; theater rising from 0.40 to 0.62 as the gap widens between the founding justification (preventing conquest) and current function (protecting atrocity-committing states). Suppression rises steadily as enforcement mechanisms mature: international legal institutions solidify the doctrine, state vetoes in the Security Council enforce it, and sanctions against intervention-attempting states harden. The measurements span 80 time units (proxy for 350+ years of doctrine operation, scaled for analytical intervals) and show the trajectory: a constraint that solved a real coordination problem but has degraded into an extractive mechanism protecting authoritarian violence.
 *
 * PERSPECTIVAL GAP:
 *   From the state-elites seat, this is a genuine coordination mechanism: non-intervention prevents the chaos of constant external interference and provides each state with certainty that its borders are inviolable. From the victim seats (persecuted populations, dissidents), the same structure is a mechanism that isolates them from external protection and shields the violence they experience. The engine computes these divergent types from the structural data: state-elites have arbitrage exit and institutional power, placing them near the beneficiary end of directionality (d near 0); populations_under_atrocity have trapped exit and powerless status, placing them at the target end (d near 1). The same constraint produces different effective extraction values for different seats because the directionality derivation is per-seat. No reconciliation of these perspectives is possible—they are structurally incompatible. The absolute reading makes them irreconcilable.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites and authoritarian regimes benefit from the doctrine without bearing its costs—they collect immunity from external pressure. Their directionality is low (d near 0.15–0.25), making effective extraction on them negligible or even subsidized (the constraint subsidizes their position). Victims (persecuted populations, dissidents, minorities) bear the full cost without benefiting—they are trapped and powerless. Their directionality is high (d near 0.85–0.95), making effective extraction on them extreme despite the base extractiveness value. Western democracies are paradoxical: they officially endorse the doctrine (beneficiary framing) but practice selective intervention (payer framing). Their directionality is moderate (d near 0.50), reflecting this contradiction. The beneficiary/victim declarations in base_properties drive this per-seat divergence automatically through the engine's directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits classical mandate drift: the founding mandate was to prevent religious wars and conquest by external powers. That problem is solved—modern states do not face conquest by religious powers justified through divine authority. Yet the doctrine persists, and its current function is to shield systematic atrocities. The constraint has transitioned from coordination (solving genuine interstate conflict) to extraction (protecting perpetrators). The theater_ratio trajectory (rising from 0.40 to 0.62) captures this: the coordination function has atrophied but the enforcement machinery is maintained through performative activity (ritual UN affirmations, selective application to some interventions but not others). The tangled_rope classification is correct: genuine coordination function remains (preventing some forms of interstate interference), but extraction has grown asymmetrically (state elites collect protection, atrocity victims bear the cost). The constraint is not a pure snare because some genuine coordination persists; it is not a pure rope because the extraction is now the dominant function relative to the coordination it provides.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_function_vs_current_function,
    'Has the founding coordination problem (preventing religious conquest and interstate interference) been solved, or does it remain live such that the doctrine''s enforcement is still justified?',
    'Historical analysis of interstate conflict patterns since 1648, controlling for the constraint''s presence/absence. Examine whether remaining interventions follow the doctrine''s logic (fear of conquest) or new logics (humanitarian concern, regime change). If remaining conflict is orthogonal to conquest fears, the founding function is dead.',
    'If the founding function is dead, the constraint exhibits mandatrophy: it persists as protection for perpetrators rather than as coordination for general peace. This would support reclassification from tangled_rope toward piton (performative maintenance) or full snare (pure extraction). If the founding function is live, the tangled_rope classification stands and the extraction is justified as coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_function_vs_current_function, empirical, 'Whether the constraint solves the problem it was built for, or persists only as protection for atrocity.').

omega_variable(
    conditional_vs_absolute_legitimacy,
    'Is absolute inviolability the reading of the kernel the system is actually committed to, or is the commitment already conditional in practice—with interventions allowed under ''humanitarian exception'' carve-outs?',
    'Systematic audit of UN Security Council authorizations and interventions since 1990. Measure the proportion of interventions justified on humanitarian grounds (atrocity response) vs. conquest-prevention grounds. If humanitarian exceptions are common and normalized, the system operates under conditional_responsibility reading, not absolute_non_intervention.',
    'If the system is already conditional in practice, this absolute reading describes a formal doctrine while actual practice is governed by a different reading (conditional_responsibility). The constraint''s type would need to account for this gap: either the formal doctrine enforces the absolute reading (and practice is violation), or the actual governance follows conditional readings and the formal doctrine is window-dressing. This affects whether the constraint is enforced or merely theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditional_vs_absolute_legitimacy, empirical, 'Whether the system actually operates under absolute or conditional sovereignty.').

omega_variable(
    structural_suppression_mechanism,
    'Is the high suppression (0.81) primarily structural (legal barriers, UN veto, enforcement institutions) or internalized (states have accepted the doctrine as legitimate even when it prevents intervention they might prefer)?',
    'Survey and archival analysis: examine confidential state communications during humanitarian crises, testimony from diplomats and legal advisors, and counterfactual analysis of what intervention patterns would emerge if the doctrine were removed. Widespread preference for intervention despite the doctrine suggests structural suppression is dominant; widespread preference for restraint suggests internalization.',
    'If suppression is primarily structural, removing the doctrine would immediately enable intervention. If primarily internalized, states would need to rebuild legitimacy narratives even after doctrine removal. This affects the persistence of the constraint post-removal and the cost of fixing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_suppression_mechanism, empirical, 'Whether suppression is external legal barriers or internalized norm acceptance.').

omega_variable(
    kernel_reading_contest_frame,
    'Which reading of the Westphalia kernel (absolute_non_intervention, conditional_responsibility, or graded_sovereignty) reflects the actual commitments of the international system, and would adopting a sibling reading constitute a logical foreclosure of this reading or merely a coexistence of different commitments?',
    'Analyze the axioms and reference frames of each reading. If any reading''s core axiom (e.g., ''sovereignty is inviolable regardless of internal conduct'') is incompatible with the sibling''s core axiom (e.g., ''sovereignty is conditional on atrocity prevention''), they foreclose each other. If the readings differ in reference frame or empirical assessment but share compatible foundational commitments, they coexist. This determination governs whether the readings are competing or complementary.',
    'If readings foreclose each other, adopting one is a rejection of the others at the foundational level; the system cannot hold multiple readings simultaneously in one framework. If readings coexist, the system currently operates under all three simultaneously (different states or forums apply different readings), which explains the observed practice variability and suggests the kernel itself is contested rather than settled. This affects long-term stability: foreclosed readings suggest eventual dominance of one reading; coexisting readings suggest permanent contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_frame, conceptual, 'Whether rival sovereignty readings logically foreclose each other or coexist as live positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0, 0.4).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 10, 0.48).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 20, 0.54).
narrative_ontology:measurement(west_tr_t30, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 30, 0.58).
narrative_ontology:measurement(west_tr_t40, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 40, 0.6).
narrative_ontology:measurement(west_tr_t50, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 50, 0.61).
narrative_ontology:measurement(west_tr_t60, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 60, 0.62).
narrative_ontology:measurement(west_tr_t80, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 80, 0.62).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(west_be_t30, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(west_be_t40, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(west_be_t50, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 50, 0.77).
narrative_ontology:measurement(west_be_t60, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 60, 0.78).
narrative_ontology:measurement(west_be_t80, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 80, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(west_su_t30, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 30, 0.77).
narrative_ontology:measurement(west_su_t40, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(west_su_t50, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(west_su_t60, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 60, 0.81).
narrative_ontology:measurement(west_su_t80, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 80, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__absolute_non_intervention, 0.14).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__graded_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Westphalia sovereignty kernel. The absolute_non_intervention reading treats sovereignty as categorical and inviolability as per se. The sibling reading conditional_responsibility makes sovereignty conditional on state protection of populations from atrocities, fundamentally altering the beneficiary/victim structure and ε-value. The graded_sovereignty reading treats sovereignty as scalar, changing the framework entirely. All three are linked as a constraint family: each reading affects the others through institutional competition and theoretical conflict. The upstream commitment (that there IS a territorial state system requiring some framework of inviolability) affects all three readings; the readings themselves compete at the level of the specific boundaries and conditions. See network.affects_constraints on each reading for the family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__absolute_non_intervention, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
