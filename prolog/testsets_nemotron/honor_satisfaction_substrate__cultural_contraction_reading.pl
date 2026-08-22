% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__cultural_contraction_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Honor Substrate Collapse — Cultural Contraction Reading
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story instantiates the cultural_contraction_reading of
 *   the honor_satisfaction_substrate kernel: the honor code itself underwent
 *   foundational transformation between roughly 1600–1900 in Western Europe
 *   and its settler colonies, such that dueling — once the paradigmatic
 *   satisfaction mechanism for honor claims — became cognitively and morally
 *   unthinkable. The constraint is not that dueling was suppressed by law
 *   (though it was); the constraint is that the interpretive substrate making
 *   dueling intelligible as a response to insult disintegrated. Cultures of
 *   honor, where worth is externally validated and defended by violence, gave
 *   way to cultures of dignity, where worth is intrinsic and non-negotiable.
 *   This is mountain erosion: the physical/social substrate supporting the
 *   practice collapsed endogenously. The engine should compute mountain from
 *   the structural data (near-zero extraction, near-zero suppression, maximal
 *   accessibility collapse, negligible resistance). The sibling readings —
 *   practice_decline_reading (exogenous suppression on persistent substrate)
 *   and composite_overdetermined_reading (both pathways) — are separate
 *   constraint stories.
 *
 * KEY AGENTS:
 *   - aristocratic_elite: Primary actors in the honor system — their status claims structured the substrate; as the substrate collapsed, their positional advantage eroded
 *   - state_legal_authority: Exogenous suppressor in sibling readings; in this reading, a secondary actor whose prohibitions tracked rather than drove the substrate shift
 *   - bourgeois_moral_intelligentsia: Articulators and carriers of the dignity culture; their normative entrepreneurship accelerated the substrate transformation
 *   - military_officer_corps: Institutional residue where honor logic persisted longest (dueling survived in armies into the 20th century); test case for residual_honor_continuity omega
 *   - analytical_observer: Sees the full structural trajectory — the substrate collapse as a cultural phase transition, not a policy outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.08).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.12).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Honor Substrate Collapse — Cultural Contraction Reading").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, 'a4e47d2c-1f33-4f2e-a5e3-db0ab1c0244b').
narrative_ontology:cs_kernel_codification('a4e47d2c-1f33-4f2e-a5e3-db0ab1c0244b', implicit).
narrative_ontology:cs_authority_grounding('a4e47d2c-1f33-4f2e-a5e3-db0ab1c0244b', practice).
narrative_ontology:cs_reading_relation('a4e47d2c-1f33-4f2e-a5e3-db0ab1c0244b', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('a4e47d2c-1f33-4f2e-a5e3-db0ab1c0244b', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('a4e47d2c-1f33-4f2e-a5e3-db0ab1c0244b', foundational, honor_substrate_endogenous_collapse).
narrative_ontology:cs_axiom_status(honor_substrate_endogenous_collapse, holdable).
narrative_ontology:cs_axiom_grounding('a4e47d2c-1f33-4f2e-a5e3-db0ab1c0244b', honor_substrate_endogenous_collapse, empirically_contingent).
narrative_ontology:cs_axiom('a4e47d2c-1f33-4f2e-a5e3-db0ab1c0244b', secondary, dignity_culture_supersedes_honor_culture).
narrative_ontology:cs_axiom_status(dignity_culture_supersedes_honor_culture, holdable).
narrative_ontology:cs_axiom_grounding('a4e47d2c-1f33-4f2e-a5e3-db0ab1c0244b', dignity_culture_supersedes_honor_culture, conventional).
narrative_ontology:cs_reference_frame('a4e47d2c-1f33-4f2e-a5e3-db0ab1c0244b', early_modern_honor_equilibrium).
narrative_ontology:cs_drift_state('a4e47d2c-1f33-4f2e-a5e3-db0ab1c0244b', long_nineteenth_century_transition, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('a4e47d2c-1f33-4f2e-a5e3-db0ab1c0244b', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, dignity_culture_replaces_honor_culture).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, dueling_exits_thinkable_action_set).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stakeholders authored EMPTY (Pattern-5: an explicit assertion that no
% entity's arrangements depend on this constraint — paired with the
% world_unchanged verdict below, enforced by the schema).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor substrate coordinated status claims and conflict resolution in weak-state environments by making violence a legitimate, intelligible response to insult — a decentralized deterrence and reputation system.
% TRANSFER_FUNCTION: No transfer function in this reading — the substrate collapse is not a distributive mechanism. The practice_decline_reading would describe a transfer from duelists to state monopoly on violence; the composite_overdetermined_reading would describe a mixed transfer.
% ABSENT_VOICES: Honor-culture carriers who experienced the transition as loss rather than liberation — aristocrats for whom dueling was existential meaning, not antiquated ritual. They are absent because the substrate that made their voice intelligible disintegrated; they cannot be 'excluded' from a conversation whose terms no longer exist.
% DISAPPEARANCE_RATIONALE: If the honor substrate vanished overnight, the world would not rearrange — because it already did. The constraint IS the substrate's historical disappearance. The disappearance_verdict reflects that this is a mountain (cultural fact), not an active arrangement. The world already rearranged around its absence.
% FOUNDING_PROBLEM: Managing status conflict and deterring predation in environments where state monopoly on violence was weak or absent — honor provided a decentralized, self-enforcing equilibrium.
% FOUNDING_PROBLEM_CORROBORATION: Standard historical sociology consensus (Weber, Elias, Pitt-Rivers, Nisbett & Cohen, Leeson & Nowrasteh): the founding problem (weak-state status governance) is dead in the core regions where the transition occurred; state monopoly on violence and impersonal markets solved it. No beneficiary set attests the problem is live — the honor substrate's own carriers were the agents of its dissolution.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.08) because no party extracts from the substrate collapse — it is a cultural phase transition, not a rent-seeking arrangement. Suppression is low (0.12) because the substrate's disappearance is endogenous; legal prohibitions followed rather than led the cultural shift. Theater ratio is negligible (0.05) — no performative maintenance of a dead practice. Accessibility collapse is maximal (0.92): once dignity culture takes hold, the honor action-set is cognitively inaccessible; one cannot 'choose' to duel for honor because the conceptual vocabulary for it has evaporated. Resistance is near-zero (0.03): the transition meets no organized opposition because the honor substrate's own carriers (aristocrats, officers) are the agents of its transformation. The measurement series tracks the slow, century-scale erosion: extractiveness and suppression creep upward slightly as state prohibitions formalize what culture has already settled, but the core dynamic is the substrate's autonomous disintegration.
 *
 * PERSPECTIVAL GAP:
 *   The practice_decline_reading would compute higher suppression and extractiveness (treating legal prohibitions as the active constraint on a living practice). The composite_overdetermined_reading would split the difference. This reading insists the substrate collapse is the primary causal arrow; the others are epiphenomenal or secondary. The engine computes per-seat types from the structural data authored here — the honor substrate is mountain from every seat because it is a cultural fact, not a political arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims declared — this is a cultural substrate transition, not an extraction arrangement. The aristocratic elite lose positional advantage but do not 'pay' in an extractive sense; the state gains monopoly on violence but does not 'collect' from the honor system's disappearance. The bourgeois intelligentsia gain normative hegemony but this is the substrate's own transformation, not a transfer. Directionality is analytically inert (all seats near d=0.5 symmetric) because the constraint is not a distributive mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable — there is no mandate to atrophy. The honor substrate was never instituted for a purpose that could be outlived; it was an emergent cultural equilibrium that dissolved when its conditions vanished. The founding problem (managing status conflict in weak-state environments) is dead, but the arrangement didn't persist — it vanished. This is mountain erosion, not mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the disappearance of dueling best explained by the endogenous transformation of the honor substrate itself (cultural_contraction_reading), or by exogenous legal suppression operating on a persistent honor substrate (practice_decline_reading), or by overdetermined co-action (composite_overdetermined_reading)?',
    'Comparative historical analysis of jurisdictions with divergent legal regimes but similar cultural trajectories; counterfactual modeling of dueling persistence under exogenous suppression alone vs. endogenous substrate collapse alone.',
    'If cultural_contraction_reading holds, the constraint is mountain erosion — the substrate disintegrates and the practice vanishes without enforcement. If practice_decline_reading holds, the substrate remains and the constraint is active suppression (snare/tangled_rope). If composite_overdetermined_reading holds, both readings capture partial structure and neither alone is ε-invariant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Disambiguates which reading captures the ε-invariant constraint structure.').

omega_variable(
    substrate_transformation_mechanism,
    'What specific mechanism drove the honor-to-dignity cultural transition? Was it market integration, state centralization, religious reformation, demographic shift, or an irreducible multi-causal process?',
    'Cross-regional comparison of transition timing against proxies for each candidate mechanism; textual analysis of normative discourse shifts in conduct manuals, legal codes, and literary representations.',
    'Identifies whether the substrate collapse was a structural inevitability (mountain) or a contingent historical trajectory that could have resolved differently (rope/scaffold).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substrate_transformation_mechanism, empirical, 'Mechanism of the honor-to-dignity cultural transition.').

omega_variable(
    residual_honor_continuity,
    'Does the honor substrate truly collapse entirely, or do residual honor logics persist in sub-domains (military, organized crime, diplomatic protocol, subcultural enclaves) as isolated pockets?',
    'Ethnographic and historical survey of honor-logic persistence in domains with weak state monopoly on violence or strong status-competition structures.',
    'If substantial residue exists, the mountain erosion claim is overstated — the constraint becomes a fragmented landscape of local mountains and valleys rather than a global collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_honor_continuity, empirical, 'Persistence of honor substrate in sub-domains after general cultural transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_tr_t1600, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1600, 0.01).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_tr_t1650, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1650, 0.02).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_tr_t1700, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1700, 0.02).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_tr_t1750, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1750, 0.03).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_tr_t1800, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1800, 0.04).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_tr_t1850, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1850, 0.04).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_tr_t1900, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1900, 0.05).

% Extraction over time
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_be_t1600, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1600, 0.02).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_be_t1650, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1650, 0.03).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_be_t1700, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1700, 0.04).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_be_t1750, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1750, 0.05).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_be_t1800, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1800, 0.06).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_be_t1850, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1850, 0.07).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_be_t1900, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1900, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_su_t1600, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1600, 0.05).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_su_t1650, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1650, 0.07).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_su_t1700, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1700, 0.08).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_su_t1750, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1750, 0.1).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_su_t1800, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1800, 0.11).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_su_t1850, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1850, 0.11).
narrative_ontology:measurement(honor_satisfaction_substrate__cultural_contraction_reading_su_t1900, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1900, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__cultural_contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__cultural_contraction_reading, 0.08).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% Kernel honor_satisfaction_substrate decomposes into three ε-invariant readings: cultural_contraction_reading (this file, mountain), practice_decline_reading (snare/tangled_rope), composite_overdetermined_reading (hybrid). The cultural_contraction_reading asserts endogenous substrate collapse as the primary structure; the other readings locate causal weight in exogenous suppression or in the interaction. All three linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
