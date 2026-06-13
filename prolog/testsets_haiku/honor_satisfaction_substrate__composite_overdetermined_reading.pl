% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Honor Satisfaction Substrate (Composite Overdetermined Reading)
 *   domain: social/legal/cultural
 *
 * SUMMARY:
 *   This reading examines the decline of European dueling (roughly 1650–1850)
 *   as the product of TWO causally entangled mechanisms operating
 *   simultaneously: exogenous legal suppression (criminalization,
 *   institutional barriers, enforcement intensity) and endogenous
 *   delegitimation of the honor substrate itself (Enlightenment reframing of
 *   status, transformation from honor-based to dignity-based ethics,
 *   internalization of new masculinity norms). The reading claims that
 *   neither mechanism alone explains the constraint's collapse, and that the
 *   mechanisms are not independent — legal pressure delegitimized the code
 *   partly by making its enforcement costly, while cultural transformation
 *   made suppression increasingly effective because fewer people believed in
 *   the code's normative force. This is the composite_overdetermined_reading
 *   of the honor_satisfaction_substrate kernel. Sibling readings attribute
 *   the decline to EITHER exogenous suppression alone
 *   (practice_decline_reading) OR endogenous cultural transformation alone
 *   (cultural_contraction_reading).
 *
 * KEY AGENTS:
 *   - aristocratic_honor_culture: maintains the coordination apparatus (reputation networks, masculine identity norms, public challenge-response mechanisms) that makes dueling obligatory
 *   - duelists_trapped_by_honor_code: identity-locked into participation; death/prosecution risk is the cost of maintaining elite status in the eyes of peers
 *   - legal_authorities_enforcing_suppression: criminalize dueling and invest enforcement resources; bear the cost of sustained institutional pressure against cultural norms
 *   - enlightenment_intellectuals: provide the alternative cultural frame (dignity ethics, rationalism, domestic virtue) that makes the honor code appear constructed rather than natural
 *   - women_and_non_elite_men: excluded from the dueling apparatus but bear economic/social costs; their absence from the conversation is structural
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.62).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.78).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Honor Satisfaction Substrate (Composite Overdetermined Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "social/legal/cultural").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, '35acaa15-c4a2-4ff6-ad6b-a9898540c9b1').
narrative_ontology:cs_kernel_codification('35acaa15-c4a2-4ff6-ad6b-a9898540c9b1', distributed).
narrative_ontology:cs_authority_grounding('35acaa15-c4a2-4ff6-ad6b-a9898540c9b1', practice).
narrative_ontology:cs_interpretation_layer_present('35acaa15-c4a2-4ff6-ad6b-a9898540c9b1').
narrative_ontology:cs_reading_relation('35acaa15-c4a2-4ff6-ad6b-a9898540c9b1', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('35acaa15-c4a2-4ff6-ad6b-a9898540c9b1', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('35acaa15-c4a2-4ff6-ad6b-a9898540c9b1', foundational, suppression_and_delegitimation_entangled).
narrative_ontology:cs_axiom_status(suppression_and_delegitimation_entangled, holdable).
narrative_ontology:cs_axiom_grounding('35acaa15-c4a2-4ff6-ad6b-a9898540c9b1', suppression_and_delegitimation_entangled, empirically_contingent).
narrative_ontology:cs_axiom('35acaa15-c4a2-4ff6-ad6b-a9898540c9b1', foundational, honor_code_transformation_simultaneous_with_suppression).
narrative_ontology:cs_axiom_status(honor_code_transformation_simultaneous_with_suppression, holdable).
narrative_ontology:cs_axiom_grounding('35acaa15-c4a2-4ff6-ad6b-a9898540c9b1', honor_code_transformation_simultaneous_with_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('35acaa15-c4a2-4ff6-ad6b-a9898540c9b1', honor_as_coordinate_and_natural).
narrative_ontology:cs_drift_state('35acaa15-c4a2-4ff6-ad6b-a9898540c9b1', post_enlightenment_delegitimation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('35acaa15-c4a2-4ff6-ad6b-a9898540c9b1', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_honor_culture).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, duelists_trapped_by_honor_code).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, legal_authorities_enforcing_suppression).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.45 at 1650) because dueling is coordinating genuine elite status disputes — participants genuinely believe the mechanism is legitimate and necessary. It rises to 0.62 by 1800 as legal suppression accelerates and cultural alternatives emerge; the constraint now persists partly through coercion and partly through cultural inertia, with fewer genuine believers in the code's necessity. It drops slightly by 1850 (0.62 → 0.62) because enforcement intensity has essentially destroyed the coordination mechanism entirely — the code persists theatrically among some elites but the material constraint (the binding obligation to duel) is broken. Suppression_requirement rises from 0.15 to 0.78 — at 1650, enforcement is minimal because the code is self-enforcing through peer pressure; by 1800, the state must actively criminalize and prosecute to maintain suppression; by 1850, enforcement remains high even as dueling rates fall (police, courts, reputation damage are all deployed). Theater_ratio rises from 0.08 to 0.41 — early on, dueling is functionally necessary to the status system; by 1850, dueling persists mainly as theatrical maintenance of an anachronistic code. The measurements track the entanglement: suppression_requirement rises BECAUSE cultural delegitimation makes the code optional; theater_ratio rises BECAUSE legal pressure makes functional coordination impossible; neither metric moves monotonically, because the two causal pathways interfere with each other.
 *
 * PERSPECTIVAL GAP:
 *   The duelist seat should compute as a complex mixed type: from inside the honor code, the constraint is genuinely coordinative (rope-ish) — a binding solution to the status problem. From the legal authority seat, it is a snare that must be suppressed — extractive and coercive. From the intellectual observer seat, it is a mountain being eroded — a natural law of honor yielding to a new natural law of dignity. The engine computes per-seat directionality from the structural data (beneficiary/victim + exit options); this story's claim is that the constraint's type CHANGES as you move seats, and the measurements track the intermediate period when both mechanisms (suppression and delegitimation) are active simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Duelists are identity-locked (exit_options: identity_locked) — refusal to duel means social annihilation, loss of elite masculine identity. They are declared both victims (bear death/prosecution risk) and beneficiaries (gain status from honor system). This dual positioning is central to the reading: the same mechanism that extracts from them (forces them to duel at risk of death) also benefits them (allocates status). Extractiveness is high (0.62 end-state) because the constraint persists through coercion and cultural inertia, not through freely-given participation. Suppression is higher (0.78) because exit is not merely expensive but psychologically/socially unthinkable — the identity-lock is deep. Legal authorities are declared as agenda_setters + payers because they must actively maintain suppression while bearing the cost of enforcement. The honor culture is the beneficiary — it collects rents in the form of continued elite status allocation and maintains the apparatus that makes the code appear natural.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly embraces mandatrophy: the founding problem (elite status coordination without institutional arbiters) is dead by 1800 — modern states provide institutional arbitration and do not tolerate private violence. Yet the constraint persists until ~1850, and faintly beyond. The persistence is mandatropic — the code is maintained by institutional inertia, cultural pride among conservative elites, and the self-reinforcing nature of reputation networks, not by the solving of any live problem. Theater_ratio documents this: as the founding problem dies, the ratio of performative to functional activity rises from 0.08 to 0.41. The composite_overdetermined_reading claims that mandatrophy was ENABLED by the simultaneous operation of suppression and delegitimation — legal pressure made the code costly to maintain, cultural transformation made it feel optional, and together they produced the era (1800–1850) when dueling is mostly theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_entanglement_vs_independence,
    'Are the two decline mechanisms (exogenous suppression and endogenous delegitimation) causally independent, or do they constitute a single over-determined system where suppression and cultural transformation reinforce each other?',
    'Counterfactual historical analysis: Did jurisdictions that suppressed dueling WITHOUT cultural delegitimation (e.g., Islamic law, Napoleonic code early adoption without Enlightenment frame) see sustained cultural persistence of the honor code despite enforcement? Did cultures that underwent dignity-transformation WITHOUT legal suppression (late 19th-century intellectual circles, bohemia) experience spontaneous abandonment of dueling or continued performance?',
    'If mechanisms are independent, the constraint is additively composite (rope + mountain = complex mixed type, no single classification). If entangled, the constraint is a single unified system exhibiting phase-transition properties — the composite type emerges from the interaction, not the sum.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_entanglement_vs_independence, empirical, 'Whether suppression and cultural delegitimation are independent causal pathways or a single coupled system.').

omega_variable(
    identity_lock_internalization_vs_structural,
    'Is the duelist''s identity-lock to the honor code primarily structural (external barriers make exit costly) or internalized (the code has become constitutive of self-concept such that exit feels psychologically impossible)?',
    'Post-suppression behavior: Among duelists who abandoned the code under legal pressure, did they experience persistent shame, identity confusion, or psychological distress suggesting internalized lock? Or did they straightforwardly adopt new status mechanisms, suggesting the lock was primarily structural and dissolved when barriers were removed?',
    'If internalized, the suppression metrics understate the true constraint — the target carries suppression with them after exit. If structural, the measured suppression captures the true constraining force and exit becomes easier once legal pressure removes external barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization_vs_structural, empirical, 'Internalized vs. structural suppression in the duelist seat.').

omega_variable(
    honor_substrate_transformation_scope,
    'Did the honor code undergo transformation only in legal/intellectual elites, or did the transformation penetrate to the broader population that the constraint governed?',
    'Cultural analysis of 19th-century popular literature, correspondence, memoirs: did non-elite duelists (military officers, provincial gentry) internalize the dignity-ethics frame, or did they experience suppression as imposed external coercion from an elite that was itself abandoning the code''s legitimacy?',
    'If transformation penetrated broadly, delegitimation was an endogenous cultural shift affecting the entire governed population. If transformation was elite-only, the broad population experienced suppression without accompanying delegitimation, and the two mechanisms operated on different populations (non-independent in mechanism, but differentiated in scope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_substrate_transformation_scope, empirical, 'Whether cultural transformation of honor code was population-wide or elite-restricted.').

omega_variable(
    alternative_status_mechanisms_availability,
    'Did modern institutional status mechanisms (credentialing, professional licensing, legal authority) become available BEFORE or AFTER legal suppression of dueling began?',
    'Historical timeline: when did universities, professional guilds, civil service exams, and courts begin allocating status and reputation in ways that competed with dueling as a status mechanism?',
    'If alternatives emerged before suppression, duelists had a coordination exit option (redirect status-seeking to new channels) and suppression would be more effective. If alternatives emerged after suppression, legal pressure forced a transition to mechanisms that had no prior legitimacy, making the post-suppression period one of institutional improvisation rather than rational switching.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_status_mechanisms_availability, empirical, 'Temporal availability of alternative status-allocation mechanisms relative to legal suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 1650, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1650, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1650, 0.08).
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1700, 0.12).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1750, 0.18).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1800, 0.32).
narrative_ontology:measurement(hono_tr_t1825, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1825, 0.39).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1850, 0.41).

% Extraction over time
narrative_ontology:measurement(hono_be_t1650, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1650, 0.45).
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1700, 0.48).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1750, 0.55).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1800, 0.62).
narrative_ontology:measurement(hono_be_t1825, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1825, 0.67).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1850, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1650, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1650, 0.15).
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1700, 0.22).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1750, 0.38).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1800, 0.68).
narrative_ontology:measurement(hono_su_t1825, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1825, 0.77).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1850, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__composite_overdetermined_reading, 0.12).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% The honor_satisfaction_substrate kernel decomposes into three constraint stories, each instantiating a different causal reading of dueling's historical decline. The composite_overdetermined_reading (this story) claims both exogenous suppression and endogenous cultural transformation operated simultaneously with non-independent causal pathways. The practice_decline_reading attributes decline to suppression alone, with the honor code persisting as normative substrate. The cultural_contraction_reading attributes decline to cultural transformation alone, with the code undergoing foundational change from honor-based to dignity-based ethics. All three readings are ε-invariant: they differ in causal structure (which mechanism drove the constraint's collapse), not in measurement basis. Each produces a different ε and a different type classification. The network links document mutual structural influence: suppression delegitimizes the code (composite affects practice_decline), cultural transformation makes suppression more effective (composite affects practice_decline), and the availability of alternative status mechanisms determines whether either mechanism works (all three inherit from cultural_contraction's outcome).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__composite_overdetermined_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
