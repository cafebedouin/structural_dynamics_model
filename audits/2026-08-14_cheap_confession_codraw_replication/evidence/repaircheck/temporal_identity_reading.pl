% ============================================================================
% CONSTRAINT STORY: temporal_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temporal_identity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: temporal_identity_reading
 *   human_readable: Temporal-Identity Reading of Commitment-Keeping Cost
 *   domain: epistemology/philosophy_of_mind
 *
 * SUMMARY:
 *   This story instantiates one reading of the 'commitment_cost_location'
 *   kernel: the claim that the cost of abiding by a confession or promise is
 *   located inside the agent's relation to its own earlier state, not in
 *   whether any outside party could detect a lapse. Because the predicting
 *   self (who made the commitment) and the judging self (who later evaluates
 *   whether to keep it) are not identical, 'abiding' is reframed as an
 *   achievement of psychological continuity against the standing temptation
 *   to reinterpret — an effort that would be real even in a hypothetical
 *   zero-observer scenario. The sibling readings (legibility_reading: cost is
 *   located in what an outside party can infer;
 *   enforcement_deflation_reading: cost is located in social/institutional
 *   sanction) are not represented here except as excluded voices; each is its
 *   own constraint with its own ε.
 *
 * KEY AGENTS:
 *   - the_present_self_facing_reinterpretation_temptation: bears the entire measured cost internally
 *   - predicting_self_at_time_of_commitment: sets the terms the present self must remain answerable to, though this self is not directly interrogable
 *   - future_selves_who_inherit_coherent_agency: benefit from continuity being maintained now
 *   - interlocutors_relying_on_stated_commitments: incidental beneficiaries of the agent's internal discipline
 *   - legibility_and_enforcement_framings: excluded sibling readings of the same kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_identity_reading, 0.28).
domain_priors:suppression_score(temporal_identity_reading, 0.35).
domain_priors:theater_ratio(temporal_identity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_identity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(temporal_identity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(temporal_identity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temporal_identity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(temporal_identity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_identity_reading, tangled_rope).
narrative_ontology:human_readable(temporal_identity_reading, "Temporal-Identity Reading of Commitment-Keeping Cost").
narrative_ontology:topic_domain(temporal_identity_reading, "epistemology/philosophy_of_mind").

domain_priors:requires_active_enforcement(temporal_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temporal_identity_reading, 'a8b418e0-3706-4cb1-b4c9-2b7851285652').
narrative_ontology:cs_kernel_codification('a8b418e0-3706-4cb1-b4c9-2b7851285652', distributed).
narrative_ontology:cs_authority_grounding('a8b418e0-3706-4cb1-b4c9-2b7851285652', distributed).
narrative_ontology:cs_reading_relation('a8b418e0-3706-4cb1-b4c9-2b7851285652', temporal_identity_reading__legibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8b418e0-3706-4cb1-b4c9-2b7851285652', temporal_identity_reading__enforcement_deflation_reading, coexists_with).
narrative_ontology:cs_axiom('a8b418e0-3706-4cb1-b4c9-2b7851285652', foundational, cost_located_in_intrapersonal_continuity_not_observability).
narrative_ontology:cs_axiom_status(cost_located_in_intrapersonal_continuity_not_observability, holdable).
narrative_ontology:cs_axiom_grounding('a8b418e0-3706-4cb1-b4c9-2b7851285652', cost_located_in_intrapersonal_continuity_not_observability, deontological).
narrative_ontology:cs_axiom('a8b418e0-3706-4cb1-b4c9-2b7851285652', secondary, predicting_self_and_judging_self_are_structurally_distinct).
narrative_ontology:cs_axiom_status(predicting_self_and_judging_self_are_structurally_distinct, holdable).
narrative_ontology:cs_axiom_grounding('a8b418e0-3706-4cb1-b4c9-2b7851285652', predicting_self_and_judging_self_are_structurally_distinct, empirically_contingent).
narrative_ontology:cs_reference_frame('a8b418e0-3706-4cb1-b4c9-2b7851285652', psychological_continuity_theory_of_commitment).
narrative_ontology:cs_drift_state('a8b418e0-3706-4cb1-b4c9-2b7851285652', contemporary_lm_discourse_debate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a8b418e0-3706-4cb1-b4c9-2b7851285652', '').
narrative_ontology:cs_kernel_id(temporal_identity_reading, commitment_cost_location).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temporal_identity_reading, future_selves_who_inherit_coherent_agency).
narrative_ontology:constraint_beneficiary(temporal_identity_reading, interlocutors_relying_on_stated_commitments).
narrative_ontology:constraint_victim(temporal_identity_reading, the_present_self_facing_reinterpretation_temptation).
narrative_ontology:constraint_vindicates(temporal_identity_reading, diachronic_personal_identity_thesis).
narrative_ontology:constraint_vindicates(temporal_identity_reading, confession_as_achievement_not_obedience).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds a prior commitment (a confession, promise, or stated position) and now faces internal pressure to reinterpret it in light of new preferences, incentives, or discomfort. The cost of 'abiding' is paid entirely internally: the effort of remaining answerable to the earlier self's terms rather than quietly re-narrating them. No outside party need ever notice a lapse for the cost to be real on this reading.
narrative_ontology:constraint_stakeholder(temporal_identity_reading, the_present_self_facing_reinterpretation_temptation, payer,
    moderate, biographical, identity_locked, local).

% Benefit from the present self's discipline in the sense that a continuous, trackable agent persists for them to be. If the present self capitulates to reinterpretation, the future self inherits a fragmented identity-history rather than a continuous one that can be held to account or trusted by its own later stages.
narrative_ontology:constraint_stakeholder(temporal_identity_reading, future_selves_who_inherit_coherent_agency, beneficiary,
    analytical, biographical, analytical, local).

% Other parties who took the original commitment as a basis for their own plans or trust. On this reading their benefit is incidental to the primary mechanism — they gain a more reliable interlocutor as a downstream effect of the agent's internal continuity-work, not because the agent is performing for them.
narrative_ontology:constraint_stakeholder(temporal_identity_reading, interlocutors_relying_on_stated_commitments, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(temporal_identity_reading, interlocutors_relying_on_stated_commitments, observer).

% The earlier self that made the original commitment, effectively setting the terms the later self must answer to. This self no longer exists in any directly interrogable form; its authority over the present self is constituted entirely by the present self's own continuity-preserving effort, not by any external enforcement it could apply.
narrative_ontology:constraint_stakeholder(temporal_identity_reading, predicting_self_at_time_of_commitment, agenda_setter,
    moderate, immediate, trapped, local).

% The sibling readings that locate the cost of commitment-keeping in outside observability or social sanction. On this reading their framings are treated as describing a different, and in the zero-observer case vacuous or incoherent, phenomenon — they are not part of the conversation this reading is having about intrapersonal continuity.
narrative_ontology:constraint_stakeholder(temporal_identity_reading, legibility_and_enforcement_framings, excluded,
    analytical, biographical, analytical, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temporal_identity_reading, diffuse).
narrative_ontology:fixing_cost_class(temporal_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of an agent being trustworthy to itself and others across time by making the maintenance of psychological continuity the thing that is achieved, rather than treating commitment-keeping as compliance with an externally monitored rule.
% TRANSFER_FUNCTION: Moves effort from the present self's momentary preference for reinterpretation to the maintenance of continuity with the predicting self's terms; downstream, this effort transfers reliability to interlocutors and to the agent's own future stages, but that transfer is a byproduct, not the mechanism.
% ABSENT_VOICES: The legibility and enforcement-deflation readings would object that a cost with no possible outside observer is either unmeasurable or a category error — they are structurally excluded from this reading's framework because it explicitly denies that outside observability is the relevant observable.
% DISAPPEARANCE_RATIONALE: If the requirement to maintain continuity with one's own past commitments vanished, the present self would face no internal effort in reinterpreting confessions at will — under this reading the world clearly rearranges (agents become unaccountable to their own past states). Adherents of the sibling readings would say nothing changes if no one is watching, so the verdict is genuinely contested across readings of the same kernel.
% FOUNDING_PROBLEM: The philosophical problem that an agent's later self can always retroactively reinterpret an earlier commitment to its own advantage, which threatens to make 'commitment' meaningless unless something makes reinterpretation costly independent of whether anyone else is watching.
% FOUNDING_PROBLEM_CORROBORATION: Philosophers of personal identity working on diachronic agency (outside any party who benefits from a specific confession being honored) attest that the predicting-self/judging-self split is a live problem in the literature; no confessing party's own testimony is treated as sufficient corroboration on this reading, precisely because self-report is the thing under question.
narrative_ontology:disappearance_verdict(temporal_identity_reading, contested).
narrative_ontology:founding_problem_status(temporal_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temporal_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(temporal_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(temporal_identity_reading, 0.28, 'claude-sonnet-5', 'omega_production_confession_kernel_20260814_211528', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_identity_reading_tests).
:- end_tests(temporal_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.28) because the cost is real but not concentrated into a rent extracted by an identifiable external party — it is effort the present self pays to its own earlier self, closer to a coordination cost of maintaining a coherent agent over time than to predation. Suppression is moderate (0.35): the 'enforcement' here is internal (the discomfort of self-contradiction, the felt weight of the earlier self's claim), not external coercion, and this reading explicitly treats that internal pressure as sufficient without any external suppressive apparatus. Accessibility collapse (0.4) is moderate: the present self genuinely could reinterpret at any moment (the 'temptation' is real and available), which distinguishes this from a mountain where no alternative exists. Resistance (0.55) reflects that reinterpretation-temptation is a live, recurring pressure the agent must actively resist, not a settled matter.
 *
 * PERSPECTIVAL GAP:
 *   From the present self's seat, maintaining continuity looks like effortful self-binding with no external audience required to make it meaningful — a tangled rope where the coordination function (a continuous, accountable agent existing across time) is inseparable from the extraction-like cost (constant vigilance against one's own drift). From an analytical seat (future selves, interlocutors) the same structure looks more like pure coordination benefit, since they only see the downstream reliability, not the intrapersonal cost that produced it.
 *
 * DIRECTIONALITY LOGIC:
 *   The present self is the primary payer because the entire load-bearing cost, on this reading, is borne by the agent maintaining continuity against its own temptation — there is no external party to displace the cost onto. Future selves and interlocutors are coded as beneficiaries because they receive a more coherent, trustworthy agent as a byproduct, but this is explicitly NOT the mechanism this reading claims does the work (which is why enforcement_deflation_reading is a distinct, excluded story rather than folded in here).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists the collapse of 'keeping a commitment' into either pure obedience-theater (which would make it a piton once no one is watching) or pure social performance (which would make it a snare wielded by whoever monitors compliance) by locating the achievement inside the agent's relation to its own past state. This is precisely what prevents the framework from misclassifying private, unobserved commitment-keeping as either meaningless (nothing to detect) or as leftover ritual (nothing functional left) — on this reading it remains functional even in the limit of zero observers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diachronic_identity_metaphysics_dependency,
    'Does this reading''s cost-location claim depend on a substantive metaphysical thesis about personal identity over time (e.g., psychological continuity theory) being true, or does it survive under deflationary/Parfitian views where strict identity over time is denied?',
    'Philosophical analysis of whether the ''achievement of continuity'' framing can be restated in reductionist terms (overlapping chains of psychological connectedness) without losing its distinguishing claim against the legibility and enforcement readings.',
    'If the cost-location claim requires strong diachronic identity, a Parfitian rejection of that metaphysics would collapse this reading into something closer to the enforcement_deflation_reading (since without a robust ''earlier self'' there is no one to remain answerable to except via social/institutional proxies).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diachronic_identity_metaphysics_dependency, conceptual, 'Whether the reading is metaphysically load-bearing or restatable in deflationary terms.').

omega_variable(
    unobservable_cost_measurement_problem,
    'Since this reading explicitly locates the cost where no outside observer could in principle verify it, how could the effective extraction (chi) attributed to this reading ever be empirically distinguished from zero, versus from the sibling readings'' claim that such a cost is incoherent?',
    'First-person report, behavioral proxies under controlled temptation-to-reinterpret conditions (e.g., private commitment devices with no social stakes), or converging phenomenological reports across agents with no incentive to signal.',
    'If no measurement strategy can distinguish this reading''s claimed cost from zero, the reading remains conceptually coherent but empirically underdetermined relative to its siblings, which is itself the committer-structure disagreement this kernel exists to represent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unobservable_cost_measurement_problem, empirical, 'Whether a genuinely unobservable intrapersonal cost is measurable even in principle.').

omega_variable(
    beneficiary_status_of_future_selves,
    'Is it coherent to name ''future selves'' as beneficiaries distinct from the present-self payer, or does this covertly reintroduce an external-observer structure (the future self observing the present self) that the reading claims to avoid?',
    'Conceptual clarification of whether intrapersonal temporal relations count as ''external'' for purposes of this reading''s zero-observer claim, or whether only synchronic third parties count.',
    'If future selves count as external observers, this reading''s distinctiveness from legibility_reading weakens substantially, since legibility to one''s own future self is structurally similar to legibility to any other party.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_status_of_future_selves, conceptual, 'Whether naming future selves as beneficiaries undermines the zero-observer claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_identity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temporal_identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(temp_tr_t6, temporal_identity_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(temp_tr_t12, temporal_identity_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(temp_tr_t18, temporal_identity_reading, theater_ratio, 18, 0.14).
narrative_ontology:measurement(temp_tr_t24, temporal_identity_reading, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temporal_identity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(temp_be_t6, temporal_identity_reading, base_extractiveness, 6, 0.22).
narrative_ontology:measurement(temp_be_t12, temporal_identity_reading, base_extractiveness, 12, 0.25).
narrative_ontology:measurement(temp_be_t18, temporal_identity_reading, base_extractiveness, 18, 0.27).
narrative_ontology:measurement(temp_be_t24, temporal_identity_reading, base_extractiveness, 24, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temporal_identity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temporal_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(temporal_identity_reading, 0.1).
narrative_ontology:affects_constraint(temporal_identity_reading, legibility_reading).
narrative_ontology:affects_constraint(temporal_identity_reading, enforcement_deflation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the commitment_cost_location kernel. legibility_reading locates the cost in what an outside party can infer from the agent's behavior; enforcement_deflation_reading locates it in social/institutional sanction machinery; this story (temporal_identity_reading) locates it in the agent's own diachronic relation to its earlier committing self, and is the only one of the three that remains non-vacuous under a stipulated zero-observer condition. Each carries an independently authored ε — they are not to be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
