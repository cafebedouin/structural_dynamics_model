% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__exogenous_override_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: imposition_pathway_kernel__exogenous_override_reading
 *   human_readable: State Imposition of New Commitment Without Fringe Adoption Pathway
 *   domain: historical/institutional/commitment-system
 *
 * SUMMARY:
 *   The Meiji Restoration provides the historical locus: Japan shifted from a
 *   lunar calendar to the Gregorian calendar in 1872 through state decree,
 *   with no prior fringe adoption pathway. The same state apparatus mandated
 *   Western-style dress, reorganized names, and restructured measurement
 *   systems — all without evidence that these commitments were climbing from
 *   grassroots adoption. The exogenous_override reading asserts that state
 *   capacity enables commitment displacement through pure coercive override:
 *   the state selects a commitment, mandates it, and enforces compliance
 *   without requiring organic fringe adoption. This reading contests the
 *   endogenous_climb framing (which compresses all apparent overnight changes
 *   into invisible fringe stages) and the hybrid_cascade framing (which
 *   allows state-initiated artificial fringe to bootstrap organic climb).
 *   This constraint story instantiates ONLY the exogenous_override reading as
 *   a clean, internally consistent constraint with its own ε,
 *   beneficiary/victim structure, and cs_structure axioms.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: institutional power, enforces the new commitment decree against the entire population.
 *   - commitment_unifiers: institutional power, ideological beneficiaries who believe the new commitment is superior and leverage state enforcement to impose it.
 *   - subject_population: powerless, trapped, forced to reframe cognitive and social commitments without voluntary adoption pathway.
 *   - competing_commitment_holders: moderate power, constrained exit, lose competitive advantage under the new commitment.
 *   - historical_analysts: observers measuring whether the mechanism is exogenous override or disguised endogenous climb.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, 0.67).
domain_priors:suppression_score(imposition_pathway_kernel__exogenous_override_reading, 0.78).
domain_priors:theater_ratio(imposition_pathway_kernel__exogenous_override_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__exogenous_override_reading, "State Imposition of New Commitment Without Fringe Adoption Pathway").
narrative_ontology:topic_domain(imposition_pathway_kernel__exogenous_override_reading, "historical/institutional/commitment-system").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__exogenous_override_reading, 'fde79190-85a8-4f3f-92b9-7f0b2aae5801').
narrative_ontology:cs_kernel_codification('fde79190-85a8-4f3f-92b9-7f0b2aae5801', distributed).
narrative_ontology:cs_authority_grounding('fde79190-85a8-4f3f-92b9-7f0b2aae5801', extraction).
narrative_ontology:cs_interpretation_layer_present('fde79190-85a8-4f3f-92b9-7f0b2aae5801').
narrative_ontology:cs_reading_relation('fde79190-85a8-4f3f-92b9-7f0b2aae5801', imposition_pathway_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('fde79190-85a8-4f3f-92b9-7f0b2aae5801', imposition_pathway_kernel__hybrid_cascade_reading, forecloses).
narrative_ontology:cs_axiom('fde79190-85a8-4f3f-92b9-7f0b2aae5801', foundational, fringe_adoption_precondition_unnecessary).
narrative_ontology:cs_axiom_status(fringe_adoption_precondition_unnecessary, holdable).
narrative_ontology:cs_axiom_grounding('fde79190-85a8-4f3f-92b9-7f0b2aae5801', fringe_adoption_precondition_unnecessary, empirically_contingent).
narrative_ontology:cs_axiom('fde79190-85a8-4f3f-92b9-7f0b2aae5801', foundational, coercive_mechanism_reducibility_false).
narrative_ontology:cs_axiom_status(coercive_mechanism_reducibility_false, holdable).
narrative_ontology:cs_axiom_grounding('fde79190-85a8-4f3f-92b9-7f0b2aae5801', coercive_mechanism_reducibility_false, empirically_contingent).
narrative_ontology:cs_reference_frame('fde79190-85a8-4f3f-92b9-7f0b2aae5801', distributed_fringe_adoption_climb).
narrative_ontology:cs_drift_state('fde79190-85a8-4f3f-92b9-7f0b2aae5801', meiji_imposition_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fde79190-85a8-4f3f-92b9-7f0b2aae5801', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, commitment_unifiers).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, subject_population).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, competing_commitment_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Selects a new commitment (calendar system, dress code, naming convention, measurement standard) and mandates it through decree. Enforces compliance via bureaucratic oversight, licensing withdrawal, and coercion. Does not require evidence that the commitment was already climbing through fringe adoption; the apparatus can impose without that precondition. Benefits from standardization that reduces administrative burden and coordinates the population under a single authoritative system.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Must adopt the new commitment on pain of legal sanction, employment loss, or physical coercion. No fringe adoption pathway preceded the decree that they could have climbed organically. Compliance is extracted through the state's monopoly on force and administrative authority, not through conviction or voluntary adoption. The cost includes cognitive reframing, economic adjustment, social isolation if they resist, and loss of autonomy over their own commitment choices.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, subject_population, payer,
    powerless, biographical, trapped, national).

% Ideological or administrative actors (reformers, planners, modernizers) who conceive the new commitment as progress or rationality. They benefit from the state apparatus's coercive capacity to impose their preferred order without negotiating with fringe populations or waiting for organic adoption. The constraint solves their coordination problem: how to shift the entire population to a new commitment all at once, bypassing the slow endogenous climb mechanism.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, commitment_unifiers, beneficiary,
    institutional, generational, analytical, national).

% Practitioners, merchants, clergy, or professionals who benefited from the old commitment (lunar calendar, traditional dress, prior naming conventions). They lose their competitive advantage, face retraining costs, and see their authority diminished when the new commitment is mandated. They may resist, but the state apparatus's enforcement capacity suppresses their exit and alternative.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, competing_commitment_holders, payer,
    moderate, biographical, constrained, national).

% In the endogenous_climb reading, fringe populations voluntarily adopt the new commitment first; but in this reading (exogenous_override), no such fringe exists. They are excluded from the story because the constraint does NOT operate through fringe mechanism. Their absence is the analytical point: the state can impose without them.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, fringe_adoption_counterparties, excluded,
    powerless, biographical, identity_locked, regional).

% Examine the historical record to determine whether the commitment shift occurred through (1) endogenous fringe climb compressed into apparent overnight change, (2) pure exogenous state override, or (3) hybrid (override initiates, climb completes). This reading's claim is that category (2) is a distinct, non-decomposable mechanism — the exogenous override cell is necessary in the M-set to capture cases where no fringe adoption pathway precedes the decree.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__exogenous_override_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardization of a focal commitment (calendar, dress, measurement) across the entire population, enabling uniform administrative coordination and eliminating fragmentation that impedes state control and economic efficiency.
% TRANSFER_FUNCTION: Moves the costs of commitment change (cognitive reframing, economic adjustment, social dislocation, loss of autonomy) from the state apparatus and commitment-unifiers to the subject population. Moves the benefits (standardization, coordination, reduced administrative friction, ideological victory) to the state and its planning class.
% ABSENT_VOICES: Practitioners of the old commitment are suppressed by enforcement machinery rather than excluded from conversation — they are silenced and forced to comply. Fringe populations who might have organically adopted are absent because they did not exist prior to the decree; their absence is a structural fact about this reading, not a matter of who gets to speak.
% DISAPPEARANCE_RATIONALE: If the decree vanished and enforcement ceased, the subject population would revert to the old commitment within months or years. The new commitment persists only because state enforcement continuously reinforces it; without coercion, the equilibrium shifts back to the pre-decree state.
% FOUNDING_PROBLEM: The state's need to impose internal standardization to consolidate administrative control and enable modern bureaucratic coordination; the old commitment fragmentation impedes state capacity.
% FOUNDING_PROBLEM_CORROBORATION: State administrators and reform ideologues attest this founding problem is live. Historians outside the state benefiting set (including comparative historians and sociologists studying state formation and modernization) corroborate that state standardization of commitments was a persistent agenda across Meiji and comparable cases. The founding problem is corroborated by non-beneficiary analytical observers.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__exogenous_override_reading, 0.67, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.67 at steady state because the constraint transfers the full cognitive and social cost of commitment change to the subject population while concentrating benefits (standardization, administrative efficiency, ideological victory) with the state and commitment-unifiers. Suppression is 0.78 because enforcement is continuous and costly — the state must maintain a commitment that would revert if enforcement slipped. Theater is low (0.22) because the functional operation (coercive standardization) matches the declared justification (state modernization); there is real work performed by the enforcement machinery, not merely performative maintenance. The measurement series shows extractiveness rising sharply in years 0–10 (the acute displacement period when resistance is highest) and then plateauing at 0.67 (steady-state extraction as the population adapts but compliance remains coerced, not internalized). Suppression likewise rises sharply and plateaus, indicating structural (not internalized) enforcement throughout. The claim/metric gap is intentional: the constraint is CLAIMED as tangled_rope (genuine coordination function — standardization — plus asymmetric extraction) while the authored metrics describe coercive imposition without meaningful prior adoption. The engine measures that divergence.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus seat perceives this as rope: a genuine coordination problem (fragmented commitments impede state control) solved through hierarchical enforcement. The subject population seat perceives it as snare: no coordination problem for them, pure extraction of compliance. The commitment-unifiers sit at the beneficiary end (they wanted the new commitment all along); the competing-commitment-holders sit at the target end (their old competitive advantage is destroyed). Historians sit outside the constraint entirely, observing whether the exogenous_override mechanism is real or whether all three readings are fictions masking a single endogenous climb. The engine computes different seat-level types from the structural data (beneficiary/victim + power + exit + scope) without authoring the types directly; the divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus is the agenda_setter at full institutional power with arbitrage exit (it could choose other commitments or no new commitment; it chooses to impose this one because it benefits). Direction d ≈ 0.2 (slight beneficiary bias, constrained by the requirement to enforce). Commitment-unifiers also sit near the beneficiary end (they wanted this outcome; they benefit from state capacity that enables it without negotiating with fringe populations). The subject population are full targets: they did not choose this commitment, they bear the full cost of reframing, and they have trapped exit (d ≈ 0.95). Competing-commitment-holders are targets for the same reasons (d ≈ 0.85). The directionality derivation from beneficiaries (state_apparatus, commitment_unifiers) + victims (subject_population, competing_commitment_holders) + exit options (trapped, constrained) produces a strong asymmetry; no override is necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (state need for internal standardization) remains live at t40 — the state continues to maintain the commitment because fragmentation still impedes control. However, the FOUNDING MECHANISM (coercive override) has become partially atrophied: by t40, the subject population has largely internalized the new commitment, so active suppression becomes increasingly theatrical (enforcement machinery remains but many subjects comply from habituation, not fear). The theater_ratio plateau at 0.22 reflects this partial atrophy: roughly one-fifth of the enforcement machinery is now maintaining the appearance of coercive necessity rather than actual coercion. The constraint has not resolved mandatrophy (founding_problem is still live, compliance is still enforced) but shows early signs of moving toward piton (theatrical maintenance increasing as internalization increases). The measurement trajectory would continue to inform: if theater_ratio rises above 0.5 while suppression remains stable, the constraint reclassifies to piton (a commitment that is believed-in by enough of the population that enforcement has become vestigial).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_adoption_visibility_question,
    'Is the apparent absence of fringe adoption evidence that none occurred, or evidence that it was too marginal to be recorded in state documents?',
    'Microhistorical reconstruction from merchant records, religious texts, folk practices, and non-state archives. Detection of measurable pre-decree adoption in hidden records.',
    'If fringe adoption is recovered, exogenous_override reading is weakened; constraint reclassifies toward endogenous_climb. If absent at measurable scale, exogenous_override is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_adoption_visibility_question, empirical, 'Pre-decree fringe adoption: absence of record vs. genuine absence.').

omega_variable(
    commitment_unifier_coercion_source,
    'Do commitment-unifiers genuinely believe the new commitment is superior, or do they coerce adoption as social control disguised as modernization?',
    'Analysis of reformer writings and institutional motivations. Examination of whether alternatives were available and actively rejected.',
    'If genuinely believed, part of measured extraction is legitimacy-loss. If cover for coercion, extraction is concentrated. The axiom cs_axiom_commitment_superiority status depends on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_unifier_coercion_source, conceptual, 'Reformer motivation: genuine superiority belief vs. coercive control cover.').

omega_variable(
    kernel_reading_framing_dependence,
    'Does pure exogenous override depend on a narrow definition of ''fringe''? If state-initiated populations count as fringe, does the reading dissolve into hybrid_cascade?',
    'Formal definition of fringe (spontaneous, voluntary, uncoerced, pre-decree, non-state-employee). Examination of definitional boundary.',
    'This is axiom_overriding: fringe definition distinguishes exogenous_override from hybrid_cascade. Relaxed definition forecloses pure exogenous override. Narrow definition preserves reading distinctness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing_dependence, conceptual, 'Definitional boundary between exogenous_override and hybrid_cascade in M-set.').

omega_variable(
    suppression_internalization_over_time,
    'Does the subject population internalize the new commitment after decades, so compliance becomes voluntary, or does compliance remain dependent on continuous suppression?',
    'Post-enforcement trajectory: relaxation of enforcement and compliance persistence. Second-generation subjects'' view of commitment as native vs. imposed.',
    'If internalized, effective extraction decreases, constraint reclassifies toward rope. If structural throughout, tangled_rope classification confirmed. Measurement plateau at t15+ suggests structural mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_over_time, empirical, 'Suppression: structural or internalized (post-compliance compliance).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(impo_tr_t5, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(impo_tr_t15, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(impo_tr_t25, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(impo_tr_t40, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(impo_be_t5, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(impo_be_t15, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(impo_be_t25, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(impo_be_t40, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 40, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(impo_su_t5, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(impo_su_t15, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 15, 0.79).
narrative_ontology:measurement(impo_su_t25, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(impo_su_t40, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the imposition_pathway_kernel. The endogenous_climb reading asserts all apparent state impositions conceal fringe adoption pathways. The hybrid_cascade reading asserts state imposition initiates artificial-fringe climb completion. This exogenous_override reading asserts state capacity enables pure coercive displacement without fringe precondition — a distinct M-set cell. All three readings are live in sociological discourse; the kernel itself (how do commitments displace at state scale?) remains contested. Each reading story carries its own ε, beneficiary/victim structure, and cs_structure axioms. The readings are linked via network.affects_constraints; they are not consolidated into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
