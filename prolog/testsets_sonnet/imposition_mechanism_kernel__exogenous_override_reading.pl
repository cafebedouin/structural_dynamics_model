% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__exogenous_override_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: imposition_mechanism_kernel__exogenous_override_reading
 *   human_readable: State-Imposed Norm Change via Monopoly on Violence (Exogenous Override Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the exogenous-override reading of the
 *   imposition_mechanism_kernel: a new norm — a legal code, land-tenure
 *   regime, ritual standard, or administrative practice — is imposed by a
 *   central state on a territory where the prior norm was culturally rooted,
 *   and the new norm's warrant rests explicitly on the state's monopoly on
 *   organized violence rather than on any process of cultural adoption. This
 *   is structurally distinct from the endogenous_climb_reading (where popular
 *   practice precedes and the state ratifies) and the
 *   hybrid_legitimation_reading (where symbolic transfer and incentive
 *   alignment do real legitimating work). In THIS reading, compliance never
 *   detaches from monitoring: rising suppression_requirement and rising
 *   theater_ratio over the interval trace an enforcement apparatus that must
 *   work harder over time to sustain a norm that never achieves independent
 *   cultural traction — the opposite trajectory from a norm undergoing
 *   genuine internalization.
 *
 * KEY AGENTS:
 *   - central_state_apparatus: primary agenda_setter (institutional/analytical) — imposes the norm and bears enforcement cost
 *   - loyalist_administrative_class: beneficiary (powerful/constrained) — captures office and revenue from enforcing the imposed order
 *   - traditional_local_elites: payer (moderate/trapped) — displaced adjudicative authority
 *   - noncompliant_subject_populations: payer (powerless/trapped) — bears the cost of forced dual compliance
 *   - state_monitoring_corps: agenda_setter/beneficiary (organized/constrained) — structurally invested in the norm remaining contested
 *   - displaced_cultural_authorities: excluded (powerless/trapped) — has no forum to contest the override
 *   - later_historians_and_administrators: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.68).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.87).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "State-Imposed Norm Change via Monopoly on Violence (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, '1f290506-0427-4171-9671-af397379c22c').
narrative_ontology:cs_kernel_codification('1f290506-0427-4171-9671-af397379c22c', implicit).
narrative_ontology:cs_authority_grounding('1f290506-0427-4171-9671-af397379c22c', extraction).
narrative_ontology:cs_interpretation_layer_present('1f290506-0427-4171-9671-af397379c22c').
narrative_ontology:cs_reading_relation('1f290506-0427-4171-9671-af397379c22c', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f290506-0427-4171-9671-af397379c22c', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('1f290506-0427-4171-9671-af397379c22c', foundational, coercive_capacity_sufficient_for_normative_authority).
narrative_ontology:cs_axiom_status(coercive_capacity_sufficient_for_normative_authority, holdable).
narrative_ontology:cs_axiom_grounding('1f290506-0427-4171-9671-af397379c22c', coercive_capacity_sufficient_for_normative_authority, conventional).
narrative_ontology:cs_axiom('1f290506-0427-4171-9671-af397379c22c', secondary, cultural_acceptance_not_required_for_binding_force).
narrative_ontology:cs_axiom_status(cultural_acceptance_not_required_for_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('1f290506-0427-4171-9671-af397379c22c', cultural_acceptance_not_required_for_binding_force, instrumental).
narrative_ontology:cs_reference_frame('1f290506-0427-4171-9671-af397379c22c', customary_normative_order).
narrative_ontology:cs_drift_state('1f290506-0427-4171-9671-af397379c22c', post_imposition_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1f290506-0427-4171-9671-af397379c22c', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, central_state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, loyalist_administrative_class).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, traditional_local_elites).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, noncompliant_subject_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, state_monitoring_corps).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the new norm by decree and backs it with the coercive apparatus — garrisons, tax officials, courts, registries. It does not wait for or cultivate popular buy-in; it treats the monopoly on organized violence as sufficient warrant to declare the old norm void and the new one binding. It bears the ongoing cost of monitoring compliance because the norm has no independent cultural momentum.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, central_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Local officials, tax farmers, and appointees whose positions exist because they enforce and interpret the imposed norm on the state's behalf. They gain office, revenue shares, and protection in exchange for compliance-policing; their standing collapses if the imposed order is ever rolled back, so they have every incentive to report resistance and suppress it.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, loyalist_administrative_class, beneficiary,
    powerful, biographical, constrained, regional).

% Held authority under the prior, culturally-rooted norm — as clan heads, religious authorities, or customary judges. The new norm strips their adjudicative and symbolic functions and transfers them to state-appointed officials. They cannot exit the territory the state controls, and open defiance invites confiscation or violence; some are absorbed into the new order at reduced status, most are simply displaced.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, traditional_local_elites, payer,
    moderate, biographical, trapped, regional).

% Ordinary households and communities who continue practicing the old norm privately while performing compliance publicly under surveillance. They bear fines, corporal punishment, or land seizure for detected noncompliance, and carry the cost of maintaining a double life between private custom and public conformity as long as monitoring persists.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, noncompliant_subject_populations, payer,
    powerless, generational, trapped, local).

% Inspectors, census-takers, and local garrisons tasked with detecting deviation from the imposed norm. Their continued employment and standing depend on the norm remaining actively contested — a fully internalized norm would make their function redundant, so they have a structural stake in compliance remaining monitoring-dependent rather than self-sustaining.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, state_monitoring_corps, agenda_setter,
    organized, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__exogenous_override_reading, state_monitoring_corps, beneficiary).

% Priests, elders, and customary specialists whose entire social role was constituted by the old norm. They are not consulted in the decree process and have no institutional channel to contest it; their objection is that the new norm has no cultural warrant at all, only force, but this argument has no forum in which to be heard.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, displaced_cultural_authorities, excluded,
    powerless, generational, trapped, local).

% Assess after the fact whether the imposed norm ever achieved genuine cultural acceptance or remained perpetually monitoring-dependent until either full suppression succeeded or the state's coercive capacity declined and the norm receded or converted into something hybrid.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, later_historians_and_administrators, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the state a uniform administrative and legal baseline across a territory that previously operated under fragmented customary norms — a single tax, land-tenure, or ritual standard that centralized bureaucracy can process without needing local translation.
% TRANSFER_FUNCTION: Moves adjudicative authority, tribute/tax flows, and symbolic status from traditional local elites and customary institutions to the central state apparatus and its loyalist administrative appointees; moves compliance costs and cultural loss onto subject populations and displaced authorities.
% ABSENT_VOICES: Displaced cultural authorities and the communities whose customary norm is being overridden have no seat in the decree process — the norm is announced and enforced, not negotiated. Their objection, that force is not the same as legitimacy, is structurally excluded from the record that produced the norm.
% DISAPPEARANCE_RATIONALE: Remove the monitoring apparatus and enforcement capacity, and compliance collapses quickly back toward the prior customary norm in most localities — because the new norm never acquired independent cultural traction, its persistence is contingent entirely on continued coercive backing, not on internalized acceptance.
% FOUNDING_PROBLEM: The state needed a single administrable norm across a territory whose fragmented customary orders resisted central taxation, conscription, or legal uniformity — coordination costs under fragmented custom were unacceptable to the center's governance project.
% FOUNDING_PROBLEM_CORROBORATION: The central state apparatus and loyalist administrators attest the imposed norm has become settled practice, citing decades of registered compliance. Displaced cultural authorities and independent historical-sociological accounts of enforcement records (tax revolt suppression logs, garrison deployment patterns persisting generations after the decree) attest that compliance has remained monitoring-conditional throughout, not a marker of accepted legitimacy — this is the corroboration from outside the beneficiary set.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__exogenous_override_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is moderately-high and rising slowly — the norm change genuinely redistributes adjudicative and fiscal authority toward the center, which is a real transfer, not merely rent extraction, so it does not reach snare-level intensity. Suppression (0.87) is high and near its ceiling because in this reading legitimacy is EXPLICITLY not derived from acceptance — the entire justificatory structure is monopoly-on-violence, which requires continuous, hardening enforcement infrastructure rather than a one-time compliance push. Theater ratio (0.42) is moderate: some enforcement activity is genuine administrative function (record-keeping, dispute resolution under the new code) but a growing share becomes performative compliance-signaling (public rituals of obedience) that mask persistent private non-adoption. Accessibility collapse is only moderate (0.5) — the old norm has NOT been extinguished culturally, only suppressed publicly, which is exactly what the exogenous_override reading predicts and what distinguishes it from a genuine mountain-like natural transition.
 *
 * PERSPECTIVAL GAP:
 *   From the central_state_apparatus seat, this looks like successful state-building — a coordination problem (fragmented custom, ungovernable jurisdiction) genuinely solved by unifying the norm. From the traditional_local_elites and noncompliant_subject_populations seats, the same structure is naked coercive imposition with a coordination story used as cover. The engine should compute a tangled_rope from the aggregate structural data (both a real coordination function and a real, enforced asymmetric extraction are present) while the per-seat divergence itself is the analytical payload — that divergence is what the exogenous_override reading is FOR: it names the case where legitimacy is asserted without being earned, and the classification should register the coercion as load-bearing, not incidental.
 *
 * DIRECTIONALITY LOGIC:
 *   The central state and its administrative appointees sit near the beneficiary end: they set the rule, staff its enforcement, and capture the redistributed authority and revenue. Traditional elites and subject populations sit near the target end: trapped by territorial control, they pay in status, tribute, and coercive cost with no meaningful exit. The state_monitoring_corps occupies an unusual dual position — nominally an arm of the agenda_setter, but structurally a beneficiary of the norm's NON-completion, since a fully internalized norm would eliminate their function; this is worth flagging as an internal tension within the beneficiary coalition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented customary jurisdiction ungovernable from the center) is genuinely live at t=0, which is why this does not present as a pure snare — there is a real coordination problem being addressed. But the founding_problem_status is authored 'contested' because the mechanism used to solve it (coercive override rather than legitimation) never resolves into self-sustaining compliance; the rising suppression_requirement trajectory is exactly the signature of a constraint whose coordination rationale persists while its LEGITIMACY never converts from asserted to accepted. This prevents the story from being mislabeled a pure mountain (natural, inevitable state consolidation) or a pure rope (voluntary coordination) — it is coordination purchased entirely by force, sustained only as long as the force is sustained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_evidentiary_basis,
    'What historical evidence would distinguish the exogenous_override_reading from the hybrid_legitimation_reading for any given case — i.e., how do we know legitimacy was NOT partially achieved through symbolic transfer or incentive alignment, and rested purely on coercive capacity?',
    'Examine whether compliance persisted or collapsed during documented periods of temporary state weakness (succession crises, military overextension, fiscal collapse). If compliance collapsed sharply whenever monitoring capacity dipped, the exogenous_override reading is supported; if compliance persisted through such gaps, a hybrid or endogenous reading better fits the case, and this story should be understood as one candidate reading rather than the settled account.',
    'If a hybrid mechanism was actually operative, applying the pure-override reading overstates suppression''s causal centrality and understates the coordination function''s independent legitimacy — reclassification toward tangled_rope with lower suppression or toward rope would follow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_evidentiary_basis, empirical, 'Whether the coercion-only mechanism is empirically distinguishable from hybrid legitimation in the historical record.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the three kernel readings (endogenous_climb, exogenous_override, hybrid_legitimation) disagree — is it the SEQUENCE of legitimacy formation (bottom-up vs top-down), the SOURCE of legitimacy (custom vs violence vs symbolic transfer), or both?',
    'Structural decomposition of each reading''s founding_problem_corroboration: the endogenous reading would be corroborated by evidence of pre-existing popular practice antedating the decree; the hybrid reading by evidence of symbolic ritual investment (e.g., ruler''s personal exemplar) plus material incentive structures; this reading by evidence that compliance tracks monitoring intensity rather than either popular practice or symbolic investment.',
    'Clarifies that this is not a single contested fact but a genuine three-way structural fork — the same historical episode can be read through any of the three lenses depending on which evidentiary thread (sequence, incentive, or coercion-intensity) is weighted as causally primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'The specific structural axis along which the three kernel readings diverge.').

omega_variable(
    monitoring_apparatus_self_interest,
    'Does the state_monitoring_corps'' structural interest in the norm remaining monitoring-dependent (rather than fully internalized) create an internal incentive to SUPPRESS the very cultural adoption that would end their function — i.e., is non-internalization partly manufactured rather than merely observed?',
    'Compare enforcement intensity in regions where local internalization appears to be emerging organically versus regions where it is not; if enforcement intensifies specifically where internalization is nascent, this suggests active suppression of legitimation rather than neutral monitoring.',
    'If confirmed, elevates the tangled_rope classification toward snare-like territory for the monitoring apparatus specifically, since its coordination cover (verifying compliance) would be masking active extraction (self-perpetuating its own necessity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monitoring_apparatus_self_interest, empirical, 'Whether the enforcement corps has an incentive to prevent the norm''s own success.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(impo_tr_t10, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(impo_tr_t30, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(impo_tr_t50, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(impo_tr_t60, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(impo_be_t10, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(impo_be_t30, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(impo_be_t50, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(impo_be_t60, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(impo_su_t10, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(impo_su_t20, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(impo_su_t30, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 40, 0.86).
narrative_ontology:measurement(impo_su_t50, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 50, 0.87).
narrative_ontology:measurement(impo_su_t60, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 60, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% Three sibling constraints decompose the natural-language concept 'how new state norms acquire legitimacy' along the source-of-legitimacy axis, per the ε-invariance principle: endogenous_climb_reading (legitimacy from bottom-up cultural adoption, state ratifies after), exogenous_override_reading (THIS STORY — legitimacy asserted purely from monopoly on violence, compliance remains monitoring-conditional), and hybrid_legitimation_reading (legitimacy from symbolic authority transfer plus institutional incentive alignment). Each carries a distinct, stable ε and distinct suppression/theater trajectories; they are not the same constraint measured three ways but three structurally distinct claims about the same class of historical episode.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
