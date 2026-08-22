% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__hybrid_reading, []).

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
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Preparedness as Layered System: Memorial Commitment plus Competence Maintenance
 *   domain: disaster preparedness / institutional memory / commitment systems
 *
 * SUMMARY:
 *   A regional emergency-management system combines annual disaster
 *   commemorations with mandatory technical certification exercises. Civil
 *   defense agencies and long-tenure responders administer both layers;
 *   memorial custodians depend institutionally on the commemorative layer's
 *   continued prominence. Frontline participants, junior staff, and
 *   budget-constrained municipalities bear the combined cost of maintaining
 *   two parallel obligations from one shared pool of time and money. The
 *   hybrid reading holds that removing either layer would degrade the system
 *   as a whole — but does not deny that the layers compete for resources and
 *   that this competition is itself extractive to those who fund and staff
 *   the base of the structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.42).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.38).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Preparedness as Layered System: Memorial Commitment plus Competence Maintenance").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "disaster preparedness / institutional memory / commitment systems").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, '3eeecffc-efd3-47ff-a787-2568c932ac33').
narrative_ontology:cs_kernel_codification('3eeecffc-efd3-47ff-a787-2568c932ac33', distributed).
narrative_ontology:cs_authority_grounding('3eeecffc-efd3-47ff-a787-2568c932ac33', practice).
narrative_ontology:cs_interpretation_layer_present('3eeecffc-efd3-47ff-a787-2568c932ac33').
narrative_ontology:cs_reading_relation('3eeecffc-efd3-47ff-a787-2568c932ac33', preparedness_commitment__husk_reading, influences).
narrative_ontology:cs_reading_relation('3eeecffc-efd3-47ff-a787-2568c932ac33', preparedness_commitment__competence_reading, influences).
narrative_ontology:cs_axiom('3eeecffc-efd3-47ff-a787-2568c932ac33', foundational, memorial_and_competence_layers_are_both_functionally_necessary).
narrative_ontology:cs_axiom_status(memorial_and_competence_layers_are_both_functionally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('3eeecffc-efd3-47ff-a787-2568c932ac33', memorial_and_competence_layers_are_both_functionally_necessary, instrumental).
narrative_ontology:cs_axiom('3eeecffc-efd3-47ff-a787-2568c932ac33', secondary, layer_tension_is_intrinsic_cost_not_evidence_of_capture).
narrative_ontology:cs_axiom_status(layer_tension_is_intrinsic_cost_not_evidence_of_capture, holdable).
narrative_ontology:cs_axiom_grounding('3eeecffc-efd3-47ff-a787-2568c932ac33', layer_tension_is_intrinsic_cost_not_evidence_of_capture, conventional).
narrative_ontology:cs_reference_frame('3eeecffc-efd3-47ff-a787-2568c932ac33', dual_layer_preparedness_doctrine).
narrative_ontology:cs_drift_state('3eeecffc-efd3-47ff-a787-2568c932ac33', contemporary_budget_austerity_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3eeecffc-efd3-47ff-a787-2568c932ac33', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, civil_defense_agencies).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, memorial_ritual_custodians).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, long_tenure_emergency_responders).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, frontline_drill_participants).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, budget_constrained_municipalities).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, junior_responders_facing_dual_burden).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, junior_responders_facing_dual_burden).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, institutional_memory_requires_dual_layering).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and mandates the layered preparedness program: annual memorial observances (disaster anniversaries, commemorative drills) alongside technical competence certifications. Decides the ratio of ceremonial to operational content and controls which layer gets funding priority in any given budget cycle.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, civil_defense_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Museum staff, survivor associations, and commemorative-event organizers whose institutional purpose and funding depend on the memorial layer's continued prominence. They benefit from public attention to anniversaries and would lose relevance if preparedness were reduced to pure technical drilling.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, memorial_ritual_custodians, beneficiary,
    organized, civilizational, identity_locked, regional).

% Veteran responders who hold institutional knowledge of both the ceremonial narrative and the operational procedures. They gain status and job security as the designated bridge between memorial framing and technical competence, and control which younger staff are certified as full inheritors of the practice.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, long_tenure_emergency_responders, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, long_tenure_emergency_responders, agenda_setter).

% Community members and junior staff required to attend both the memorial commemorations and the technical readiness exercises. They bear the time cost of a doubled calendar of obligations, some of which (the memorial portion) does not build any operational skill they can use in an actual emergency.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, frontline_drill_participants, payer,
    moderate, immediate, constrained, local).

% Local governments funding both layers from the same finite emergency-management budget. Every dollar spent maintaining a memorial observance is a dollar not spent on equipment, technical drills, or updated hazard modeling — they cannot easily unbundle the two without appearing to disrespect past disaster victims.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, budget_constrained_municipalities, payer,
    moderate, biographical, constrained, regional).

% New hires who must pass both a memorial-literacy component (knowing the history, participating in commemorations) and a technical competence exam to advance. They benefit eventually from genuine skill acquisition but pay disproportionately in the early career years when the dual requirement is heaviest relative to their institutional standing.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, junior_responders_facing_dual_burden, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, junior_responders_facing_dual_burden, beneficiary).

% The communities whose losses the memorial layer commemorates are rarely consulted on whether the commemorations translate into operational readiness that would actually protect them in a future event. Their interest is in effective preparedness, not necessarily in the specific ceremonial form it takes, but they have no seat in the design process.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, disaster_survivor_communities, excluded,
    powerless, civilizational, trapped, local).

% Study whether memorial and competence layers reinforce or cannibalize each other across jurisdictions, publishing comparative findings on which combinations of ritual and drill produce measurable improvements in disaster outcomes.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, disaster_preparedness_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves two distinct coordination problems with one combined structure: sustaining political and community will to keep funding preparedness across the long gaps between disasters (memorial function), and maintaining actual operational skill so that when a disaster occurs, response capacity has not atrophied (competence function). Neither layer alone reliably survives budget and attention cycles.
% TRANSFER_FUNCTION: Moves time, budget, and attention from participants and municipalities toward the institutions that administer both layers; within that flow, memorial custodians and veteran responders capture disproportionate share relative to the direct benefit returned to frontline participants and survivor communities.
% ABSENT_VOICES: Disaster survivor communities, whose losses anchor the memorial content, are rarely asked whether the commemorative form actually serves their protective interest or has become decoupled from it. Junior staff bearing the heaviest dual-layer time burden have little voice in how the ratio between ceremony and drilling is set.
% DISAPPEARANCE_RATIONALE: Civil defense agencies and memorial custodians argue that if the layered structure disappeared, political will for preparedness funding would collapse within a generation as disaster memory faded (world_rearranges from their view). Researchers and some municipalities argue that a leaner, competence-only structure could maintain adequate readiness at lower cost, meaning the world would not meaningfully change for operational outcomes if the memorial layer specifically were removed (world_unchanged from their view) — hence contested rather than settled.
% FOUNDING_PROBLEM: Early standalone competence-only preparedness programs decayed rapidly once the founding disaster receded from living memory — drills were skipped, budgets were cut, and institutional knowledge was lost between major events. The memorial layer was added to give preparedness a durable social and political anchor that could survive multi-decade gaps between disasters.
% FOUNDING_PROBLEM_CORROBORATION: Civil defense agencies and memorial custodians attest the anchoring problem remains live, citing historical decay episodes. Independent disaster-preparedness researchers, writing from outside the institutions that administer either layer, report mixed evidence: in some jurisdictions the memorial layer measurably sustains funding, in others it has become decoupled from operational content and persists mainly as calendar ritual — corroboration exists but is not unanimous, which is itself part of why the hybrid reading treats the founding problem as only partially resolved.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, contested).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__hybrid_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).
:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than low, because even under the hybrid reading's own charitable framing, the coordination benefit does not fully offset the disproportionate share captured by memorial custodians and senior responders relative to what junior staff and municipalities receive back in protective capacity. Theater ratio is meaningfully elevated (0.40) and rising over the interval, reflecting the hybrid reading's own acknowledgment that the memorial layer's operational contribution is harder to verify than the competence layer's and drifts toward performance under budget pressure — this is the tension the reading explicitly names, not a claim that the whole structure is theater. Suppression is moderate and enforced through certification gating and funding conditionality, not raw coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil defense agencies and long-tenure responders sit near the beneficiary end: they administer the combined system and their institutional standing depends on its continuation. Memorial custodians benefit specifically from the commemorative layer's prominence and are identity-locked to it — their institutional purpose would dissolve if the memorial layer were stripped out, which is why the hybrid reading treats their interest as structurally distinct from mere administration. Frontline participants, junior responders, and municipalities sit toward the target end: they fund and staff both layers without proportionate control over the ratio between them. Junior responders carry a dual role — genuine long-run beneficiaries of the competence they eventually acquire, but disproportionate short-run payers of the combined time burden.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading is explicitly constructed to avoid two mandatrophy failure modes symmetrically: mislabeling the entire structure as pure extraction (the husk_reading's implicit charge) when the competence layer is doing real, verifiable protective work; and mislabeling it as pure efficient coordination (the competence_reading's implicit charge) when the memorial layer imposes real costs that are not fully justified by its stabilizing function alone. Classifying this as tangled_rope rather than rope or snare reflects the hybrid reading's core claim: coordination and extraction are BOTH genuinely present, riding on the same administrative structure, and neither can be assessed by looking at only one layer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_reading_of_preparedness_commitment_kernel,
    'This constraint is one reading (hybrid_reading) of the preparedness_commitment kernel. The sibling readings — competence_reading (preparedness reduces to exercised operational knowledge; memorial elements are unnecessary overhead) and husk_reading (memorial elements have displaced competence entirely, leaving performance without function) — are separate constraints, not alternative measurements of this one. Which reading correctly characterizes any given jurisdiction''s actual preparedness system?',
    'Longitudinal disaster-outcome studies comparing jurisdictions with different memorial-to-competence ratios; if outcomes track competence-layer intensity regardless of memorial-layer intensity, the competence_reading is closer to correct for that population and the memorial layer''s stabilizing claim would need independent verification (e.g. funding-continuity data across multi-decade gaps).',
    'If the memorial layer is shown to have no measurable effect on long-run funding continuity or drill participation, this hybrid_reading''s core premise (that the memorial layer does real stabilizing work) collapses toward the husk_reading for that jurisdiction. If the competence layer is shown to decay without the memorial anchor, the hybrid_reading is vindicated over the competence_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_of_preparedness_commitment_kernel, empirical, 'Kernel-committer disclosure: this story is one reading among three; the sibling readings are separate constraints linked via network.affects_constraints, not alternative measurements of this ε.').

omega_variable(
    memorial_competence_tension_location,
    'Where exactly does the tension between the memorial and competence layers concentrate — is it primarily a budget-allocation tension (money spent on one is unavailable to the other), a time-allocation tension (participant hours), or a legitimacy tension (memorial framing crowding out honest assessment of competence gaps because criticizing drills feels like disrespecting the memorialized dead)?',
    'Structured interviews with municipal budget officers and frontline participants distinguishing which resource is actually the binding constraint in practice, plus content analysis of whether competence failures get publicly named or are softened by memorial framing.',
    'If the tension is mainly legitimacy-based (memorial framing suppressing honest competence critique), the theater_ratio trajectory should be read as a symptom of that suppression mechanism specifically, which would argue for structurally separating evaluation of the two layers rather than merely rebalancing their budgets.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(memorial_competence_tension_location, conceptual, 'Whether the layered tension is fiscal, temporal, or legitimacy-based — affects what intervention would actually address it.').

omega_variable(
    false_summit_check_layered_necessity,
    'Is the claim that ''both layers are necessary'' itself partly a beneficiary-serving narrative — i.e., do civil defense agencies and memorial custodians have an institutional interest in presenting the dual-layer structure as naturally necessary, independent of whether a leaner structure would work as well?',
    'Compare preparedness outcomes in matched jurisdictions that have experimentally reduced the memorial layer''s resource share; if outcomes hold steady, the necessity claim is weakened and the hybrid_reading''s own tangled_rope classification (rather than a false mountain-like ''both layers are just how preparedness works'' framing) is the more honest one.',
    'This omega documents why the hybrid_reading is authored as tangled_rope rather than as a naturalized necessity claim — the reading acknowledges its own coordination/extraction hybrid rather than presenting the layered structure as an unquestionable natural requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_check_layered_necessity, conceptual, 'Guards against the hybrid reading smuggling in a naturalized necessity claim that primarily serves its own administering beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(prep_tr_t8, preparedness_commitment__hybrid_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(prep_tr_t16, preparedness_commitment__hybrid_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(prep_tr_t24, preparedness_commitment__hybrid_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(prep_tr_t32, preparedness_commitment__hybrid_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__hybrid_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(prep_be_t8, preparedness_commitment__hybrid_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(prep_be_t16, preparedness_commitment__hybrid_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(prep_be_t24, preparedness_commitment__hybrid_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(prep_be_t32, preparedness_commitment__hybrid_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__hybrid_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(prep_su_t8, preparedness_commitment__hybrid_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(prep_su_t16, preparedness_commitment__hybrid_reading, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(prep_su_t24, preparedness_commitment__hybrid_reading, suppression_requirement, 24, 0.36).
narrative_ontology:measurement(prep_su_t32, preparedness_commitment__hybrid_reading, suppression_requirement, 32, 0.37).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__hybrid_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the preparedness_commitment kernel. competence_reading treats preparedness as reducible to exercised operational knowledge (memorial elements are unnecessary overhead — likely lower ε, closer to rope or mountain-adjacent). husk_reading treats the memorial layer as having fully displaced competence, leaving performance without function (likely higher ε and theater_ratio, closer to piton or snare). This hybrid_reading occupies the middle: both layers are authored as doing genuine, separable work, with the tension between them as the source of extraction — hence tangled_rope. Each reading has its own ε and its own stakeholder structure; they are not three measurements of one constraint but three distinct constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
