% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Layered Disaster Preparedness: Memorial Commitment Plus Operational Competence
 *   domain: institutional/civic
 *
 * SUMMARY:
 *   A regional emergency management system was built after a major disaster
 *   to prevent both political abandonment of preparedness funding and
 *   operational skill decay as the survivor generation aged out of public
 *   life. Over four decades it has developed two visibly distinct tracks —
 *   commemorative events tied to the anniversary of the founding disaster,
 *   and competence-building exercises (drills, equipment checks, protocol
 *   revision) — administered by overlapping but not identical institutions
 *   and competing for the same municipal budget line.
 *
 * KEY AGENTS:
 *   - emergency_management_agencies: institutional agenda-setter administering both layers, judged on both public trust and operational outcomes
 *   - commemorative_institutions: organized beneficiaries whose standing depends on the memorial layer's continuation independent of competence
 *   - frontline_drill_participants: powerless payers absorbing the time cost of a system that mixes ceremonial and functional training without always separating them
 *   - disaster_historians: analytical observers positioned to distinguish ritual from function using outcome data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.42).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.38).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Layered Disaster Preparedness: Memorial Commitment Plus Operational Competence").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "institutional/civic").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, '8f8f1a9c-0dda-444a-a1c5-fcaf1b0d1f3d').
narrative_ontology:cs_kernel_codification('8f8f1a9c-0dda-444a-a1c5-fcaf1b0d1f3d', distributed).
narrative_ontology:cs_authority_grounding('8f8f1a9c-0dda-444a-a1c5-fcaf1b0d1f3d', practice).
narrative_ontology:cs_interpretation_layer_present('8f8f1a9c-0dda-444a-a1c5-fcaf1b0d1f3d').
narrative_ontology:cs_reading_relation('8f8f1a9c-0dda-444a-a1c5-fcaf1b0d1f3d', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f8f1a9c-0dda-444a-a1c5-fcaf1b0d1f3d', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_axiom('8f8f1a9c-0dda-444a-a1c5-fcaf1b0d1f3d', foundational, memorial_and_competence_are_independently_necessary).
narrative_ontology:cs_axiom_status(memorial_and_competence_are_independently_necessary, holdable).
narrative_ontology:cs_axiom_grounding('8f8f1a9c-0dda-444a-a1c5-fcaf1b0d1f3d', memorial_and_competence_are_independently_necessary, empirically_contingent).
narrative_ontology:cs_axiom('8f8f1a9c-0dda-444a-a1c5-fcaf1b0d1f3d', secondary, layer_tension_generates_real_maintenance_cost).
narrative_ontology:cs_axiom_status(layer_tension_generates_real_maintenance_cost, holdable).
narrative_ontology:cs_axiom_grounding('8f8f1a9c-0dda-444a-a1c5-fcaf1b0d1f3d', layer_tension_generates_real_maintenance_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('8f8f1a9c-0dda-444a-a1c5-fcaf1b0d1f3d', post_founding_disaster_generational_transfer).
narrative_ontology:cs_drift_state('8f8f1a9c-0dda-444a-a1c5-fcaf1b0d1f3d', contemporary_fourth_generation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8f8f1a9c-0dda-444a-a1c5-fcaf1b0d1f3d', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, resident_population).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, commemorative_institutions).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, frontline_drill_participants).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, budget_constrained_municipalities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, resident_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and mandates the annual drill calendar, memorial ceremonies, and competency certifications. Draws legitimacy and budget renewal from being seen to 'keep the memory alive,' while also being judged on whether the region actually survives the next event. Cannot easily drop either layer without losing either public trust or operational credibility.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, emergency_management_agencies, beneficiary).

% Benefits from both the psychological reassurance of commemorated disaster history and any real evacuation competence that results from drills. Also pays through taxes and time spent on mandatory participation, some of which is ceremonial rather than functionally useful.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, resident_population, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, resident_population, payer).

% Museums, memorial associations, and anniversary committees that maintain the narrative of past disasters. They receive funding and cultural standing tied to the constraint's memorial layer continuing, independent of whether competence is actually maintained.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, commemorative_institutions, beneficiary,
    organized, civilizational, mobile, regional).

% Municipal workers, school staff, and volunteers who must repeat drills that mix genuine skill-building with performative ritual elements added to satisfy the memorial mandate. They bear the time cost and burnout risk of a system that layers ceremony onto operational training without always distinguishing which parts matter.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, frontline_drill_participants, payer,
    powerless, immediate, trapped, local).

% Must fund both memorial observances (plaques, ceremonies, anniversary events) and competence infrastructure (equipment, real exercises, updated protocols) from the same limited disaster-preparedness budget line, forcing tradeoffs neither layer's advocates fully acknowledge.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, budget_constrained_municipalities, payer,
    moderate, biographical, constrained, local).

% Study whether preparedness systems that persist across generations retain functional capacity or decay into ritual. Can distinguish, from outside the system, which drills produce measurable competence gains and which serve only commemorative purposes.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, disaster_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine dual problem: disaster memory fades within roughly one generation (nobody who lived through the event remains active), which causes both political will to fund preparedness AND practical skill at executing it to atrophy simultaneously. A layered system uses memorial ritual to keep the political/cultural commitment alive across the generational gap, while competence exercises keep the operational skill alive within it.
% TRANSFER_FUNCTION: Moves time, attention, and municipal budget from residents and frontline workers to a combined apparatus of commemorative institutions and emergency agencies. In return it moves risk reduction (real, where competence holds) and psychological reassurance (real or hollow, depending on which layer is doing the work at any given moment) back to residents.
% ABSENT_VOICES: Actual disaster survivors from the founding event are mostly gone or aging out; their direct testimony about what preparedness originally needed to accomplish is thin. Budget officers who would prefer to fund only the competence layer, or only the memorial layer, rarely get a seat in a system designed to defend both simultaneously.
% DISAPPEARANCE_RATIONALE: If the memorial layer vanished, agencies argue political support and funding would erode within a decade, degrading competence indirectly; disaster historians argue the memorial layer could vanish with no operational effect if competence exercises were funded on their own merits. If the competence layer vanished, everyone agrees the world rearranges catastrophically at the next disaster. The two layers are not symmetrically load-bearing, which is exactly the tension the hybrid reading names.
% FOUNDING_PROBLEM: After a major regional disaster, both political will to fund prevention and hands-on operational knowledge of how to respond were concentrated in the generation that lived through it. As that generation left public life, the arrangement needed a mechanism to prevent both the funding commitment and the operational skill from disappearing at the same rate.
% FOUNDING_PROBLEM_CORROBORATION: Emergency management agencies and commemorative institutions attest the founding problem remains fully live and both layers are necessary. Disaster historians, reviewing drill-outcome data independent of the agencies, attest that the memorial layer has outgrown its original function relative to the competence layer in several jurisdictions, while acknowledging the competence layer's problem remains genuinely live.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, contested).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42) and rising slowly — the system is not primarily extractive, but each layer generates some overhead the other doesn't need, and that overhead accumulates as institutions defending each layer entrench. Theater ratio is the most diagnostic metric here (0.45, trending upward): a meaningful share of drill and ceremony time is performative relative to what pure competence-maintenance would require, but it is not dominant, consistent with a genuinely mixed system rather than the husk_reading's near-total ritual capture. Suppression is moderate and largely institutional (mandatory drill participation, budget-line lock-in) rather than coercive in a harsh sense.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency management agencies and commemorative institutions sit near the beneficiary end: they set the agenda and draw funding/standing from the arrangement's continuation. Frontline drill participants and budget-constrained municipalities sit nearer the target end: they pay in time and money without controlling the ratio between memorial and competence activity. Residents are genuinely mixed — real beneficiaries of whichever layer is functioning, real payers of tax and time regardless of which layer is theater at a given moment.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading is precisely the position that resists collapsing this constraint into either 'pure coordination that works' or 'pure extraction dressed as memory.' Classifying it as tangled_rope rather than rope or snare captures that a genuine coordination function (bridging the generational forgetting gap) coexists with real extraction (drill participants and municipalities bearing costs generated by inter-layer tension, and commemorative institutions capturing standing independent of operational contribution). Treating this as a pure rope would erase the frontline participants' burden; treating it as a pure snare would erase the documented cases where memorial-driven political will preserved funding that pure competence framing failed to secure on its own.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Is the preparedness_commitment kernel better read as hybrid (both layers doing real, separable work), husk (memorial has displaced competence), or competence (memorial is incidental to real operational maintenance)? This story instantiates the hybrid_reading only.',
    'Outcome data comparing regions/eras with strong memorial layers and weak competence layers against the reverse, controlling for disaster frequency, would show whether the layers are genuinely separable and independently load-bearing as the hybrid reading claims, or whether one layer is doing all the operational work while the other free-rides on its legitimacy.',
    'If competence outcomes track drill quality regardless of memorial intensity, the competence_reading is closer to correct and the memorial layer''s claimed stabilizing function is largely decorative — pushing this constraint toward the husk_reading''s classification. If memorial intensity independently predicts sustained funding even where competence lags, the hybrid reading''s core claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which of the three kernel readings best fits the observed system; this story assumes hybrid without independently establishing it against the siblings.').

omega_variable(
    layer_interaction_cost_attribution,
    'Is the rising theater_ratio and extractiveness genuinely caused by tension BETWEEN the two layers (as the hybrid reading''s expected structural delta claims), or is it simple institutional drift within a single layer that happens to coexist with the other?',
    'Process-trace specific budget and calendar decisions to see whether memorial-layer advocates and competence-layer advocates actively compete for the same resources (confirming interaction cost) or whether each layer degrades independently for its own reasons (undermining the hybrid-specific causal claim).',
    'If interaction cost is confirmed, the tangled_rope classification is well-grounded in a real coordination/extraction hybrid; if the layers degrade independently, the ''hybrid'' framing may be doing less analytical work than claimed and the constraint might better decompose into two nearly-independent constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_interaction_cost_attribution, empirical, 'Whether the authored maintenance cost is a genuine layer-interaction effect or two independent decay processes mislabeled as interacting.').

omega_variable(
    memorial_beneficiary_versus_natural_status,
    'Do commemorative institutions constitute a genuine extractive beneficiary class, or is their persistence closer to a natural byproduct of any society that has experienced disaster and would commemorate it regardless of the preparedness system''s operational needs?',
    'Compare commemorative activity levels in regions with disaster history but no formal preparedness apparatus against regions where memorial and preparedness are institutionally fused; divergence would indicate the fusion (not memory itself) is the extractive element.',
    'If commemoration would exist independent of the preparedness apparatus, listing commemorative_institutions as a beneficiary overstates the extraction; if the fusion itself created and sustains the commemorative institutions'' funding, the beneficiary designation is well-founded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(memorial_beneficiary_versus_natural_status, conceptual, 'Whether commemorative institutions are an independent cultural phenomenon or a beneficiary class created by fusion with the preparedness budget.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prep_tr_t8, preparedness_commitment__hybrid_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(prep_tr_t16, preparedness_commitment__hybrid_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(prep_tr_t24, preparedness_commitment__hybrid_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(prep_tr_t32, preparedness_commitment__hybrid_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__hybrid_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(prep_be_t8, preparedness_commitment__hybrid_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(prep_be_t16, preparedness_commitment__hybrid_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(prep_be_t24, preparedness_commitment__hybrid_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(prep_be_t32, preparedness_commitment__hybrid_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__hybrid_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(prep_su_t8, preparedness_commitment__hybrid_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(prep_su_t16, preparedness_commitment__hybrid_reading, suppression_requirement, 16, 0.32).
narrative_ontology:measurement(prep_su_t24, preparedness_commitment__hybrid_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(prep_su_t32, preparedness_commitment__hybrid_reading, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__hybrid_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the preparedness_commitment kernel, decomposed per the epsilon-invariance principle because the three readings assign structurally different extraction profiles to what natural language calls 'disaster preparedness.' husk_reading treats the arrangement as substantially extractive ritual (high theater, low real competence); competence_reading treats it as a low-extraction rope (real skill maintenance, memorial elements incidental); this hybrid_reading takes an intermediate tangled_rope position, asserting both a genuine coordination function and real extraction arising specifically from inter-layer tension. All three share the same underlying institutional referent but are authored as separate constraints with separate ε values, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
