% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__hybrid_reading, []).

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
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Stratified Preparedness Transmission — Engineering Intact, Coordination Decayed
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   This story instantiates the hybrid reading of the
 *   preparedness-transmission kernel: the claim that physical infrastructure
 *   competence (engineering codes, inspections, structural retrofits) has
 *   been actively maintained across generations, while civilian coordination
 *   competence (evacuation routing, communication cascades, interagency
 *   mutual aid) has decayed from disuse. Unlike the husk reading, this
 *   reading does not claim the whole apparatus has hollowed into memorial
 *   ritual — the engineering half is genuinely, verifiably functional. Unlike
 *   the competence reading, it does not claim drills and inspections
 *   uniformly re-validate capability — it asserts a split: one layer
 *   re-validates, the other does not. The D5 break, in this reading, sits
 *   specifically in the coordination layer, not the physical layer, which is
 *   the structural delta that distinguishes this constraint from its two
 *   siblings.
 *
 * KEY AGENTS:
 *   - engineering_and_inspection_bodies: maintain genuinely live, re-validated competence — beneficiary of the credit the whole system receives
 *   - elected_officials_claiming_readiness: agenda-setters who conflate infrastructure readiness with total readiness in public claims
 *   - municipal_emergency_managers: payers who must operationalize a coordination plan built on assumptions that no longer hold
 *   - first_responders: payers who absorb the coordination gap physically and immediately, with no exit
 *   - residents_in_hazard_zones: powerless payers who cannot detect the stratification until an event exposes it
 *   - disaster_historians_and_after_action_reviewers: analytical observers whose reports are the empirical basis for the stratification claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.42).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.38).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness Transmission — Engineering Intact, Coordination Decayed").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster_risk_management/institutional_memory/civil_defense").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, 'e8ece86d-b4f1-4295-bf09-686cc4daa3f2').
narrative_ontology:cs_kernel_codification('e8ece86d-b4f1-4295-bf09-686cc4daa3f2', distributed).
narrative_ontology:cs_authority_grounding('e8ece86d-b4f1-4295-bf09-686cc4daa3f2', practice).
narrative_ontology:cs_interpretation_layer_present('e8ece86d-b4f1-4295-bf09-686cc4daa3f2').
narrative_ontology:cs_reading_relation('e8ece86d-b4f1-4295-bf09-686cc4daa3f2', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8ece86d-b4f1-4295-bf09-686cc4daa3f2', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_axiom('e8ece86d-b4f1-4295-bf09-686cc4daa3f2', foundational, preparedness_competence_is_layer_separable).
narrative_ontology:cs_axiom_status(preparedness_competence_is_layer_separable, holdable).
narrative_ontology:cs_axiom_grounding('e8ece86d-b4f1-4295-bf09-686cc4daa3f2', preparedness_competence_is_layer_separable, empirically_contingent).
narrative_ontology:cs_axiom('e8ece86d-b4f1-4295-bf09-686cc4daa3f2', secondary, structural_hardening_does_not_entail_coordination_readiness).
narrative_ontology:cs_axiom_status(structural_hardening_does_not_entail_coordination_readiness, holdable).
narrative_ontology:cs_axiom_grounding('e8ece86d-b4f1-4295-bf09-686cc4daa3f2', structural_hardening_does_not_entail_coordination_readiness, empirically_contingent).
narrative_ontology:cs_reference_frame('e8ece86d-b4f1-4295-bf09-686cc4daa3f2', unified_civil_defense_readiness_model).
narrative_ontology:cs_drift_state('e8ece86d-b4f1-4295-bf09-686cc4daa3f2', contemporary_post_incident_review_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e8ece86d-b4f1-4295-bf09-686cc4daa3f2', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, engineering_and_inspection_bodies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, elected_officials_claiming_readiness).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, residents_in_hazard_zones).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, first_responders).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, municipal_emergency_managers).
narrative_ontology:constraint_vindicates(preparedness_transmission__hybrid_reading, infrastructure_hardening_is_sufficient_preparedness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design codes, run structural inspections, and certify levees, seawalls, and retrofits against physical failure modes. This work is genuinely maintained — apprenticeship, licensing exams, and post-incident forensic review keep the engineering discipline sharp. Their competence is real and their certifications are the visible proof the system points to when claiming readiness, even though their scope never included evacuation logistics or citizen coordination.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, engineering_and_inspection_bodies, beneficiary,
    institutional, generational, arbitrage, regional).

% Cite hardened infrastructure and passed inspections as evidence of overall disaster readiness in budget hearings and public statements, and allocate capital funding toward physical retrofits because the metrics are legible and photogenic. They are not lying about the infrastructure; they are silent about the coordination layer because no comparably crisp metric exists for it, and admitting the gap would require funding a slower, less visible line item.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, elected_officials_claiming_readiness, agenda_setter,
    institutional, biographical, mobile, regional).

% Are responsible for translating hardened infrastructure into an actual evacuation or shelter-in-place response during an event, using coordination plans, radio protocols, and mutual-aid agreements that are rehearsed rarely and often by rotating, under-trained staff. When an event exceeds the drilled scenario, they discover in real time that the plan assumes personnel, phone trees, or interagency habits that no longer exist. They cannot fix this by hardening a wall; the gap is theirs to absorb under time pressure with no exit.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, municipal_emergency_managers, payer,
    moderate, immediate, constrained, local).

% Execute evacuation and rescue orders built on the assumption of functioning civilian coordination — clear communication trees, informed residents, rehearsed rally points. When coordination knowledge has decayed, responders improvise under conditions the plan did not anticipate, absorbing physical risk and blame that properly belongs to an atrophied planning layer they did not create and cannot repair mid-incident.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, first_responders, payer,
    moderate, immediate, trapped, local).

% Are told the levee/seawall/building code has been upgraded and inspected, which is true, and infer from this that they are broadly protected, which is not the same claim. When an event requires evacuation or coordinated sheltering rather than structural resistance, they discover the coordination apparatus — sirens, routes, communication cascades, community wardens — was assumed rather than actually rehearsed at the density needed. They bear the consequence of a gap they had no way to detect from the outside.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, residents_in_hazard_zones, payer,
    powerless, immediate, trapped, local).

% Conduct after-action reviews following near-misses and actual disasters, comparing what the infrastructure actually did against what the coordination response actually did. Their reports consistently find the physical systems performed to specification while evacuation timing, communication, and interagency coordination underperformed — this is the empirical basis for treating the two layers as structurally distinct rather than a single 'preparedness' variable.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, disaster_historians_and_after_action_reviewers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement genuinely coordinates two things well within their own domains: engineering standards coordinate builders, inspectors, and regulators around a shared, testable specification for physical resilience, and this coordination is actively practiced and re-validated. The civilian coordination layer — evacuation routing, communication cascades, interagency mutual aid — was designed to coordinate a much larger and more diffuse set of actors (residents, volunteers, multiple agencies) and that coordination has not been actively re-practiced at the scale or frequency needed to remain functional.
% TRANSFER_FUNCTION: Public trust and political credit flow toward the visible, verifiable infrastructure layer and its administrators, while the invisible cost of an unrehearsed coordination layer is transferred onto whoever is standing in the hazard zone when an event exceeds the infrastructure's design envelope — residents, first responders, and municipal staff absorb the gap as elevated risk, confusion, and blame during the event itself, not before.
% ABSENT_VOICES: Frontline emergency management staff and community-level coordination volunteers (block captains, neighborhood wardens) rarely have a formal channel to flag that coordination drills are underfunded or under-attended relative to structural inspections; their concerns typically surface only in after-action reports written after a failure, when they are least able to change the resource allocation that produced the gap.
% DISAPPEARANCE_RATIONALE: If the stratification itself vanished — meaning coordination competence were restored to match infrastructure competence — hazard-zone residents and first responders would experience materially different outcomes during stress events (faster evacuations, fewer improvisation failures), so from their seat the world clearly rearranges. From the engineering and political seats, day-to-day operation would look almost identical, since their visible function (certified, hardened infrastructure) is unaffected either way — hence the verdict is contested rather than uniform across seats.
% FOUNDING_PROBLEM: The original preparedness system was built to address both physical vulnerability (buildings and barriers that fail catastrophically) and human vulnerability (populations that panic, disperse chaotically, or fail to receive warnings in time) as a single integrated problem — mid-20th-century civil defense programs explicitly paired hardened infrastructure with widely rehearsed civilian drills.
% FOUNDING_PROBLEM_CORROBORATION: Disaster historians and after-action reviewers, an analytical seat outside both the engineering bodies and the elected officials who benefit from the current framing, corroborate that the physical-vulnerability half of the founding problem remains actively addressed while the human-coordination half has been allowed to lapse in resourcing and rehearsal frequency; no beneficiary-side source disputes the underlying after-action data, only its policy significance.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, contested).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).
:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high because this is not a story of deliberate rent extraction — the engineering competence genuinely exists and genuinely protects people from structural failure modes; the harm is a mismatch between claimed total readiness and actual partial readiness, which is a governance failure more than a capture. Theater ratio is elevated and rising (0.20 to 0.48) because an increasing share of 'preparedness' activity — press conferences citing inspection pass rates, budget documents listing hardened assets — substitutes for the harder, less visible work of running large-scale coordination exercises. Suppression is moderate: no one is actively blocked from running coordination drills, but political incentives and funding structures passively starve that layer relative to the legible infrastructure layer.
 *
 * DIRECTIONALITY LOGIC:
 *   Engineering bodies and elected officials sit near the beneficiary end: their visible competence and credit-claiming are real and largely uncontested, and neither bears the downside when coordination fails. Residents, first responders, and emergency managers sit toward the target end: they experience the full consequence of the coordination gap at the moment of highest stress, with the least ability to have prevented it beforehand. The directionality split tracks exactly the structural split the reading claims — the beneficiary/victim line runs along the same seam as the infrastructure/coordination seam, not across it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was originally unified — protect people from both structural and coordination failure. Under this reading, half the founding problem (structural) remains genuinely live and actively solved, while the other half (coordination) has quietly gone dead without anyone declaring it so; the system continues to present itself as solving the unified original problem. This is the piton signature: no concentrated beneficiary is extracting value from the coordination gap specifically — elected officials benefit diffusely from not having to fund an unglamorous line item, but no one profits FROM the gap the way a snare's operator profits from extraction. The cost of closing the gap (sustained investment in large-scale, realistic coordination exercises) is diffuse and politically unrewarding, while the cost of the gap persisting falls on residents and responders who have no standing to force the reallocation. Classifying this as piton rather than snare prevents mislabeling institutional neglect as deliberate extraction, and prevents mislabeling it as a functioning rope by crediting the coordination layer with the engineering layer's genuine competence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_reading_is_true_at_scale,
    'Given the kernel contest, is the stratified hybrid reading the accurate structural diagnosis, or is one of the sibling readings (uniform competence, or uniform hollowing) closer to what a large-scale stress event would actually reveal?',
    'A genuine large-scale disaster event (or a sufficiently realistic full-scale exercise involving actual civilian evacuation, not just tabletop planning) would empirically discriminate: infrastructure performing to spec while coordination fails supports this reading; both failing supports husk_reading; both performing supports competence_reading.',
    'If the husk_reading proves more accurate, the engineering layer''s apparent competence may itself be partly theatrical (inspections passing on paper without capturing real degradation), which would raise this story''s extractiveness substantially and shift classification toward snare or tangled_rope. If competence_reading proves more accurate, this story''s claimed decay in coordination knowledge is overstated and the constraint may be closer to a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_is_true_at_scale, empirical, 'Whether the stratified/hybrid diagnosis holds under real stress versus the two sibling readings.').

omega_variable(
    coordination_metric_absence,
    'Is civilian coordination competence actually measurable with anything like the rigor of engineering inspection, or does the absence of a comparably legible metric partly explain — rather than merely reveal — the resource stratification?',
    'Compare jurisdictions that have implemented rigorous, frequent, large-scale coordination exercises with standardized scoring against those that have not, and measure whether funding and political attention followed the introduction of a legible coordination metric.',
    'If legible metrics drive funding, this reframes the stratification as a measurement-design problem rather than a values or capture problem — implying the fix is instrumentation (developing a coordination-competence index) rather than political will alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_metric_absence, conceptual, 'Whether metric legibility itself, not just political incentive, drives the infrastructure/coordination resource split.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prep_tr_t8, preparedness_transmission__hybrid_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(prep_tr_t16, preparedness_transmission__hybrid_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__hybrid_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(prep_tr_t32, preparedness_transmission__hybrid_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__hybrid_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(prep_be_t8, preparedness_transmission__hybrid_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(prep_be_t16, preparedness_transmission__hybrid_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__hybrid_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(prep_be_t32, preparedness_transmission__hybrid_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__hybrid_reading, base_extractiveness, 40, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_transmission__hybrid_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the preparedness_transmission kernel. husk_reading claims uniform hollowing across the whole apparatus (higher ε); competence_reading claims uniform live validation (lower ε); this hybrid_reading claims a structural split between a genuinely live engineering layer and a decayed coordination layer, and authors an intermediate ε (0.42) reflecting that only half the apparatus is compromised. All three share the same underlying kernel — the civil defense preparedness apparatus — but instantiate structurally distinct claims about where, and how completely, its D5 break sits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
