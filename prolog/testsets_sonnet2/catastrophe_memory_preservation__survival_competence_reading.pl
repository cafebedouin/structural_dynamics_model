% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Catastrophe-Memory Ritual as Operational Threat-Recognition Transfer
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates the survival-competence reading of the
 *   catastrophe-memory-preservation kernel: a community ritual that reenacts
 *   a historical disaster is read here as a functioning transfer mechanism
 *   for operational threat-recognition — evacuation routes, warning signs,
 *   resource-hoarding timing — carried across generations precisely because
 *   the ritual forces rehearsal frequency no other mechanism would sustain.
 *   On this reading the coordination function is real (intergenerational
 *   knowledge transfer across a low-frequency high-consequence threat gap)
 *   and the extraction is real and asymmetric (present-generation
 *   psychological and time cost, non-reciprocal transfer to unborn future
 *   beneficiaries), which is why the type is authored as tangled_rope rather
 *   than pure rope or pure snare. Two sibling readings of the same ritual —
 *   mourning_practice_reading (symbolic continuity without operational
 *   transfer) and hybrid_atrophy_reading (operational function that has
 *   decayed into mourning under modernity) — are separate constraint stories
 *   with their own ε and structural data; they are not blended into this one.
 *
 * KEY AGENTS:
 *   - ritual_custodial_lineage: institutional agenda-setter, identity-locked to the ritual's continuation, collects authority from being the transmission vector
 *   - present_generation_participants: moderate-power payers bearing repeated traumatic re-exposure and mandatory drill cost
 *   - future_generations_facing_recurrence: powerless, not-yet-existing beneficiary of the transferred procedural knowledge
 *   - children_and_adolescents_in_training: powerless dual payer/beneficiary, inducted before consenting age
 *   - skeptical_community_members: excluded voices arguing the threat condition has lapsed
 *   - ethnographic_and_historical_observers: analytical observers assessing procedural fidelity independent of community self-understanding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.71).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Catastrophe-Memory Ritual as Operational Threat-Recognition Transfer").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, '368d8500-f95e-4939-b501-e9bfc24dea87').
narrative_ontology:cs_kernel_codification('368d8500-f95e-4939-b501-e9bfc24dea87', implicit).
narrative_ontology:cs_authority_grounding('368d8500-f95e-4939-b501-e9bfc24dea87', practice).
narrative_ontology:cs_interpretation_layer_present('368d8500-f95e-4939-b501-e9bfc24dea87').
narrative_ontology:cs_reading_relation('368d8500-f95e-4939-b501-e9bfc24dea87', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('368d8500-f95e-4939-b501-e9bfc24dea87', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('368d8500-f95e-4939-b501-e9bfc24dea87', foundational, ritual_content_retains_operational_correspondence).
narrative_ontology:cs_axiom_status(ritual_content_retains_operational_correspondence, holdable).
narrative_ontology:cs_axiom_grounding('368d8500-f95e-4939-b501-e9bfc24dea87', ritual_content_retains_operational_correspondence, empirically_contingent).
narrative_ontology:cs_axiom('368d8500-f95e-4939-b501-e9bfc24dea87', foundational, intergenerational_transfer_justifies_present_costly_participation).
narrative_ontology:cs_axiom_status(intergenerational_transfer_justifies_present_costly_participation, holdable).
narrative_ontology:cs_axiom_grounding('368d8500-f95e-4939-b501-e9bfc24dea87', intergenerational_transfer_justifies_present_costly_participation, instrumental).
narrative_ontology:cs_reference_frame('368d8500-f95e-4939-b501-e9bfc24dea87', post_catastrophe_founding_generation_drill).
narrative_ontology:cs_drift_state('368d8500-f95e-4939-b501-e9bfc24dea87', contemporary_infrastructure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('368d8500-f95e-4939-b501-e9bfc24dea87', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generations_facing_recurrence).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, ritual_custodial_lineage).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, children_and_adolescents_in_training).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, children_and_adolescents_in_training).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elders, priests, or designated tradition-bearers design and enforce the ritual calendar, deciding which elements of the catastrophe narrative are dramatized, which behavioral drills are embedded, and who must attend. Their institutional standing derives from being the transmission vector; their authority and the ritual's continuation are mutually constitutive.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_custodial_lineage, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__survival_competence_reading, ritual_custodial_lineage, beneficiary).

% Community members required to attend annual or cyclical observances that reenact a historical catastrophe (flood, famine, invasion, plague) in vivid, often traumatic detail, and to perform associated survival drills (evacuation routes, food caching, warning signals). They bear the psychological cost of repeated re-exposure to disaster narrative and the time/resource cost of participation, whether or not the original threat is currently live for them. Non-participation carries social sanction; participation is not optional in any meaningful sense within the community.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants, payer,
    moderate, biographical, constrained, local).

% Not yet born or not yet of age to face the threat the ritual encodes. If the catastrophe recurs (flood cycle, drought, invasion pattern), they inherit the embodied procedural knowledge — evacuation routes, warning-sign recognition, resource-hoarding timing — carried forward only because the ritual forced its rehearsal in every generation. They cannot consent to or compensate the present generation for bearing this cost on their behalf.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, future_generations_facing_recurrence, beneficiary,
    powerless, civilizational, analytical, regional).

% Are inducted into the ritual's darker content — reenacted death, loss, and fear — before they have the option to consent to that exposure, on the theory that early and repeated exposure is what encodes procedural memory durably. They are simultaneously the primary target of the transfer (the generation being trained) and the least able to evaluate whether the transfer is worth its cost to them personally.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, children_and_adolescents_in_training, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__survival_competence_reading, children_and_adolescents_in_training, beneficiary).

% Individuals who believe the original threat is no longer operative (the river was dammed, the invading polity no longer exists) and view the ritual's costs as no longer justified by its stated function. Their view is rarely solicited in ritual-calendar decisions and voicing it publicly risks being read as disrespect to the dead or to tradition, which forecloses the debate before it starts.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, skeptical_community_members, excluded,
    powerless, biographical, constrained, local).

% Researchers who study whether the ritual's drilled content actually corresponds to effective threat response (e.g., whether evacuation routes rehearsed in ritual match viable routes under current conditions) versus whether the content has drifted into pure symbolism. They can assess procedural fidelity independent of the community's own self-understanding.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ethnographic_and_historical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__survival_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual solves a genuine intergenerational transmission problem: procedural knowledge about how to recognize and respond to a low-frequency, high-consequence threat (the kind of event that may occur only once or twice per human lifetime) will not survive in a population's operational memory without a forcing mechanism, because no single generation experiences the event often enough to naturally rehearse the response. Embedding the drill inside a mandatory, emotionally weighted ritual guarantees rehearsal frequency that ordinary transmission (oral history, casual instruction) could not sustain.
% TRANSFER_FUNCTION: Moves psychological and time cost from the present generation of participants — who bear repeated exposure to traumatic reenactment and the burden of enforced drill even when the threat is not currently live for them — to future generations, who receive functioning procedural threat-recognition without having to pay for its acquisition themselves. The transfer is intergenerational and non-reciprocal: the present generation cannot be compensated by the future generation it protects.
% ABSENT_VOICES: Skeptical community members who believe the original threat condition no longer holds are structurally excluded from ritual-calendar revision; children inducted into traumatic content before consenting age are also absent from the decision to induct them. Both groups would likely argue for reduced intensity or narrowed scope of the ritual's costlier elements while preserving a lighter-weight informational core.
% DISAPPEARANCE_RATIONALE: The custodial lineage and much of the community would say the world rearranges catastrophically if the ritual vanished — the next occurrence of the threat would meet a population with no rehearsed response, reproducing historical mortality. Skeptics and some outside observers would say the world stays largely unchanged, since the original threat conditions have shifted enough (infrastructure, monitoring technology, changed geography) that the operational content is now more symbolic than functional. The verdict genuinely depends on unresolved empirical questions about current threat base rates.
% FOUNDING_PROBLEM: A catastrophic, low-frequency event (flood, famine, invasion, epidemic) once devastated the community, and survivors recognized that without an enforced transmission mechanism, the next generation would face the same event with no procedural memory of what worked, because ordinary generational turnover erases operational detail faster than the threat recurs.
% FOUNDING_PROBLEM_CORROBORATION: The custodial lineage and elder participants attest the threat condition remains live, citing recent near-miss events or ongoing environmental indicators. Ethnographic and historical observers, working from external hazard records and infrastructure assessments, are divided: some corroborate continued operational relevance, others document that the specific historical hazard has been substantially mitigated by non-ritual means (engineering, early-warning systems, altered settlement patterns) since the ritual's founding, making the transmitted content partially obsolete even where the ritual's felt urgency has not decreased.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__survival_competence_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 at interval end) and rising because the ritual's costly demands on present participants — the traumatic reenactment content, the mandatory time and resource investment in drills — persist and intensify even as the underlying threat's currency becomes less certain, which increases the ratio of cost-borne-now to benefit-realized-now (the benefit is deferred to hypothetical future recurrence). Suppression (0.62, also rising) reflects the social sanction against non-participation and against the skeptical minority's ability to renegotiate ritual intensity — this is a structural property of the enforcement mechanism, not scaled by scope. Theater ratio is kept comparatively low (0.28) because, under THIS reading, the drilled content is asserted to retain genuine operational correspondence to viable threat response, not mere performance — that is the defining commitment of the survival-competence reading as against hybrid_atrophy_reading, where theater_ratio would be authored substantially higher for the same underlying practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are the structural beneficiaries under this reading (low d, though they are not agents present to collect anything themselves — the transfer accrues to whoever occupies that generational position when the threat recurs). Present-generation participants are the structural targets (high d): they pay the ritual's costs without being able to negotiate compensation from the future beneficiaries. The custodial lineage sits closer to beneficiary because its institutional authority is reproduced by the ritual's continuation, independent of whether the operational content is ever actually needed. Children in training carry the highest effective extraction relative to consent capacity — they are inducted into costly content before they can evaluate or refuse it, which the exit_options=trapped declaration reflects directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents this story from collapsing into either a pure rope (ignoring the real, non-consensual cost imposed on present participants and especially children) or a pure snare (ignoring the real coordination function of transmitting rehearsed threat-response across a generational gap too wide for casual transmission). The founding_problem is authored as contested rather than resolved dead specifically because that is the fact pattern this reading requires: if the founding problem were cleanly dead, the honest classification would drift toward hybrid_atrophy_reading's territory, not this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_fidelity_of_transmitted_content,
    'Does the ritual''s drilled content (evacuation routes, warning-sign recognition, resource-hoarding timing) still correspond to a viable response under current hazard and infrastructure conditions, or has the content drifted from the original threat while the ritual''s form persisted?',
    'Independent hazard-engineering assessment comparing the ritual''s prescribed procedures against current geography, infrastructure, and monitoring capability; comparison with hybrid_atrophy_reading''s theater_ratio measurements for the same practice.',
    'If fidelity is confirmed high, this reading''s low theater_ratio and tangled_rope classification are supported. If fidelity has substantially decayed, the constraint is better described by hybrid_atrophy_reading and this reading''s extraction claim loses its coordination-function grounding, pushing the honest classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_fidelity_of_transmitted_content, empirical, 'Whether the ritual''s operational content still matches a real, current threat-response need.').

omega_variable(
    which_reading_is_the_community_actually_living,
    'Among the three declared kernel readings (survival_competence, mourning_practice, hybrid_atrophy), which one accurately describes what the ritual is currently doing for THIS community, and is that a single fact or does it vary by sub-group within the community (custodial lineage vs. skeptical members vs. children)?',
    'Ethnographic fieldwork triangulating custodial-lineage self-report, skeptical-member testimony, and independent procedural-fidelity assessment; document where sub-groups within one community are effectively living different readings simultaneously.',
    'If different sub-groups are living different readings, the single-community case may itself warrant a within-community reading-split analogous to the kernel-level split, rather than a single verdict being imposed on the whole population.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_community_actually_living, conceptual, 'Whether the three kernel readings are mutually exclusive descriptions or coexist across sub-groups within one community.').

omega_variable(
    consent_deficit_of_child_induction,
    'Is the induction of children into traumatic ritual content before consenting age justified by the operational-transfer benefit it purchases, or does the consent deficit itself constitute an independent wrong regardless of downstream benefit?',
    'Comparative study of communities that delay traumatic content until later adolescence versus those that induct earlier, tracking both retention of operational competence and reported psychological cost.',
    'If delayed induction preserves comparable operational transfer with lower psychological cost, the current practice''s extractiveness toward children would be judged avoidable rather than structurally necessary, sharpening the tangled_rope''s extraction component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_deficit_of_child_induction, preference, 'Whether early traumatic induction is a necessary cost of the transfer mechanism or an avoidable excess.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 100, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 80, 0.59).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__survival_competence_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the catastrophe_memory_preservation kernel. survival_competence_reading (this story) claims genuine, currently-live operational transfer and authors high, entangled extraction (tangled_rope). mourning_practice_reading claims the ritual's function is symbolic/identity continuity only, with no operational claim, and should author substantially lower extraction and a rope-leaning classification. hybrid_atrophy_reading claims the operational function was once real but has decayed into mourning theater, and should author a much higher theater_ratio than this story for structurally the same observable practice. The three ε values differ by construction — they are not three measurements of one constraint but three distinct structural claims about what the same ritual practice is doing, per the ε-invariance decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_preservation__survival_competence_reading, powerless, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
