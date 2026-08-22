% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe-Memory Ritual — Atrophied from Survival Competence to Mourning Practice
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates the hybrid-atrophy reading of the
 *   catastrophe-memory-preservation kernel: the ritual DID once carry genuine
 *   operational survival knowledge (a claim shared with the
 *   survival_competence_reading) but that function has decayed over time,
 *   leaving a practice whose form persists while its content has hollowed
 *   into symbolic mourning (a claim shared in outcome, but not in
 *   origin-story, with the mourning_practice_reading). The distinguishing
 *   move of this reading is temporal: it asserts a transition FROM functional
 *   coordination TO identity performance, which is why the constraint is
 *   classified as piton rather than as a stable rope (survival reading) or a
 *   stable identity-coordination mechanism (mourning reading). Extraction is
 *   moderate and has risen over the measured interval as the theater ratio
 *   climbed — the ritual's cost has not fallen even as its function emptied
 *   out.
 *
 * KEY AGENTS:
 *   - ritual_elders_custodians: administers the observance, moderate power, identity-locked exit
 *   - in_group_identity_custodians: beneficiary of continued performance without bearing its administrative or physical cost
 *   - present_generation_practitioners: bears the ritual's cost, powerless, identity-locked
 *   - younger_diaspora_members: excluded skeptics, mobile exit
 *   - folklorists_ethnographers: analytical observers reconstructing the functional-to-symbolic transition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.38).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe-Memory Ritual — Atrophied from Survival Competence to Mourning Practice").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, '1b4fbd93-264f-4198-940f-9690534188a3').
narrative_ontology:cs_kernel_codification('1b4fbd93-264f-4198-940f-9690534188a3', implicit).
narrative_ontology:cs_authority_grounding('1b4fbd93-264f-4198-940f-9690534188a3', practice).
narrative_ontology:cs_interpretation_layer_present('1b4fbd93-264f-4198-940f-9690534188a3').
narrative_ontology:cs_reading_relation('1b4fbd93-264f-4198-940f-9690534188a3', catastrophe_memory_preservation__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('1b4fbd93-264f-4198-940f-9690534188a3', catastrophe_memory_preservation__mourning_practice_reading, influences).
narrative_ontology:cs_axiom('1b4fbd93-264f-4198-940f-9690534188a3', foundational, ritual_function_has_decayed_over_time).
narrative_ontology:cs_axiom_status(ritual_function_has_decayed_over_time, holdable).
narrative_ontology:cs_axiom_grounding('1b4fbd93-264f-4198-940f-9690534188a3', ritual_function_has_decayed_over_time, empirically_contingent).
narrative_ontology:cs_axiom('1b4fbd93-264f-4198-940f-9690534188a3', secondary, form_persists_independent_of_original_content).
narrative_ontology:cs_axiom_status(form_persists_independent_of_original_content, holdable).
narrative_ontology:cs_axiom_grounding('1b4fbd93-264f-4198-940f-9690534188a3', form_persists_independent_of_original_content, empirically_contingent).
narrative_ontology:cs_reference_frame('1b4fbd93-264f-4198-940f-9690534188a3', originating_catastrophe_transmission_event).
narrative_ontology:cs_drift_state('1b4fbd93-264f-4198-940f-9690534188a3', contemporary_diaspora_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1b4fbd93-264f-4198-940f-9690534188a3', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_custodians).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the calendar of observances, teaches the correct forms, and polices fidelity to inherited practice. Once transmitted concrete threat-recognition knowledge (flood signs, famine precursors, predator behavior); now largely transmits the performance of remembering. Could simplify or modernize the ritual but the cost of being seen to abandon ancestral form is high, so the elaborate form persists by default rather than by demonstrated necessity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_elders_custodians, agenda_setter,
    moderate, generational, identity_locked, regional).

% Community leaders and identity-invested members who draw social cohesion, belonging, and boundary-marking value from the ritual's continued performance. They benefit from the ritual's persistence as a marker of group distinctiveness even though the original survival function it encoded is gone; they do not administer it and do not bear its heaviest costs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_custodians, beneficiary,
    moderate, generational, constrained, regional).

% Inherit the full time, labor, and resource cost of the ritual cycle — travel, materials, days of preparation, social obligation — without any corresponding operational payoff, because the environmental threats the ritual once encoded no longer occur in a recognizable form or have been superseded by other institutions (early warning systems, insurance, medicine). Exit is possible in principle but costs standing within family and community; many perform the ritual while privately regarding it as symbolic rather than functional.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Have left the ancestral region and increasingly question why the ritual's exact, costly form must be maintained when its stated purpose (surviving a specific historical catastrophe) has no bearing on their present lives. Their objections are voiced privately or in generational disputes but rarely enter the formal deliberations that set ritual practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, younger_diaspora_members, excluded,
    powerless, biographical, mobile, global).

% Study the ritual's historical layers, comparing recorded forms against oral testimony and archival disaster records to reconstruct which elements originally encoded functional knowledge and which are later theatrical accretion. Their analysis is the primary outside evidence for the atrophy claim.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, folklorists_ethnographers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_custodians).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally coordinated intergenerational transmission of concrete hazard-recognition knowledge (signs preceding a historical catastrophe) through memorable, emotionally weighted repetition; now coordinates collective mourning and group-identity affirmation around the memory of that catastrophe.
% TRANSFER_FUNCTION: Moves time, labor, and material resources from present-generation practitioners to the maintenance of an elaborate observance whose original informational payload (actionable threat knowledge) has decayed; what is received in return is symbolic continuity and in-group standing, captured most fully by identity custodians rather than by those bearing the cost.
% ABSENT_VOICES: Younger diaspora members and functionally-minded descendants who question the ritual's continued elaborate form are rarely part of the bodies that set its calendar and content; their skepticism surfaces as private disengagement rather than formal challenge.
% DISAPPEARANCE_RATIONALE: Identity custodians and elders would say the world rearranges catastrophically — a thread of ancestral memory and group cohesion severed. Folklorists and disengaged descendants would say the world is largely unchanged operationally, since the ritual's functional payload eroded long ago and other institutions now perform the hazard-mitigation role; the dispute over which is true is itself part of the constraint's present operation.
% FOUNDING_PROBLEM: A historical catastrophe (environmental, epidemic, or violent) required survivors to encode recognizable precursor signs and response behaviors in a transmissible form so descendants could recognize and respond to recurrence.
% FOUNDING_PROBLEM_CORROBORATION: Folklorists and ethnographers, working from archival disaster records and comparative oral-tradition analysis outside the ritual's own custodial structures, corroborate that the specific environmental precursors the ritual originally encoded have not recurred in observable form for many generations and that modern institutions (early-warning systems, public health infrastructure) now perform the equivalent function. Ritual elders and identity custodians dispute this, treating the founding problem as still spiritually or symbolically live even as they acknowledge no one currently uses the ritual's content to detect an actual hazard.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).
:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42) and RISING rather than static or high, because the piton signature here is specifically about a function that atrophied gradually rather than a constraint built for extraction from the start. Theater ratio rises sharply (0.10 to 0.71) over the interval, which is the central authored claim of this reading: the ratio of performative to functional content has grown as the environmental hazard the ritual once tracked ceased recurring, while the observance's form was preserved or even elaborated as compensation for its lost instrumental grounding. Suppression is moderate (0.38) — no active coercive enforcement, but social cost of visible defection remains real. Accessibility collapse is moderate (0.40): alternatives (secular commemoration, informal remembrance) exist and are visibly used by diaspora members, so collapse is partial, not total.
 *
 * PERSPECTIVAL GAP:
 *   From the identity-custodian seat, the ritual reads as living heritage — coordination of memory and belonging, functioning exactly as intended. From the present-generation-practitioner seat, the same structure reads as unrewarded obligation: hazard-relevant content is absent, and what remains is cost without adaptive payoff. The engine should compute these as structurally different experiences of the identical arrangement, which is the seat divergence this reading is built to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   In-group identity custodians are declared beneficiaries because they draw ongoing cohesion and boundary-marking value from the ritual's persistence without bearing its administrative or physical labor cost — this yields low d, close to the beneficiary end. Present-generation practitioners are declared victims: they pay the labor/resource/time cost repeatedly, identity-locked (defection costs standing), which the derivation chain should push toward high d. Ritual elders sit ambiguously between agenda-setter and quasi-beneficiary but are authored purely as agenda_setter because their reward is administrative standing, not the identity capital that flows to the wider custodial community.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (recognizing precursors to a historical catastrophe) is declared dead by outside corroboration (folklorists, comparative disaster records) even though the mandate — perform the ritual faithfully — persists unchanged in form. This is the textbook mandatrophy signature: mandate outlives function. The piton classification, rather than snare, is justified because no concentrated beneficiary captures rents at victim expense through coercive extraction; the cost-asymmetry is diffuse (identity custodians benefit modestly, practitioners pay moderately, no one profits enough to actively defend against reform, no one is hurt badly enough to force reform) — which is exactly the piton test: an administrator (elders) who COULD simplify the practice, set against a cost-to-fix that exceeds what the administrator itself bears, because the social risk of appearing to abandon ancestral form falls on the elders while the labor cost falls on practitioners.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrophy_versus_original_symbolism,
    'Did the ritual ever encode genuine, actionable survival knowledge (as the hybrid_atrophy and survival_competence readings both assume), or was its content always primarily symbolic/mnemonic rather than operationally instructive (as the mourning_practice reading assumes)?',
    'Comparative textual and archaeological analysis of the earliest recoverable ritual forms against independent records of the catastrophe''s actual precursor signs — do the ritual''s specific gestures, timings, and warnings map onto documented environmental precursors, or are they generic mourning forms with no specific informational content?',
    'If early forms show demonstrable specific correspondence to real precursor signs, the hybrid_atrophy reading''s origin claim is strongly supported and the piton classification (degraded former rope) is the correct structural read. If no such correspondence is found, the mourning_practice reading''s claim that the ritual was always symbolic becomes more plausible, undermining this reading''s premise and favoring reclassification as a stable identity/attachment coordination constraint rather than an atrophied one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_versus_original_symbolism, empirical, 'Whether the ritual''s historical content was ever genuinely operational, which is the load-bearing premise distinguishing this reading from mourning_practice_reading.').

omega_variable(
    residual_functional_transfer,
    'Does any residual operational content survive within the now-largely-symbolic practice (e.g., embedded practical knowledge about food storage, water sourcing, or shelter-building performed as part of the ritual sequence) that the survival_competence_reading would treat as still live?',
    'Fine-grained ethnographic decomposition of the ritual''s component actions, separating symbolic/commemorative gestures from any that retain practical instructional value, cross-checked against whether practitioners can perform the practical components correctly without ritual guidance (indicating independent transmission) or only within the ritual frame (indicating the ritual is the sole remaining transmission vector).',
    'If meaningful residual functional transfer is found, the survival_competence_reading''s claim of ongoing operational value gains support and this reading''s atrophy claim would need to be narrowed to specific decayed components rather than the whole practice. If no residual transfer is found, this reading''s classification of near-total functional loss is corroborated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_functional_transfer, empirical, 'Whether any operationally live component survives inside the largely symbolic present-day practice.').

omega_variable(
    identity_versus_extraction_boundary,
    'Is the value identity custodians derive from the ritual''s persistence a genuine, low-cost coordination benefit (shared cultural meaning available to all) or does it function as a rent extracted from practitioners who bear disproportionate cost for custodians'' social capital?',
    'Compare the distribution of costs (labor, time, resources) against the distribution of social/status benefits across custodian and practitioner populations; a wide asymmetry with concentrated status benefit would indicate rent extraction rather than shared coordination.',
    'A finding of concentrated benefit with diffuse cost would push the classification from piton toward tangled_rope or snare (active extraction under coordination cover); a finding of genuinely diffuse, low-cost shared benefit would support keeping the piton classification (inertial persistence, no concentrated capturer).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_versus_extraction_boundary, conceptual, 'Whether identity-custodian benefit constitutes genuine diffuse coordination value or concentrated extraction from practitioners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 60, 0.53).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 80, 0.64).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 100, 0.71).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 60, 0.37).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 100, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_preservation__hybrid_atrophy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__mourning_practice_reading).

% DUAL FORMULATION NOTE:
% Three sibling stories decompose the natural-language concept 'the ritual preserves catastrophe memory' per the epsilon-invariance principle: survival_competence_reading (ε low, claims ongoing operational transfer, classifies near rope), mourning_practice_reading (ε low-moderate, claims purely symbolic value with no operational claim to falsify, classifies near stable identity_coordination), and this hybrid_atrophy_reading (ε moderate and rising, claims a functional-to-symbolic TRANSITION, classifies as piton). The three share a beneficiary/victim referent — the standing ritual practice — but differ in what they claim about its historical and present content, which is exactly the kind of natural-language ambiguity the framework requires decomposing rather than averaging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
