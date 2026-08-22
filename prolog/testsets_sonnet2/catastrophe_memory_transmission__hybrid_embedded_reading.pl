% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__hybrid_embedded_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Ritual Fidelity as Co-Constitutive Substrate for Catastrophe-Survival Knowledge Transmission
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This story instantiates the hybrid_embedded_reading of the
 *   catastrophe_memory_transmission kernel: symbolic form and operational
 *   survival competence are treated as co-constitutive rather than as a
 *   wrapper around extractable content. A community facing a recurring,
 *   low-frequency catastrophic hazard transmits response competence (timing,
 *   sequencing, spatial coordination, threat cues) through faithful ritual
 *   enactment rather than explicit instruction. Unlike the
 *   operational_competence_reading (which treats the ritual as a vehicle
 *   whose payload is the competence and whose form is instrumentally
 *   replaceable) or the symbol_continuity_reading (which treats
 *   identity/mourning preservation as the survival mechanism, with
 *   operational content secondary), this reading holds that the two are
 *   inseparable: altering the enacted form measurably degrades the
 *   transmitted competence, and the competence has no existence independent
 *   of correctly enacted form. ε is low because the arrangement is a genuine
 *   coordination mechanism with no identifiable extractive party — the
 *   beneficiaries are the same population that maintains the practice, across
 *   time.
 *
 * KEY AGENTS:
 *   - practicing_community_members: primary beneficiaries and bearers of the transmission burden (organized/constrained)
 *   - ritual_specialists: agenda-setters whose office is constituted by fidelity maintenance (moderate/identity_locked)
 *   - future_generations_facing_recurrence: downstream beneficiaries with no voice in current transmission choices (powerless/analytical)
 *   - innovators_seeking_reform: excluded voice proposing content/form separation (moderate/constrained)
 *   - external_ethnographers: analytical observers correlating fidelity with measured outcomes (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.14).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Ritual Fidelity as Co-Constitutive Substrate for Catastrophe-Survival Knowledge Transmission").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, 'ef0a8821-55ee-4bec-bf8e-83b8b12fddff').
narrative_ontology:cs_kernel_codification('ef0a8821-55ee-4bec-bf8e-83b8b12fddff', implicit).
narrative_ontology:cs_authority_grounding('ef0a8821-55ee-4bec-bf8e-83b8b12fddff', practice).
narrative_ontology:cs_interpretation_layer_present('ef0a8821-55ee-4bec-bf8e-83b8b12fddff').
narrative_ontology:cs_reading_relation('ef0a8821-55ee-4bec-bf8e-83b8b12fddff', catastrophe_memory_transmission__operational_competence_reading, influences).
narrative_ontology:cs_reading_relation('ef0a8821-55ee-4bec-bf8e-83b8b12fddff', catastrophe_memory_transmission__symbol_continuity_reading, influences).
narrative_ontology:cs_axiom('ef0a8821-55ee-4bec-bf8e-83b8b12fddff', foundational, form_function_coconstitution).
narrative_ontology:cs_axiom_status(form_function_coconstitution, holdable).
narrative_ontology:cs_axiom_grounding('ef0a8821-55ee-4bec-bf8e-83b8b12fddff', form_function_coconstitution, empirically_contingent).
narrative_ontology:cs_axiom('ef0a8821-55ee-4bec-bf8e-83b8b12fddff', secondary, non_propositional_transmission_irreducibility).
narrative_ontology:cs_axiom_status(non_propositional_transmission_irreducibility, holdable).
narrative_ontology:cs_axiom_grounding('ef0a8821-55ee-4bec-bf8e-83b8b12fddff', non_propositional_transmission_irreducibility, empirically_contingent).
narrative_ontology:cs_reference_frame('ef0a8821-55ee-4bec-bf8e-83b8b12fddff', embedded_competence_transmission_baseline).
narrative_ontology:cs_drift_state('ef0a8821-55ee-4bec-bf8e-83b8b12fddff', contemporary_literacy_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('ef0a8821-55ee-4bec-bf8e-83b8b12fddff', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_specialists).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, future_generations_facing_recurrence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the ritual cycle across a lifetime, absorbing embodied cues (timing, spatial arrangement, sequence of acts) that encode when and how to respond to a recurring hazard. Cannot extract the operational content and discard the symbolic form without degrading recall and coordination fidelity; the two arrive bundled through repeated enactment. Exit from the practice is possible but costs the transmitted competence itself, not merely social standing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community_members, beneficiary,
    organized, generational, constrained, regional).

% Maintain fidelity of the ritual sequence across performances, correcting drift in enactment and serving as the living reference for correct form. Their authority and self-understanding are constituted by faithful transmission; they benefit from social standing but the office exists to keep the embedded knowledge intact, not to extract surplus from participants.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_specialists, agenda_setter,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_specialists, beneficiary).

% Not yet born or not yet present when the hazard recurs; they will inherit whatever operational competence the ritual has preserved through faithful enactment by prior generations, with no ability to negotiate the transmission mechanism they depend on.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, future_generations_facing_recurrence, beneficiary,
    powerless, civilizational, analytical, regional).

% Would argue for modernizing or streamlining the ritual — extracting the 'lesson' into explicit instruction and dropping costly or opaque elements. Structurally sidelined because the hybrid-embedded reading holds that separating content from form degrades the very competence being preserved, so their proposals get little institutional purchase.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, innovators_seeking_reform, excluded,
    moderate, biographical, constrained, regional).

% Study the ritual's dual function from outside the community, documenting correlations between fidelity of enactment and measured outcomes (response times, resource allocation accuracy) when the hazard recurs, without being bound by the transmission mechanism themselves.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, external_ethnographers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__hybrid_embedded_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__hybrid_embedded_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Faithful ritual enactment coordinates a community's collective response to a low-frequency, high-consequence hazard by embedding operational knowledge (timing, sequence, spatial coordination, threat cues) inside a symbolic practice that everyone learns through participation rather than through explicit instruction.
% TRANSFER_FUNCTION: Transmits accumulated survival competence from generation to generation through repeated embodied enactment; nothing is extracted from any party to benefit another — the arrangement moves knowledge across time, not resources across persons.
% ABSENT_VOICES: Innovators who would prefer to codify the operational lessons explicitly and discard costly or opaque ritual elements are largely unheard; the hybrid-embedded reading treats their proposed separation as itself a threat to the competence being preserved, so their case rarely enters deliberation on equal footing.
% DISAPPEARANCE_RATIONALE: If ritual fidelity collapsed overnight, the embedded operational knowledge would not survive as a separable residue — testimony from communities that lost ritual continuity after displacement or forced conversion shows measurable degradation in hazard-response competence within two generations, not mere loss of symbolic meaning.
% FOUNDING_PROBLEM: A recurring catastrophic hazard (flood, volcanic event, famine cycle, or comparable low-frequency disaster) required a transmission mechanism robust enough to survive across generational gaps longer than direct memory, without relying on literacy or centralized record-keeping.
% FOUNDING_PROBLEM_CORROBORATION: External ethnographers and hazard historians studying comparable communities attest that recurrence intervals for the underlying hazard remain within the range the ritual cycle addresses, and that measured response competence tracks ritual fidelity independently of community self-report — this corroboration comes from outside the practicing community and outside the ritual specialists who administer it.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 0.14, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).
:- end_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness stays low and only drifts slightly upward across the 200-unit interval (0.10 to 0.14) because no party captures a surplus from the arrangement; the tiny rise reflects incremental specialist-office consolidation typical of any durable institution, not extraction from participants. Suppression (0.22) is low-moderate: fidelity is maintained through social correction and identity investment rather than coercion, though drift-correction by specialists has a mild suppressive edge for would-be reformers. Accessibility_collapse is high (0.72) because, once a community understands that operational competence is bundled with enacted form, alternative transmission paths (explicit instruction, written codification) are seen — accurately, per this reading's premises — as inadequate substitutes, which sharply narrows perceived alternatives even though no one is coercively blocked from trying them. Resistance is low (0.28): the main resistance comes from the excluded innovator faction, not from the beneficiary population, who mostly experience the arrangement as sensible practice rather than as an imposition.
 *
 * PERSPECTIVAL GAP:
 *   Ritual specialists and ordinary practicing members should compute close to the same seat under this reading (both beneficiaries/coordination participants), which is itself informative: the hybrid_embedded reading predicts LOW seat divergence compared to the operational_competence_reading (where specialists might extract status disproportionate to competence transmitted) or the symbol_continuity_reading (where meaning-preservation could diverge sharply from operational payoff for participants who never face the hazard). The absence of strong divergence here is evidence for, not against, the coordination-with-mountain-substrate classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (practicing members, specialists, future generations) all sit near the low end of directionality because the constraint transmits something they need and could not otherwise reliably acquire; no group is identified as bearing a net cost through the mechanism itself. Innovators are excluded rather than victimized — their exclusion is from the conversation, not from a material extraction. No victims are declared because the analytical claim is that discontinuing the practice, not maintaining it, is where costs would fall (on future generations who would lose transmitted competence) — but future_generations_facing_recurrence are coded as beneficiaries of the present arrangement, not victims of it, since the arrangement is what protects rather than harms them.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy risk this reading is built to avoid is mislabeling a still-functioning coordination mechanism as pure symbolic performance (theater) simply because its content is non-propositional and hard to verify from outside. Because the founding_problem_status is 'live' (the hazard recurrence interval has not elapsed) and corroborated by observers outside the practicing community, mandatrophy is NOT indicated here — the founding problem persists and the mechanism that solves it persists correspondingly. Were the hazard demonstrably retired (founding_problem_status: dead) while the ritual persisted at full intensity, THAT would be the mandatrophy signature — and it would push this story toward piton, not rope. The rising theater_ratio trend (0.10 to 0.18) is worth watching as an early signal but has not crossed a threshold that would indicate substitution of performance for function within this interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_function_separability,
    'Can the operational competence encoded in the ritual be extracted and transmitted through an alternative, non-ritual channel (explicit instruction, written protocol) without measurable degradation, or is the embedded-form claim correct that no separable extraction is possible?',
    'Comparative studies of communities that lost ritual continuity (displacement, forced conversion, modernization campaigns) against communities that maintained it, measuring hazard-response competence at time of recurrence, controlling for hazard frequency and severity.',
    'If competence transmits adequately through non-ritual channels, this reading collapses toward the operational_competence_reading (ritual becomes an inefficient or optional vehicle, not a co-constitutive substrate) and the classification would likely shift toward scaffold or piton depending on whether reform is underway. If competence reliably fails to transmit through non-ritual channels, the hybrid_embedded reading is vindicated and the rope-with-mountain-substrate classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(form_function_separability, empirical, 'Whether ritual form and operational competence are truly inseparable, as this reading claims, or separable in practice.').

omega_variable(
    specialist_office_capture_risk,
    'Does the ritual_specialists'' role, over long time horizons, drift from fidelity-maintenance (coordination function) toward status-extraction (specialists benefiting disproportionately from gatekeeping correct form, regardless of whether competence is actually transmitted)?',
    'Longitudinal tracking of specialist compensation/status relative to measured transmission fidelity and hazard-response outcomes; a divergence where specialist status rises while outcome-linked fidelity metrics stagnate or fall would indicate capture.',
    'If specialist benefit decouples from transmission fidelity, the constraint would reclassify toward tangled_rope (coordination function persisting alongside an emergent extractive layer), requiring victims to be named among the practicing community.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(specialist_office_capture_risk, empirical, 'Whether the specialist office risks drifting from pure coordination maintenance into partial extraction over generational timescales.').

omega_variable(
    kernel_reading_choice_evidentiary_basis,
    'What observational signal, if any, distinguishes the hybrid_embedded_reading from the operational_competence_reading and the symbol_continuity_reading, given that all three describe the same enacted practice?',
    'Controlled degradation studies: selectively alter symbolic elements of the ritual judged ''non-operational'' by the operational_competence_reading''s own taxonomy, and separately alter operationally-coded elements judged ''merely symbolic'' by the symbol_continuity_reading''s taxonomy, then measure competence outcomes under both perturbations.',
    'If both perturbation types degrade competence outcomes comparably, the hybrid_embedded reading''s co-constitutive claim is supported over either separable-function sibling reading. If only operationally-coded elements matter, the operational_competence_reading is supported instead, and this story''s classification would need revision toward a more instrumental, less mountain-substrate framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_choice_evidentiary_basis, conceptual, 'The framing choice among the three kernel readings is itself under-determined by observation of a single intact practice; only intervention data could discriminate them, and none yet exists for most such rituals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(cata_tr_t120, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 120, 0.15).
narrative_ontology:measurement(cata_tr_t160, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 160, 0.17).
narrative_ontology:measurement(cata_tr_t200, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 200, 0.18).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 80, 0.12).
narrative_ontology:measurement(cata_be_t120, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 120, 0.13).
narrative_ontology:measurement(cata_be_t160, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 160, 0.13).
narrative_ontology:measurement(cata_be_t200, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 200, 0.14).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_transmission__hybrid_embedded_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__hybrid_embedded_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, symbol_continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the catastrophe_memory_transmission kernel, all sharing the same enacted practice but authoring structurally distinct claims about what is transmitted and how. hybrid_embedded_reading claims co-constitutive inseparability (rope-with-mountain-substrate, ε≈0.14). operational_competence_reading (sibling) claims the ritual is an instrumentally replaceable vehicle for an extractable competence payload — likely also low-ε rope but with a different disappearance profile (competence might survive form loss). symbol_continuity_reading (sibling) claims identity/mourning preservation is itself the survival mechanism, deprioritizing operational payoff — likely rope with different beneficiary emphasis (meaning-preservation over hazard-response). Per the ε-invariance principle, these are three separate constraints, not one constraint measured three ways; each carries its own ε, stakeholders, and omegas, linked here via affects_constraints rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
