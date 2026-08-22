% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Mourning-Ritual as Intergenerational Trauma-Transmission Warning System
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A community that survived a historical catastrophe (persecution,
 *   expulsion, mass violence) maintains a recurring ritual calendar of
 *   mourning and re-narration. Under the trauma-encoding reading, the
 *   practice is read as a mechanism that transmits the affective and
 *   cognitive signature of catastrophic threat to generations who did not
 *   experience it directly, on the theory that felt vigilance is more
 *   reliable than intellectual awareness for detecting recurring danger. The
 *   mechanism has a genuine coordination function (a real historical failure
 *   of early recognition) but also imposes a measurable psychological cost on
 *   those who receive the transmission without having chosen it and often
 *   without the interpretive resources to contextualize it.
 *
 * KEY AGENTS:
 *   - elder_ritual_custodians: agenda_setter/beneficiary (organized/identity_locked) — administer the ritual calendar, their identity bound to its continuation
 *   - descendant_generations: payer (powerless/identity_locked) — bear diffuse anxiety and hypervigilance from communal transmission
 *   - children_of_survivors: payer (powerless/trapped) — bear concentrated, direct-household transmission with least ability to exit
 *   - collective_threat_vigilance: beneficiary, non-agent — the diffuse protective capacity credited to the mechanism
 *   - mental_health_clinicians: excluded observer-adjacent — treat symptoms but are outside the liturgical decision body
 *   - reform_minded_descendants: payer/excluded — propose reform from inside the community but are rarely heeded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.63).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.48).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Mourning-Ritual as Intergenerational Trauma-Transmission Warning System").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, '4e523ce0-9372-42cb-9011-31b11b48a54e').
narrative_ontology:cs_kernel_codification('4e523ce0-9372-42cb-9011-31b11b48a54e', implicit).
narrative_ontology:cs_authority_grounding('4e523ce0-9372-42cb-9011-31b11b48a54e', practice).
narrative_ontology:cs_interpretation_layer_present('4e523ce0-9372-42cb-9011-31b11b48a54e').
narrative_ontology:cs_reading_relation('4e523ce0-9372-42cb-9011-31b11b48a54e', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e523ce0-9372-42cb-9011-31b11b48a54e', catastrophe_memory_kernel__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('4e523ce0-9372-42cb-9011-31b11b48a54e', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('4e523ce0-9372-42cb-9011-31b11b48a54e', foundational, felt_vigilance_exceeds_intellectual_awareness).
narrative_ontology:cs_axiom_status(felt_vigilance_exceeds_intellectual_awareness, holdable).
narrative_ontology:cs_axiom_grounding('4e523ce0-9372-42cb-9011-31b11b48a54e', felt_vigilance_exceeds_intellectual_awareness, empirically_contingent).
narrative_ontology:cs_axiom('4e523ce0-9372-42cb-9011-31b11b48a54e', foundational, descendant_psychological_burden_is_a_real_cost_not_mere_inheritance).
narrative_ontology:cs_axiom_status(descendant_psychological_burden_is_a_real_cost_not_mere_inheritance, holdable).
narrative_ontology:cs_axiom_grounding('4e523ce0-9372-42cb-9011-31b11b48a54e', descendant_psychological_burden_is_a_real_cost_not_mere_inheritance, deontological).
narrative_ontology:cs_reference_frame('4e523ce0-9372-42cb-9011-31b11b48a54e', post_catastrophe_founding_generation_witness).
narrative_ontology:cs_drift_state('4e523ce0-9372-42cb-9011-31b11b48a54e', third_and_fourth_generation_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4e523ce0-9372-42cb-9011-31b11b48a54e', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, elder_ritual_custodians).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generations).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, children_of_survivors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, reform_minded_descendants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lead the commemorative liturgy, decide what details of the catastrophe are dramatized, mourned, and re-enacted each cycle, and enforce participation as a marker of belonging. Their standing within the community is bound up with being the transmitters of the memory; abandoning the ritual would cost them their role and, they believe, expose the community to unrecognized danger.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, elder_ritual_custodians, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__trauma_encoding_reading, elder_ritual_custodians, beneficiary).

% Are born into a community where the catastrophe is re-narrated as personally felt threat, often before they have the capacity to contextualize it historically. They report anxiety, hypervigilance, and a persistent sense of unresolved danger that the community explains as appropriate inheritance rather than treatable harm. Leaving the ritual cycle means leaving the community that raised them; most do not exit even when they recognize the cost.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generations, payer,
    powerless, biographical, identity_locked, national).

% Are the direct addressees of survivor testimony woven into the ritual calendar; they absorb the affective content of catastrophe narratives delivered by parents and grandparents who lived the events. Clinical literature on this population documents elevated startle response, guilt, and identity fusion with the catastrophe narrative. They have essentially no exit from the household transmission, distinct from the community-level ritual.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, children_of_survivors, payer,
    powerless, biographical, trapped, local).

% The community's standing early-warning capacity — heightened sensitivity to precursor signs of persecution, expulsion, or violence — that the ritual is credited with maintaining. Not an actor itself; a collective capacity that accrues from the transmission mechanism, named here for completeness of the beneficiary structure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance).

% Treat descendants for anxiety and trauma symptoms with no clear precipitating event in the patient's own life, and increasingly recognize the pattern as a transmission effect of communal mourning practice. Their clinical framing rarely enters the community's own account of what the ritual is for; they are not part of the liturgical or communal decision-making body.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, mental_health_clinicians, excluded,
    moderate, biographical, analytical, national).

% Adults who have come to see the intensity of ritualized catastrophe transmission as harmful to children and have proposed softening or reframing commemorative practice for the youngest cohort. Their proposals are typically read by custodians as forgetting or assimilation, and are rarely adopted; they remain inside the community but at its margin on this question.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, reform_minded_descendants, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__trauma_encoding_reading, reform_minded_descendants, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__trauma_encoding_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__trauma_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The community faces a genuine recurring risk-detection problem: historically, early warning signs of persecution were missed or dismissed by prior generations who lacked a felt sense of danger. Ritualized transmission of catastrophe narrative is a mechanism for keeping the community's threat-detection apparatus 'live' across generations who did not personally experience the precipitating events.
% TRANSFER_FUNCTION: Moves psychological burden — anxiety, hypervigilance, identity fusion with catastrophe — from the generation that experienced the events to generations that did not, in exchange for a diffusely held collective capacity to recognize threat precursors faster than an unwarned community would.
% ABSENT_VOICES: Descendants who experience the transmission primarily as harm, and clinicians who treat its symptoms, are structurally outside the liturgical decision-making body; the ritual calendar is set by custodians whose own identity is bound to its continuation, not by a body that weighs the transmission's psychological cost against its protective yield.
% DISAPPEARANCE_RATIONALE: Custodians and much of the older generation would say the community's protective vigilance and cohesion would visibly erode without the ritual — that assimilation and forgetting would follow within a generation or two, echoing historical patterns they hold responsible for prior catastrophes. Reform-minded descendants and clinicians would say the community would persist, and its members would suffer measurably less, with threat-awareness achievable through less totalizing means (education, archive, commemoration without re-enactment). Both positions are genuinely held within the same community, which is why this sits at contested rather than a clean verdict.
% FOUNDING_PROBLEM: A specific historical catastrophe was not adequately anticipated or resisted because the generation living through its early stages did not recognize the danger signs in time; the ritual was built, at least in part, to ensure that failure of recognition would not recur.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the originating catastrophe and some communal elders corroborate that failure of early recognition was a real causal factor, supporting the founding problem's historicity. However, whether the PRESENT intensity and mode of transmission (versus a less totalizing commemorative form) remains necessary to that founding problem is disputed by clinicians and reform-minded descendants, who are outside the custodial body and who attest that the current form now generates more psychological harm than warning capacity it demonstrably preserves.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.63, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.63) reflects a moderate-to-high but not extreme reading: the ritual has a real, historically grounded coordination rationale (the founding catastrophe was worsened by a documented failure of early recognition), which caps this below a pure-snare reading, but the cost falls concentrated and involuntarily on children and descendants who had no part in choosing the transmission mode, which is what pushes it well above a rope. Suppression (0.48) is moderate and, notably, trending slightly DOWN over the measured interval — as clinical documentation of transmission harms has accumulated and reform-minded voices have gained some standing, the community's capacity to compel full participation without dissent has eroded somewhat, even as extractiveness itself has crept upward (more intensified, elaborated ritual content over successive commemorative cycles, a mild ratchet). Theater ratio (0.28) is low-moderate: the protective/warning function is not fictional, but an increasing share of ritual elaboration (0.12 to 0.28 over the interval) appears to serve communal identity performance and status competition among custodians rather than the original threat-detection function — a mild Goodhart drift worth flagging without claiming the whole practice is now theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Elder custodians sit near the beneficiary end: they administer the mechanism, their communal standing derives from doing so, and the psychological costs fall on others, not themselves as the primary transmission target (though they too carry the original trauma). Descendant generations and children of survivors sit near the target end: they receive the transmitted content without having chosen it, bear its symptomatic costs, and have identity-locked or fully trapped exit options respectively — children of survivors more so, given the household-level (versus community-level) character of their exposure. Collective_threat_vigilance is marked as a non-agent beneficiary (agent: false) because it is a diffuse capacity, not an actor that could itself be held accountable or that collects rents in any ordinary sense — it is named for completeness of the beneficiary structure per the schema's non-agent registry guidance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (failure of early threat recognition led to catastrophe) is genealogically real and corroborated by historians outside the custodial body — this is not a manufactured founding myth. But founding_problem_status is authored as contested rather than dead or live because the two live communal factions disagree on whether TODAY's specific transmission intensity remains proportionate to that founding function, or whether the mechanism has drifted into serving custodian identity and status maintenance (rising theater_ratio) beyond what threat-detection alone would require. Classifying this as tangled_rope rather than snare or rope preserves that ambiguity structurally: there is a real coordination kernel (making it not a pure snare) but also an identifiable, involuntary, asymmetric cost borne by a population that does not administer or set the terms of the mechanism (making it more than a pure rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vigilance_benefit_measurability,
    'Does the ritual''s transmission mechanism actually produce measurably superior threat-detection capacity in descendants, or is the protective benefit assumed rather than demonstrated?',
    'Comparative study of communities with high-intensity ritualized transmission versus communities using non-traumatic historical education about the same catastrophe class, measuring actual early-warning behavior in analogous threat scenarios.',
    'If the vigilance benefit is not empirically distinguishable from education-based awareness, the extraction is closer to pure cost-without-corresponding-coordination-function, pushing the classification toward snare. If the benefit is real and specific to the affective transmission mode, the tangled_rope reading (real coordination function plus real cost) is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vigilance_benefit_measurability, empirical, 'Whether ritual-transmitted vigilance is causally superior to non-traumatic historical education for threat-detection.').

omega_variable(
    kernel_reading_selection,
    'Is ''trauma encoding'' the correct primary lens for this ritual, or is the trauma-transmission effect a side effect of a mechanism whose primary function is symbol continuity, survival-skill transmission, or boundary maintenance (the three sibling readings)?',
    'This is inherently a framing question rather than a fact resolvable by data alone; it would be informed by which stakeholders'' own self-understanding of the ritual''s purpose is treated as authoritative, and by whether the psychological-harm literature is centered or treated as incidental in communal accounts of the practice.',
    'Adopting a different sibling reading as primary would not change this story''s own ε (each reading is ε-invariant and separately authored per the kernel/reading discipline) but would change which constraint a given observer treats as the dominant account of the SAME ritual practice, with correspondingly different beneficiary/victim structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which of the four kernel readings (trauma_encoding, symbol_continuity, survival_competence, boundary_maintenance) is the dominant structural account of this practice, versus a co-present but secondary effect.').

omega_variable(
    harm_attribution_specificity,
    'Is the anxiety and hypervigilance documented in descendant populations specifically attributable to the ritual mechanism, or to broader intergenerational trauma transmission pathways (family narrative, epigenetic hypotheses, general minority-stress) of which the ritual is only one channel?',
    'Within-community comparison of households/individuals with high versus low ritual participation, controlling for other transmission pathways (direct survivor testimony, general communal narrative exposure, socioeconomic stress).',
    'If the ritual specifically (versus general communal narrative) is the dominant transmission channel, the extraction attribution to THIS constraint is well-grounded. If ritual participation is not separable from general household narrative exposure, some of the authored extractiveness may be more properly attributed to a broader, unritualized transmission constraint not modeled here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_attribution_specificity, empirical, 'Whether documented psychological harm is specifically attributable to ritual participation versus general intergenerational narrative transmission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 50, 0.61).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 40, 0.49).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 50, 0.485).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__trauma_encoding_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the catastrophe_memory_kernel, each authored as a separate ε-invariant constraint per the decomposition principle. trauma_encoding_reading authors the highest extractiveness of the four candidate readings because it centers the involuntary psychological cost borne by descendants as the primary transfer, whereas symbol_continuity_reading and survival_competence_reading center benefits (identity preservation, adaptive skill) that are more symmetrically distributed across the community, and boundary_maintenance_reading centers a different victim set (out-group / marginal-member exclusion) entirely. All four should be generated as separate files and cross-linked via affects_constraints; none of the four's ε should be treated as commensurable with, or a component of, the others' — they are structurally distinct claims sharing one narrative label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
