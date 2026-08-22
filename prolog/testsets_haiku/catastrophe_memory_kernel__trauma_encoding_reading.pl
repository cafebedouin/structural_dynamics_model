% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Intergenerational Trauma Encoding in Mourning Ritual
 *   domain: religious/collective_memory
 *
 * SUMMARY:
 *   This constraint models ritual mourning practice as a mechanism for
 *   encoding intergenerational trauma, instantiating the trauma-encoding
 *   reading of the catastrophe_memory_kernel. The ritual sustains collective
 *   threat-vigilance by structuring descendants' perception and embodied
 *   response to reproduce the ancestral fear-state; the constraint extracts
 *   psychological burden (elevated anxiety, hypervigilance, intrusive memory)
 *   from younger participants to produce early-warning capacity the group
 *   interprets as protective. The reading asserts that ritual's primary
 *   mechanism is trauma transmission, not symbolic continuity or boundary
 *   maintenance — and that descendants bear a real psychological cost for
 *   this function. This is one of four structurally distinct interpretations
 *   of the same mourning practice, each producing a different classification,
 *   beneficiary set, and harm profile.
 *
 * KEY AGENTS:
 *   - Elder ritual practitioners: maintain the mourning practice, decide which trauma narratives are encoded, control initiation of younger participants into somatically enacted catastrophe memory.
 *   - Descendant bearers of trauma: participate from childhood, absorb traumatic narratives somatically, carry elevated baseline anxiety and hypervigilance across lifespan. Exit requires severing group identity.
 *   - Collective threat-vigilance (non-agent): the faculty or early-warning capacity the constraint vindicated. Beneficiary is structural rather than agentic.
 *   - Younger initiates (powerless, identity-locked): forced participation in mourning ritual before cognitive maturity or capacity to refuse. Resistance invisible because normalized as solemn duty.
 *   - Mental health practitioners (excluded): barred from reframing the ritual's burden as pathology rather than sacred knowledge. Would argue for decoupling threat-awareness from somatic trauma.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.45).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Intergenerational Trauma Encoding in Mourning Ritual").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, '0bea5993-2e7f-44ad-96bc-8c3f4b87fb19').
narrative_ontology:cs_kernel_codification('0bea5993-2e7f-44ad-96bc-8c3f4b87fb19', distributed).
narrative_ontology:cs_authority_grounding('0bea5993-2e7f-44ad-96bc-8c3f4b87fb19', lineage).
narrative_ontology:cs_interpretation_layer_present('0bea5993-2e7f-44ad-96bc-8c3f4b87fb19').
narrative_ontology:cs_reading_relation('0bea5993-2e7f-44ad-96bc-8c3f4b87fb19', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0bea5993-2e7f-44ad-96bc-8c3f4b87fb19', catastrophe_memory_kernel__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('0bea5993-2e7f-44ad-96bc-8c3f4b87fb19', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('0bea5993-2e7f-44ad-96bc-8c3f4b87fb19', foundational, trauma_transmission_functional_necessity).
narrative_ontology:cs_axiom_status(trauma_transmission_functional_necessity, holdable).
narrative_ontology:cs_axiom_grounding('0bea5993-2e7f-44ad-96bc-8c3f4b87fb19', trauma_transmission_functional_necessity, empirically_contingent).
narrative_ontology:cs_axiom('0bea5993-2e7f-44ad-96bc-8c3f4b87fb19', foundational, ancestral_catastrophe_perpetually_recurrent).
narrative_ontology:cs_axiom_status(ancestral_catastrophe_perpetually_recurrent, holdable).
narrative_ontology:cs_axiom_grounding('0bea5993-2e7f-44ad-96bc-8c3f4b87fb19', ancestral_catastrophe_perpetually_recurrent, empirically_contingent).
narrative_ontology:cs_reference_frame('0bea5993-2e7f-44ad-96bc-8c3f4b87fb19', ancestral_trauma_as_protective_knowledge).
narrative_ontology:cs_drift_state('0bea5993-2e7f-44ad-96bc-8c3f4b87fb19', contemporary_diaspora_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0bea5993-2e7f-44ad-96bc-8c3f4b87fb19', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendant_bearers_of_trauma).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, younger_generation_initiates).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, younger_generation_initiates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and transmit the mourning ritual, structuring its emotional intensity and narrative content to encode specific threat-awareness lessons. They select which catastrophe details are emphasized, which are ritually relived, and how younger participants are inducted into the felt knowledge of ancestral suffering. They justify this as protective — descendants who carry the trauma in their bodies will recognize danger earlier.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, elder_ritual_practitioners, agenda_setter,
    organized, generational, identity_locked, local).

% Participate in mourning rituals from childhood onward, absorbing traumatic narratives and enacting them somatically — crying, keening, re-experiencing ancestral fear during ceremony. They carry elevated baseline anxiety, hypervigilance, and intrusive memory across their lifespans. Exit from ritual participation would mean severing group identity and rejecting the claim that they are the carriers of collective survival knowledge.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendant_bearers_of_trauma, payer,
    moderate, biographical, identity_locked, local).

% The group's early-warning capacity for persecution, violence, or recurring catastrophe. The constraint structures descendants' perception to trigger threat-recognition faster than learning from historical accounts alone would produce. As a non-agent beneficiary — a faculty or capacity rather than an actor — it collects no rents but is vindicated by the constraint's operation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance, beneficiary,
    analytical, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance).

% Are inducted into the ritual starting in childhood, required to participate in mourning ceremonies before they have developed the cognitive capacity to consent or the social standing to refuse. They experience the emotional weight as overwhelming initially but are socialized to interpret the experience as sacred responsibility and protective gift. Their resistance is largely invisible because it is normalized as solemn duty.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, younger_generation_initiates, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__trauma_encoding_reading, younger_generation_initiates, beneficiary).

% Analyze from outside whether the ritual's trauma encoding produces adaptive threat-recognition or pathologizes descendants through inherited dysregulation. They examine generational patterns of PTSD, anxiety disorders, and survivor-guilt transmission against the counterfactual of historical knowledge transfer without somatic encoding.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, narrative_historians, observer,
    analytical, generational, analytical, global).

% Are structurally barred from the ritual space or from re-framing descendants' experience because the ritual's framing explicitly positions trauma as sacred knowledge, not pathology. Were they admitted to the conversation, they would argue for decoupling threat-awareness from somatic trauma transmission — separating the survival competence function from the psychological burden.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, mental_health_practitioners, excluded,
    institutional, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__trauma_encoding_reading, elder_ritual_practitioners).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__trauma_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits adaptive threat-recognition across generations without requiring each cohort to rediscover persecution risk from scratch. The ritual encodes survival knowledge somatically — descendants' hypervigilance and intrusive memory become an early-warning system calibrated to historical catastrophe patterns.
% TRANSFER_FUNCTION: Moves psychological burden — intrusive memory, baseline anxiety, hypervigilance, survivor-guilt — from elders who survived catastrophe to descendants who did not, in exchange for embodied threat-detection capacity the group interprets as protective.
% ABSENT_VOICES: Mental health practitioners, descendants who have left the group, secular interpreters of the same history, descendants experiencing trauma-related pathology who question whether the ritual's framing as sacred obligation masks extractive burden. These voices are structurally excluded by the ritual's claim that therapeutic reframing would weaken the warning function.
% DISAPPEARANCE_RATIONALE: If the trauma-encoding ritual disappeared, the collective threat-vigilance would degrade over a generation or two — historical knowledge would persist but without the somatic embedding that produces early-warning sensitivity. Simultaneously, descendant baseline anxiety and PTSD prevalence would shift, likely downward, absent the ritual's ongoing traumatic re-enactment. The group's self-understanding as carriers of sacred survival knowledge would dissolve.
% FOUNDING_PROBLEM: After catastrophe (persecution, genocide, diaspora), how do survivor cohorts ensure that descendants — who did not witness the original threat — will recognize and respond to its recurrence fast enough to survive? Historical narrative alone often fails to motivate the vigilance that saved the original survivors.
% FOUNDING_PROBLEM_CORROBORATION: Elders and ritual practitioners attest the founding problem is perpetually live: every generation faces new persecution threats that replicate the original catastrophe's pattern. Descendants who have exited the group, and trauma researchers studying intergenerational trauma transmission, attest the founding problem is a historical one — the original catastrophe is documented, descendant threat environment is substantially different, and the ritual's trauma encoding is producing maladaptive hypervigilance rather than adaptive protection. No corroborating voice from outside the ritual tradition affirms the ongoing catastrophe-recurrence claim.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness begins moderate (0.48) and gradually increases to plateau at 0.68 as the ritual stabilizes over generational cycles — the traumatic encoding becomes normalized and descendants internalize it as inevitable truth rather than contingent practice. Theater ratio mirrors this drift: initially moderate-to-high (0.38) as the ritual's trauma-transmission function is explicit and reinforced, then increases (0.52 at interval end) as more of the ritual's activity becomes performative — re-enacting trauma without actively teaching new threat-recognition, maintaining the form as cultural duty after the original threat environment has shifted. Suppression begins higher (0.52) because early participants resist the traumatic induction; it declines over time as internalization and identity-lock deepen — by interval end, suppression is lower (0.45) because descendants do not actively resist what they have internalized as identity and responsibility. The measurement grid shares a single time axis so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The elder practitioners and the descendant bearers compute very differently from this structure. The practitioners experience the constraint as protective wisdom-transmission and fulfill their role as agenda-setters — from their seat, the arrangement is genuine coordination solving a real threat-recognition problem. The powerless initiates and descendant bearers experience it as enforced traumatic induction with suppressed exit — from their seat, the same structure operates as extraction. Older descendants who have exited the group and are now processing inherited trauma report that the ritual's framing masked a real burden: the early-warning function, if present, did not require the magnitude of psychological harm. Mental health practitioners, observing from outside, classify the burden as trauma-transmission rather than adaptive learning. The engine computes this divergence from the power, exit_options, and role structure; the authored claim does not adjudicate between readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Elders are structural beneficiaries and agenda-setters (directionality near 0 — they maintain the practice, control its framing, and their own trauma is legitimated by descendants' participation). Collective threat-vigilance is a non-agent beneficiary (a faculty the constraint vindicated; if it were an agent, d would be near 0). Descendant bearers are the targets (d near 1 — they bear the psychological burden, have identity-locked exit, receive no direct benefit from the practice, and cannot independently choose to stop participating). Younger initiates are the most severely targeted (d at or near 1 — trapped, identity-locked, coerced into participation before capacity to consent). The powerless initiates' situation also models a potential piton signature at scale: if enough descendants have been captured by the identity-lock (born into the practice, socialized to interpret it as sacred duty) that the practice persists even among those who intellectually question it, theater_ratio rising (0.52 at interval end) would indicate inertial maintenance rather than functional threat-response. However, the suppression requirement declining (0.45 at interval end) suggests internalization rather than pure inertia — descendants are carrying the trauma as identity, not performing it under coercion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to transmit ancestral threat-recognition to descendants who did not experience the original catastrophe) is contested. Elders attest it is live — each generation faces new persecution risks that replicate the original catastrophe's structure. Descendants who have exited, and trauma researchers, attest it is dead — the original historical threat is documented, the current threat environment is substantially different, and the ritual's encoding is producing maladaptive hypervigilance rather than adaptive learning. No external corroborating voice affirms the perpetual-catastrophe claim, which is the ritual tradition's own assertion. The mismatch is between founding_problem_status=contested and disappearance_verdict=world_rearranges: if the ritual vanished, the group's collective threat-vigilance would indeed degrade, and descendant baseline anxiety would shift. This mismatch does NOT trigger mandatrophy classification here because the constraint is claimed as tangled_rope (genuine coordination + asymmetric extraction), not as mountain or piton. Were the founding problem dead and the practice maintained theatrically for inertial reasons, the theater_ratio would be much higher (0.7+) and suppression would be near baseline (0.0–0.1), indicating pure performance. The measured theater_ratio of 0.52 reflects genuine tension: the trauma encoding is functional for threat-detection AND extractive of descendant wellbeing — both are true simultaneously, which is exactly the tangled_rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trauma_encoding_vs_learned_fear,
    'Is descendants'' elevated threat-vigilance a product of somatic trauma encoding (embodied state that persists across contexts), or learned threat-recognition that they could acquire from historical narrative without the psychological burden?',
    'Longitudinal study of descendants raised within the ritual versus those who learned the same historical catastrophe narrative outside the ritual context. Measure baseline anxiety, threat-detection speed, PTSD prevalence, and adaptive vs. maladaptive hypervigilance patterns. Natural experiments from diaspora communities that abandoned the ritual while retaining historical memory.',
    'If somatic encoding is necessary for the threat-detection function, the extraction is a structural cost of the coordination. If historical learning alone produces equivalent threat-recognition, the somatic trauma transmission is pure extraction riding on coordination that could be achieved without it — pushing the constraint toward snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trauma_encoding_vs_learned_fear, empirical, 'Whether the trauma encoding is functionally necessary for early-warning capacity or an extractive add-on to historical knowledge transmission.').

omega_variable(
    generational_catastrophe_recurrence,
    'Does the original catastrophe genuinely recur across generations within this community''s lived environment, or is the ''perpetual threat'' a ritual-reinforced narrative that exaggerates actual threat-environment similarity?',
    'Comparative historical analysis of the original catastrophe event(s), the intervening period, and the contemporary context. Documentation of actual persecution/violence risk the community faces versus the narrative frame of perpetual recurrence the ritual encodes. Interviews with descendants who have exited the community about whether their actual threat environment changed.',
    'High contemporary threat recurrence would support the founding_problem_status=live reading and justify the constraint as adaption. Divergence between historical threat environment and contemporary one would support founding_problem_status=dead, reclassifying the practice as piton (inertial maintenance of a function that no longer exists) or as false-summit extraction (trauma imposed for a threat that does not materialize).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_catastrophe_recurrence, empirical, 'Whether the ritual''s encoded catastrophe genuinely recurs or whether the perpetual-threat narrative is ritual-reinforced.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (descendants'' non-resistance to the ritual) structural (they cannot exit due to economic/legal/social barriers) or internalized (they have fused their identity with the trauma-bearing role and psychologically resist their own exit)?',
    'Study of descendants who physically exited the community: do they continue experiencing pressure to re-enter and conform, or does the suppression dissolve after geographic/institutional exit? Do they report post-exit identity fragmentation or relief? Examination of the explicit socialization message (''trauma-bearing is sacred duty'' vs. ''trauma-bearing is inescapable burden'').',
    'If suppression is primarily structural (legal/economic barriers to exit), removal of barriers would enable exit and would pressure the practice toward adaptation or collapse. If internalized (identity fused with trauma-bearing role), barrier removal would not produce exit — descendants would maintain participation even if they could leave. Internalized suppression is a deeper extraction because it persists after the coercive mechanism is removed. This feeds back into the theater_ratio interpretation: if theater is high but suppression is internalized, the practice is both theatrical AND psychologically captured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether the constraint''s suppression is structural or internalized identity-lock.').

omega_variable(
    reading_foreclosure_structure,
    'Which sibling readings (symbol_continuity, survival_competence, boundary_maintenance) are logically compatible with this trauma-encoding reading within a single coherent ritual framework, and which are mutually foreclosed?',
    'Textual analysis of the ritual''s declared justifications across time and across ritual practitioners. Interviews with practitioners about whether the ritual serves trauma-transmission, identity-continuity, competence-encoding, and/or boundary-maintenance — do they affirm all four functions as simultaneous, do they prioritize one and downplay others, or do they view some as antithetical?',
    'If practitioners hold all four functions as simultaneous and non-contradictory, the constraint is a single unified reading that encompasses multiple functions — the trauma-encoding reading becomes simply the trauma-detection-focused lens on a multivalent practice. If practitioners foreclose some functions in favor of this one (e.g., ''it is trauma encoding, not identity continuity''), the readings are genuinely distinct constraints with potential foreclosure relations. If they treat different readings as equally valid but hold by different communities, the readings coexist and we have a true constraint family with no foreclosure — just competing interpretations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Whether this reading forecloses, influences, or coexists with the sibling readings in the catastrophe_memory_kernel family.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(cata_tr_t33, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 33, 0.47).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 50, 0.51).
narrative_ontology:measurement(cata_tr_t67, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 67, 0.52).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 100, 0.52).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(cata_be_t33, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 33, 0.61).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 50, 0.66).
narrative_ontology:measurement(cata_be_t67, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 67, 0.68).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(cata_su_t33, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 33, 0.47).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement(cata_su_t67, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 67, 0.44).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__trauma_encoding_reading, 0.14).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the catastrophe_memory_kernel — a contested mourning practice. The kernel itself is not a constraint; each reading instantiates a structurally distinct constraint with different ε values, beneficiary/victim sets, and classifications. The trauma-encoding reading (THIS constraint) models the ritual as psychological burden imposed for early-warning capacity; sibling readings model the same practice as identity-continuity, competence-transmission, or boundary-maintenance. The four readings all describe the same observable ritual but extract different functional and extractive relationships from it. Upstream influencers: foundational claims about trauma transmission and collective memory (neuroscience, psychology, anthropology). Downstream influenced: institutional policies on intergenerational trauma treatment, regulatory debates over cultural practice vs. child welfare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__trauma_encoding_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
