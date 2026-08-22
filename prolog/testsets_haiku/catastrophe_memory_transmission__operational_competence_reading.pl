% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__operational_competence_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: catastrophe_memory_transmission__operational_competence_reading
 *   human_readable: Catastrophe Memory as Operational Competence Transmission
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint instantiates the OPERATIONAL COMPETENCE READING of the
 *   catastrophe-memory transmission kernel. The reading frames ritual as a
 *   mechanism that encodes and rehearses survival competence—pattern
 *   recognition, resource coordination under scarcity, threat
 *   assessment—through structured repetition and embodied practice.
 *   Passover's rapid-departure protocol, Tisha B'Av's resource-scarcity
 *   disciplines, and parallel rituals in other high-trauma communities are
 *   read as survival-training systems that persist across institutional
 *   collapse. The constraint benefits future survival capacity by preserving
 *   operationally-critical knowledge in non-propositional form. This is a
 *   ROPE (coordination mechanism solving a genuine problem: encoding
 *   competence for transmission across catastrophes) from the reading's
 *   perspective, though the claim/metric divergence is intentional—the
 *   reading asserts coordination while measuring modest extractiveness (0.38)
 *   to capture the asymmetry between what the reading claims and what
 *   operational measurement describes.
 *
 * KEY AGENTS:
 *   - ritual_practitioners: Embodied participants in the encoded competence; they execute the patterns without necessarily understanding them as survival-training.
 *   - community_coordinators: Maintain ritual fidelity; their authority rests on competence-transmission claim.
 *   - future_descendants: Non-agent beneficiary category; inherit the encoded knowledge.
 *   - symbol_continuity_traditionalists: Excluded from this reading's frame; would contest the instrumentalization of ritual.
 *   - secular_practitioners: Participate without the operational frame; potentially miss the encoded content.
 *   - institutional_memory_systems: Observer seat; cross-cultural evidence for competence-encoding hypothesis.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Catastrophe Memory as Operational Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, '2edc5b72-b676-4d2b-a2ba-0ab9a8304e52').
narrative_ontology:cs_kernel_codification('2edc5b72-b676-4d2b-a2ba-0ab9a8304e52', distributed).
narrative_ontology:cs_authority_grounding('2edc5b72-b676-4d2b-a2ba-0ab9a8304e52', practice).
narrative_ontology:cs_interpretation_layer_present('2edc5b72-b676-4d2b-a2ba-0ab9a8304e52').
narrative_ontology:cs_reading_relation('2edc5b72-b676-4d2b-a2ba-0ab9a8304e52', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('2edc5b72-b676-4d2b-a2ba-0ab9a8304e52', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('2edc5b72-b676-4d2b-a2ba-0ab9a8304e52', foundational, ritual_encodes_operational_competence).
narrative_ontology:cs_axiom_status(ritual_encodes_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('2edc5b72-b676-4d2b-a2ba-0ab9a8304e52', ritual_encodes_operational_competence, empirically_contingent).
narrative_ontology:cs_axiom('2edc5b72-b676-4d2b-a2ba-0ab9a8304e52', foundational, embodied_practice_preserves_response_automaticity).
narrative_ontology:cs_axiom_status(embodied_practice_preserves_response_automaticity, holdable).
narrative_ontology:cs_axiom_grounding('2edc5b72-b676-4d2b-a2ba-0ab9a8304e52', embodied_practice_preserves_response_automaticity, empirically_contingent).
narrative_ontology:cs_reference_frame('2edc5b72-b676-4d2b-a2ba-0ab9a8304e52', ritual_as_survival_training).
narrative_ontology:cs_drift_state('2edc5b72-b676-4d2b-a2ba-0ab9a8304e52', contemporary_institutional_documentation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2edc5b72-b676-4d2b-a2ba-0ab9a8304e52', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_survival_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, ritual_practitioners).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, secular_practitioners).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, pattern_recognition_under_duress).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, resource_coordination_efficiency).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, threat_assessment_rehearsal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in structured catastrophe-memory rituals (Passover seder, Tisha B'Av fasting, Nakba day commemoration) that encode survival behaviors: rapid departure readiness, resource scarcity response, threat recognition patterns. The ritual embeds operational competence through repeated enactment—muscle memory for evacuation routes coded into narrative structure, rationing disciplines coded into fasting protocol, threat-assessment frameworks coded into story-telling sequences.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_practitioners, beneficiary,
    organized, generational, constrained, global).

% Inherit the encoded competence through cultural transmission. The constraint benefits this non-agent category by preserving survival knowledge in forms that persist even when written records do not, when institutions collapse, when formal education is blocked.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, future_descendants, beneficiary,
    powerless, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__operational_competence_reading, future_descendants).

% Maintain and transmit the ritual structure across generations. They decide which elements are core (resource coordination sequences, threat-assessment narratives) and which are peripheral (symbolic elaborations). They enforce the practice schedule and teach the embedded competence to new cohorts. Their authority rests on the claim that ritual fidelity preserves operational yield.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, community_coordinators, agenda_setter,
    organized, generational, constrained, regional).

% Would argue that ritual's primary function is identity preservation and mourning, not operational competence extraction. They are excluded from the operational-competence reading's framing—their voice would highlight the parts of ritual that carry symbolic but not operationally-testable content, and would resist the reduction of ritual to survival-training machinery.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, symbol_continuity_traditionalists, excluded,
    organized, generational, constrained, regional).

% Participate in the ritual forms but without the operational-competence frame—they attend Passover seder for family continuity, Tisha B'Av for identity affirmation, but do not read the narrative as rapid-departure training. They bear the time cost and cognitive effort of ritual participation while potentially missing the operationally-encoded information this reading claims is the constraint's real function. Their exit option is reframing ritual as symbolic only.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, secular_practitioners, payer,
    moderate, biographical, mobile, national).

% Record and analyze ritual structures across cultures and historical periods. They observe that high-trauma societies often encode survival competence in ceremonial forms, and that ritual persistence correlates with group survival through repeated crises. This observer seat provides cross-cultural evidence for the competence-transmission hypothesis.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, institutional_memory_systems, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__operational_competence_reading, institutional_memory_systems).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__operational_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__operational_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes and rehearses survival competence (pattern recognition, resource coordination under scarcity, threat assessment) in forms that persist across institutional collapse, written-record destruction, and transmission blockages. The ritual solves the problem of preserving operationally-critical knowledge in non-propositional, embodied form—muscle memory, narrative pacing, repeated enactment that trains response patterns.
% TRANSFER_FUNCTION: Transfers survival-critical information from prior catastrophe (siege, diaspora, displacement, mass threat) to future cohorts through structured practice. The mechanism moves implicit operational competence—not data, but trained response capacity—from elders through ritual enactment to youth who will face the next crisis.
% ABSENT_VOICES: Symbol-continuity traditionalists would argue that reducing ritual to operational competence training misses the intrinsic value of mourning, identity preservation, and relational bonding. Secular practitioners who treat ritual as purely symbolic (rather than as encoded survival training) would challenge the operationalist reading as instrumentalizing sacred practice. Both are structurally excluded from the operational-competence framing because the framing depends on reading ritual elements as survival-behaviors encoded, not as symbols appreciated for their own sake.
% DISAPPEARANCE_RATIONALE: The operational-competence reading asserts that if catastrophe-memory ritual vanished, survival capacity would degrade—the next generation would lack the rehearsed threat-assessment patterns, resource-scarcity disciplines, and rapid-response protocols encoded in ritual form. The symbol-continuity reading contests this, arguing that ritual's disappearance would damage identity and mourning practice but not operationally-measurable survival capacity. The contest hinges on whether the ritual's operational content is separable from its symbolic content, and whether survival competence can be transmitted through non-ritual channels.
% FOUNDING_PROBLEM: Catastrophic threat (siege, diaspora, forced displacement, mass persecution) occurs at intervals longer than individual lifespans and often destroys institutional memory systems. Ritual encodes survival competence in embodied, non-propositional forms that persist even when writing systems, formal institutions, and documentation are destroyed by the same catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological and historical analysis documents that high-trauma societies frequently develop ritualized encoding of survival knowledge (e.g., Passover's rapid-departure protocol maps to Bronze Age exigencies; Tisha B'Av fasting disciplines map to siege-rationing requirements). Cognitive science research on embodied learning supports the claim that repeated enactment encodes response patterns more reliably than verbal instruction. However, scholars in symbol-continuity traditions contest whether these operational elements are primary or incidental; they would argue the founding problem is the need to preserve identity and mourning, not operational competence per se. Outside both reading camps, emergency-management researchers and disaster-recovery specialists have observed that communities with strong ritual-encoded threat-response protocols do recover faster from crises—but dispute whether this is because ritual preserves competence or because ritual-practicing communities maintain other forms of mutual aid and institutional memory.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__operational_competence_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__operational_competence_reading_tests).
:- end_tests(catastrophe_memory_transmission__operational_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.38) because the constraint does not concentrate benefit or burden on a single actor—it distributes survival-critical knowledge across a community, and the 'extraction' is the cognitive and time cost of ritual participation weighed against diffuse survival benefit. The measurement series shows extractiveness rising from 0.28 to 0.38 over the interval (t=0 to t=25), reflecting increasing professionalization and codification of ritual-as-training as modern institutional systems document and systematize what were historically embedded practices—as ritual becomes more explicitly framed as competence-training (rather than implicit embodied practice), the measurement captures this shift as extractive overhead. Theater_ratio rises from 0.08 to 0.18, indicating that as ritual becomes more self-conscious about its operational function, a growing share of ritual activity is devoted to explicit instruction and explanation rather than pure enactment—the 'theater' is the pedagogical framing, not a decline in function. Accessibility_collapse is high (0.72) because once the operational competence frame is adopted, alternatives (written manuals, classroom instruction, disaster drills) do not replace ritual encoding—embodied, repeated enactment produces response automaticity that propositional instruction does not. Resistance is moderate (0.45) because secular practitioners and symbol-continuity readers actively resist the operational frame, even while participating in the ritual forms.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats diverge sharply. Ritual practitioners and future generations are beneficiaries (receive the encoded competence), but secular practitioners who participate without the operational frame are partial payers (time cost, cognitive effort) with unclear benefit recognition. Community coordinators are agenda_setters and beneficiaries (they maintain authority by being guardians of competence transmission). Symbol-continuity traditionalists are excluded because this reading's frame directly contradicts their understanding of ritual's primary function.
 *
 * DIRECTIONALITY LOGIC:
 *   The operational_competence_reading declares future_survival_capacity as the beneficiary (a non-agent entity that represents the outcome the constraint is structured to produce). Ritual_practitioners are beneficiaries because they receive encoded competence, though they may not consciously recognize it. Community_coordinators are beneficiaries (their role is justified by competence-preservation claims) and agenda_setters (they maintain ritual structure). Secular_practitioners are payers (time cost) because they participate without full recognition of the operational frame—they bear the constraint's overhead but may not access its benefit. Symbol_continuity_traditionalists are excluded by the reading's frame, not by structural position. The directionality derivation should show: agenda_setters near beneficiary end (d near 0.1-0.2); ritual_practitioners near symmetric (d near 0.4-0.5, receiving real benefit but investing time); secular_practitioners slightly toward target end (d near 0.55-0.65, time cost without acknowledged benefit); future_descendants at full beneficiary (d near 0.0, benefit without cost). No override needed—the structural data produces appropriate d values from the beneficiary/victim declarations and exit constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (preserving survival competence across catastrophes that destroy institutions) has contested status (live for some communities, dead or solved through alternative means for others). The disappearance_verdict is also contested: operational-competence readers argue the world would rearrange (survival capacity would degrade); symbol-continuity readers argue the world would be impoverished in mourning/identity but not necessarily less capable of surviving. The constraint is NOT resolved mandatrophy (the founding problem remains contested, not clearly dead). The operational-competence reading's persistence does depend on continued belief that ritual encodes competence more reliably than alternatives—if that claim were decisively refuted (e.g., by evidence that secular survival-training produces equal or better outcomes), the constraint would face mandatrophy pressure. But the claim remains live in the literature and in community practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_yield_measurement,
    'Can survival competence encoded in ritual form be measured independently, and does it correlate with actual survival outcomes under repeated catastrophic threat?',
    'Longitudinal historical analysis comparing survival rates, recovery times, and institutional persistence across communities that maintain catastrophe-memory ritual versus matched communities without such rituals, controlling for other institutional and resource differences. Contemporary disaster-response studies comparing ritual-practicing versus non-ritual communities. Experimental cognitive science research on retention and automaticity of responses trained through embodied repetition versus propositional instruction.',
    'If operational yield is measurable and correlates strongly with survival, the operational-competence reading gains empirical support and the constraint''s type may shift toward mountain (natural law of cultural transmission). If yield is not measurable or does not correlate significantly, the constraint is better understood as symbolic or coordinating community identity rather than operationally productive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_yield_measurement, empirical, 'Whether ritual-encoded competence produces measurable survival advantages.').

omega_variable(
    reading_frame_dependency,
    'Is the constraint''s function (what it transmits) independent of how the reading frames it, or does the operational-competence frame constitute the competence it claims to preserve?',
    'Ethnographic research comparing ritual practice in communities where operational-competence framing is explicit versus communities where the same ritual forms persist without operational justification. Analysis of historical periods when the operational frame was absent—did the same rituals still transmit survival competence, or did the frame itself become necessary to preserve the competence?',
    'If the frame is constitutive (the competence exists because and only because participants read the ritual as competence-training), the constraint is better understood as a narrative or symbolic boundary that coordinates interpretation, not as a natural law of cultural transmission. If the competence persists independent of framing, the operational-competence reading captures a real structural fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_frame_dependency, conceptual, 'Whether operational competence is a property of ritual or a property of how the ritual is read.').

omega_variable(
    symbolic_embodiment_ambiguity,
    'Are symbolic form and operational content separable, or does the ritual encode competence precisely by embedding it in symbolic form such that competence cannot be extracted without symbol?',
    'Attempt to translate ritual-encoded competence into explicit, propositional survival-training manuals and test whether transmission via manual alone preserves the operationally-critical elements. Research on non-propositional knowledge and embodied cognition to determine whether ritual''s symbolic form carries functional information that propositional translation loses.',
    'If competence and symbol are inseparable, the hybrid_embedded_reading is correct and the operational/symbol boundary this reading presupposes is false. If competence can be extracted and transmitted separately from symbol, the operational-competence reading''s framing is coherent. If competence transmission requires symbol but could use alternative symbolic forms, the constraint coordinates a symbolic boundary (this reading) rather than discovering a necessity of cultural transmission.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbolic_embodiment_ambiguity, conceptual, 'Whether operational competence requires symbolic embedding or merely happens to be embedded in ritual.').

omega_variable(
    identity_lock_mechanism_for_traditionalists,
    'Do symbol-continuity traditionalists resist the operational-competence frame because it is empirically wrong, or because they have a relational/ideological stake in reading ritual as intrinsically meaningful rather than instrumentally competence-preserving?',
    'Ethnographic interviewing of traditional practitioners to identify the mechanism of resistance. Analysis of whether traditionalist resistance decreases if empirical evidence for operational yield emerges, or remains constant even with supporting evidence. Historical analysis of whether the symbol-continuity reading itself changed when ritual function came under scrutiny from modern institutions.',
    'If resistance is purely empirical (traditionalists believe the operational frame is factually wrong), evidence could shift the reading consensus. If resistance is relational/ideological (traditionalists are identity-locked to a pre-operational reading of ritual''s meaning), the constraint''s persistence depends on managing the competing readings as incommensurable rather than competing-for-truth. If resistance is identity-locked, then the constraint distributes costs differently across seats: traditionalists bear a cost (their reading is excluded) even if the operational frame is empirically superior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_traditionalists, empirical, 'Whether symbol-continuity resistance is factual or identity-locked.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement_basis(cata_tr_t5, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement_basis(cata_tr_t15, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(cata_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(cata_be_t5, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(cata_be_t15, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(cata_be_t25, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_transmission__operational_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__operational_competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__operational_competence_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% The catastrophe-memory-transmission kernel decomposes into three distinct constraint stories, each instantiating a different reading of the same ritual apparatus. The operational-competence reading (this file) evaluates ritual by operational yield. The symbol-continuity reading evaluates ritual as identity preservation and mourning. The hybrid-embedded reading asserts that operational competence and symbolic form are inseparable. Each reading has its own ε, its own beneficiary/victim structure, and its own type. They are linked because they compete for the same phenomena—the same ritual forms—and a change in one reading's acceptance affects the others' institutional status. This story's influences and coexists_with relations are declared in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
