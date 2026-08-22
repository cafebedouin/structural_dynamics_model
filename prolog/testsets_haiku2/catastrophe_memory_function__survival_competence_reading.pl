% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Ritual Transmission of Catastrophe-Survival Competence
 *   domain: religious/cultural/institutional
 *
 * SUMMARY:
 *   This constraint reads the catastrophe-memory kernel through the lens of
 *   survival-competence transmission: commemorative rituals (exemplified by
 *   Passover and analogous practices in Indigenous and post-disaster
 *   cultures) function to encode and distribute adaptive knowledge for
 *   institutional transformation and decentralized continuity. The
 *   survival-competence reading isolates the transmission mechanism — how a
 *   ritual body preserves functional knowledge of catastrophe response,
 *   adaptation, and institutional resilience across generations and across
 *   geographic dispersal. The reading asserts that the ritual's primary (or
 *   equal-primary) function is not memorialization of loss, but rehearsal and
 *   transmission of the adaptive strategies that enabled survival. This
 *   reading coexists with the mourning-practice reading (which emphasizes
 *   memorial obligation and identity boundary maintenance) and the hybrid
 *   reading (which holds that both functions operate together). This JSON
 *   instantiates ONLY the survival-competence reading; the other two are
 *   separate constraint stories linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - ritual_community: bearers and practitioners of the commemorative ritual; participate in embodied transmission and decentralized knowledge distribution
 *   - institutional_memory_system: the distributed network of participants through which adaptive knowledge persists across time and geographic separation
 *   - future_adaptive_capacity: the downstream institutional resilience enabled by transmission; not an agent, but a vindicated proposition about what the constraint enables
 *   - catastrophe_recognition: the condition that triggers understanding of what knowledge the ritual encodes; a structural referent, not an agent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.18).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Ritual Transmission of Catastrophe-Survival Competence").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious/cultural/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, '738bd948-fb8d-4911-b375-8147f034e9f4').
narrative_ontology:cs_kernel_codification('738bd948-fb8d-4911-b375-8147f034e9f4', distributed).
narrative_ontology:cs_authority_grounding('738bd948-fb8d-4911-b375-8147f034e9f4', practice).
narrative_ontology:cs_interpretation_layer_present('738bd948-fb8d-4911-b375-8147f034e9f4').
narrative_ontology:cs_reading_relation('738bd948-fb8d-4911-b375-8147f034e9f4', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('738bd948-fb8d-4911-b375-8147f034e9f4', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('738bd948-fb8d-4911-b375-8147f034e9f4', foundational, adaptive_transmission_is_primary_function).
narrative_ontology:cs_axiom_status(adaptive_transmission_is_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('738bd948-fb8d-4911-b375-8147f034e9f4', adaptive_transmission_is_primary_function, empirically_contingent).
narrative_ontology:cs_axiom('738bd948-fb8d-4911-b375-8147f034e9f4', foundational, decentralized_distribution_enables_resilience).
narrative_ontology:cs_axiom_status(decentralized_distribution_enables_resilience, holdable).
narrative_ontology:cs_axiom_grounding('738bd948-fb8d-4911-b375-8147f034e9f4', decentralized_distribution_enables_resilience, empirically_contingent).
narrative_ontology:cs_reference_frame('738bd948-fb8d-4911-b375-8147f034e9f4', distributed_embodied_knowledge_transmission).
narrative_ontology:cs_drift_state('738bd948-fb8d-4911-b375-8147f034e9f4', contemporary_institutional_rationalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('738bd948-fb8d-4911-b375-8147f034e9f4', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, community_continuity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, institutional_memory_bearers).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, future_adaptive_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, ritual_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participants in commemoration of catastrophe (Passover participants, Indigenous ceremony keepers, disaster-response communities). They maintain the ritual, transmit knowledge across generations, and adapt the ritual's form to new contexts while preserving its encoding of survival knowledge. They do not extract from others; they participate voluntarily (though enculturation shapes choice). They collectively author the constraint through participation, without centralized authority dictating form. Exit from the constraint is constrained by cultural identity fusion (breaking with the ritual means breaking with the community), but the constraint itself does not suppress alternatives — practitioners choose the ritual because they value transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_community, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, ritual_community, agenda_setter).

% Formal and informal keepers of transmitted knowledge (historians, elders, community archivists, cultural institutions that preserve ritual records). They receive the constraint's benefit (access to encoded knowledge they help preserve) and contribute to its maintenance. They may experience the constraint as obligatory (cultural duty to preserve), but the obligation is internal to their identified role, not externally imposed.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, institutional_memory_bearers, beneficiary,
    organized, generational, constrained, global).

% Future generations who inherit adaptive knowledge encoded in the ritual. They are not yet in the constraint's circle; they are analytic beneficiaries of knowledge transmission that will occur. If catastrophe strikes, they will possess embodied, distributed knowledge of adaptation because the ritual was maintained. This seat is structurally analytical: future actors cannot be parties to present decisions, but the constraint's entire purpose is to benefit them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, future_institutional_actors, beneficiary,
    powerless, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_function__survival_competence_reading, future_institutional_actors).

% State and secular institutional actors that do not participate in the ritual but observe its operation (government agencies, educational institutions, heritage preservation bodies). They may support or oppose the constraint (funding transmission or restricting ritual practice), but they are not constituted by the constraint itself. Under the survival-competence reading, they have no extraction relationship to the ritual; they are outsiders assessing its social function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, secular_institutional_authorities, observer,
    institutional, biographical, analytical, national).

% Institutional knowledge transmission systems (formal education, digital archiving, organizational training) that might offer alternative paths to adaptive knowledge. They are excluded not by suppression, but by structural incomparability: written instruction, digital repositories, and institutional training do not replicate embodied, distributed, intergenerational transmission. If they were included (if the ritual were replaced by formal curricula), the constraint would be structurally different. Their exclusion is structural, not enforced.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, competing_knowledge_systems, excluded,
    moderate, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The coordination problem solved: how can adaptive knowledge of surviving catastrophe, institutional transformation, and decentralized continuity persist across generations and geographic dispersal? Written records are fragile; centralized institutions fail; embodied, distributed knowledge survives because it lives in many bodies, not in one text or one hierarchy. The ritual encodes survival knowledge in performance, memory, and transmitted practice.
% TRANSFER_FUNCTION: What moves: adaptive institutional knowledge (how to preserve community identity and function across catastrophe, how to transform institutional forms when existing forms fail, how to maintain continuity when centralized authority collapses). From whom to whom: from the ritual community and institutional memory bearers (present) to future institutional actors (those who will face catastrophe). The transfer is diffuse and distributed; there is no concentrated capturer, no extraction.
% ABSENT_VOICES: Secular institutional authorities and competing knowledge systems (educational, archival, corporate) would argue that formal instruction, written documentation, and digital preservation better serve knowledge transmission. They are excluded from the constraint's operation not by suppression but by structural design: the ritual's benefit is specifically its embodied, distributed character, which formal alternatives do not replicate. If they were included (if the ritual were replaced), the constraint would be a different constraint.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the community would lose access to embodied, distributed knowledge of catastrophe survival. Future institutional actors would lack the transmitted adaptive capacity. New catastrophe would strike without the encoded knowledge that enabled prior survival. The community's capacity to maintain identity across geographic dispersal would degrade because the ritual is the primary vehicle for distributed memory maintenance. Institutional transformation would become slower and more chaotic because the ritual encodes institutional adaptation mechanisms.
% FOUNDING_PROBLEM: Communities facing catastrophe — displacement, persecution, institutional collapse — require knowledge of how to survive, adapt, and maintain continuity. Centralized institutions fail; written records are lost; but distributed embodied knowledge survives because it lives in many bodies. The ritual was built to solve this problem: encode survival knowledge in performance, distribute it across the community, transmit it across generations, make it resilient to institutional failure.
% FOUNDING_PROBLEM_CORROBORATION: Communities in diaspora (Jewish communities maintaining Passover across 2000 years of geographic dispersal and institutional discontinuity; Indigenous communities maintaining ceremonies across colonization and forced relocation) attest the problem remains live: survival of identity and adaptive capacity across catastrophe is an ongoing need, not a historical artifact. Historians and anthropologists document that communities maintaining ritual transmission survive institutional and geographic catastrophe with greater institutional continuity than those that abandon ritual. Disaster-response researchers note that communities with embedded cultural memory of past crises (encoding adaptive strategies in ritual and narrative) mobilize faster and more effectively in new crises. The founding problem is corroborated by observers outside the benefiting parties: historians studying diaspora, researchers studying disaster resilience, and institutional analysts studying decentralized continuity mechanisms.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__survival_competence_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because this reading frames the ritual's operation as non-extractive coordination: participants voluntarily participate in knowledge transmission that benefits future adaptive capacity without a structured transfer from targets to beneficiaries — the constraint is a collective good, not a extraction mechanism. Suppression is very low (0.12) because the reading does not require coercive maintenance; ritual participants choose participation (some through cultural enculturation, but not through direct suppression). Theater ratio is minimal (0.08) because under this reading the performance is functionally valuable — it actually transmits knowledge — not primarily theatrical maintenance of institutional form. Accessibility collapse is moderate-high (0.72) because once the ritual's encoding is understood, alternatives (written manuals, institutional training, digital repositories) are theoretically available, but they do not replicate the embodied, distributed, intergenerational transmission mechanism; the constraint's structure makes alternatives appear incomplete. Resistance is moderate (0.35) because contemporary secular and institutional actors challenge whether ritualized transmission outperforms explicit instruction, but the constraint does not rely on suppressing this challenge — communities that value adaptive transmission maintain the ritual; those that do not, abandon it. Measurements show stability across the interval: extractiveness and suppression remain flat because the constraint's function (adaptive transmission) does not depend on enforcement or asymmetric transfer.
 *
 * PERSPECTIVAL GAP:
 *   Institutional authorities (rabbinical, ecclesiastical, state) that administer ritualized commemoration may experience the constraint differently: they see themselves as custodians of transmission, whereas distributed community members experience participation as volitional knowledge-sharing. The survival-competence reading emphasizes the distributed perspective — the constraint works precisely because knowledge is not monopolized by central authority, but distributed across practitioners. If authority becomes concentrated and transmission becomes coercive (top-down dictation of ritual form), the constraint shifts from rope (genuine coordination) toward tangled rope or even snare (authority extraction). The measurement interval shows stability, suggesting no drift toward concentration; if concentration were occurring, extractiveness and suppression would rise.
 *
 * DIRECTIONALITY LOGIC:
 *   The survival-competence reading does not posit asymmetric benefit/cost. The 'beneficiaries' in the base_properties are community_continuity, institutional_memory_bearers, and future_adaptive_capacity — all diffuse, non-extractive beneficiaries. There are no victims because the reading does not claim extraction. Directionality across all seats is near-symmetric (d ≈ 0.5): participants invest time and attention in ritual performance and transmit knowledge; they receive adaptive capacity and cultural continuity in return. The absence of an agenda-setter is structural to this reading: the ritual is not administered by a concentrated authority extracting from dispersed participants, but is distributed across the community. If a reading-specific agenda-setter emerged (e.g., rabbinical authorities centralizing Passover interpretation), that would shift the directionality and would belong to a different constraint story (hybrid or mourning-practice reading, which do feature authority structures).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (catastrophe survival through distributed knowledge) remains live in this reading's framework: communities that have experienced catastrophe (diaspora, displacement, persecution, institutional breakdown) continue to enact rituals encoding survival knowledge. The constraint does not display mandatrophy (atrophied function maintained theatrically) under this reading because the function (adaptive transmission) is continuously vindicated by historical circumstances and by communities' voluntary participation. A mandatrophy diagnosis would arise if the ritual persisted despite the founding problem being solved — if communities with institutional continuity and external security unambiguously abandoned the ritual while those facing catastrophe did not. The measured theater_ratio of 0.08 indicates minimal performative overhead; most of the constraint's operation is functional knowledge transmission, not theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_competence_vs_mourning_boundary,
    'Does this reading''s focus on adaptive transmission structurally foreclose the mourning-practice reading''s focus on memorial obligation, or do both readings coexist in the same ritual performance?',
    'Ethnographic analysis of practitioner accounts: do ritual participants experience the performance as primarily (a) transmitting survival knowledge, (b) maintaining memorial obligation, or (c) both simultaneously? Structural coexistence requires that individual participants can hold both meanings without logical contradiction.',
    'If survival competence and mourning are structurally inseparable (both axioms instantiate in one performance), the kernel supports a hybrid reading rather than competing readings. If they partition the meaning-space (different participants or different performance phases emphasize different aspects), they coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_competence_vs_mourning_boundary, conceptual, 'Whether survival-competence and mourning-practice readings are logically exclusive or coinstantiated in ritual practice.').

omega_variable(
    transmission_mechanism_verifiability,
    'What constitutes evidence that a ritual actually transmits adaptive survival knowledge, versus performing symbolic commemoration?',
    'Comparative institutional analysis: do communities maintaining active survival-competence transmission rituals (Passover, Indigenous land-use ceremonies, disaster-response protocols embedded in cultural memory) demonstrate measurably higher adaptive capacity in novel crises than communities without such rituals? Or is the survival-competence claim post-hoc narrative imposed on mourning practice?',
    'If transmission is empirically verifiable via crisis-response success rates, the reading instantiates a testable constraint on institutional resilience. If transmission is structurally unverifiable (meaning is always performed, never validated), the reading is a narrative framing of mourning without independent adaptive function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transmission_mechanism_verifiability, empirical, 'Whether ritual survival-competence transmission is empirically testable or purely symbolic.').

omega_variable(
    decentralized_continuity_mechanism,
    'How does a ritual that survives destruction (because knowledge lives in distributed participants, not centralized institutions) differ structurally from a ritual that preserves institutional identity through centralized canonical transmission?',
    'Historical comparison: in diaspora contexts, do decentralized versions of the ritual (each community maintains its own practice) preserve adaptive capacity differently than canonically-centered versions? Do decentralized versions drift in ways that strengthen or degrade survival competence?',
    'If decentralization strengthens adaptive capacity (variation enables local adaptation), the survival-competence reading''s structural claim is vindicated. If decentralization degrades institutional resilience (loss of central coordination impairs response), the mourning-practice reading''s focus on canonical boundary preservation becomes dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_continuity_mechanism, empirical, 'Whether decentralized ritual transmission enhances or compromises adaptive institutional capacity.').

omega_variable(
    reading_distinctness_from_transformation,
    'Is this reading (survival-competence focus) distinctly different from the hybrid-transformation reading, or does insisting on survival competence collapse into hybrid recognition?',
    'Axiom distinction: does the survival-competence reading require that ONLY adaptive transmission matters, or does it permit mourning as a vehicle for transmission? If mourning is merely instrumental to transmission, the reading is pure survival-competence. If mourning has co-equal standing, hybrid reading holds.',
    'A pure survival-competence axiom would foreclose the hybrid reading (cannot hold both ''mourning is instrumental'' and ''mourning has intrinsic memorial value'' simultaneously). A permissive axiom permits hybrid. The chosen axiom determines the reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_distinctness_from_transformation, conceptual, 'Whether survival-competence reading axioms are logically compatible with hybrid transformation reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_function__survival_competence_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_function__survival_competence_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_function__survival_competence_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__survival_competence_reading, theater_ratio, 20, 0.08).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 5, 0.17).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 15, 0.19).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 20, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(cata_su_t5, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 5, 0.11).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 15, 0.13).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 20, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__survival_competence_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_function kernel decomposes into three distinct constraint stories, one per reading. All three share the same referent (commemorative ritual in response to catastrophe) but assign different primary functions and different extraction profiles. The survival_competence_reading isolates the adaptive transmission function; the mourning_practice_reading emphasizes memorial obligation; the hybrid_transformation_reading claims both functions operate together. Each reading produces a different ε-value and a different directionality profile. Stories are linked via network.affects_constraints to indicate family membership and kernel origins. The three readings coexist in the same ritual practice; the constraint framework models each reading as a separate constraint to enable per-reading classification and to permit empirical testing of which reading's structural claims are supported by data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
