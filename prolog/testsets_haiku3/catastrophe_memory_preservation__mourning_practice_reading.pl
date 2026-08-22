% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__mourning_practice_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Catastrophe Memory Preservation Through Mourning Ritual
 *   domain: religious/social/collective_memory
 *
 * SUMMARY:
 *   In this reading, catastrophe-memory ritual is understood as a
 *   coordination mechanism that preserves collective identity through
 *   voluntary, embodied practice. Participants gather annually or
 *   periodically to enact a shared narrative of the catastrophe and its
 *   meaning, binding the community emotionally and generationally. The ritual
 *   requires no external enforcement — participation is opt-in and the
 *   constraint's power lies in its role as a marker of belonging. The reading
 *   treats the ritual as functionally successful at what it claims to do:
 *   keep memory alive through living practice, not through institutional
 *   archive alone. Extractiveness is low because no actor extracts material
 *   benefit; beneficiaries are the community members themselves, who receive
 *   continuity and identity in return for attention and participation.
 *
 * KEY AGENTS:
 *   - ritual_community: primary beneficiary (receives belonging, continuity, identity through participation)
 *   - ritual_practitioners: agenda-setters (maintain form, teach transmission, moderate changes)
 *   - younger_generation: secondary beneficiaries and observers (inherit ritual, decide transmission forward)
 *   - external_secular_institutions: excluded parties (operate parallel memory mechanisms, claim codified authority)
 *   - non_participating_members: observers and potential returners (signal possible atrophy)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.18).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Catastrophe Memory Preservation Through Mourning Ritual").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious/social/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, 'f463d71b-a45f-4daf-b6ca-1fd5b647946e').
narrative_ontology:cs_kernel_codification('f463d71b-a45f-4daf-b6ca-1fd5b647946e', implicit).
narrative_ontology:cs_authority_grounding('f463d71b-a45f-4daf-b6ca-1fd5b647946e', practice).
narrative_ontology:cs_interpretation_layer_present('f463d71b-a45f-4daf-b6ca-1fd5b647946e').
narrative_ontology:cs_reading_relation('f463d71b-a45f-4daf-b6ca-1fd5b647946e', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f463d71b-a45f-4daf-b6ca-1fd5b647946e', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('f463d71b-a45f-4daf-b6ca-1fd5b647946e', foundational, ritual_performativity_preserves_meaning).
narrative_ontology:cs_axiom_status(ritual_performativity_preserves_meaning, holdable).
narrative_ontology:cs_axiom_grounding('f463d71b-a45f-4daf-b6ca-1fd5b647946e', ritual_performativity_preserves_meaning, conventional).
narrative_ontology:cs_axiom('f463d71b-a45f-4daf-b6ca-1fd5b647946e', foundational, voluntary_participation_constitutes_binding).
narrative_ontology:cs_axiom_status(voluntary_participation_constitutes_binding, holdable).
narrative_ontology:cs_axiom_grounding('f463d71b-a45f-4daf-b6ca-1fd5b647946e', voluntary_participation_constitutes_binding, deontological).
narrative_ontology:cs_reference_frame('f463d71b-a45f-4daf-b6ca-1fd5b647946e', collective_memory_through_embodied_practice).
narrative_ontology:cs_drift_state('f463d71b-a45f-4daf-b6ca-1fd5b647946e', modernity_with_historical_distance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f463d71b-a45f-4daf-b6ca-1fd5b647946e', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, ritual_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, younger_generation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in annual or periodic mourning rituals that mark the catastrophe's anniversary. The ritual reaffirms group membership, transmits the community's narrative of what happened and why it matters, and renews commitment to collective memory. Participants may choose to attend or not; the ritual's power lies in its voluntary adoption as a meaningful identity marker.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_community, beneficiary,
    organized, generational, mobile, local).

% Designated keepers of ritual form — elders, religious specialists, community leaders — who maintain the ritual's structure, teach it to younger generations, and make decisions about modification or continuity. They invest effort in transmission but do not extract material benefit from participation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_practitioners, agenda_setter,
    moderate, biographical, constrained, local).

% Learn the ritual from practitioners and develop their own relationship to the catastrophe through participation. Their attendance is culturally encouraged but not coerced. They inherit both the ritual form and the responsibility to decide whether and how to transmit it forward.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, younger_generation, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__mourning_practice_reading, younger_generation, observer).

% Schools, museums, government commemoration bodies operate parallel memory-preservation mechanisms (curricula, monuments, official narratives). The ritual and these institutions occupy the same commemorative space but operate on different logics — the ritual claims continuity through lived practice, the institutions claim authority through codification.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, external_secular_institutions, excluded,
    institutional, generational, analytical, national).

% Community members who have drifted from ritual participation, assimilated to secular frameworks, or made conscious choices not to attend. They retain option to return; their non-participation does not prevent ritual continuity but does signal possible constraint atrophy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, non_participating_community_members, observer,
    moderate, biographical, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of keeping catastrophe memory alive in a form that binds the community through lived practice rather than through external institutional authority. The ritual embeds the narrative in sensory, emotional, and relational experience — participants enact what they must remember together, making the memory durable across generations without requiring each generation to independently rediscover why the catastrophe matters.
% TRANSFER_FUNCTION: Transfers no material goods or services. The constraint moves attention, emotional labor, and symbolic meaning: from older practitioners to younger ones, from individual memory to collective narrative, from private grief to shared ritual time. Participants invest time and emotional presence; they receive belonging and continuity in return.
% ABSENT_VOICES: Non-participating community members who have stepped away from ritual practice are structurally excluded from the ritual's decision-making about its own form and meaning. Historians and external scholars who study the catastrophe operate on different logics and are not consulted about ritual modifications. Descendants of the catastrophe's perpetrators (if applicable) are often absent from the ritual space entirely.
% DISAPPEARANCE_RATIONALE: If the ritual disappeared, the community's primary mechanism for embodied, intergenerational transmission of catastrophe memory would vanish. External institutions (schools, museums, government) might preserve factual narratives, but the lived practice that binds participants emotionally and communally to the memory would be gone. Younger generations would relate to the catastrophe through decontextualized information rather than through ritual enactment. The community's coherence around this shared memory would erode.
% FOUNDING_PROBLEM: After catastrophe, a community must preserve both the fact of what happened and the meaning-structure that keeps members bound to each other through that shared memory. Writing, monuments, and testimony preserve facts; but facts alone do not bind subsequent generations to the emotional and relational stakes of the catastrophe. Ritual solves the binding problem: it makes the memory lived and embodied, not merely archived.
% FOUNDING_PROBLEM_CORROBORATION: Historians, anthropologists, and survivors who have studied post-catastrophe communities attest that memory-preservation depends on living practice, not archive alone. Communities that stopped ritualizing show measurable weakening of intergenerational memory transmission. Religious scholars and ritual specialists attest that the founding problem — how to keep meaning alive across generations — persists as long as communities exist and time passes.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end) because the ritual operates on voluntary participation and produces no material extraction — participants invest time and emotional presence; they receive identity and belonging. Suppression is minimal (0.12) because the ritual persists through cultural coherence, not through exclusion of alternatives — people who do not attend face social pressure, not coercive exclusion. Theater_ratio rises moderately (0.22 to 0.42) over the 80-year interval, signaling the constraint's drift from functionally operative (transmitting actual survival competence early) toward increasingly performative enactment (as immediate survival threat recedes and the ritual becomes a purely identity marker). The measurement grid shows slow, steady increase in theater over time — the ritual's original operational function atrophied while its symbolic function persisted, causing the ratio to rise. Suppression requirement is low and stable because the ritual does not require enforcement; participation is sustained by cultural buy-in, not by coercion.
 *
 * PERSPECTIVAL GAP:
 *   Ritual practitioners experience this constraint as a sacred trust — they are stewards of continuity and see their role as maintaining form for transmission. Younger participants experience it as identity-constituting but increasingly optional — as time passes and the catastrophe recedes into history, the ritual's urgency weakens. Non-participating community members experience the ritual as increasingly performative and potentially exclusionary (marking who belongs). External institutions experience the ritual as provincial or irrational, and claim superior authority through codified narrative. The engine should compute rope-type classification from all seats because the coordination function (keeping memory alive through living practice) remains genuine across the perspectives, even though the urgency and perceived necessity diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   The ritual_community as an organized collective sits near the beneficiary end (d ≈ 0.2): they receive identity and continuity without running the constraint; practitioners distribute the maintenance burden voluntarily. Practitioners sit near symmetric (d ≈ 0.5): they invest effort in maintaining form but receive no material extraction, only the satisfaction of stewardship and continued belonging. The younger generation sits near beneficiary (d ≈ 0.15): they inherit the practice and its identity value without having built it. Non-participating members sit near symmetric (d ≈ 0.5): they can choose to participate but face mild social pressure if they do not. The structural derivation does not require override because the power atoms and exit options (organized/moderate power, constrained/mobile exit) produce appropriate d values from the voluntary-participation structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading specifically rejects the hybrid_atrophy_reading framing. In this mourning_practice reading, the ritual is not understood as atrophied from an earlier survival function — it is understood as successfully fulfilling its actual function: preserving symbolic continuity and collective identity. The rise in theater_ratio over time is not read as functional decay but as the natural outcome of the catastrophe becoming historical rather than immediate. The reading avoids the trap of comparing the constraint's present state to an imagined past state (when survival competence supposedly mattered); instead, it examines the constraint on its own terms: does it preserve memory? Yes. Does it bind the community? Yes. Is the binding voluntary? Mostly yes. Therefore the classification is rope, not piton, because the constraint solves a real coordination problem (keeping meaning alive across generations) that persists regardless of whether the original survival threat remains acute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_functionality_vs_performativity,
    'Does the ritual''s rising theater_ratio indicate functional atrophy (the constraint is becoming pure performance with no real coordination work), or is the performative dimension actually inseparable from the coordination function (the performance IS the mechanism by which meaning is preserved)?',
    'Empirical: compare communities where rituals have been formalized/recorded (moving toward archive) with communities maintaining oral-embodied practice; measure intergenerational memory retention and identity cohesion in each. If embodied practice consistently outperforms archive on both dimensions, performativity is functional, not atrophied.',
    'If performativity is functional (not atrophied), the theater_ratio rise is evidence the constraint is working harder, not degrading — re-classify from piton candidate to stable rope. If performativity masks functional decay, the constraint is closer to hybrid_atrophy_reading than mourning_practice_reading, triggering reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_functionality_vs_performativity, empirical, 'Whether the ritual''s performative character is functional or symptomatic of atrophy.').

omega_variable(
    kernel_reading_boundary,
    'Is this reading (mourning_practice) genuinely distinct from survival_competence_reading, or are they the same constraint read through different time horizons (survival_competence when the threat is acute, mourning_practice when it has receded)?',
    'Conceptual: examine whether a single community''s members can simultaneously hold both readings about the same ritual (some see it as threat-recognition, others as identity-preservation) without contradiction. If yes, they are coexisting readings of one kernel. If the readings require different empirical premises (e.g., whether the catastrophe is repeating), they are decomposed constraints.',
    'If the readings are the same constraint read through time, the kernel boundary dissolves and both readings should be authored in a single story with time-indexed axioms. If decomposed, each story stands independently and network edges express the dependency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether mourning_practice and survival_competence are readings of one kernel or separate constraints.').

omega_variable(
    voluntary_participation_stability,
    'As the catastrophe recedes in time and becomes historical rather than experienced, does voluntary participation remain stable, or does it collapse toward pure non-participation (people stop showing up)?',
    'Longitudinal measurement: attendance data from multi-generational ritual communities; surveys of why non-participants have dropped out; comparison of participation rates when survival threat is acute vs. historical.',
    'If participation erodes to near-zero as memory becomes historical, the constraint''s ε may be higher than modeled (suppression is masked by norms that are weakening); re-classify toward piton. If participation stabilizes at a sustainable level, rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_participation_stability, empirical, 'Whether voluntary participation in mourning ritual persists as catastrophe becomes historical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(cata_tr_t60, observed).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement_basis(cata_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 60, 0.18).
narrative_ontology:measurement_basis(cata_be_t60, observed).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement_basis(cata_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 10, 0.09).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 40, 0.11).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 60, 0.12).
narrative_ontology:measurement_basis(cata_su_t60, observed).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 80, 0.12).
narrative_ontology:measurement_basis(cata_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__mourning_practice_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the catastrophe_memory_preservation kernel. The survival_competence_reading interprets ritual as preserving operational threat-recognition (mountain classification). The hybrid_atrophy_reading interprets ritual as functionally atrophied from survival role to pure mourning (piton classification). This mourning_practice_reading interprets ritual as successfully fulfilling its actual present function: preserving symbolic continuity (rope classification). The three readings produce different ε values and structural classifications because they are readings of the same kernel, not observations of different constraints. The ε-invariance principle requires separate stories per reading, with network edges expressing the kernel dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
