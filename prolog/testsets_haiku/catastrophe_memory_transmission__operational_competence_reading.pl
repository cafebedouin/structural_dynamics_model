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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_transmission__operational_competence_reading
 *   human_readable: Catastrophe Memory Transmission via Operational Competence Encoding
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint reads the kernel 'catastrophe_memory_transmission'
 *   through the operational competence lens: ritual encodes and transmits
 *   survival-critical knowledge—rapid departure readiness (Passover),
 *   resource-scarcity discipline (Tisha B'Av), threat assessment under
 *   duress—through embodied practice and pattern recognition. This is NOT a
 *   reading of ritual as symbol-continuity or embedded-symbolism; it is
 *   specifically about operational yield. The constraint is classified as
 *   rope (coordination mechanism solving a genuine survival-knowledge
 *   transmission problem) under this reading, though the claimed type
 *   reflects uncertainty about whether the mechanism operates as genuine
 *   coordination or as something more foundational (the omega variables
 *   document this ambiguity).
 *
 * KEY AGENTS:
 *   - ritual_participants: organized agents performing the annually-rehearsed response sequence; internalizing threat-response competence
 *   - community_knowledge_holders: interpret ritual elements for operational relevance in contemporary conditions; maintain the encoding-to-action mapping
 *   - future_generations: inherit the practiced competence through generational transmission; powerless receivers benefiting from ancestors' encoding work
 *   - secular_interpreters: could validate operational yield but are structurally excluded from authority circles
 *   - literalist_practitioners: bear the cost of ritual performance without extracting the operational benefit; at risk of fragmenting the transmission chain
 *   - analytical_observer: examines whether the constraint maintains operational function or drifts to symbol-only
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.31).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.18).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Catastrophe Memory Transmission via Operational Competence Encoding").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, '5b45c1df-9e0f-40c9-b982-df9c038804e8').
narrative_ontology:cs_kernel_codification('5b45c1df-9e0f-40c9-b982-df9c038804e8', distributed).
narrative_ontology:cs_authority_grounding('5b45c1df-9e0f-40c9-b982-df9c038804e8', practice).
narrative_ontology:cs_interpretation_layer_present('5b45c1df-9e0f-40c9-b982-df9c038804e8').
narrative_ontology:cs_reading_relation('5b45c1df-9e0f-40c9-b982-df9c038804e8', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b45c1df-9e0f-40c9-b982-df9c038804e8', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('5b45c1df-9e0f-40c9-b982-df9c038804e8', foundational, survival_competence_operationally_measurable).
narrative_ontology:cs_axiom_status(survival_competence_operationally_measurable, holdable).
narrative_ontology:cs_axiom_grounding('5b45c1df-9e0f-40c9-b982-df9c038804e8', survival_competence_operationally_measurable, empirically_contingent).
narrative_ontology:cs_axiom('5b45c1df-9e0f-40c9-b982-df9c038804e8', secondary, embodied_knowledge_superior_to_propositional).
narrative_ontology:cs_axiom_status(embodied_knowledge_superior_to_propositional, holdable).
narrative_ontology:cs_axiom_grounding('5b45c1df-9e0f-40c9-b982-df9c038804e8', embodied_knowledge_superior_to_propositional, empirically_contingent).
narrative_ontology:cs_reference_frame('5b45c1df-9e0f-40c9-b982-df9c038804e8', operational_competence_encoding_intact).
narrative_ontology:cs_drift_state('5b45c1df-9e0f-40c9-b982-df9c038804e8', contemporary_attestation_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5b45c1df-9e0f-40c9-b982-df9c038804e8', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, community_survival_capacity).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).

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
 *   Extractiveness is moderate-low (0.31 at interval end) because the constraint is primarily coordination—it solves a genuine problem (survival-knowledge transmission) that participants choose to solve via ritual rather than being coerced into a one-way transfer. Suppression is very low (0.18) because the constraint's persistence rests on participants recognizing its operational value, not on enforced compliance. Theater-ratio rises gradually (0.08 to 0.22 over the interval), indicating slow drift toward symbol-only performance as some practitioners lose sight of the operational decoding and the constraint becomes aestheticized rather than functionally rehearsed. Accessibility collapse is high (0.72) because the embodied knowledge is available nowhere else—written rules and verbal instruction fail under actual threat; once you recognize this, the 'alternative' of symbolic purity without operational rehearsal collapses as inaccessible to survival. Resistance is moderate (0.42) because some participants reject the operational reading and insist the constraint is about symbol and identity, not survival—that contention sustains a permanent pressure against full acceptance of the operational framing.
 *
 * PERSPECTIVAL GAP:
 *   Community knowledge-holders seat: the constraint is transparent coordination—a real problem (survival-knowledge transmission), a solution that works (embodied ritual practice), participants who benefit and choose to maintain it. Future populations seat: the constraint is a gift they do not choose; their survival capacity is increased but they bear no active cost. Literalist practitioners seat: the constraint becomes extraction—they perform costly ritual for reasons they do not understand (symbol/devotion) while missing the operational benefit, bearing cost without agency. The analytical observer seat: the constraint is empirically measurable (correlate ritual elements with survival outcomes) but the authority structure for interpretation excludes secular measurement. The engine computes these divergences from the stakeholder structural data; the reading does not assert a single unified type across all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual participants are beneficiaries (they receive practiced competence for their own threat-response capacity) and agenda-setters (they maintain the practice). Future generations are beneficiaries (they inherit competence without choice). Literalist practitioners are payers (they bear the cost of performance without extracting the survival benefit). Community knowledge-holders are agenda-setters (they interpret and maintain the encoding). Secular interpreters and literalists are somewhat excluded from the authority structure that decides how the constraint evolves, even though they would contribute to validating its operational yield. The directionality for each seat flows from their structural relationship to the survival-knowledge transfer, not from formal role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: communities still face catastrophic threats and still need reliable survival-knowledge transmission. The constraint is NOT mandatrophic in its current operational form. However, the rising theater_ratio (drift toward symbol-only performance) flags a risk: if the community and knowledge-holders lose sight of the operational decoding, the constraint will become mandatrophic—the ritual will persist as symbol/identity/mourning-practice while the survival-competence transmission function atrophies. The R5 mismatch check (founding_problem_status='live' + disappearance_verdict='world_rearranges') does NOT flag mandatrophy now, but the measurement series show the condition emerging. The omega variables document this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_decoding_preservation,
    'Does the constraint transmit operational survival knowledge, or is the knowledge now primarily symbolic/aesthetic, with the operational yield atrophying across generations?',
    'Empirical measurement: track whether participants in the ritual demonstrate measurably higher threat-response readiness, resource-scarcity discipline, and rapid-decision competence than non-participants in stress-test scenarios. Compare survival outcomes in real catastrophe between communities with maintained operational-decoding traditions and those without.',
    'If operational yield persists measurably, the constraint remains rope (coordination with genuine survival benefit). If operational yield has decayed and only symbol-continuity persists, the constraint reclassifies toward piton (atrophied function, maintained by inertia/identity). The theater_ratio rise across the interval suggests this trajectory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operational_decoding_preservation, empirical, 'Whether the constraint still transmits operational competence or has drifted to symbol-only performance.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Do the operational_competence_reading and the symbol_continuity_reading logically foreclose each other, or can a single community hold both simultaneously?',
    'Ethnographic observation: do communities that emphasize operational decoding (e.g., teaching Passover ritual as evacuation-readiness practice) simultaneously value and practice the symbol-continuity elements (mourning, identity-affirmation)? Or do they treat symbol-elements as optional/secondary? Can a person authentically hold ''this ritual transmits survival competence AND it preserves our identity and mourning-practice'' without logical contradiction?',
    'If the readings coexist without contradiction, they are coexist_with relations. If communities that adopt the operational reading actively suppress the symbol-reading, the relation becomes forecloses (rare). If the operational reading creates downstream pressure that redefines what counts as valid symbol-maintenance, the relation is influences. The resolution matters for mapping which sibling reading this constraint neighbors in the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Logical structure of the contest between operational and symbolic readings.').

omega_variable(
    secular_authority_exclusion_effect,
    'Does the structural exclusion of secular interpreters and empirical validators from the community''s authority circle for ritual interpretation degrade the constraint''s operational yield?',
    'Comparative case study: communities that maintain secular-empirical cross-checking of ritual-element relevance versus communities that exclude it. Measure the rate of theater-ratio rise, the preservation of operational decoding across generations, and the responsiveness of the ritual to actual threat changes.',
    'If secular inclusion improves the constraint''s operational yield and slows theater-drift, the exclusion is itself a suppression mechanism weakening the constraint. If secular involvement is neutral or degrades the constraint, the current authority structure is not extractive. The finding would affect how to classify the excluded stakeholder: are they genuinely absent voices whose exclusion hurts coordination, or are they outside the constraint''s domain entirely?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_authority_exclusion_effect, empirical, 'Whether the authority-structure exclusion of secular interpreters weakens operational transmission.').

omega_variable(
    kernel_reading_distinction_sharpness,
    'How clearly does the operational_competence_reading distinguish itself from the hybrid_embedded_reading? Is the distinction in what is transmitted (propositional operational knowledge vs. non-propositional embedded knowledge), or in how we measure success?',
    'Analytical: specify what counts as evidence that operational knowledge is being transmitted (e.g., behavioral changes under threat, post-ritual self-reports of readiness, performance on stress-test drills) versus evidence that embedded knowledge is being transmitted (e.g., practitioners report a felt knowledge they cannot articulate, improvement in responses without explicit learning). If these evidence bases differ, the readings are separable; if they point to the same phenomena described differently, the readings may be semantically identical.',
    'If the readings are semantically identical (just different vocabulary for the same mechanism), the kernel is over-split and should be recombined. If they are separable (different mechanisms, different evidence profiles), the constraint family stands as authored and the omega documents a solved ambiguity. This omega is an explicit defense against false decomposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_distinction_sharpness, conceptual, 'Semantic distinctness of operational vs. embedded-knowledge readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 80, 0.22).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 60, 0.31).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 80, 0.31).

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
% The kernel 'catastrophe_memory_transmission' is instantiated by three structurally distinct constraint stories: (1) symbol_continuity_reading emphasizes identity and mourning-practice as intrinsic goods; (2) hybrid_embedded_reading emphasizes non-propositional knowledge embedded within symbolic form, inseparable from it; (3) operational_competence_reading (this constraint) emphasizes measurable operational yield and threat-response competence encoded in ritual sequence. Each reading carries its own epsilon value, beneficiary structure, and type classification. They are linked as coexistent or influential readings of a single contested kernel, not as reformulations of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
