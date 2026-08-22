% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_occupation, []).

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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Study as Legitimate Occupation of Sacrificial Obligation
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   In the aftermath of the Second Temple's destruction, the rabbinic
 *   tradition confronted the impossibility of performing the sacrificial
 *   commandments. The reading 'study as occupation' (Talmud Menachot 110a:
 *   'whoever engages in the laws of sacrifice is as if he offered a
 *   sacrifice') transposes the obligation's fulfillment from material
 *   performance to hermeneutic engagement. This constraint story captures
 *   that reading as a standalone constraint: low extractiveness, no victim
 *   set, authority structure absorbing impossibility without revision
 *   pressure. The constraint operates as a rope — genuine coordination
 *   solving a collective-action problem (how to keep an impossible
 *   commandment alive) with minimal coercive overhead.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.08).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.12).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study as Legitimate Occupation of Sacrificial Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/halakhic/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, '15e1db14-4e91-46e8-a69a-0c528b1b64ba').
narrative_ontology:cs_kernel_codification('15e1db14-4e91-46e8-a69a-0c528b1b64ba', fixed_text).
narrative_ontology:cs_authority_grounding('15e1db14-4e91-46e8-a69a-0c528b1b64ba', lineage).
narrative_ontology:cs_interpretation_layer_present('15e1db14-4e91-46e8-a69a-0c528b1b64ba').
narrative_ontology:cs_reading_relation('15e1db14-4e91-46e8-a69a-0c528b1b64ba', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('15e1db14-4e91-46e8-a69a-0c528b1b64ba', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('15e1db14-4e91-46e8-a69a-0c528b1b64ba', foundational, study_constitutes_fulfillment_now).
narrative_ontology:cs_axiom_status(study_constitutes_fulfillment_now, holdable).
narrative_ontology:cs_axiom_grounding('15e1db14-4e91-46e8-a69a-0c528b1b64ba', study_constitutes_fulfillment_now, deontological).
narrative_ontology:cs_axiom('15e1db14-4e91-46e8-a69a-0c528b1b64ba', foundational, hermeneutic_engagement_as_avodah).
narrative_ontology:cs_axiom_status(hermeneutic_engagement_as_avodah, holdable).
narrative_ontology:cs_axiom_grounding('15e1db14-4e91-46e8-a69a-0c528b1b64ba', hermeneutic_engagement_as_avodah, deontological).
narrative_ontology:cs_reference_frame('15e1db14-4e91-46e8-a69a-0c528b1b64ba', rabbinic_transposition_of_temple_service).
narrative_ontology:cs_drift_state('15e1db14-4e91-46e8-a69a-0c528b1b64ba', contemporary_halakhic_consensus, gap(stable, minor, true)).
narrative_ontology:cs_created_at('15e1db14-4e91-46e8-a69a-0c528b1b64ba', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, halakhic_authorities).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, scholarly_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, lay_observant_jews).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, study_fulfills_positive_commandment).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, torah_study_as_avodah_substitute).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, halakhic_continuity_through_learning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate the hermeneutic framework that defines study as fulfillment; their interpretive authority is sustained by the kernel's stability. They neither collect rents nor bear costs from the constraint directly, but their institutional position depends on the obligation remaining intelligible and actionable through study.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, halakhic_authorities, agenda_setter,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_occupation, halakhic_authorities, beneficiary).

% Their vocational activity (learning, teaching, publishing) is structurally validated as the highest form of divine service. No extraction from them; they receive status, communal support, and institutional recognition because the constraint renders their work obligatory rather than voluntary.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, scholarly_community, beneficiary,
    organized, biographical, mobile, global).

% Participate through supporting scholars, attending shiurim, and studying themselves. The constraint gives their participation positive valence — they are 'occupying the obligation' vicariously. No coercion; exit is constrained by communal identity but not trapped.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, lay_observant_jews, beneficiary,
    moderate, biographical, constrained, global).

% Advocate for literal restoration of sacrificial performance. Their position is structurally excluded by the study-as-occupation reading, which renders literal performance unnecessary and even presumptuous. They would object to the constraint's claim that study suffices; their exclusion is hermeneutic, not physical.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, temple_mount_activists, excluded,
    organized, biographical, identity_locked, regional).

% Analyze the constraint's historical development, textual basis, and sociological function from outside the commitment. They neither benefit nor pay; their seat is analytical.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, academic_scholars_of_halakha, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the intelligibility and continuity of a positive commandment (sacrifices) whose material performance has been impossible for two millennia, by transposing its fulfillment into the hermeneutic activity of studying its laws — thereby preventing the obligation from becoming a dead letter or a source of despair.
% TRANSFER_FUNCTION: Moves the weight of obligation from material performance (animals, altar, priesthood) to cognitive performance (study, analysis, transmission). No material transfer; the transfer is semiotic — the obligation's 'site' shifts from the Temple courtyard to the study hall.
% ABSENT_VOICES: Temple Mount activists and messianic literalists who hold that the obligation cannot be fulfilled without the physical Temple, priesthood, and altar. They are excluded because their reading would require rejecting the hermeneutic move that the constraint rests on. Also absent: historical priestly lineages (kohanim) whose specific role is rendered vestigial by the transposition.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight — i.e., if the halakhic consensus shifted to 'study does NOT fulfill the obligation' — the sacrificial commandment would become a pure impossibility with no actionable form. This would restructure halakhic priorities (what is the highest mitzvah?), undermine the institutional rationale for kollel and yeshiva systems organized around sacrificial-order study, and create a crisis of 'unfulfillable obligation' that the current reading resolves.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), the positive commandments of sacrificial worship became materially impossible to perform. The rabbinic tradition faced a structural dilemma: either the obligations are void (undermining Torah's eternality), or they must be fulfilled through a substitute mode. The founding problem was preserving the obligation's force without the Temple.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Talmud itself (Menachot 110a, Ta'anit 27b) and by medieval codifiers (Rambam Hilkhot Tefillah 1:4, Ramban on Leviticus 26:11) who explicitly frame prayer and study as substitutes. However, the status is contested: the messianic_suspension reading (Rambam Hilkhot Melakhim 11:1, Raavad ad loc.) holds the problem is not solved but suspended; the study_as_archiving reading (certain achronim) holds study preserves but does not fulfill. No single external corroborator resolves the dispute — the contest is internal to the tradition.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_occupation_tests).
:- end_tests(temple_sacrifice_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08) because the constraint demands no material transfer, extracts no rents, and imposes no penalties for non-participation. Suppression is minimal (0.12) — the constraint persists through communal normativity and textual authority, not enforcement. Theater ratio is low but non-zero (0.15) — some institutional incentives (kollel funding, communal status) attach to the performance of study-as-occupation, creating a performative layer. Accessibility collapse is high (0.88) because once the hermeneutic move is accepted, alternative fulfillments (literal performance) are structurally excluded by the constraint's own logic. Resistance is near-zero (0.05) because the reading resolves an existential halakhic crisis rather than imposing a burden.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter/beneficiary seat (halakhic authorities) experiences the constraint as genuine coordination — it solves the crisis of impossible commandments. The excluded seat (Temple Mount activists) experiences it as a foreclosure — their literalist reading is rendered incoherent within the dominant framework. The engine computes this divergence from the structural data; the claimed_type (rope) reflects the dominant coordination frame, not the excluded frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities (agenda_setter/beneficiary) sit at d ≈ 0.1 — they benefit from the constraint's stabilization of their interpretive role. Scholarly community (beneficiary) at d ≈ 0.15 — their vocation is validated. Lay observant Jews (beneficiary) at d ≈ 0.3 — they participate vicariously with mild identity-constrained exit. Temple Mount activists (excluded) are not in the directionality derivation because they are structurally excluded from the constraint's operation; their d would be high if included, but the constraint's logic renders them irrelevant. Academic observers sit at analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows no mandatrophy signature. The founding problem (impossible commandments) remains live in the sense that the Temple is still absent, but the reading's solution is stable and uncontested within its framework. The arrangement does not persist by inertia — it is actively maintained through daily study cycles, institutional curricula, and liturgical embedding. If the Temple were rebuilt tomorrow, the constraint would dissolve naturally (the sunset condition is built into the reading's own logic: study occupies the obligation 'in the Temple's absence').
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_foreclosure_boundary,
    'Does the study_as_occupation reading logically foreclose the messianic_suspension reading within a single halakhic framework, or do they coexist as complementary (study now, literal performance later)?',
    'Examine whether major poskim who endorse study_as_occupation also rule that literal sacrifices will be restored (and if so, whether study continues to ''count'' post-restoration). A ruling that study fulfills the obligation *now* and sacrifices will *also* be required later would indicate coexistence; a ruling that study *replaces* sacrifices permanently would indicate foreclosure.',
    'If forecloses, the two readings cannot be held simultaneously by one authority — the kernel splits into mutually exclusive frameworks. If coexists_with, the constraint family shows internal pluralism within a single commitment system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Whether study_as_occupation and messianic_suspension are logically compatible within one framework').

omega_variable(
    study_archiving_distinction,
    'Is the distinction between study_as_occupation (fulfillment) and study_as_archiving (preservation) a genuine halakhic difference or a semantic one?',
    'Compare the practical implications: do proponents of each reading differ on what *kind* of study counts, how much study is required, or whether study without intent to fulfill is valid? If practical halakha diverges, the distinction is structural; if only theoretical framing differs, it may be semantic.',
    'If semantic, the two readings may collapse into one constraint with a framing difference. If structural, they represent distinct coordination functions (fulfillment vs. transmission) with different extractiveness profiles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_archiving_distinction, conceptual, 'Whether the occupation/archiving distinction maps to different operational requirements').

omega_variable(
    temporal_horizon_of_occupation,
    'Does the reading implicitly assume the Temple''s absence is permanent (or indefinitely extended), or does it remain agnostic on timeline?',
    'Analyze whether the reading''s proponents treat study-as-occupation as a stable steady state or an interim measure. Rambam''s formulation (''as if he offered'') suggests stable substitution; others may frame it explicitly as ''until the Temple is rebuilt.''',
    'If the reading assumes permanence, it functions more like a mountain (structural transposition). If explicitly interim, it functions as a scaffold with an implicit sunset clause tied to messianic restoration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_horizon_of_occupation, preference, 'Whether the reading''s temporal horizon is open or closed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.08).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 500, 0.1).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1000, 0.12).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1500, 0.14).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 2000, 0.15).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 500, 0.06).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1000, 0.07).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1500, 0.07).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 2000, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 500, 0.1).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1000, 0.11).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1500, 0.12).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 2000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_occupation, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% Part of the temple_sacrifice_obligation constraint family. This reading (study_as_occupation) holds ε ≈ 0.08 (low extractiveness, fulfillment-now framing). The messianic_suspension reading holds ε ≈ 0.02 (near-zero extractiveness, obligation suspended). The study_as_archiving reading holds ε ≈ 0.12 (moderate extractiveness — study as preservation labor without fulfillment valence). The three readings share the same kernel but instantiate different constraints with different beneficiary structures and coordination functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
