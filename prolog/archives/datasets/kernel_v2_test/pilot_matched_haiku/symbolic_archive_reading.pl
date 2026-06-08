% ============================================================================
% CONSTRAINT STORY: symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_symbolic_archive_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: symbolic_archive_reading
 *   human_readable: Sacrifice Law as Symbolic Archive: Study as Voluntary Cultural Preservation
 *   domain: religious_law/halakhic_authority/commitment_systems
 *
 * SUMMARY:
 *   The symbolic-archive reading frames sacrifice law as a
 *   cultural-historical archive preserved through voluntary study and
 *   interpretive practice. This reading instantiates one coherent response to
 *   the structural problem posed by the sacrifice obligation kernel: how to
 *   maintain a complex legal tradition when the primary obligation (Temple
 *   sacrifice) is not performable in the absence of the Temple. The
 *   symbolic-archive reading solves this by reframing sacrifice law study as
 *   cultural memory work rather than binding obligation. No coercion exists;
 *   no victim set emerges; beneficiaries are the Jewish collective memory and
 *   the interpretive tradition itself. This reading coexists with three
 *   sibling readings (study-as-exercise, performance-only,
 *   messianic-suspension), each offering a different structural resolution to
 *   the same kernel. The symbolic-archive reading is distinguished by its
 *   explicit claim that no binding obligation exists and that study is
 *   voluntary cultural practice.
 *
 * KEY AGENTS:
 *   - Jewish Collective Memory: Primary beneficiary (institutional/arbitrage) — benefits from continuous preservation and reinterpretation of sacrifice law tradition
 *   - Engaged Jewish Learners: Secondary beneficiary (moderate/mobile) — voluntarily participate in study; gain cultural knowledge and participate in interpretive community
 *   - Halakhic Interpretive Tradition: Institutional beneficiary (institutional/arbitrage) — maintains textual authority and interpretive continuity through study and reinterpretation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as pure coordination solving the collective-action problem of cultural memory preservation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(symbolic_archive_reading, 0.0).
domain_priors:suppression_score(symbolic_archive_reading, 0.0).
domain_priors:theater_ratio(symbolic_archive_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(symbolic_archive_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(symbolic_archive_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(symbolic_archive_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(symbolic_archive_reading, rope).
narrative_ontology:human_readable(symbolic_archive_reading, "Sacrifice Law as Symbolic Archive: Study as Voluntary Cultural Preservation").
narrative_ontology:topic_domain(symbolic_archive_reading, "religious_law/halakhic_authority/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(symbolic_archive_reading, 'b8e94903-1008-451e-afb8-708d2166f3c6').
narrative_ontology:cs_kernel_codification('b8e94903-1008-451e-afb8-708d2166f3c6', fixed_text).
narrative_ontology:cs_authority_grounding('b8e94903-1008-451e-afb8-708d2166f3c6', lineage).
narrative_ontology:cs_interpretation_layer_present('b8e94903-1008-451e-afb8-708d2166f3c6').
narrative_ontology:cs_reading_relation('b8e94903-1008-451e-afb8-708d2166f3c6', symbolic_archive_reading__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8e94903-1008-451e-afb8-708d2166f3c6', symbolic_archive_reading__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8e94903-1008-451e-afb8-708d2166f3c6', symbolic_archive_reading__messianic_suspension_reading, influences).
narrative_ontology:cs_axiom('b8e94903-1008-451e-afb8-708d2166f3c6', foundational, no_binding_obligation_without_temple).
narrative_ontology:cs_axiom_status(no_binding_obligation_without_temple, holdable).
narrative_ontology:cs_axiom_grounding('b8e94903-1008-451e-afb8-708d2166f3c6', no_binding_obligation_without_temple, deontological).
narrative_ontology:cs_axiom('b8e94903-1008-451e-afb8-708d2166f3c6', foundational, study_as_voluntary_cultural_practice).
narrative_ontology:cs_axiom_status(study_as_voluntary_cultural_practice, holdable).
narrative_ontology:cs_axiom_grounding('b8e94903-1008-451e-afb8-708d2166f3c6', study_as_voluntary_cultural_practice, conventional).
narrative_ontology:cs_reference_frame('b8e94903-1008-451e-afb8-708d2166f3c6', sacrifice_law_as_binding_obligation).
narrative_ontology:cs_drift_state('b8e94903-1008-451e-afb8-708d2166f3c6', post_temple_destruction, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b8e94903-1008-451e-afb8-708d2166f3c6', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(symbolic_archive_reading, jewish_collective_memory).
narrative_ontology:constraint_beneficiary(symbolic_archive_reading, interpretive_tradition_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(symbolic_archive_reading, engaged_jewish_learners).
narrative_ontology:constraint_beneficiary(symbolic_archive_reading, halakhic_interpretive_tradition).
narrative_ontology:constraint_vindicates(symbolic_archive_reading, cultural_memory_preservation_through_study).
narrative_ontology:constraint_vindicates(symbolic_archive_reading, non_binding_halakhic_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective memory of the Jewish people benefits from continuous preservation and reinterpretation of sacrifice law tradition. This is not an agent that collects rents but a non-agent entity (a doctrine, a cultural good) that is preserved through the constraint. Marked agent=false per OQ-64 guard.
narrative_ontology:constraint_stakeholder(symbolic_archive_reading, jewish_collective_memory, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_non_agent(symbolic_archive_reading, jewish_collective_memory).

% Voluntary participants in sacrifice law study. They gain cultural knowledge, participate in an interpretive community, and contribute to the preservation of tradition. They can exit at any time without penalty. They are beneficiaries because they choose to participate and gain from the learning experience.
narrative_ontology:constraint_stakeholder(symbolic_archive_reading, engaged_jewish_learners, beneficiary,
    moderate, generational, mobile, global).

% The institutional structure of halakhic interpretation benefits from continuous study and reinterpretation of sacrifice law. The tradition maintains textual authority and interpretive continuity through learners' engagement. The tradition has arbitrage options (could emphasize other legal domains, could allow sacrifice law to fade from active study). No coercion is exerted on learners.
narrative_ontology:constraint_stakeholder(symbolic_archive_reading, halakhic_interpretive_tradition, beneficiary,
    institutional, civilizational, arbitrage, global).

% Those who choose not to study sacrifice law. Within the symbolic-archive reading, they face no penalty or social cost for non-participation. They are observers of the constraint rather than participants or victims. They have full exit (they are not in the constraint at all).
narrative_ontology:constraint_stakeholder(symbolic_archive_reading, non_participants, observer,
    powerless, biographical, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preservation and continuous reinterpretation of a complex legal tradition (sacrifice law) that cannot be performed in the absence of the Temple. The constraint solves the collective-action problem of maintaining textual authority and interpretive continuity across generations.
% TRANSFER_FUNCTION: The constraint moves cultural knowledge and interpretive authority from the tradition to learners, and moves learners' engagement and reinterpretation back to the tradition. No material transfer occurs; the transfer is epistemic and cultural.
% ABSENT_VOICES: Those who reject the symbolic-archive reading (adherents of the study-as-exercise, performance-only, or messianic-suspension readings) are not absent from the conversation but are present as competing interpretations. Within the symbolic-archive reading's own framework, there are no absent voices — the reading is inclusive of all who wish to study voluntarily.
% DISAPPEARANCE_RATIONALE: If the symbolic-archive reading disappeared (if the tradition ceased to frame sacrifice law as cultural memory work), the interpretive tradition would lose a major framework for maintaining textual continuity. The tradition would have to adopt one of the sibling readings (study-as-exercise, performance-only, messianic-suspension) or allow sacrifice law to fade from active study. The world would rearrange because the tradition's institutional structure depends on continuous engagement with sacrifice law texts.
% FOUNDING_PROBLEM: How to maintain a complex legal tradition (sacrifice law) when the primary obligation (Temple sacrifice) is not performable in the absence of the Temple. The founding problem is the structural tension between the textual tradition's claim on Jewish law and the practical impossibility of fulfilling that claim.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by all four sibling readings (study-as-exercise, performance-only, messianic-suspension, and symbolic-archive). Each reading offers a different solution to the same structural problem. The problem is live because the Temple has not been rebuilt, and the tradition continues to engage with sacrifice law texts. Corroboration comes from halakhic authorities across all four reading traditions, who acknowledge that sacrifice law requires interpretation in the absence of the Temple.
narrative_ontology:disappearance_verdict(symbolic_archive_reading, world_rearranges).
narrative_ontology:founding_problem_status(symbolic_archive_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENGAGED JEWISH LEARNER (ROPE) — Voluntary participant in sacrifice law study. Experiences the constraint as pure coordination: learning sacrifice law alongside others solves the collective-action problem of preserving textual tradition and interpretive continuity. No extraction — the learner benefits from access to tradition and from the shared epistemic community. Exit is costless (can stop studying); participation is chosen. Low theater: the study is functionally what it claims to be.
constraint_indexing:constraint_classification(symbolic_archive_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERPRETIVE TRADITION / HALAKHIC AUTHORITY (ROPE) — The tradition benefits from continuous study and reinterpretation of sacrifice law. The constraint solves the coordination problem of maintaining textual authority and interpretive continuity across generations. No extraction: the tradition does not coerce participation or extract from learners. The tradition has arbitrage options (could emphasize other legal domains, could allow sacrifice law to fade). Theater is low: the study is genuinely preserving and reinterpreting the text, not performing a false obligation.
constraint_indexing:constraint_classification(symbolic_archive_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: LEARNER WITHOUT OBLIGATION (ROPE) — From the perspective of someone who has internalized the symbolic-archive reading, sacrifice law study is a voluntary cultural practice with no binding force. The constraint coordinates learning and memory preservation without coercion. Exit is available (one can choose not to study). Beneficiary status is clear: the learner gains cultural knowledge and participates in collective memory. No extraction, no suppression.
constraint_indexing:constraint_classification(symbolic_archive_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a civilizational/universal perspective, the symbolic-archive reading instantiates pure coordination: the constraint solves the genuine collective-action problem of preserving a complex legal tradition without binding anyone to impossible obligations (sacrifice is not performable in the absence of the Temple). The reading is structurally coherent and non-extractive. Theater is minimal: the study is what it claims to be — cultural memory work, not performance of false obligation.
constraint_indexing:constraint_classification(symbolic_archive_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(symbolic_archive_reading_tests).
:- end_tests(symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.0): Zero. The symbolic-archive reading explicitly denies binding obligation. Study is voluntary; no one is coerced to participate. No extraction flows from learners to beneficiaries — the tradition benefits from study, but learners are not forced to provide that benefit. Suppression (0.0): Zero. No barriers to exit exist; learners can choose not to study without legal, social, or material penalty (within this reading's framework). Theater ratio (0.15, rising slightly to 0.18): Low and stable. The study is functionally what it claims to be — cultural memory work and interpretive practice. The slight rise reflects increasing formalization of study curricula and institutional frameworks for teaching sacrifice law, but this formalization is transparent (not performative). The theater is not hiding a false obligation; it is making the voluntary practice more structured.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is minimal — all perspectives converge on the Rope classification. This convergence is diagnostic: it indicates that the symbolic-archive reading is structurally coherent and non-extractive. The engaged learner, the tradition, the learner without obligation, and the analytical observer all see the same constraint: voluntary coordination for cultural memory preservation. The absence of perspectival gap is itself the signal that this reading has resolved the kernel's structural tension without creating asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for all perspectives is near 0.0 (full beneficiary) because no extraction occurs. Learners are not targets of extraction; they are voluntary participants who benefit from access to tradition. The tradition benefits from study but does not extract from learners. The beneficiary/victim derivation is straightforward: beneficiaries are the collective memory and the tradition; victims are absent (no one is harmed by the constraint). Exit options are mobile for learners (can stop studying) and arbitrage for the tradition (could emphasize other domains). Power levels vary (powerless learner, institutional tradition, analytical observer) but all produce the same directionality because the structural relationship is non-extractive across all positions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is sacrifice law genuinely a symbolic archive with no binding force, or does the obligation persist in some form (study as exercise, performance as metaphor, messianic suspension)?',
    'Textual analysis of halakhic sources; ethnographic observation of how different Jewish communities frame sacrifice law study; comparison of how this reading is taught vs. how sibling readings are taught in different institutional contexts',
    'If symbolic-archive reading is correct: extractiveness remains 0.0, classification is Rope, no victim set. If sibling readings are correct: extractiveness rises (obligation creates binding force), classification shifts to Tangled Rope or Snare, victim set emerges (those bound by obligation they cannot fulfill).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether sacrifice law is genuinely non-binding or obligation persists in some form').

omega_variable(
    voluntary_participation_boundary,
    'At what point does cultural expectation to study sacrifice law cross from voluntary coordination into subtle coercion?',
    'Ethnographic study of how learners experience the expectation to study; measurement of social cost for non-participation; comparison across communities with different institutional enforcement of study norms',
    'If boundary is crossed: suppression rises above 0.0, exit_options shift from mobile to constrained, classification may shift from Rope to Tangled Rope. If boundary is not crossed: Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_participation_boundary, empirical, 'Whether cultural expectation to study becomes subtle coercion').

omega_variable(
    sibling_reading_coexistence,
    'Can the symbolic-archive reading coexist with the study-as-exercise reading and performance-only reading within a single halakhic framework, or do they foreclose each other?',
    'Analysis of how different halakhic authorities frame the relationship between these readings; examination of whether a single learner or community can hold multiple readings simultaneously without logical contradiction',
    'If readings coexist: network structure is ''coexists_with'' for all siblings. If readings foreclose each other: network structure shifts to ''forecloses'' for some pairs, indicating deeper structural incompatibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether sibling readings logically coexist or foreclose each other').

omega_variable(
    messianic_suspension_pressure,
    'Does the messianic-suspension reading (sacrifice law is suspended until the Temple is rebuilt) create structural pressure on the symbolic-archive reading, or are they independent?',
    'Textual analysis of how messianic theology relates to archive preservation; ethnographic observation of whether communities that emphasize messianic suspension also de-emphasize symbolic-archive study',
    'If messianic reading creates pressure: relationship is ''influences'' rather than ''coexists_with''. If independent: ''coexists_with'' holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_suspension_pressure, conceptual, 'Whether messianic-suspension reading influences symbolic-archive reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(symbolic_archive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(symarch_tr_t0, symbolic_archive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(symarch_tr_t50, symbolic_archive_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(symarch_tr_t100, symbolic_archive_reading, theater_ratio, 100, 0.18).

% Extraction over time
narrative_ontology:measurement(symarch_be_t0, symbolic_archive_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(symarch_be_t50, symbolic_archive_reading, base_extractiveness, 50, 0.0).
narrative_ontology:measurement(symarch_be_t100, symbolic_archive_reading, base_extractiveness, 100, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(symbolic_archive_reading, identity_coordination).
narrative_ontology:affects_constraint(symbolic_archive_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(symbolic_archive_reading, performance_only_reading).
narrative_ontology:affects_constraint(symbolic_archive_reading, messianic_suspension_reading).

% DUAL FORMULATION NOTE:
% The sacrifice obligation kernel decomposes into four structurally distinct constraints, each with a different ε value and victim set. The symbolic-archive reading (this story) has zero extractiveness and no victim set. The study-as-exercise reading has moderate extractiveness (binding obligation). The performance-only reading has zero extractiveness (no obligation without Temple). The messianic-suspension reading has zero extractiveness in the present but creates a future victim set. All four readings are linked via network.affects_constraints to enable contamination analysis and cross-reading comparison.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
