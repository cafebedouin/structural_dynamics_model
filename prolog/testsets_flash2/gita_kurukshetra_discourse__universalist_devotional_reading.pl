% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__universalist_devotional_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Bhagavad Gita: Universalist Devotional Reading
 *   domain: religious_studies/ethics/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the 'universalist devotional' reading of the
 *   Bhagavad Gita's Kurukshetra discourse. It interprets the text as teaching
 *   a path of devotion (bhakti) accessible to all, regardless of caste, and
 *   redefines dharma as surrender to divine will rather than adherence to
 *   social role. This reading dissolves caste as a spiritual barrier,
 *   promotes egalitarian access to salvation, and implicitly undermines
 *   traditional Brahminical gatekeeping authority. It is presented as a
 *   'rope' because it genuinely coordinates a broad community around a shared
 *   spiritual path, with relatively low extraction from its beneficiaries
 *   (universal devotees) and moderate suppression of alternative, more
 *   hierarchical readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.15).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.25).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Bhagavad Gita: Universalist Devotional Reading").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_studies/ethics/hermeneutics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, '253c8711-f73f-4380-9b16-df96e25584ff').
narrative_ontology:cs_kernel_codification('253c8711-f73f-4380-9b16-df96e25584ff', fixed_text).
narrative_ontology:cs_authority_grounding('253c8711-f73f-4380-9b16-df96e25584ff', practice).
narrative_ontology:cs_interpretation_layer_present('253c8711-f73f-4380-9b16-df96e25584ff').
narrative_ontology:cs_reading_relation('253c8711-f73f-4380-9b16-df96e25584ff', gita_kurukshetra_discourse__orthodox_literal_reading, influences).
narrative_ontology:cs_reading_relation('253c8711-f73f-4380-9b16-df96e25584ff', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('253c8711-f73f-4380-9b16-df96e25584ff', foundational, bhakti_yoga_universal_path).
narrative_ontology:cs_axiom_status(bhakti_yoga_universal_path, holdable).
narrative_ontology:cs_axiom_grounding('253c8711-f73f-4380-9b16-df96e25584ff', bhakti_yoga_universal_path, deontological).
narrative_ontology:cs_axiom('253c8711-f73f-4380-9b16-df96e25584ff', foundational, dharma_is_divine_surrender).
narrative_ontology:cs_axiom_status(dharma_is_divine_surrender, holdable).
narrative_ontology:cs_axiom_grounding('253c8711-f73f-4380-9b16-df96e25584ff', dharma_is_divine_surrender, theological).
narrative_ontology:cs_reference_frame('253c8711-f73f-4380-9b16-df96e25584ff', egalitarian_devotional_path).
narrative_ontology:cs_drift_state('253c8711-f73f-4380-9b16-df96e25584ff', contemporary_hindu_reform_movements, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('253c8711-f73f-4380-9b16-df96e25584ff', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, traditional_brahminical_priesthood).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains direct access to spiritual liberation through devotion, bypassing traditional caste-based hierarchies and ritualistic gatekeeping. This reading empowers individuals regardless of their social standing.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class, beneficiary,
    powerless, biographical, mobile, global).

% Loses its exclusive authority as spiritual intermediaries and gatekeepers of dharma, as devotion becomes universally accessible. This challenges their traditional social and religious power structure.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, traditional_brahminical_priesthood, payer,
    institutional, generational, constrained, regional).

% Finds its literal and caste-affirming interpretations of the Gita challenged by this reading, which redefines dharma and spiritual access. Their academic and religious authority is undermined.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_scholars, payer,
    organized, generational, constrained, national).

% Observes this reading as a partial ally in challenging caste hierarchy, but may diverge on the interpretation of violence, which this reading does not allegorize but rather de-emphasizes as central.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, gandhian_interpreters, observer,
    powerful, generational, analytical, global).

% Actively promote this reading to foster social equality and inclusivity within Hinduism, using it as a textual basis for challenging caste discrimination and promoting individual spiritual agency.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, modern_hindu_reformers, agenda_setter,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a diverse spiritual community around a shared path of devotion (bhakti) that transcends social divisions, fostering unity and egalitarian access to divine grace.
% TRANSFER_FUNCTION: Transfers spiritual authority and access from traditional caste-based intermediaries to individual devotees, reallocating the means of salvation.
% ABSENT_VOICES: Those historically excluded from spiritual practice due to caste or gender, whose voices would affirm the liberating power of this reading if they were present in traditional interpretive circles.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the spiritual landscape would revert to more hierarchical, caste-bound interpretations, diminishing individual agency and reinforcing traditional gatekeeping. The social and religious reform movements built on this interpretation would lose a foundational text.
% FOUNDING_PROBLEM: The problem of spiritual exclusion and social hierarchy within traditional Hindu society, where access to dharma and moksha was often restricted by birth and ritual status.
% FOUNDING_PROBLEM_CORROBORATION: Modern Hindu reformers and social justice advocates attest that the problem of caste discrimination and spiritual exclusion remains live, despite legal reforms. Sociological studies of contemporary India corroborate the persistence of caste-based disparities, supporting the continued relevance of this reading's challenge to traditional structures.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).
:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because this reading primarily offers liberation and access, rather than imposing significant costs on its adherents. Suppression is moderate (0.25) as it actively challenges and seeks to displace more orthodox, hierarchical interpretations, but does not coercively eliminate them. Theater ratio is low (0.1) because its core function of spiritual liberation and social reform is genuine and direct. Accessibility collapse is high (0.7) because once understood, the path of universal devotion makes many traditional, complex rituals seem less necessary. Resistance is moderate (0.3) from traditionalists whose authority is challenged.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the universal devotee class, this reading is a liberating rope. From the perspective of the traditional Brahminical priesthood, it is a threat to their established order, potentially experienced as a snare or tangled rope that extracts their authority and social capital. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'universal devotee class' is the primary beneficiary, gaining direct spiritual access (low d). Traditional Brahminical priesthood and orthodox scholars are payers, as their authority and interpretive monopoly are challenged (high d). Modern Hindu reformers act as agenda-setters, actively promoting and disseminating this reading. Gandhian interpreters are observers, sharing some common ground (anti-caste) but potentially differing on other aspects (e.g., allegorical violence).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_legitimacy,
    'By what authority does this reading claim to supersede or reinterpret traditional caste-affirming readings?',
    'Analysis of the interpretive tradition''s internal coherence, its historical reception, and its appeal to alternative grounding principles (e.g., direct spiritual experience vs. scriptural literalism).',
    'If its interpretive authority is weak, this reading might be reclassified as a ''snare'' for those who adopt it, as it could lead them away from established paths without sufficient grounding. If strong, it reinforces its ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, conceptual, 'The basis of legitimacy for reinterpreting a foundational text.').

omega_variable(
    social_impact_vs_textual_claim,
    'To what extent has this reading actually dissolved caste as a spiritual barrier in practice, versus merely offering a theoretical alternative?',
    'Empirical sociological studies measuring the actual participation of historically marginalized groups in devotional practices, and the decline of caste-based discrimination in spiritual contexts influenced by this reading.',
    'If the practical impact is minimal despite the textual claim, the ''accessibility_collapse'' and ''resistance'' metrics might need adjustment, potentially shifting the classification towards a ''piton'' (if the claim is mostly performative) or a ''tangled_rope'' (if it coordinates some but still extracts from others through subtle means).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_impact_vs_textual_claim, empirical, 'Gap between textual promise of universal access and real-world social change.').

omega_variable(
    violence_interpretation_divergence,
    'How does this reading''s de-emphasis of violence as central compare to the explicit allegorization of violence in the Gandhian reading, and the literal mandate in the orthodox reading?',
    'Detailed textual analysis of specific verses related to conflict and duty, comparing the hermeneutical strategies employed by each reading to handle the Kurukshetra war narrative.',
    'If this reading''s approach to violence is found to be a subtle form of allegorization or evasion, it might align more closely with the ''Gandhian allegorical'' reading, affecting network relations. If it genuinely offers a distinct non-violent interpretation, its unique contribution is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(violence_interpretation_divergence, conceptual, 'Comparison of violence interpretations across Gita readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gita_tr_t10, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(gita_tr_t30, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(gita_tr_t50, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(gita_be_t10, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(gita_be_t30, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(gita_be_t50, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gita_su_t10, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 20, 0.23).
narrative_ontology:measurement(gita_su_t30, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 30, 0.24).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(gita_su_t50, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 50, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Bhagavad Gita's Kurukshetra discourse kernel. Each reading presents a different structural constraint, with varying beneficiaries, victims, and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
