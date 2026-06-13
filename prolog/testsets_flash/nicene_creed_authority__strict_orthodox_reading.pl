% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed Authority (Strict Orthodox Reading)
 *   domain: systematic_theology/ecclesiology/history_of_christian_doctrine
 *
 * SUMMARY:
 *   This constraint represents the 'strict orthodox reading' of the Nicene
 *   Creed, where it functions as a binding metaphysical ontology for all
 *   believers, and deviation is considered heresy warranting sanction. This
 *   reading emphasizes doctrinal uniformity and the authority of a
 *   centralized clergy to define and enforce correct belief. It is one of
 *   several possible interpretations of the creed's function within Christian
 *   traditions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.7).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.8).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, snare).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed Authority (Strict Orthodox Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "systematic_theology/ecclesiology/history_of_christian_doctrine").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '4cb10c5b-a7ac-42ee-915c-21d2c9c7200a').
narrative_ontology:cs_kernel_codification('4cb10c5b-a7ac-42ee-915c-21d2c9c7200a', fixed_text).
narrative_ontology:cs_authority_grounding('4cb10c5b-a7ac-42ee-915c-21d2c9c7200a', lineage).
narrative_ontology:cs_interpretation_layer_present('4cb10c5b-a7ac-42ee-915c-21d2c9c7200a').
narrative_ontology:cs_reading_relation('4cb10c5b-a7ac-42ee-915c-21d2c9c7200a', nicene_creed_authority__symbolic_confessional_reading, forecloses).
narrative_ontology:cs_reading_relation('4cb10c5b-a7ac-42ee-915c-21d2c9c7200a', nicene_creed_authority__liturgical_habituation_reading, influences).
narrative_ontology:cs_axiom('4cb10c5b-a7ac-42ee-915c-21d2c9c7200a', foundational, creed_as_literal_metaphysical_truth).
narrative_ontology:cs_axiom_status(creed_as_literal_metaphysical_truth, holdable).
narrative_ontology:cs_axiom_grounding('4cb10c5b-a7ac-42ee-915c-21d2c9c7200a', creed_as_literal_metaphysical_truth, deontological).
narrative_ontology:cs_axiom('4cb10c5b-a7ac-42ee-915c-21d2c9c7200a', secondary, heresy_warrants_sanction).
narrative_ontology:cs_axiom_status(heresy_warrants_sanction, holdable).
narrative_ontology:cs_axiom_grounding('4cb10c5b-a7ac-42ee-915c-21d2c9c7200a', heresy_warrants_sanction, conventional).
narrative_ontology:cs_reference_frame('4cb10c5b-a7ac-42ee-915c-21d2c9c7200a', patristic_doctrinal_orthodoxy).
narrative_ontology:cs_drift_state('4cb10c5b-a7ac-42ee-915c-21d2c9c7200a', contemporary_pluralistic_theology, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4cb10c5b-a7ac-42ee-915c-21d2c9c7200a', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, orthodox_institutions).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, theological_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, liturgical_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces doctrinal conformity, defines heresy, and administers sanctions. Benefits from the stability and authority derived from a unified metaphysical understanding of the creed. Their power is directly tied to the creed's strict interpretation.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the clear boundaries and unified identity provided by the strict creedal interpretation. This allows for consistent theological education, liturgical practice, and evangelism, attracting those seeking doctrinal certainty.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, orthodox_institutions, beneficiary,
    organized, generational, constrained, global).

% Face excommunication, social ostracism, and theological condemnation for deviating from the prescribed metaphysical ontology. Their interpretations are suppressed, and their members may be pressured to conform or leave their faith tradition.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    powerless, biographical, identity_locked, local).

% Are expected to adhere to the official metaphysical interpretation, limiting their personal theological exploration. While not always directly sanctioned, social pressure and lack of institutional support constrain their interpretive freedom.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, payer,
    moderate, biographical, constrained, local).

% Those who propose new theological understandings or challenge the established metaphysical ontology face accusations of heresy, career limitations within orthodox institutions, and exclusion from mainstream discourse. Their work is often suppressed or ignored.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, theological_innovators, payer,
    moderate, biographical, constrained, global).

% Experience the creed as a unifying element in worship, providing a sense of continuity and shared identity. For many, the metaphysical implications are secondary to the communal experience of reciting the creed.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, liturgical_participants, beneficiary,
    moderate, immediate, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a singular, authoritative metaphysical framework for Christian belief, ensuring doctrinal uniformity across diverse communities and preventing theological fragmentation.
% TRANSFER_FUNCTION: Transfers interpretive authority and doctrinal control from individual believers and local communities to a centralized, hierarchical clergy, in exchange for perceived theological stability and unity.
% ABSENT_VOICES: Early Christian communities with diverse theological expressions, contemporary pluralistic theologians, and those who prioritize personal spiritual experience over dogmatic adherence are excluded. They would argue for a more expansive or less rigid understanding of creedal authority.
% DISAPPEARANCE_RATIONALE: If the strict orthodox interpretation of the Nicene Creed vanished, the hierarchical structures built upon its enforcement would lose significant legitimacy. Theological education, church governance, and inter-denominational relations would undergo profound reorganization as diverse metaphysical interpretations gained equal footing, leading to a more fragmented but potentially more pluralistic theological landscape.
% FOUNDING_PROBLEM: The early Christian church faced widespread theological disputes regarding the nature of Christ and the Trinity, threatening its unity and coherence. The Nicene Creed was formulated to resolve these controversies and establish a common doctrinal foundation.
% FOUNDING_PROBLEM_CORROBORATION: Hierarchical clergy and orthodox institutions attest that the problem of theological fragmentation and heterodoxy remains live, requiring continued strict adherence to the creed. While some external observers (e.g., secular historians of religion) might view the original problem as resolved, they would corroborate that the *perceived* need for doctrinal unity persists within these communities, driving the constraint's maintenance.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because adherence to a specific metaphysical ontology is demanded, limiting intellectual and spiritual freedom, with significant costs for deviation. Suppression is also high (0.8) due to active heresy policing, excommunication, and social pressure. The theater ratio is low (0.1) as the enforcement of this reading is largely genuine and functional, not merely performative. Accessibility collapse is high (0.75) because for those within the tradition, alternatives to the prescribed ontology are severely constrained or deemed illegitimate. Resistance is moderate (0.6) as there is ongoing, though often suppressed, theological dissent and innovation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the hierarchical clergy, this constraint is a necessary Rope or even a Mountain, ensuring the integrity and unity of the faith. From the perspective of heterodox communities or theological innovators, it is a clear Snare, actively extracting conformity and suppressing alternative interpretations. The engine's computation of per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Hierarchical clergy and orthodox institutions are clear beneficiaries (d near 0.0) as they gain authority, stability, and control. Heterodox communities, lay interpreters, and theological innovators are targets (d near 1.0) as they bear the costs of conformity or sanction. Liturgical participants are beneficiaries (d near 0.2) as they gain communal identity, but their metaphysical assent is less directly extracted than for clergy or theologians.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the strict orthodox reading as a 'Rope' (pure coordination) by highlighting the active enforcement, identifiable victims, and high extractiveness. While it provides a form of coordination (doctrinal unity), the asymmetric costs and suppression mechanisms firmly place it in the Snare category, indicating that its persistence relies on coercion rather than universal benefit. The 'live' status of the founding problem (theological fragmentation) is used as a justification for continued enforcement, but the high extractiveness suggests the solution has become a mechanism for power consolidation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_vs_symbolic_interpretation,
    'Is the Nicene Creed''s primary function to establish a literal metaphysical ontology, or to serve as a symbolic confession of faith and identity?',
    'Historical-critical analysis of early creedal usage, theological hermeneutics, and empirical study of how diverse Christian communities actually interpret and use the creed.',
    'If primarily symbolic, the measured extractiveness and suppression would be re-evaluated as disproportionate to the coordination function, potentially reclassifying this reading as a more severe Snare or even a Piton if the metaphysical enforcement becomes purely theatrical. If primarily metaphysical, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metaphysical_vs_symbolic_interpretation, conceptual, 'Ambiguity in the creed''s interpretive function.').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent is the suppression of heterodox views structural (excommunication, institutional barriers) versus internalized (self-censorship, fear of social ostracism, identity fusion with orthodoxy)?',
    'Sociological studies of ex-members of orthodox communities, psychological analysis of identity formation within strict religious groups, and observation of theological discourse in contexts with reduced institutional enforcement.',
    'If internalized suppression is a significant component, the effective suppression for individuals is higher than the structural measure suggests, as the constraint persists even after direct external enforcement is removed. This would amplify the perceived extractiveness for affected individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for theological deviation.').

omega_variable(
    founding_problem_obsolescence,
    'Has the original problem of theological fragmentation (which the Nicene Creed was designed to solve) evolved or diminished to the point where the strict orthodox reading''s enforcement is no longer proportionate or necessary?',
    'Comparative historical analysis of theological diversity in early vs. contemporary Christianity, and sociological studies of the actual impact of doctrinal variations on community cohesion.',
    'If the founding problem is largely obsolete, the constraint''s persistence with high extractiveness would strongly indicate mandatrophy, potentially reclassifying it as a Piton or a Snare with a purely extractive function, as the coordination justification would have atrophied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem still justifies the constraint''s severity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 325, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 325, 0.05).
narrative_ontology:measurement(nice_tr_t800, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 800, 0.08).
narrative_ontology:measurement(nice_tr_t1500, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1500, 0.12).
narrative_ontology:measurement(nice_tr_t1800, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(nice_tr_t2024, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 325, 0.6).
narrative_ontology:measurement(nice_be_t800, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 800, 0.7).
narrative_ontology:measurement(nice_be_t1500, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1500, 0.75).
narrative_ontology:measurement(nice_be_t1800, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1800, 0.72).
narrative_ontology:measurement(nice_be_t2024, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 325, 0.7).
narrative_ontology:measurement(nice_su_t800, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 800, 0.85).
narrative_ontology:measurement(nice_su_t1500, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1500, 0.9).
narrative_ontology:measurement(nice_su_t1800, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1800, 0.85).
narrative_ontology:measurement(nice_su_t2024, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__liturgical_habituation_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Nicene Creed's authority. Each reading has a different structural function and impact, necessitating separate constraint stories linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
