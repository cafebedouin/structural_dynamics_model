% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Kodashim Commandment Status: Performance Only Reading
 *   domain: religious_studies/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'performance_only' reading of the Kodashim
 *   (sacrificial laws) kernel, which holds that these commandments are
 *   strictly contingent on the existence of the Temple and its altar. Without
 *   these, the commandment is suspended, and its study, while traditional, is
 *   functionally a 'husk' — a performance without a referent. This reading
 *   emphasizes the practical inapplicability of the laws in the present day,
 *   leading to a high theater ratio and extraction from continued scholarly
 *   investment. The claimed type is 'piton' because the primary function has
 *   atrophied, but the constraint persists due to institutional inertia and
 *   theatrical maintenance, with diffuse costs and no concentrated
 *   beneficiary actively maintaining it for profit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.65).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.7).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.65).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, piton).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Kodashim Commandment Status: Performance Only Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious_studies/halakhic_theory/commitment_system_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, '6a1a4107-a77c-4762-8353-68fe8ee51ad6').
narrative_ontology:cs_kernel_codification('6a1a4107-a77c-4762-8353-68fe8ee51ad6', fixed_text).
narrative_ontology:cs_authority_grounding('6a1a4107-a77c-4762-8353-68fe8ee51ad6', lineage).
narrative_ontology:cs_interpretation_layer_present('6a1a4107-a77c-4762-8353-68fe8ee51ad6').
narrative_ontology:cs_reading_relation('6a1a4107-a77c-4762-8353-68fe8ee51ad6', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_reading_relation('6a1a4107-a77c-4762-8353-68fe8ee51ad6', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('6a1a4107-a77c-4762-8353-68fe8ee51ad6', foundational, commandment_contingent_on_temple).
narrative_ontology:cs_axiom_status(commandment_contingent_on_temple, holdable).
narrative_ontology:cs_axiom_grounding('6a1a4107-a77c-4762-8353-68fe8ee51ad6', commandment_contingent_on_temple, conventional).
narrative_ontology:cs_axiom('6a1a4107-a77c-4762-8353-68fe8ee51ad6', foundational, study_is_not_performance).
narrative_ontology:cs_axiom_status(study_is_not_performance, holdable).
narrative_ontology:cs_axiom_grounding('6a1a4107-a77c-4762-8353-68fe8ee51ad6', study_is_not_performance, deontological).
narrative_ontology:cs_reference_frame('6a1a4107-a77c-4762-8353-68fe8ee51ad6', post_temple_destruction_suspension).
narrative_ontology:cs_drift_state('6a1a4107-a77c-4762-8353-68fe8ee51ad6', contemporary_yeshiva_curriculum, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6a1a4107-a77c-4762-8353-68fe8ee51ad6', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, yeshiva_students).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, community_resources).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, halakhic_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invest significant intellectual and temporal resources into studying the Kodashim (sacrificial) laws, despite their practical inapplicability without a standing Temple. Their identity as scholars is often tied to this traditional curriculum.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_students, payer,
    moderate, biographical, identity_locked, global).

% Administer the curriculum and maintain the interpretive tradition that prioritizes the study of Kodashim, even while acknowledging its current suspension. They could redirect scholarly effort but maintain the inertial focus.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, rabbinic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Financial and intellectual capital within the community that is allocated to supporting institutions focused on the study of currently inoperative laws, rather than to areas of active halakhic or social need.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, community_resources, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__performance_only, community_resources).

% The potential for new legal and ethical developments in areas of contemporary relevance, which are foregone or under-resourced due to the continued emphasis on obsolete sacrificial laws.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, halakhic_innovation, excluded,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__performance_only, halakhic_innovation).

% Scholars of religious studies or sociology who analyze the persistence of ritual and legal study in the absence of its practical referent, observing the resource allocation and identity formation processes.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a continuous scholarly tradition and a sense of historical continuity with ancient Temple practices, providing a shared intellectual framework for religious scholars.
% TRANSFER_FUNCTION: Transfers significant intellectual effort, time, and institutional funding from contemporary halakhic or social issues to the study of inoperative sacrificial laws.
% ABSENT_VOICES: Advocates for a more pragmatic or socially engaged halakhic curriculum, or those who question the utility of extensive study of non-performable commandments, are often marginalized within traditional yeshiva settings.
% DISAPPEARANCE_RATIONALE: If the emphasis on studying Kodashim laws vanished, the curriculum of many yeshivas would undergo a radical shift, intellectual resources would be redirected to other areas of Jewish law or thought, and the identity of many scholars would need re-evaluation. The institutional structure supporting this study would also need to adapt or dissolve.
% FOUNDING_PROBLEM: To preserve the knowledge and understanding of the sacrificial system for a future time when the Temple might be rebuilt, ensuring the continuity of Jewish law and tradition.
% FOUNDING_PROBLEM_CORROBORATION: Traditional rabbinic authorities attest the problem is live, emphasizing the messianic hope for Temple restoration. Critical scholars and some community members, from outside the benefiting parties, argue the problem is largely 'dead' in practical terms, and the continued emphasis serves more as an identity marker or institutional inertia than genuine preparation.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because significant intellectual and communal resources are diverted to a domain of study with no current practical application. Suppression (0.70) is moderate-high, as the weight of tradition and identity-lock mechanisms discourage questioning this curriculum. The theater ratio (0.85) is very high, reflecting that the activity (study) is largely performative, maintaining a connection to a past practice rather than preparing for an imminent future. Accessibility collapse (0.80) is high because the traditional curriculum leaves little room for alternatives, and resistance (0.10) is low due to the strong identity-lock and cultural reverence for tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of those deeply embedded in the tradition, the study is inherently valuable and necessary for continuity. From an external or critical perspective, the same activity appears as a significant misallocation of resources and intellectual effort, sustained by inertia rather than active function. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Yeshiva students and community resources are the primary targets (payers), bearing the costs of this inertial practice. Rabbinic authorities act as agenda-setters, maintaining the curriculum. Halakhic innovation is an excluded victim, as resources are diverted from its potential. No single beneficiary actively profits from this specific constraint; its persistence is due to inertia and diffuse cultural value, characteristic of a piton.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a clear case of mandatrophy. The original mandate (preserving knowledge for Temple restoration) is 'contested' in its status, and the constraint persists largely due to institutional inertia and the identity-locked nature of its participants. The high theater ratio and diffuse costs, without a concentrated beneficiary, prevent it from being mislabeled as a snare; it's a piton because no one benefits enough to actively maintain it, and no one is hurt enough to fix it, leading to its inertial persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_obsolescence_ambiguity,
    'Is the founding problem (preserving knowledge for Temple restoration) genuinely ''live'' or ''dead'' in contemporary practice?',
    'Empirical study of actual messianic expectations and the practical application of Kodashim knowledge in modern halakhic discourse, beyond purely academic study.',
    'If ''dead'', the constraint''s piton classification is strongly reinforced, indicating pure inertia. If ''live'', it suggests a more complex ''tangled rope'' where coordination for future restoration is intertwined with current resource extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence_ambiguity, empirical, 'Ambiguity regarding the true status of the founding mandate.').

omega_variable(
    identity_lock_vs_free_choice,
    'To what extent is the continued study of Kodashim a free choice of scholars, versus an identity-locked path dictated by institutional and cultural norms?',
    'Sociological studies of yeshiva curricula and career paths, examining the social and professional costs of pursuing alternative halakhic studies.',
    'If primarily identity-locked, the suppression metric is more accurate, and the piton classification holds. If genuinely free choice, the extractiveness is lower, as scholars are choosing to ''pay'' for a valued tradition, potentially reclassifying towards a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_free_choice, empirical, 'Structural vs. internalized suppression mechanism in scholarly choice.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''kodashim_commandment_status'' kernel, what specific structural elements would change if a different reading (e.g., ''messianic_deferral'' or ''study_as_performance'') were adopted?',
    'Comparative analysis of curricula, resource allocation, and scholarly output in institutions adhering to different readings.',
    'If ''messianic_deferral'' were dominant, the theater ratio might decrease (study seen as active preparation), and extractiveness might be re-framed as investment. If ''study_as_performance'' were dominant, extractiveness might be lower (study is the ''performance''), but the suppression of alternative interpretations might increase.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Impact of alternative kernel readings on constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t1900, kodashim_commandment_status__performance_only, theater_ratio, 1900, 0.7).
narrative_ontology:measurement(koda_tr_t1930, kodashim_commandment_status__performance_only, theater_ratio, 1930, 0.75).
narrative_ontology:measurement(koda_tr_t1960, kodashim_commandment_status__performance_only, theater_ratio, 1960, 0.8).
narrative_ontology:measurement(koda_tr_t1990, kodashim_commandment_status__performance_only, theater_ratio, 1990, 0.83).
narrative_ontology:measurement(koda_tr_t2024, kodashim_commandment_status__performance_only, theater_ratio, 2024, 0.85).

% Extraction over time
narrative_ontology:measurement(koda_be_t1900, kodashim_commandment_status__performance_only, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement(koda_be_t1930, kodashim_commandment_status__performance_only, base_extractiveness, 1930, 0.58).
narrative_ontology:measurement(koda_be_t1960, kodashim_commandment_status__performance_only, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(koda_be_t1990, kodashim_commandment_status__performance_only, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement(koda_be_t2024, kodashim_commandment_status__performance_only, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t1900, kodashim_commandment_status__performance_only, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(koda_su_t1930, kodashim_commandment_status__performance_only, suppression_requirement, 1930, 0.63).
narrative_ontology:measurement(koda_su_t1960, kodashim_commandment_status__performance_only, suppression_requirement, 1960, 0.66).
narrative_ontology:measurement(koda_su_t1990, kodashim_commandment_status__performance_only, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(koda_su_t2024, kodashim_commandment_status__performance_only, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
