% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Sacrifice Obligation: Messianic Suspension Reading
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This constraint represents the 'messianic suspension' reading of the
 *   sacrifice obligation kernel. It posits that the divine obligation for
 *   sacrifices is not transformed or fulfilled by other means (like prayer or
 *   study as a substitute), but is rather divinely suspended until the
 *   messianic era and the rebuilding of the Temple. During this period, the
 *   study of sacrificial laws is understood as maintaining 'operational
 *   readiness' for the future, not as a fulfillment of the mitzvah itself.
 *   This reading emphasizes divine sovereignty and the literal fulfillment of
 *   commandments.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.05).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.02).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, mountain).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Sacrifice Obligation: Messianic Suspension Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__messianic_suspension_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, 'd9bd2047-5697-4253-bd08-065ac5dd87c7').
narrative_ontology:cs_kernel_codification('d9bd2047-5697-4253-bd08-065ac5dd87c7', fixed_text).
narrative_ontology:cs_authority_grounding('d9bd2047-5697-4253-bd08-065ac5dd87c7', lineage).
narrative_ontology:cs_interpretation_layer_present('d9bd2047-5697-4253-bd08-065ac5dd87c7').
narrative_ontology:cs_reading_relation('d9bd2047-5697-4253-bd08-065ac5dd87c7', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('d9bd2047-5697-4253-bd08-065ac5dd87c7', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9bd2047-5697-4253-bd08-065ac5dd87c7', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('d9bd2047-5697-4253-bd08-065ac5dd87c7', foundational, divine_command_literal_fulfillment).
narrative_ontology:cs_axiom_status(divine_command_literal_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('d9bd2047-5697-4253-bd08-065ac5dd87c7', divine_command_literal_fulfillment, theological).
narrative_ontology:cs_axiom('d9bd2047-5697-4253-bd08-065ac5dd87c7', foundational, messianic_restoration_prerequisite).
narrative_ontology:cs_axiom_status(messianic_restoration_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('d9bd2047-5697-4253-bd08-065ac5dd87c7', messianic_restoration_prerequisite, theological).
narrative_ontology:cs_reference_frame('d9bd2047-5697-4253-bd08-065ac5dd87c7', divinely_suspended_obligation).
narrative_ontology:cs_drift_state('d9bd2047-5697-4253-bd08-065ac5dd87c7', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d9bd2047-5697-4253-bd08-065ac5dd87c7', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_of_israel).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, kohanim_priesthood).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, observant_jews).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, divine_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, messianic_hope_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives the preserved knowledge and operational readiness for the eventual restoration of the Temple service, ensuring continuity of religious practice. Their identity is deeply tied to this future.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_of_israel, beneficiary,
    powerless, generational, identity_locked, global).

% Maintains their ancestral role and specific knowledge of sacrificial rites through study, ensuring they are prepared to resume service upon restoration. Their professional and religious identity is defined by this future role.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, kohanim_priesthood, beneficiary,
    organized, generational, identity_locked, global).

% Interpret and transmit the halakhic understanding of the suspension, emphasizing the instrumental role of study. They shape the communal understanding of religious obligation during this period.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_scholars, agenda_setter,
    institutional, generational, constrained, global).

% Bear the 'cost' of maintaining the knowledge through study and adherence to the messianic expectation, without direct performance of sacrifices. This is a spiritual and intellectual investment rather than a material one.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, observant_jews, payer,
    moderate, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective religious practice and expectation of the Jewish people during a period of Temple destruction, ensuring continuity of tradition and readiness for future restoration.
% TRANSFER_FUNCTION: Transfers the responsibility for physical sacrifice into a state of divine suspension, while transferring the obligation of 'operational readiness' to the community through study and preservation of knowledge.
% ABSENT_VOICES: Those who might advocate for immediate, symbolic, or transformed sacrificial practices (e.g., through prayer or ethical deeds as substitutes) are implicitly excluded by the strict 'suspension, not transformation' doctrine. Their voices are present in other readings of the kernel.
% DISAPPEARANCE_RATIONALE: If the understanding of divine suspension vanished, the entire framework of Jewish religious law and messianic expectation regarding sacrifices would collapse. The community's current practices, identity, and future hopes are deeply intertwined with this understanding.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered physical performance of sacrifices impossible, creating a crisis of religious obligation and continuity.
% FOUNDING_PROBLEM_CORROBORATION: The problem of Temple destruction and the inability to perform sacrifices remains a live issue, attested by historical fact and ongoing communal prayer for restoration. This is universally acknowledged across all readings of the kernel, not just by beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_obligation_kernel__messianic_suspension_reading),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low because the obligation is suspended, not actively extracting from participants. The 'cost' of study is a voluntary spiritual investment, not a coercive extraction. Suppression is minimal as adherence is primarily driven by internal religious commitment rather than external coercion. Theater ratio is low because the study is genuinely aimed at preserving knowledge for a future, real performance, not merely for show. Accessibility collapse is high because, within this reading, there are no legitimate alternatives to the suspended obligation; one simply waits and prepares. Resistance is negligible as this reading is a widely accepted, if not universally held, interpretation within Orthodox Judaism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of observant Jews, this constraint is a divine decree, a 'mountain' of religious law. From an external, secular analytical perspective, it functions as a 'rope' of identity coordination, maintaining communal cohesion and historical continuity through shared expectation and study. The engine's classification will likely reflect the latter, while the claimed type reflects the internal theological understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and the Kohanim priesthood are beneficiaries as this reading ensures the preservation of their heritage and future roles. Halakhic scholars act as agenda-setters by interpreting and transmitting this understanding. Observant Jews are 'payers' in the sense that they bear the spiritual and intellectual 'cost' of maintaining this readiness, but this is a chosen obligation, not an extraction. All are identity-locked by their religious commitment.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the 'suspension' as either a 'snare' (if extraction were high due to coercive study) or a 'piton' (if the study became purely theatrical without genuine messianic expectation). By emphasizing the low extractiveness and genuine purpose of maintaining operational readiness, it correctly identifies the constraint as a form of coordination (a Mountain from the internal perspective, a Rope from an external one) rather than a degraded or extractive mechanism. The 'live' status of the founding problem further supports this, as the problem of Temple destruction persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_theology,
    'Is the divine suspension of sacrifice obligation a genuine natural law (a Mountain) or a constructed theological interpretation (a Rope/Tangled Rope) that benefits identifiable agents?',
    'Analysis of theological arguments for divine command theory vs. sociological studies of religious authority and community formation. If the ''divine'' aspect is primarily a legitimizing narrative for a communal coordination strategy, reclassify.',
    'If constructed, the constraint''s classification would shift from Mountain to Rope or Tangled Rope, reflecting its function in coordinating communal identity and practice, with potential for subtle extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_theology, conceptual, 'Ambiguity between divine decree and communal theological construction.').

omega_variable(
    study_as_substitute_ambiguity,
    'Does the study of sacrificial laws, in practice, function as a psychological or spiritual substitute for the actual performance of sacrifices, despite the theological claim of ''suspension, not transformation''?',
    'Qualitative sociological and psychological studies of observant Jewish communities regarding their experience of study and its role in fulfilling religious longing. If study consistently provides a sense of fulfillment equivalent to performance, the ''suspension'' claim is functionally undermined.',
    'If study functions as a de facto substitute, the ''messianic suspension'' reading would be functionally closer to the ''study as exercise'' reading, potentially increasing its effective extractiveness (as it demands a ''performance'' of study) and altering its classification towards a Tangled Rope or even Snare if the ''suspension'' claim is used to suppress alternative forms of religious expression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_as_substitute_ambiguity, empirical, 'Functional role of study vs. theological claim of suspension.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 500, 0.1).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 2000, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 500, 0.02).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1000, 0.02).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1500, 0.02).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 2000, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
