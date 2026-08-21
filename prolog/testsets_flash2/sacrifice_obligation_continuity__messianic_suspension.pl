% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__messianic_suspension, []).

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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Sacrifice Obligation Continuity (Messianic Suspension Reading)
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint represents the 'messianic suspension' reading of the
 *   sacrifice obligation continuity kernel. It posits that the obligation to
 *   perform sacrifices is neither abrogated nor currently active, but
 *   suspended until the messianic era and the rebuilding of the Temple. The
 *   primary function of religious study in this reading is to maintain
 *   readiness and knowledge for that future restoration. This reading avoids
 *   the guilt of non-performance while maintaining the normative force of the
 *   commandment. The metrics reflect a moderate burden of readiness and
 *   study, with low theatricality as the commitment is genuine.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.45).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.6).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.45).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Sacrifice Obligation Continuity (Messianic Suspension Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, '0ac1e245-cf55-458e-9e64-6786d9510d29').
narrative_ontology:cs_kernel_codification('0ac1e245-cf55-458e-9e64-6786d9510d29', fixed_text).
narrative_ontology:cs_authority_grounding('0ac1e245-cf55-458e-9e64-6786d9510d29', lineage).
narrative_ontology:cs_interpretation_layer_present('0ac1e245-cf55-458e-9e64-6786d9510d29').
narrative_ontology:cs_reading_relation('0ac1e245-cf55-458e-9e64-6786d9510d29', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('0ac1e245-cf55-458e-9e64-6786d9510d29', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('0ac1e245-cf55-458e-9e64-6786d9510d29', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('0ac1e245-cf55-458e-9e64-6786d9510d29', foundational, obligation_is_temporally_suspended).
narrative_ontology:cs_axiom_status(obligation_is_temporally_suspended, holdable).
narrative_ontology:cs_axiom_grounding('0ac1e245-cf55-458e-9e64-6786d9510d29', obligation_is_temporally_suspended, deontological).
narrative_ontology:cs_axiom('0ac1e245-cf55-458e-9e64-6786d9510d29', secondary, study_maintains_future_readiness).
narrative_ontology:cs_axiom_status(study_maintains_future_readiness, holdable).
narrative_ontology:cs_axiom_grounding('0ac1e245-cf55-458e-9e64-6786d9510d29', study_maintains_future_readiness, conventional).
narrative_ontology:cs_reference_frame('0ac1e245-cf55-458e-9e64-6786d9510d29', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('0ac1e245-cf55-458e-9e64-6786d9510d29', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0ac1e245-cf55-458e-9e64-6786d9510d29', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, religious_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, religious_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, individual_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the textual tradition, maintaining the framework for future ritual practice. Their careers and social standing are tied to the continuity of the tradition and the belief in its eventual restoration. They bear the primary burden of study and teaching.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, religious_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Receives spiritual guidance and a sense of continuity from the maintained tradition. They are spared the direct burden of performing sacrifices while retaining the future possibility. Their identity is deeply intertwined with the messianic hope and the continuity of the religious law.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, religious_community, beneficiary,
    organized, generational, identity_locked, global).

% Bear the diffuse cost of maintaining readiness: dedicating time to study, supporting religious institutions, and living with the deferred expectation of messianic restoration. Their commitment is sustained by faith, making exit difficult without abandoning core identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, individual_adherents, payer,
    moderate, biographical, identity_locked, local).

% The theoretical future agents who would perform the sacrifices upon restoration. They are 'excluded' from current practice but are the ultimate beneficiaries of the 'readiness' maintained by this constraint. Their role is a conceptual anchor for the present obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, messianic_era_priests, excluded,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious community's collective expectation and readiness for a future messianic era, ensuring the knowledge and framework for ritual sacrifice are preserved and understood, preventing fragmentation or abandonment of the tradition.
% TRANSFER_FUNCTION: Transfers the burden of active ritual performance from the current generation to a future messianic era, while transferring the obligation of study and readiness to the current religious scholars and community.
% ABSENT_VOICES: Those who believe the obligation is entirely abrogated or fulfilled by other means (e.g., prayer, ethical conduct) are not part of this interpretive framework. They would argue that the 'suspension' is an unnecessary burden or a misinterpretation of divine will.
% DISAPPEARANCE_RATIONALE: If this understanding of suspended obligation vanished, the religious community would face a crisis of continuity. Either the obligation would be seen as abrogated (leading to a loss of tradition) or immediately binding (leading to impossible demands), fundamentally altering religious practice and identity.
% FOUNDING_PROBLEM: The destruction of the Temple and the cessation of sacrificial rituals created a crisis of religious practice and continuity: how to maintain divine commandment without the means to fulfill it.
% FOUNDING_PROBLEM_CORROBORATION: Religious texts and centuries of rabbinic commentary attest to the problem. The ongoing absence of the Temple and the messianic era confirms the problem's live status for the religious community. This is corroborated by the historical record of Jewish legal development.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__messianic_suspension, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__messianic_suspension_tests).
:- end_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) due to the ongoing burden of study and the deferred expectation, which requires significant communal and individual investment without immediate ritual fulfillment. Suppression is moderate (0.6) as the community's identity is deeply tied to this interpretation, making alternative readings difficult to adopt without significant social and spiritual cost (identity_locked exit). Theater ratio is low (0.1) because the commitment to future restoration and present study is genuine, not performative; the constraint serves a real function of continuity and identity maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious scholars, this constraint is a vital mechanism for preserving divine law and communal identity. From the perspective of individual adherents, it is a demanding obligation that requires continuous effort without immediate gratification, sustained by faith. The 'identity_locked' exit option for both groups highlights the deep integration of this constraint into their self-concept.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious scholars (agenda_setter) benefit from their central role in interpreting and transmitting this tradition, and bear the primary burden of study. The religious community (beneficiary) benefits from the spiritual continuity and deferred obligation. Individual adherents (payer) bear the diffuse costs of study and expectation. Messianic era priests are a conceptual 'excluded' group, as their role is future-oriented.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nature_of_messianic_restoration,
    'What constitutes ''messianic restoration'' and how would its arrival be recognized, triggering the end of suspension?',
    'Theological consensus among authoritative religious bodies, or a widely accepted historical event fulfilling messianic prophecies.',
    'Ambiguity allows indefinite suspension, potentially increasing long-term extractiveness by deferring fulfillment indefinitely. Clarity would either activate the obligation or definitively abrogate it, altering the constraint''s nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nature_of_messianic_restoration, conceptual, 'Ambiguity regarding the conditions for ending the suspension of sacrifice obligation.').

omega_variable(
    burden_of_study_vs_fulfillment,
    'Is the burden of ''readiness'' (study, communal support) genuinely less extractive than actual ritual performance would be, or does it merely shift the form of extraction?',
    'Comparative analysis of historical and contemporary religious communities, assessing the resource and time demands of active ritual systems versus intensive textual study and deferred expectation.',
    'If the ''readiness'' burden is found to be comparable or higher, the ''suspension'' claim''s coordination function is weakened, and the constraint leans more towards pure extraction (snare-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_of_study_vs_fulfillment, empirical, 'Whether the ''suspended'' obligation genuinely reduces burden or merely reconfigures it.').

omega_variable(
    identity_lock_mechanism,
    'To what extent is the ''identity_locked'' exit option for adherents a result of genuine spiritual conviction versus social or institutional pressure?',
    'Sociological studies of ex-adherents'' experiences, examining the persistence of identity-related costs after physical exit from the community.',
    'If primarily social/institutional, the suppression metric''s ''internalized'' component is lower, and the constraint''s persistence relies more on external enforcement (even if diffuse). If primarily spiritual, the internal suppression is higher and more resilient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural vs. internalized suppression mechanism for identity-locked adherents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 500, 0.1).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 500, 0.42).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1000, 0.43).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1500, 0.44).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 2000, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 500, 0.57).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1000, 0.58).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1500, 0.59).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 2000, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice obligation continuity' kernel. Its structural properties differ significantly from other readings, necessitating separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
