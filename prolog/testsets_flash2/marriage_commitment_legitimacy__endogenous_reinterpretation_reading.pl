% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: Divine Command for Marriage Reversal (Endogenous Reinterpretation)
 *   domain: religious/institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the reading that the 1890 Manifesto, which
 *   reversed the Church's prior marriage practices, was a genuine prophetic
 *   revelation. God commanded the change to preserve the Church for higher
 *   purposes, not as a capitulation to external pressure. This reading
 *   emphasizes theological continuity through reinterpretation (monogamy as a
 *   new covenant stage) and maintains the legitimacy of prophetic succession.
 *   It is classified as a Mountain because, from this internal perspective,
 *   the divine command is an unchangeable, irreducible limit on practice,
 *   emerging from the very nature of God's ongoing guidance to the Church.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.15).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.25).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, mountain).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "Divine Command for Marriage Reversal (Endogenous Reinterpretation)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious/institutional_history/political_theology/commitment_systems").

domain_priors:emerges_naturally(marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '95e94956-cc2c-48ff-bef2-360da5a09ce2').
narrative_ontology:cs_kernel_codification('95e94956-cc2c-48ff-bef2-360da5a09ce2', fixed_text).
narrative_ontology:cs_authority_grounding('95e94956-cc2c-48ff-bef2-360da5a09ce2', lineage).
narrative_ontology:cs_interpretation_layer_present('95e94956-cc2c-48ff-bef2-360da5a09ce2').
narrative_ontology:cs_reading_relation('95e94956-cc2c-48ff-bef2-360da5a09ce2', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('95e94956-cc2c-48ff-bef2-360da5a09ce2', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('95e94956-cc2c-48ff-bef2-360da5a09ce2', foundational, divine_will_guides_church_through_prophet).
narrative_ontology:cs_axiom_status(divine_will_guides_church_through_prophet, holdable).
narrative_ontology:cs_axiom_grounding('95e94956-cc2c-48ff-bef2-360da5a09ce2', divine_will_guides_church_through_prophet, theological).
narrative_ontology:cs_axiom('95e94956-cc2c-48ff-bef2-360da5a09ce2', foundational, institutional_preservation_is_divine_mandate).
narrative_ontology:cs_axiom_status(institutional_preservation_is_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('95e94956-cc2c-48ff-bef2-360da5a09ce2', institutional_preservation_is_divine_mandate, theological).
narrative_ontology:cs_reference_frame('95e94956-cc2c-48ff-bef2-360da5a09ce2', continuous_prophetic_guidance).
narrative_ontology:cs_drift_state('95e94956-cc2c-48ff-bef2-360da5a09ce2', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('95e94956-cc2c-48ff-bef2-360da5a09ce2', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_members).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_guidance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and transmits divine will, guiding the Church through periods of change. Benefits from the continued legitimacy of prophetic authority and the preservation of the institution. Their identity is fused with the prophetic role.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Receive spiritual guidance and the promise of salvation through adherence to the Church's teachings. Their faith and identity are deeply intertwined with the belief in continuous divine revelation and the Church's unique mission.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_members, beneficiary,
    moderate, biographical, identity_locked, global).

% Analyze the historical and theological consistency of the Manifesto's claims within the broader tradition of religious thought. Their role is to understand the internal logic and external coherence of the reinterpretation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, historical_theologians, observer,
    analytical, civilizational, analytical, universal).

% Exerted legal pressure that coincided with the Manifesto's issuance. From this reading's perspective, the government's actions were a catalyst, not the ultimate cause, of the divine command. It is excluded from the theological justification.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, federal_government, excluded,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective understanding and practice of marriage within the Church, ensuring theological consistency and institutional survival in the face of external pressures, by reinterpreting divine will.
% TRANSFER_FUNCTION: Transfers the authority for defining marriage from a prior, more expansive interpretation to a new, more restrictive one, preserving the Church's prophetic succession and institutional integrity.
% ABSENT_VOICES: Those who interpret the Manifesto as a capitulation to federal power, or as a purely pragmatic institutional move, are absent from this reading's theological justification. They would argue for a different source of the change.
% DISAPPEARANCE_RATIONALE: If this specific reading of the Manifesto vanished, the historical fact of the reversal would remain, but the theological justification for it would be contested. The Church would still exist, but the narrative of its divine guidance through this specific event would be lost or replaced by alternative interpretations.
% FOUNDING_PROBLEM: The Church faced existential legal and social threats due to its prior marriage practices, jeopardizing its ability to continue its mission and preserve its members.
% FOUNDING_PROBLEM_CORROBORATION: Church leadership attests the problem was existential and the divine command was the only path to preserve the institution. Faithful members corroborate this through their lived experience of adherence and continued faith. External historians and sociologists acknowledge the severe external pressures faced by the Church at the time, corroborating the context of the problem, though not necessarily the divine command as its resolution.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_unchanged).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, ExtMetricName, E),
    domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(marriage_commitment_legitimacy__endogenous_reinterpretation_reading),
    narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the change is framed as a divine command for the collective good, not as a means of extracting resources or benefits from members. Suppression is low because adherence is seen as voluntary submission to divine will, not coercion. Theater ratio is low as the prophetic claim is central to the Church's identity and is genuinely believed. Accessibility collapse is high (0.85) because, from this perspective, the divine command leaves no legitimate alternative for faithful members. Resistance is low (0.05) because internal dissent is framed as resistance to divine will, which is minimized within this reading.
 *
 * PERSPECTIVAL GAP:
 *   From this reading, the change is a divine imperative, a Mountain. From an 'exogenous override' reading, it would be a Snare or Tangled Rope, driven by external coercion. The engine's classification will highlight this divergence based on the structural data provided for each reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership and faithful members are beneficiaries, as they gain spiritual and institutional preservation. Their identity is deeply tied to the prophetic authority, making their exit options 'identity_locked'. Historical theologians are observers, analyzing the theological coherence. The federal government is 'excluded' from the theological justification, seen as a catalyst rather than the cause.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by asserting the divine mandate is not only live but actively guiding. The 'founding_problem_status' being 'live' and 'disappearance_verdict' being 'world_unchanged' (for the theological justification, not the historical event) reinforces the claim that the constraint's function is ongoing and essential, not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_command_vs_external_pressure,
    'Is the Manifesto primarily a divine command (endogenous revelation) or a response to overwhelming federal pressure (exogenous coercion)?',
    'Discovery of new historical documents detailing internal deliberations, or a shift in the Church''s official narrative acknowledging external influence as a primary driver.',
    'If primarily exogenous, the constraint''s classification would shift from Mountain to Snare or Tangled Rope, reflecting a higher extractiveness and suppression driven by external forces rather than internal divine will.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_command_vs_external_pressure, empirical, 'Ambiguity regarding the ultimate cause of the Manifesto''s issuance.').

omega_variable(
    theological_continuity_vs_rupture,
    'Does the reinterpretation of marriage truly maintain theological continuity, or does it represent a significant rupture in doctrine justified post-hoc?',
    'Extensive theological analysis by independent scholars, or a future prophetic declaration that clarifies or re-establishes prior doctrines.',
    'If a rupture, the ''theater_ratio'' would increase, and the ''extractiveness'' might be re-evaluated upward, as the justification for the change would be seen as less ''natural'' and more ''constructed'' to maintain institutional power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_continuity_vs_rupture, conceptual, 'Ambiguity regarding the theological consistency of the reinterpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.05).
narrative_ontology:measurement(marr_tr_t1920, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.07).
narrative_ontology:measurement(marr_tr_t1950, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(marr_tr_t1980, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1980, 0.09).
narrative_ontology:measurement(marr_tr_t2024, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.1).
narrative_ontology:measurement(marr_be_t1920, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.12).
narrative_ontology:measurement(marr_be_t1950, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1950, 0.13).
narrative_ontology:measurement(marr_be_t1980, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1980, 0.14).
narrative_ontology:measurement(marr_be_t2024, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.2).
narrative_ontology:measurement(marr_su_t1920, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.22).
narrative_ontology:measurement(marr_su_t1950, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1950, 0.23).
narrative_ontology:measurement(marr_su_t1980, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1980, 0.24).
narrative_ontology:measurement(marr_su_t2024, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
