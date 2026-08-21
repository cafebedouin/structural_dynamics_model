% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__messianic_suspension, []).

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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Temple Sacrifice Obligation: Messianic Suspension Reading
 *   domain: religious/halakhic_authority
 *
 * SUMMARY:
 *   This constraint represents the 'messianic suspension' reading of the
 *   Temple sacrifice obligation within Halakha (Jewish law). It posits that
 *   the obligation for Temple sacrifices is suspended, neither fulfilled nor
 *   violated, pending the messianic restoration of the Temple. This reading
 *   provides a framework for religious continuity in the absence of the
 *   Temple, with very low extractiveness and no identifiable victim set, as
 *   the obligation is not currently active. The claimed type is 'rope'
 *   because it solves a genuine collective-action problem (how to maintain
 *   religious life without the Temple) with minimal coercion and net benefits
 *   for participants.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.05).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.1).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Temple Sacrifice Obligation: Messianic Suspension Reading").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious/halakhic_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, 'd7cdc0ec-cb13-45ad-af69-906cde1989f3').
narrative_ontology:cs_kernel_codification('d7cdc0ec-cb13-45ad-af69-906cde1989f3', fixed_text).
narrative_ontology:cs_authority_grounding('d7cdc0ec-cb13-45ad-af69-906cde1989f3', lineage).
narrative_ontology:cs_interpretation_layer_present('d7cdc0ec-cb13-45ad-af69-906cde1989f3').
narrative_ontology:cs_reading_relation('d7cdc0ec-cb13-45ad-af69-906cde1989f3', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('d7cdc0ec-cb13-45ad-af69-906cde1989f3', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('d7cdc0ec-cb13-45ad-af69-906cde1989f3', foundational, temple_destruction_suspends_obligation).
narrative_ontology:cs_axiom_status(temple_destruction_suspends_obligation, holdable).
narrative_ontology:cs_axiom_grounding('d7cdc0ec-cb13-45ad-af69-906cde1989f3', temple_destruction_suspends_obligation, theological).
narrative_ontology:cs_axiom('d7cdc0ec-cb13-45ad-af69-906cde1989f3', foundational, divine_command_requires_specific_conditions).
narrative_ontology:cs_axiom_status(divine_command_requires_specific_conditions, holdable).
narrative_ontology:cs_axiom_grounding('d7cdc0ec-cb13-45ad-af69-906cde1989f3', divine_command_requires_specific_conditions, deontological).
narrative_ontology:cs_reference_frame('d7cdc0ec-cb13-45ad-af69-906cde1989f3', halakhic_continuity_in_exile).
narrative_ontology:cs_drift_state('d7cdc0ec-cb13-45ad-af69-906cde1989f3', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d7cdc0ec-cb13-45ad-af69-906cde1989f3', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, observant_jews).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, messianic_activists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the halakhic framework, providing authoritative guidance on the suspended status of sacrificial obligations. They benefit from the stability and continuity of the halakhic system, which this interpretation provides.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, rabbinic_scholars, agenda_setter,
    institutional, generational, constrained, global).

% Adhere to the halakhic framework, finding clarity and continuity in religious practice despite the Temple's absence. They are relieved of an impossible obligation, allowing focus on other mitzvot (commandments) without theological despair.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, observant_jews, beneficiary,
    moderate, biographical, constrained, global).

% Their worldview is affirmed by the expectation of future restoration. While they actively work towards hastening the messianic era, the suspension of sacrifice means their efforts are not burdened by an immediate, impossible ritual requirement, allowing them to focus on other forms of preparation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, messianic_activists, beneficiary,
    organized, generational, identity_locked, global).

% Do not engage with the halakhic framework or its theological implications, and thus are outside the scope of this particular constraint's coordination function. They are neither bound by the obligation nor directly benefit from its suspension.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, secular_jews, excluded,
    powerless, biographical, mobile, local).

% Study the historical development and theological implications of the sacrificial system and its suspension, providing academic analysis without direct participation in the halakhic system or its practical observance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, historical_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and stable framework for Jewish religious practice in the absence of the Temple, preventing theological despair, internal schism, or premature, unauthorized attempts at restoration of sacrificial rites.
% TRANSFER_FUNCTION: Transfers the immediate burden of sacrificial obligation from the observant community to a future messianic era, allowing for continuity of religious life and focus on other commandments.
% ABSENT_VOICES: Those who might argue for alternative, immediate forms of sacrificial fulfillment (e.g., symbolic sacrifices, or a more radical reinterpretation of obligation) are implicitly excluded by the dominant halakhic consensus that upholds the suspension.
% DISAPPEARANCE_RATIONALE: If the halakhic suspension of sacrifice vanished overnight, the entire system of Jewish religious law and practice would be thrown into chaos. Observant Jews would be faced with an impossible, divinely commanded obligation, leading to widespread theological crisis and a breakdown of communal religious life.
% FOUNDING_PROBLEM: How to maintain the integrity of divine command and the continuity of Jewish religious life after the destruction of the Second Temple, when central obligations like Temple sacrifices could no longer be physically fulfilled.
% FOUNDING_PROBLEM_CORROBORATION: Historical rabbinic texts (e.g., Talmudic discussions), centuries of halakhic practice, and the ongoing theological discourse within Orthodox Judaism consistently attest to the problem's enduring relevance. The physical absence of the Temple and the non-arrival of the messianic era are universally acknowledged facts that corroborate the problem's live status.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__messianic_suspension, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).
:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the constraint primarily provides clarity and relief from an impossible obligation, rather than imposing costs. Suppression is low (0.1) as adherence to this interpretation is largely voluntary within the observant community, driven by theological coherence rather than active enforcement against dissent. Theater ratio is also very low (0.05) because the suspension is a genuine theological stance, not a performative cover for other functions. Accessibility collapse is high (0.9) due to the physical impossibility of performing sacrifices without the Temple. Resistance is low (0.1) as the suspension itself is widely accepted within Orthodox Judaism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of observant Jews, this constraint is a beneficial coordination mechanism that resolves a profound theological dilemma. From the perspective of rabbinic scholars, it is a foundational principle ensuring the continuity and coherence of Halakha. There is no significant perspectival gap that would lead to different classifications among those who accept the premise of the obligation and its suspension.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars act as agenda-setters, maintaining the halakhic framework and benefiting from its stability. Observant Jews are beneficiaries, gaining clarity and continuity in their religious practice. Messianic activists also benefit, as the suspension allows them to focus on hastening the messianic era without the burden of an impossible ritual. Secular Jews are excluded, as they do not participate in the halakhic system. Historical scholars are observers, analyzing the phenomenon academically.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nature_of_suspension,
    'Is the suspension of Temple sacrifice a temporary deferral of an immutable obligation, or does it represent a fundamental reinterpretation of the obligation''s nature in the absence of its physical conditions?',
    'Further theological and halakhic discourse, potentially informed by future events (e.g., a new historical context for Temple rebuilding) or new interpretive insights.',
    'If a fundamental reinterpretation, it implies greater flexibility in halakhic adaptation, potentially lowering the perceived ''cost'' of the suspension. If strictly a temporary deferral, it reinforces the immutability of the original command and the urgency of messianic restoration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nature_of_suspension, conceptual, 'Ambiguity regarding the theological depth of the suspension.').

omega_variable(
    study_as_fulfillment_ambiguity,
    'Does the study of Temple sacrifice laws, in the absence of the Temple, constitute a form of ''fulfillment'' of the obligation, or is it solely a means of ''archiving'' knowledge for future restoration, or merely ''maintenance'' of the halakhic system?',
    'Analysis of rabbinic responsa and theological writings across different schools of thought, examining the explicit and implicit claims about the efficacy of study in the interim period.',
    'If study is considered fulfillment (as in the ''study_as_occupation'' reading), it would imply a different form of ''payment'' or ''benefit'' for observant Jews. If it is purely archiving (as in ''study_as_archiving''), it reinforces the ''suspended'' nature of the obligation. This reading emphasizes maintenance, distinct from both fulfillment and pure archiving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_fulfillment_ambiguity, conceptual, 'Ambiguity regarding the role of study in relation to the suspended obligation, differentiating this reading from its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.05).
narrative_ontology:measurement(temp_tr_t400, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 400, 0.05).
narrative_ontology:measurement(temp_tr_t800, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 800, 0.05).
narrative_ontology:measurement(temp_tr_t1200, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1200, 0.05).
narrative_ontology:measurement(temp_tr_t1600, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1600, 0.05).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(temp_be_t400, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 400, 0.05).
narrative_ontology:measurement(temp_be_t800, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 800, 0.05).
narrative_ontology:measurement(temp_be_t1200, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1200, 0.05).
narrative_ontology:measurement(temp_be_t1600, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1600, 0.05).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 2000, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(temp_su_t400, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 400, 0.1).
narrative_ontology:measurement(temp_su_t800, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 800, 0.1).
narrative_ontology:measurement(temp_su_t1200, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1200, 0.1).
narrative_ontology:measurement(temp_su_t1600, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1600, 0.1).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 2000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'temple_sacrifice_obligation' kernel, each representing a distinct halakhic interpretation of the obligation in the absence of the Temple. This reading emphasizes suspension, while others focus on study as occupation or archiving.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
