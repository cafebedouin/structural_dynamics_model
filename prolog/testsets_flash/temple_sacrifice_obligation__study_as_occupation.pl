% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Study of Sacrifice Law as Occupation of Obligation in Temple's Absence
 *   domain: religious_studies/halakhic_authority/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the Halakhic (Jewish legal) interpretation that
 *   the study of the laws pertaining to Temple sacrifices is considered a
 *   legitimate and spiritually meritorious 'occupation' or fulfillment of the
 *   obligation, even in the absence of the Temple itself. This reading
 *   asserts that the divine commandment remains active and can be engaged
 *   with through intellectual and spiritual means, rather than being
 *   suspended or merely archived for a future restoration. It is a
 *   foundational principle for many observant Jews, providing continuity and
 *   meaning in a post-Temple era.
 *
 * KEY AGENTS:
 *   - halakhic_scholars: Agenda setter (institutional/analytical) — interpret and transmit the tradition, benefiting from its continuity.
 *   - observant_jews: Beneficiary (organized/moderate) — fulfill a core religious obligation through study, gaining spiritual merit and communal identity.
 *   - messianic_activists: Excluded (organized) — prioritize physical restoration of the Temple, viewing study as insufficient or a distraction from active preparation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.1).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.05).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.1).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, mountain).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study of Sacrifice Law as Occupation of Obligation in Temple's Absence").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious_studies/halakhic_authority/commitment_systems").

domain_priors:emerges_naturally(temple_sacrifice_obligation__study_as_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, '39ac2105-97c3-443e-8faf-430e21f9265d').
narrative_ontology:cs_kernel_codification('39ac2105-97c3-443e-8faf-430e21f9265d', fixed_text).
narrative_ontology:cs_authority_grounding('39ac2105-97c3-443e-8faf-430e21f9265d', lineage).
narrative_ontology:cs_interpretation_layer_present('39ac2105-97c3-443e-8faf-430e21f9265d').
narrative_ontology:cs_reading_relation('39ac2105-97c3-443e-8faf-430e21f9265d', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('39ac2105-97c3-443e-8faf-430e21f9265d', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('39ac2105-97c3-443e-8faf-430e21f9265d', foundational, study_is_equivalent_to_performance).
narrative_ontology:cs_axiom_status(study_is_equivalent_to_performance, holdable).
narrative_ontology:cs_axiom_grounding('39ac2105-97c3-443e-8faf-430e21f9265d', study_is_equivalent_to_performance, theological).
narrative_ontology:cs_axiom('39ac2105-97c3-443e-8faf-430e21f9265d', foundational, divine_commandment_is_perpetual).
narrative_ontology:cs_axiom_status(divine_commandment_is_perpetual, holdable).
narrative_ontology:cs_axiom_grounding('39ac2105-97c3-443e-8faf-430e21f9265d', divine_commandment_is_perpetual, deontological).
narrative_ontology:cs_reference_frame('39ac2105-97c3-443e-8faf-430e21f9265d', rabbinic_continuity_post_temple).
narrative_ontology:cs_drift_state('39ac2105-97c3-443e-8faf-430e21f9265d', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('39ac2105-97c3-443e-8faf-430e21f9265d', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, halakhic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, observant_jews).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, torah_study_as_equivalent_to_performance).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, continuity_of_divine_commandment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the Halakhic tradition, defining what constitutes legitimate fulfillment of religious obligations. They benefit from the continuity and intellectual engagement provided by this reading, which reinforces their role and authority.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, halakhic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Adhere to the Halakhic tradition and find spiritual meaning and communal identity through the study of sacrifice laws. This reading provides them with a tangible way to fulfill a core religious obligation in the present, fostering a sense of continuity with historical practice.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, observant_jews, beneficiary,
    organized, biographical, identity_locked, global).

% Advocate for the active rebuilding of the Temple and the resumption of physical sacrifices. They may view 'study as occupation' as a passive or insufficient response, potentially diverting energy from more direct messianic efforts. Their voice is often marginalized in mainstream Halakhic discourse regarding this specific interpretation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, messianic_activists, excluded,
    organized, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_occupation, diffuse).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and accessible framework for observant Jews to engage with and fulfill the divine commandment of Temple sacrifices, maintaining spiritual and communal continuity in the absence of the physical Temple.
% TRANSFER_FUNCTION: Transfers spiritual merit, communal identity, and intellectual engagement to observant Jews and Halakhic scholars, in exchange for their commitment to study and adherence to the interpretation.
% ABSENT_VOICES: Messianic activists and those who believe the obligation is strictly suspended or requires physical performance would object, arguing that study, while valuable, does not constitute actual 'occupation' or fulfillment. Their voices are present in broader theological debates but are not central to the internal logic of this specific Halakhic reading.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, a significant portion of observant Jewish life would lose a primary mode of spiritual engagement and fulfillment related to the Temple. It would create a profound theological void, forcing a re-evaluation of the nature of divine commandment and human obligation, leading to a substantial rearrangement of religious practice and identity.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) rendered the central act of Jewish worship – sacrificial offerings – impossible, creating a profound crisis of religious practice and continuity for observant Jews.
% FOUNDING_PROBLEM_CORROBORATION: The problem of fulfilling divine commandments related to the Temple in its absence is still a live theological and practical concern for observant Jews. Halakhic authorities and historical texts from outside the immediate beneficiaries (e.g., early rabbinic literature, medieval commentators) corroborate the ongoing nature of this challenge and the role of study as a response.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_occupation_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_obligation__study_as_occupation),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because it is presented as an immutable theological truth, a direct consequence of divine command and historical circumstance (the destruction of the Temple). Its extractiveness is very low (0.1) because it primarily offers spiritual benefit and continuity, rather than imposing material costs or extracting resources. Suppression is negligible (0.05) as participation is voluntary and deeply integrated into religious life; there are no coercive mechanisms. Theater ratio is zero (0.0) as the activity is considered genuinely functional and meaningful. Accessibility collapse is high (0.9) because, within this framework, there are no 'alternatives' to fulfilling the obligation through study in the Temple's absence; it is the prescribed path. Resistance is very low (0.02) as this reading is widely accepted within Orthodox Judaism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Halakhic scholars and observant Jews, this is a clear Mountain, an unchangeable truth that provides a path for spiritual fulfillment. From an external, secular analytical perspective, it might be viewed as a constructed interpretation that serves to maintain the relevance and authority of religious law and its interpreters in changed circumstances. However, within the internal logic of the commitment system, its Mountain status is robust.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars are beneficiaries and agenda-setters: they interpret, transmit, and benefit from the continuity of the tradition. Observant Jews are beneficiaries: they fulfill a core religious obligation and gain spiritual merit. There are no direct 'victims' as the constraint offers a path to fulfillment rather than imposing a burden. The directionality for both groups is towards the beneficiary end (low d), reflecting the spiritual gains and lack of material extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents the 'mandate' of Temple sacrifice from atrophying into irrelevance by providing an alternative, accessible mode of fulfillment. It avoids mislabeling a genuine spiritual practice as pure extraction by emphasizing the internal logic of obligation and fulfillment through study. The 'mandatrophy_resolved' flag is not set because the mandate is not considered to have atrophied; rather, its mode of fulfillment has adapted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_obligation,
    'Is the obligation to study sacrifice law in the Temple''s absence a genuine natural law (a direct consequence of divine command and historical circumstance), or a constructed interpretation that benefits identifiable agents (Halakhic scholars, observant Jews)?',
    'Analysis of alternative interpretations and their historical reception; theological and philosophical inquiry into the nature of divine commandment and human obligation.',
    'If purely constructed, the constraint might be reclassified as a Rope or even a Tangled Rope, depending on the degree of extraction and suppression involved in maintaining the interpretation. If a genuine natural law, its Mountain classification is affirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_obligation, conceptual, 'Ambiguity between natural law and constructed obligation for study as occupation.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''study_as_occupation'' reading of the ''temple_sacrifice_obligation'' kernel. What would change if a sibling reading were adopted?',
    'Examining the structural implications of ''messianic_suspension'' (obligation suspended) or ''study_as_archiving'' (study preserves knowledge but doesn''t fulfill obligation).',
    'If ''messianic_suspension'' were adopted, the obligation would be seen as dormant, not actively fulfilled, potentially reducing the perceived spiritual benefit of study. If ''study_as_archiving'' were adopted, study would be seen as preparatory, not performative, altering the nature of the ''occupation''. This reading''s low extractiveness and lack of victims would be challenged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of adopting sibling readings of the temple_sacrifice_obligation kernel.').

omega_variable(
    disagreement_location,
    'Where is the disagreement located between the ''study_as_occupation'' reading and its siblings?',
    'Analysis of the core axioms and their grounding types across the different readings.',
    'The disagreement is located in the nature of fulfillment and the status of the obligation in the absence of the Temple. This reading asserts active fulfillment through study, while siblings assert suspension or preparatory action. This impacts the perceived ''live'' status of the founding problem and the role of human agency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location, conceptual, 'Location of disagreement between kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.0).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 100, 0.0).
narrative_ontology:measurement(temp_tr_t200, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 200, 0.0).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 100, 0.1).
narrative_ontology:measurement(temp_be_t200, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 200, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 100, 0.05).
narrative_ontology:measurement(temp_su_t200, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 200, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'temple_sacrifice_obligation' kernel. Each reading represents a distinct structural claim about the nature and fulfillment of the obligation in the absence of the Temple.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
