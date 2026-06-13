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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Temple Sacrifice Obligation: Messianic Suspension Reading
 *   domain: religious_studies/halakhic_authority
 *
 * SUMMARY:
 *   This constraint represents the Halakhic (Jewish legal) reading that the
 *   obligation for Temple sacrifices is suspended, neither fulfilled nor
 *   violated, during the period of the Temple's destruction, pending its
 *   messianic restoration. This reading is a specific interpretation of the
 *   broader 'temple_sacrifice_obligation' kernel. It asserts that the
 *   physical impossibility of performance means the obligation is dormant,
 *   not abrogated, and not to be replaced by symbolic acts. This provides a
 *   coherent framework for religious life in exile without imposing an
 *   impossible burden or creating illegitimate substitutes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.05).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.02).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, mountain).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Temple Sacrifice Obligation: Messianic Suspension Reading").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious_studies/halakhic_authority").

domain_priors:emerges_naturally(temple_sacrifice_obligation__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, '179bae26-3b2d-4c04-a17d-d8d3f605e749').
narrative_ontology:cs_kernel_codification('179bae26-3b2d-4c04-a17d-d8d3f605e749', fixed_text).
narrative_ontology:cs_authority_grounding('179bae26-3b2d-4c04-a17d-d8d3f605e749', lineage).
narrative_ontology:cs_interpretation_layer_present('179bae26-3b2d-4c04-a17d-d8d3f605e749').
narrative_ontology:cs_reading_relation('179bae26-3b2d-4c04-a17d-d8d3f605e749', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('179bae26-3b2d-4c04-a17d-d8d3f605e749', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('179bae26-3b2d-4c04-a17d-d8d3f605e749', foundational, obligation_is_literal_performance).
narrative_ontology:cs_axiom_status(obligation_is_literal_performance, holdable).
narrative_ontology:cs_axiom_grounding('179bae26-3b2d-4c04-a17d-d8d3f605e749', obligation_is_literal_performance, deontological).
narrative_ontology:cs_axiom('179bae26-3b2d-4c04-a17d-d8d3f605e749', foundational, messianic_restoration_is_prerequisite).
narrative_ontology:cs_axiom_status(messianic_restoration_is_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('179bae26-3b2d-4c04-a17d-d8d3f605e749', messianic_restoration_is_prerequisite, theological).
narrative_ontology:cs_reference_frame('179bae26-3b2d-4c04-a17d-d8d3f605e749', halakhic_coherence_in_exile).
narrative_ontology:cs_drift_state('179bae26-3b2d-4c04-a17d-d8d3f605e749', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('179bae26-3b2d-4c04-a17d-d8d3f605e749', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, jewish_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, messianic_era_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, scholars_of_sacrifice_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit Halakha, including the status of Temple obligations. This reading allows them to maintain the integrity of the sacrificial system's divine command without requiring impossible actions from the community. They benefit from the stability and coherence this interpretation provides to the legal system.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, rabbinic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Are relieved of an impossible obligation, allowing them to live in accordance with Halakha without guilt or the need for symbolic substitutes. They benefit from the clarity and spiritual peace this interpretation offers, maintaining continuity with tradition without current burden.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, jewish_community, beneficiary,
    organized, generational, identity_locked, global).

% The future community that will actually fulfill the obligation upon the Temple's restoration. This reading preserves the obligation's full force and detail for them, ensuring its authenticity when the time comes. They are the ultimate beneficiaries of the obligation's integrity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, messianic_era_community, beneficiary,
    analytical, civilizational, analytical, universal).

% Their study of the intricate laws of sacrifice is framed as a valuable act of preserving knowledge for the future, rather than a substitute for actual performance. They benefit from the legitimacy and purpose this reading gives to their intellectual pursuit, without the pressure of 'fulfilling' the obligation through study.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, scholars_of_sacrifice_law, beneficiary,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Jewish community's relationship to a central divine commandment (Temple sacrifices) during a period when its performance is physically impossible, preventing despair or the creation of illegitimate substitutes.
% TRANSFER_FUNCTION: Transfers the active burden of the obligation from the present community to a future, messianically restored community, while preserving the knowledge and intent of the commandment.
% ABSENT_VOICES: Those who might argue for symbolic or spiritual fulfillment of the obligation in the present, or for the complete abrogation of the commandment due to its impossibility, are implicitly excluded by this reading's insistence on literal suspension and future restoration.
% DISAPPEARANCE_RATIONALE: If the concept of messianic suspension vanished, the Jewish community would face a profound crisis regarding the status of a central divine commandment. It would either lead to widespread guilt over unfulfilled obligations, or to the creation of new, potentially heterodox, forms of 'fulfillment' that would fundamentally alter Halakhic practice and theological understanding.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the performance of central divine commandments (Temple sacrifices) physically impossible, creating a crisis of religious observance and theological coherence for the Jewish people in exile.
% FOUNDING_PROBLEM_CORROBORATION: The problem of Temple destruction and the inability to perform sacrifices remains a live theological and practical issue for the Jewish community, attested by centuries of rabbinic literature, prayer, and communal mourning. This is corroborated by historical texts and ongoing religious practice across diverse Jewish denominations, not just by rabbinic authorities.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_obligation__messianic_suspension),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because no current action is required, and no party is actively paying or being victimized by the constraint's operation. Suppression is negligible (0.02) as there's no coercion to perform an impossible act, nor is there significant resistance to this widely accepted interpretation. Theater ratio is minimal (0.01) as the constraint's function is primarily theological and legal coherence, not performative maintenance. The high accessibility_collapse (0.95) reflects the physical impossibility of performing sacrifices, making alternatives (actual performance) non-existent. Resistance is low (0.01) because this reading is largely accepted within Orthodox Judaism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the present-day community, this constraint is a Mountain, representing a theological truth about the nature of divine command and historical circumstance. From the perspective of those who might seek immediate, symbolic fulfillment, it might be seen as a 'Rope' coordinating a deferral, but this reading explicitly rejects such 'fulfillment' as a substitute. The engine's classification should reflect the Mountain-like nature of an obligation that is genuinely suspended by external, unchangeable conditions.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and the Jewish community are beneficiaries, as this reading provides a coherent and non-burdensome path for religious observance in exile. The future messianic community is also a beneficiary, as the integrity of the obligation is preserved for them. Scholars of sacrifice law benefit from the legitimacy given to their study as a form of knowledge preservation. There are no direct victims, as no one is currently compelled to pay or perform an impossible act.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nature_of_suspension,
    'Is the suspension of the obligation a temporary abrogation, a deferral, or a redefinition of the obligation''s current form?',
    'Theological and Halakhic analysis of precedents for suspended commandments and the nature of messianic redemption in Jewish thought.',
    'If it''s a temporary abrogation, the ''Mountain'' classification is stronger. If it''s a redefinition, it might imply a subtle ''Rope'' coordinating a new form of observance, even if passive. This reading emphasizes deferral, maintaining the Mountain-like quality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nature_of_suspension, conceptual, 'Clarifies the theological nature of the obligation''s suspended status.').

omega_variable(
    study_as_fulfillment_ambiguity,
    'Does the act of studying the laws of sacrifice, even within this reading, carry any implicit ''fulfillment'' value that could be seen as a form of active, albeit non-sacrificial, observance?',
    'Analysis of rabbinic texts that discuss the merit of Torah study, specifically regarding sacrificial laws, and whether such merit is distinct from or connected to the actual performance of the mitzvah.',
    'If study is seen as a form of ''fulfillment'' or ''occupation'' (as in sibling readings), it would introduce a subtle form of ''extraction'' (the ''cost'' of study as a substitute) and shift the classification towards a ''Rope'' or ''Tangled Rope''. This reading explicitly denies such fulfillment, maintaining low extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_fulfillment_ambiguity, conceptual, 'Examines whether study implicitly fulfills the obligation, impacting extractiveness.').

omega_variable(
    natural_law_vs_divine_decree,
    'Is the ''natural'' impossibility of Temple sacrifice a ''Mountain'' in the sense of physical law, or a ''Mountain'' in the sense of a divine decree that simply acknowledges the physical reality?',
    'Theological inquiry into the relationship between divine will and physical reality in Jewish thought, and whether the ''suspension'' is a consequence of physical impossibility or a direct divine command.',
    'If purely physical, it''s a stronger ''Mountain''. If a divine decree, it introduces an ''agenda-setter'' (God) whose ''will'' is the ultimate constraint, potentially shifting the classification towards a ''Rope'' if there''s an implicit coordination function in accepting the decree.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_divine_decree, conceptual, 'Distinguishes between physical and theological grounding of the constraint''s ''naturalness''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.01).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 500, 0.01).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1000, 0.01).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1500, 0.01).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 2000, 0.01).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 2000, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 500, 0.02).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1000, 0.02).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1500, 0.02).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 2000, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
