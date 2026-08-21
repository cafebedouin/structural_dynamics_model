% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: ICRC Customary Law Reading of Common Article 3 Scope
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint story instantiates the ICRC's reading of Common Article
 *   3's scope, which emphasizes its determination through evolving state
 *   practice and opinio juris, as tracked through customary international
 *   law. This reading provides a procedural mechanism for IHL to adapt to new
 *   conflict realities without formal treaty amendment. While intended as a
 *   coordination mechanism for humanitarian protection, the process of
 *   customary law formation can impose costs on states that resist expanded
 *   obligations, leading to moderate extractiveness and suppression. The
 *   claimed type is 'rope' due to its primary coordination function, but the
 *   metrics reflect the friction and contestation inherent in its operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.45).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.55).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "ICRC Customary Law Reading of Common Article 3 Scope").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__icrc_customary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, '426c74e9-9f2b-45c4-8f59-81661a477b2f').
narrative_ontology:cs_kernel_codification('426c74e9-9f2b-45c4-8f59-81661a477b2f', distributed).
narrative_ontology:cs_authority_grounding('426c74e9-9f2b-45c4-8f59-81661a477b2f', expertise).
narrative_ontology:cs_interpretation_layer_present('426c74e9-9f2b-45c4-8f59-81661a477b2f').
narrative_ontology:cs_reading_relation('426c74e9-9f2b-45c4-8f59-81661a477b2f', common_article_3_scope__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('426c74e9-9f2b-45c4-8f59-81661a477b2f', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_axiom('426c74e9-9f2b-45c4-8f59-81661a477b2f', foundational, customary_law_as_dynamic_source).
narrative_ontology:cs_axiom_status(customary_law_as_dynamic_source, holdable).
narrative_ontology:cs_axiom_grounding('426c74e9-9f2b-45c4-8f59-81661a477b2f', customary_law_as_dynamic_source, empirically_contingent).
narrative_ontology:cs_axiom('426c74e9-9f2b-45c4-8f59-81661a477b2f', secondary, icrc_impartial_documentation_mandate).
narrative_ontology:cs_axiom_status(icrc_impartial_documentation_mandate, holdable).
narrative_ontology:cs_axiom_grounding('426c74e9-9f2b-45c4-8f59-81661a477b2f', icrc_impartial_documentation_mandate, conventional).
narrative_ontology:cs_reference_frame('426c74e9-9f2b-45c4-8f59-81661a477b2f', dynamic_customary_evolution).
narrative_ontology:cs_drift_state('426c74e9-9f2b-45c4-8f59-81661a477b2f', contemporary_geopolitical_fragmentation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('426c74e9-9f2b-45c4-8f59-81661a477b2f', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, victims_of_armed_conflict).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, international_courts_and_tribunals).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, human_rights_advocates).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, states_resisting_expanded_obligations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, states_parties_to_geneva_conventions).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, states_parties_to_geneva_conventions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tracks, documents, and advocates for the application of customary international humanitarian law, including the evolving scope of Common Article 3. Its authority derives from its mandate and expertise, not coercive power.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from a clearer, more adaptable framework for IHL, but also bear the costs of conforming to evolving customary obligations, which may expand beyond their initial treaty commitments. They contribute to state practice and opinio juris.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, states_parties_to_geneva_conventions, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, states_parties_to_geneva_conventions, payer).

% Are the ultimate beneficiaries of any expanded scope of Common Article 3, as it provides minimum humanitarian protections in non-international armed conflicts. They have no direct voice in the formation of customary law.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, victims_of_armed_conflict, beneficiary,
    powerless, immediate, trapped, local).

% Utilize customary international law, including the ICRC's documentation, to interpret and apply IHL in their judgments, thereby reinforcing its authority and scope.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_courts_and_tribunals, beneficiary,
    institutional, generational, analytical, global).

% Support the expansion of humanitarian protections and find the ICRC's customary law approach a valuable tool for aligning IHL with evolving human rights standards, even if their own reading is more expansive.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, human_rights_advocates, beneficiary,
    organized, biographical, mobile, global).

% Bear the costs of adapting their military doctrine and practice to evolving customary norms, particularly when these expand their obligations in non-international armed conflicts. They may challenge the interpretation of state practice or opinio juris.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, states_resisting_expanded_obligations, payer,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a dynamic, consensus-based mechanism for states to coordinate on the evolving scope of minimum humanitarian protections in non-international armed conflicts, adapting to new forms of violence without requiring formal treaty amendment.
% TRANSFER_FUNCTION: Transfers the burden of interpreting and applying CA3 from purely static treaty interpretation to a more dynamic, evidence-based process informed by state practice and opinio juris, ultimately transferring greater protection to victims.
% ABSENT_VOICES: Non-state armed groups, who are directly affected by CA3 but have no formal voice in its customary development, would likely advocate for clearer, more consistent application and accountability from all parties.
% DISAPPEARANCE_RATIONALE: If the ICRC's customary law reading vanished overnight, the interpretation of CA3's scope would likely revert to more restrictive, state-centric views, leading to less protection for victims and greater legal uncertainty in non-international armed conflicts. The dynamic adaptation mechanism would be lost.
% FOUNDING_PROBLEM: The static nature of treaty law struggled to adapt Common Article 3's scope to the evolving realities of non-international armed conflicts, leaving gaps in humanitarian protection and legal uncertainty.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and UN bodies corroborate the ongoing challenge of adapting IHL to new conflict dynamics, supporting the need for dynamic interpretation. This corroboration comes from outside the direct beneficiaries of the ICRC's institutional role.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).
:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the political and practical costs for states to adapt to evolving customary norms, even if the reading itself aims for coordination. Suppression (0.55) is moderate, as states can resist, but face diplomatic and reputational pressure to conform. Theater ratio is low (0.15) because the ICRC's documentation of customary law is a serious, evidence-based endeavor, not primarily performative. Resistance (0.60) is high because states frequently challenge interpretations that expand their obligations. Accessibility collapse (0.40) is moderate, as states have alternatives (e.g., persistent objector status) but face costs for non-conformity.
 *
 * PERSPECTIVAL GAP:
 *   From the ICRC's and victims' perspectives, this reading is a vital coordination mechanism for humanitarian protection. From the perspective of states resisting expanded obligations, it can be seen as an imposition of new duties without explicit consent, leading to a perception of extraction. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICRC, victims of armed conflict, international courts, and human rights advocates are beneficiaries, as this reading clarifies and expands protections. States resisting expanded obligations are targets, bearing the costs of adapting to evolving norms. States parties generally benefit from a clearer framework but also pay through increased obligations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opinio_juris_determination_ambiguity,
    'How is ''opinio juris'' (a sense of legal obligation) reliably determined in practice, and whose ''opinion'' carries the most weight in shaping customary international law?',
    'Detailed empirical studies of state declarations, voting patterns in international fora, and legal arguments in domestic and international courts, disaggregated by state power and influence.',
    'If powerful states'' opinions disproportionately shape custom, the constraint''s effective extraction from less powerful states is higher than measured; if it''s a genuinely distributed consensus, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opinio_juris_determination_ambiguity, empirical, 'Ambiguity in the evidentiary basis and power dynamics of opinio juris.').

omega_variable(
    customary_law_vs_treaty_law_primacy,
    'To what extent does the dynamic evolution of customary international law, as tracked by the ICRC, supersede or merely complement the more static, consent-based obligations of treaty law for states parties?',
    'Analysis of state practice regarding reservations to treaties versus adherence to customary norms, and judicial decisions on the hierarchy of sources in specific cases.',
    'If customary law is seen as superseding treaty law, the constraint''s effective suppression on states is higher, as their ability to limit obligations through reservations is diminished. If complementary, the coordination function is more flexible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_vs_treaty_law_primacy, conceptual, 'The relationship between customary and treaty law in determining state obligations.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression on states resisting expanded obligations primarily structural (e.g., diplomatic pressure, reputational costs) or internalized (e.g., states'' self-perception as law-abiding actors)?',
    'Post-resistance trajectory: if states continue to adhere to norms after external pressure subsides, reclassify as partially internalized. Analysis of domestic legal advice and public discourse within resisting states.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — states carry the suppression with them after external pressure is removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1990, common_article_3_scope__icrc_customary_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(comm_tr_t1998, common_article_3_scope__icrc_customary_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(comm_tr_t2006, common_article_3_scope__icrc_customary_reading, theater_ratio, 2006, 0.13).
narrative_ontology:measurement(comm_tr_t2014, common_article_3_scope__icrc_customary_reading, theater_ratio, 2014, 0.14).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__icrc_customary_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(comm_be_t1990, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(comm_be_t1998, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1998, 0.38).
narrative_ontology:measurement(comm_be_t2006, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2006, 0.41).
narrative_ontology:measurement(comm_be_t2014, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2014, 0.43).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1990, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(comm_su_t1998, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1998, 0.49).
narrative_ontology:measurement(comm_su_t2006, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2006, 0.52).
narrative_ontology:measurement(comm_su_t2014, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2014, 0.54).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__expansive_human_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'common_article_3_scope' kernel, focusing on the ICRC's customary law approach. Its ε value differs significantly from the more restrictive 'state_centric_reading' and the more expansive 'expansive_human_rights_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
