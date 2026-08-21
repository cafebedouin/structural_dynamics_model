% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__expansive_human_rights_reading, []).

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
 *   constraint_id: common_article_3_scope__expansive_human_rights_reading
 *   human_readable: Common Article 3 Scope: Expansive Human Rights Reading
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This constraint represents the 'expansive human rights reading' of Common
 *   Article 3 (CA3) of the Geneva Conventions, which asserts that CA3's
 *   minimum humanitarian standards apply to any organized armed violence,
 *   regardless of its formal classification as an international or
 *   non-international armed conflict. This reading aims to close legal gaps
 *   and ensure universal protection for victims, but it imposes significant
 *   compliance costs on states and armed groups who often prefer narrower
 *   interpretations. The constraint is claimed as a Tangled Rope because it
 *   genuinely coordinates minimum standards while simultaneously extracting
 *   compliance from resistant actors through active enforcement and potential
 *   prosecution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.8).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.75).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "Common Article 3 Scope: Expansive Human Rights Reading").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, 'c6cb24f3-4f2a-452f-bc47-1de383547321').
narrative_ontology:cs_kernel_codification('c6cb24f3-4f2a-452f-bc47-1de383547321', fixed_text).
narrative_ontology:cs_authority_grounding('c6cb24f3-4f2a-452f-bc47-1de383547321', lineage).
narrative_ontology:cs_interpretation_layer_present('c6cb24f3-4f2a-452f-bc47-1de383547321').
narrative_ontology:cs_reading_relation('c6cb24f3-4f2a-452f-bc47-1de383547321', common_article_3_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('c6cb24f3-4f2a-452f-bc47-1de383547321', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('c6cb24f3-4f2a-452f-bc47-1de383547321', foundational, human_dignity_universal_and_non_derogable).
narrative_ontology:cs_axiom_status(human_dignity_universal_and_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('c6cb24f3-4f2a-452f-bc47-1de383547321', human_dignity_universal_and_non_derogable, deontological).
narrative_ontology:cs_axiom('c6cb24f3-4f2a-452f-bc47-1de383547321', foundational, conflict_classification_irrelevant_to_basic_protections).
narrative_ontology:cs_axiom_status(conflict_classification_irrelevant_to_basic_protections, holdable).
narrative_ontology:cs_axiom_grounding('c6cb24f3-4f2a-452f-bc47-1de383547321', conflict_classification_irrelevant_to_basic_protections, conventional).
narrative_ontology:cs_reference_frame('c6cb24f3-4f2a-452f-bc47-1de383547321', universal_human_dignity_framework).
narrative_ontology:cs_drift_state('c6cb24f3-4f2a-452f-bc47-1de383547321', post_cold_war_human_rights_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('c6cb24f3-4f2a-452f-bc47-1de383547321', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, international_human_rights_bodies).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, victims_of_organized_armed_violence).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, states_engaging_in_organized_violence).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and promotes the broad application of CA3, interpreting it as a universal floor of human rights standards. They monitor compliance, issue reports, and push for accountability through international mechanisms. They benefit from the expanded scope of protection.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_human_rights_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Are the primary intended beneficiaries of this expansive reading, as it seeks to extend minimum humanitarian protections to them regardless of the formal classification of the conflict they are caught in. They bear the direct costs of violations.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, victims_of_organized_armed_violence, beneficiary,
    powerless, immediate, trapped, local).

% Are constrained by this reading, as it imposes obligations on their conduct in a wider range of situations, including internal security operations or low-intensity conflicts they might prefer to define outside IHL. They face potential legal and reputational costs for non-compliance.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, states_engaging_in_organized_violence, payer,
    institutional, biographical, constrained, national).

% Are also constrained by this reading, which asserts that CA3 applies to their conduct regardless of whether they are recognized as parties to an international armed conflict. They face pressure to adhere to humanitarian standards and potential prosecution for violations.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups, payer,
    organized, immediate, constrained, regional).

% Serve as enforcement mechanisms, interpreting and applying CA3 broadly to prosecute individuals responsible for war crimes in situations of organized armed violence, thereby reinforcing the expansive reading's scope.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_criminal_courts, agenda_setter,
    institutional, generational, analytical, global).

% Monitors and promotes IHL, including CA3. While it tracks customary international law (which may be more conservative), it also engages with human rights bodies and acknowledges the need for broad protection, observing the contest over CA3's scope.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, icrc, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal minimum floor of humanitarian standards applicable to all forms of organized armed violence, ensuring basic human dignity and protection for affected populations regardless of the formal classification of the conflict.
% TRANSFER_FUNCTION: Transfers legal obligations and accountability from a narrow, state-centric view of conflict to a broader, human-rights-centric view, imposing costs of compliance and potential prosecution on actors engaged in violence, and extending protections to a wider victim set.
% ABSENT_VOICES: Individuals and communities suffering from violence in situations not traditionally classified as armed conflict (e.g., internal disturbances, state repression) are often marginalized. They would strongly advocate for this expansive reading, but their voices are frequently suppressed by state power or lack of legal standing.
% DISAPPEARANCE_RATIONALE: If this expansive reading vanished overnight, the normative landscape for armed conflict would shift dramatically. Many acts of organized violence would fall into legal gaps, leading to a significant reduction in accountability and protection for victims, and a more permissive environment for human rights abuses by states and non-state actors.
% FOUNDING_PROBLEM: The historical failure of traditional IHL to adequately protect civilians and combatants in non-international armed conflicts and other forms of organized violence, particularly where states sought to avoid IHL obligations by denying conflict classification or asserting domestic jurisdiction.
% FOUNDING_PROBLEM_CORROBORATION: Human rights reports from organizations like Amnesty International and Human Rights Watch, UN investigations, and extensive academic legal scholarship consistently document ongoing protection gaps and abuses in situations that this reading seeks to cover. International criminal tribunals' jurisprudence also reflects this evolving interpretation.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(common_article_3_scope__expansive_human_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__expansive_human_rights_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__expansive_human_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__expansive_human_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.8) because this reading significantly expands the scope of legal obligations, imposing substantial costs on actors who would prefer to operate with fewer constraints. Suppression is also high (0.75) due to the active enforcement efforts by international courts and human rights bodies, which seek to compel adherence and punish violations. The theater ratio is moderate (0.4), reflecting that while some actors may pay lip service to these standards, there is genuine intent and effort to enforce them, though full compliance remains elusive. Accessibility collapse is high (0.85) as this reading aims to eliminate legal loopholes that previously allowed actors to evade accountability. Resistance is high (0.7) because states and armed groups frequently challenge this broad application, viewing it as an infringement on sovereignty or operational flexibility.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human rights advocates and victims, this reading is a necessary and just expansion of protection. From the perspective of states and armed groups, it is an overreach that unduly constrains their actions and blurs the lines between IHL and human rights law. The engine's per-seat classification will reflect this divergence, showing a beneficial classification for advocates and victims, and an extractive one for states and armed groups.
 *
 * DIRECTIONALITY LOGIC:
 *   International human rights bodies and victims of organized armed violence are the primary beneficiaries (low d), as this reading expands their protective scope and advocacy mandate. States and non-state armed groups engaged in violence are the targets (high d), as they bear the costs of increased legal obligations and potential prosecution. The ICRC, while promoting IHL, acts more as an observer in this specific interpretive contest, maintaining a more neutral stance on the precise scope of CA3's application.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_organized_violence,
    'What constitutes ''organized armed violence'' for the purpose of triggering CA3 under this reading, particularly in ambiguous situations like sustained internal disturbances or counter-terrorism operations?',
    'Further jurisprudence from international courts or authoritative interpretations by human rights bodies that provide clear criteria for ''organized'' and ''violence'' in diverse contexts.',
    'A clearer definition would solidify the constraint''s application, reducing ambiguity for both perpetrators and victims. An overly broad definition might face increased resistance from states; an overly narrow one would undermine the expansive intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_organized_violence, conceptual, 'Ambiguity in the threshold for applying CA3 under an expansive reading.').

omega_variable(
    tension_with_state_sovereignty,
    'To what extent does this expansive reading of CA3 infringe upon state sovereignty and domestic jurisdiction over internal security matters, and how is this tension resolved in practice?',
    'Analysis of state practice and international legal decisions where states have invoked sovereignty to resist CA3''s application, and how international bodies have responded.',
    'If the tension is consistently resolved in favor of state sovereignty, the effective scope and enforcement of this reading would be diminished. If international human rights norms consistently override sovereignty claims in these contexts, the constraint''s power would be amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tension_with_state_sovereignty, empirical, 'The practical resolution of the conflict between expansive CA3 application and state sovereignty claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1990, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement(comm_tr_t1997, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1997, 0.42).
narrative_ontology:measurement(comm_tr_t2004, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2004, 0.4).
narrative_ontology:measurement(comm_tr_t2011, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2011, 0.38).
narrative_ontology:measurement(comm_tr_t2018, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2018, 0.39).
narrative_ontology:measurement(comm_tr_t2025, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(comm_be_t1990, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(comm_be_t1997, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1997, 0.68).
narrative_ontology:measurement(comm_be_t2004, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2004, 0.73).
narrative_ontology:measurement(comm_be_t2011, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2011, 0.77).
narrative_ontology:measurement(comm_be_t2018, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2018, 0.79).
narrative_ontology:measurement(comm_be_t2025, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2025, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1990, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(comm_su_t1997, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1997, 0.62).
narrative_ontology:measurement(comm_su_t2004, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2004, 0.68).
narrative_ontology:measurement(comm_su_t2011, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2011, 0.72).
narrative_ontology:measurement(comm_su_t2018, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2018, 0.74).
narrative_ontology:measurement(comm_su_t2025, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
