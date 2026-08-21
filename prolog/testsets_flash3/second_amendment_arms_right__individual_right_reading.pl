% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__individual_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment: Individual Right to Bear Arms
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'individual right' reading of the Second
 *   Amendment, asserting that the right to keep and bear arms is a
 *   pre-existing individual liberty protected against federal infringement.
 *   This reading has gained significant legal traction, particularly since
 *   the late 20th century, and has profound implications for gun control
 *   legislation. It is one of several competing interpretations of the Second
 *   Amendment, each with distinct structural consequences. The claimed type
 *   'tangled_rope' reflects the dual nature of this constraint: it
 *   coordinates individual liberty for gun owners while extracting regulatory
 *   capacity from government bodies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.7).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.6).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment: Individual Right to Bear Arms").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, '21816dd2-39cb-4121-9aca-21813894ccfd').
narrative_ontology:cs_kernel_codification('21816dd2-39cb-4121-9aca-21813894ccfd', fixed_text).
narrative_ontology:cs_authority_grounding('21816dd2-39cb-4121-9aca-21813894ccfd', lineage).
narrative_ontology:cs_interpretation_layer_present('21816dd2-39cb-4121-9aca-21813894ccfd').
narrative_ontology:cs_reading_relation('21816dd2-39cb-4121-9aca-21813894ccfd', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('21816dd2-39cb-4121-9aca-21813894ccfd', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('21816dd2-39cb-4121-9aca-21813894ccfd', foundational, individual_right_to_self_defense).
narrative_ontology:cs_axiom_status(individual_right_to_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('21816dd2-39cb-4121-9aca-21813894ccfd', individual_right_to_self_defense, deontological).
narrative_ontology:cs_axiom('21816dd2-39cb-4121-9aca-21813894ccfd', foundational, pre_existing_natural_right).
narrative_ontology:cs_axiom_status(pre_existing_natural_right, holdable).
narrative_ontology:cs_axiom_grounding('21816dd2-39cb-4121-9aca-21813894ccfd', pre_existing_natural_right, deontological).
narrative_ontology:cs_reference_frame('21816dd2-39cb-4121-9aca-21813894ccfd', originalist_individual_liberty).
narrative_ontology:cs_drift_state('21816dd2-39cb-4121-9aca-21813894ccfd', contemporary_jurisprudence, gap(stable, minor, true)).
narrative_ontology:cs_created_at('21816dd2-39cb-4121-9aca-21813894ccfd', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, federal_regulatory_agencies).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, state_legislatures).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, natural_rights_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, individual_liberty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the legal protection of their right to own firearms for self-defense and other lawful purposes. They actively resist any legislation that infringes upon this right, viewing it as fundamental. Exit options are limited to non-compliance or relocation to more permissive jurisdictions.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, constrained, national).

% Benefits from a robust market for firearms and accessories, protected by this interpretation of the Second Amendment. They lobby extensively against restrictive legislation and fund legal challenges. Their mobility allows them to adapt to some regulatory changes, but core prohibitions would be highly damaging.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, firearms_industry, beneficiary,
    powerful, generational, mobile, national).

% Bear the costs of legal challenges and political resistance when attempting to implement firearm regulations. Their ability to enact public safety measures is constrained by this reading, leading to diffuse costs in terms of public health and safety outcomes. Exit options are limited by judicial review and legislative mandates.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_regulatory_agencies, payer,
    institutional, generational, constrained, national).

% Face similar constraints to federal agencies, often finding their attempts to regulate firearms preempted or overturned by courts applying this individual rights interpretation. They bear the political and social costs of perceived inaction on gun violence. Exit options are limited by federal judicial supremacy.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, state_legislatures, payer,
    institutional, generational, constrained, regional).

% Advocate for an interpretation where the Second Amendment primarily protects a state's right to maintain a militia, not an individual's right to own guns for any purpose. Their voice is largely marginalized in the dominant legal discourse shaped by the individual rights reading.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, collective_right_advocates, excluded,
    moderate, generational, identity_locked, national).

% Analyze the Second Amendment through the lens of armed citizenship as a prerequisite for republican self-governance, distinct from both purely individual and state-centered rights. They observe the contestation and its implications for political theory.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, civic_republican_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the legal framework for individual firearm ownership, providing a clear (though contested) standard for what constitutes a protected right, thereby reducing uncertainty for gun owners and the firearms industry.
% TRANSFER_FUNCTION: Transfers the burden of public safety measures from individual gun owners and the firearms industry to federal and state governments, which are constrained in their ability to regulate. It also transfers political capital and influence to pro-gun advocacy groups.
% ABSENT_VOICES: Advocates for a collective or civic republican right to bear arms are largely excluded from the dominant legal and political discourse, which is heavily shaped by the individual rights interpretation. Their arguments for state militia authority or civic duty are not given equal weight.
% DISAPPEARANCE_RATIONALE: If this individual rights reading vanished, federal and state governments would gain significantly more latitude to regulate firearms, leading to a rapid increase in gun control legislation. The firearms industry would face severe market contraction, and individual gun owners would lose a key legal defense, fundamentally altering the landscape of gun ownership and public safety policy.
% FOUNDING_PROBLEM: The Second Amendment was adopted to ensure the security of a free state, particularly in the context of a standing army and the need for a well-regulated militia.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the individual right argue that the founding problem was always about individual liberty and self-defense, citing historical texts and framers' intent. Opponents (collective/civic republican advocates, public health experts) argue the original intent was primarily about militia service, and the individual right reading has expanded beyond the founding problem; historical scholarship and legal analysis from outside the benefiting parties support the contested nature of the founding problem's status.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high because this reading significantly limits the ability of federal and state governments to enact firearm regulations, imposing substantial costs on public safety efforts. Suppression (0.6) is also high, as the legal and political machinery actively suppresses alternative interpretations and regulatory attempts. Resistance (0.8) is very high, reflecting the intense political and legal opposition to any perceived infringement on this right. The claimed type 'tangled_rope' acknowledges the coordination function (protecting individual liberty) alongside the asymmetric extraction from regulatory bodies. The temporal measurements show a clear trend of increasing extractiveness and suppression over the past 40 years, reflecting the growing dominance of this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individual gun owners, this constraint is a 'rope' or even a 'mountain' – a fundamental, unalienable right that coordinates their liberty. From the perspective of federal and state regulatory agencies, it operates as a 'snare' or 'tangled_rope', extracting their ability to govern and imposing costs on public safety. The engine's classification will reflect this divergence based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry are clear beneficiaries, experiencing low directionality as the constraint subsidizes their interests. Federal and state regulatory agencies are targets, experiencing high directionality as their legislative and enforcement powers are curtailed. Advocates for collective or civic republican rights are structurally excluded, their positions marginalized by the ascendance of this individual rights reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protecting individual liberty) is still live for its beneficiaries, but its function has arguably drifted from the original intent of a 'well-regulated militia' to a broader individual right. This prevents mislabeling it as a pure snare by acknowledging the genuine coordination of individual liberty, while still highlighting the extractive nature of its impact on regulatory capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_ambiguity,
    'Was the original intent of the Second Amendment primarily to protect an individual right to self-defense, or a collective right related to militia service?',
    'Further historical and legal scholarship, potentially new textual discoveries or a definitive Supreme Court ruling that re-evaluates historical evidence.',
    'If a collective right intent were definitively established, the individual rights reading would be weakened, potentially reducing its extractiveness on regulatory bodies. If individual right intent were further solidified, regulatory efforts would face even greater suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_intent_ambiguity, empirical, 'Ambiguity regarding the original intent of the Second Amendment.').

omega_variable(
    public_safety_cost_quantification,
    'What is the quantifiable cost (e.g., in lives, healthcare, economic impact) of the regulatory limitations imposed by this individual rights reading?',
    'Comprehensive, longitudinal public health and economic studies, potentially mandated by legislative bodies, to assess the impact of firearm availability and regulatory gaps.',
    'Clear quantification of high costs would strengthen arguments for re-interpreting or amending the Second Amendment, potentially shifting the balance of power towards regulatory bodies. Low or unquantifiable costs would support the current reading''s proponents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_safety_cost_quantification, empirical, 'Quantification of public safety costs due to regulatory limitations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__individual_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t10, second_amendment_arms_right__individual_right_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(seco_tr_t20, second_amendment_arms_right__individual_right_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(seco_tr_t30, second_amendment_arms_right__individual_right_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(seco_tr_t40, second_amendment_arms_right__individual_right_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__individual_right_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(seco_be_t10, second_amendment_arms_right__individual_right_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(seco_be_t20, second_amendment_arms_right__individual_right_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(seco_be_t30, second_amendment_arms_right__individual_right_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(seco_be_t40, second_amendment_arms_right__individual_right_reading, base_extractiveness, 40, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__individual_right_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(seco_su_t10, second_amendment_arms_right__individual_right_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(seco_su_t20, second_amendment_arms_right__individual_right_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(seco_su_t30, second_amendment_arms_right__individual_right_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(seco_su_t40, second_amendment_arms_right__individual_right_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, gun_control_legislation).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, federal_preemption_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'second_amendment_arms_right' kernel. It coexists with and influences other readings, such as the 'collective_right_reading' and 'civic_republican_reading', by shaping the legal and political landscape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
