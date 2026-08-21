% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: UDHR Article 3: Procedural Hybrid Reading (Due Process Protections)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents a 'procedural hybrid' reading of Article 3 of
 *   the Universal Declaration of Human Rights (UDHR), which guarantees the
 *   right to life, liberty, and security of person. This reading emphasizes
 *   procedural protections like habeas corpus and the prohibition of torture,
 *   without explicitly resolving the debate over whether Article 3 implies
 *   substantive positive entitlements (e.g., to welfare or healthcare). It
 *   functions as a Tangled Rope, coordinating states to a minimum standard of
 *   conduct while extracting from those states that would prefer to operate
 *   with fewer constraints on their power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.6).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.7).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "UDHR Article 3: Procedural Hybrid Reading (Due Process Protections)").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, 'd9cb817f-bbaa-4dc4-a542-d6da95325cbd').
narrative_ontology:cs_kernel_codification('d9cb817f-bbaa-4dc4-a542-d6da95325cbd', fixed_text).
narrative_ontology:cs_authority_grounding('d9cb817f-bbaa-4dc4-a542-d6da95325cbd', lineage).
narrative_ontology:cs_interpretation_layer_present('d9cb817f-bbaa-4dc4-a542-d6da95325cbd').
narrative_ontology:cs_reading_relation('d9cb817f-bbaa-4dc4-a542-d6da95325cbd', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9cb817f-bbaa-4dc4-a542-d6da95325cbd', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('d9cb817f-bbaa-4dc4-a542-d6da95325cbd', foundational, procedural_justice_is_foundational).
narrative_ontology:cs_axiom_status(procedural_justice_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('d9cb817f-bbaa-4dc4-a542-d6da95325cbd', procedural_justice_is_foundational, deontological).
narrative_ontology:cs_axiom('d9cb817f-bbaa-4dc4-a542-d6da95325cbd', secondary, state_action_must_be_constrained_by_law).
narrative_ontology:cs_axiom_status(state_action_must_be_constrained_by_law, holdable).
narrative_ontology:cs_axiom_grounding('d9cb817f-bbaa-4dc4-a542-d6da95325cbd', state_action_must_be_constrained_by_law, conventional).
narrative_ontology:cs_reference_frame('d9cb817f-bbaa-4dc4-a542-d6da95325cbd', post_wwii_human_rights_consensus).
narrative_ontology:cs_drift_state('d9cb817f-bbaa-4dc4-a542-d6da95325cbd', contemporary_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d9cb817f-bbaa-4dc4-a542-d6da95325cbd', '').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, individuals).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, human_rights_advocates).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, states_seeking_unfettered_power).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, security_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive fundamental protections against arbitrary deprivation of life, liberty, and security of person, including safeguards like habeas corpus and prohibition of torture. Their ability to exercise these rights depends on state adherence and enforcement mechanisms.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, individuals, beneficiary,
    powerless, biographical, trapped, global).

% Are constrained by the requirement to provide due process, refrain from torture, and ensure legal avenues for challenging detention. This extracts from their ability to act arbitrarily or prioritize security over individual rights without legal oversight.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, states_seeking_unfettered_power, payer,
    institutional, generational, constrained, global).

% Utilize Article 3 as a foundational legal and moral standard to monitor state compliance, document violations, and advocate for stronger protections. They benefit from the existence of this clear procedural standard.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, human_rights_advocates, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, human_rights_advocates, observer).

% Interpret and apply Article 3, setting precedents and monitoring state compliance. They serve as a crucial enforcement mechanism, though their jurisdiction and power are often limited by state sovereignty.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, international_courts_and_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Are directly constrained in their operations by due process requirements, prohibitions on torture, and the need for legal justification for detention. This extracts from their operational flexibility, requiring adherence to legal frameworks.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, security_agencies, payer,
    institutional, immediate, constrained, national).

% Believe Article 3 should be interpreted to include positive entitlements to welfare, healthcare, or housing as necessary for life and security. They are excluded from this reading's focus on purely procedural safeguards, but continue to advocate for their interpretation.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, proponents_of_substantive_rights, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal minimum standard for state conduct regarding the life, liberty, and security of person, preventing arbitrary state action and providing a common framework for legal challenge.
% TRANSFER_FUNCTION: Transfers a degree of power from states (to act arbitrarily) to individuals (to demand due process and legal safeguards); it also transfers legitimacy to states that adhere to these standards.
% ABSENT_VOICES: Those who advocate for an expansive reading of Article 3 to include positive socio-economic entitlements are excluded from this procedural focus. Also, proponents of absolute state sovereignty or unfettered security powers would object to any external limitation.
% DISAPPEARANCE_RATIONALE: If Article 3's procedural guarantees vanished, it would remove a fundamental legal and moral bulwark against arbitrary state power. This would likely lead to a significant increase in arbitrary detention, torture, and extrajudicial killings, fundamentally reorganizing international human rights law and state-citizen relations.
% FOUNDING_PROBLEM: The widespread atrocities, arbitrary detention, and torture witnessed during World War II, which demonstrated the urgent need for fundamental, non-derogable human rights to protect individuals from state abuses.
% FOUNDING_PROBLEM_CORROBORATION: Numerous reports from UN human rights bodies, Amnesty International, Human Rights Watch, and academic studies consistently document ongoing violations of due process and prohibitions against torture globally, corroborating that the founding problem remains live.
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__procedural_hybrid_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.60) because it limits state power to act arbitrarily, but does not impose extensive positive obligations. Suppression is high (0.70) as states often resist external oversight and limitations on their security apparatus, requiring active enforcement by international and national bodies. The theater ratio is moderate (0.40) reflecting that while many states genuinely adhere to these principles, others engage in performative compliance or seek loopholes, particularly in times of perceived crisis. Resistance is also moderate (0.60) as states and security agencies frequently push back against strict interpretations or external monitoring.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals and human rights advocates, Article 3 is a vital 'Rope' that coordinates states to uphold fundamental dignity. From the perspective of states prioritizing security or sovereignty, it can feel like a 'Snare' that unduly restricts their operational capacity. This reading, as a Tangled Rope, acknowledges both the coordination function and the asymmetric extraction from state power.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals are clear beneficiaries, gaining protection from arbitrary state action. Human rights advocates also benefit by having a clear standard to leverage. States seeking unfettered power and their security agencies are the primary targets/payers, as the constraint extracts from their ability to operate without legal oversight. International courts and bodies act as agenda-setters, interpreting and enforcing the constraint. Proponents of substantive rights are 'excluded' from this specific reading's scope, as their interpretation is not the focus here.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantive_vs_procedural_scope,
    'Does Article 3 of the UDHR implicitly or explicitly require states to provide positive entitlements (e.g., welfare, healthcare, housing) necessary for life and security, or is its scope limited to procedural protections against arbitrary state action?',
    'Further development of international jurisprudence, state practice, and scholarly consensus on the interpretation of ''security of person'' and ''life'' in Article 3.',
    'If resolved towards positive entitlements, the constraint''s extractiveness from states would significantly increase, and its classification might shift towards a more robust Tangled Rope or even Snare for states, while expanding benefits for individuals. If resolved strictly procedurally, the current classification holds, but the ''excluded'' stakeholders'' claims remain unaddressed by this specific article.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantive_vs_procedural_scope, conceptual, 'Ambiguity regarding the substantive vs. procedural scope of Article 3.').

omega_variable(
    state_adherence_vs_circumvention,
    'To what extent do states genuinely adhere to the procedural protections of Article 3, versus engaging in performative compliance or finding legal/extra-legal means to circumvent them, particularly in contexts of national security or emergency?',
    'Longitudinal empirical studies of state practice, judicial review outcomes, and independent human rights monitoring reports across diverse jurisdictions and political contexts.',
    'If widespread circumvention is revealed, the constraint''s effective suppression and theater_ratio would be higher than currently estimated, potentially pushing it closer to a Piton (if function atrophies) or a Snare (if extraction through arbitrary detention becomes more pronounced).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_adherence_vs_circumvention, empirical, 'Gap between declared adherence and actual state practice regarding Article 3.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(udhr_tr_t1965, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1965, 0.25).
narrative_ontology:measurement(udhr_tr_t1980, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(udhr_tr_t1995, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(udhr_tr_t2010, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(udhr_tr_t2023, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2023, 0.4).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1948, 0.45).
narrative_ontology:measurement(udhr_be_t1965, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(udhr_be_t1980, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(udhr_be_t1995, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(udhr_be_t2010, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(udhr_be_t2023, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2023, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement(udhr_su_t1965, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement(udhr_su_t1980, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(udhr_su_t1995, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(udhr_su_t2010, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(udhr_su_t2023, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2023, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, iccpr_article_9__due_process_rights).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, cat_prohibition_of_torture).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the UDHR Article 3 kernel, focusing on procedural safeguards. It coexists with a 'negative_liberty_reading' (emphasizing freedom from state interference) and a 'positive_entitlement_reading' (emphasizing state provision of welfare), which are distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
