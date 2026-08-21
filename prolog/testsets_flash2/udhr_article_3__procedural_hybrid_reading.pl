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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: UDHR Article 3: Procedural Hybrid Reading (Due Process Protections)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents a 'procedural hybrid' reading of Article 3 of
 *   the Universal Declaration of Human Rights (UDHR), which guarantees
 *   fundamental due process protections such as habeas corpus and the
 *   prohibition of torture. This reading focuses on the 'how' of state
 *   action, ensuring fair process, without explicitly resolving the broader
 *   philosophical debates about the 'what' of liberty (negative freedom from
 *   interference vs. positive entitlement to welfare). It is one reading of
 *   the 'udhr_article_3' kernel, coexisting with both
 *   negative_liberty_reading and positive_entitlement_reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.35).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.45).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "UDHR Article 3: Procedural Hybrid Reading (Due Process Protections)").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, '444a1a23-fba7-4532-8424-fc40153f979a').
narrative_ontology:cs_kernel_codification('444a1a23-fba7-4532-8424-fc40153f979a', fixed_text).
narrative_ontology:cs_authority_grounding('444a1a23-fba7-4532-8424-fc40153f979a', lineage).
narrative_ontology:cs_interpretation_layer_present('444a1a23-fba7-4532-8424-fc40153f979a').
narrative_ontology:cs_reading_relation('444a1a23-fba7-4532-8424-fc40153f979a', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('444a1a23-fba7-4532-8424-fc40153f979a', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('444a1a23-fba7-4532-8424-fc40153f979a', foundational, due_process_is_foundational_to_liberty).
narrative_ontology:cs_axiom_status(due_process_is_foundational_to_liberty, holdable).
narrative_ontology:cs_axiom_grounding('444a1a23-fba7-4532-8424-fc40153f979a', due_process_is_foundational_to_liberty, deontological).
narrative_ontology:cs_axiom('444a1a23-fba7-4532-8424-fc40153f979a', foundational, procedural_justice_precedes_substantive_resolution).
narrative_ontology:cs_axiom_status(procedural_justice_precedes_substantive_resolution, holdable).
narrative_ontology:cs_axiom_grounding('444a1a23-fba7-4532-8424-fc40153f979a', procedural_justice_precedes_substantive_resolution, conventional).
narrative_ontology:cs_reference_frame('444a1a23-fba7-4532-8424-fc40153f979a', post_wwii_procedural_consensus).
narrative_ontology:cs_drift_state('444a1a23-fba7-4532-8424-fc40153f979a', contemporary_counter_terrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('444a1a23-fba7-4532-8424-fc40153f979a', '').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, individuals_facing_state_detention).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, human_rights_advocates).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, habeas_corpus_principle).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, prohibition_of_torture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly benefits from the procedural safeguards against arbitrary detention and torture, ensuring a minimum standard of humane treatment and legal recourse. Their ability to exercise these rights is often dependent on external advocacy.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, individuals_facing_state_detention, beneficiary,
    powerless, immediate, trapped, global).

% Utilize this reading to challenge state practices that violate due process, providing a legal and moral framework for their work. They benefit from the clarity of procedural guarantees, even when substantive rights are debated.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, human_rights_advocates, beneficiary,
    organized, generational, constrained, global).

% Are bound by the procedural guarantees of Article 3, requiring them to implement legal frameworks that prevent arbitrary detention and torture. They face international scrutiny and legal challenges if they fail to uphold these standards.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, states_parties_to_udhr, agenda_setter,
    institutional, generational, constrained, global).

% Interpret and enforce Article 3, particularly its procedural aspects. They provide a forum for redress and contribute to the evolving understanding of these protections, influencing state behavior through jurisprudence and recommendations.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, international_courts_and_bodies, agenda_setter,
    institutional, civilizational, analytical, global).

% View this reading as a necessary but insufficient step towards their broader goal of limiting state power and ensuring freedom from interference. They support procedural protections but argue for a more expansive interpretation of 'liberty'.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, negative_liberty_advocates, observer,
    organized, generational, mobile, global).

% See this reading as a foundational layer for human dignity, but argue it falls short by not explicitly mandating state provision of welfare and material conditions. They support procedural protections but push for a more robust 'right to life' that includes socio-economic rights.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, positive_entitlement_advocates, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal minimum standard for state conduct regarding detention and treatment of individuals, providing a common framework for international human rights law and advocacy.
% TRANSFER_FUNCTION: Transfers the burden of proof and justification for detention onto the state, and prohibits the use of torture, thereby protecting individuals from arbitrary state power.
% ABSENT_VOICES: Individuals subject to secret detention or extrajudicial rendition are often entirely absent from the conversation, their procedural rights systematically denied. Their voices would highlight the gaps in enforcement and the need for stronger accountability mechanisms.
% DISAPPEARANCE_RATIONALE: If this procedural reading of Article 3 vanished, states would face fewer constraints on arbitrary detention and coercive interrogation, leading to a significant increase in human rights abuses and a collapse of international legal norms around due process. The global human rights architecture would be fundamentally undermined.
% FOUNDING_PROBLEM: The widespread atrocities and arbitrary deprivations of life and liberty during World War II, necessitating a universal declaration of fundamental human rights and legal protections.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, international legal scholars, and victims of state abuses consistently corroborate that the problem of arbitrary detention and torture remains live, despite the existence of Article 3. Reports from the UN Human Rights Committee and Amnesty International provide ongoing evidence.
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__procedural_hybrid_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).
:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because while states are constrained, the procedural nature means it doesn't directly mandate resource allocation, thus limiting direct extraction from states. Suppression (0.45) reflects the ongoing need for active enforcement and advocacy to ensure states adhere to these protections, as violations still occur. Theater ratio is low (0.15) because the core prohibitions (torture, arbitrary detention) are clear and widely accepted, even if sometimes violated; the performance is less about obscuring function and more about maintaining a baseline norm. The metrics show a slight decrease in extractiveness and suppression over time as international norms strengthened, with a minor uptick in recent years due to new challenges.
 *
 * PERSPECTIVAL GAP:
 *   States, particularly those with strong national sovereignty claims, may perceive the constraint as more extractive due to perceived limitations on their internal affairs, while individuals and advocates see it as a fundamental protection. The engine's per-seat classification will reflect these differing structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals facing state detention and human rights advocates are the primary beneficiaries, as the constraint provides them with legal tools and a normative framework. States parties are the agenda-setters, responsible for implementation and enforcement, bearing the cost of compliance. International courts act as institutional agenda-setters. There are no direct 'victims' of this reading, as its purpose is protective; any 'extraction' is from state power, not from individuals. Advocates of other readings (negative/positive liberty) are observers, supporting the procedural aspects but pushing for their own broader interpretations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantive_vs_procedural_scope,
    'To what extent can procedural protections effectively guarantee ''life, liberty and security of person'' without explicit substantive guarantees of welfare or freedom from non-state actors?',
    'Empirical studies comparing human rights outcomes in jurisdictions with strong procedural but weak substantive rights vs. those with both. Conceptual analysis of the interdependency of rights.',
    'If procedural protections are found insufficient, it would strengthen the case for the ''positive_entitlement_reading'' or a more expansive ''negative_liberty_reading'', potentially reclassifying this reading as a ''scaffold'' for a more comprehensive approach.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantive_vs_procedural_scope, conceptual, 'Ambiguity regarding the sufficiency of procedural guarantees alone.').

omega_variable(
    enforcement_gap_vs_normative_strength,
    'Is the observed ''suppression'' due to inherent state resistance to human rights norms, or a lack of effective enforcement mechanisms at the international level?',
    'Analysis of state compliance patterns in response to varying enforcement pressures (e.g., sanctions, judicial rulings) versus changes in domestic political will or normative shifts.',
    'If primarily due to state resistance, the constraint''s ''suppression'' is a direct measure of ongoing contestation. If due to weak enforcement, the normative strength of the reading is higher than its observed suppression suggests, indicating a ''rope'' that is under-enforced rather than inherently weak.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_vs_normative_strength, empirical, 'Distinguishing between state resistance and enforcement capacity as drivers of suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(udhr_tr_t1960, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(udhr_tr_t1980, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(udhr_tr_t2000, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(udhr_tr_t2010, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(udhr_tr_t2024, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(udhr_be_t1960, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1960, 0.38).
narrative_ontology:measurement(udhr_be_t1980, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(udhr_be_t2000, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement(udhr_be_t2010, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2010, 0.34).
narrative_ontology:measurement(udhr_be_t2024, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(udhr_su_t1960, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1960, 0.48).
narrative_ontology:measurement(udhr_su_t1980, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(udhr_su_t2000, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(udhr_su_t2010, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2010, 0.44).
narrative_ontology:measurement(udhr_su_t2024, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, iccpr_article_9__arbitrary_detention).

% DUAL FORMULATION NOTE:
% This is one of three readings of UDHR Article 3, focusing on procedural protections. It influences and coexists with the negative liberty and positive entitlement readings, as all contribute to the overall interpretation of the article.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
