% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment: Individual Right to Firearms Ownership
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the 'individual right' reading of the Second
 *   Amendment, which interprets the right to keep and bear arms as belonging
 *   to individuals for self-defense, unconnected to militia service. This
 *   reading, solidified by Supreme Court decisions like D.C. v. Heller
 *   (2008), significantly constrains state and local governments' ability to
 *   regulate firearms, benefiting gun owners and the firearms industry while
 *   imposing costs on public safety and victims of gun violence. The
 *   constraint is claimed as a 'tangled_rope' because it provides a
 *   coordination function (individual self-defense) but also involves
 *   significant asymmetric extraction (costs borne by those seeking gun
 *   control and victims of violence) and requires active enforcement by the
 *   judiciary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.75).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment: Individual Right to Firearms Ownership").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, 'ba34deeb-b5a1-4711-aec8-31fe48298109').
narrative_ontology:cs_kernel_codification('ba34deeb-b5a1-4711-aec8-31fe48298109', fixed_text).
narrative_ontology:cs_authority_grounding('ba34deeb-b5a1-4711-aec8-31fe48298109', lineage).
narrative_ontology:cs_interpretation_layer_present('ba34deeb-b5a1-4711-aec8-31fe48298109').
narrative_ontology:cs_reading_relation('ba34deeb-b5a1-4711-aec8-31fe48298109', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('ba34deeb-b5a1-4711-aec8-31fe48298109', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('ba34deeb-b5a1-4711-aec8-31fe48298109', foundational, individual_right_to_self_defense).
narrative_ontology:cs_axiom_status(individual_right_to_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('ba34deeb-b5a1-4711-aec8-31fe48298109', individual_right_to_self_defense, deontological).
narrative_ontology:cs_axiom('ba34deeb-b5a1-4711-aec8-31fe48298109', foundational, militia_clause_is_prefatory).
narrative_ontology:cs_axiom_status(militia_clause_is_prefatory, holdable).
narrative_ontology:cs_axiom_grounding('ba34deeb-b5a1-4711-aec8-31fe48298109', militia_clause_is_prefatory, conventional).
narrative_ontology:cs_reference_frame('ba34deeb-b5a1-4711-aec8-31fe48298109', post_heller_individual_right).
narrative_ontology:cs_drift_state('ba34deeb-b5a1-4711-aec8-31fe48298109', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ba34deeb-b5a1-4711-aec8-31fe48298109', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_manufacturers_and_retailers).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_and_local_governments).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, victims_of_gun_violence).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, public_safety_advocates).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, individual_liberty_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, self_defense_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who assert a fundamental right to own firearms for any lawful purpose, including self-defense, without connection to militia service. They benefit from the constraint's broad protection of gun ownership and resist any attempts at regulation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Businesses that profit from the sale of firearms and related accessories. They benefit directly from the expansive interpretation of the Second Amendment, which creates a large and relatively unregulated market for their products. They actively lobby against restrictions.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_manufacturers_and_retailers, beneficiary,
    institutional, generational, arbitrage, national).

% Government entities tasked with public safety and welfare. They bear the costs of increased gun violence and are constrained in their ability to enact regulations to address it due to judicial interpretations of the individual right. Their options are limited to narrow, judicially approved restrictions.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_and_local_governments, payer,
    institutional, generational, constrained, national).

% Individuals and communities directly affected by gun violence. They bear the ultimate human cost of the expansive individual right, experiencing injury, death, and trauma. Their ability to influence policy is often diffuse and outmatched by organized gun rights groups.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, victims_of_gun_violence, payer,
    powerless, immediate, trapped, local).

% Organizations and individuals who advocate for stricter gun control measures to reduce violence. They bear the costs of legislative and legal battles against the individual right interpretation and face significant political and legal hurdles in their efforts.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, public_safety_advocates, payer,
    organized, generational, constrained, national).

% The ultimate arbiter of constitutional meaning. Its rulings (e.g., Heller, McDonald) established and reinforced the individual right interpretation, setting precedents that constrain legislative action and lower court decisions. It actively enforces this reading through judicial review.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, universal).

% Scholars and activists who argue the Second Amendment primarily protects a state's right to maintain a militia, not an individual's right to own guns for any purpose. Their interpretation has been largely sidelined by the dominant individual right reading.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, collective_right_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for individual self-defense and resistance against potential tyranny, ensuring citizens are armed. It coordinates the expectation of individual firearm access across the nation.
% TRANSFER_FUNCTION: Transfers the right to bear arms from a state-centric, militia-focused context to a broad individual right, effectively transferring regulatory power away from state and local governments and towards individual gun owners and the firearms industry.
% ABSENT_VOICES: Advocates for a collective or civic right interpretation of the Second Amendment are largely excluded from the dominant legal and political discourse, as their arguments are often dismissed by the prevailing individual right framework. Victims of gun violence, while present, often lack the organized power to effectively counter the beneficiaries.
% DISAPPEARANCE_RATIONALE: If the individual right interpretation vanished overnight, state and local governments would immediately move to enact stricter gun control laws, the firearms industry would face significant market contraction, and the legal landscape around gun ownership would fundamentally shift, leading to a complete reorganization of gun policy and culture.
% FOUNDING_PROBLEM: The Second Amendment was established to ensure the security of a free state by allowing for the existence of a well-regulated militia, and to protect the right of the people to keep and bear arms.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court (in Heller) and gun rights advocates attest that the individual right to self-defense is a live and central aspect of the founding problem. Public safety advocates and some legal historians, however, argue that the original intent was primarily tied to militia service, and that the individual right interpretation has superseded the original problem, leading to a 'dead' or 'transformed' founding problem.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high because the broad individual right interpretation limits the ability of governments to address the societal costs of widespread firearm availability. Suppression (0.75) is also high, as state and local governments are actively suppressed by judicial precedent from enacting desired regulations. The theater ratio (0.20) is relatively low, as the judicial enforcement of this right is a genuine, active function, not merely performative. Accessibility collapse (0.45) is moderate, as while legislative alternatives are constrained, advocacy and legal challenges persist. Resistance (0.70) is high, reflecting ongoing efforts by public safety advocates and governments to challenge the scope of this right.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of firearms owners, this constraint is a fundamental protection of liberty (a 'rope' or even a 'mountain'). From the perspective of public safety advocates and victims of gun violence, it operates as a 'snare' or 'tangled_rope' that extracts immense societal costs. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Firearms owners and the firearms industry are clear beneficiaries (d near 0.0) as the constraint protects and expands their interests. State and local governments, victims of gun violence, and public safety advocates are targets (d near 1.0) as they bear the costs and are constrained by the interpretation. The Supreme Court acts as the agenda-setter, actively enforcing this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_relevance,
    'To what extent does the ''well regulated Militia'' clause of the Second Amendment still inform or constrain the individual right to bear arms?',
    'Further Supreme Court jurisprudence explicitly addressing the relationship between the two clauses, or historical scholarship uncovering new evidence of original intent.',
    'If the militia clause is found to have significant contemporary relevance, it could reintroduce conditions on the individual right, reducing its extractiveness and suppression. If it is definitively deemed vestigial, the individual right reading would be further solidified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_clause_relevance, conceptual, 'Ambiguity regarding the ''well regulated Militia'' clause''s impact on the individual right.').

omega_variable(
    societal_cost_quantification,
    'What is the full societal cost (economic, health, social) of the expansive individual right to bear arms, and how does it compare to the perceived benefits of self-defense?',
    'Comprehensive, longitudinal epidemiological and economic studies on gun violence, public health impacts, and the efficacy of self-defense in various contexts.',
    'Clear quantification of disproportionate costs could shift public and judicial preference, potentially leading to a re-evaluation of the constraint''s balance and reducing its perceived legitimacy. If benefits are found to outweigh costs, the current reading would be reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(societal_cost_quantification, empirical, 'Uncertainty in the precise quantification of societal costs versus benefits.').

omega_variable(
    judicial_activism_vs_interpretation,
    'Is the individual right reading a legitimate interpretation of the Second Amendment''s text and history, or an instance of judicial activism that created a new right?',
    'Ongoing legal scholarship, historical analysis, and shifts in judicial philosophy over generations. No definitive empirical resolution is likely.',
    'If widely seen as judicial activism, the legitimacy of the constraint could erode, increasing resistance and potentially leading to legislative or constitutional challenges. If affirmed as legitimate interpretation, its stability would increase.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_activism_vs_interpretation, conceptual, 'Debate over the legitimacy of the individual right interpretation as a product of judicial interpretation versus activism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_scope__individual_right_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(seco_tr_t1985, second_amendment_scope__individual_right_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_scope__individual_right_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__individual_right_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_scope__individual_right_reading, theater_ratio, 2016, 0.19).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_scope__individual_right_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_scope__individual_right_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(seco_be_t1985, second_amendment_scope__individual_right_reading, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(seco_be_t2000, second_amendment_scope__individual_right_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__individual_right_reading, base_extractiveness, 2008, 0.65).
narrative_ontology:measurement(seco_be_t2016, second_amendment_scope__individual_right_reading, base_extractiveness, 2016, 0.67).
narrative_ontology:measurement(seco_be_t2024, second_amendment_scope__individual_right_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_scope__individual_right_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(seco_su_t1985, second_amendment_scope__individual_right_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(seco_su_t2000, second_amendment_scope__individual_right_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__individual_right_reading, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement(seco_su_t2016, second_amendment_scope__individual_right_reading, suppression_requirement, 2016, 0.73).
narrative_ontology:measurement(seco_su_t2024, second_amendment_scope__individual_right_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, gun_control_legislation_constraint).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, police_use_of_force_doctrine).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
