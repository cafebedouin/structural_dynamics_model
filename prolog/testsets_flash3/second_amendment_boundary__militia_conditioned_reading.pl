% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__militia_conditioned_reading, []).

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
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Second Amendment: Militia-Conditioned Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'militia-conditioned' reading of the
 *   Second Amendment, where the prefatory clause ('A well regulated Militia,
 *   being necessary to the security of a free State') defines the scope of
 *   the operative clause ('the right of the people to keep and bear Arms') to
 *   a collective defense context. This interpretation permits comprehensive
 *   regulation of firearms, presuming state regulatory authority as
 *   legitimate. It is one reading of the broader 'second_amendment_boundary'
 *   kernel, which also includes individual_right_reading and
 *   insurrectionist_reading. The constraint is classified as a Tangled Rope
 *   because it genuinely coordinates public safety with a regulated right,
 *   but also extracts from gun owners through restrictions and requires
 *   active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.45).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.6).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Second Amendment: Militia-Conditioned Reading").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, '00deabee-8ed4-449e-bebe-ee0c26b1b04a').
narrative_ontology:cs_kernel_codification('00deabee-8ed4-449e-bebe-ee0c26b1b04a', fixed_text).
narrative_ontology:cs_authority_grounding('00deabee-8ed4-449e-bebe-ee0c26b1b04a', lineage).
narrative_ontology:cs_interpretation_layer_present('00deabee-8ed4-449e-bebe-ee0c26b1b04a').
narrative_ontology:cs_reading_relation('00deabee-8ed4-449e-bebe-ee0c26b1b04a', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('00deabee-8ed4-449e-bebe-ee0c26b1b04a', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('00deabee-8ed4-449e-bebe-ee0c26b1b04a', foundational, militia_clause_defines_scope).
narrative_ontology:cs_axiom_status(militia_clause_defines_scope, holdable).
narrative_ontology:cs_axiom_grounding('00deabee-8ed4-449e-bebe-ee0c26b1b04a', militia_clause_defines_scope, conventional).
narrative_ontology:cs_axiom('00deabee-8ed4-449e-bebe-ee0c26b1b04a', secondary, public_safety_justifies_regulation).
narrative_ontology:cs_axiom_status(public_safety_justifies_regulation, holdable).
narrative_ontology:cs_axiom_grounding('00deabee-8ed4-449e-bebe-ee0c26b1b04a', public_safety_justifies_regulation, instrumental).
narrative_ontology:cs_reference_frame('00deabee-8ed4-449e-bebe-ee0c26b1b04a', collective_right_regulatory_framework).
narrative_ontology:cs_drift_state('00deabee-8ed4-449e-bebe-ee0c26b1b04a', contemporary_judicial_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('00deabee-8ed4-449e-bebe-ee0c26b1b04a', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_regulatory_authorities).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, public_safety_advocates).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, gun_owners_restricted_by_regulation).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These authorities interpret the Second Amendment as permitting comprehensive regulation of firearms, viewing the right as primarily tied to militia service. They enact and enforce laws restricting types of arms, capacities, and conditions of ownership, believing this enhances public safety. Their legitimacy is derived from this interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_regulatory_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for stricter gun control measures, seeing this reading as essential for reducing gun violence and promoting community safety. They benefit from the legal framework that allows for such regulations, as it aligns with their policy goals.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, public_safety_advocates, beneficiary,
    organized, biographical, mobile, national).

% Individuals who own firearms for collection, sport, or self-defense but face restrictions on the types of weapons they can possess, where they can carry them, or how they must store them, due to regulations enacted under this interpretation. They bear the costs of compliance and reduced access.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_owners_restricted_by_regulation, payer,
    moderate, biographical, constrained, local).

% Companies that produce firearms and related accessories. They face restrictions on the types of products they can sell, the features they can include, and the markets they can access, impacting their business models and profitability. They bear economic costs from regulatory compliance and market contraction.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_manufacturers, payer,
    powerful, biographical, constrained, national).

% Advocate for an individual right to bear arms, independent of militia service, and oppose comprehensive regulation. Under this reading, their arguments are marginalized or dismissed in legal and policy discourse, effectively excluding their preferred interpretation from being enacted.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, individual_rights_advocates, excluded,
    organized, generational, trapped, national).

% Analyze the historical context, textual meaning, and legal precedents of the Second Amendment. They provide academic interpretations that inform, but do not directly control, the policy and legal debates surrounding firearms regulation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of the right to bear arms with the state's interest in public safety, by defining the scope of the right in a way that permits extensive regulation. This allows for a balance between individual liberty and collective security, as interpreted by the state.
% TRANSFER_FUNCTION: Transfers regulatory authority over firearms from individual discretion to state and federal governments. It transfers the burden of proof for the legitimacy of gun ownership from the state to the individual, and transfers the cost of compliance to gun owners and manufacturers.
% ABSENT_VOICES: Advocates for an individual, unrestricted right to bear arms are largely excluded from the policy-making process under this reading, as their core premise is deemed inconsistent with the militia-conditioned interpretation. They would argue for minimal regulation and a broad individual right.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal landscape for firearms would dramatically shift. State regulatory authority would be severely curtailed, leading to a rapid deregulation of firearms. This would likely result in increased gun ownership, changes in public safety outcomes, and a complete re-evaluation of the Second Amendment's meaning, forcing a rearrangement of legal and social structures.
% FOUNDING_PROBLEM: The founding problem was how to balance the need for a citizen militia to secure a free state with concerns about individual arms possession and potential misuse, particularly in a post-revolutionary context where state power was viewed with suspicion.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars outside of direct advocacy groups corroborate that the tension between collective security and individual liberty regarding arms remains a live and contested issue, reflecting ongoing societal debates about public safety and constitutional rights.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__militia_conditioned_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).
:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because while it restricts some gun owners, it also provides a framework for public safety, which is a collective benefit. Suppression (0.6) is significant as it requires active legal and enforcement mechanisms to restrict firearms and suppress alternative interpretations. The accessibility collapse (0.4) is moderate; while some alternatives (e.g., unrestricted gun ownership) are curtailed, other forms of gun ownership and self-defense remain possible within the regulatory framework. Resistance (0.7) is high, reflecting ongoing legal and political challenges from those who advocate for a broader individual right. The theater ratio (0.1) is low, as the regulatory actions are generally functional rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state regulatory authorities, this reading is a legitimate and necessary coordination mechanism for public safety. From the perspective of restricted gun owners, it is an extractive constraint that limits their rights. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory authorities and public safety advocates are beneficiaries, as this reading empowers their policy goals and provides a legal basis for their actions. Gun owners restricted by regulation and firearms manufacturers are payers, bearing the costs of compliance and market restrictions. Individual rights advocates are excluded, as their core interpretive framework is marginalized by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_intent_ambiguity,
    'To what extent does historical evidence definitively support a militia-conditioned interpretation of the Second Amendment''s original intent?',
    'Further historical and linguistic analysis of founding-era documents, debates, and legal practices, with consensus among non-partisan historians.',
    'Strong historical corroboration would bolster the legitimacy of this reading, potentially reducing resistance. Weak or contested evidence would expose it more clearly as a policy choice rather than an original constitutional mandate, increasing its perceived extractiveness for payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_intent_ambiguity, empirical, 'Ambiguity regarding the original intent of the Second Amendment''s framers.').

omega_variable(
    public_safety_efficacy_ambiguity,
    'Does comprehensive firearms regulation, as permitted by this reading, demonstrably lead to a significant reduction in gun violence and enhancement of public safety?',
    'Longitudinal empirical studies comparing public safety outcomes in jurisdictions with varying levels of regulation, controlling for confounding factors.',
    'Clear evidence of efficacy would strengthen the coordination function and reduce perceived extractiveness for beneficiaries. Lack of clear evidence would weaken the justification for the restrictions, increasing perceived extractiveness for payers and potentially shifting the classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_safety_efficacy_ambiguity, empirical, 'Uncertainty about the causal link between regulation and public safety outcomes.').

omega_variable(
    framing_underdetermination_militia_vs_individual,
    'Is the ''militia-conditioned'' framing the only defensible interpretation of the Second Amendment, or is the ''individual-right'' framing equally coherent?',
    'Conceptual analysis of constitutional text and structure, and judicial precedent. The existence of a robust, internally consistent alternative framing (e.g., the individual_right_reading) would confirm under-determination.',
    'If the individual-right framing is equally coherent, this constraint''s classification as a Tangled Rope (with its coordination function) becomes more contested, as its legitimacy rests on a specific interpretive choice rather than an unambiguous constitutional mandate. This would increase the perceived suppression for individual_rights_advocates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_militia_vs_individual, conceptual, 'The choice between a militia-conditioned and an individual-right interpretation is under-determined by the text alone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(seco_be_t10, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(seco_be_t20, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(seco_be_t30, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(seco_be_t40, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(seco_be_t50, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(seco_su_t10, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(seco_su_t20, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(seco_su_t30, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 30, 0.59).
narrative_ontology:measurement(seco_su_t40, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(seco_su_t50, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, firearms_market_regulation).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, public_safety_legislation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'second_amendment_boundary' kernel. Its classification as a Tangled Rope (coordination + extraction) is distinct from the other readings, which may classify differently based on their structural properties and beneficiary/victim sets. All three readings are linked in the network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
