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
 *   human_readable: Second Amendment: Militia-Conditioned Right to Bear Arms
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'militia-conditioned' reading of
 *   the Second Amendment, where the prefatory clause ('A well regulated
 *   Militia, being necessary to the security of a free State') defines the
 *   scope of the operative clause ('the right of the people to keep and bear
 *   Arms') to a collective defense context. This interpretation permits
 *   comprehensive government regulation of firearms, viewing private
 *   possession as subservient to the state's interest in maintaining a
 *   militia and public order. The metrics reflect the substantial extraction
 *   from gun owners and the active suppression of alternatives (unrestricted
 *   ownership) inherent in this regulatory framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.65).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.75).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Second Amendment: Militia-Conditioned Right to Bear Arms").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, '819a40eb-22af-4f78-a57e-be688fdae037').
narrative_ontology:cs_kernel_codification('819a40eb-22af-4f78-a57e-be688fdae037', fixed_text).
narrative_ontology:cs_authority_grounding('819a40eb-22af-4f78-a57e-be688fdae037', lineage).
narrative_ontology:cs_interpretation_layer_present('819a40eb-22af-4f78-a57e-be688fdae037').
narrative_ontology:cs_reading_relation('819a40eb-22af-4f78-a57e-be688fdae037', second_amendment_boundary__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('819a40eb-22af-4f78-a57e-be688fdae037', second_amendment_boundary__insurrectionist_reading, forecloses).
narrative_ontology:cs_axiom('819a40eb-22af-4f78-a57e-be688fdae037', foundational, militia_clause_defines_scope).
narrative_ontology:cs_axiom_status(militia_clause_defines_scope, holdable).
narrative_ontology:cs_axiom_grounding('819a40eb-22af-4f78-a57e-be688fdae037', militia_clause_defines_scope, conventional).
narrative_ontology:cs_axiom('819a40eb-22af-4f78-a57e-be688fdae037', foundational, state_has_legitimate_regulatory_power_over_arms).
narrative_ontology:cs_axiom_status(state_has_legitimate_regulatory_power_over_arms, holdable).
narrative_ontology:cs_axiom_grounding('819a40eb-22af-4f78-a57e-be688fdae037', state_has_legitimate_regulatory_power_over_arms, deontological).
narrative_ontology:cs_reference_frame('819a40eb-22af-4f78-a57e-be688fdae037', historical_militia_context).
narrative_ontology:cs_drift_state('819a40eb-22af-4f78-a57e-be688fdae037', contemporary_individual_right_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('819a40eb-22af-4f78-a57e-be688fdae037', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_regulatory_authorities).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, public_safety_advocates).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, general_public).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, gun_owners_seeking_unrestricted_possession).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_manufacturers_and_retailers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities interpret the Second Amendment as primarily enabling collective defense, thereby legitimizing their authority to enact and enforce comprehensive firearms regulations. They benefit from the ability to control the flow and type of arms in circulation, aiming to enhance public safety.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_regulatory_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Advocate for stricter gun control measures, viewing them as essential for reducing gun violence and enhancing community safety. This reading aligns with their goals, as it provides a constitutional basis for such regulations.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, public_safety_advocates, beneficiary,
    organized, generational, mobile, national).

% Benefits from the perceived increase in public safety due to firearms regulation. While some members may also be gun owners, the overall benefit of reduced gun violence is seen as a collective good. They indirectly bear costs if regulations are overly burdensome or infringe on perceived self-defense needs.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, general_public, beneficiary,
    moderate, biographical, constrained, national).

% Bear the direct costs of this reading through restrictions on the types of firearms they can own, where they can carry them, and the processes required for acquisition. Their desired right to unrestricted individual possession is curtailed.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_owners_seeking_unrestricted_possession, payer,
    organized, biographical, constrained, national).

% Face economic costs due to market restrictions, bans on certain types of firearms, and increased regulatory burdens. Their business models are directly impacted by the scope of permissible regulation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_manufacturers_and_retailers, payer,
    powerful, biographical, constrained, national).

% Their core premise of an unconditioned individual right is rejected by this reading. While they actively contest this interpretation in courts and legislatures, their foundational argument is structurally excluded from the framework of this specific reading.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, individual_right_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state and federal power to regulate firearms, ensuring public safety and maintaining the capacity for a 'well regulated Militia' by defining the scope of the right to bear arms within a collective defense context.
% TRANSFER_FUNCTION: Transfers the presumptive right to unrestricted individual firearm possession to the collective (state/militia), enabling the state to impose regulations on the firearms market and individual ownership, thereby transferring a degree of individual liberty for collective security.
% ABSENT_VOICES: Advocates for an unconditioned individual right to bear arms and those who believe the right is for resistance against tyranny are structurally excluded from the core premise of this reading. They would argue for minimal or no regulation, but their foundational interpretations are rejected by this framework.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the legal and policy landscape for firearms in the United States would shift dramatically towards an unconditioned individual right. This would lead to widespread deregulation, increased gun ownership, and a fundamental reorganization of public safety policy, potentially increasing gun violence and altering the balance of power between citizens and the state.
% FOUNDING_PROBLEM: To define the scope of the right to bear arms within the context of a functioning state, balancing individual liberty with the collective security provided by a 'well regulated Militia' and the state's need for order.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, historians, and political scientists from diverse perspectives acknowledge the historical context of militias and the ongoing debate about the Second Amendment's original intent and contemporary application, even if they disagree on the ultimate interpretation. Supreme Court decisions and lower court rulings reflect this ongoing contestation.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__militia_conditioned_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because this reading allows for significant restrictions on firearm ownership, imposing costs on those who desire unrestricted access. Suppression (0.75) is high due to the active legal and enforcement mechanisms required to maintain these regulations against strong opposition. Theater ratio is low (0.1) as the regulatory function is genuine and actively pursued, not merely performative. Resistance is high (0.8) reflecting the intense political and legal contestation from individual rights advocates. The claimed type is 'tangled_rope' because it serves a coordination function (public safety, organized militia) but simultaneously involves asymmetric extraction from specific groups (gun owners, manufacturers) through active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state regulatory authorities and public safety advocates, this reading is a legitimate and necessary coordination mechanism for public safety. From the perspective of gun owners and manufacturers, it is an extractive mechanism that infringes on fundamental rights. The engine's per-seat classification will reflect this divergence based on the declared roles and positional atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory authorities and public safety advocates are beneficiaries, as this reading empowers them to achieve their goals. The general public is also a beneficiary, gaining perceived safety, though some members may bear indirect costs. Gun owners seeking unrestricted possession and firearms manufacturers/retailers are victims, as their interests are directly curtailed by the regulations this reading permits. Individual right advocates are 'excluded' from this reading's core premise, as their foundational arguments are rejected within this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring the extraction from gun owners) or a pure Snare (ignoring the genuine coordination function of public safety and militia context). The 'live' status of the founding problem, despite contestation, indicates that the constraint's mandate is still actively debated and applied, rather than having atrophied into mere inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_interpretation_ambiguity,
    'Is the Second Amendment''s ''well regulated Militia'' clause a prefatory statement of purpose that limits the right''s scope, or merely an explanatory clause that does not limit an inherent individual right?',
    'Further Supreme Court jurisprudence or a constitutional amendment explicitly clarifying the relationship between the prefatory and operative clauses.',
    'If resolved towards an unconditioned individual right, the extractiveness of this reading would be deemed illegitimate, leading to its reclassification as a Snare or its dissolution. If resolved firmly towards the militia-conditioned view, its coordination function would be strengthened, potentially solidifying its Rope-like aspects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_interpretation_ambiguity, conceptual, 'Ambiguity regarding the interpretive weight of the Second Amendment''s prefatory clause.').

omega_variable(
    empirical_public_safety_efficacy,
    'To what extent do comprehensive firearms regulations, enabled by this reading, empirically contribute to public safety and reduce gun violence?',
    'Longitudinal studies comparing gun violence rates in jurisdictions with varying regulatory strictness, controlling for confounding socioeconomic factors.',
    'Strong empirical evidence of public safety benefits would bolster the coordination function, potentially reducing perceived extraction for the general public. Weak or contradictory evidence would undermine the justification for extraction, pushing the constraint closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_public_safety_efficacy, empirical, 'Empirical efficacy of firearms regulation in achieving public safety goals.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of unrestricted gun ownership primarily structural (legal barriers, enforcement) or internalized (social norms, perceived futility of resistance)?',
    'Post-legal-challenge behavior: if gun owners continue to self-regulate or avoid certain firearms even after legal restrictions are lifted, it suggests internalized suppression. If ownership patterns immediately shift, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them. This would make the constraint more resilient to legal challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for gun ownership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1900, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(seco_tr_t1934, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1934, 0.08).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1968, 0.1).
narrative_ontology:measurement(seco_tr_t1994, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1994, 0.1).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(seco_tr_t2020, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1900, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(seco_be_t1934, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1934, 0.5).
narrative_ontology:measurement(seco_be_t1968, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1968, 0.58).
narrative_ontology:measurement(seco_be_t1994, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1994, 0.62).
narrative_ontology:measurement(seco_be_t2008, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2008, 0.6).
narrative_ontology:measurement(seco_be_t2020, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1900, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(seco_su_t1934, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1934, 0.45).
narrative_ontology:measurement(seco_su_t1968, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement(seco_su_t1994, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1994, 0.7).
narrative_ontology:measurement(seco_su_t2008, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(seco_su_t2020, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, firearms_market_regulation).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, public_safety_legislation).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'second_amendment_boundary' kernel, each with different structural properties and classifications. They are linked to reflect their shared origin in the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
