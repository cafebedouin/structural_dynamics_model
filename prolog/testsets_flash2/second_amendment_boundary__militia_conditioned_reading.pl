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
 *   human_readable: Second Amendment: Militia-Conditioned Right Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'militia-conditioned' reading of the
 *   Second Amendment, which interprets the right to keep and bear arms as
 *   primarily tied to the context of a 'well regulated Militia' and
 *   collective defense. This reading presumes legitimate state regulatory
 *   authority over firearms, subjecting private possession to means-end
 *   scrutiny and exposing the firearms market to democratic restriction. This
 *   reading was dominant for much of the 20th century, particularly after the
 *   1939 Miller v. United States Supreme Court decision, until challenged by
 *   the individual rights reading in the late 20th and early 21st centuries,
 *   culminating in the 2008 Heller decision.
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
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Second Amendment: Militia-Conditioned Right Reading").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, '8c2cbe40-cd73-4dab-89b6-4f7fb0f6df5d').
narrative_ontology:cs_kernel_codification('8c2cbe40-cd73-4dab-89b6-4f7fb0f6df5d', fixed_text).
narrative_ontology:cs_authority_grounding('8c2cbe40-cd73-4dab-89b6-4f7fb0f6df5d', lineage).
narrative_ontology:cs_interpretation_layer_present('8c2cbe40-cd73-4dab-89b6-4f7fb0f6df5d').
narrative_ontology:cs_reading_relation('8c2cbe40-cd73-4dab-89b6-4f7fb0f6df5d', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c2cbe40-cd73-4dab-89b6-4f7fb0f6df5d', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('8c2cbe40-cd73-4dab-89b6-4f7fb0f6df5d', foundational, militia_clause_defines_scope).
narrative_ontology:cs_axiom_status(militia_clause_defines_scope, holdable).
narrative_ontology:cs_axiom_grounding('8c2cbe40-cd73-4dab-89b6-4f7fb0f6df5d', militia_clause_defines_scope, conventional).
narrative_ontology:cs_axiom('8c2cbe40-cd73-4dab-89b6-4f7fb0f6df5d', secondary, public_safety_justifies_regulation).
narrative_ontology:cs_axiom_status(public_safety_justifies_regulation, holdable).
narrative_ontology:cs_axiom_grounding('8c2cbe40-cd73-4dab-89b6-4f7fb0f6df5d', public_safety_justifies_regulation, instrumental).
narrative_ontology:cs_reference_frame('8c2cbe40-cd73-4dab-89b6-4f7fb0f6df5d', collective_right_regulatory_supremacy).
narrative_ontology:cs_drift_state('8c2cbe40-cd73-4dab-89b6-4f7fb0f6df5d', post_heller_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8c2cbe40-cd73-4dab-89b6-4f7fb0f6df5d', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_regulatory_authorities).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, public_safety_advocates).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, gun_owners_seeking_unrestricted_possession).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These authorities interpret the Second Amendment as permitting comprehensive regulation of firearms, viewing the right as primarily tied to militia service. They benefit from the ability to enact and enforce public safety laws, which this reading legitimizes.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_regulatory_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for stricter gun control measures, believing that a militia-conditioned reading of the Second Amendment is essential for reducing gun violence. They benefit from the legal and political space this interpretation creates for their policy goals.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, public_safety_advocates, beneficiary,
    organized, biographical, mobile, national).

% Experience restrictions on their ability to acquire and possess firearms for personal use, collecting, or self-defense, particularly in jurisdictions with strong gun control laws. They bear the costs of compliance, licensing, and potential confiscation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_owners_seeking_unrestricted_possession, payer,
    moderate, biographical, constrained, local).

% Face market restrictions, bans on certain types of firearms, and increased regulatory burdens due to the expansive interpretation of state power under this reading. Their business models are directly impacted by the scope of permissible regulation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_manufacturers, payer,
    powerful, generational, constrained, national).

% Advocate for an individual right to bear arms, independent of militia service, and are actively excluded from the policy-making process that flows from the militia-conditioned reading. They are forced to litigate or lobby against policies derived from this interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, individual_right_advocates, excluded,
    organized, generational, trapped, national).

% Analyze the historical context, textual meaning, and legal implications of the Second Amendment, often debating the merits of different readings. They provide academic commentary and influence legal discourse but do not directly enforce or pay into the constraint.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of the right to bear arms with the state's interest in public safety, allowing for a framework of regulation that balances individual liberty with collective security concerns.
% TRANSFER_FUNCTION: Transfers regulatory authority over firearms from individuals to the state, allowing for restrictions on certain types of weapons or modes of possession, from gun owners and manufacturers to state authorities and public safety advocates.
% ABSENT_VOICES: Advocates for an individual, unrestricted right to bear arms are largely absent from the legislative and judicial processes that uphold this reading, as their core premise is rejected by this interpretation. They would argue for minimal regulation and a broader scope of individual possession.
% DISAPPEARANCE_RATIONALE: If this reading vanished, state regulatory authority over firearms would be severely curtailed, leading to a rapid expansion of permissible firearm types and modes of possession. Public safety laws would be challenged and overturned, and the firearms market would become significantly less regulated, fundamentally altering the legal and social landscape.
% FOUNDING_PROBLEM: The problem of balancing individual liberty with the collective need for security and order, particularly concerning the potential for armed private citizens to disrupt public peace or challenge state authority.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and legal historians attest to the ongoing debate regarding the Second Amendment's original intent and its application to modern society. Public safety organizations and state governments consistently highlight the live problem of gun violence and the need for regulatory frameworks, corroborating the continued relevance of this interpretive approach.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate, as it restricts certain gun owners but does not prohibit all possession. Suppression (0.6) is significant because it requires active enforcement of regulations and suppression of challenges to state authority. The theater ratio (0.1) is low, indicating that the regulatory function is largely genuine, not performative. Accessibility collapse (0.4) is moderate, as alternatives (e.g., non-firearm self-defense, regulated firearm possession) exist but are constrained. Resistance (0.7) is high, reflecting ongoing political and legal challenges from those advocating for an individual right.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state regulatory authorities, this reading is a legitimate and necessary framework for public safety. From the perspective of gun owners seeking unrestricted possession, it is an extractive constraint that infringes on a fundamental individual right. The engine's classification will reflect this divergence based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory authorities and public safety advocates are beneficiaries, gaining the power to regulate and promote collective security. Gun owners seeking unrestricted possession and firearms manufacturers are payers, bearing the costs of regulation and market restrictions. Individual right advocates are excluded, as their core interpretive premise is rejected by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_originalism_vs_living_constitution,
    'Is the ''well regulated Militia'' clause a historical artifact limiting the right to its 18th-century context, or does it represent an enduring principle for contemporary regulation?',
    'Further historical and legal scholarship on the original public meaning, combined with judicial decisions that explicitly address the temporal scope of the prefatory clause.',
    'If a historical artifact, the militia-conditioned reading''s legitimacy would erode, shifting power towards individual rights interpretations. If an enduring principle, its regulatory authority would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_originalism_vs_living_constitution, conceptual, 'Ambiguity regarding the temporal relevance of the prefatory clause.').

omega_variable(
    empirical_impact_of_regulation,
    'What is the demonstrable empirical effect of comprehensive firearms regulation (as permitted by this reading) on public safety outcomes (e.g., gun violence rates)?',
    'Large-scale, longitudinal epidemiological and sociological studies comparing public safety outcomes in jurisdictions with varying levels of regulation, controlling for confounding factors.',
    'Strong empirical evidence of positive public safety outcomes would bolster the legitimacy of this reading and its associated regulations. Weak or contradictory evidence would undermine its instrumental justification, increasing resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_impact_of_regulation, empirical, 'Uncertainty about the causal link between regulation and public safety.').

omega_variable(
    militia_definition_ambiguity,
    'What constitutes a ''well regulated Militia'' in the modern era? Is it limited to the National Guard, or does it encompass a broader body of citizens?',
    'Judicial clarification or legislative redefinition of ''militia'' in contemporary legal and military contexts.',
    'A narrow definition (e.g., National Guard only) would further strengthen state regulatory power over private arms. A broad definition could create new avenues for challenging regulations based on a broader ''militia'' concept.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_definition_ambiguity, conceptual, 'Ambiguity in the modern definition of ''militia''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 1939, 2008).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1939, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1939, 0.05).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1968, 0.08).
narrative_ontology:measurement(seco_tr_t1986, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2008, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1939, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1939, 0.3).
narrative_ontology:measurement(seco_be_t1968, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1968, 0.35).
narrative_ontology:measurement(seco_be_t1986, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1986, 0.4).
narrative_ontology:measurement(seco_be_t2008, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2008, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1939, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1939, 0.4).
narrative_ontology:measurement(seco_su_t1968, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1968, 0.5).
narrative_ontology:measurement(seco_su_t1986, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1986, 0.55).
narrative_ontology:measurement(seco_su_t2008, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2008, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
