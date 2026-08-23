% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Second Amendment Individual Right Reading Constraint
 *   domain: constitutional_law
 *
 * SUMMARY:
 *   This constraint story models the individual right reading of the Second
 *   Amendment as a legal constraint on government regulatory power. The
 *   reading claims the right is a pre-existing individual liberty that the
 *   Constitution protects against federal (and via incorporation, state)
 *   infringement. The constraint is the judicial doctrine that enforces this
 *   reading, requiring courts to strike down laws that infringe the
 *   individual right. The claim/metric gap is deliberate: the reading claims
 *   a natural-right (mountain) status, but the authored metrics describe a
 *   legally constructed constraint with substantial extraction from
 *   government regulatory authority and active enforcement by courts. The
 *   engine measures this divergence; do not reconcile the claim to the
 *   metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.72).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.78).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment Individual Right Reading Constraint").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional_law").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, '9027362c-12e8-4861-9a99-32ca5c9236f9').
narrative_ontology:cs_kernel_codification('9027362c-12e8-4861-9a99-32ca5c9236f9', fixed_text).
narrative_ontology:cs_authority_grounding('9027362c-12e8-4861-9a99-32ca5c9236f9', lineage).
narrative_ontology:cs_interpretation_layer_present('9027362c-12e8-4861-9a99-32ca5c9236f9').
narrative_ontology:cs_reading_relation('9027362c-12e8-4861-9a99-32ca5c9236f9', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('9027362c-12e8-4861-9a99-32ca5c9236f9', second_amendment_arms_right__civic_republican_reading, forecloses).
narrative_ontology:cs_axiom('9027362c-12e8-4861-9a99-32ca5c9236f9', foundational, individual_right_to_bear_arms_preexists_government).
narrative_ontology:cs_axiom_status(individual_right_to_bear_arms_preexists_government, holdable).
narrative_ontology:cs_axiom_grounding('9027362c-12e8-4861-9a99-32ca5c9236f9', individual_right_to_bear_arms_preexists_government, deontological).
narrative_ontology:cs_axiom('9027362c-12e8-4861-9a99-32ca5c9236f9', foundational, second_amendment_protects_individual_self_defense).
narrative_ontology:cs_axiom_status(second_amendment_protects_individual_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('9027362c-12e8-4861-9a99-32ca5c9236f9', second_amendment_protects_individual_self_defense, deontological).
narrative_ontology:cs_reference_frame('9027362c-12e8-4861-9a99-32ca5c9236f9', founding_era_understanding).
narrative_ontology:cs_drift_state('9027362c-12e8-4861-9a99-32ca5c9236f9', post_heller_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9027362c-12e8-4861-9a99-32ca5c9236f9', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, federal_regulatory_authority).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, state_regulatory_authority).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, individual_right_to_bear_arms_preexists_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the judicial doctrine that protects their right to own firearms from government infringement. They organize politically through groups like the NRA to defend and expand this doctrine. Their exit from the constraint is constrained because they are subject to the legal framework but cannot easily opt out of the constitutional order.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, constrained, national).

% The federal government's regulatory power over firearms is constrained by this doctrine; it cannot enact certain prohibitions or regulations that would infringe the individual right as defined by the Supreme Court. It is trapped within the constitutional framework and cannot exit the constraint without constitutional amendment.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_regulatory_authority, payer,
    institutional, generational, trapped, national).

% State and local governments are also constrained by this doctrine via the Fourteenth Amendment incorporation (McDonald v. Chicago). They lose regulatory authority over firearms and are similarly trapped within the constitutional order.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, state_regulatory_authority, payer,
    institutional, generational, trapped, national).

% They would object to the constraint because it prevents gun regulations they favor; they are structurally excluded from the judicial interpretation that establishes this constraint and must work through political channels to appoint judges who might overturn it.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_control_advocates, excluded,
    organized, biographical, constrained, national).

% The Supreme Court sets the agenda by interpreting the Second Amendment and striking down laws that infringe the individual right; it administers the constraint through judicial review and precedent.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% They analyze the doctrine from an academic perspective, debating its historical origins, textual basis, and doctrinal coherence. They do not directly collect benefits or pay costs but shape the intellectual environment.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects an individual's pre-existing right to keep and bear arms from government infringement, providing a stable legal framework for gun ownership and a clear rule of decision for courts.
% TRANSFER_FUNCTION: Moves regulatory authority over firearms from government (federal and state) to the individual gun owner, by judicially invalidating laws that infringe the right as defined by the individual right reading.
% ABSENT_VOICES: Gun control advocates and communities affected by gun violence are structurally excluded from the judicial interpretation that establishes this constraint; they would argue for a collective right reading that permits regulation.
% DISAPPEARANCE_RATIONALE: If the individual right reading disappeared overnight, the judicial barrier to gun regulations would vanish, allowing federal and state governments to enact a wide range of firearms restrictions; the legal landscape would rearrange dramatically.
% FOUNDING_PROBLEM: The founding problem was the fear that the new federal government would disarm the citizenry, rendering them unable to resist tyranny, and the need to assure ratification by protecting the pre-existing right of individuals to own arms.
% FOUNDING_PROBLEM_CORROBORATION: The individual right reading is attested by the Supreme Court in Heller (2008) and McDonald (2010); however, historians and legal scholars outside the benefiting parties (e.g., collective right proponents) contest the historical accuracy of this founding problem narrative.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.72) because the constraint removes substantial regulatory authority from governments over a wide range of firearms regulations. Suppression is high (0.78) because the constraint's persistence depends on active judicial enforcement that suppresses alternative regulatory regimes. Theater ratio is moderate (0.32) because the coordination function (protecting individual right) is real but a growing share of doctrinal activity serves to entrench the constraint against political challenge. Accessibility collapse (0.64) reflects that alternative regulatory frameworks are largely collapsed once the individual right doctrine is accepted. Resistance (0.68) captures ongoing political, scholarly, and judicial resistance from dissenting justices and gun control advocates.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (government authorities) and beneficiary seat (gun owners) compute differently: from the gun owners' position the constraint is a genuine protection of liberty; from the government's position the same structure operates as enforced extraction of regulatory power. The agenda_setter (Court) experiences it as institutional authority maintenance. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners are structural beneficiaries (d near beneficiary end) — they collect the protection of the right without bearing the cost of enforcement. Federal and state regulatory authorities are targets (d near target end) — they bear the full cost of lost regulatory power and are trapped in the constitutional framework. The Supreme Court as agenda_setter sits near the beneficiary end (it gains institutional authority from being the final arbiter). Gun control advocates are excluded — their exclusion is part of the constraint's enforcement structure. Legal scholars are analytical observers with no direct stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fear of federal disarmament) is contested as still live. The constraint persists even as the original militia context has atrophied, but the coordination function (protecting individual self-defense) remains live. This is not a piton because the constraint is actively maintained and expanded by a powerful beneficiary coalition, not merely performed out of inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_right_vs_legal_construct,
    'Is the individual right to bear arms a genuine natural law (mountain) or a constructed legal doctrine (tangled_rope) that benefits identifiable agents?',
    'Historical analysis of founding-era understanding combined with doctrinal analysis of whether the right''s content is determined by pre-existing natural law or by judicial construction.',
    'If natural law, the constraint would be a mountain with near-zero extraction; if constructed, the measured extraction and suppression are real and the tangled_rope classification holds. The False Summit Mountain signature would trigger if beneficiaries are declared on a mountain claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_right_vs_legal_construct, conceptual, 'Whether the constraint''s natural-law framing is genuine or a cover for extractive legal doctrine.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (judicial enforcement) or partially internalized (political acceptance of the doctrine as settled law)?',
    'Track regulatory behavior after hypothetical doctrinal reversal: if governments immediately enact previously forbidden regulations, suppression was structural; if they hesitate due to political norms, internalization plays a role.',
    'If internalized, effective suppression is higher than the structural measure suggests — the constraint persists even without active judicial enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in constitutional doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(second_amendment_individual_right_tr_t0, second_amendment_arms_right__individual_right_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(second_amendment_individual_right_tr_t2, second_amendment_arms_right__individual_right_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement(second_amendment_individual_right_tr_t7, second_amendment_arms_right__individual_right_reading, theater_ratio, 7, 0.3).
narrative_ontology:measurement(second_amendment_individual_right_tr_t12, second_amendment_arms_right__individual_right_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(second_amendment_individual_right_tr_t17, second_amendment_arms_right__individual_right_reading, theater_ratio, 17, 0.32).

% Extraction over time
narrative_ontology:measurement(second_amendment_individual_right_be_t0, second_amendment_arms_right__individual_right_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(second_amendment_individual_right_be_t2, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2, 0.62).
narrative_ontology:measurement(second_amendment_individual_right_be_t7, second_amendment_arms_right__individual_right_reading, base_extractiveness, 7, 0.68).
narrative_ontology:measurement(second_amendment_individual_right_be_t12, second_amendment_arms_right__individual_right_reading, base_extractiveness, 12, 0.71).
narrative_ontology:measurement(second_amendment_individual_right_be_t17, second_amendment_arms_right__individual_right_reading, base_extractiveness, 17, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(second_amendment_individual_right_su_t0, second_amendment_arms_right__individual_right_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(second_amendment_individual_right_su_t2, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2, 0.7).
narrative_ontology:measurement(second_amendment_individual_right_su_t7, second_amendment_arms_right__individual_right_reading, suppression_requirement, 7, 0.74).
narrative_ontology:measurement(second_amendment_individual_right_su_t12, second_amendment_arms_right__individual_right_reading, suppression_requirement, 12, 0.76).
narrative_ontology:measurement(second_amendment_individual_right_su_t17, second_amendment_arms_right__individual_right_reading, suppression_requirement, 17, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the second_amendment_arms_right kernel. The individual right reading forecloses the collective right reading in any single legal framework because they posit mutually exclusive right-holders (individual vs. state). It also forecloses the civic republican reading because that reading ties the right to collective self-governance while this reading ties it to individual self-defense. All three readings compete for authority over the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
