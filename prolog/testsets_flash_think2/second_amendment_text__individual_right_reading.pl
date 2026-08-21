% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment: Individual Right Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'individual right' reading of the Second
 *   Amendment, which interprets the operative clause as guaranteeing a
 *   personal right to bear arms for self-defense, largely independent of
 *   militia service. This reading gained significant legal traction in the
 *   late 20th and early 21st centuries, culminating in landmark Supreme Court
 *   decisions. It is actively defended by gun rights advocates and the
 *   firearms industry, while facing strong resistance from public safety
 *   advocates and those who interpret the amendment as primarily tied to
 *   collective security.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.75).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment: Individual Right Reading").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, '3d01a321-6012-4694-9266-719b29764be0').
narrative_ontology:cs_kernel_codification('3d01a321-6012-4694-9266-719b29764be0', fixed_text).
narrative_ontology:cs_authority_grounding('3d01a321-6012-4694-9266-719b29764be0', lineage).
narrative_ontology:cs_interpretation_layer_present('3d01a321-6012-4694-9266-719b29764be0').
narrative_ontology:cs_reading_relation('3d01a321-6012-4694-9266-719b29764be0', second_amendment_text__collective_security_reading, forecloses).
narrative_ontology:cs_reading_relation('3d01a321-6012-4694-9266-719b29764be0', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('3d01a321-6012-4694-9266-719b29764be0', foundational, individual_right_to_bear_arms_for_self_defense).
narrative_ontology:cs_axiom_status(individual_right_to_bear_arms_for_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('3d01a321-6012-4694-9266-719b29764be0', individual_right_to_bear_arms_for_self_defense, deontological).
narrative_ontology:cs_axiom('3d01a321-6012-4694-9266-719b29764be0', secondary, militia_clause_is_prefatory_not_substantive).
narrative_ontology:cs_axiom_status(militia_clause_is_prefatory_not_substantive, holdable).
narrative_ontology:cs_axiom_grounding('3d01a321-6012-4694-9266-719b29764be0', militia_clause_is_prefatory_not_substantive, conventional).
narrative_ontology:cs_reference_frame('3d01a321-6012-4694-9266-719b29764be0', post_heller_interpretation).
narrative_ontology:cs_drift_state('3d01a321-6012-4694-9266-719b29764be0', contemporary_legal_landscape, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3d01a321-6012-4694-9266-719b29764be0', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, disarmed_populations).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, gun_violence_victims).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, public_safety_advocates).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, individual_liberty_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, self_defense_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the legal protection of their right to own firearms for self-defense, free from most state-level restrictions. They actively exercise this right and resist attempts to curtail it.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    powerful, biographical, mobile, national).

% Benefits directly from the legal framework that protects gun ownership, leading to a robust market for firearms and accessories. They actively lobby and litigate to maintain and expand this interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, firearms_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Individuals legally prohibited from owning firearms (e.g., convicted felons, those with domestic violence restraining orders) bear the cost of this constraint by being denied the right to self-defense, even if they pose no current threat. Their situation is often identity-locked by legal status.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, disarmed_populations, payer,
    powerless, immediate, trapped, local).

% Bear the direct and indirect costs of gun violence, which is argued to be exacerbated by the widespread availability of firearms protected by this interpretation. They have no direct exit from the societal consequences.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_violence_victims, payer,
    powerless, immediate, trapped, local).

% Advocate for stricter gun control measures to reduce gun violence, but face significant legal and political barriers due to the individual right interpretation. They bear the cost of legislative inaction and societal harm.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, public_safety_advocates, payer,
    organized, biographical, constrained, national).

% Are tasked with creating laws that balance individual rights and public safety, but are heavily constrained by judicial interpretations of the Second Amendment and political pressure from gun rights advocates. They administer the legal framework.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, legislators, agenda_setter,
    institutional, biographical, constrained, national).

% Are the primary interpreters and enforcers of the Second Amendment, shaping its scope and application through landmark rulings. Their decisions actively define the constraint for all other parties.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Argue that the Second Amendment's primary purpose is tied to a 'well-regulated militia' for collective defense, not unlimited individual ownership. Their interpretation is largely sidelined in contemporary legal discourse, making them effectively excluded from the dominant framing.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, collective_security_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__individual_right_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_text__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual self-defense and provides a check against potential governmental overreach by ensuring an armed citizenry, fostering a sense of personal security and liberty for gun owners.
% TRANSFER_FUNCTION: Transfers the burden of self-protection to individuals, while transferring the societal costs of gun violence (e.g., healthcare, law enforcement, lost lives) to the general public, and economic gains to the firearms industry.
% ABSENT_VOICES: Those who prioritize collective security and public health over individual gun ownership, particularly those directly affected by gun violence, are often marginalized in the legal and political discourse that upholds this interpretation.
% DISAPPEARANCE_RATIONALE: If the individual right interpretation vanished overnight, it would fundamentally alter firearms policy, likely leading to widespread gun control legislation, a significant restructuring of the firearms industry, and a shift in public safety strategies. The balance of power between citizens and the state regarding arms would be profoundly reorganized.
% FOUNDING_PROBLEM: To ensure the capacity for individual self-defense and to maintain a 'well-regulated militia' as a check against a standing army and potential tyranny, drawing on historical fears of centralized power.
% FOUNDING_PROBLEM_CORROBORATION: Individual rights advocates and the firearms industry assert the founding problem (tyranny, self-defense) is still live and relevant. Public safety advocates and some constitutional scholars argue the original context has changed dramatically, and the current interpretation creates new problems, citing legislative hearings and historical analyses from outside the benefiting parties.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because this interpretation imposes significant costs on society (e.g., gun violence) and disarmed populations, while concentrating benefits on gun owners and the firearms industry. Suppression is also high (0.80) as it actively suppresses legislative attempts at gun control and alternative interpretations through judicial enforcement and political lobbying. Theater ratio is low (0.13) because the constraint is actively and genuinely enforced, with real consequences for those who challenge it. Accessibility collapse is moderate (0.60) as it significantly limits the policy alternatives for gun control, but does not completely eliminate them. Resistance is high (0.80) due to ongoing efforts by public safety groups to challenge this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individual gun owners, this constraint is a fundamental liberty, a 'rope' coordinating self-defense. From the perspective of gun violence victims or public safety advocates, it operates as a 'snare' or 'tangled rope,' extracting a heavy societal cost and suppressing effective regulation. The engine's computation of per-seat classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry are clear beneficiaries, experiencing low directionality as the constraint subsidizes their interests. Disarmed populations, gun violence victims, and public safety advocates are targets, bearing significant costs and experiencing high directionality. Legislators and courts act as agenda-setters, shaping and enforcing the constraint, but are also constrained by its established interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_interpretation_ambiguity,
    'Is the ''well-regulated militia'' clause a prefatory statement of purpose or a substantive limitation on the right to bear arms?',
    'Further historical and legal scholarship, or a future Supreme Court ruling that re-evaluates the clause''s relationship to the operative clause.',
    'If found to be a substantive limitation, the constraint''s scope would narrow, potentially reducing extractiveness and suppression by allowing more regulation tied to militia service. If confirmed as purely prefatory, the individual right interpretation would be further solidified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_clause_interpretation_ambiguity, conceptual, 'Ambiguity regarding the ''well-regulated militia'' clause''s legal effect.').

omega_variable(
    modern_firearms_technology_impact,
    'How does the advent of modern, high-capacity firearms and their use in mass shootings impact the original intent or contemporary application of the individual right to bear arms?',
    'Empirical studies on the relationship between firearm availability, technology, and public safety outcomes, combined with judicial re-evaluation of ''dangerous and unusual weapons'' in light of modern capabilities.',
    'If modern firearms are deemed outside the scope of protected arms, the constraint''s extractiveness (societal cost) could decrease, and suppression of regulation could lessen. If they are fully protected, the current high extractiveness and suppression would persist or increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_firearms_technology_impact, empirical, 'Impact of modern firearms technology on the Second Amendment''s scope.').

omega_variable(
    individual_vs_collective_safety_priority,
    'Should individual self-defense rights take precedence over collective public safety concerns, or vice versa, when interpreting the Second Amendment?',
    'Societal consensus shifts, legislative action reflecting new priorities, or a re-balancing by the judiciary based on evolving constitutional principles and empirical evidence.',
    'A shift towards prioritizing collective safety would lead to a re-interpretation that allows for more restrictive gun control, reducing the constraint''s extractiveness and suppression. Continued prioritization of individual rights would maintain or increase current levels.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(individual_vs_collective_safety_priority, preference, 'The fundamental normative tension between individual liberty and collective safety.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_text__individual_right_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(seco_tr_t1985, second_amendment_text__individual_right_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_text__individual_right_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__individual_right_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2015, second_amendment_text__individual_right_reading, theater_ratio, 2015, 0.14).
narrative_ontology:measurement(seco_tr_t2025, second_amendment_text__individual_right_reading, theater_ratio, 2025, 0.13).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_text__individual_right_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(seco_be_t1985, second_amendment_text__individual_right_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(seco_be_t2000, second_amendment_text__individual_right_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__individual_right_reading, base_extractiveness, 2008, 0.68).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2015, second_amendment_text__individual_right_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(seco_be_t2025, second_amendment_text__individual_right_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_text__individual_right_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(seco_su_t1985, second_amendment_text__individual_right_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(seco_su_t2000, second_amendment_text__individual_right_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__individual_right_reading, suppression_requirement, 2008, 0.75).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2015, second_amendment_text__individual_right_reading, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement(seco_su_t2025, second_amendment_text__individual_right_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, gun_control_legislation).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, public_safety_policy).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Second Amendment text kernel. Its extractiveness and suppression metrics differ significantly from the 'collective_security_reading' and 'originalist_civic_virtue_reading' due to differing interpretations of the right's scope and purpose.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
