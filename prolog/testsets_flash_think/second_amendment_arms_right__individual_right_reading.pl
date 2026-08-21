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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment: Individual Right to Bear Arms (Individual Right Reading)
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'individual right' reading of the Second
 *   Amendment, asserting that the right to keep and bear arms is a
 *   pre-existing individual liberty protected against federal infringement.
 *   This reading has gained prominence through Supreme Court jurisprudence,
 *   shifting the balance of power from government regulation to individual
 *   gun ownership. The claimed type is 'tangled_rope' because it coordinates
 *   individual liberty while extracting regulatory capacity from the state,
 *   requiring active judicial enforcement to maintain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.8).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.7).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment: Individual Right to Bear Arms (Individual Right Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, '2275ca4b-18fe-4d57-a405-5500379bdb5a').
narrative_ontology:cs_kernel_codification('2275ca4b-18fe-4d57-a405-5500379bdb5a', fixed_text).
narrative_ontology:cs_authority_grounding('2275ca4b-18fe-4d57-a405-5500379bdb5a', lineage).
narrative_ontology:cs_interpretation_layer_present('2275ca4b-18fe-4d57-a405-5500379bdb5a').
narrative_ontology:cs_reading_relation('2275ca4b-18fe-4d57-a405-5500379bdb5a', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('2275ca4b-18fe-4d57-a405-5500379bdb5a', second_amendment_arms_right__civic_republican_reading, forecloses).
narrative_ontology:cs_axiom('2275ca4b-18fe-4d57-a405-5500379bdb5a', foundational, individual_self_defense_right).
narrative_ontology:cs_axiom_status(individual_self_defense_right, holdable).
narrative_ontology:cs_axiom_grounding('2275ca4b-18fe-4d57-a405-5500379bdb5a', individual_self_defense_right, deontological).
narrative_ontology:cs_axiom('2275ca4b-18fe-4d57-a405-5500379bdb5a', foundational, pre_existing_natural_right).
narrative_ontology:cs_axiom_status(pre_existing_natural_right, holdable).
narrative_ontology:cs_axiom_grounding('2275ca4b-18fe-4d57-a405-5500379bdb5a', pre_existing_natural_right, deontological).
narrative_ontology:cs_reference_frame('2275ca4b-18fe-4d57-a405-5500379bdb5a', original_intent_individual_right).
narrative_ontology:cs_drift_state('2275ca4b-18fe-4d57-a405-5500379bdb5a', contemporary_post_bruen_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('2275ca4b-18fe-4d57-a405-5500379bdb5a', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, federal_state_local_governments).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, public_safety_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise a fundamental, pre-existing right to possess firearms for self-defense and other lawful purposes. They actively resist any legislative or regulatory measures that would infringe upon this right, often viewing it as essential to their liberty and identity.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Benefits directly from the expansive interpretation of the individual right, which reduces regulatory burdens and expands the market for firearms and accessories. They actively lobby against restrictions and support legal challenges to gun control laws.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, firearms_industry, beneficiary,
    organized, biographical, arbitrage, global).

% Their ability to enact and enforce gun control laws aimed at public safety is significantly constrained by this reading. They bear the costs of increased gun violence and the legal challenges to their regulatory efforts.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_state_local_governments, payer,
    institutional, generational, constrained, national).

% Advocate for stricter gun control measures to reduce gun violence and enhance public safety. They bear the social and human costs of gun violence and face significant legal and political barriers due to this reading's broad scope.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, public_safety_advocates, payer,
    organized, biographical, constrained, national).

% The ultimate arbiter of the Second Amendment's meaning. Its rulings (e.g., Heller, McDonald, Bruen) have solidified and expanded the individual right reading, actively shaping the legal landscape for firearms ownership and regulation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Argue that the Second Amendment primarily protects the right of states to maintain militias, not an individual right to own guns for any purpose. Their interpretation is largely foreclosed by the individual right reading, marginalizing their voice in contemporary legal discourse.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, collective_right_proponents, excluded,
    organized, biographical, constrained, national).

% Contend that the right to bear arms is tied to the civic duty of armed citizenship for republican self-governance, implying a more regulated and public-oriented right than pure individual liberty. Their nuanced position is often overshadowed or dismissed by the dominant individual right interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, civic_republican_proponents, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__individual_right_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects the individual's ability to possess firearms for self-defense and other lawful purposes, ensuring a baseline of personal security against potential threats and potential government overreach.
% TRANSFER_FUNCTION: Transfers regulatory authority over firearms from government bodies to individual citizens, and transfers the primary burden of self-defense from the state to the individual. It also transfers economic gains to the firearms industry by limiting market restrictions.
% ABSENT_VOICES: Proponents of the collective right and civic republican readings are structurally excluded from this reading's core premise, as their interpretations would fundamentally alter the scope of the right. Victims of gun violence and their families are also often marginalized in the discourse around this reading, as their concerns are framed as secondary to the individual right.
% DISAPPEARANCE_RATIONALE: If this individual right reading vanished overnight, federal and state governments would immediately move to enact stricter gun control laws, fundamentally altering the firearms market, public safety landscape, and the relationship between citizens and the state regarding arms. The entire legal and social framework around gun ownership would reorganize.
% FOUNDING_PROBLEM: To ensure the capacity for individual self-defense and collective resistance against potential tyranny, both individually and as part of a well-regulated militia, in the context of a newly formed republic wary of centralized power and standing armies.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading (individual gun owners, firearms industry, some legal scholars) argue the problem of self-defense and potential government overreach remains live and relevant. Opponents (public safety advocates, other legal scholars, collective right proponents) attest that the original context of militias is largely obsolete and the problem has shifted to gun violence, making the current interpretation a source of harm rather than protection. Legislative-hearing testimony, historical analysis, and contemporary crime statistics are cited by both sides.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.8) is high because this reading significantly curtails the ability of federal, state, and local governments to enact gun control measures, imposing substantial costs on public safety efforts. Suppression (0.7) is also high, reflecting the active judicial and political efforts to prevent or overturn regulations. The theater ratio is low (0.1) as the enforcement of this right is direct and impactful, not performative. Accessibility collapse (0.6) is moderate, as it limits regulatory alternatives, and resistance (0.8) is high due to ongoing political and legal challenges from public safety advocates. The temporal measurements reflect the increasing judicial affirmation of this reading, particularly since the late 20th century, leading to a rise in both extractiveness and suppression of regulatory efforts.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individual gun owners and the firearms industry, this constraint is a fundamental protection of liberty and a necessary coordination mechanism for self-defense. From the perspective of governments and public safety advocates, it is an extractive mechanism that imposes significant societal costs by limiting effective gun control. The Supreme Court, as the agenda-setter, actively shapes this divergence through its interpretations.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry are clear beneficiaries, experiencing low effective extraction (or even subsidy) as their rights and market access are protected. Federal, state, and local governments, along with public safety advocates, are targets, bearing the costs of restricted regulatory power and increased gun violence, leading to high effective extraction. The Supreme Court, while an agenda-setter, operates from an analytical position, interpreting the constraint rather than directly benefiting or paying in the same way as other stakeholders.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_vs_collective_ambiguity,
    'Is the Second Amendment''s right to keep and bear arms primarily an individual right or a collective right tied to militia service?',
    'Further Supreme Court rulings clarifying the historical and textual basis, or a constitutional amendment explicitly defining the scope.',
    'If resolved as primarily collective, the individual right reading''s extractiveness would collapse, and regulatory authority would be restored to governments. If further solidified as individual, regulatory efforts would face even greater suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_vs_collective_ambiguity, conceptual, 'Ambiguity regarding the primary beneficiary and purpose of the Second Amendment.').

omega_variable(
    pre_existing_vs_granted_ambiguity,
    'Is the right to keep and bear arms a pre-existing natural right, or a right granted and therefore limited by the Constitution?',
    'Philosophical and legal consensus on the nature of constitutional rights, or explicit textual clarification.',
    'If pre-existing, its scope is inherently broad and resistant to government limitation. If granted, it is more amenable to legislative definition and restriction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_existing_vs_granted_ambiguity, conceptual, 'Ambiguity regarding the source and inherent limits of the right.').

omega_variable(
    public_safety_impact_quantification,
    'What is the quantifiable impact of this individual right reading on public safety outcomes (e.g., rates of gun violence, accidental deaths, mass shootings)?',
    'Longitudinal epidemiological studies, comparative analysis across jurisdictions with varying gun laws, and robust statistical modeling controlling for confounding factors.',
    'Strong empirical evidence of negative public safety impacts would increase pressure for reinterpretation or legislative action, potentially shifting the balance of extraction. Evidence of no impact or positive impact would strengthen the current reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_safety_impact_quantification, empirical, 'Empirical uncertainty regarding the societal costs and benefits of the broad individual right.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_arms_right__individual_right_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_arms_right__individual_right_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(seco_tr_t1990, second_amendment_arms_right__individual_right_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_arms_right__individual_right_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_arms_right__individual_right_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(seco_tr_t2025, second_amendment_arms_right__individual_right_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(seco_be_t1980, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(seco_be_t1990, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(seco_be_t2000, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(seco_be_t2010, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(seco_be_t2025, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2025, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(seco_su_t1980, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(seco_su_t1990, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(seco_su_t2000, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(seco_su_t2010, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(seco_su_t2025, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, gun_control_legislation).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, federal_police_powers).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, state_police_powers).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Second Amendment kernel, each with different structural implications for beneficiaries, victims, and regulatory authority. This reading directly influences and forecloses aspects of the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
