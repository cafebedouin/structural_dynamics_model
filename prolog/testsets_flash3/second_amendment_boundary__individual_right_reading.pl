% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__individual_right_reading, []).

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
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Second Amendment: Individual Right Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'individual right' reading of the
 *   Second Amendment, where the operative clause establishes a pre-existing
 *   individual right to bear arms, and the prefatory militia clause states a
 *   purpose but does not limit the right's scope. This reading has gained
 *   prominence, particularly since the late 20th century, leading to
 *   significant legal and social consequences. Sibling readings include the
 *   'militia conditioned' reading (where the right is tied to militia
 *   service) and the 'insurrectionist' reading (where the right is for armed
 *   resistance against government).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.75).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment: Individual Right Reading").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, '88b8f667-5096-413b-8f9f-358e42d24f5a').
narrative_ontology:cs_kernel_codification('88b8f667-5096-413b-8f9f-358e42d24f5a', fixed_text).
narrative_ontology:cs_authority_grounding('88b8f667-5096-413b-8f9f-358e42d24f5a', lineage).
narrative_ontology:cs_interpretation_layer_present('88b8f667-5096-413b-8f9f-358e42d24f5a').
narrative_ontology:cs_reading_relation('88b8f667-5096-413b-8f9f-358e42d24f5a', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('88b8f667-5096-413b-8f9f-358e42d24f5a', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('88b8f667-5096-413b-8f9f-358e42d24f5a', foundational, individual_right_pre_exists_constitution).
narrative_ontology:cs_axiom_status(individual_right_pre_exists_constitution, holdable).
narrative_ontology:cs_axiom_grounding('88b8f667-5096-413b-8f9f-358e42d24f5a', individual_right_pre_exists_constitution, deontological).
narrative_ontology:cs_axiom('88b8f667-5096-413b-8f9f-358e42d24f5a', foundational, militia_clause_is_prefatory_not_limiting).
narrative_ontology:cs_axiom_status(militia_clause_is_prefatory_not_limiting, holdable).
narrative_ontology:cs_axiom_grounding('88b8f667-5096-413b-8f9f-358e42d24f5a', militia_clause_is_prefatory_not_limiting, conventional).
narrative_ontology:cs_reference_frame('88b8f667-5096-413b-8f9f-358e42d24f5a', individual_right_as_fundamental).
narrative_ontology:cs_drift_state('88b8f667-5096-413b-8f9f-358e42d24f5a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('88b8f667-5096-413b-8f9f-358e42d24f5a', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_owners).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_manufacturers_and_retailers).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, mass_shooting_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, domestic_violence_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, suicide_completers_with_firearm_access).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, public_safety_advocates).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, self_defense_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, individual_liberty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the constitutional protection of private firearm ownership, viewing it as a fundamental right for self-defense and sport. Identity is often fused with gun ownership, making any restriction feel like an attack on self. Actively resists regulation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Benefits from a constitutionally shielded market for firearms, with state regulation treated as presumptive infringement. Profits from increased sales and reduced regulatory burden. Funds advocacy to maintain this interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_manufacturers_and_retailers, beneficiary,
    institutional, generational, arbitrage, national).

% Bears the direct and indirect costs of gun violence, including physical harm, psychological trauma, and loss of life. Has no direct exit from the consequences of widespread firearm access.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, mass_shooting_victims, payer,
    powerless, immediate, trapped, local).

% Faces heightened risk and lethality when abusers have easy access to firearms. The individual right reading often complicates efforts to disarm domestic abusers, increasing their vulnerability.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, domestic_violence_victims, payer,
    powerless, immediate, trapped, local).

% Individuals in crisis who complete suicide with firearms, where easy access to a highly lethal means increases the likelihood of a fatal outcome. The constraint contributes to the availability of such means.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, suicide_completers_with_firearm_access, payer,
    powerless, immediate, trapped, local).

% Works to reduce gun violence through legislative and policy changes. Bears the cost of legislative gridlock and the difficulty of enacting effective firearm regulations due to the individual right interpretation. Their exit is to abandon advocacy, which is not a true option.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, public_safety_advocates, payer,
    organized, generational, constrained, national).

% Responsible for public safety but constrained in their ability to enact firearm regulations by judicial interpretations of the Second Amendment. Their agenda-setting power is limited by the scope of the individual right reading.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, state_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Interprets the Second Amendment, establishing the legal boundaries of the individual right and reviewing challenges to firearm regulations. Their rulings shape the constraint's operation and enforcement.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, consistent legal framework for firearm ownership across jurisdictions, ensuring individuals can exercise a perceived fundamental right without arbitrary state interference.
% TRANSFER_FUNCTION: Transfers the burden of gun violence from the state (which is constrained in its regulatory capacity) to individuals and communities, particularly victims of gun violence. It also transfers economic gains to the firearms industry.
% ABSENT_VOICES: Future generations who will inherit the consequences of current firearm policy, and those who are silenced by gun violence itself, are absent from the constitutional debate that shapes this reading.
% DISAPPEARANCE_RATIONALE: If this reading of the Second Amendment vanished overnight, state and federal legislatures would immediately move to enact comprehensive firearm regulations, the firearms market would face significant new restrictions, and the public safety landscape would fundamentally shift as the balance between individual rights and collective safety is re-evaluated.
% FOUNDING_PROBLEM: The founding problem was to ensure the security of a free state by allowing citizens to keep and bear arms, primarily in the context of a militia, and to prevent federal overreach that might disarm the populace.
% FOUNDING_PROBLEM_CORROBORATION: Firearms owners and advocacy groups attest the problem is still live, citing the need for self-defense against crime and potential tyranny. Public safety advocates and legal scholars attest the original problem (militia for state security) is largely dead, and the current interpretation has expanded beyond its original intent, leading to new problems of gun violence. Historical analysis and contemporary crime statistics from outside the benefiting parties support the contested status.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__individual_right_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the broad interpretation of the individual right imposes significant costs on public safety and victims of gun violence, while benefiting firearms owners and the industry. Suppression is also high (0.75) as this reading actively suppresses legislative efforts to regulate firearms, effectively trapping victims in a high-risk environment. Theater ratio is low (0.20) because the constraint is actively enforced and defended, with real consequences, rather than being merely performative. The increasing extractiveness and suppression over time reflect the hardening of this interpretation and its impact.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of firearms owners, this is a fundamental protection of liberty (a Rope or even Mountain). From the perspective of victims and public safety advocates, it is a highly extractive Snare that enables violence. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Firearms owners and the industry are clear beneficiaries (low directionality), as the constraint protects their interests and market. Victims of gun violence and public safety advocates are clear targets (high directionality), bearing the costs and facing suppressed alternatives for redress. State legislatures and the federal judiciary, while agenda-setters, are also constrained by this reading, limiting their ability to act on public safety concerns.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate has shifted from its original context of a 'well regulated Militia' to a broad individual right. This shift has led to a situation where the original problem (state security via militia) is largely superseded, but the constraint persists and expands, creating new problems (gun violence) that it does not address. The classification as a Tangled Rope reflects this hybrid nature: a coordination function (providing a framework for gun ownership) intertwined with significant asymmetric extraction from victims and public safety.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_vs_contemporary_reading,
    'To what extent does the contemporary ''individual right'' reading align with the original intent of the Second Amendment''s framers?',
    'Historical and textual analysis of founding-era documents, legal scholarship, and judicial precedent, particularly focusing on the period before the late 20th century shift in interpretation.',
    'If a significant divergence is established, it would weaken the ''lineage'' grounding of this reading''s authority, potentially opening avenues for re-interpretation or legislative action. If strong alignment is found, it would reinforce the current reading''s legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_vs_contemporary_reading, empirical, 'Assesses the historical fidelity of the individual right interpretation.').

omega_variable(
    causal_link_to_gun_violence,
    'What is the precise causal relationship between the broad interpretation of the individual right to bear arms and the incidence and lethality of gun violence (mass shootings, domestic violence, suicide)?',
    'Epidemiological studies, comparative analysis of firearm regulations and violence rates across jurisdictions, and econometric modeling controlling for confounding factors.',
    'Strong empirical evidence of a direct causal link would strengthen the case for regulatory intervention and challenge the ''self-defense'' justification for unrestricted access. Weak or ambiguous links would support arguments against further regulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_link_to_gun_violence, empirical, 'Quantifies the public safety impact of the individual right reading.').

omega_variable(
    reading_framing_ambiguity,
    'Is this constraint a genuine ''individual right'' as understood in constitutional law, or is it a ''collective right'' that has been re-framed as individual for political purposes?',
    'Conceptual analysis of rights theory, comparative constitutional law, and the political history of the Second Amendment''s interpretation. This is a conceptual, not empirical, question.',
    'A re-framing as a collective right would fundamentally alter the legal landscape, potentially allowing for much broader regulation. If it is affirmed as a genuine individual right, the current legal challenges would continue within the existing framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Examines the conceptual framing of the Second Amendment right.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_boundary__individual_right_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(seco_tr_t1985, second_amendment_boundary__individual_right_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_boundary__individual_right_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_boundary__individual_right_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__individual_right_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_boundary__individual_right_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(seco_be_t1985, second_amendment_boundary__individual_right_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(seco_be_t2000, second_amendment_boundary__individual_right_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(seco_be_t2010, second_amendment_boundary__individual_right_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__individual_right_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_boundary__individual_right_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(seco_su_t1985, second_amendment_boundary__individual_right_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(seco_su_t2000, second_amendment_boundary__individual_right_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(seco_su_t2010, second_amendment_boundary__individual_right_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__individual_right_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, firearms_market_regulation).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, public_safety_legislation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'second_amendment_boundary' kernel. It focuses on the individual right interpretation, distinct from the militia-conditioned and insurrectionist readings, each of which constitutes a separate constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
