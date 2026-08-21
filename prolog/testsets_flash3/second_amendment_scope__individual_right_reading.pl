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
 *   Amendment, which interprets the right to bear arms as belonging to
 *   individuals for self-defense and other lawful purposes, largely
 *   unconnected to militia service. This reading has gained prominence
 *   through landmark Supreme Court decisions (e.g., DC v. Heller, McDonald v.
 *   City of Chicago). The constraint is classified as a Tangled Rope because
 *   it provides a coordination function (clarity on individual rights) but
 *   also involves significant asymmetric extraction from state regulatory
 *   authority and public safety interests, requiring active enforcement
 *   through legal challenges.
 *
 * KEY AGENTS:
 *   - individual_firearms_owners: Primary beneficiary (organized/constrained)
 *   - firearms_manufacturers: Primary beneficiary (powerful/mobile)
 *   - firearms_lobby: Agenda setter (institutional/arbitrage)
 *   - state_legislatures: Primary payer (institutional/constrained)
 *   - gun_violence_victims: Primary payer (powerless/trapped)
 *   - public_safety_advocates: Payer (organized/constrained)
 *   - supreme_court: Agenda setter (institutional/analytical)
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
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, '0c0113fa-1f2f-4e75-850b-2a5f6084cb59').
narrative_ontology:cs_kernel_codification('0c0113fa-1f2f-4e75-850b-2a5f6084cb59', fixed_text).
narrative_ontology:cs_authority_grounding('0c0113fa-1f2f-4e75-850b-2a5f6084cb59', lineage).
narrative_ontology:cs_interpretation_layer_present('0c0113fa-1f2f-4e75-850b-2a5f6084cb59').
narrative_ontology:cs_reading_relation('0c0113fa-1f2f-4e75-850b-2a5f6084cb59', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c0113fa-1f2f-4e75-850b-2a5f6084cb59', second_amendment_scope__civic_right_reading, coexists_with).
narrative_ontology:cs_axiom('0c0113fa-1f2f-4e75-850b-2a5f6084cb59', foundational, individual_right_to_bear_arms_unconnected_to_militia).
narrative_ontology:cs_axiom_status(individual_right_to_bear_arms_unconnected_to_militia, holdable).
narrative_ontology:cs_axiom_grounding('0c0113fa-1f2f-4e75-850b-2a5f6084cb59', individual_right_to_bear_arms_unconnected_to_militia, deontological).
narrative_ontology:cs_axiom('0c0113fa-1f2f-4e75-850b-2a5f6084cb59', foundational, self_defense_as_fundamental_right).
narrative_ontology:cs_axiom_status(self_defense_as_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('0c0113fa-1f2f-4e75-850b-2a5f6084cb59', self_defense_as_fundamental_right, deontological).
narrative_ontology:cs_reference_frame('0c0113fa-1f2f-4e75-850b-2a5f6084cb59', post_heller_jurisprudence).
narrative_ontology:cs_drift_state('0c0113fa-1f2f-4e75-850b-2a5f6084cb59', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0c0113fa-1f2f-4e75-850b-2a5f6084cb59', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, individual_firearms_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_lobby).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_legislatures).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, gun_violence_victims).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, public_safety_advocates).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, self_defense_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, individual_liberty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the legal protection of their right to own firearms for various purposes, including self-defense, sport, and collection, without direct connection to militia service. They face costs from regulatory efforts but are empowered by the constitutional interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, individual_firearms_owners, beneficiary,
    organized, biographical, constrained, national).

% Benefit from an expansive market for firearms due to the individual right interpretation. They actively lobby to maintain and expand this interpretation, facing minimal direct costs from the constraint itself.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_manufacturers, beneficiary,
    powerful, generational, mobile, national).

% Actively shapes legal and political discourse to uphold and strengthen the individual right interpretation. They administer and enforce the constraint through legal challenges and political pressure, benefiting from its broad application.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_lobby, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the costs of constrained regulatory authority over firearms. Their efforts to enact public safety laws are frequently challenged and overturned based on this interpretation, leading to legislative and legal burdens.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_legislatures, payer,
    institutional, biographical, constrained, national).

% Suffer direct harm from gun violence, which they argue is exacerbated by the broad availability of firearms protected by this interpretation. They have minimal agency to alter the constraint's operation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_violence_victims, payer,
    powerless, immediate, trapped, local).

% Work to promote policies that reduce gun violence. They bear the costs of constant opposition from firearms rights groups and the legal system's deference to the individual right interpretation, making their advocacy efforts difficult.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, public_safety_advocates, payer,
    organized, generational, constrained, national).

% The ultimate arbiter of the Second Amendment's meaning, whose rulings (e.g., Heller, McDonald) established and reinforced the individual right interpretation. Its decisions set the legal framework for all other actors.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform legal standard for firearms ownership across states, providing clarity for individuals and manufacturers regarding their rights and responsibilities under federal law.
% TRANSFER_FUNCTION: Transfers regulatory authority over firearms from state and local governments to individual citizens, limiting the scope of public safety legislation and transferring the burden of self-protection to individuals.
% ABSENT_VOICES: The voices of future generations, who will inherit the consequences of current firearms policy, are absent from the immediate legal and political debates. Their interests in public safety and reduced violence are not directly represented.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, state and local governments would immediately gain broader authority to regulate firearms, leading to a patchwork of new laws, significant legal challenges, and a fundamental shift in the firearms market and public safety landscape.
% FOUNDING_PROBLEM: The Second Amendment was drafted to ensure the security of a free state, with a well-regulated militia seen as essential for this purpose, and to protect the right of the people to keep and bear arms.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the individual right reading argue it directly addresses the founding problem of individual self-defense and liberty. Opponents, including many historians and legal scholars, argue the original intent was primarily collective or civic, and that the individual right reading has expanded beyond the founding problem, as evidenced by historical legal interpretations and the context of the Bill of Rights.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68) because this interpretation significantly limits the ability of states to regulate firearms, imposing costs on public safety efforts and those affected by gun violence. Suppression is also high (0.75) as the legal framework actively suppresses alternative regulatory approaches and challenges to the individual right. The theater ratio is low (0.20) because the enforcement (legal challenges, lobbying) is genuinely aimed at maintaining the individual right, not merely performing a function. The temporal measurements show a clear trend of increasing extractiveness and suppression since 1970, reflecting the judicial expansion of this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individual firearms owners and the firearms industry, this constraint is a vital protection of liberty (a Rope or even a Mountain of natural right). From the perspective of state legislatures and public safety advocates, it is a significant impediment to governance and a source of harm (a Snare). The engine's classification as Tangled Rope reflects this hybrid nature, where a coordination function (rights clarity) is intertwined with asymmetric extraction and active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual firearms owners, manufacturers, and the firearms lobby are clear beneficiaries, experiencing low directionality as the constraint subsidizes their interests. State legislatures, gun violence victims, and public safety advocates are targets, experiencing high directionality as the constraint extracts regulatory power and imposes social costs. The Supreme Court, as the ultimate arbiter, acts as an agenda-setter, shaping the constraint's directionality for all other parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this as a pure Snare by acknowledging the genuine coordination function of clarifying individual rights, while simultaneously recognizing the asymmetric extraction from public safety interests. It also avoids mislabeling it as a pure Rope, which would ignore the significant costs and active suppression involved. The 'contested' status of the founding problem highlights the ongoing debate about whether the constraint's current operation aligns with its original mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_ambiguity,
    'Was the original intent of the Second Amendment primarily to protect an individual right to self-defense, or a collective/civic right related to militia service?',
    'Further historical and legal scholarship, potentially new judicial interpretations that re-evaluate the historical context and framers'' intent.',
    'If resolved as primarily collective/civic, the individual right reading''s legitimacy would erode, potentially leading to reclassification towards a Snare for individual owners and a Rope for state regulatory power. If resolved as unequivocally individual, the current classification would be reinforced, potentially shifting towards a Mountain for individual rights.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_intent_ambiguity, conceptual, 'Ambiguity regarding the Second Amendment''s original intent and its implications for individual vs. collective rights.').

omega_variable(
    public_safety_vs_individual_right_balance,
    'What is the optimal balance between individual firearms ownership rights and the state''s interest in public safety, and how does this reading achieve or fail to achieve it?',
    'Empirical studies on the effects of various firearms regulations on gun violence rates, coupled with public deliberation and legislative action to recalibrate policy.',
    'If the current balance is found to severely undermine public safety without proportional benefit, the extractiveness and suppression metrics would be re-evaluated upwards, pushing the classification closer to a Snare. If the balance is deemed appropriate, the current metrics would be affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_safety_vs_individual_right_balance, preference, 'The normative trade-off between individual rights and collective public safety.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_scope__individual_right_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(seco_tr_t1990, second_amendment_scope__individual_right_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__individual_right_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_scope__individual_right_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_scope__individual_right_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(seco_be_t1990, second_amendment_scope__individual_right_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__individual_right_reading, base_extractiveness, 2008, 0.65).
narrative_ontology:measurement(seco_be_t2024, second_amendment_scope__individual_right_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_scope__individual_right_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(seco_su_t1990, second_amendment_scope__individual_right_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__individual_right_reading, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement(seco_su_t2024, second_amendment_scope__individual_right_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, gun_control_legislation_enforcement).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, firearms_industry_regulation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'second_amendment_scope' kernel. It focuses on the individual right interpretation, which differs significantly in its beneficiary/victim structure and extractiveness from the 'collective_right_reading' and 'civic_right_reading' siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
