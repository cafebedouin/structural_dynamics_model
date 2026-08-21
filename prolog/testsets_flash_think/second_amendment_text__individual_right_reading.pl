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
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment: Individual Right to Bear Arms Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'individual right' reading of the
 *   Second Amendment, which interprets the operative clause as guaranteeing a
 *   personal right to bear arms for self-defense, largely independent of
 *   militia service. This reading gained significant legal prominence with
 *   the Supreme Court's decision in D.C. v. Heller (2008). It is a contested
 *   interpretation within the broader kernel of the Second Amendment text,
 *   with other readings emphasizing collective security or civic virtue.
 *
 * KEY AGENTS:
 *   - Individual gun owners: Primary beneficiaries (organized/constrained)
 *   - Firearms industry: Primary beneficiaries (institutional/arbitrage)
 *   - Disarmed populations: Primary victims (powerless/trapped)
 *   - Gun control advocates: Primary victims (organized/constrained)
 *   - Public safety officials: Secondary victims (institutional/constrained)
 *   - Courts: Agenda setters (institutional/analytical)
 *   - Legislators: Agenda setters (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.7).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.8).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment: Individual Right to Bear Arms Reading").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, '079e8256-5893-4a3a-99a1-4e9540176328').
narrative_ontology:cs_kernel_codification('079e8256-5893-4a3a-99a1-4e9540176328', fixed_text).
narrative_ontology:cs_authority_grounding('079e8256-5893-4a3a-99a1-4e9540176328', lineage).
narrative_ontology:cs_interpretation_layer_present('079e8256-5893-4a3a-99a1-4e9540176328').
narrative_ontology:cs_reading_relation('079e8256-5893-4a3a-99a1-4e9540176328', second_amendment_text__collective_security_reading, forecloses).
narrative_ontology:cs_reading_relation('079e8256-5893-4a3a-99a1-4e9540176328', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('079e8256-5893-4a3a-99a1-4e9540176328', foundational, individual_right_to_self_defense).
narrative_ontology:cs_axiom_status(individual_right_to_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('079e8256-5893-4a3a-99a1-4e9540176328', individual_right_to_self_defense, deontological).
narrative_ontology:cs_axiom('079e8256-5893-4a3a-99a1-4e9540176328', foundational, militia_clause_prefatory).
narrative_ontology:cs_axiom_status(militia_clause_prefatory, holdable).
narrative_ontology:cs_axiom_grounding('079e8256-5893-4a3a-99a1-4e9540176328', militia_clause_prefatory, conventional).
narrative_ontology:cs_reference_frame('079e8256-5893-4a3a-99a1-4e9540176328', post_heller_interpretation).
narrative_ontology:cs_drift_state('079e8256-5893-4a3a-99a1-4e9540176328', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('079e8256-5893-4a3a-99a1-4e9540176328', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, disarmed_populations).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, gun_control_advocates).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, public_safety_officials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the legal protection of their right to own firearms for self-defense, often actively resisting new regulations. Their exit options are constrained by the legal framework, but they are highly mobilized to defend their rights.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, constrained, national).

% Benefits from an expanded market for firearms and accessories due to the broad interpretation of the individual right. They actively lobby and litigate to maintain and expand this interpretation, facing few direct costs from the constraint itself.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, firearms_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Individuals legally prohibited from owning firearms (e.g., convicted felons, those with domestic violence restraining orders) bear the cost of being disarmed, often facing severe penalties for possession. They have virtually no exit options from this status.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, disarmed_populations, payer,
    powerless, immediate, trapped, local).

% Bear the cost of legislative and legal defeats in their efforts to regulate firearms. Their options are limited to continued advocacy, litigation, and electoral politics, facing an uphill battle against established legal precedent.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_control_advocates, payer,
    organized, generational, constrained, national).

% Face increased challenges in managing gun violence and enforcing public order due to the broad availability of firearms. Their ability to implement effective safety measures is constrained by the individual right interpretation, leading to higher operational costs and risks.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, public_safety_officials, payer,
    institutional, biographical, constrained, local).

% Interpret and enforce the Second Amendment, shaping the scope of the individual right through landmark rulings. They are the primary arbiters of the constraint's meaning and application.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Propose and pass laws related to firearms, but their actions are heavily constrained by judicial interpretations of the Second Amendment. They navigate intense political pressure from both gun rights and gun control advocates.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, legislators, agenda_setter,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__individual_right_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_text__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the right of individual citizens to possess firearms for self-defense, providing a clear legal framework for gun ownership and limiting state power to disarm the populace.
% TRANSFER_FUNCTION: Transfers the authority over firearms from the state to the individual, limiting legislative and executive power to regulate, and imposing costs on those seeking stricter gun control or those legally disarmed.
% ABSENT_VOICES: Victims of gun violence and their families are often marginalized in the legal and political discourse, as their experiences are framed as individual tragedies rather than systemic failures addressable by regulation. Future generations, who will inherit the consequences of current policy, also lack a direct voice.
% DISAPPEARANCE_RATIONALE: If the individual right interpretation of the Second Amendment vanished overnight, it would fundamentally alter the legal landscape of firearms ownership in the United States. Extensive gun control legislation would likely be enacted, dramatically reducing gun availability, changing public safety dynamics, and shifting the balance of power between citizens and the state.
% FOUNDING_PROBLEM: To ensure the capacity for individual self-defense and to provide a check against potential government overreach, rooted in post-revolutionary fears of standing armies and the need for a citizen militia.
% FOUNDING_PROBLEM_CORROBORATION: Gun rights advocates and some historical interpretations attest that the founding problem (individual self-defense, check on tyranny) remains live. Gun control advocates and other historians argue that the original problem (citizen militia) is largely dead or transformed in modern society, and the individual right interpretation is a modern construct. Legal scholars and historians outside the immediate advocacy groups offer diverse, often conflicting, corroboration.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The claimed type is 'tangled_rope' because this reading genuinely coordinates the rights of individual gun owners (a coordination function) but simultaneously imposes significant costs and restrictions on other groups (disarmed populations, gun control advocates, public safety) through the same legal structure. The high extractiveness (0.70) reflects the costs borne by victims, including the societal costs of gun violence and the political costs of legislative gridlock. Suppression (0.80) is high due to the active legal and political enforcement required to maintain this interpretation against challenges, effectively suppressing alternative regulatory approaches. Theater ratio (0.20) is low because the enforcement and consequences of this interpretation are very real, not merely performative. Accessibility collapse (0.60) is moderate-high, as legal avenues for comprehensive gun control are significantly narrowed. Resistance (0.75) is high, reflecting ongoing efforts by gun control advocates and affected communities to challenge this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The individual right reading is experienced as a fundamental protection by gun owners and the firearms industry, who see it as a 'rope' or even a 'mountain' (an unalienable right). Conversely, disarmed populations, gun control advocates, and public safety officials experience the same constraint as a 'snare' due to the costs and limitations it imposes. The engine's per-seat classification will reflect this divergence based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry are clear beneficiaries, as the constraint protects and expands their ability to own and sell firearms, respectively (low directionality). Disarmed populations, gun control advocates, and public safety officials are targets, bearing the direct and indirect costs of this interpretation (high directionality). Courts and legislators, while agenda setters, also face constraints in their ability to act, placing them closer to symmetric or moderately targeted depending on their specific actions.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a 'tangled_rope' prevents mislabeling it as a 'pure rope' (which would ignore the significant extraction from victims) or a 'pure snare' (which would ignore the genuine coordination function for gun owners). The classification highlights the dual nature of the constraint: a coordination mechanism for one group that simultaneously extracts from others, requiring active enforcement to persist. The 'contested' status of the founding problem further supports the 'tangled_rope' classification, indicating that while an original coordination problem may have existed, its current operation involves significant asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, independent individual right, or is its scope and nature fundamentally conditioned by the ''well regulated Militia'' clause?',
    'Further Supreme Court rulings clarifying the relationship between the prefatory and operative clauses, or a constitutional amendment. Historical and legal scholarship can inform, but not definitively resolve, this interpretive question.',
    'If the right is found to be conditioned by militia service, the constraint''s scope would narrow, potentially reducing extractiveness on gun control advocates and disarmed populations, and shifting its classification towards a ''collective security'' framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity regarding the independence of the individual right from militia service.').

omega_variable(
    victim_set_necessity,
    'Is the disarming of certain populations (e.g., felons, domestic abusers) a necessary and proportional consequence of the individual right, or an extractive feature that could be mitigated without undermining the core right?',
    'Empirical studies on the effectiveness and proportionality of current prohibitions, and legal challenges testing the boundaries of ''who'' constitutes ''the people'' in the Second Amendment context.',
    'If current prohibitions are found to be overly broad or disproportionate, the victim set might shrink, or the nature of their ''trapped'' exit options could be re-evaluated, potentially reducing the overall extractiveness of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_necessity, empirical, 'Whether the disarming of specific populations is a necessary or extractive component.').

omega_variable(
    framing_underdetermination_individual_right,
    'Is the ''individual right'' framing the only defensible interpretation of the Second Amendment, or would an alternative framing (e.g., ''collective security'' or ''civic virtue'') produce a different structural classification?',
    'Analysis of the structural implications of alternative readings, as instantiated in separate constraint stories. The divergence in classifications across these readings would highlight the impact of framing on perceived constraint type.',
    'Adopting a ''collective security'' or ''civic virtue'' framing would likely shift the constraint''s classification towards a ''rope'' or even ''mountain'' for the state''s regulatory power, and reduce the perceived extractiveness on gun control advocates, while potentially increasing it for individual gun owners.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_individual_right, conceptual, 'Framing under-determination between individual right, collective security, and civic virtue readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__individual_right_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(seco_tr_t1850, second_amendment_text__individual_right_reading, theater_ratio, 1850, 0.1).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_text__individual_right_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(seco_tr_t1950, second_amendment_text__individual_right_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_text__individual_right_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_text__individual_right_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__individual_right_reading, base_extractiveness, 1791, 0.3).
narrative_ontology:measurement(seco_be_t1850, second_amendment_text__individual_right_reading, base_extractiveness, 1850, 0.35).
narrative_ontology:measurement(seco_be_t1900, second_amendment_text__individual_right_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(seco_be_t1950, second_amendment_text__individual_right_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(seco_be_t2000, second_amendment_text__individual_right_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(seco_be_t2024, second_amendment_text__individual_right_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_text__individual_right_reading, suppression_requirement, 1791, 0.2).
narrative_ontology:measurement(seco_su_t1850, second_amendment_text__individual_right_reading, suppression_requirement, 1850, 0.25).
narrative_ontology:measurement(seco_su_t1900, second_amendment_text__individual_right_reading, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement(seco_su_t1950, second_amendment_text__individual_right_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(seco_su_t2000, second_amendment_text__individual_right_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(seco_su_t2024, second_amendment_text__individual_right_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, gun_control_legislation).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, public_safety_policy).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'second_amendment_text' kernel. Its ε value and structural properties differ significantly from the 'collective_security_reading' and 'originalist_civic_virtue_reading' siblings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
