% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute Westphalian Sovereignty (Non-Interference Principle)
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint represents the 'absolute sovereignty' reading of the
 *   Westphalian principle, asserting that states have unconditional authority
 *   over their domestic affairs and external interference is categorically
 *   illegitimate. This reading prioritizes state autonomy and
 *   non-intervention, often at the expense of human rights or international
 *   accountability. It is one reading of the broader
 *   'westphalian_sovereignty' kernel, alongside 'conditional_sovereignty' and
 *   'graduated_sovereignty'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.55).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.7).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.55).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute Westphalian Sovereignty (Non-Interference Principle)").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, '84694524-684a-4c9e-9c35-da95e711ebdb').
narrative_ontology:cs_kernel_codification('84694524-684a-4c9e-9c35-da95e711ebdb', formalized).
narrative_ontology:cs_authority_grounding('84694524-684a-4c9e-9c35-da95e711ebdb', lineage).
narrative_ontology:cs_interpretation_layer_present('84694524-684a-4c9e-9c35-da95e711ebdb').
narrative_ontology:cs_reading_relation('84694524-684a-4c9e-9c35-da95e711ebdb', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('84694524-684a-4c9e-9c35-da95e711ebdb', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('84694524-684a-4c9e-9c35-da95e711ebdb', foundational, state_autonomy_is_absolute).
narrative_ontology:cs_axiom_status(state_autonomy_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('84694524-684a-4c9e-9c35-da95e711ebdb', state_autonomy_is_absolute, deontological).
narrative_ontology:cs_axiom('84694524-684a-4c9e-9c35-da95e711ebdb', foundational, non_interference_is_categorical).
narrative_ontology:cs_axiom_status(non_interference_is_categorical, holdable).
narrative_ontology:cs_axiom_grounding('84694524-684a-4c9e-9c35-da95e711ebdb', non_interference_is_categorical, conventional).
narrative_ontology:cs_reference_frame('84694524-684a-4c9e-9c35-da95e711ebdb', post_westphalian_state_system).
narrative_ontology:cs_drift_state('84694524-684a-4c9e-9c35-da95e711ebdb', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('84694524-684a-4c9e-9c35-da95e711ebdb', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, states_seeking_autonomy).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, international_human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shielded from external scrutiny or intervention regarding domestic policies, including human rights abuses. They actively invoke this principle to deflect criticism and maintain internal control without accountability to external actors.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes, beneficiary,
    institutional, generational, arbitrage, national).

% Benefit from the principle of non-interference, allowing them to pursue independent domestic and foreign policies without external coercion. They see it as a bulwark against neo-colonialism or great power dominance.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, states_seeking_autonomy, beneficiary,
    institutional, generational, mobile, national).

% Bear the costs of unchecked state power, including human rights violations, lack of political freedoms, and economic exploitation, with no legitimate avenue for external relief or intervention. Their suffering is often rendered invisible by the non-interference norm.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression, payer,
    powerless, immediate, trapped, national).

% Struggle to hold states accountable for human rights abuses when those states invoke absolute sovereignty. Their advocacy is often dismissed as illegitimate interference, limiting their ability to effect change.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_human_rights_advocates, payer,
    organized, generational, constrained, global).

% Nominally responsible for international peace and security, but its actions are often constrained by the absolute sovereignty principle, particularly when permanent members invoke it to protect allies or their own interests. It can authorize interventions but faces significant political hurdles.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, united_nations_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Often find themselves in a dilemma, balancing the principle of non-interference with their stated commitment to human rights. They may criticize abuses but are hesitant to intervene directly due to the legal and political implications of violating sovereignty.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, liberal_democracies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for international relations based on mutual recognition of state authority and non-interference, aiming to prevent interstate conflict arising from internal affairs.
% TRANSFER_FUNCTION: Transfers the right to internal self-determination and non-accountability for domestic actions from the international community to individual states, particularly those with repressive regimes. It also transfers the burden of suffering from state actions to domestic populations.
% ABSENT_VOICES: Populations suffering under repressive regimes, who would advocate for a right to external protection or intervention, are systematically excluded from the international legal discourse that upholds absolute sovereignty.
% DISAPPEARANCE_RATIONALE: If the principle of absolute sovereignty vanished overnight, the international system would undergo a profound rearrangement. States would lose their primary shield against external scrutiny, leading to potential interventions, shifts in global power dynamics, and a redefinition of international law and human rights enforcement.
% FOUNDING_PROBLEM: The Thirty Years' War and subsequent conflicts demonstrated the destructive potential of religious and political interference in the internal affairs of other states, necessitating a principle to stabilize interstate relations.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international relations scholars widely corroborate the historical context of the Treaty of Westphalia. Many states, particularly those in the Global South, continue to attest that the principle remains vital for protecting their autonomy against powerful external actors, even as human rights advocates contest its absolute application.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) is substantial because this reading allows states to operate without external checks, enabling internal repression and rent-seeking from their populations. Suppression (0.7) is high as it actively legitimizes the suppression of internal dissent by framing external support for such dissent as illegitimate interference. Theater ratio (0.2) is relatively low, as the principle is genuinely invoked and enforced by states, though its stated purpose of preventing interstate conflict can sometimes mask its use as a shield for internal abuses.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authoritarian regimes, this is a legitimate 'rope' ensuring national self-determination. From the perspective of repressed populations, it is a 'snare' that traps them without recourse. The engine's classification will reflect this divergence based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes and states prioritizing autonomy are clear beneficiaries, as the constraint shields them from external accountability. Domestic populations under repression and international human rights advocates are victims, as their ability to seek redress or intervention is severely curtailed. The UN Security Council acts as an agenda-setter, but its actions are often constrained by this reading, reflecting a 'tangled rope' dynamic where coordination (preventing interstate war) is intertwined with extraction (shielding repressive regimes).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to prevent interstate conflict by establishing clear boundaries of state authority. While this function remains live, the 'absolute sovereignty' reading has arguably outlived its utility in a globalized world where human rights are increasingly seen as universal. The persistence of this reading, despite its costs to human rights, suggests a degree of mandatrophy where the original coordination function is now intertwined with, and often overshadowed by, its extractive shielding function for certain regimes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolute_vs_conditional_sovereignty,
    'Is state sovereignty truly absolute, or is it conditional upon adherence to international human rights norms?',
    'Evolution of international customary law and treaty interpretations, particularly regarding the Responsibility to Protect (R2P) doctrine. Empirical observation of state practice regarding intervention in cases of mass atrocities.',
    'If sovereignty is deemed conditional, this constraint would shift towards a ''tangled rope'' or ''snare'' for states violating human rights, and a ''rope'' or ''scaffold'' for populations seeking protection, significantly altering its classification and the legitimacy of external intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolute_vs_conditional_sovereignty, conceptual, 'Ambiguity regarding the scope and limits of state sovereignty in international law.').

omega_variable(
    legitimacy_of_non_interference,
    'Does the principle of non-interference genuinely prevent interstate conflict, or does it merely defer or enable internal conflicts and human rights abuses?',
    'Comparative historical analysis of regions with strong non-interference norms versus those with more interventionist approaches. Causal inference studies on the relationship between non-interference and internal stability/human rights outcomes.',
    'If non-interference is found to enable abuses without preventing conflict, the coordination function of this constraint would be severely undermined, pushing its classification closer to a ''snare'' due to its high extractiveness and suppression of victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_non_interference, empirical, 'The actual efficacy of non-interference in achieving its stated coordination goal versus its unintended consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0, 0.15).
narrative_ontology:measurement(west_tr_t10, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 10, 0.16).
narrative_ontology:measurement(west_tr_t20, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 20, 0.17).
narrative_ontology:measurement(west_tr_t30, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 30, 0.18).
narrative_ontology:measurement(west_tr_t40, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 40, 0.19).
narrative_ontology:measurement(west_tr_t50, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 50, 0.2).
narrative_ontology:measurement(west_tr_t60, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 60, 0.2).
narrative_ontology:measurement(west_tr_t70, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 70, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(west_be_t10, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(west_be_t20, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(west_be_t30, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(west_be_t40, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(west_be_t50, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 50, 0.54).
narrative_ontology:measurement(west_be_t60, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(west_be_t70, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 70, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(west_su_t10, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(west_su_t20, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(west_su_t30, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(west_su_t40, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(west_su_t50, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 50, 0.69).
narrative_ontology:measurement(west_su_t60, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(west_su_t70, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 70, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, international_human_rights_law).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, responsibility_to_protect_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is the 'absolute_sovereignty' reading of the 'westphalian_sovereignty' kernel. It is linked to 'conditional_sovereignty' and 'graduated_sovereignty' as sibling readings that offer alternative interpretations of state authority and intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
