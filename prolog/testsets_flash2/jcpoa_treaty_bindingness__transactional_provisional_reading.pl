% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__transactional_provisional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__transactional_provisional_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA as Provisional Transactional Framework (Transactional-Provisional Reading)
 *   domain: international_law/nuclear_non_proliferation
 *
 * SUMMARY:
 *   This constraint models the Joint Comprehensive Plan of Action (JCPOA)
 *   through a 'transactional-provisional' reading, where the agreement is
 *   seen as a temporary, revocable framework. Under this interpretation,
 *   states retain a low constraint on unilateral withdrawal, sanctions
 *   reimposition follows national determination of bad faith, and Iranian
 *   violations justify immediate exit. The primary beneficiaries are
 *   individual state sovereignty and domestic political coalitions that
 *   oppose the deal, as it grants them flexibility and political leverage.
 *   The Iranian economy and advocates for multilateral diplomacy are the
 *   primary victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.65).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.7).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, snare).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA as Provisional Transactional Framework (Transactional-Provisional Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_non_proliferation").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, '31f22179-e6e4-4d07-bb82-61603e2f2e96').
narrative_ontology:cs_kernel_codification('31f22179-e6e4-4d07-bb82-61603e2f2e96', formalized).
narrative_ontology:cs_authority_grounding('31f22179-e6e4-4d07-bb82-61603e2f2e96', extraction).
narrative_ontology:cs_interpretation_layer_present('31f22179-e6e4-4d07-bb82-61603e2f2e96').
narrative_ontology:cs_reading_relation('31f22179-e6e4-4d07-bb82-61603e2f2e96', jcpoa_treaty_bindingness__binding_multilateral_reading, influences).
narrative_ontology:cs_reading_relation('31f22179-e6e4-4d07-bb82-61603e2f2e96', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('31f22179-e6e4-4d07-bb82-61603e2f2e96', foundational, state_sovereignty_trumps_multilateral_treaty).
narrative_ontology:cs_axiom_status(state_sovereignty_trumps_multilateral_treaty, holdable).
narrative_ontology:cs_axiom_grounding('31f22179-e6e4-4d07-bb82-61603e2f2e96', state_sovereignty_trumps_multilateral_treaty, conventional).
narrative_ontology:cs_axiom('31f22179-e6e4-4d07-bb82-61603e2f2e96', foundational, unilateral_determination_of_bad_faith_justifies_exit).
narrative_ontology:cs_axiom_status(unilateral_determination_of_bad_faith_justifies_exit, holdable).
narrative_ontology:cs_axiom_grounding('31f22179-e6e4-4d07-bb82-61603e2f2e96', unilateral_determination_of_bad_faith_justifies_exit, conventional).
narrative_ontology:cs_reference_frame('31f22179-e6e4-4d07-bb82-61603e2f2e96', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('31f22179-e6e4-4d07-bb82-61603e2f2e96', post_withdrawal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('31f22179-e6e4-4d07-bb82-61603e2f2e96', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, state_sovereignty_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_political_coalitions_opposing_deal).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_economy).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_diplomacy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the interpretation that allows unilateral withdrawal based on national interest, reinforcing the principle of state sovereignty over international agreements. This group includes political factions and legal scholars who prioritize national autonomy.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, state_sovereignty_advocates, beneficiary,
    institutional, generational, arbitrage, national).

% Gains political capital and policy influence by advocating for and executing unilateral withdrawal, aligning with their ideological stance against the deal. Their benefit is primarily political and short-term.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_political_coalitions_opposing_deal, beneficiary,
    organized, immediate, mobile, national).

% Bears the direct costs of sanctions reimposition following unilateral withdrawal, leading to economic hardship, reduced trade, and isolation. Its options are severely limited by international financial systems and political pressure.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_economy, payer,
    powerless, biographical, trapped, national).

% Suffers a setback to their efforts to resolve international disputes through consensus-based, binding agreements. The unilateral voiding of the deal undermines the credibility and effectiveness of multilateral institutions and diplomatic processes.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_diplomacy_advocates, payer,
    moderate, generational, constrained, global).

% Monitors Iran's nuclear program and verifies compliance with the JCPOA. From this reading, its verification efforts become less relevant if the framework is unilaterally voidable, shifting its role from compliance enforcer to intelligence gatherer.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, international_atomic_energy_agency, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a temporary, conditional framework for managing nuclear proliferation risks with Iran, allowing for transactional engagement as long as perceived benefits outweigh costs for individual states.
% TRANSFER_FUNCTION: Transfers the burden of compliance and economic costs to Iran, while transferring the flexibility of engagement and the right to unilateral action to states interpreting the deal provisionally.
% ABSENT_VOICES: Advocates for a stronger, more permanent international non-proliferation regime, whose concerns about the precedent set by unilateral withdrawal are sidelined by the transactional interpretation. Also, Iranian civil society, which bears the brunt of economic sanctions but has little voice in international policy.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, states would be more constrained in their ability to unilaterally withdraw from international agreements, leading to a shift towards more robust multilateral frameworks and potentially altering the dynamics of international treaty negotiations.
% FOUNDING_PROBLEM: The perceived threat of Iran developing nuclear weapons, coupled with a desire to avoid military conflict and manage regional instability through a temporary, reversible agreement.
% FOUNDING_PROBLEM_CORROBORATION: The transactional nature of the deal was explicitly framed by some signatories as a temporary measure, and domestic political debates in several countries consistently highlighted the provisional and revocable aspects. This is corroborated by public statements from political leaders and legislative records from outside the immediate beneficiaries of this reading.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because this reading allows for the imposition of severe economic costs on Iran based on unilateral determinations, without requiring a multilateral consensus. Suppression (0.7) is also high, as the threat of sanctions is a powerful coercive tool. The theater ratio (0.4) reflects that while some diplomatic engagement occurs, a significant portion of the framework's operation is performative, maintaining the option for unilateral action rather than genuinely seeking long-term multilateral coordination. The claimed type is 'snare' because the coordination story (managing proliferation) is largely cover for the unilateral extraction and suppression mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries of this reading perceive it as a necessary safeguard of national interest and a flexible tool for managing threats. The victims, however, experience it as a coercive mechanism that undermines international law and inflicts economic harm without proportional multilateral accountability. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State sovereignty advocates and domestic political coalitions are clear beneficiaries (low d) as this reading empowers their policy preferences and political agendas. The Iranian economy is a direct target (high d) due to the vulnerability to sanctions. Multilateral diplomacy advocates are also targets (high d) as their preferred mode of international engagement is undermined. The IAEA, while an observer, finds its role diminished, pushing its d towards the target end as its mandate is made contingent on unilateral political will.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unilateral_determination_legitimacy,
    'Is a unilateral determination of ''bad faith'' a legitimate basis for voiding an international agreement, or does it require multilateral consensus or a formal dispute resolution process?',
    'Analysis of international legal precedents regarding treaty withdrawal and the role of UN Security Council resolutions in validating such actions.',
    'If unilateral determination is deemed illegitimate, the constraint''s suppression and extractiveness are revealed as purely coercive, reclassifying it more firmly as a Snare. If legitimate, it reinforces the ''transactional'' aspect, but still highlights the asymmetric power dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unilateral_determination_legitimacy, conceptual, 'Ambiguity regarding the legal basis for unilateral withdrawal from the JCPOA.').

omega_variable(
    proliferation_risk_management_efficacy,
    'Does this transactional-provisional reading effectively manage nuclear proliferation risks, or does its inherent instability increase long-term risks?',
    'Empirical analysis of Iran''s nuclear activities and regional stability trends under this interpretation versus alternative scenarios (e.g., full compliance, no deal).',
    'If it increases risk, the coordination function claimed by this reading is revealed as theatrical, pushing the constraint closer to a pure Snare. If it demonstrably manages risk, it retains a minimal coordination component, albeit highly extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_risk_management_efficacy, empirical, 'Whether the provisional nature of the deal genuinely contributes to non-proliferation or exacerbates risks.').

omega_variable(
    domestic_political_vs_international_law,
    'To what extent does domestic political expediency override international legal obligations in this reading, and what are the long-term consequences for international law?',
    'Comparative legal analysis of state practice and scholarly commentary on the hierarchy of domestic political will versus international treaty law.',
    'If domestic politics consistently overrides international law, this reading contributes to a systemic erosion of treaty bindingness, making all similar agreements more fragile and extractive. This would shift the classification towards a broader ''Snare'' for the international system itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_political_vs_international_law, preference, 'The tension between national political interests and the integrity of international legal frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(jcpo_tr_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(jcpo_be_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(jcpo_su_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_nuclear_program_sanctions_regime).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the JCPOA treaty bindingness kernel. Its transactional-provisional interpretation directly influences the viability and perceived legitimacy of other readings, particularly by setting a precedent for unilateral withdrawal and sanctions reimposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
