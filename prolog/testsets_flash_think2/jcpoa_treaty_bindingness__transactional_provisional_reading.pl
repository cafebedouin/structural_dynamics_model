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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA as Provisional Transactional Framework (Unilateral Voidability Reading)
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   This constraint story models the Joint Comprehensive Plan of Action
 *   (JCPOA) from the perspective of a 'transactional provisional framework
 *   voidable upon unilateral determination of bad faith.' This reading
 *   emphasizes the right of a state to withdraw from the agreement based on
 *   its own assessment, rather than requiring multilateral consensus or a
 *   graduated response. It views the agreement as a temporary, flexible
 *   arrangement, not a binding treaty in the traditional sense. The high
 *   extractiveness and suppression reflect the costs imposed on other parties
 *   and the multilateral system by this unilateral interpretation, even
 *   though the claimed type is 'scaffold' due to its 'provisional' nature.
 *
 * KEY AGENTS:
 *   - state_asserting_unilateral_exit: Agenda-setter (institutional/arbitrage) — benefits from flexibility
 *   - domestic_political_factions_opposing_deal: Beneficiary (organized/mobile) — benefits from policy alignment
 *   - iran: Payer (powerful/trapped) — bears risk of unilateral action
 *   - p5_plus_1_members_seeking_stability: Payer (institutional/constrained) — bears cost of undermined multilateralism
 *   - multilateral_diplomacy_institutions: Excluded (institutional/constrained) — sidelined by unilateralism
 *   - international_atomic_energy_agency: Observer (institutional/analytical) — technical role subject to political interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.78).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.85).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, scaffold).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA as Provisional Transactional Framework (Unilateral Voidability Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:has_sunset_clause(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, 'ac0c416b-b87f-4538-9d8c-e23ee1d0f39b').
narrative_ontology:cs_kernel_codification('ac0c416b-b87f-4538-9d8c-e23ee1d0f39b', fixed_text).
narrative_ontology:cs_authority_grounding('ac0c416b-b87f-4538-9d8c-e23ee1d0f39b', extraction).
narrative_ontology:cs_interpretation_layer_present('ac0c416b-b87f-4538-9d8c-e23ee1d0f39b').
narrative_ontology:cs_reading_relation('ac0c416b-b87f-4538-9d8c-e23ee1d0f39b', jcpoa_treaty_bindingness__binding_multilateral_reading, forecloses).
narrative_ontology:cs_reading_relation('ac0c416b-b87f-4538-9d8c-e23ee1d0f39b', jcpoa_treaty_bindingness__graduated_compliance_reading, forecloses).
narrative_ontology:cs_axiom('ac0c416b-b87f-4538-9d8c-e23ee1d0f39b', foundational, state_sovereignty_over_treaty_bindingness).
narrative_ontology:cs_axiom_status(state_sovereignty_over_treaty_bindingness, holdable).
narrative_ontology:cs_axiom_grounding('ac0c416b-b87f-4538-9d8c-e23ee1d0f39b', state_sovereignty_over_treaty_bindingness, deontological).
narrative_ontology:cs_axiom('ac0c416b-b87f-4538-9d8c-e23ee1d0f39b', foundational, unilateral_determination_of_bad_faith_is_sufficient_cause_for_exit).
narrative_ontology:cs_axiom_status(unilateral_determination_of_bad_faith_is_sufficient_cause_for_exit, holdable).
narrative_ontology:cs_axiom_grounding('ac0c416b-b87f-4538-9d8c-e23ee1d0f39b', unilateral_determination_of_bad_faith_is_sufficient_cause_for_exit, conventional).
narrative_ontology:cs_reference_frame('ac0c416b-b87f-4538-9d8c-e23ee1d0f39b', unilateral_sovereignty_framework).
narrative_ontology:cs_drift_state('ac0c416b-b87f-4538-9d8c-e23ee1d0f39b', post_withdrawal_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ac0c416b-b87f-4538-9d8c-e23ee1d0f39b', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, state_asserting_unilateral_exit).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_political_factions_opposing_deal).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iran).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, p5_plus_1_members_seeking_stability).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_diplomacy_institutions).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__transactional_provisional_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__transactional_provisional_reading, national_interest_first_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state (e.g., US under a specific administration) that interprets the JCPOA as a provisional framework, voidable upon its unilateral determination of bad faith by Iran. It benefits from the flexibility to withdraw and reimpose sanctions, aligning with its domestic political agenda.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, state_asserting_unilateral_exit, agenda_setter,
    institutional, biographical, arbitrage, global).

% Political groups within the state asserting unilateral exit who opposed the JCPOA from its inception. They benefit from an interpretation that allows for easy withdrawal, validating their prior criticisms and strengthening their political position.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_political_factions_opposing_deal, beneficiary,
    organized, biographical, mobile, national).

% The primary target of the nuclear non-proliferation regime. Under this reading, Iran bears the risk of unilateral withdrawal and sanctions reimposition, even if it complies with the agreement, as its fate is subject to another state's 'bad faith' determination.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iran, payer,
    powerful, generational, trapped, national).

% Other signatory states (e.g., EU3, Russia, China) who view the JCPOA as a binding multilateral agreement and seek its stability. They bear the cost of the unilateral interpretation, which undermines the agreement's integrity and multilateral diplomatic efforts.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, p5_plus_1_members_seeking_stability, payer,
    institutional, generational, constrained, global).

% International bodies and norms that promote consensus-based, binding treaty frameworks. This reading sidelines their role in dispute resolution and treaty modification, effectively excluding their preferred mode of operation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_diplomacy_institutions, excluded,
    institutional, generational, constrained, global).

% The technical verification body for Iran's nuclear program. While it continues its monitoring role, its findings are subject to political interpretation, and its authority to certify compliance is undermined by unilateral 'bad faith' determinations.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, international_atomic_energy_agency, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__transactional_provisional_reading, state_asserting_unilateral_exit).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__transactional_provisional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a temporary, transactional framework for managing Iran's nuclear program, allowing for a pause in proliferation activities in exchange for sanctions relief, with a clear, albeit unilateral, exit mechanism for one party.
% TRANSFER_FUNCTION: Transfers the burden of maintaining the agreement's stability and the risk of its collapse to Iran and other P5+1 members, while transferring significant leverage and policy flexibility to the state asserting unilateral exit.
% ABSENT_VOICES: The voice of a truly binding international legal framework, and the voice of a consensus-based multilateral approach to treaty enforcement, are structurally excluded by this unilateralist reading. They would argue for adherence to international law and collective decision-making.
% DISAPPEARANCE_RATIONALE: If this provisional framework (as interpreted) vanished, the international community would immediately face a severe crisis regarding Iran's nuclear program, requiring new, potentially more confrontational, diplomatic or coercive arrangements. The existing non-proliferation architecture would be significantly destabilized.
% FOUNDING_PROBLEM: To prevent Iran from developing nuclear weapons, ensure the peaceful nature of its nuclear program, and provide a diplomatic resolution to a long-standing international security concern.
% FOUNDING_PROBLEM_CORROBORATION: The IAEA and some P5+1 members attest to the deal's initial effectiveness in curbing Iran's nuclear program. The state asserting unilateral exit and its domestic factions dispute its effectiveness or necessity, often citing Iran's other regional activities or the deal's perceived flaws, with supporting arguments from think tanks and political commentators aligned with their views.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The `claimed_type` is 'scaffold' because this reading defines the JCPOA as a 'provisional transactional framework,' implying a temporary and transitional nature. However, the metrics reflect the highly extractive and suppressive operation of this specific interpretation. `Extractiveness` is high (0.78) because the unilateral voidability imposes significant costs and uncertainty on Iran and other P5+1 members who seek stability. `Suppression` is very high (0.85) as this reading actively suppresses alternative interpretations of treaty bindingness and multilateral enforcement mechanisms. `Theater_ratio` is low (0.15) because the actions taken under this interpretation (e.g., sanctions reimposition) have real, non-performative consequences. The temporal measurements show a sharp increase in extractiveness and suppression around year 3, reflecting a hypothetical unilateral withdrawal and subsequent enforcement actions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'state_asserting_unilateral_exit' and 'domestic_political_factions_opposing_deal', this framework is a flexible tool that serves national interests, allowing for a temporary arrangement with an easy exit. For them, it functions more like a 'rope' or 'scaffold' that coordinates their policy goals. However, from the perspective of 'iran' and 'p5_plus_1_members_seeking_stability', the same structure operates as a highly extractive and suppressive mechanism, akin to a 'snare' or 'tangled_rope', due to the imposed uncertainty and lack of recourse against unilateral action. The engine will compute these divergent classifications based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'state_asserting_unilateral_exit' and 'domestic_political_factions_opposing_deal' are clear beneficiaries, as the constraint's provisional nature and voidability serve their interests (low d). 'Iran' and 'p5_plus_1_members_seeking_stability' are targets, bearing the costs of uncertainty and undermined multilateralism (high d). 'Multilateral_diplomacy_institutions' are excluded, as their preferred mode of operation is suppressed. The 'international_atomic_energy_agency' is an analytical observer.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a highly asymmetric and potentially destabilizing interpretation as a benign 'scaffold' by highlighting the high extractiveness and suppression. While the 'provisional' nature aligns with a scaffold's temporality, the 'unilateral determination of bad faith' mechanism introduces a level of coercive power that pushes the effective classification towards a more extractive type for most parties. The 'founding_problem_status' being 'contested' further supports the idea that the original mandate is no longer universally accepted, contributing to the constraint's contentious operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''transactional_provisional_reading'' of the ''jcpoa_treaty_bindingness'' kernel?',
    'Analysis of official statements, policy documents, and legal interpretations from the ''state_asserting_unilateral_exit'' to confirm the explicit or implicit adoption of this specific reading.',
    'If misidentified, the entire analysis of this constraint''s structural properties and its relations to sibling readings would be invalid, requiring re-classification under a different kernel or reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific kernel reading being analyzed.').

omega_variable(
    unilateral_determination_legitimacy,
    'Is the ''unilateral determination of bad faith'' a legitimate basis for voiding an international agreement under customary international law or the Vienna Convention on the Law of Treaties?',
    'Legal analysis by international law experts and rulings by international courts (e.g., ICJ) on similar cases of unilateral treaty withdrawal.',
    'If deemed illegitimate, the ''state_asserting_unilateral_exit''s'' actions would be reclassified as a breach of international law, increasing the effective extractiveness and suppression of the constraint from the perspective of other parties and the international legal order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_determination_legitimacy, conceptual, 'Legitimacy of unilateral bad faith determination in international law.').

omega_variable(
    impact_on_multilateral_norms,
    'To what extent does this ''transactional provisional'' reading erode the broader norms of multilateral treaty bindingness and good faith in international relations?',
    'Longitudinal study of subsequent treaty negotiations and compliance behaviors, and analysis of diplomatic discourse regarding the sanctity of international agreements.',
    'If the erosion is substantial, the ''multilateral_diplomacy_institutions'' would experience a higher effective extraction, and the overall stability of the international legal order would be diminished, potentially leading to a re-evaluation of the constraint''s systemic impact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_on_multilateral_norms, empirical, 'Erosion of multilateral norms by unilateral interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jcpo_tr_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2, 0.12).
narrative_ontology:measurement(jcpo_tr_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 3, 0.15).
narrative_ontology:measurement(jcpo_tr_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(jcpo_tr_t7, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 7, 0.15).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(jcpo_be_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2, 0.65).
narrative_ontology:measurement(jcpo_be_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 3, 0.75).
narrative_ontology:measurement(jcpo_be_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 5, 0.78).
narrative_ontology:measurement(jcpo_be_t7, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 7, 0.78).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 10, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(jcpo_su_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2, 0.75).
narrative_ontology:measurement(jcpo_su_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 3, 0.85).
narrative_ontology:measurement(jcpo_su_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(jcpo_su_t7, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 7, 0.85).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 10, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, iran_nuclear_program_sanctions_regime).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, non_proliferation_treaty_regime).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jcpoa_treaty_bindingness' kernel. It is structurally distinct from the 'binding_multilateral_reading' and 'graduated_compliance_reading' due to fundamental differences in the interpretation of treaty obligations, exit mechanisms, and the role of unilateral state action. All three readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
