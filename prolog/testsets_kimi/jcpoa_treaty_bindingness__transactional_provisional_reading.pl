% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__transactional_provisional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: JCPOA Transactional Provisional Reading (Unilateral Voidability)
 *   domain: international_law/nuclear_nonproliferation
 *
 * SUMMARY:
 *   This constraint instantiates the transactional provisional reading of the
 *   JCPOA treaty-bindingness kernel. Under this reading, the Joint
 *   Comprehensive Plan of Action is not a binding multilateral treaty but a
 *   reversible transaction: Iran accepts nuclear limits while the United
 *   States retains unilateral authority to declare bad faith, void the
 *   arrangement, and reimpose sanctions. The reading emerged in political
 *   practice most clearly during the 2018 US withdrawal, when the executive
 *   bypassed the JCPOA's Joint Commission dispute mechanism to unilaterally
 *   restore sanctions. It benefits the unilaterally acting state and its
 *   domestic hawkish coalitions by preserving sovereign discretion and a
 *   coercive bargaining chip. It extracts from Iran, which accepted
 *   verifiable constraints without a reciprocal guarantee of stable relief,
 *   and from EU commercial actors caught between resumed trade and
 *   extraterritorial US enforcement. The constraint carries a genuine
 *   coordination functionânuclear rollbackâbut its provisional
 *   voidability layers asymmetric extraction onto that function.
 *
 * KEY AGENTS:
 *   - us_executive_branch: Agenda-setter and primary beneficiary (institutional/global) â unilaterally determines bad faith and triggers snapback sanctions
 *   - domestic_hawkish_coalitions: Secondary beneficiary (organized/national) â political coalitions opposing the deal who gain from retained hawkish posture
 *   - iranian_republic: Primary payer (institutional/national) â bears compliance obligations without reciprocal guarantee against unilateral withdrawal
 *   - eu_commercial_stakeholders: Secondary payer (organized/continental) â bear extraterritorial sanctions costs and diplomatic friction
 *   - technical_verification_agencies: Analytical observer (institutional/global) â monitors compliance but is overridden by political unilateral determinations
 *   - multilateral_binding_advocates: Excluded voice (organized/global) â argue for consensus-based dissolution but are sidelined by unilateralist reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.7).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.65).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA Transactional Provisional Reading (Unilateral Voidability)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_nonproliferation").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, '53fe8821-5f8d-41fe-9b23-bed7afee2e88').
narrative_ontology:cs_kernel_codification('53fe8821-5f8d-41fe-9b23-bed7afee2e88', formalized).
narrative_ontology:cs_authority_grounding('53fe8821-5f8d-41fe-9b23-bed7afee2e88', extraction).
narrative_ontology:cs_interpretation_layer_present('53fe8821-5f8d-41fe-9b23-bed7afee2e88').
narrative_ontology:cs_reading_relation('53fe8821-5f8d-41fe-9b23-bed7afee2e88', jcpoa_treaty_bindingness__binding_multilateral_reading, forecloses).
narrative_ontology:cs_reading_relation('53fe8821-5f8d-41fe-9b23-bed7afee2e88', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('53fe8821-5f8d-41fe-9b23-bed7afee2e88', foundational, unilateral_exit_as_inherent_sovereign_prerogative).
narrative_ontology:cs_axiom_status(unilateral_exit_as_inherent_sovereign_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('53fe8821-5f8d-41fe-9b23-bed7afee2e88', unilateral_exit_as_inherent_sovereign_prerogative, conventional).
narrative_ontology:cs_axiom('53fe8821-5f8d-41fe-9b23-bed7afee2e88', foundational, non_binding_political_commitment_form).
narrative_ontology:cs_axiom_status(non_binding_political_commitment_form, holdable).
narrative_ontology:cs_axiom_grounding('53fe8821-5f8d-41fe-9b23-bed7afee2e88', non_binding_political_commitment_form, conventional).
narrative_ontology:cs_reference_frame('53fe8821-5f8d-41fe-9b23-bed7afee2e88', unilateral_snapback_authority).
narrative_ontology:cs_drift_state('53fe8821-5f8d-41fe-9b23-bed7afee2e88', post_us_withdrawal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('53fe8821-5f8d-41fe-9b23-bed7afee2e88', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, us_executive_branch).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_hawkish_coalitions).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_republic).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, eu_commercial_stakeholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and enforces the JCPOA under a reading that reserves the right to unilaterally declare Iranian bad faith and reimpose sanctions. Retains full sovereign discretion to void the arrangement without multilateral consensus, capturing strategic leverage and domestic political credibility from the threat of snapback.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, us_executive_branch, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, us_executive_branch, beneficiary).

% Political factions within the withdrawing state that oppose diplomatic normalization with Iran. They benefit electorally and ideologically from a posture that treats the JCPOA as inherently provisional and suspect, using the threat of unilateral exit to block rapprochement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_hawkish_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Accepted nuclear program limitations and IAEA inspections in exchange for promised sanctions relief. Under this reading, its compliance is treated as an ongoing condition while counterpart obligations remain provisional; any perceived deviation can trigger immediate reimposition of sanctions by a single state.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_republic, payer,
    institutional, generational, constrained, national).

% European banks, insurers, and industrial firms that resumed trade with Iran under the deal. They face extraterritorial US sanctions when the unilateral voiding is exercised, forcing them to choose between Iranian market access and US financial system access.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, eu_commercial_stakeholders, payer,
    organized, biographical, constrained, continental).

% International technical bodies charged with monitoring Iranian nuclear facilities and reporting factual compliance. Their findings are systematically overridden or ignored when a political actor makes a unilateral bad-faith determination, subordinating technical verification to sovereign discretion.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, technical_verification_agencies, observer,
    institutional, generational, analytical, global).

% Legal scholars and diplomats who argue that the JCPOA should be read as requiring consensus modification or dissolution. They are excluded from the operative interpretation because the provisional reading bypasses multilateral forums in favor of unilateral state action.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_binding_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__transactional_provisional_reading, us_executive_branch).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__transactional_provisional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates P5+1 states and Iran around a quid-pro-quo exchange of nuclear program limitations for sanctions relief, with the unique feature that the arrangement is treated as provisional and voidable by any party upon its own determination of bad faith.
% TRANSFER_FUNCTION: Moves obligation stability and sanctions risk from the withdrawing state to Iran and to EU commercial actors; transfers diplomatic authority from multilateral consensus mechanisms to unilateral state determination.
% ABSENT_VOICES: Advocates of binding multilateral treaty form who would require consensus for dissolution; Iranian civilian sectors not represented by the security apparatus; EU commercial entities subjected to extraterritorial sanctions but excluded from bad-faith determination.
% DISAPPEARANCE_RATIONALE: If the provisional voidable reading disappeared and the JCPOA were treated as binding, Iran's compliance calculus would shift toward long-term commitment, EU-Iran trade normalization could proceed without unilateral override, and US domestic politics would lose the snapback mechanism as a diplomatic cudgel.
% FOUNDING_PROBLEM: Iranian nuclear program expansion threatened regional stability and triggered a sanctions regime lacking a diplomatic off-ramp; the P5+1 needed a framework to freeze and roll back enrichment in exchange for verifiable relief.
% FOUNDING_PROBLEM_CORROBORATION: IAEA technical verification reports from 2016-2018 (outside the US beneficiary circle) confirmed Iranian compliance; EU diplomatic corps corroborated the deal's non-proliferation efficacy. No independent non-beneficiary source supports the claim that Iranian expansion in 2018 required unilateral voiding.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.7, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.70) is high because the constraint structurally transfers all long-term obligation risk to Iran and EU actors while the withdrawing state bears none. Suppression (0.65) reflects the active enforcement required to maintain extraterritorial sanctions and deter EU defections from the US position. Theater ratio (0.48) captures the post-2018 shift where a substantial share of diplomatic activity became performative enforcement of maximum pressure rather than genuine non-proliferation coordination. Accessibility collapse (0.58) registers that binding-multilateral alternatives are legally available but politically blocked by the dominant reading. Resistance (0.60) reflects Iranian nuclear expansion in response, EU INSTEX creation, and ongoing diplomatic contestation. The temporal series align on a single grid and show the inflection at US withdrawal (time_point 4).
 *
 * PERSPECTIVAL GAP:
 *   The US executive seat experiences this constraint as a sovereignty-preserving coordination tool that keeps leverage intact; the engine will compute a low directionality and damped extraction there. The Iranian and EU seats experience it as an asymmetric gamble where their compliance investments are hostage to another party's domestic politics; the engine will compute high directionality and amplified extraction. The divergence is structural, not perspectival error.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to us_executive_branch and domestic_hawkish_coalitions: they collect strategic flexibility and political gains, so their derived directionality sits near the beneficiary end. Victim declarations map to iranian_republic and eu_commercial_stakeholders: they bear the costs of sanctions reimposition and investment instability, so their derived directionality sits near the target end. Technical verification agencies are observers with analytical exit; multilateral binding advocates are excluded but structurally powerless to alter the reading. No override is needed because beneficiary and victim roles cleanly separate the seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâa nuclear program lacking a diplomatic off-rampâwas substantially addressed by the 2015 arrangement. The transactional provisional reading persists beyond that solved problem because it serves a different function: preserving unilateral coercive leverage for the withdrawing state and satisfying domestic political opposition. The framework prevents mislabeling this as pure coordination by requiring declared victims and active enforcement; without those, a provisional voidability might look like a rope (flexible diplomacy). The presence of identifiable victims whose compliance is locked in while counterpart obligations evaporate upon unilateral whim is what makes it tangled rope rather than rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unilateral_determination_as_cover,
    'Is unilateral bad-faith determination a structurally necessary enforcement mechanism for nuclear non-proliferation, or a cover for sovereign extraction that bypasses multilateral accountability?',
    'Comparative case analysis of other non-proliferation frameworks: if binding multilateral forms achieve comparable compliance without unilateral exit clauses, the determination is cover.',
    'If cover, the constraint''s extractiveness is intrinsic rather than incidental, supporting reclassification toward snare; if necessary, the extraction is the price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_determination_as_cover, conceptual, 'Whether unilateral voidability serves non-proliferation or sovereign extraction').

omega_variable(
    kernel_bindingness_contest,
    'Does the JCPOA kernel admit a single stable legal form, or is it irreducibly contested between binding multilateral and provisional transactional readings?',
    'International court advisory opinion or unanimous P5+1 joint interpretive statement establishing canonical form.',
    'If irreducibly contested, the kernel is a generator of constraint-family divergence rather than a resolvable ambiguity; if resolvable, sibling readings collapse to one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_bindingness_contest, conceptual, 'Contested kernel ambiguity between binding and provisional readings').

omega_variable(
    eu_resistance_sustainability,
    'Can EU commercial and diplomatic resistance to US unilateral snapback structurally endure, or will extraterritorial financial coercion eventually collapse EU autonomy?',
    'Longitudinal tracking of EU-Iran trade volumes and INSTEX operationalization.',
    'If EU resistance collapses, effective suppression rises and accessibility_collapse increases; if sustained, the constraint''s extraction is partially checked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_resistance_sustainability, empirical, 'Durability of EU resistance to extraterritorial sanctions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_tp_tr_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jcpoa_tp_tr_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(jcpoa_tp_tr_t4, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 4, 0.6).
narrative_ontology:measurement(jcpoa_tp_tr_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 6, 0.55).
narrative_ontology:measurement(jcpoa_tp_tr_t8, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 8, 0.5).
narrative_ontology:measurement(jcpoa_tp_tr_t9, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 9, 0.48).

% Extraction over time
narrative_ontology:measurement(jcpoa_tp_be_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(jcpoa_tp_be_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(jcpoa_tp_be_t4, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 4, 0.78).
narrative_ontology:measurement(jcpoa_tp_be_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 6, 0.75).
narrative_ontology:measurement(jcpoa_tp_be_t8, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement(jcpoa_tp_be_t9, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 9, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(jcpoa_tp_su_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(jcpoa_tp_su_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2, 0.45).
narrative_ontology:measurement(jcpoa_tp_su_t4, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 4, 0.85).
narrative_ontology:measurement(jcpoa_tp_su_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(jcpoa_tp_su_t8, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(jcpoa_tp_su_t9, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 9, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, graduated_compliance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jcpoa_treaty_bindingness kernel, which decomposes into structurally distinct claims: binding multilateral treaty form, graduated reciprocal compliance, and transactional provisional voidability. Each reading carries a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
