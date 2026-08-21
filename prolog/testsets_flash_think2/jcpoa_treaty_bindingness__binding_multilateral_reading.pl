% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__binding_multilateral_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA as Binding Multilateral Treaty (Binding Multilateral Reading)
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   This constraint story analyzes the Joint Comprehensive Plan of Action
 *   (JCPOA) through the 'binding multilateral treaty' reading. In this
 *   interpretation, the JCPOA is a legally binding international agreement
 *   requiring consensus among its signatories (P5+1+EU) for modification or
 *   dissolution. Unilateral withdrawal or non-compliance is viewed as a
 *   violation of international law, triggering multilateral dispute
 *   resolution mechanisms before any 'snapback' of sanctions. The
 *   beneficiaries are the non-proliferation regime and multilateral
 *   institutions, while Iran and states seeking unilateral action bear the
 *   costs of constrained sovereignty.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.65).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.75).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA as Binding Multilateral Treaty (Binding Multilateral Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, '74f11f6a-9449-4a5c-9588-9288a43ea9b3').
narrative_ontology:cs_kernel_codification('74f11f6a-9449-4a5c-9588-9288a43ea9b3', formalized).
narrative_ontology:cs_authority_grounding('74f11f6a-9449-4a5c-9588-9288a43ea9b3', lineage).
narrative_ontology:cs_interpretation_layer_present('74f11f6a-9449-4a5c-9588-9288a43ea9b3').
narrative_ontology:cs_reading_relation('74f11f6a-9449-4a5c-9588-9288a43ea9b3', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('74f11f6a-9449-4a5c-9588-9288a43ea9b3', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('74f11f6a-9449-4a5c-9588-9288a43ea9b3', foundational, pacta_sunt_servanda).
narrative_ontology:cs_axiom_status(pacta_sunt_servanda, holdable).
narrative_ontology:cs_axiom_grounding('74f11f6a-9449-4a5c-9588-9288a43ea9b3', pacta_sunt_servanda, deontological).
narrative_ontology:cs_axiom('74f11f6a-9449-4a5c-9588-9288a43ea9b3', foundational, unsc_primacy_in_sanctions).
narrative_ontology:cs_axiom_status(unsc_primacy_in_sanctions, holdable).
narrative_ontology:cs_axiom_grounding('74f11f6a-9449-4a5c-9588-9288a43ea9b3', unsc_primacy_in_sanctions, conventional).
narrative_ontology:cs_reference_frame('74f11f6a-9449-4a5c-9588-9288a43ea9b3', international_law_binding_framework).
narrative_ontology:cs_drift_state('74f11f6a-9449-4a5c-9588-9288a43ea9b3', post_us_withdrawal_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('74f11f6a-9449-4a5c-9588-9288a43ea9b3', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_eu_states).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, russia_china).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iran).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, hardline_iranian_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_advocates).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime_stability).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_diplomacy_efficacy).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, international_law_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Agreed to significant limitations on its nuclear program and intrusive inspections in exchange for sanctions relief. Bears the cost of limited sovereignty and economic opportunity, but benefits from international legitimacy and access to global markets. Exit means renewed sanctions and potential military action.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iran, payer,
    powerful, generational, constrained, national).

% A key negotiator and signatory, but later unilaterally withdrew under a different administration, reimposing sanctions. This reading asserts its actions were a violation of binding international law, incurring diplomatic costs. Its power allows it to disrupt but not fully dissolve the treaty for other parties.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, united_states, agenda_setter,
    institutional, biographical, arbitrage, global).

% Primary advocates for the treaty's preservation as a cornerstone of non-proliferation and multilateral diplomacy. They benefit from regional stability and reduced proliferation risk, and actively work to maintain the treaty despite US withdrawal. Their exit options are limited by their commitment to international law.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_eu_states, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, e3_eu_states, agenda_setter).

% Support the JCPOA as a binding multilateral agreement, viewing it as crucial for global non-proliferation and a model for international cooperation. They benefit from the stability it provides and resist unilateral actions that undermine it. Their exit options are constrained by their geopolitical interests in multilateralism.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, russia_china, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, russia_china, agenda_setter).

% Responsible for verifying Iran's compliance with its nuclear commitments. Its technical reports provide the factual basis for assessing the treaty's effectiveness and any violations. Benefits from the clear mandate and international support for its verification role.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea, agenda_setter,
    institutional, generational, analytical, global).

% The ultimate arbiter of international peace and security, whose resolutions underpin the JCPOA's legal framework and sanctions regime. Its consensus is required for major modifications or reimposition of UN sanctions, reinforcing the treaty's binding nature.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council, agenda_setter,
    institutional, civilizational, analytical, universal).

% Civil society organizations and experts who champion nuclear non-proliferation. They view the JCPOA as a vital instrument for preventing the spread of nuclear weapons and upholding international norms. They benefit from the treaty's existence and advocate for its full implementation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_advocates, beneficiary,
    organized, generational, analytical, global).

% Oppose the JCPOA's limitations on Iran's nuclear program and its perceived infringement on national sovereignty. They bear the costs of the treaty's restrictions and are excluded from its benefits, advocating for its dissolution and a more assertive nuclear posture. Their options are limited by the state's commitment to the treaty.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, hardline_iranian_factions, payer,
    organized, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, hardline_iranian_factions, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__binding_multilateral_reading, diffuse).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__binding_multilateral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent Iran from developing nuclear weapons by imposing verifiable limits on its nuclear program, thereby coordinating international efforts to maintain nuclear non-proliferation and regional stability.
% TRANSFER_FUNCTION: Transfers limitations on Iran's nuclear program and intrusive inspections from Iran to the international community, in exchange for sanctions relief and international legitimacy from the P5+1+EU. It also transfers the burden of enforcement and dispute resolution to multilateral bodies.
% ABSENT_VOICES: Hardline factions in Iran and the US, as well as some regional states (e.g., Israel, Saudi Arabia), who view the treaty as either too restrictive or not restrictive enough, and who would advocate for its dissolution or more aggressive enforcement/renegotiation. They are excluded from the consensus-based modification process.
% DISAPPEARANCE_RATIONALE: If the JCPOA's binding nature vanished overnight, Iran would likely rapidly expand its enrichment program, leading to a severe international crisis, potential military confrontation, and a collapse of the non-proliferation regime. The global security landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: Iran's accelerating nuclear enrichment program and the international community's concern that it was moving towards nuclear weapons capability, coupled with Iran's desire for relief from international sanctions.
% FOUNDING_PROBLEM_CORROBORATION: IAEA reports consistently confirm Iran's nuclear activities and the need for verification. Statements from the E3/EU, Russia, and China, as well as numerous non-proliferation experts, corroborate that the underlying proliferation risk and the need for a diplomatic solution remain live, despite challenges to the treaty's implementation.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the significant limitations placed on Iran's nuclear program and the constraints on sovereign action for all parties, particularly regarding unilateral withdrawal or sanctions. Suppression (0.75) is high due to the reliance on international law, UN Security Council resolutions, and the threat of sanctions to enforce compliance and prevent unilateral exits. The theater ratio (0.15) is low, indicating that the treaty's functions are genuinely operational and have real-world consequences, despite periods of non-compliance and political posturing. Resistance (0.70) is high, reflecting the ongoing challenges to the treaty's implementation and the active opposition from certain parties.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Iran and hardline factions, the treaty's bindingness is an imposition on national sovereignty. From the perspective of the E3/EU, Russia, China, and non-proliferation advocates, it is a necessary and legitimate framework for global security. The engine's per-seat classification will reflect these divergent experiences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Iran is a primary target (payer) due to the severe limitations on its nuclear program and economic activity. The E3/EU, Russia, China, IAEA, and UN Security Council are beneficiaries and agenda-setters, as they uphold the multilateral framework and benefit from non-proliferation. The United States, particularly under administrations that withdrew, acts as a target of the treaty's bindingness, as its unilateral actions are seen as violations. Hardline Iranian factions are also payers, as they are constrained by the treaty's existence.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling the JCPOA as a pure Rope (ignoring the significant extraction from Iran's sovereignty and the costs of compliance) or a pure Snare (ignoring the genuine coordination function of non-proliferation and the benefits to global security). The 'binding multilateral' reading emphasizes both the coordination of non-proliferation and the asymmetric extraction of sovereign flexibility, which requires active enforcement to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unilateral_withdrawal_legitimacy,
    'Under what conditions, if any, is unilateral withdrawal from a binding multilateral treaty like the JCPOA legitimate under international law?',
    'Adjudication by the International Court of Justice or a definitive consensus among international legal scholars regarding the applicability of ''fundamental change of circumstances'' (rebus sic stantibus) or ''material breach'' doctrines.',
    'If unilateral withdrawal is deemed legitimate, the treaty''s bindingness is weakened, potentially reclassifying it closer to a transactional framework. If deemed illegitimate, the withdrawing party incurs greater diplomatic and legal costs, reinforcing the binding multilateral reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_withdrawal_legitimacy, conceptual, 'Ambiguity regarding the legal legitimacy of unilateral withdrawal from the JCPOA.').

omega_variable(
    snapback_mechanism_trigger_ambiguity,
    'What constitutes a ''significant non-performance'' by Iran sufficient to trigger the JCPOA''s dispute resolution mechanism and potential snapback sanctions, and who holds the ultimate authority to make this determination?',
    'A clear, agreed-upon threshold for violations established by the Joint Commission or a definitive ruling by the UN Security Council on specific Iranian actions.',
    'If the trigger is easily invoked unilaterally, the treaty leans towards a transactional reading. If it requires broad consensus and clear thresholds, it reinforces the binding multilateral reading''s emphasis on multilateral dispute resolution before punitive action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(snapback_mechanism_trigger_ambiguity, empirical, 'Ambiguity in the criteria and authority for triggering the JCPOA''s snapback mechanism.').

omega_variable(
    reading_identity_ambiguity_transactional,
    'Is the JCPOA fundamentally a binding multilateral treaty, or is it a provisional transactional framework voidable upon unilateral determination of bad faith (as per the ''transactional_provisional_reading'')?',
    'Sustained adherence by all remaining parties despite unilateral withdrawal attempts, and consistent international legal interpretations affirming its binding status.',
    'If the transactional reading gains dominance, the ''binding_multilateral_reading'' would be foreclosed, and the constraint would reclassify to a more flexible, less extractive type with lower suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_ambiguity_transactional, conceptual, 'Contest between binding multilateral and transactional provisional interpretations of the JCPOA.').

omega_variable(
    reading_identity_ambiguity_graduated,
    'Is the JCPOA''s enforcement primarily based on strict binding obligations, or is it a scaled reciprocal commitment with graduated enforcement tied to proportional compliance assessment (as per the ''graduated_compliance_reading'')?',
    'The consistent application of either strict, immediate responses to violations or a more nuanced, proportional approach by the Joint Commission and other parties.',
    'If the graduated compliance reading becomes the dominant interpretation, the ''binding_multilateral_reading'' would coexist but be influenced, leading to potentially lower suppression and extractiveness in practice, as enforcement becomes more flexible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_ambiguity_graduated, conceptual, 'Contest between binding multilateral and graduated compliance interpretations of the JCPOA.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(jcpo_tr_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2017, 0.12).
narrative_ontology:measurement(jcpo_tr_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2019, 0.2).
narrative_ontology:measurement(jcpo_tr_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2021, 0.18).
narrative_ontology:measurement(jcpo_tr_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2023, 0.15).
narrative_ontology:measurement(jcpo_tr_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(jcpo_be_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2017, 0.6).
narrative_ontology:measurement(jcpo_be_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2019, 0.7).
narrative_ontology:measurement(jcpo_be_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2021, 0.68).
narrative_ontology:measurement(jcpo_be_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2023, 0.65).
narrative_ontology:measurement(jcpo_be_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(jcpo_su_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2017, 0.72).
narrative_ontology:measurement(jcpo_su_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2019, 0.8).
narrative_ontology:measurement(jcpo_su_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2021, 0.78).
narrative_ontology:measurement(jcpo_su_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2023, 0.75).
narrative_ontology:measurement(jcpo_su_t2025, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_nuclear_program_limits).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, un_sanctions_regime).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_treaty_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'jcpoa_treaty_bindingness' kernel, alongside 'transactional_provisional_reading' and 'graduated_compliance_reading'. Each reading represents a distinct structural interpretation of the treaty's nature and persistence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
