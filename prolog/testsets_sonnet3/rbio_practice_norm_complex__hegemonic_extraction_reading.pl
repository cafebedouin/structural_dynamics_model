% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: Rules-Based International Order (RBIO) Norm Complex — Hegemonic Extraction Reading
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates the hegemonic_extraction_reading of the RBIO
 *   practice-norm kernel: the claim that the rules-based international order
 *   is a formally revisable but practically frozen hegemonic settlement,
 *   whose amendment channels (UN Charter revision requiring P5 ratification,
 *   IMF/World Bank quota reform requiring supermajorities that P5-aligned
 *   capital effectively controls) never produce revisions that touch the
 *   founding distribution of power. Selective enforcement — intervention or
 *   sanction applied to non-aligned targets while structurally similar
 *   conduct by P5 members or close allies draws no consequence — is read here
 *   as diagnostic evidence that the norm's operative function is extraction
 *   (policy space, capital access, debt service) rather than universal rule
 *   application. This reading does NOT describe the
 *   liberal_institutional_reading's world (universal, consent-based,
 *   revisable-in-good-faith norms undermined only by a capacity gap) or the
 *   sovereignty_maximalist_reading's world (absolute sovereignty against
 *   which any external conditionality is per se illegitimate). Each of those
 *   is a separate constraint with its own epsilon, authored separately and
 *   linked via network.affects_constraints and
 *   cs_structure.reading_relations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.81).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.76).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "Rules-Based International Order (RBIO) Norm Complex — Hegemonic Extraction Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, '17a3ffe7-6576-4869-8dde-0c2643437509').
narrative_ontology:cs_kernel_codification('17a3ffe7-6576-4869-8dde-0c2643437509', formalized).
narrative_ontology:cs_authority_grounding('17a3ffe7-6576-4869-8dde-0c2643437509', extraction).
narrative_ontology:cs_interpretation_layer_present('17a3ffe7-6576-4869-8dde-0c2643437509').
narrative_ontology:cs_reading_relation('17a3ffe7-6576-4869-8dde-0c2643437509', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('17a3ffe7-6576-4869-8dde-0c2643437509', rbio_practice_norm_complex__sovereignty_maximalist_reading, influences).
narrative_ontology:cs_axiom('17a3ffe7-6576-4869-8dde-0c2643437509', foundational, enforcement_selectivity_reveals_extractive_function).
narrative_ontology:cs_axiom_status(enforcement_selectivity_reveals_extractive_function, holdable).
narrative_ontology:cs_axiom_grounding('17a3ffe7-6576-4869-8dde-0c2643437509', enforcement_selectivity_reveals_extractive_function, empirically_contingent).
narrative_ontology:cs_axiom('17a3ffe7-6576-4869-8dde-0c2643437509', foundational, formal_amendment_process_captured_by_founding_beneficiaries).
narrative_ontology:cs_axiom_status(formal_amendment_process_captured_by_founding_beneficiaries, holdable).
narrative_ontology:cs_axiom_grounding('17a3ffe7-6576-4869-8dde-0c2643437509', formal_amendment_process_captured_by_founding_beneficiaries, empirically_contingent).
narrative_ontology:cs_axiom('17a3ffe7-6576-4869-8dde-0c2643437509', secondary, conditionality_is_coerced_not_consensual_contract).
narrative_ontology:cs_axiom_status(conditionality_is_coerced_not_consensual_contract, holdable).
narrative_ontology:cs_axiom_grounding('17a3ffe7-6576-4869-8dde-0c2643437509', conditionality_is_coerced_not_consensual_contract, deontological).
narrative_ontology:cs_reference_frame('17a3ffe7-6576-4869-8dde-0c2643437509', id_1945_charter_settlement_as_universal_rule_of_law).
narrative_ontology:cs_drift_state('17a3ffe7-6576-4869-8dde-0c2643437509', post_1980s_debt_crisis_conditionality_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('17a3ffe7-6576-4869-8dde-0c2643437509', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_and_european_capital).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_permanent_members).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, bretton_woods_headquarters_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_debtor_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_populations).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, non_p5_un_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, target_states_of_selective_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power over Security Council authorization of intervention and sanctions, and sit atop the postwar institutional architecture (UN Charter, Bretton Woods voting shares). Can block any formal amendment that would dilute their position while continuing to invoke the same norms selectively against non-veto-holding states. Amendment of the Charter requires their ratification, so the formal revisability channel never produces a revision that touches their prerogatives.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_permanent_members, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Benefits from conditionality regimes attached to lending, trade access, and reconstruction aid that open target economies to capital inflows, privatization, and debt servicing on terms set in Washington, Brussels, and London. Bears none of the compliance costs imposed on borrowing states and can exit any single country relationship without systemic consequence.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, us_and_european_capital, beneficiary,
    institutional, generational, arbitrage, global).

% Formally sovereign and equal under the UN Charter, but access to credit, aid, and reconstruction financing is conditioned on structural adjustment terms they did not draft and cannot renegotiate from a position of parity. Refusing conditionality means exclusion from capital markets; accepting it means ceding fiscal and regulatory policy space. No veto, no exit that does not cost more than compliance.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_debtor_states, payer,
    powerless, generational, trapped, national).

% Bear the direct costs of austerity, currency devaluation, and privatization of public services imposed as conditions of the loans their governments accept. Have no seat in the negotiations that set these terms and no meaningful capacity for individual or collective exit.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_populations, payer,
    powerless, biographical, trapped, national).

% Vote in the General Assembly and participate in multilateral fora, but have no veto over Security Council action and cannot block Charter amendments that entrench P5 privilege. Repeatedly propose Charter reform (expanded Council seats, veto restriction) that never advances past committee because the amendment mechanism itself requires P5 ratification.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, non_p5_un_member_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, non_p5_un_member_states, excluded).

% Subjected to sanctions, intervention, or tribunal referral for conduct that P5 members or their close allies commit without consequence. The gap between invoked norm and applied norm is treated by this reading as the diagnostic evidence that the norm's function is selective extraction, not universal rule enforcement.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, target_states_of_selective_enforcement, payer,
    powerless, biographical, trapped, national).

% Document the pattern of selective enforcement and coerced conditionality from outside the institutions that administer RBIO, publishing critiques that are read within academic and some diplomatic circles but carry no formal standing in Charter amendment or IMF/World Bank governance processes.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, third_world_approaches_to_international_law_scholars, excluded,
    moderate, generational, analytical, global).

% The formal legal and institutional architecture itself — the Charter text, IMF Articles of Agreement, World Bank voting formula — which this reading treats as the codified surface concealing the underlying hegemonic bargain. Listed for completeness as the non-agent instrument through which the extraction operates, not as a party that acts.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, un_charter_and_bretton_woods_institutions, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(rbio_practice_norm_complex__hegemonic_extraction_reading, un_charter_and_bretton_woods_institutions).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__hegemonic_extraction_reading, us_and_european_capital).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__hegemonic_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The RBIO complex does solve real coordination problems — preventing great-power war through a collective security mechanism, providing liquidity to states facing balance-of-payments crises, and establishing common rules for trade and finance that reduce transaction costs for all participants.
% TRANSFER_FUNCTION: Moves policy sovereignty, fiscal discretion, and long-run capital income from Global South debtor states and the populations who bear structural adjustment, to P5 governments' strategic latitude and to US/European capital's access to opened markets, privatized assets, and debt service streams.
% ABSENT_VOICES: Global South populations subjected to austerity conditions were never party to the loan negotiations that set those conditions. Non-P5 states have repeatedly proposed Security Council and Bretton Woods governance reform in the General Assembly and at UNCTAD, but the amendment pathways that would seat them require the consent of the very actors whose position would be diluted.
% DISAPPEARANCE_RATIONALE: The beneficiary seats hold that without the RBIO complex, global coordination on security and finance collapses into unmediated great-power competition and creditor chaos — the world rearranges for the worse. The payer seats hold that without the selective-enforcement and conditionality machinery, Global South states would regain policy space currently constrained by loan conditionality and asymmetric Charter voting, and would negotiate financing and security arrangements on more genuinely multilateral terms — the world rearranges for the better. Both agree the world rearranges; they dispute the direction and beneficiary of the rearrangement, which is why this is authored as contested rather than either uniform verdict.
% FOUNDING_PROBLEM: The post-1945 order was built to prevent a repeat of interwar great-power war and interwar financial collapse, by locking in the wartime victors' coordination and providing a lender-of-last-resort mechanism for reconstruction and later development finance.
% FOUNDING_PROBLEM_CORROBORATION: P5 governments and IMF/World Bank leadership attest the founding problem (great-power war, financial collapse) remains live and the institutions still address it. Independent corroboration from outside the beneficiary set — TWAIL scholarship, UNCTAD trade and development reports, and General Assembly reform resolutions repeatedly co-sponsored by over 100 non-P5 member states — holds that the security-and-liquidity founding problem was substantially addressed decades ago and that the persisting machinery now primarily administers conditionality and selective enforcement rather than the original coordination function.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) and rising over the interval because conditionality lending, structural adjustment programs, and selective sanctions regimes intensified from the 1980s debt crisis onward, layering an extraction function onto the original 1945 security-and-liquidity coordination function. Suppression is authored high (0.76) because exit from the conditionality regime for a debtor state means exclusion from capital markets — a structural barrier, not mere preference. Theater ratio rises to 0.62 because an increasing share of RBIO's institutional activity (universality rhetoric, consultative processes with no binding effect on outcomes, Charter-reform committees that never report out amendments) is, on this reading, performative maintenance of a legitimacy narrative rather than functional coordination. Accessibility collapse (0.58) is moderate rather than extreme because formal alternatives — regional financial arrangements, South-South cooperation frameworks, BRICS-adjacent institutions — exist and are growing, even though they remain marginal relative to Bretton Woods scale.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 permanent members and US/European capital sit at the beneficiary end: they set or profit from the rules, can block amendments that would dilute their position, and retain exit/arbitrage options no other seat has. Global South debtor states, structural-adjustment populations, and states subject to selective enforcement sit at the target end: trapped exit options, generational-to-biographical time horizons dominated by compliance costs they did not negotiate. Non-P5 UN member states occupy an intermediate position — they have voice (General Assembly vote) but no veto, which is why they are marked payer/excluded rather than pure beneficiary despite formal sovereign equality.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is deliberately chosen over snare because this reading concedes a genuine coordination function existed and partially persists (great-power war prevention, liquidity provision) — it is not claiming pure extraction dressed as coordination from the start. What has happened, on this reading, is mandatrophy: the founding problem (1945 great-power war and financial collapse) has been substantially addressed, but the institutional machinery built to address it has not sunset — instead its enforcement selectivity and conditionality functions have grown to fill the space, extracting rents from actors who have no path to amend the arrangement that governs them. This is why founding_problem_status is authored 'contested' rather than 'dead': the beneficiary seats can still point to ongoing great-power tension as evidence the problem is live, while independent corroboration (TWAIL scholarship, UNCTAD, repeated GA reform resolutions) holds the founding function is substantially discharged and what remains is extraction wearing the coordination function's clothes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_freeze_vs_genuine_consensus_absence,
    'Is the practical un-amendability of the UN Charter and Bretton Woods voting formulas evidence of a frozen hegemonic bargain being actively defended, or evidence that no alternative arrangement commands sufficient genuine multilateral consensus to replace it?',
    'Track record of specific, concretely drafted reform proposals (Security Council expansion formulas, IMF quota reallocation formulas) that reached voting stage: if proposals with broad non-P5 support are blocked specifically by P5 veto or blocking-minority votes from capital-exporting states, that supports the frozen-hegemony reading; if proposals fail from broad disagreement among non-P5 states themselves, that weakens it.',
    'If amendment failure is primarily P5/capital-bloc blocking, this reading''s tangled_rope classification (genuine coordination captured by asymmetric enforcement) is strongly supported. If amendment failure is primarily non-P5 states'' own disagreement, some of the authored suppression score should be attributed to a genuine multilateral coordination problem rather than hegemonic defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_freeze_vs_genuine_consensus_absence, empirical, 'Whether Charter/quota amendment freeze reflects hegemonic defense or absence of alternative consensus.').

omega_variable(
    selective_enforcement_evidentiary_weight,
    'Does the empirical pattern of selective enforcement (sanctions/intervention/tribunal referral applied asymmetrically by power position) establish extractive intent, or is it explicable by capacity and strategic-interest variation without implying a coordinated extraction project?',
    'Comparative case analysis across a large sample of comparable violations, controlling for target state''s relationship to P5 members, to isolate whether power-position is doing independent explanatory work after controlling for severity, verifiability, and enforcement feasibility.',
    'If power-position remains a strong independent predictor after controls, the hegemonic_extraction_reading''s core diagnostic claim is strengthened. If it does not, the liberal_institutional_reading''s capacity-gap explanation gains ground, and this story''s high extractiveness score should be revisited in a future draw.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_evidentiary_weight, empirical, 'Whether enforcement selectivity is best explained by extractive intent or by capacity/interest variation.').

omega_variable(
    kernel_framing_alternative_authority_layer,
    'Is the correct kernel object the formal legal text (Charter, Articles of Agreement) as this reading treats it, or is the more structurally decisive kernel the informal legitimacy narrative (''rules-based order,'' ''international community'') that the formal text is invoked to authorize? Two framings could produce different cs_structure classifications: the formal-text framing supports fixed_text/lineage; a legitimacy-narrative framing might support implicit/distributed authority with a different drift profile.',
    'Track whether contested enforcement actions are justified primarily by citation to specific Charter/treaty provisions (supporting fixed_text framing) or by appeal to the diffuse ''rules-based order'' narrative independent of specific textual warrant (supporting implicit/narrative framing).',
    'If the diffuse legitimacy-narrative framing is the operative one, authority_grounding might better read as a blend of extraction and diffuse_epistemic rather than pure lineage/extraction over a fixed text, and the drift_state''s acknowledged flag might shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_authority_layer, conceptual, 'Whether the kernel is best modeled as the formal legal text or the informal legitimacy narrative layered above it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(rbio_tr_t1971, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1971, 0.28).
narrative_ontology:measurement(rbio_tr_t1985, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1985, 0.4).
narrative_ontology:measurement(rbio_tr_t2001, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2001, 0.5).
narrative_ontology:measurement(rbio_tr_t2015, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2015, 0.58).
narrative_ontology:measurement(rbio_tr_t2025, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2025, 0.62).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1945, 0.42).
narrative_ontology:measurement(rbio_be_t1971, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1971, 0.52).
narrative_ontology:measurement(rbio_be_t1985, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1985, 0.66).
narrative_ontology:measurement(rbio_be_t2001, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2001, 0.74).
narrative_ontology:measurement(rbio_be_t2015, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2015, 0.79).
narrative_ontology:measurement(rbio_be_t2025, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2025, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(rbio_su_t1971, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1971, 0.55).
narrative_ontology:measurement(rbio_su_t1985, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1985, 0.63).
narrative_ontology:measurement(rbio_su_t2001, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2001, 0.71).
narrative_ontology:measurement(rbio_su_t2015, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(rbio_su_t2025, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2025, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.1).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, sovereignty_maximalist_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, imf_conditionality_lending).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, un_security_council_veto_power).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the rbio_practice_norm_complex kernel (hegemonic_extraction_reading, liberal_institutional_reading, sovereignty_maximalist_reading). Each reading authors its own epsilon over the same standing arrangement — the RBIO norm complex as currently practiced — assessed by that reading's own lights, per the epsilon-referent rule for kernel-reading stories. This reading's epsilon (0.81) reflects a tangled_rope diagnosis; the liberal_institutional_reading would author a near-mountain-to-rope epsilon over the same standing arrangement; the sovereignty_maximalist_reading would author high epsilon for a different reason (any non-consented external constraint is illegitimate per se, independent of distributive outcome). All three link to each other via reading_relations in cs_structure and should be treated as a constraint family, not decomposed further or merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
