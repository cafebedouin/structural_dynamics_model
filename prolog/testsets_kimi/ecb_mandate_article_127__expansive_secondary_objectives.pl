% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__expansive_secondary_objectives, []).

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
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Article 127 Expansive Secondary Objectives Reading
 *   domain: monetary_policy/eu_institutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the expansive reading of Article 127(1)
 *   TFEU, under which the ECB may assign operational weight to employment and
 *   growth objectives so long as price stability is 'not threatened.' The
 *   'without prejudice' clause is read as a permissive balancing device
 *   rather than a strict hierarchy. This reading is contested within the
 *   kernel ecb_mandate_article_127, rivaling the orthodox_price_stability
 *   reading (exclusive focus on inflation) and influencing the
 *   climate_incorporation reading (expansion into climate risk). The
 *   constraint coordinates macroeconomic stabilization across the Eurozone
 *   but asymmetrically extracts purchasing power from savers and retirees to
 *   benefit debtors and workers.
 *
 * KEY AGENTS:
 *   - ECB Governing Council (agenda_setter, institutional): Interprets and enforces the expansive reading, expanding its own operational discretion.
 *   - Workers (beneficiary, organized): Receive employment-weighted monetary policy.
 *   - Debtor member states (beneficiary, institutional): Receive fiscal space from accommodative policy.
 *   - Indebted households (beneficiary, moderate): Receive lower real debt service.
 *   - Saver households (payer, moderate): Bear negative real returns on safe assets.
 *   - Fixed-income retirees (payer, powerless): Bear inflation-driven purchasing power losses with minimal exit.
 *   - Orthodox monetarists (excluded, organized): Their strict subordination reading is institutionally suppressed.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.52).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.55).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.52).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Article 127 Expansive Secondary Objectives Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary_policy/eu_institutional_law").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, '6e70e91f-1eee-43e0-bfbb-fc4f4416d2d2').
narrative_ontology:cs_kernel_codification('6e70e91f-1eee-43e0-bfbb-fc4f4416d2d2', fixed_text).
narrative_ontology:cs_authority_grounding('6e70e91f-1eee-43e0-bfbb-fc4f4416d2d2', lineage).
narrative_ontology:cs_interpretation_layer_present('6e70e91f-1eee-43e0-bfbb-fc4f4416d2d2').
narrative_ontology:cs_reading_relation('6e70e91f-1eee-43e0-bfbb-fc4f4416d2d2', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('6e70e91f-1eee-43e0-bfbb-fc4f4416d2d2', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('6e70e91f-1eee-43e0-bfbb-fc4f4416d2d2', foundational, employment_growth_operational_weight_permitted).
narrative_ontology:cs_axiom_status(employment_growth_operational_weight_permitted, holdable).
narrative_ontology:cs_axiom_grounding('6e70e91f-1eee-43e0-bfbb-fc4f4416d2d2', employment_growth_operational_weight_permitted, conventional).
narrative_ontology:cs_axiom('6e70e91f-1eee-43e0-bfbb-fc4f4416d2d2', foundational, without_prejudice_discretionary_balancing).
narrative_ontology:cs_axiom_status(without_prejudice_discretionary_balancing, holdable).
narrative_ontology:cs_axiom_grounding('6e70e91f-1eee-43e0-bfbb-fc4f4416d2d2', without_prejudice_discretionary_balancing, conventional).
narrative_ontology:cs_reference_frame('6e70e91f-1eee-43e0-bfbb-fc4f4416d2d2', balanced_macro_stability_mandate).
narrative_ontology:cs_drift_state('6e70e91f-1eee-43e0-bfbb-fc4f4416d2d2', post_pspp_challenge_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('6e70e91f-1eee-43e0-bfbb-fc4f4416d2d2', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, workers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, debtor_member_states).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, indebted_households).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, saver_households).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, fixed_income_retirees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 127 TFEU to permit operational weight on employment and growth whenever price stability is not threatened; must actively defend this reading against legal challenges, national constitutional courts, and internal hawkish dissent.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from monetary policy that treats employment as an operational objective; job creation and wage growth are directly supported when the ECB keeps rates low under this mandate reading.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, workers, beneficiary,
    organized, biographical, constrained, continental).

% Gain fiscal space and debt sustainability from prolonged accommodative monetary policy that is justified under the expansive employment-and-growth reading of the mandate.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, debtor_member_states, beneficiary,
    institutional, generational, constrained, continental).

% Benefit from lower real interest rates and easier refinancing conditions when monetary policy is oriented toward broad macroeconomic outcomes rather than strict inflation targeting.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, indebted_households, beneficiary,
    moderate, biographical, constrained, continental).

% Bear the cost of negative or suppressed real returns on deposits and safe assets when the ECB prioritizes employment and growth over strict price stability; cannot opt out of Eurozone monetary conditions.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, saver_households, payer,
    moderate, biographical, constrained, continental).

% Pension and annuity income loses purchasing power when inflation is tolerated for employment objectives; politically underrepresented in ECB governance and structurally unable to relocate to alternative currency regimes.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, fixed_income_retirees, payer,
    powerless, biographical, trapped, continental).

% Argue that secondary objectives are legally subordinate and non-operational under Article 127; their reading is institutionally suppressed by ECB practice and ECJ jurisprudence, leaving them without effective agenda access.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_monetarists, excluded,
    organized, civilizational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__expansive_secondary_objectives, diffuse).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__expansive_secondary_objectives, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal basis for the ECB to weigh employment and growth as operational objectives alongside price stability, enabling macroeconomic stabilization across a heterogeneous currency union when inflation is not under immediate threat.
% TRANSFER_FUNCTION: Moves the distributional burden of monetary policy from debtors and labor toward savers and fixed-income recipients through tolerated inflation risk and suppressed real rates; simultaneously transfers institutional authority from strict price-stability exclusivity to discretionary balancing by the ECB Governing Council.
% ABSENT_VOICES: Net-saver constituencies and ordoliberal legal scholars who hold that secondary objectives are hierarchically void of operational content; their reading is structurally excluded from ECB decision premises and ECJ standing.
% DISAPPEARANCE_RATIONALE: If this expansive reading vanished, the ECB would revert to strict price-stability orthodoxy; monetary policy would tighten preemptively, employment would lose operational standing, debtor states and households would face higher real rates, and the institutional balance between the ECB and fiscal authorities would shift sharply toward austerity.
% FOUNDING_PROBLEM: The Maastricht Treaty framers needed to reconcile German ordoliberal demands for a price-stability mandate with broader European social-democratic demands for employment-focused macroeconomic policy, producing a treaty text whose secondary objectives were deliberately ambiguous.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts of the Maastricht negotiations and independent constitutional scholarship outside the ECB attest the ambiguity was deliberate; ECB orthodox accounts and some German legal authorities assert the text clearly subordinates secondary objectives. No universally accepted neutral corroboration exists.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate but rising: the distributional transfer from savers to debtors under negative real rates is structural and intentional under this reading, not a side effect. Suppression (0.55) reflects the active institutional marginalization of the orthodox reading via ECJ precedent and ECB internal doctrine. Theater ratio (0.28) captures the performative legal argumentation around treaty text that obscures distributional choices without being entirely hollowâthere is genuine coordination in employment stabilization. Accessibility collapse (0.48) indicates that while the orthodox alternative still exists intellectually, it has been largely closed off institutionally. Resistance (0.58) reflects persistent legal challenges (German Constitutional Court) and hawkish dissent.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (ECB Governing Council) experiences this constraint as necessary legal flexibility to manage a heterogeneous currency union. The beneficiary seats (workers, debtors) experience it as a legitimate distributive correction. The payer seats (savers, retirees) experience the identical constraint as institutionalized expropriation of purchasing power. The excluded seat (orthodox monetarists) experiences it as a jurisprudential usurpation. The engine should compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council sits near the beneficiary end (low d) because the reading expands its operational discretion and institutional legitimacy. Workers and debtor agents are beneficiaries (low d). Saver households and fixed-income retirees are victims/payers (high d); retirees are trapped, pushing them closer to full-target than moderate savers. Orthodox monetarists are excluded and structurally suppressed (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification resists collapsing the constraint into a pure Snare by acknowledging the genuine coordination function: employment-weighted policy does stabilize the currency union and solves a real collective-action problem in macroeconomic management. It resists collapsing into a pure Rope by naming the asymmetric extraction (savers/retirees bear costs) and the active suppression of the orthodox alternative. The Tangled Rope classification captures that the same legal clause coordinates and extracts simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_127_secondary_objective_status,
    'Does Article 127(1) TFEU''s ''without prejudice to the objective of price stability'' render secondary objectives hierarchically subordinate and legally non-operational, or does it merely prioritize price stability while permitting operational balancing?',
    'Comparative analysis of ECJ jurisprudence, travaux prÃ©paratoires, and national constitutional court rulings.',
    'If subordination is the correct legal characterization, this constraint collapses toward a Snare (extraction without genuine coordination); if permissive balancing is correct, it remains a Tangled Rope or strong Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_127_secondary_objective_status, conceptual, 'Core legal ambiguity at the kernel level: hierarchy versus balancing.').

omega_variable(
    distributional_effect_attribution,
    'Can the distributional transfer from savers to debtors under this reading be empirically distinguished from general equilibrium business-cycle effects?',
    'Microdata on household portfolios combined with counterfactual monetary policy rules simulating strict price-stability orthodoxy.',
    'If separable, the extraction is targeted and asymmetric; if inseparable, the constraint is closer to a Rope with incidental distributional side effects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributional_effect_attribution, empirical, 'Whether saver-debtor redistribution is a separable feature of the constraint.').

omega_variable(
    suppression_mechanism_legal_vs_political,
    'Is the suppression of the orthodox reading achieved through legal institutionalization (ECJ precedent, ECB doctrine) or political delegation (member state acquiescence)?',
    'Analysis of national constitutional court rulings and ECB Governing Council voting records.',
    'Legal institutionalization implies higher persistence and lower reversibility; political delegation implies potential for rapid regime change through electoral shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_legal_vs_political, conceptual, 'Source of suppression determines constraint persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_exp_sec_obj_tr_t0, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ecb_exp_sec_obj_tr_t4, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 4, 0.14).
narrative_ontology:measurement(ecb_exp_sec_obj_tr_t8, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 8, 0.17).
narrative_ontology:measurement(ecb_exp_sec_obj_tr_t12, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 12, 0.2).
narrative_ontology:measurement(ecb_exp_sec_obj_tr_t16, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 16, 0.23).
narrative_ontology:measurement(ecb_exp_sec_obj_tr_t20, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 20, 0.26).
narrative_ontology:measurement(ecb_exp_sec_obj_tr_t24, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(ecb_exp_sec_obj_be_t0, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ecb_exp_sec_obj_be_t4, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(ecb_exp_sec_obj_be_t8, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(ecb_exp_sec_obj_be_t12, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(ecb_exp_sec_obj_be_t16, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(ecb_exp_sec_obj_be_t20, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(ecb_exp_sec_obj_be_t24, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ecb_exp_sec_obj_su_t0, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(ecb_exp_sec_obj_su_t4, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(ecb_exp_sec_obj_su_t8, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(ecb_exp_sec_obj_su_t12, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(ecb_exp_sec_obj_su_t16, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(ecb_exp_sec_obj_su_t20, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(ecb_exp_sec_obj_su_t24, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, climate_incorporation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ecb_mandate_article_127 kernel. The expansive_secondary_objectives reading, the orthodox_price_stability reading, and the climate_incorporation reading are mutually coupled through shared treaty text and institutional precedent. They form a constraint family linked by institutional competition over the same legal kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
