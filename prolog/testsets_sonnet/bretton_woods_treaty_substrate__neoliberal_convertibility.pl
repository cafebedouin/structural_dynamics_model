% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__neoliberal_convertibility, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Bretton Woods as Convertibility Discipline on State Intervention
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the neoliberal_convertibility reading of the
 *   Bretton Woods kernel: the treaty substrate is read as a constraint on
 *   government intervention (particularly capital controls) designed to
 *   enable free capital markets. Under this reading, national policy autonomy
 *   is a victim of the arrangement, international finance and creditor
 *   exporters are its beneficiaries, and capital controls are treated as
 *   violations of the underlying commitment rather than legitimate tools the
 *   treaty protects. This is a distinct constraint from the sibling readings
 *   (keynesian_embedded_liberalism reads the same text as protecting domestic
 *   policy space FROM capital; sovereignty_defense reads it as preserving
 *   national monetary sovereignty from external discipline) — each reading
 *   has its own beneficiary/victim structure and its own epsilon; they are
 *   not the same constraint measured differently.
 *
 * KEY AGENTS:
 *   - international_finance_capital: primary beneficiary (institutional/arbitrage) — gains mobility and return without policy-autonomy risk premium
 *   - reserve_currency_issuer_treasury: agenda-setter and co-beneficiary (institutional/arbitrage) — sets convertibility terms, collects seigniorage
 *   - developing_state_planners: primary target (moderate/trapped) — loses legitimate access to capital controls as development tool
 *   - domestic_labor_constituencies: diffuse victim (powerless/trapped) — absorbs adjustment cost with no institutional voice
 *   - economic_historians: analytical observer — traces the interpretive drift from capital-control legitimacy to convertibility norm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.62).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.58).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.62).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods as Convertibility Discipline on State Intervention").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, 'c494ef6c-317d-4a76-962c-2abe68f29996').
narrative_ontology:cs_kernel_codification('c494ef6c-317d-4a76-962c-2abe68f29996', fixed_text).
narrative_ontology:cs_authority_grounding('c494ef6c-317d-4a76-962c-2abe68f29996', extraction).
narrative_ontology:cs_interpretation_layer_present('c494ef6c-317d-4a76-962c-2abe68f29996').
narrative_ontology:cs_reading_relation('c494ef6c-317d-4a76-962c-2abe68f29996', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, forecloses).
narrative_ontology:cs_reading_relation('c494ef6c-317d-4a76-962c-2abe68f29996', bretton_woods_treaty_substrate__sovereignty_defense, influences).
narrative_ontology:cs_axiom('c494ef6c-317d-4a76-962c-2abe68f29996', foundational, capital_mobility_is_the_treaty_terminus).
narrative_ontology:cs_axiom_status(capital_mobility_is_the_treaty_terminus, holdable).
narrative_ontology:cs_axiom_grounding('c494ef6c-317d-4a76-962c-2abe68f29996', capital_mobility_is_the_treaty_terminus, instrumental).
narrative_ontology:cs_axiom('c494ef6c-317d-4a76-962c-2abe68f29996', foundational, state_intervention_requires_justification_not_capital).
narrative_ontology:cs_axiom_status(state_intervention_requires_justification_not_capital, holdable).
narrative_ontology:cs_axiom_grounding('c494ef6c-317d-4a76-962c-2abe68f29996', state_intervention_requires_justification_not_capital, empirically_contingent).
narrative_ontology:cs_reference_frame('c494ef6c-317d-4a76-962c-2abe68f29996', capital_controls_permanently_legitimate_1944_articles).
narrative_ontology:cs_drift_state('c494ef6c-317d-4a76-962c-2abe68f29996', post_nixon_shock_conditionality_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('c494ef6c-317d-4a76-962c-2abe68f29996', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance_capital).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, creditor_nation_exporters).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, reserve_currency_issuer_treasury).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_state_planners).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_labor_constituencies).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_control_dependent_governments).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_mobility_efficiency_doctrine).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__neoliberal_convertibility, convertibility_as_market_discipline).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Private banks, bondholders, and later portfolio investors gain from currency convertibility and the gradual dismantling of capital controls: money can move to wherever returns are highest, and states that try to intervene against capital flight face balance-of-payments discipline enforced by the fixed-but-adjustable peg system. This constituency did not write the Bretton Woods text but became its primary beneficiary as the IMF's Article VIII convertibility norm hardened from aspiration into requirement.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance_capital, beneficiary,
    institutional, generational, arbitrage, global).

% The United States, as issuer of the dollar-gold anchor currency, sets the terms under which other states can intervene in their own currency markets and administers (through IMF voting weight) which states get balance-of-payments support and on what conditions. It runs persistent deficits without the adjustment discipline imposed on others (Triffin's observation), collecting seigniorage benefits while enforcing convertibility norms on peripheral states.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, reserve_currency_issuer_treasury, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__neoliberal_convertibility, reserve_currency_issuer_treasury, beneficiary).

% Export-surplus economies (postwar Germany, later Japan) benefit from a stable exchange-rate architecture that lets them sell into open markets without their trading partners resorting to competitive devaluation or emergency capital controls, since the treaty framework treats such intervention as a violation requiring IMF sanction rather than a legitimate policy tool.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, creditor_nation_exporters, beneficiary,
    powerful, generational, mobile, continental).

% Governments pursuing import-substitution industrialization or land reform need capital controls to prevent capital flight and protect infant industries, but the treaty's convertibility norm and IMF conditionality treat these controls as deviations to be corrected rather than legitimate development tools. Access to IMF balance-of-payments support is conditioned on liberalization, so the exit from convertibility discipline is formally available but practically foreclosed by dependence on international credit.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_state_planners, payer,
    moderate, biographical, trapped, national).

% Workers in states forced into austerity by balance-of-payments pressure absorb the adjustment cost — wage suppression, unemployment, currency devaluation's real-income effects — because the treaty framework locates the burden of correcting external imbalance on domestic policy rather than on capital flows themselves. They have no seat in IMF governance and no exit from the national economy whose policy space has narrowed.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_labor_constituencies, payer,
    powerless, biographical, trapped, national).

% States whose macroeconomic stability was designed (in the original 1944 architecture) to rest on the presumption that capital controls were legitimate and expected are structurally undermined as the convertibility norm hardens; they must either violate the emerging market-discipline reading of the treaty (risking IMF and market sanction) or surrender the policy autonomy the same treaty text was originally understood to protect.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_control_dependent_governments, payer,
    moderate, generational, constrained, national).

% The framers who wrote capital controls into the original Articles of Agreement as permanent, legitimate tools (not emergency exceptions) have no voice in the institution's later interpretive drift; their embedded-liberalism reading is treated by this reading's proponents as a transitional stage superseded by convertibility, not as the treaty's actual founding commitment.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, keynesian_treaty_architects_legacy, excluded,
    analytical, civilizational, analytical, global).

% Trace how Article VIII convertibility obligations, IMF conditionality practice, and the eventual Jamaica Accords transformed a treaty designed around capital-control legitimacy into one whose operative norm treats capital mobility as the default and state intervention as the deviation requiring justification.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine coordination problem: without some shared convertibility norm, competitive currency manipulation and capital flight crises recur, and international trade and investment require confidence that currency can be exchanged and repatriated.
% TRANSFER_FUNCTION: Moves policy autonomy and adjustment burden from capital to states, and within states from capital owners to labor and domestic constituencies who cannot exit; moves stability and return-on-capital benefits to internationally mobile finance and to the reserve-currency issuer.
% ABSENT_VOICES: The IMF's original framers who intended capital controls as a permanent feature (not an emergency exception) are read out of the institution's later self-understanding. Developing states and organized labor movements affected by conditionality have no formal seat in the interpretive process that hardened convertibility into the operative norm.
% DISAPPEARANCE_RATIONALE: If the convertibility-discipline reading of Bretton Woods institutions vanished overnight, states would regain formally uncontested legitimacy to deploy capital controls without IMF sanction risk; capital would face repricing for policy-autonomy risk it currently does not bear; the seigniorage advantage of the reserve currency issuer would face renewed challenge; adjustment burdens could shift back toward capital rather than resting on domestic labor and fiscal policy.
% FOUNDING_PROBLEM: The interwar collapse of fixed exchange rates, competitive devaluation, and capital flight crises (1930s) that deepened the Depression and were widely diagnosed as caused by unregulated hot-money flows and beggar-thy-neighbor currency policy.
% FOUNDING_PROBLEM_CORROBORATION: International finance and the U.S. Treasury attest the founding problem (currency chaos) is solved and convertibility is now simply sound economic management. Economic historians outside the beneficiary set (e.g. Eric Helleiner's institutional history, Dani Rodrik's trilemma analysis) attest that the actual founding text treated capital controls as legitimate and permanent, and that the convertibility norm is a later interpretive substitution that reverses rather than fulfills the founders' diagnosis of the 1930s crisis.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins low (0.25 in 1944, when capital controls were still explicitly sanctioned under Article VI) and rises substantially through the Nixon shock (1971) and the IMF's post-1970s conditionality practice, reaching 0.62 by 2000 as convertibility hardened from an aspiration into an enforced norm via IMF Article VIII graduation and structural adjustment lending. Suppression tracks the same arc: the treaty's enforcement machinery (IMF conditionality, market discipline via capital flight risk) grew more binding as the convertibility reading displaced the original controls-permitted reading. Theater ratio is moderate (0.4) reflecting that a real coordination function (avoiding competitive devaluation, enabling trade settlement) persists alongside the extractive reading — this is not pure performance.
 *
 * DIRECTIONALITY LOGIC:
 *   International finance and the reserve-currency issuer sit near the full-beneficiary end: they set terms, capture returns, and hold exit (arbitrage) that state actors under conditionality lack. Developing-state planners and capital-control-dependent governments sit near the full-target end: formally sovereign but practically trapped by dependence on IMF support and market access, which are conditioned on accepting the convertibility discipline. Domestic labor bears the adjustment cost with the least voice and the least exit of any seat — powerless and trapped, absorbing wage and employment effects of externally-imposed macroeconomic correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1930s currency chaos and capital flight) is genuinely contested as live or dead: the neoliberal reading treats it as solved by convertibility itself becoming the discipline mechanism, while critics (outside the beneficiary set) argue the actual founding commitment — permanent legitimacy of capital controls — has been substituted by its opposite. This is exactly the seat-divergence classification is built to surface: the same treaty text, read through the convertibility lens, computes as tangled_rope (real coordination function plus asymmetric extraction with active enforcement) rather than either a pure rope (if the coordination story were the whole truth) or a pure snare (if there were no genuine trade-settlement benefit at all).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convertibility_as_founding_or_drift,
    'Was convertibility-as-discipline the treaty framers'' actual founding intent, or is it a later interpretive substitution imposed by the dominant capital-exporting states after the fact?',
    'Archival analysis of the 1944 negotiating record (White and Keynes drafts, Article VI text explicitly sanctioning capital controls) compared against the IMF''s actual post-1970s Article VIII enforcement practice and conditionality lending patterns.',
    'If convertibility was always the intended terminus and controls were merely transitional, this reading is closer to the treaty''s true structure and the tangled_rope classification understates its legitimacy. If controls were meant as permanent, this reading is a captured reinterpretation and the extraction is better understood as drift-driven rather than founding-driven.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convertibility_as_founding_or_drift, conceptual, 'Whether the convertibility norm is founding intent or interpretive drift.').

omega_variable(
    reading_selection_criterion,
    'What structural signal justifies treating neoliberal_convertibility, rather than keynesian_embedded_liberalism or sovereignty_defense, as the operative reading for any given historical period?',
    'Track which reading''s predictions (capital control legitimacy vs. illegitimacy; whose policy space narrows) actually matched IMF enforcement behavior and state practice at each decade — the reading with better predictive fit for that period is the operative one for that period.',
    'If the operative reading shifted over time (embedded liberalism 1944-1971, convertibility discipline 1971-present), then this story''s single-epsilon assumption is only valid for the post-1971 period and an additional decomposition by period may be warranted rather than treating 1944-2024 as one continuous constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_criterion, conceptual, 'Which reading is operative in which historical period, and whether period-based decomposition is needed.').

omega_variable(
    beneficiary_naturalization_risk,
    'Does market-efficiency framing of capital mobility (''capital flows to its most productive use'') function as a natural-law style justification that obscures the asymmetric beneficiary structure documented here?',
    'Compare returns to internationally mobile capital against wage and employment outcomes in states subject to IMF conditionality across the measured interval; a sustained asymmetry favoring mobile capital over domestic labor would indicate the efficiency framing functions as cover rather than description.',
    'If the efficiency framing is cover, the claimed_type of tangled_rope is generous — the coordination function may be thinner than the extraction it enables, pushing the classification toward snare in later periods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_naturalization_risk, empirical, 'Whether market-efficiency framing masks asymmetric extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1944, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1944, 0.15).
narrative_ontology:measurement(bret_tr_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1958, 0.22).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1971, 0.3).
narrative_ontology:measurement(bret_tr_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(bret_tr_t2000, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(bret_tr_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1944, 0.25).
narrative_ontology:measurement(bret_be_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1958, 0.35).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1971, 0.48).
narrative_ontology:measurement(bret_be_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(bret_be_t2000, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(bret_be_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1944, 0.2).
narrative_ontology:measurement(bret_su_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1958, 0.3).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1971, 0.45).
narrative_ontology:measurement(bret_su_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(bret_su_t2000, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(bret_su_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, enforcement_mechanism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, sovereignty_defense).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the bretton_woods_treaty_substrate kernel. keynesian_embedded_liberalism reads the same treaty text as protecting domestic policy autonomy from capital mobility (capital controls as legitimate defensive tools; capital as the constrained party). sovereignty_defense reads it as preserving national monetary sovereignty against external monetary discipline. neoliberal_convertibility (this story) reads it as constraining government intervention to enable capital mobility (capital controls as violations; national policy autonomy as the constrained party). Each reading has a distinct epsilon, distinct beneficiary/victim sets, and distinct classification — they are linked, not merged, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
