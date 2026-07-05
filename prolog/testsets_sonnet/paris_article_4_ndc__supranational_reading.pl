% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__supranational_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: paris_article_4_ndc__supranational_reading
 *   human_readable: Paris Agreement Article 4 NDCs as Binding Ratchet Toward Net-Zero (Supranational Reading)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Paris Agreement
 *   Article 4 kernel: the supranational reading, under which Nationally
 *   Determined Contributions are treated as binding commitments on an
 *   internationally accountable, ratcheting trajectory toward net-zero. Under
 *   this reading, the treaty's 'ambition mechanism' (successive NDC cycles,
 *   the global stocktake, enhanced transparency framework) functions as a de
 *   facto enforcement architecture — non-compliance carries reputational
 *   sanction through public stocktake findings and increasingly, financial
 *   sanction through conditional climate finance and green-trade instruments.
 *   This reading is distinct from the sovereigntist reading (NDCs as
 *   voluntary, non-binding, nationally self-determined pledges preserving
 *   energy sovereignty — a different constraint, with much lower ε) and from
 *   the equity reading (NDCs governed by Common But Differentiated
 *   Responsibilities requiring structural North-South distinctions — a
 *   different beneficiary/victim structure again). The three readings are not
 *   the same constraint measured three ways; they are three different
 *   constructions of what Article 4 obligates, each with its own coordination
 *   function, victim set, and enforcement mechanism. This file addresses only
 *   the supranational reading.
 *
 * KEY AGENTS:
 *   - unfccc_secretariat_and_treaty_bodies: administers the ratchet and stocktake (institutional/analytical) — agenda setter
 *   - climate_vulnerable_states: primary beneficiary of binding enforceability (organized/constrained)
 *   - carbon_intensive_industries: primary target of regulatory extinction under the ratchet (powerful/trapped)
 *   - fossil_fuel_dependent_developing_states: bears foreclosed development pathway without equivalent absorption capacity (moderate/trapped)
 *   - coal_region_workers: bears concentrated local cost with no seat at the table (powerless/trapped)
 *   - energy_sovereignty_holdout_states: disputes the binding characterization itself and faces sanction for the dispute (organized/constrained)
 *   - renewable_energy_industry: beneficiary whose market is created by the ratchet (organized/mobile)
 *   - multilateral_climate_finance_institutions: beneficiary and co-agenda-setter through conditional finance (institutional/arbitrage)
 *   - wealthy_high_emitting_states: dual position — funder/target on paper, but retains leverage to slow-walk enforcement against itself (institutional/mobile)
 *   - independent_climate_science_bodies: analytical observer establishing the carbon-budget baseline (analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.71).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.62).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris Agreement Article 4 NDCs as Binding Ratchet Toward Net-Zero (Supranational Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, 'adb09dc9-8c9f-44d1-ac42-36009bf350b8').
narrative_ontology:cs_kernel_codification('adb09dc9-8c9f-44d1-ac42-36009bf350b8', fixed_text).
narrative_ontology:cs_authority_grounding('adb09dc9-8c9f-44d1-ac42-36009bf350b8', extraction).
narrative_ontology:cs_interpretation_layer_present('adb09dc9-8c9f-44d1-ac42-36009bf350b8').
narrative_ontology:cs_reading_relation('adb09dc9-8c9f-44d1-ac42-36009bf350b8', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('adb09dc9-8c9f-44d1-ac42-36009bf350b8', paris_article_4_ndc__equity_reading, influences).
narrative_ontology:cs_axiom('adb09dc9-8c9f-44d1-ac42-36009bf350b8', foundational, ndc_trajectory_is_legally_binding).
narrative_ontology:cs_axiom_status(ndc_trajectory_is_legally_binding, holdable).
narrative_ontology:cs_axiom_grounding('adb09dc9-8c9f-44d1-ac42-36009bf350b8', ndc_trajectory_is_legally_binding, conventional).
narrative_ontology:cs_axiom('adb09dc9-8c9f-44d1-ac42-36009bf350b8', secondary, reputational_financial_sanction_is_legitimate_enforcement).
narrative_ontology:cs_axiom_status(reputational_financial_sanction_is_legitimate_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('adb09dc9-8c9f-44d1-ac42-36009bf350b8', reputational_financial_sanction_is_legitimate_enforcement, instrumental).
narrative_ontology:cs_reference_frame('adb09dc9-8c9f-44d1-ac42-36009bf350b8', kyoto_voluntary_pledge_failure_baseline).
narrative_ontology:cs_drift_state('adb09dc9-8c9f-44d1-ac42-36009bf350b8', post_paris_ratchet_institutionalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('adb09dc9-8c9f-44d1-ac42-36009bf350b8', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, climate_vulnerable_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, renewable_energy_industry).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, multilateral_climate_finance_institutions).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, international_secretariat_bodies).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_dependent_developing_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, coal_region_workers).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, energy_sovereignty_holdout_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, wealthy_high_emitting_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, wealthy_high_emitting_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the ratchet mechanism, compiles the global stocktake, and certifies whether state NDCs and their implementation meet the tightening trajectory required for net-zero pathways. Has no enforcement army but shapes reputational consequences through public compliance reporting and technical review processes that feed into diplomatic and financial leverage exercised by other bodies.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, unfccc_secretariat_and_treaty_bodies, agenda_setter,
    institutional, civilizational, analytical, global).

% Small island states and low-lying nations whose survival depends on aggressive global mitigation. Push hardest for binding ratchets and compliance mechanisms because voluntary pledges historically produced insufficient action; benefit directly from any enforceable trajectory toward net-zero, though their own leverage to compel compliance from large emitters remains limited.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, climate_vulnerable_states, beneficiary,
    organized, generational, constrained, global).

% Coal, oil, gas, cement, and heavy manufacturing sectors whose business models require continued high emissions. Under the ratchet reading, they face progressively tightening carbon budgets, stranded-asset risk, and regulatory extinction on a fixed timeline they cannot renegotiate once binding targets are locked into successive NDC cycles. Exit means relocating to non-compliant jurisdictions or ceasing to exist as currently constituted.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    powerful, biographical, trapped, global).

% States whose government revenue and export earnings depend on fossil fuel extraction but who lack the diversified economy of wealthy emitters. Under a binding-ratchet reading their development pathway is foreclosed on the same timeline as wealthy states' surplus-driven transitions, without equivalent absorption capacity. Their objections that ratchets ignore differentiated starting positions are heard in negotiations but rarely alter the trajectory's binding character.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_dependent_developing_states, payer,
    moderate, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, fossil_fuel_dependent_developing_states, excluded).

% Workers and communities whose livelihoods depend on the industries the ratchet is designed to phase out. They bear the concentrated local cost of a global trajectory decided in international forums where they have no seat; just-transition funding, where it exists, arrives slower than facility closures.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, coal_region_workers, payer,
    powerless, biographical, trapped, regional).

% States that signed the Paris Agreement expecting NDCs to remain nationally self-determined but now face reputational and financial sanction — credit downgrades, exclusion from green finance, diplomatic isolation — for failing to ratchet ambition on the schedule the supranational reading treats as binding. They argue the treaty text itself never made the ratchet legally binding and experience compliance pressure as an imposed reinterpretation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, energy_sovereignty_holdout_states, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, energy_sovereignty_holdout_states, excluded).

% Solar, wind, battery, and grid-technology firms whose market growth is directly created by binding decarbonization schedules. Lobby actively for stronger ratchet enforcement because every tightened NDC cycle expands their addressable market; can relocate operations to wherever compliance pressure is strongest.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, renewable_energy_industry, beneficiary,
    organized, biographical, mobile, global).

% Green Climate Fund and allied bodies administer conditional finance flows tied to NDC compliance, gaining institutional mandate, budget, and staff growth from the enforcement architecture. Their continued relevance depends on the ratchet being treated as binding rather than aspirational.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, multilateral_climate_finance_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, multilateral_climate_finance_institutions, agenda_setter).

% Large historical emitters bear obligations to both tighten domestic targets and fund transfers to the Global South, but retain enough diplomatic and economic leverage to slow-walk enforcement against themselves while it bites hardest on smaller and fossil-dependent states; benefit from first-mover advantage in green technology export markets created by the same ratchet.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, wealthy_high_emitting_states, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, wealthy_high_emitting_states, beneficiary).

% IPCC and allied scientific assessment bodies establish the carbon budgets the ratchet trajectory is calibrated against, without directly enforcing compliance or receiving financial benefit from the treaty architecture.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, independent_climate_science_bodies, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common, escalating global emissions trajectory so that no state's unilateral inaction undercuts collective action against a problem no single actor can solve alone, and gives climate-vulnerable states a mechanism to demand accountability from large emitters.
% TRANSFER_FUNCTION: Moves regulatory certainty and market share toward renewable-energy industries and climate-vulnerable states, and moves compliance costs, stranded-asset risk, and reputational/financial sanction risk onto carbon-intensive industries, fossil-fuel-dependent developing states, and states asserting the treaty's voluntary character.
% ABSENT_VOICES: Coal-region workers and fossil-fuel-dependent state populations are rarely present at COP-level ratchet negotiations; energy-sovereignty holdout states raise the voluntariness objection formally but are outvoted by the diplomatic weight favoring binding interpretation. Their objection — that the treaty text says 'nationally determined' and the ratchet reading substitutes an interpretive gloss for the negotiated text — is on record but structurally sidelined.
% DISAPPEARANCE_RATIONALE: Climate-vulnerable states and finance institutions would say the world rearranges catastrophically if the binding-ratchet reading disappeared — mitigation ambition would stall to the sovereigntist floor. Energy-sovereignty holdout states and carbon-intensive industries would say very little changes structurally, since national policy already diverges from stated NDC trajectories in practice; the binding character is asserted more than enforced. This is exactly the underlying kernel contest this story is one reading of.
% FOUNDING_PROBLEM: Voluntary, unenforceable pledges under the Kyoto Protocol model produced insufficient aggregate ambition and allowed major emitters to exit or ignore commitments without consequence; the ratchet-with-accountability architecture was built to close that gap by treating the trajectory itself, not just the pledge-making process, as the binding object.
% FOUNDING_PROBLEM_CORROBORATION: Independent climate science bodies (IPCC) corroborate that the emissions gap between current NDCs and required trajectories remains open, supporting the claim that the founding problem is live. However, treaty-law scholars outside both the vulnerable-state and finance-institution coalitions note that Article 4's actual text commits states only to 'pursue domestic measures' toward self-determined targets, not to a legally binding trajectory — meaning the 'binding ratchet' status is itself a contested interpretive claim rather than a settled textual fact, corroborated by no single authoritative body outside the parties who benefit from the binding reading.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, contested).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__supranational_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__supranational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__supranational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) and rising because under the supranational reading, the ratchet mechanism increasingly forecloses policy space for carbon-dependent economies on a schedule set externally, with financial and reputational consequences attaching to non-compliance. Suppression is authored moderate-high (0.62) and rising over the interval, reflecting the maturation of the enhanced transparency framework and the growing willingness of finance institutions and trade partners to condition access on NDC compliance — this is enforcement-capacity hardening, not merely extraction, which is why suppression_requirement is tracked separately and rises on its own trajectory. Theater ratio starts moderate-high (0.55) reflecting early-Paris-era pledge theater with weak follow-through, and falls modestly as the stocktake and compliance architecture mature into genuine (if contested) accountability machinery — the theater is being displaced by enforcement, not increasing. Accessibility collapse is moderate (0.48): alternatives to the ratchet framework (bilateral deals, sub-national action, voluntary carbon markets) remain available, so collapse is not mountain-grade. Resistance is high (0.74) because carbon-intensive industries, fossil-fuel states, and sovereigntist-reading holdouts actively contest the binding characterization in negotiations, domestic courts, and diplomatic fora.
 *
 * DIRECTIONALITY LOGIC:
 *   Climate-vulnerable states and the finance/secretariat institutions sit near the beneficiary end: they collect enforceability, market growth, and institutional mandate from treating the ratchet as binding. Carbon-intensive industries, coal-region workers, and fossil-fuel-dependent developing states sit near the target end: trapped exit options and concentrated, schedule-fixed costs. Energy-sovereignty holdout states are treated as targets despite institutional/organized power because their exit option is constrained by reputational and financial machinery specifically built to prevent the exit their name implies — this is a case where positional power does not translate into positional exit, and an observer might expect to override d downward; I have NOT overridden it, because the derivation from victim declaration + constrained exit already captures the asymmetry the story needs to show: sanction infrastructure targeting a nominally organized/sovereign actor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (insufficient ambition under fully voluntary Kyoto-style pledges) remains partially live per IPCC gap-analysis, which prevents automatic classification as a dead-mandate piton. But the founding_problem_status is authored 'contested' rather than 'live' because independent treaty-law scholarship disputes that Article 4's text ever created a legally binding trajectory — meaning part of what is being enforced may be an institutional reinterpretation of the mandate rather than the mandate itself. This is precisely the kind of divergence the classification exists to surface: a tangled_rope claim whose coordination function (closing the ambition gap) is real and whose extraction (schedule-fixed foreclosure on fossil-dependent economies, enforced through finance and reputation) is also real and asymmetric — hence tangled_rope rather than pure rope or pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_bindingness_of_article_4,
    'Does Article 4''s actual treaty text (''pursue domestic measures with the aim of achieving the objectives of such contributions'') create a legally binding obligation to ratchet ambition on a fixed trajectory, or only a procedural obligation to submit and update pledges?',
    'International Court of Justice advisory opinion or authoritative treaty-law scholarship consensus on whether the enhanced transparency framework and global stocktake constitute binding enforcement mechanisms or merely reporting/review procedures without independent legal force.',
    'If the text is found non-binding, the supranational reading''s claim to legitimate enforcement collapses into the sovereigntist reading''s voluntary-pledge structure, and much of the authored extractiveness and suppression here would be recharacterized as extra-textual institutional overreach rather than treaty-grounded accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_bindingness_of_article_4, conceptual, 'Whether Article 4 text supports a legally binding ratchet or only voluntary procedural pledges — the central kernel contest.').

omega_variable(
    north_south_burden_distribution_under_ratchet,
    'Does the binding-ratchet mechanism, as actually administered, apply the same schedule and sanction severity to wealthy high-emitting states as to fossil-fuel-dependent developing states, or does enforcement asymmetrically concentrate on states with less diplomatic leverage?',
    'Comparative analysis of stocktake findings, finance conditionality enforcement, and trade-measure application across wealthy vs. developing non-compliant states over multiple NDC cycles.',
    'If enforcement is asymmetric by leverage rather than by CBDR-differentiated obligation, the supranational reading is functioning as a tangled_rope that extracts disproportionately from less powerful states under cover of a formally uniform trajectory — strengthening the victim classification of fossil_fuel_dependent_developing_states relative to wealthy_high_emitting_states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(north_south_burden_distribution_under_ratchet, empirical, 'Whether ratchet enforcement is applied uniformly or asymmetrically by state power.').

omega_variable(
    supranational_reading_naturalization_risk,
    'Is the supranational reading being presented by its proponents as the only technically correct reading of the treaty (a false-mountain move), obscuring that it is one of three live and contested interpretive constructions?',
    'Discourse analysis of UNFCCC secretariat communications and finance-institution materials: do they present bindingness as settled fact or as one interpretive position among the sovereigntist and equity readings?',
    'If the supranational reading is naturalized as simply ''what Paris means,'' the interpretive contest documented here becomes invisible and the reading''s extraction is laundered as neutral treaty implementation rather than a contested political victory of one coalition''s interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_reading_naturalization_risk, conceptual, 'Whether the supranational reading is being presented as settled fact rather than one contested reading among three.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__supranational_reading, theater_ratio, 2015, 0.55).
narrative_ontology:measurement_basis(pari_tr_t2015, observed).
narrative_ontology:measurement(pari_tr_t2019, paris_article_4_ndc__supranational_reading, theater_ratio, 2019, 0.52).
narrative_ontology:measurement_basis(pari_tr_t2019, observed).
narrative_ontology:measurement(pari_tr_t2023, paris_article_4_ndc__supranational_reading, theater_ratio, 2023, 0.48).
narrative_ontology:measurement_basis(pari_tr_t2023, observed).
narrative_ontology:measurement(pari_tr_t2027, paris_article_4_ndc__supranational_reading, theater_ratio, 2027, 0.46).
narrative_ontology:measurement_basis(pari_tr_t2027, projected).
narrative_ontology:measurement(pari_tr_t2031, paris_article_4_ndc__supranational_reading, theater_ratio, 2031, 0.45).
narrative_ontology:measurement_basis(pari_tr_t2031, projected).
narrative_ontology:measurement(pari_tr_t2035, paris_article_4_ndc__supranational_reading, theater_ratio, 2035, 0.44).
narrative_ontology:measurement_basis(pari_tr_t2035, projected).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__supranational_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement_basis(pari_be_t2015, observed).
narrative_ontology:measurement(pari_be_t2019, paris_article_4_ndc__supranational_reading, base_extractiveness, 2019, 0.51).
narrative_ontology:measurement_basis(pari_be_t2019, observed).
narrative_ontology:measurement(pari_be_t2023, paris_article_4_ndc__supranational_reading, base_extractiveness, 2023, 0.62).
narrative_ontology:measurement_basis(pari_be_t2023, observed).
narrative_ontology:measurement(pari_be_t2027, paris_article_4_ndc__supranational_reading, base_extractiveness, 2027, 0.68).
narrative_ontology:measurement_basis(pari_be_t2027, projected).
narrative_ontology:measurement(pari_be_t2031, paris_article_4_ndc__supranational_reading, base_extractiveness, 2031, 0.7).
narrative_ontology:measurement_basis(pari_be_t2031, projected).
narrative_ontology:measurement(pari_be_t2035, paris_article_4_ndc__supranational_reading, base_extractiveness, 2035, 0.71).
narrative_ontology:measurement_basis(pari_be_t2035, projected).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__supranational_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement_basis(pari_su_t2015, observed).
narrative_ontology:measurement(pari_su_t2019, paris_article_4_ndc__supranational_reading, suppression_requirement, 2019, 0.4).
narrative_ontology:measurement_basis(pari_su_t2019, observed).
narrative_ontology:measurement(pari_su_t2023, paris_article_4_ndc__supranational_reading, suppression_requirement, 2023, 0.5).
narrative_ontology:measurement_basis(pari_su_t2023, observed).
narrative_ontology:measurement(pari_su_t2027, paris_article_4_ndc__supranational_reading, suppression_requirement, 2027, 0.57).
narrative_ontology:measurement_basis(pari_su_t2027, projected).
narrative_ontology:measurement(pari_su_t2031, paris_article_4_ndc__supranational_reading, suppression_requirement, 2031, 0.6).
narrative_ontology:measurement_basis(pari_su_t2031, projected).
narrative_ontology:measurement(pari_su_t2035, paris_article_4_ndc__supranational_reading, suppression_requirement, 2035, 0.62).
narrative_ontology:measurement_basis(pari_su_t2035, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__supranational_reading, 0.12).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__equity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the paris_article_4_ndc kernel, decomposed per the ε-invariance principle rather than authored as a single constraint with an observable-dependent ε. sovereigntist_reading treats NDCs as voluntary and non-binding (low ε, minimal victim set, rope-leaning). equity_reading organizes the constraint around CBDR-differentiated obligation rather than binding-vs-voluntary status (different beneficiary/victim split: wealthy states as obligated financers, developing states as beneficiaries of differentiated treatment). This file (supranational_reading) authors the highest-ε member of the triplet: binding ratchet with real enforcement teeth, tangled_rope-leaning, substantial victim set among carbon-dependent economies and workers. All three share the same treaty text as their kernel but instantiate structurally distinct constraints with distinct classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
