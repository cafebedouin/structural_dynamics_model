% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: paris_article_4_ndc__supranational_reading
 *   human_readable: NDC Binding Ratchet with International Accountability (Supranational Reading)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Paris Article 4 NDC kernel:
 *   the supranational reading, under which nationally determined
 *   contributions are binding commitments on a ratcheting trajectory toward
 *   net-zero, enforced through international accountability — transparency
 *   reviews, global stocktakes, compliance machinery, finance conditionality,
 *   and market-access linkage. The constraint's ε referent is this standing
 *   supranational accountability arrangement, assessed by the reading's own
 *   lights: the reading deems the arrangement necessary and legitimate, and
 *   still authors substantial extraction, because bindingness imposes real
 *   costs — regulatory extinction pressure on carbon-intensive industries,
 *   contracted rent streams for fossil exporters, treasury and household
 *   burdens in developed states, institutionalized North-South transfers. Per
 *   the one-reading rule, the contest over Article 4 is NOT described inside
 *   this constraint: the sovereigntist and equity readings are separate
 *   constraint files, linked through network.affects_constraints, each with
 *   its own ε, victim set, and classification. KEY AGENTS (by structural
 *   relationship): - unfccc_climate_institutions: Agenda setter
 *   (institutional/identity_locked) — administers review, stocktake, and
 *   compliance machinery - small_island_states: Primary beneficiary
 *   (organized/trapped) — converts survival stakes into accountability
 *   leverage - least_developed_countries: Beneficiary and transfer recipient
 *   (organized/constrained) - renewable_energy_sectors: Beneficiary
 *   (powerful/arbitrage) — captures mandated-demand rents -
 *   carbon_intensive_industries: Primary target (powerful/constrained) —
 *   faces regulatory extinction pressure - fossil_fuel_exporting_states:
 *   Target (institutional/constrained) — rent contraction along the ratchet
 *   path - developed_state_households: Payer (moderate/constrained) — double
 *   burden via transfers and price pass-through -
 *   emerging_industrial_economies: Dual-positioned beneficiary/payer
 *   (powerful/constrained) - future_generations: Excluded (powerless/trapped)
 *   — outcome-risk bearer with no seat - ipcc_assessment_body: Analytical
 *   observer — defines the ambition gap the ratchet acts on
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.7).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.63).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "NDC Binding Ratchet with International Accountability (Supranational Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, 'fe667d52-0709-4188-b885-7d7a592b833e').
narrative_ontology:cs_kernel_codification('fe667d52-0709-4188-b885-7d7a592b833e', fixed_text).
narrative_ontology:cs_authority_grounding('fe667d52-0709-4188-b885-7d7a592b833e', lineage).
narrative_ontology:cs_interpretation_layer_present('fe667d52-0709-4188-b885-7d7a592b833e').
narrative_ontology:cs_reading_relation('fe667d52-0709-4188-b885-7d7a592b833e', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('fe667d52-0709-4188-b885-7d7a592b833e', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('fe667d52-0709-4188-b885-7d7a592b833e', foundational, ndc_commitments_legally_binding).
narrative_ontology:cs_axiom_status(ndc_commitments_legally_binding, holdable).
narrative_ontology:cs_axiom_grounding('fe667d52-0709-4188-b885-7d7a592b833e', ndc_commitments_legally_binding, conventional).
narrative_ontology:cs_axiom('fe667d52-0709-4188-b885-7d7a592b833e', foundational, ambition_progression_obligatory).
narrative_ontology:cs_axiom_status(ambition_progression_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('fe667d52-0709-4188-b885-7d7a592b833e', ambition_progression_obligatory, instrumental).
narrative_ontology:cs_reference_frame('fe667d52-0709-4188-b885-7d7a592b833e', binding_supranational_ratchet).
narrative_ontology:cs_drift_state('fe667d52-0709-4188-b885-7d7a592b833e', post_first_global_stocktake, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fe667d52-0709-4188-b885-7d7a592b833e', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, small_island_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, renewable_energy_sectors).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, unfccc_climate_institutions).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, developed_state_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, emerging_industrial_economies).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, emerging_industrial_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the transparency reviews, convenes the global stocktakes, and operates the compliance committee that translate pledged targets into reviewed, compared, and publicly scored performance. Budgets, staffing, and mandate expand with each decision cycle. Staff careers and the institutions' self-conception are constituted by the regime; dissolution of the accountability architecture would dissolve the institutions themselves.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, unfccc_climate_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Negotiate as a coalition and gain accountability leverage over major emitters whose cumulative emissions determine their physical survival. They cannot exit the climate system, so their entire strategy depends on the review-and-stocktake machinery keeping ambition comparisons honest and public.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, small_island_states, beneficiary,
    organized, generational, trapped, global).

% Receive institutionalized climate finance, adaptation support, and capacity-building flows, and acquire standing to contest developed-state backsliding through the review processes. Limited domestic fiscal capacity makes the continuation of transfers material to their transition plans.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, least_developed_countries, beneficiary,
    organized, generational, constrained, global).

% Benefit from the demand trajectory the ratchet implies: mandated deployment paths, subsidized buildout, and investor certainty anchored to successive target rounds. Capital is mobile and reallocates toward jurisdictions whose commitments signal stronger long-term demand.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, renewable_energy_sectors, beneficiary,
    powerful, biographical, arbitrage, global).

% Face tightening emission obligations, mandatory disclosure, and contracting demand along the ratchet trajectory. Plants, mines, and refining assets are site-specific and long-lived, so exit means stranding rather than relocation. They lobby for delay, compensation, and reinterpretation of target accounting.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    powerful, biographical, constrained, global).

% State revenue depends on hydrocarbon rent streams that the net-zero trajectory contracts. Formal participation in the regime coexists with strategic dilution of accountability language in negotiations; diversification away from rent dependence is slow, capital-intensive, and politically destabilizing to attempt.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_states, payer,
    institutional, generational, constrained, global).

% Bear the constraint's costs twice: through treasury-financed climate transfers and through energy-price pass-through from decarbonization mandates. Representation runs through executive delegations they do not directly control; exit means emigration, which few exercise.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, developed_state_households, payer,
    moderate, biographical, constrained, national).

% Receive finance and technology-transfer flows and hold differentiation headroom today, while the ratchet trajectory schedules progressively harder obligations for them as output and emissions grow. They negotiate to defer the payer side of their own position as far into the future as possible.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, emerging_industrial_economies, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, emerging_industrial_economies, payer).

% Bear the outcome risk of whatever ambition level survives the accountability process but hold no seat in any negotiation round. Their interests enter only through advocacy proxies embedded inside state delegations and observer organizations.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, future_generations, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(paris_article_4_ndc__supranational_reading, future_generations).

% Produces the scientific assessments that define the gap between aggregate pledged ambition and stabilization pathways. Collects no rents from the regime and bears no compliance burden; its findings feed the stocktake cycle that drives ratchet pressure.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, ipcc_assessment_body, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__supranational_reading, least_developed_countries).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__supranational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the global free-rider problem in emission reduction: a single accounting, review, and stocktake architecture makes each state's effort visible and comparable, lowering the transaction cost of mutual verification and enabling reciprocal ambition-raising toward a shared atmospheric budget.
% TRANSFER_FUNCTION: Moves decarbonization obligations onto carbon-intensive industries and fossil-rent-dependent economies; moves climate finance from developed-state treasuries to developing-state mitigation and adaptation; moves reputational and legal standing between states according to reviewed target performance.
% ABSENT_VOICES: Future generations hold no seat though they bear the residual outcome risk. Domestic households bearing transfer and energy-price costs are represented only indirectly through executive delegations. Workers and communities dependent on carbon-intensive production enter chiefly through state delegations that weight their interests against national bargaining positions. All three sit outside the negotiation perimeter, present only as proxies.
% DISAPPEARANCE_RATIONALE: If the binding-accountability architecture vanished overnight, the reciprocal ambition cycle would stop — no reviews, no stocktake comparisons, no public scoring — and burden-sharing disputes would revert to ad hoc bilateral friction. Climate finance flows would lose their reporting anchor, corporate disclosure chains built on NDC alignment would unwind, and climate litigation strategies premised on reviewed commitments would lose their object. The diplomatic, financial, and legal arrangements organized around the ratchet would all rearrange.
% FOUNDING_PROBLEM: After the previous top-down binding-target regime collapsed around non-participation of major emitters, the problem was how to secure universal participation while preventing free-riding — obtaining commitments every state would join without sacrificing the escalation mechanism needed to close the emissions gap.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessments document the continuing gap between aggregate commitments and stabilization pathways from outside the beneficiary set. Academic regime-effectiveness literature independently tracks compliance and ambition shortfalls. Adversarially, governments holding the sovereigntist reading attest the free-rider problem's reality through their own fairness objections — arguing others are not doing enough presupposes the coordination stakes. Insurance and reinsurance physical-risk pricing corroborates the underlying problem the arrangement addresses.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__supranational_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.70 at interval end) because the ratchet's costs are concentrated and growing: asset stranding in heavy industry, rent contraction for exporters, and institutionalized transfers financed by developed-state treasuries and households. Suppression (0.63) is a raw structural property, unscaled by power or scope: persistence depends on actively closing exits — finance conditionality, disclosure mandates, and market-access linkage penalize non-compliance, while formal withdrawal remains available but reputationally and financially costly. Theater ratio DECLINES across the interval (0.55 to 0.31): early operation was dominated by pledge announcement and summit ceremony; adoption of the rulebook, launch of enhanced transparency reporting, and the first global stocktake shifted activity toward functional review work. Rising extractiveness alongside falling theater models institutional maturation, not decay — the machinery got better at doing what it does, and what it does is costly. Resistance (0.58) reflects petrostate obstruction, the major-power withdrawal episode, and sustained industry lobbying; accessibility_collapse (0.45) stays mid-range because alternatives — unilateral abstention, adaptation-only strategies, geoengineering, withdrawal — remain partially available given incomplete enforcement. The three measurement series run on one shared time grid ({0,2,4,6,8,10}); COP-cycle oscillation in ambition rhetoric averages out at two-year sampling, so the series are modeled as monotonic trends rather than cycles. Identity-lock note: the agenda-setter seat is institutionally fused — the organizations have become their function, so exit is unthinkable from inside regardless of external incentives.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat, the arrangement is legitimate institution-building: each review cycle strengthens the accountability it exists to provide. From the target seats — carbon-intensive industries and fossil exporters — the same machinery operates as an extinction schedule with a procedural face. From the beneficiary seats, it is the only available lever on actors whose emissions determine their survival. From developed-state households, it is a cost stream they never voted on directly. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: small_island_states (trapped exit amplifies their beneficiary-side position — they cannot leave the climate system, so the constraint subsidizes their bargaining position absolutely), least_developed_countries (constrained, transfer-receiving), renewable_energy_sectors (arbitrage-grade exit places them nearest the full-beneficiary end — they capture the demand the ratchet creates and can relocate if any jurisdiction's commitments weaken), and unfccc_climate_institutions (budget and mandate growth accrue to the administrator). Victim declarations drive high directionality: carbon_intensive_industries (constrained by asset specificity — stranding, not relocation), fossil_fuel_exporting_states (constrained by rent dependence), developed_state_households (constrained, double-burdened). Emerging_industrial_economies sit mid-spectrum: today's transfers flow to them, but the ratchet schedules their own binding obligations forward, so their derived position blends beneficiary and target. No directionality overrides were needed: the beneficiary/victim declarations plus exit-option differentiation already separate the same-power agents (renewables vs. heavy industry both powerful, opposite ends).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — universal participation without free-riding — remains live, so no mandatrophy resolution is declared and the status-by-verdict pair (live x world_rearranges) raises no zombie flag. Classification discipline cuts both ways here. Labeling the arrangement a pure snare fails because the coordination function is genuine and central: climate stabilization is the paradigm collective-action problem, and the review-and-stocktake architecture is a real solution to free-riding, not cover for it. Labeling it a pure rope fails because extraction through the same structure is substantial and asymmetric: identifiable victims bear regulatory extinction and rent contraction while identifiable seats capture mandated demand, finance flows, and institutional budgets. Tangled rope holds both facts. The falling theater ratio additionally guards against piton misclassification — the machinery is consolidating functionally, not performing a dead function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paris_kernel_reading_position,
    'This constraint is the supranational_reading of kernel paris_article_4_ndc — what structural deltas would the sovereigntist_reading and equity_reading instantiations produce, and where exactly is the disagreement located?',
    'Comparative classification across the three sibling story files; divergent epsilon values and victim/beneficiary sets locate the structural disagreement (bindingness, differentiation, accountability).',
    'The sovereigntist instantiation yields a low-extraction voluntary-coordination constraint with no victim set; the equity instantiation redistributes victim and beneficiary positions along development categories; only this reading carries binding-accountability extraction with regulatory-extinction victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(paris_kernel_reading_position, conceptual, 'Committer structure: one reading of the Article 4 kernel, with sibling readings as separate constraints.').

omega_variable(
    bindingness_descriptive_accuracy,
    'Does the accountability machinery actually constrain state behavior through material consequences (finance conditionality, market-access linkage, litigation exposure), or does compliance in practice remain reputational only?',
    'Compliance-rate studies, incident data on withheld finance and applied border adjustments, and tracking of domestic litigation that invokes reviewed NDC performance.',
    'If consequences are reputational only, the authored epsilon is overstated and the operative constraint converges toward the sovereigntist instantiation; if material consequences bind, the high-extraction profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bindingness_descriptive_accuracy, empirical, 'Whether the supranational reading''s bindingness claim is descriptively true of the machinery''s operation.').

omega_variable(
    transfer_incidence,
    'Who ultimately bears the North-South transfer burden — general treasuries, household energy prices, or compressed industry margins?',
    'Fiscal incidence analysis of climate-finance budget lines combined with energy-price pass-through studies in contributing states.',
    'Determines which payer seat computes the highest effective extraction; shifts per-seat classifications between developed_state_households and carbon_intensive_industries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_incidence, empirical, 'Incidence of the institutionalized wealth transfers across payer seats.').

omega_variable(
    extinction_vs_managed_transition,
    'Does the ratchet trajectory impose regulatory extinction on carbon-intensive industries, or a compensated managed decline with stranding absorbed by adjustment policy?',
    'Asset-stranding valuations versus phase-out compensation legislation across jurisdictions; comparative study of coal, steel, and refining closure regimes.',
    'An extinction reading raises victim-seat extraction sharply and supports snare-drift hypotheses; a managed-transition reading preserves the tangled-rope balance between coordination and extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extinction_vs_managed_transition, empirical, 'Severity of the victim position for carbon-intensive industry under the ratchet.').

omega_variable(
    cs_framing_text_vs_practice,
    'Is the operative kernel the Article 4 treaty text (fixed_text/lineage framing used here), or the accumulating COP decision practice and stocktake outcomes that increasingly constitute the regime''s actual rules?',
    'Examine what parties and tribunals cite when accountability is contested: the treaty text, or CMA decisions and stocktake outputs. The framing choice was guided by the text''s citation primacy in formal dispute contexts.',
    'A practice-framing would move kernel_codification toward distributed and authority_grounding toward practice, and would re-read the drift vector as codification_collapse rather than practice_drift — the written kernel ceding constitutive authority to accumulated decisions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_text_vs_practice, conceptual, 'Commitment-system framing under-determination: text-kernel versus practice-kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__supranational_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(pari_tr_t2, paris_article_4_ndc__supranational_reading, theater_ratio, 2, 0.5).
narrative_ontology:measurement(pari_tr_t4, paris_article_4_ndc__supranational_reading, theater_ratio, 4, 0.44).
narrative_ontology:measurement(pari_tr_t6, paris_article_4_ndc__supranational_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(pari_tr_t8, paris_article_4_ndc__supranational_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(pari_tr_t10, paris_article_4_ndc__supranational_reading, theater_ratio, 10, 0.31).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__supranational_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(pari_be_t2, paris_article_4_ndc__supranational_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(pari_be_t4, paris_article_4_ndc__supranational_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(pari_be_t6, paris_article_4_ndc__supranational_reading, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(pari_be_t8, paris_article_4_ndc__supranational_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(pari_be_t10, paris_article_4_ndc__supranational_reading, base_extractiveness, 10, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__supranational_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(pari_su_t2, paris_article_4_ndc__supranational_reading, suppression_requirement, 2, 0.42).
narrative_ontology:measurement(pari_su_t4, paris_article_4_ndc__supranational_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(pari_su_t6, paris_article_4_ndc__supranational_reading, suppression_requirement, 6, 0.54).
narrative_ontology:measurement(pari_su_t8, paris_article_4_ndc__supranational_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement(pari_su_t10, paris_article_4_ndc__supranational_reading, suppression_requirement, 10, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__equity_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, carbon_border_adjustment_mechanisms).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'Paris Article 4 NDCs' decomposes into three structurally distinct constraints — one per reading of the kernel. This supranational reading instantiates binding commitments with international accountability (high epsilon, extinction-pressure victims, institutionalized transfers). The sovereigntist sibling instantiates voluntary self-determined pledges (low epsilon, no victim set, coordination-by-transparency only). The equity sibling instantiates differentiated obligations structured by development category (redistributed victim/beneficiary sets along CBDR lines). The upstream/downstream gradient runs from this reading outward: bindingness claims are cited as interpretive pressure on both siblings' operating environments. Each file links the others via network.affects_constraints; epsilon differs across the family because the readings instantiate different constraints, not because one constraint is measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
