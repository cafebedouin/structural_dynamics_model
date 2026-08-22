% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__hybrid_selectivity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: structural_adjustment_conditionalities__hybrid_selectivity_reading
 *   human_readable: Structural Adjustment Conditionalities — Hybrid Selectivity Reading
 *   domain: international political economy / development finance / institutional economics
 *
 * SUMMARY:
 *   The conditionalities regime attached to sovereign crisis lending requires
 *   debtor states to adopt fiscal austerity, privatization, trade
 *   liberalization, and subsidy removal as the price of financing. This story
 *   instantiates the hybrid_selectivity_reading of that kernel: the
 *   discipline is real but is applied by geopolitical position rather than by
 *   rule — enforced to the letter on weak, peripheral debtors and waived or
 *   softened for strategically aligned ones (front-line states, basing
 *   partners, alliance votes). KEY AGENTS (by structural relationship): the
 *   creditor-side institutions and shareholder governments who set and
 *   selectively enforce terms; the core financial creditors whose claims the
 *   framework services; hegemon-aligned strategic debtors who buy softened
 *   treatment with alignment; geopolitically peripheral debtors and their
 *   populations who bear the full schedule; export industries in creditor
 *   countries who collect opened markets without participating; excluded
 *   debtor civil society; and the analytical observers who document the
 *   waiver asymmetries. Per the ε-referent rule, ε is authored for the
 *   standing conditionalities arrangement as this reading assesses it — high
 *   but not maximal, because the coordination function is genuine — and never
 *   for the uniform-treatment alternative this reading would endorse. The
 *   claim (tangled_rope) and the metrics are independent authored facts; the
 *   engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - imf_conditionality_architects: agenda-setter (institutional / identity_locked) — designs and enforces program conditions, administers waivers; its operational identity is fused with the coordinator role
 *   - g7_shareholder_governments: agenda-setter and beneficiary (institutional / arbitrage) — quota control, effective veto, and the waiver decisions that constitute the selectivity this reading is about
 *   - core_creditor_financial_institutions: primary beneficiary (institutional / arbitrage) — debt service flows to them; losses are socialized through official rescue packages
 *   - hegemon_aligned_strategic_debtors: beneficiary with payer costs (organized / constrained) — softened terms purchased with geopolitical alignment
 *   - geopolitically_peripheral_debtors: primary target (moderate / trapped) — full conditionality enforced, alternatives closed
 *   - debtor_state_populations: primary target (powerless / trapped) — bear the austerity measures, hold no design seat
 *   - creditor_country_export_industries: secondary beneficiary (powerful / mobile) — collect opened markets and privatization acquisitions without participating in the lending arrangement
 *   - debtor_civil_society_advocates: excluded voice (moderate / constrained) — would object to selectivity and cost distribution; consulted after terms are fixed
 *   - development_economists: analytical observer (analytical / analytical) — sees the full structure, including the waiver asymmetries the official framing denies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.7).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.7).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Structural Adjustment Conditionalities — Hybrid Selectivity Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international political economy / development finance / institutional economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'ea586e55-0c43-42ba-bfe6-af1478b337d8').
narrative_ontology:cs_kernel_codification('ea586e55-0c43-42ba-bfe6-af1478b337d8', formalized).
narrative_ontology:cs_authority_grounding('ea586e55-0c43-42ba-bfe6-af1478b337d8', extraction).
narrative_ontology:cs_interpretation_layer_present('ea586e55-0c43-42ba-bfe6-af1478b337d8').
narrative_ontology:cs_reading_relation('ea586e55-0c43-42ba-bfe6-af1478b337d8', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea586e55-0c43-42ba-bfe6-af1478b337d8', structural_adjustment_conditionalities__debtor_extraction_reading, influences).
narrative_ontology:cs_axiom('ea586e55-0c43-42ba-bfe6-af1478b337d8', foundational, enforcement_tracks_geopolitical_alignment_not_merit).
narrative_ontology:cs_axiom_status(enforcement_tracks_geopolitical_alignment_not_merit, holdable).
narrative_ontology:cs_axiom_grounding('ea586e55-0c43-42ba-bfe6-af1478b337d8', enforcement_tracks_geopolitical_alignment_not_merit, empirically_contingent).
narrative_ontology:cs_axiom('ea586e55-0c43-42ba-bfe6-af1478b337d8', foundational, coordination_genuine_but_asymmetrically_burdened).
narrative_ontology:cs_axiom_status(coordination_genuine_but_asymmetrically_burdened, holdable).
narrative_ontology:cs_axiom_grounding('ea586e55-0c43-42ba-bfe6-af1478b337d8', coordination_genuine_but_asymmetrically_burdened, empirically_contingent).
narrative_ontology:cs_reference_frame('ea586e55-0c43-42ba-bfe6-af1478b337d8', uniform_rules_based_discipline_framework).
narrative_ontology:cs_drift_state('ea586e55-0c43-42ba-bfe6-af1478b337d8', contemporary_multipolar_creditor_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ea586e55-0c43-42ba-bfe6-af1478b337d8', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, g7_shareholder_governments).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_financial_institutions).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_strategic_debtors).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, creditor_country_export_industries).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_peripheral_debtors).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, debtor_state_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_strategic_debtors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs loan conditions through staff missions and Executive Board approval; administers reviews, waivers, and program suspension. Its budget, staffing pipeline, and institutional self-conception are bound up with being the sovereign crisis coordinator; the organization has become the function it performs, and no alternative institutional role is available to it at comparable scale.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, imf_conditionality_architects, agenda_setter,
    institutional, generational, identity_locked, global).

% Control quota-weighted voting shares, with the largest shareholder holding an effective veto over structural decisions. They decide when benchmarks are enforced and when they are waived, and their decisions track alliance relationships. Their export sectors gain opened markets under liberalization conditions, and they can lend outside the framework through bilateral loans and swap lines whenever bilateral influence is cheaper.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, g7_shareholder_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, g7_shareholder_governments, beneficiary).

% Hold sovereign bonds and bank claims on program countries. Framework-backed programs keep debt service flowing and socialize losses through official rescue packages when write-downs would otherwise fall on them. They can reprice or exit exposures through bond markets at will.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_financial_institutions, beneficiary,
    institutional, biographical, arbitrage, global).

% States whose geopolitical alignment — military basing, alliance voting, front-line status — purchases softened conditions, exceptional financing, and waived benchmarks. They still service debt, implement partial reforms, and carry the political cost of association with the program, but face a fraction of the discipline applied to otherwise similar states.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_strategic_debtors, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_strategic_debtors, payer).

% States without strategic leverage face the full conditionality schedule — fiscal austerity, privatization, subsidy removal, trade opening — with benchmarks enforced to the letter and suspension for missed reviews. Alternative creditors are limited or come with their own political strings; unilateral default means losing market access and follow-on donor financing at once.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_peripheral_debtors, payer,
    moderate, biographical, trapped, national).

% Bear subsidy removal, public wage cuts, and contraction of health and education spending as direct program measures. They have no seat in program design; their governments negotiate under duress and ratify after the fact. Individual exit means emigration; collective refusal has historically produced unrest that programs treat as a security variable rather than a voice.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, debtor_state_populations, payer,
    powerless, immediate, trapped, national).

% Trade-liberalization conditions open program-country markets to their goods and place state enterprises on the auction block where they are positioned to acquire. They need not participate in, fund, or answer for the lending arrangement to collect from its operation.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, creditor_country_export_industries, beneficiary,
    powerful, biographical, mobile, global).

% Debt-relief movements, labor unions, and non-governmental organizations document social impacts and demand seats in program design. They are consulted after terms are set, if at all, and hold no formal voice in Executive Board decisions. Their leverage runs through publicity and creditor-country politics rather than the decision procedure itself.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, debtor_civil_society_advocates, excluded,
    moderate, biographical, constrained, regional).

% Study program outcomes across countries and decades, compare waiver rates against economic fundamentals, and produce the evaluation literature that every other seat cites or contests. They bear none of the arrangement's costs and collect none of its proceeds.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, development_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_financial_institutions).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__hybrid_selectivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real creditor collective-action problem: a single negotiating table with a common framework prevents each creditor from racing to seize assets or free-riding on others' concessions; provides crisis liquidity on terms private markets will not offer mid-crisis; and supplies a policy-reform signal that reopens private capital flows. The coordination function is genuine and is exercised in every major debt crisis of the interval.
% TRANSFER_FUNCTION: Moves policy control — fiscal, monetary, trade, and ownership decisions — from debtor states to the creditor-side institutions; moves debt-service capacity and austerity costs from debtor states and their populations to creditors; and moves market access, geopolitical loyalty, and opened markets to the hegemon bloc and its aligned debtors.
% ABSENT_VOICES: Debtor populations and the finance ministries of peripheral program countries are absent from design: quota-weighted boards give creditor economies controlling votes, and program documents reach debtor parliaments and civil society after terms are fixed. Present at the table, they would object to the selectivity itself — that identical policy failures receive waivers in aligned states and suspension in peripheral ones — and to the distribution of adjustment costs onto consumption and public services.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, every distressed sovereign would renegotiate bilaterally or through rival arrangements — bondholder councils, contractual collective-action clauses, or the large non-Western bilateral lenders — and the geography of crisis lending would reorganize around whoever supplies the coordination function instead. Creditor losses in the next default would land differently, and aligned states would lose the subsidy channel that alignment currently buys.
% FOUNDING_PROBLEM: The sovereign debt overhang of the late 1970s and early 1980s: recycled petrodollar lending left developing states with obligations they could not service, and disorderly sequential default threatened the solvency of the major commercial banks. A coordination mechanism was needed to reschedule, stabilize, and restore creditworthiness.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and the institution's own Independent Evaluation Office attest that the original coordination problem was real and that the framework solved it; that corroboration comes from outside the benefiting parties. Debtor-country finance ministries, UNCTAD analyses, and the debt-relief movement literature attest that the arrangement has persisted well past solvency restoration and that its current application tracks geopolitical alignment rather than the founding problem — so the status is genuinely disputed across seats, not self-asserted by either side.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.70: the burden of adjustment falls on populations with no design seat, program measures open debtor markets to creditor-country firms, and debt service is prioritized over social spending — but the framework also delivers crisis liquidity and debt reprofiling that debtors voluntarily seek, which caps ε below the pure-instrument level. Suppression is 0.70: enforcement runs through suspension of disbursements, cross-default triggers, loss of market access, and follow-on donor cutoffs — machinery that binds only the trapped. Theater_ratio is 0.42 and rising over the interval: the rebranding from structural adjustment to poverty-reduction-and-growth, the country-ownership rhetoric, and results-based frameworks have layered performative participation onto an unchanged decision structure. Accessibility_collapse is 0.45: alternatives exist for creditworthy or resource-backed states (bond markets, non-Western bilateral lenders, Gulf creditors) but collapse for the weakest — which is exactly where enforcement is harshest. Resistance is 0.62: subsidy riots, default threats, and the debt-relief movement are real, but debtor coalitions have repeatedly failed for the same collective-action reasons that make the creditor coordination function valuable — the arrangement in part exploits a coordination deficit on the debtor side, and the coalition-power possibility for powerless seats remains structurally unrealized. The three metric series share one time grid; every tracked metric is authored at every examined point. The trajectories are not cyclical but crisis-stepped: extraction and enforcement ratchet up through the 1980s-90s debt crises, dip slightly in the early 2000s as relief initiatives and strategic waivers (post-2001 front-line financing) soften headline terms while non-strategic enforcement holds, then plateau — the selectivity this reading names becomes more visible as headline severity falls.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the structural data is built to make them do so. From the agenda-setter seats the arrangement is a functioning coordination mechanism they operate under mandate; from the trapped peripheral debtor and its population the same machinery is enforced discipline with no comparable exit. The sharpest divergence is between two seats of formally identical status — sovereign program borrowers: the aligned strategic debtor experiences subsidized liquidity and waived benchmarks while the peripheral debtor experiences the full schedule, and the only authored variable separating them is geopolitical position. Within a single debtor state, the government seat and the population seat diverge on time horizon and exit: the ministry negotiates for market access over a biographical horizon, the population absorbs price and service changes on an immediate one with no exit at all. The engine computes these per-seat classifications from power, exit, and declared position; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real receipt: g7 shareholder governments collect opened markets and geopolitical compliance while controlling the waiver decisions; core financial creditors collect serviced claims; export industries collect liberalized markets; strategic debtors collect subsidized liquidity. Victim declarations map to the trapped seats: peripheral debtors and their populations bear the full schedule with no exit. The derivation chain handles most seats, with one override: hegemon_aligned_strategic_debtors are declared beneficiaries, so the structural derivation would read them as near-full beneficiaries (d around 0.1), but they carry real debt-service burdens, implement partial reforms, and pay the political cost of program association — an override to d = 0.32 on the organized power atom (which no other seat holds, so the override is unambiguous) encodes that dual position. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents both mislabels. Reading the arrangement as pure coordination (the creditor reading) erases the victims — the waiver record shows enforcement is not uniform and the burden is not symmetric. Reading it as pure instrument (the debtor reading) erases the genuine crisis function — liquidity and creditor coordination that even the harshest critics of specific programs rely on in the next crisis. The founding problem (1980s debt-overhang coordination) is contested rather than dead: debt crises recur and the coordination demand with them, so the arrangement has not atrophied into performance — its function is actively exercised every crisis — but its application has drifted from problem-solving to position-indexed discipline, which is drift the theater_ratio series partially registers. Mandatrophy is not resolved and no sunset applies: this is a steady-state hybrid, not a transitional support. The R5 mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges) and should find no dead-mandate zombie flag — the flag risk in this family runs the other way, toward the creditor reading's claim of a permanently live founding problem laundering selective enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the hybrid_selectivity_reading of the structural_adjustment_conditionalities kernel; how would the sibling readings re-author the same arrangement, and does the disagreement reduce to ε alone or to genuinely different constraints?',
    'Compare the three sibling stories'' structural data: the creditor_coordination_reading authors low ε with no victims; the debtor_extraction_reading authors near-maximal ε with hegemon states as the concentrated beneficiary and all debtor populations as victims; this reading authors intermediate-high ε with victimhood indexed to geopolitical position. If the sibling victim sets differ as declared, the readings are different constraints sharing one kernel; if they reduce to ε disagreement over one victim set, it is one constraint with observer dispute.',
    'Different victim sets mean the kernel is a family of constraints and per-seat classifications diverge structurally, not just numerically; a single shared victim set would collapse the family into one constraint with a contested ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one kernel, three readings; this file is the hybrid selectivity instantiation and the siblings would change both ε and the victim set.').

omega_variable(
    selectivity_deliberateness,
    'Is differential enforcement driven by deliberate geopolitical condition-setting by the creditor bloc, or by market-confidence dynamics that merely correlate with alignment?',
    'Program-level panel analysis comparing waiver rates, benchmark density, financing terms, and suspension frequency between fundamentals-matched aligned and non-aligned debtors, controlling for debt ratios, growth, and shock exposure.',
    'Deliberate selectivity makes the asymmetric application structural rather than incidental, pushing the peripheral-debtor seats toward the pure-extraction end and weakening the coordination defense; incidental correlation supports the coordination framing and lowers effective ε for the agenda-setter seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_deliberateness, empirical, 'Whether the selectivity is designed policy or an emergent market artifact.').

omega_variable(
    counterfactual_coordination_necessity,
    'Would sovereign debt crises resolve at acceptable cost without the hegemon-run coordinator — via contractual collective-action clauses, bondholder councils, or rival bilateral lenders?',
    'Compare resolution speed, haircuts, and post-restructuring market reaccess across episodes resolved outside the framework (contractual CAC restructurings, non-Western bilateral workouts) against framework-mediated episodes of comparable size.',
    'If outside resolution is comparable, the coordination function does not justify the arrangement''s inherent cost floor and the constraint drifts toward pure extraction for the trapped seats; if outside resolution is disorderly and slower, the coordination claim strengthens and the tangled_rope reading is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_coordination_necessity, conceptual, 'Whether the coordination function is genuinely supplied only by this arrangement.').

omega_variable(
    victim_boundary_indexing,
    'Is victimhood a property of geopolitical position (as this reading declares) or of debtor-program status as such (as the debtor_extraction_reading would have it)?',
    'Within-country welfare analysis comparing social outcomes across program countries stratified by alignment, controlling for program size, financing volume, and external shocks.',
    'If strategic-debtor populations also bear substantial adjustment burdens, the victim set widens, the selectivity premise weakens, and this reading collapses toward the debtor reading; if only peripheral populations bear the full schedule, the hybrid structure is confirmed and the victim declarations stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_boundary_indexing, empirical, 'Where the victim boundary sits: geopolitical position or debtor status.').

omega_variable(
    persistence_basis,
    'Does the arrangement persist because it supplies creditor coordination, or because it maintains hegemon financial influence that would require another instrument otherwise?',
    'Observe hegemon behavior when coordination could be supplied elsewhere: if influence is bought bilaterally (swap lines, parallel financing, ad hoc coalitions) whenever bilateral channels are cheaper, the framework functions as the influence instrument; if the hegemon routes even convenient cases through the framework, coordination is the operative function.',
    'Influence-instrument persistence pushes the trapped seats'' computed classifications toward pure extraction and makes the g7 seat the effective capturer; coordination persistence supports the tangled_rope claim and keeps ε anchored at the hybrid level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_basis, empirical, 'What actually maintains the arrangement across the interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sac_hybrid_selectivity_tr_t1980, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(sac_hybrid_selectivity_tr_t1985, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(sac_hybrid_selectivity_tr_t1990, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(sac_hybrid_selectivity_tr_t1996, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 1996, 0.35).
narrative_ontology:measurement(sac_hybrid_selectivity_tr_t2002, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2002, 0.42).
narrative_ontology:measurement(sac_hybrid_selectivity_tr_t2008, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2008, 0.45).
narrative_ontology:measurement(sac_hybrid_selectivity_tr_t2015, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2015, 0.44).
narrative_ontology:measurement(sac_hybrid_selectivity_tr_t2025, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(sac_hybrid_selectivity_be_t1980, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(sac_hybrid_selectivity_be_t1985, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 1985, 0.66).
narrative_ontology:measurement(sac_hybrid_selectivity_be_t1990, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 1990, 0.72).
narrative_ontology:measurement(sac_hybrid_selectivity_be_t1996, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 1996, 0.74).
narrative_ontology:measurement(sac_hybrid_selectivity_be_t2002, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2002, 0.7).
narrative_ontology:measurement(sac_hybrid_selectivity_be_t2008, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2008, 0.71).
narrative_ontology:measurement(sac_hybrid_selectivity_be_t2015, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2015, 0.69).
narrative_ontology:measurement(sac_hybrid_selectivity_be_t2025, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 2025, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(sac_hybrid_selectivity_su_t1980, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(sac_hybrid_selectivity_su_t1985, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 1985, 0.68).
narrative_ontology:measurement(sac_hybrid_selectivity_su_t1990, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 1990, 0.74).
narrative_ontology:measurement(sac_hybrid_selectivity_su_t1996, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 1996, 0.76).
narrative_ontology:measurement(sac_hybrid_selectivity_su_t2002, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2002, 0.74).
narrative_ontology:measurement(sac_hybrid_selectivity_su_t2008, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2008, 0.72).
narrative_ontology:measurement(sac_hybrid_selectivity_su_t2015, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(sac_hybrid_selectivity_su_t2025, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, hipc_debt_relief_regime).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'IMF conditionality' covers three structurally distinct claims and is decomposed per the ε-invariance principle into three stories sharing the kernel structural_adjustment_conditionalities. The creditor_coordination_reading is the upstream, established claim (official framing, cited as justification for the arrangement); the debtor_extraction_reading is the downstream oppositional claim; this hybrid_selectivity_reading sits between and its waiver documentation is the primary evidentiary resource the downstream extraction reading draws on, while partially affirming the upstream coordination claim. Each story carries its own ε, beneficiaries, and victims; the edges above carry contamination and influence analysis across the family, and hipc_debt_relief_regime is included because the relief regime was created under pressure from this arrangement's extraction profile and now modulates its enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
