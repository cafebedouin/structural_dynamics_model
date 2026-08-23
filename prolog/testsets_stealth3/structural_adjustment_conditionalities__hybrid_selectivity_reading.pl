% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Structural Adjustment Conditionalities (Hybrid Selectivity Reading)
 *   domain: economic/political/international
 *
 * SUMMARY:
 *   International sovereign-debt conditionality, the practice of lending
 *   against negotiated policy commitments (austerity targets, subsidy
 *   removal, privatization, debt-service priority) enforced through tranche
 *   disbursement and program suspension, presents itself as uniform,
 *   technical discipline. This story instantiates the
 *   hybrid_selectivity_reading of that kernel: the discipline is real, but
 *   its application is indexed to geopolitical position. Weak, non-aligned
 *   debtors face the full conditionality sequence enforced through market
 *   lockout; strategically aligned debtors receive waived milestones,
 *   continued flows, and tolerated off-program spending (Cold War-era Zaire
 *   and Egypt, post-9/11 Pakistan, contemporary geostrategic borrowers). The
 *   arrangement is therefore a hybrid: a genuine coordination function
 *   operating through the same structure that extracts asymmetrically from
 *   the geopolitically peripheral. Per the epsilon-invariance decomposition
 *   rule, the colloquial label 'conditionality' is split into three linked
 *   stories over one kernel: the creditor_coordination reading authors low
 *   epsilon over this referent (coordination cost only), the
 *   debtor_extraction reading authors very high epsilon (pure rent,
 *   coordination as cover), and this reading authors intermediate-high
 *   epsilon (0.68) because it holds both components real. The referent of
 *   epsilon here is the standing conditional-lending arrangement as this
 *   reading sees it, never the reformed regime this reading would prefer. The
 *   sibling readings are separate constraint files linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - ifi_bureaucracy: Agenda-setter and enforcer (institutional / identity_locked) — designs programs and administers waivers; careers and professional identity are built inside the adjustment paradigm
 *   - hegemon_state: Agenda-setter and selectivity-rent collector (institutional / arbitrage) — quota veto plus security apparatus direct enforcement toward rivals' clients and forbearance toward allies
 *   - core_creditor_institutions: Primary beneficiary (institutional / arbitrage) — claims serviced ahead of domestic spending; can reprice or exit exposures at will
 *   - hegemon_aligned_debtors: Secondary beneficiary (moderate / constrained) — borrow under the same programs with softened or waived conditions
 *   - geopolitically_peripheral_debtors: Primary target (powerless / trapped) — full conditionality enforced by tranche suspension and market lockout
 *   - debtor_state_populations: Primary target (powerless / constrained) — bear the austerity transfers directly; exit mainly by emigration
 *   - debtor_civil_society: Excluded voice (organized / trapped) — outside program negotiation by design; objection surfaces as street protest after terms are set
 *   - heterodox_economists: Analytical observer (analytical / analytical) — document waiver patterns and alignment-exposure correlations from outside the benefiting set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.68).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.7).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Structural Adjustment Conditionalities (Hybrid Selectivity Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "economic/political/international").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, '8a1266ef-2d8c-4f64-ae33-34573970f9d4').
narrative_ontology:cs_kernel_codification('8a1266ef-2d8c-4f64-ae33-34573970f9d4', formalized).
narrative_ontology:cs_authority_grounding('8a1266ef-2d8c-4f64-ae33-34573970f9d4', extraction).
narrative_ontology:cs_interpretation_layer_present('8a1266ef-2d8c-4f64-ae33-34573970f9d4').
narrative_ontology:cs_reading_relation('8a1266ef-2d8c-4f64-ae33-34573970f9d4', structural_adjustment_conditionalities__creditor_coordination_reading, influences).
narrative_ontology:cs_reading_relation('8a1266ef-2d8c-4f64-ae33-34573970f9d4', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('8a1266ef-2d8c-4f64-ae33-34573970f9d4', foundational, enforcement_tracks_geopolitical_alignment).
narrative_ontology:cs_axiom_status(enforcement_tracks_geopolitical_alignment, holdable).
narrative_ontology:cs_axiom_grounding('8a1266ef-2d8c-4f64-ae33-34573970f9d4', enforcement_tracks_geopolitical_alignment, empirically_contingent).
narrative_ontology:cs_axiom('8a1266ef-2d8c-4f64-ae33-34573970f9d4', foundational, coordination_and_extraction_co_occur).
narrative_ontology:cs_axiom_status(coordination_and_extraction_co_occur, holdable).
narrative_ontology:cs_axiom_grounding('8a1266ef-2d8c-4f64-ae33-34573970f9d4', coordination_and_extraction_co_occur, empirically_contingent).
narrative_ontology:cs_reference_frame('8a1266ef-2d8c-4f64-ae33-34573970f9d4', geopolitically_indexed_enforcement).
narrative_ontology:cs_drift_state('8a1266ef-2d8c-4f64-ae33-34573970f9d4', contemporary_multipolar_lending_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8a1266ef-2d8c-4f64-ae33-34573970f9d4', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_state).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_debtors).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, ifi_bureaucracy).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_peripheral_debtors).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, debtor_state_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs program conditionality, conducts reviews, and administers waiver decisions for sovereign lending programs. Staff careers, promotion, and professional standing are built inside the adjustment paradigm; economists who dissent from the framework leave or are marginalized rather than revising it. Exit would mean repudiating the expertise that constitutes their professional identity.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, ifi_bureaucracy, agenda_setter,
    institutional, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, ifi_bureaucracy, beneficiary).

% Holds the largest quota share and an effective veto over major lending decisions, and directs enforcement attention through its treasury and security apparatus. It benefits on two ledgers: creditor claims are protected through program priority, and alignment rents accrue as strategic debtors receive waivers and continued flows while rivals' clients face full discipline. It writes and rewrites the rules, so exit is meaningless; it can reprice its participation at will.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_state, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_state, agenda_setter).

% Commercial banks, bondholder committees, and Paris Club creditor governments whose claims are serviced ahead of domestic spending under program conditionality. They can reprice, hedge, or exit exposures at will and bear no domestic adjustment costs. Their coordination problem, that no single creditor can enforce adjustment on a sovereign alone, is what the arrangement solves for them.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions, beneficiary,
    institutional, biographical, arbitrage, global).

% Strategically positioned debtor states that borrow under the same programs but receive softened or waived conditions when their alignment is valued: continued flows, deferred milestones, tolerated off-program spending. They still depend on official finance and cannot walk away from it, but the discipline they face is calibrated to their geopolitical price.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_debtors, beneficiary,
    moderate, generational, constrained, national).

% Weak, non-aligned debtor states that face the full conditionality sequence: subsidy removal, public wage freezes, privatization, and debt-service priority, enforced through tranche suspension and market lockout. Default means years of exclusion from capital markets; no alternative lender of comparable scale exists for them, so program terms are effectively take-it-or-leave-it.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_peripheral_debtors, payer,
    powerless, generational, trapped, national).

% Citizens who bear the transfers directly: higher food and fuel prices after subsidy removal, frozen public wages, thinner health and education budgets, and privatized services. Their consent is never sought; their main lever is unrest, which programs treat as an implementation risk. Skilled workers can emigrate; the rest absorb the adjustment in place.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, debtor_state_populations, payer,
    powerless, biographical, constrained, national).

% Unions, churches, and NGOs in debtor states who would contest program terms if admitted to the negotiation. They are outside the room by design: programs are concluded between a finance ministry negotiating under duress and the lending institutions. Their objection surfaces as street protest, the structural adjustment riots, after terms are already set.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, debtor_civil_society, excluded,
    organized, biographical, trapped, national).

% Researchers inside and outside the institutions who document waiver patterns, program outcomes, and the correlation between alignment and enforcement intensity. They bear none of the costs and collect none of the rents; their analyses circulate in academic and policy venues and occasionally reach evaluation offices but not program terms.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, heterodox_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__hybrid_selectivity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem in sovereign lending that no single creditor can solve alone: individual creditors cannot enforce fiscal adjustment on a sovereign debtor, cannot coordinate forbearance among themselves, and cannot restore the market confidence that keeps refinancing channels open. A senior, conditional official lender internalizes the enforcement problem, coordinates the creditors, and supplies the fresh money that makes adjustment politically survivable for the debtor government.
% TRANSFER_FUNCTION: Moves three things. Fiscal resources: subsidy removal, wage freezes, and privatization proceeds transfer purchasing power from debtor-state populations and budgets to creditors and asset purchasers, with debt service prioritized over domestic spending. Policy control: program conditions transfer decision rights over budgets, tariffs, and state assets from debtor polities to lending-institution review. Enforcement allocation: the discretion to waive or intensify discipline is allocated by the hegemon's geopolitical priorities, so discipline flows toward the unaligned and forbearance toward the aligned.
% ABSENT_VOICES: Debtor-state populations bear the transfers but are absent: programs are negotiated between a finance ministry negotiating under duress and the institutions, with no popular or parliamentary consent requirement. Debtor civil society is outside the room by design. Alternative lenders outside the creditor cartel, and the debtor states they would finance, are consultative at best. Heterodox economists reach evaluation offices but not program terms.
% DISAPPEARANCE_RATIONALE: If conditional lending and its enforcement machinery vanished overnight, the creditor-coordination function would collapse: banks and bondholder committees would face sovereign defaults without a senior disciplinarian, market lockout would lose its institutional amplifier, and debt crises would resolve through disorderly default and ad hoc creditor committees. The geopolitical allocation of discipline would not vanish with it; it would migrate to whatever lender supplies the coordination function next, which is this reading's central structural claim rather than an artifact of the label.
% FOUNDING_PROBLEM: The sovereign debt overhang of the late 1970s and 1980s: petrodollar-recycled commercial loans to developing states became unserviceable after the Volcker rate shock; individual creditors could not coordinate forbearance or adjustment, debtor defaults threatened major banks' solvency, and development finance flows were at risk. Conditionality was built to solve that coordination problem.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself is corroborated from outside the benefiting set: debtor-state central bank records, contemporaneous financial press, and economic history document the 1980s crisis and the genuine creditor-coordination failure. That the founding problem remains the operative justification for today's apparatus is attested mainly by the benefiting parties, IFI management and creditor governments. Corroboration for the shifted-function reading also comes from outside the set: the IMF's own Independent Evaluation Office has documented conditionality overreach and design failures, and comparative program studies by academic and UN-affiliated researchers document the alignment-exposure pattern this reading names.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.68 because this reading holds both components real: programs do coordinate lending and restore market access for some participants, while the adjustment burden concentrates on debtors whose geopolitical price is low. Suppression (0.70) is authored as a raw structural property, the enforcement machinery being tranche suspension, cross-default clauses, rating-agency amplification, and market lockout; it is the engine, not this story, that scales extractiveness by directionality and scope. Theater (0.45) tracks the widening gap between the uniform-technical narrative and the documented selective practice: the technical analysis inside program design is partly real, but a substantial share of the regime's public justification, uniform discipline by an apolitical technocracy, is performative cover for alignment allocation. Accessibility collapse is moderate (0.55): default, alternative official lenders, and regional arrangements partially exist but collapse into lockout for most peripheral debtors once cross-default and rating effects are understood. Resistance (0.60) is substantial: structural-adjustment riots, debt-relief movements, heterodox economics, and selective defaults. The temporal series run on one shared nine-point grid, every tracked metric authored at every point. The base_extractiveness series is wave-shaped rather than monotonic: peaks track debt-crisis waves when creditor leverage is highest, dips track debt-relief episodes and commodity booms when debtor alternatives briefly improve; the cycle is driven by external factors (global rates, commodity prices), not by intermittent reinforcement. The suppression_requirement series is authored because this story specifically traces enforcement-capacity change: enforcement infrastructure matured and hardened through the 1990s (cross-conditionality, HIPC-era discipline, ratings amplification), then plateaued as the machinery's allocation rather than its intensity became the political variable, dipped during the pandemic's emergency facilities, and re-tightened under the Common Framework. The dominant coordination function is enforcement: conditionality is a governance structure whose failure, unenforceable conditions, would immediately dissolve the creditor coordination it exists to provide, so boltzmann.coordination_type is enforcement_mechanism with the type default floor.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from the same structure. From the core creditor seat the arrangement is a rope: it solves a collective-action problem no single creditor can solve alone, and the creditor can exit at will. From the hegemon-aligned debtor seat it is also rope-flavored: subsidized forbearance calibrated to alignment. From the geopolitically peripheral debtor seat the same structure computes as near-snare: full enforcement, trapped exit, no coalition lever. From the IFI staff seat it is a professional vocation whose identity lock makes the paradigm constitutive rather than chosen. The engine computes these per-seat classifications from the structural data; this story's claimed_type adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: core creditors and the hegemon state sit nearest the beneficiary end because the arrangement subsidizes them through debt-service priority and alignment rents; hegemon-aligned debtors sit low-to-mid because they pay interest but receive waived discipline, a net subsidy relative to peripheral peers; IFI staff derive mid-low directionality through institutional rents, with identity lock binding them to the arrangement's continuation. Victim declarations drive high directionality: peripheral debtor states sit near the full-target end, amplified by trapped exit; their populations sit near it with constrained exit, emigration being partial arbitrage. Global spatial scope amplifies effective extraction for the target seats because verification and exit are hardest at that scale. The payer seats' coalition lever, collective default or a debtors' cartel, is historically fragile: sequencing and defection pressure dissolved every attempt, which is what keeps their effective position near the target end; the debtor_coalition_viability omega holds the question open.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, coordinating sovereign-debt resolution when individual creditors cannot enforce adjustment, is live: debt crises recur. But the disciplinary form has drifted: waiver discretion now allocates enforcement by alignment rather than by fiscal logic, which is the drift the temporal series tracks. The tangled_rope classification prevents two mislabels. Reading the arrangement as pure rope (the creditor_coordination sibling) erases the documented asymmetry: fiscally similar positions receive different discipline. Reading it as pure snare (the debtor_extraction sibling) erases the real coordination function: unconditioned official lending has documented failure modes of its own. The receipt surface sharpens the mandatrophy question: gain_flow names core_creditor_institutions because debt-service priority demonstrably accrues there, while the hegemon collects a non-monetary selectivity premium; and fixing_cost is authored as cheap because the waiver mechanism itself demonstrates that switching the allocation is administratively inexpensive for the agenda-setter, so persistence is interest-driven, not cost-driven. That combination rules out the piton reading: the administrator could change the allocation at low cost and does not, because the selectivity is the payoff.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the structural_adjustment_conditionalities kernel: which epsilon and victim set would the sibling readings (creditor_coordination_reading, debtor_extraction_reading) instantiate, and is the disagreement located in application uniformity, in the reality of the coordination function, or in the identity of the beneficiaries?',
    'Cross-reading comparison: compile all three sibling stories and compare authored epsilon, victim sets, and per-seat classifications over the same historical record; the disagreement localizes where the readings'' structural declarations diverge (uniformity of application, genuineness of coordination, identity of beneficiaries).',
    'If the creditor_coordination reading is right, this story''s epsilon is overstated and the arrangement is a rope with coordination costs; if the debtor_extraction reading is right, the coordination component is cover and this story understates epsilon; the hybrid reading stands only if application is demonstrably position-indexed while the coordination function remains real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexicality of epsilon over the shared conditional-lending kernel.').

omega_variable(
    selectivity_empirical_signature,
    'Is enforcement intensity systematically predicted by geopolitical alignment rather than by fiscal indicators alone?',
    'Comparative analysis of program documents and waiver records: regress conditionality density, waiver frequency, and program interruption on alignment measures (UN voting affinity, basing agreements, alliance ties) controlling for debt ratios and adjustment need.',
    'A significant alignment coefficient confirms this reading''s core axiom and supports tangled_rope classification with position-indexed victims; a null result would shift weight toward the creditor_coordination reading and reclassify toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_empirical_signature, empirical, 'Whether the alignment-exposure correlation in enforcement is measurable and robust.').

omega_variable(
    alternative_lender_exit_drift,
    'Does alternative official credit give peripheral debtors real exit, or does it carry its own conditions (collateral, geopolitical alignment) that reproduce the same structure under a different flag?',
    'Track borrowing terms, collateral clauses, and renegotiation outcomes for states that shifted borrowing away from the IFI system; compare the enforcement intensity of the alternative lenders'' own conditions.',
    'If exit is real, suppression falls and the constraint drifts toward rope as the creditor cartel loses pricing power; if the alternative reproduces conditionality in another guise, the trapped-exit structure persists and the victim set stays stable while the enforcing hegemon''s identity changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_lender_exit_drift, empirical, 'Whether alternative lending dissolves or relocates the exit trap.').

omega_variable(
    debtor_coalition_viability,
    'Could peripheral debtor states coordinate collective default or a debtors'' cartel to change the enforcement structure, and what suppresses the attempt?',
    'Historical analysis of collective-action episodes (debtors'' club proposals, the debt round of the 1970s-80s New International Economic Order, contemporary Common Framework negotiations): identify the sequencing and defection pressures that dissolved coordination.',
    'A viable debtor coalition would raise payer power, lower effective extraction, and pressure the arrangement toward renegotiated terms; demonstrated inviability confirms the trapped-exit structure that sustains high effective extraction on the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debtor_coalition_viability, empirical, 'Whether payer-side coalition power is structurally available.').

omega_variable(
    counterfactual_discipline_baseline,
    'What would unconditioned or uniformly-conditioned lending have produced for peripheral debtors: is measured extraction assessed against a genuine coordination counterfactual or an idealized one?',
    'Compare outcomes across regimes: states that defaulted and stayed out, states under full programs, and states with alternative finance, holding initial conditions constant where possible.',
    'If unconditioned access produced comparable or worse outcomes, part of measured epsilon is coordination cost and the rope component is larger; if program states systematically underperformed matched non-program states, the extraction component dominates and the classification hardens toward snare for the payer seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_discipline_baseline, conceptual, 'Counterfactual dependence of the extraction/coordination split.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(stru_tr_t6, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(stru_tr_t12, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(stru_tr_t18, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(stru_tr_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(stru_tr_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(stru_tr_t36, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 36, 0.45).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(stru_tr_t44, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 44, 0.45).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(stru_be_t6, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(stru_be_t12, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(stru_be_t18, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(stru_be_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(stru_be_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(stru_be_t36, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 36, 0.66).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(stru_be_t44, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 44, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(stru_su_t6, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(stru_su_t12, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(stru_su_t18, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(stru_su_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(stru_su_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(stru_su_t36, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 36, 0.72).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(stru_su_t44, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 44, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__debtor_extraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'IMF/World Bank conditionality' covers three structurally distinct claims over one kernel (structural_adjustment_conditionalities). Epsilon differs by reading over the same referent, the standing conditional-lending arrangement: creditor_coordination authors low epsilon (uniform technical discipline, coordination cost only), debtor_extraction authors very high epsilon (pure rent, coordination as cover), and this hybrid reading authors intermediate-high epsilon (0.68, genuine coordination plus position-indexed extraction). The upstream creditor_coordination story is the most established and is cited by the regime itself as justification; this reading's waiver documentation feeds the debtor_extraction story's evidentiary base. All three files link one another through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
