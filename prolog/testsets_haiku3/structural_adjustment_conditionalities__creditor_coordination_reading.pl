% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__creditor_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__creditor_coordination_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: structural_adjustment_conditionalities__creditor_coordination_reading
 *   human_readable: Structural Adjustment Conditionalities (Creditor Coordination Reading)
 *   domain: economic/political
 *
 * SUMMARY:
 *   Structural adjustment conditionalities are the IMF's and World Bank's
 *   central tool for enforcing fiscal discipline on borrowing states. Under
 *   the creditor coordination reading, conditionalities solve a real
 *   coordination problem: multiple creditors need assurance that the
 *   borrowing state prioritizes debt service; capital markets need rules
 *   about the borrower's conduct or they flee; governments need external
 *   cover to enforce unpopular reforms. The victims under this reading are
 *   inefficient state sectors and subsidy-dependent populations whose exit
 *   from the budget is the condition for market confidence and debt
 *   sustainability. This reading treats conditionalities as a Rope
 *   constraint: genuine coordination function, beneficiaries (future
 *   taxpayers protected from default; capital markets reassured), and
 *   beneficiaries also include creditor governments whose citizens' pension
 *   funds depend on continued debt service. Crucially: this is ONE reading of
 *   the contested kernel 'structural adjustment conditionalities.' The
 *   sibling reading (debtor_extraction_reading) treats the same
 *   conditionalities as pure extraction using creditor power to remake debtor
 *   states into profit centers. This story author instantiates ONLY the
 *   creditor coordination reading; sibling stories are separate constraint
 *   files.
 *
 * KEY AGENTS:
 *   - IMF/World Bank: agenda-setters; control program design and enforcement
 *   - Creditor governments: beneficiaries; back the institutional mandate
 *   - International financial markets: beneficiaries; demand conditionality signals
 *   - Borrowing state leadership: payers; accept conditionality to retain market access
 *   - Public sector workers and subsidy-dependent populations: victims; concentrated adjustment costs
 *   - Future taxpayers: beneficiary-proxy; supposedly protected from default
 *   - Alternative creditors: excluded; would compete on policy terms if admitted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.48).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.62).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "Structural Adjustment Conditionalities (Creditor Coordination Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "economic/political").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__creditor_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, 'f1c17373-c13c-400b-b750-acb3407223fb').
narrative_ontology:cs_kernel_codification('f1c17373-c13c-400b-b750-acb3407223fb', fixed_text).
narrative_ontology:cs_authority_grounding('f1c17373-c13c-400b-b750-acb3407223fb', extraction).
narrative_ontology:cs_interpretation_layer_present('f1c17373-c13c-400b-b750-acb3407223fb').
narrative_ontology:cs_reading_relation('f1c17373-c13c-400b-b750-acb3407223fb', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1c17373-c13c-400b-b750-acb3407223fb', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('f1c17373-c13c-400b-b750-acb3407223fb', foundational, fiscal_discipline_ensures_sustainability).
narrative_ontology:cs_axiom_status(fiscal_discipline_ensures_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('f1c17373-c13c-400b-b750-acb3407223fb', fiscal_discipline_ensures_sustainability, empirically_contingent).
narrative_ontology:cs_axiom('f1c17373-c13c-400b-b750-acb3407223fb', foundational, capital_markets_discipline_states_through_exit_threat).
narrative_ontology:cs_axiom_status(capital_markets_discipline_states_through_exit_threat, holdable).
narrative_ontology:cs_axiom_grounding('f1c17373-c13c-400b-b750-acb3407223fb', capital_markets_discipline_states_through_exit_threat, empirically_contingent).
narrative_ontology:cs_axiom('f1c17373-c13c-400b-b750-acb3407223fb', secondary, creditor_coordination_produces_optimal_debt_outcomes).
narrative_ontology:cs_axiom_status(creditor_coordination_produces_optimal_debt_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('f1c17373-c13c-400b-b750-acb3407223fb', creditor_coordination_produces_optimal_debt_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('f1c17373-c13c-400b-b750-acb3407223fb', creditor_coordination_framework).
narrative_ontology:cs_drift_state('f1c17373-c13c-400b-b750-acb3407223fb', contemporary_alternative_financing_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f1c17373-c13c-400b-b750-acb3407223fb', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, creditor_governments).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, state_employment_sectors).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, public_service_beneficiaries).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, subsidy_dependent_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, domestic_business_elite).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, borrowing_state_leadership).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, domestic_business_elite).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, market_discipline_produces_efficiency).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, fiscal_sustainability_requires_deficit_reduction).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, capital_flight_risk_constrains_policy_space).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce structural adjustment programs as conditions for lending. Frame conditionalities as technical requirements ensuring fiscal sustainability, debt servicing, and market confidence. Control the enforcement machinery: tranches, milestones, policy dialogue. Maintain this reading by publishing technical papers, defending before creditor governments, and rejecting alternative framings as politically motivated.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, imf_world_bank, agenda_setter,
    institutional, generational, arbitrage, global).

% Back the IMF/World Bank mandate through voting power and capital contributions. Benefit from restored debt servicing capacity of borrowing states, which protects their own citizens' pension funds and banking exposures. Frame conditionalities as responsible stewardship ensuring the borrowing state does not default and destabilize regional markets.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, creditor_governments, beneficiary,
    institutional, generational, arbitrage, global).

% Price-in conditionality regimes as reducing default risk and improving debt service reliability. Demand their continuation as a signal of fiscal discipline. Threaten capital flight if programs are abandoned. Benefit from restored state creditworthiness and resumed access to markets on terms favorable to creditors.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets, beneficiary,
    organized, biographical, mobile, global).

% Face the sovereign choice: accept conditionalities and retain market access and emergency lending, or reject them and face capital flight, currency collapse, and inability to service existing debt. Accept conditionality terms that dismantle the programs they were elected to sustain, in exchange for continuing to function as a state. Cannot exit without catastrophic economic consequences; can only negotiate the pace and modulation of reform.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, borrowing_state_leadership, payer,
    powerful, biographical, constrained, national).

% Public sector workers, administrators, educators, health workers. Conditionalities mandate headcount reductions, wage freezes, benefit cuts. They cannot strike without triggering capital flight (the market interprets labor militancy as evidence the state is backsliding). They cannot negotiate collectively without international officials labeling them a 'structural obstacle.' Exit means unemployment in economies without safety nets; staying means accepting declining compensation and job security.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, state_employment_sectors, payer,
    powerless, immediate, trapped, national).

% Rural and urban poor dependent on food, energy, transport subsidies maintained as social policy. Conditionalities mandate subsidy removal to 'eliminate fiscal distortions.' They cannot protest without being labeled anti-reform; cannot organize politically without destabilizing the program. Exit means migration to informal economy or neighboring countries. No seat at the table when the pace and modulation of subsidy removal is decided.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, subsidy_dependent_populations, payer,
    powerless, immediate, trapped, national).

% The unborn generation that will service the debt if today's borrowing state defaults. Conditionalities supposedly protect their interests by preventing debt explosion. They cannot negotiate; they are represented only in the technical papers that claim to act in their interest. This seat is a structural proxy for 'fiscal sustainability,' not a negotiating party.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers, beneficiary,
    analytical, generational, analytical, national).

% Often benefit from privatization programs and reduced state competition; can access credit more easily when markets are reassured. Also may be payers if they face higher taxation or if public services they depend on contract. Can exit through capital flight; their presence or absence signals confidence in the program's credibility.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, domestic_business_elite, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, domestic_business_elite, payer).

% Non-Western creditors (China, Gulf states, others) who offer financing without conditionalities or with different terms. They would compete on policy space and financing terms if admitted to the negotiation. Excluded by institutional rules (IMF primacy, Paris Club debt restructuring, donor coordination). Their exclusion is what the enforcement machinery exists to prevent.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, alternative_creditor_coalitions, excluded,
    powerful, biographical, trapped, global).

% Health, education, labor, environmental organizations representing affected populations. They would argue that the coordination problem can be solved differently (progressive taxation, exchange-rate adjustment, rescheduling rather than austerity). Excluded from technical policy dialogue; invited only to 'implementation' after the program is designed. Their exclusion is not accidental—the coordination reading requires treating them as sectional interests, not stakeholders.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, civil_society_organizations, excluded,
    moderate, biographical, constrained, national).

% Researchers and practitioners outside the IMF/World Bank orthodoxy who produce alternative analyses: questioning counterfactual assumptions, documenting distributional harm, proposing alternative remedies. They scrutinize whether the coordination problem is real, whether conditionalities solve it, and whether less extractive arrangements are possible. Their analytical standing is contested by the institutional seats.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, development_economists_critical, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__creditor_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Conditionalities coordinate three real coordination problems: (1) creditor synergy — multiple creditors need assurance that the borrowing state will prioritize debt service (preventing free-rider temptation to default on some and pay others); (2) capital flight prevention — markets need rules about the borrower's fiscal conduct or they will pull capital, triggering collapse; (3) domestic political commitment — international conditionality provides political cover for governments to enforce unpopular reforms against domestic constituencies that might otherwise block them. Under this reading, conditionalities solve genuine collective-action problems whose alternative is worse (cascade default, capital flight, political paralysis).
% TRANSFER_FUNCTION: Moves fiscal discipline and market confidence FROM the creditor system TO the borrowing state. Specifically: (1) debt service flow from the borrowing state to creditors is reliably maintained; (2) capital flight risk is reduced because markets see the state following agreed rules; (3) the borrowing state accepts permanent constraint on policy space — it forgoes discretionary spending on employment, subsidy, or social programs — as the price of continued market access. The transfer is the borrowing state's future policy autonomy, sold for present creditworthiness.
% ABSENT_VOICES: Populations bearing the concentrated costs of adjustment (public sector workers, subsidy-dependent poor) are structurally excluded from policy design. Alternative creditor coalitions are excluded from the negotiation. Development economists and civil society organizations critical of the program are admitted only for 'implementation consultation' after the core design is done. The coordination reading depends on treating these exclusions as technical necessities (they lack standing, they are sectional interests) rather than as contestable power moves.
% DISAPPEARANCE_RATIONALE: If structural adjustment conditionalities disappeared overnight, borrowing states would immediately face capital flight (markets would interpret the withdrawal as loss of fiscal discipline signaling); they would default unless they found alternative financing sources; creditor governments would lose leverage to enforce reforms they prefer; the IMF and World Bank would lose their core policy tool and raison d'être as enforcers. The system depends on conditionalities' persistence for its stability—they are not background facts but active coordination mechanisms whose absence would trigger reorganization.
% FOUNDING_PROBLEM: In the late 1970s–1980s, developing states had accumulated debt and faced balance-of-payments crises. Creditors had to coordinate on lending terms and borrowers had to demonstrate commitment to repay. Without some agreement mechanism, either creditors would panic (capital flight) or borrowers would default. The IMF emerged as the coordinator: assessing whether a state could sustain debt service, designing reforms to achieve that sustainability, and conditioning continued lending on compliance. The founding problem was real: multiple creditors needed assurance, borrowing states needed a shared understanding of what 'sustainable' meant.
% FOUNDING_PROBLEM_CORROBORATION: IMF and creditor-government economists attest the founding problem is live: debt crises still occur, market confidence is fragile, conditionality is the only enforcement mechanism that works. Development economists and debtor-state officials attest the founding problem is substantially solved (debt crises after 1990s are rarer; alternative financing sources exist; the problem now is that conditionality persists beyond its technical necessity, becoming pure policy control). Empirical studies documenting that post-adjustment growth is lower than predicted, and that alternative (non-conditionality) programs achieve similar outcomes, corroborate the contestation.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.48 (moderate-high) because under this reading, while a real coordination function exists (preventing capital flight, coordinating creditors), the transfer of policy space from the borrowing state to the creditor system is substantial and permanent. The beneficiaries (capital markets, creditor governments, future taxpayers) are secure; the victims (public sector workers, subsidy populations) bear concentrated, immediate costs. Theater ratio is moderate (0.28): the technical papers and policy dialogue are substantive (the coordination problem is real), but some portion of enforcement activity is performative (reassuring markets, signaling discipline to voters). Suppression is high (0.62) because the arrangement depends on creditor exclusivity (alternative financing is cut off if the state seeks it) and on state leadership's inability to organize effective political opposition (labor movements, civil society are weakened by the austerity itself). Accessibility collapse is high (0.71) because once a state enters the conditionality system, the alternatives (default, capital flight, seeking alternative financing) all carry catastrophic costs; the state is locked in. Resistance is very high (0.74) because public sector workers, subsidy populations, and civil society organizations mount real opposition—strikes, protests, alternative policy proposals—but this resistance is structural (costs of exit are too high) rather than successful (it shapes the margin but not the core agenda). The extractiveness series slowly rises from 0.38 to 0.52 over the interval because the conditionality system has drifted: initial programs were time-bound and focused on balance-of-payments stabilization; contemporary conditionalities are permanent fixtures focused on 'structural reform' (privatization, deregulation) that extract policy space indefinitely. Theater ratio and suppression_requirement both rise because enforcement must work harder as alternatives emerge (China's financing, regional coalitions, alternative development models).
 *
 * PERSPECTIVAL GAP:
 *   From the creditor-government and IMF seat, conditionalities are necessary technical tools ensuring fiscal sustainability and preventing default cascades—a genuine rope constraint solving a collective-action problem. From the state-leadership seat, they are an unavoidable constraint on policy space accepted to retain market access; the coordination function is real but so is the permanent subordination. From the public-sector-worker and subsidy-dependent-population seats, conditionalities are pure extraction: the coordination problem (if real at all) is between creditors and distant future taxpayers; the present populations are not parties to this coordination, merely bearing its costs. From the development-economist observer seat, the empirical case for conditionalities solving the founding problem is weak (post-adjustment growth lags alternatives) and the case for alternative arrangements (debt relief, alternative financing, democratic domestic reform) is stronger. The engine computes these divergences from the structural data; the coordinating function is real enough from certain seats that the constraint does not collapse into pure snare, but the asymmetry is substantial enough that contested-type designation is warranted.
 *
 * DIRECTIONALITY LOGIC:
 *   Directional values by stakeholder: (1) IMF/World Bank: d ≈ 0.0 (full beneficiary; they are the organizational platform collecting legitimacy and institutional power from conditionality); (2) Creditor governments: d ≈ 0.1 (beneficiary, though they bear some diplomatic costs and capital-flight risk); (3) International financial markets: d ≈ 0.15 (net beneficiary; lower risk and steady returns, but exposed to systemic default risk); (4) Borrowing state leadership: d ≈ 0.65 (payer, but not full target; they retain organizational power, can negotiate the margin, can exit at catastrophic cost); (5) Public sector workers: d ≈ 0.95 (near-full target; identity-locked to employment, trapped exit, suppression both structural and internalized); (6) Subsidy-dependent populations: d ≈ 0.95 (near-full target; trapped in geography and dependence, no political voice); (7) Future taxpayers: d ≈ 0.0 (beneficiary-proxy; they do not exist to negotiate, but the constraint is justified in their names); (8) Alternative creditors: d ≈ 0.5 (symmetric to the system but excluded from the negotiation, so effectively targets); (9) Civil society organizations: d ≈ 0.75 (targets; weakened by austerity, excluded from design, but still present in the story). The power atoms modulate these base directionalities: powerless agents get higher effective d (trapped exit amplifies extraction); institutional agents get lower d even when they are nominal payers (they can negotiate, lobby, exit through policy choice rather than desperation).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (balance-of-payments crises, creditor coordination) was real in the 1980s and early 1990s. The program (structural adjustment conditionalities) was designed to solve it by enforcing fiscal discipline and signaling market confidence. By the 2000s, the founding problem's status became contested: balance-of-payments crises became less frequent; borrowing-state economies stabilized; alternative financing emerged. Yet conditionalities persisted and even expanded into 'structural reform' (privatization, deregulation) that serves creditor capital interests rather than fiscal sustainability. This is the mandatrophy signature: the arrangement persists because the agenda-setters (IMF, creditor governments) benefit from it even after the original coordination problem is solved. The remedy—debt relief, conditional-policy flexibility, alternative financing—would require creditors to forgo the policy control they now exercise, which they resist. The arrangement is not pure inertia (it is actively maintained) but it has shifted from coordination to extraction as the founding problem receded. Declaring this constraint as Rope (not Tangled Rope) is structurally defensible if you accept the creditor-coordination reading's claim that the program's core function (ensuring fiscal sustainability) remains live and necessary. Declaring it as Tangled Rope or Snare is defensible if you emphasize that the program now primarily serves creditor capital interests (extraction) while the coordination function has become secondary and contestable. Under the creditor reading, Rope is the appropriate claim; the measuring of base_extractiveness at 0.48 (moderate-to-high) reflects that even the creditor reading acknowledges substantial transfer of policy space, not just coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (balance-of-payments crisis, creditor coordination failure, capital flight risk) still live, or has it been substantially solved since the 1980s, with conditionality now serving primarily to lock policy space for creditor interests?',
    'Comparative analysis of (a) post-conditionality crisis frequency and severity, (b) alternative-financing availability and cost, (c) whether borrowing states accepting alternative programs (non-IMF) experience worse outcomes than those accepting IMF conditionality. Null hypothesis: if crisis frequency, financing costs, and growth outcomes are comparable between conditionality-accepting and alternative-program states, the founding problem is substantially solved.',
    'If founding problem is solved, conditionality shifts from Rope (coordination) to Snare (extraction dressed as coordination). If problem is live, Rope classification is warranted and the constraint''s legitimacy is stronger. The classification hinge is whether the transfer of policy space serves coordination (solving a collective-action problem affecting the borrowing state''s own future) or extraction (benefiting creditors at the expense of domestic constituencies).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether structural adjustment''s founding coordination problem persists or has been solved.').

omega_variable(
    coordination_function_inseparability,
    'Can the genuine coordination functions (creditor synergy on debt priority, capital-flight prevention) be separated from the policy-space extraction (permanent subordination to external rule-setting), or are they structurally inseparable?',
    'Natural experiment from jurisdictions that implement coordinated debt restructuring or regional financing without IMF conditionality: do capital markets stabilize, do creditors coordinate, does debt service resume—without surrendering policy space? Examples: BRICS development bank, regional bond markets, debt-for-development swaps.',
    'If separable, the extraction component is unnecessary and can be removed without destabilizing the coordination. If inseparable, some policy-space transfer is the price of coordination and the Rope classification is strengthened. The creditor reading depends on inseparability; alternative readings depend on separability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_inseparability, conceptual, 'Whether coordination and policy extraction can be structurally separated or are intrinsically linked.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (state-leadership inability to defy conditionality, public-sector-worker inability to strike effectively) structural (external barriers: capital flight, institutional exclusion) or internalized (borrowed legitimacy, acceptance of market-discipline doctrine)?',
    'Post-exit trajectory analysis: when states exit conditionality (Iceland 2008, Argentina 2001, Bolivia under alternative leadership), do suppression effects persist (internalized) or dissolve (structural)? If they persist (workers remain compliant, policies remain austerity-oriented despite no external requirement), suppression has become internalized and the constraint''s effective suppression is higher than measured. If they dissolve (strikes succeed, alternative policies are implemented, growth resumes), suppression was primarily structural.',
    'If internalized, the constraint is more effective and durable than the measured suppression score suggests. The coordination reading is strengthened (markets truly discipline via internalized norms) but the extraction reading is also strengthened (the constraint has colonized decision-making structures). If structural, suppression depends on continued creditor coordination and can be broken by coalition-building and alternative financing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative policies is external or has become internalized in borrowing states.').

omega_variable(
    kernel_framing_contention,
    'This story instantiates the creditor-coordination reading of the kernel ''structural adjustment conditionalities.'' The sibling debtor-extraction reading treats the same arrangements (IMF programs, conditionality rules) as pure extraction mechanisms. These are fundamentally different constraint stories with different ε values, victim/beneficiary structures, and types. Is this decomposition (one kernel, multiple constraints) the appropriate framing, or should the contest be handled as a single constraint with reading-indexed ε values?',
    'Structural property test: do the two readings produce different victim sets, different beneficiary sets, different answers to the founding-problem question, and different classification outcomes? If yes on all, they are different constraints requiring separate stories. If no (they agree on structure, differ only on interpretation), they are one constraint with epistemic uncertainty.',
    'If separate stories (as this framework treats them), each story can author its own ε, beneficiary/victim structure, and type without compromise; the corpus models the contest directly. If one constraint with reading-indexed ε, the engine must support multiple ε values per constraint-reading pair, which increases complexity but models the kernel''s contested nature more directly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_contention, conceptual, 'Whether this kernel should be modeled as one constraint with multiple readings or as multiple constraints with network relationships.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(stru_tr_t0, observed).
narrative_ontology:measurement(stru_tr_t5, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(stru_tr_t5, observed).
narrative_ontology:measurement(stru_tr_t10, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(stru_tr_t10, observed).
narrative_ontology:measurement(stru_tr_t15, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(stru_tr_t15, observed).
narrative_ontology:measurement(stru_tr_t20, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(stru_tr_t20, observed).
narrative_ontology:measurement(stru_tr_t30, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(stru_tr_t30, projected).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement_basis(stru_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(stru_be_t0, observed).
narrative_ontology:measurement(stru_be_t5, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement_basis(stru_be_t5, observed).
narrative_ontology:measurement(stru_be_t10, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(stru_be_t10, observed).
narrative_ontology:measurement(stru_be_t15, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement_basis(stru_be_t15, observed).
narrative_ontology:measurement(stru_be_t20, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(stru_be_t20, observed).
narrative_ontology:measurement(stru_be_t30, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement_basis(stru_be_t30, projected).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(stru_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(stru_su_t0, observed).
narrative_ontology:measurement(stru_su_t5, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement_basis(stru_su_t5, observed).
narrative_ontology:measurement(stru_su_t10, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement_basis(stru_su_t10, observed).
narrative_ontology:measurement(stru_su_t15, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement_basis(stru_su_t15, observed).
narrative_ontology:measurement(stru_su_t20, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(stru_su_t20, observed).
narrative_ontology:measurement(stru_su_t30, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement_basis(stru_su_t30, projected).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement_basis(stru_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__creditor_coordination_reading, 0.18).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (creditor-coordination) of the contested kernel 'structural adjustment conditionalities.' Sibling constraints are debtor_extraction_reading (treating the same arrangements as pure extraction) and hybrid_selectivity_reading (treating them as selectively applied discipline). The three stories are not rival measurements of a single constraint but rather three different constraints instantiated from the same kernel by different reading frames. Network links connect the family; each story carries its own ε, beneficiary/victim structure, and classification. The ε-invariance principle (DP-001) requires that if reading A's core premise is that conditionalities coordinate creditors and reading B's core premise is that they extract capital, these are structurally different claims about different constraints, not perspectives on the same constraint. The stories are linked because the kernel contest is real—parties in the world hold all three readings—but the framework models the contest by producing three separate constraint stories with different types, not by trying to fold multiple readings into one ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__creditor_coordination_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
