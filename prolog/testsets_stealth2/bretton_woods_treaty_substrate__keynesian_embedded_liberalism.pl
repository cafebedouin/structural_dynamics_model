% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__keynesian_embedded_liberalism, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods Capital-Control Regime (Embedded-Liberalism Reading)
 *   domain: international political economy / monetary history / institutional design
 *
 * SUMMARY:
 *   The Bretton Woods arrangement (1944-1973) pegged member currencies at par
 *   values adjustable only with Fund concurrence, anchored the system in
 *   dollar-gold convertibility, pooled liquidity for current-account deficits
 *   through the IMF, and — through Article VI of the Articles of Agreement —
 *   expressly authorized national restrictions on cross-border capital
 *   movements. This story instantiates the keynesian_embedded_liberalism
 *   reading of the bretton_woods_treaty_substrate kernel: the arrangement is
 *   a constraint on international capital whose function is protecting
 *   domestic macroeconomic policy space. Per the kernel-reading epsilon rule,
 *   epsilon's referent is the standing 1944-73 arrangement as this reading
 *   assesses it — not the floating, capital-mobile system this reading's
 *   tradition would criticize. The colloquial label 'Bretton Woods system'
 *   decomposes into three readings with incompatible victim/beneficiary
 *   structures (this one; neoliberal_convertibility, which reverses the
 *   constraint's direction onto governments; sovereignty_defense, which
 *   emphasizes external-discipline constraints on monetary autonomy); they
 *   are authored as separate linked stories per the epsilon-invariance
 *   principle. The claimed type and the metrics are authored independently:
 *   the structure — genuine coordination function plus asymmetric, actively
 *   enforced cost-bearing — supports tangled_rope; the metrics describe the
 *   arrangement's actual operation across its arc.
 *
 * KEY AGENTS:
 *   - welfare_state_governments: primary beneficiary (institutional power, constrained exit) — receive the policy space the capital-control regime purchases; also co-author and administer it
 *   - domestic_households_workers: secondary beneficiary (organized, trapped) — receive employment stability and welfare expansion; themselves subject to exchange control
 *   - international_speculative_finance: primary target (organized, constrained exit) — bears mobility restriction and forgone arbitrage; built the eurodollar exit
 *   - trade_credit_providers: dual-positioned beneficiary/payer (organized, constrained) — gain rate stability, bear lending limits
 *   - united_states_treasury: anchor seat (powerful, arbitrage exit) — collects seigniorage, bears the Triffin burden; alone able to exit unilaterally
 *   - imf_secretariat: agenda_setter (institutional, trapped) — administers par values, adjudicates control legality, lends to deficit members
 *   - postcolonial_development_states: excluded (powerless, trapped) — absent from the 1944 design table; inherited the rules and later used the space
 *   - economic_historians: analytical observer — sees all seats' ledgers at once
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.55).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.55).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.55).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods Capital-Control Regime (Embedded-Liberalism Reading)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international political economy / monetary history / institutional design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '4007e59a-43f4-421e-ab43-91feef77b182').
narrative_ontology:cs_kernel_codification('4007e59a-43f4-421e-ab43-91feef77b182', formalized).
narrative_ontology:cs_authority_grounding('4007e59a-43f4-421e-ab43-91feef77b182', lineage).
narrative_ontology:cs_interpretation_layer_present('4007e59a-43f4-421e-ab43-91feef77b182').
narrative_ontology:cs_reading_relation('4007e59a-43f4-421e-ab43-91feef77b182', bretton_woods_treaty_substrate__neoliberal_convertibility, forecloses).
narrative_ontology:cs_reading_relation('4007e59a-43f4-421e-ab43-91feef77b182', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('4007e59a-43f4-421e-ab43-91feef77b182', foundational, domestic_policy_autonomy_prior_to_capital_mobility).
narrative_ontology:cs_axiom_status(domestic_policy_autonomy_prior_to_capital_mobility, holdable).
narrative_ontology:cs_axiom_grounding('4007e59a-43f4-421e-ab43-91feef77b182', domestic_policy_autonomy_prior_to_capital_mobility, deontological).
narrative_ontology:cs_axiom('4007e59a-43f4-421e-ab43-91feef77b182', secondary, capital_controls_legitimate_policy_instruments).
narrative_ontology:cs_axiom_status(capital_controls_legitimate_policy_instruments, holdable).
narrative_ontology:cs_axiom_grounding('4007e59a-43f4-421e-ab43-91feef77b182', capital_controls_legitimate_policy_instruments, conventional).
narrative_ontology:cs_reference_frame('4007e59a-43f4-421e-ab43-91feef77b182', embedded_liberalism_compromise).
narrative_ontology:cs_drift_state('4007e59a-43f4-421e-ab43-91feef77b182', generalized_floating_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('4007e59a-43f4-421e-ab43-91feef77b182', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, welfare_state_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_households_workers).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, trade_credit_providers).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, united_states_treasury).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_speculative_finance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, trade_credit_providers).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, united_states_treasury).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, keynesian_full_employment_doctrine).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, embedded_liberalism_compromise).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_article_vi_capital_control_legality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Signed the Articles of Agreement, pegged their currencies at par values adjustable only with Fund concurrence, and administered national exchange controls on cross-border capital movements. In exchange they received the ability to run countercyclical fiscal and monetary policy — full-employment budgets, welfare expansion — without facing capital flight or speculative attack on the currency. Leaving the arrangement meant abandoning the Fund and the trading system's settlement arrangements, which none attempted.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, welfare_state_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, welfare_state_governments, agenda_setter).

% Held wages and savings denominated in national currencies and were themselves subject to exchange control — most European residents could not freely buy foreign securities or currency. Received the employment stability and expanding public services that protected macroeconomic policy made possible. Moving savings or labor abroad was legally restricted for most and practically unavailable to nearly all.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_households_workers, beneficiary,
    organized, biographical, trapped, national).

% Cross-border banks, portfolio investors, and currency dealers faced legal barriers on moving capital between currencies and jurisdictions: prior authorization, quantitative limits, taxes on foreign lending (the US Interest Equalization Tax), and outright prohibitions in many jurisdictions. Returns on cross-border activity ran below what unconstrained arbitrage would have yielded, while the peg suppressed the exchange-rate volatility that currency trading profits from. Some built the eurodollar market in London as an offshore channel outside national control administrations — the closest thing to an exit, and it grew steadily through the 1960s.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_speculative_finance, payer,
    organized, biographical, constrained, global).

% Banks and exporters financing cross-border goods trade benefited from fixed par values, which removed most currency risk from invoicing and trade lending. They also bore the same authorization requirements and lending limits as other cross-border finance, and the US voluntary restraint programs pressed them to curtail foreign lending. Their position was mixed: stability they could price into contracts, restrictions they could not avoid.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, trade_credit_providers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, trade_credit_providers, payer).

% Anchored the system: the dollar was convertible to gold at $35 an ounce and all other currencies pegged to the dollar. Collected reserve-currency privilege — other nations willingly held dollar balances, financing US deficits cheaply. Also bore the system's adjustment burden: maintaining convertibility required running deficits and watching gold drain, which by the late 1960s threatened the gold backing itself (the Triffin dilemma). Unlike every other seat, it could and ultimately did exit unilaterally, suspending gold convertibility in August 1971; the system dissolved around that decision.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, united_states_treasury, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, united_states_treasury, payer).

% Administered the par-value system: registered par values, approved adjustments, monitored member exchange practices, managed the reserve pool, and lent to members with current-account deficits. Its function was the system itself; it had no existence outside the arrangement it administered. Its interpretations — which restrictions were permissible, which practices violated the code — were the operative law between formal amendments.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_secretariat, agenda_setter,
    institutional, generational, trapped, global).

% Most were still colonies in 1944 and had no seat at the Bretton Woods conference; quotas, par values, and control rules were designed without them. After independence they inherited the arrangements — and used the policy space the rules provided for import-substitution and development planning — but had no voice in the design and little weight in Fund governance. Their collective objection arrived decades late, in the 1970s New International Economic Order campaign.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, postcolonial_development_states, excluded,
    powerless, generational, trapped, continental).

% Reconstruct the system's operation from archival and quantitative records: Ruggie's 'embedded liberalism' synthesis names this reading; Frieden, Eichengreen, and Helleiner trace the control regimes, the eurodollar exit, and the collapse. They hold no position in the arrangement and can see all seats' ledgers at once.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, welfare_state_governments).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interwar collective-action failure: when every major country could devalue, restrict trade, and attract or repel hot money at will, each devaluation invited retaliation and each national capital market transmitted others' crises. The Articles coordinated exchange rates around pegged par values adjustable with Fund concurrence, pooled reserves for current-account financing, and expressly authorized (Article VI) national restrictions on capital-account movements — so no government had to choose between holding the peg and running domestic full-employment policy.
% TRANSFER_FUNCTION: Moves the cost of macroeconomic adjustment from domestic populations onto holders of mobile capital: because capital cannot flee a currency whose exit is legally barred, governments gain room to run deficits, reflate, and build welfare systems without financing punishment. It also moves reserve-currency seigniorage to the United States and, through Fund lending, moves liquidity from surplus to deficit members.
% ABSENT_VOICES: The colonized world had no seat at the 1944 table — the design was negotiated by the United States and the United Kingdom with allied governments in attendance; postcolonial economies inherited rules they never shaped (authored as the excluded stakeholder). Ordinary savers and depositors, themselves subject to exchange control, were represented only through their governments. Weighted voting in the Fund muted small members throughout.
% DISAPPEARANCE_RATIONALE: It did rearrange, on the historical record: after the 1971-73 collapse, exchange rates floated, the eurodollar market became the core of a vastly larger offshore financial system, capital mobility returned — and with it the market discipline on domestic policy that the arrangement had barred — and the 1970s brought simultaneous inflation and unemployment that pegged-rate policy space had contained. Governments re-encountered the founding problem from the other side; the IMF's 2012 institutional view formally re-legitimizing capital-flow management concedes the problem outlived the arrangement.
% FOUNDING_PROBLEM: The interwar monetary collapse: competitive devaluation, retaliatory trade restriction, and hot-money flights that toppled governments and destroyed both trade and employment — the problem Keynes and White designed against, compounded by postwar reconstruction liquidity needs.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the payer seat's own conduct: international financiers built the eurodollar market specifically to escape the control regime, which attests both that the constraint was real and that the mobility it barred was valuable. Post-collapse scholarship across the political spectrum — Friedman's attack on the arrangement as well as Eichengreen's and Helleiner's histories — attests the founding problem's reality and the arrangement's genuine function. No attestation from beneficiary governments is required for the problem's status.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55 for the mature arrangement: finance bore real, legally enforced costs (prior authorization, quantitative limits, the Interest Equalization Tax, forgone arbitrage), but the same pegged-rate structure removed currency risk from trade and lending, so part of what finance paid returned as stability it could price. Suppression is 0.55: the constraint's force was legal-administrative (exit barred by statute and Fund surveillance), not violent, but it genuinely removed the exit option for two decades — and suppression is authored as a raw structural property, unscaled by power or scope, while the engine scales effective extraction for the global scope of the system. Theater is 0.30 at maturity — the function was mostly real — with the series showing theatrical maintenance emerging late (the gold pool's dual-tier fiction after 1968, the Smithsonian defense of 1971). Accessibility_collapse is 0.45: alternatives (floating rates, offshore markets) never collapsed — the eurodollar market proved the alternative was buildable. Resistance is 0.55: continuous financial-sector resistance, from lobbying against the Interest Equalization Tax to constructing the offshore exit. The three measurement series share one time grid (1944, 1950, 1958, 1965, 1968, 1971, 1973) so no metric's end-state value is substituted into earlier rows; base_properties describe the mature standing arrangement, while the series traces the full arc including the terminal collapse, where suppression decays faster than extraction.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience one structure. From welfare_state_governments the regime is the enabling floor of the welfare state — the reason a postwar government could reflate without a run on the currency. From domestic_households_workers it is the invisible guarantee behind full employment. From international_speculative_finance it is confiscation of mobility and return, administered by remote bureaucracies. From the imf_secretariat it is technical balance-of-payments management — neither freedom nor confiscation. The payer and beneficiary seats should compute different types from the same structural data; that divergence is the measurement the corpus exists to take, not an inconsistency to reconcile. Inter-institutionally, the US seat's arbitrage exit — exercised in 1971 — means the arrangement's strongest member was never structurally bound by it, a persistence condition the weaker members did not share; same-level actors (European governments at nominally equal institutional standing) differed in exit because the anchor privilege was concentrated in one of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. welfare_state_governments and domestic_households_workers sit near the beneficiary end (low d, effective extraction damped or inverted into subsidy) — the arrangement subsidizes their policy autonomy. international_speculative_finance is the declared victim with constrained exit, placing it near the full-target end: a target that cannot leave amplifies effective extraction. trade_credit_providers are dual-declared (beneficiary + payer) and should compute near symmetric. united_states_treasury is dual-declared with arbitrage-grade exit — the derivation pulls it toward the beneficiary end on exit, which is accurate for seigniorage but incomplete for the Triffin burden; the open question is carried in the us_net_position_triffin omega rather than a directionality override, because the override key (power atom) cannot separate the US seat from other powerful agents and would misfire across seats. No directionality_overrides are authored: the role plus exit declarations already differentiate every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Two misclassifications are blocked. Reading the arrangement as pure coordination ignores what this reading itself declares: finance bore asymmetric, actively enforced costs — the victim declaration is load-bearing, and the enforcement requirement is structural, not rhetorical. Reading it as pure extraction ignores the genuine coordination function (rate stability, pooled liquidity, the end of beggar-thy-neighbor devaluation) and the absence of a concentrated rent-capturing seat: the gains accrued to member governments' policy space and their publics. gain_flow names welfare_state_governments because the extraction demonstrably purchased their policy autonomy — receipt of the constraint's gains — not because any seat captured rents for private enrichment. On mandatrophy: the founding problem (interwar monetary chaos) was live throughout the interval, and the arrangement died by arbitrage-driven collapse rather than by outliving its function — founding_problem_status 'live' combined with disappearance verdict 'world_rearranges' produces no dead-arrangement mismatch flag. The piton reading fails on the same evidence: maintenance was not mostly theatrical at maturity, and the death was caused by an exit (the eurodollar market) that the constraint itself could not close — the signature of a binding structure losing to arbitrage, not of inertial performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the keynesian_embedded_liberalism reading of the bretton_woods_treaty_substrate kernel; would the sibling readings (neoliberal_convertibility, sovereignty_defense) assign the victim and beneficiary roles differently over the same arrangement?',
    'Generate the sibling stories and compare as a family: neoliberal_convertibility assigns the victim role to national governments and the beneficiary role to mobile capital; sovereignty_defense keeps governments as beneficiaries but relocates cost-bearing to external-discipline and gold-convertibility obligations. Compare the three epsilon values and victim sets.',
    'If the neoliberal reading computes as the better structural fit for the Articles'' actual operation, this reading''s victim/beneficiary structure is a normative overlay rather than a description; if this reading fits the 1944-73 operation, the neoliberal reading describes the post-1973 world rather than the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Which reading of the Bretton Woods kernel the declared victim/beneficiary structure belongs to.').

omega_variable(
    trilemma_separability,
    'Were capital controls structurally necessary for pegged rates plus domestic policy autonomy (the Mundellian trilemma), or separable from the system''s coordination function?',
    'Comparative analysis of systems that attempted pegged rates with open capital accounts (the interwar gold standard, the EMS before 1992): if they consistently broke under speculative attack while control regimes held, the trilemma binds and the controls were the enabling instrument.',
    'If inseparable, the measured costs to finance are the price of the coordination itself and this reading''s classification sits closer to pure coordination; if separable, the controls were a distinct cost-imposing layer riding on rate stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trilemma_separability, empirical, 'Whether the constraint on capital was the coordination mechanism or an add-on to it.').

omega_variable(
    eurodollar_exit_effect,
    'Did the eurodollar market give international finance effective arbitrage-grade exit during the interval, lowering its actual cost-bearing below what the legal regime implies?',
    'Quantify offshore market volumes against onshore controlled activity by decade (BIS and Bank of England data): if offshore volumes rival onshore by the mid-1960s, the constraint''s effective reach was decaying well before 1971.',
    'High offshore penetration means effective extraction on finance was falling through the late interval and the arrangement''s terminal state was already largely inertial — the 1971-73 collapse ratified an exit that had already been built rather than causing one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eurodollar_exit_effect, empirical, 'How much of the declared victim set''s cost-bearing the offshore exit actually neutralized.').

omega_variable(
    us_net_position_triffin,
    'Was the United States a net beneficiary or net cost-bearer of the arrangement by the late 1960s, given seigniorage gains against gold-outflow losses?',
    'Ledger analysis of US gold stock, dollar liabilities, and the value of reserve-currency seigniorage, 1958-1971.',
    'If net cost-bearer, the arrangement''s persistence was maintained despite its anchor''s interests — a different persistence mechanism than beneficiary maintenance, and one that predicts unilateral collapse (which is what occurred in 1971).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_net_position_triffin, empirical, 'Whether the anchor seat''s dual declaration resolves to net gain or net burden.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bw_embedded_liberalism_tr_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1944, 0.15).
narrative_ontology:measurement_basis(bw_embedded_liberalism_tr_t1944, observed).
narrative_ontology:measurement(bw_embedded_liberalism_tr_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1950, 0.17).
narrative_ontology:measurement_basis(bw_embedded_liberalism_tr_t1950, observed).
narrative_ontology:measurement(bw_embedded_liberalism_tr_t1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1958, 0.22).
narrative_ontology:measurement_basis(bw_embedded_liberalism_tr_t1958, observed).
narrative_ontology:measurement(bw_embedded_liberalism_tr_t1965, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1965, 0.28).
narrative_ontology:measurement_basis(bw_embedded_liberalism_tr_t1965, observed).
narrative_ontology:measurement(bw_embedded_liberalism_tr_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1968, 0.4).
narrative_ontology:measurement_basis(bw_embedded_liberalism_tr_t1968, observed).
narrative_ontology:measurement(bw_embedded_liberalism_tr_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1971, 0.48).
narrative_ontology:measurement_basis(bw_embedded_liberalism_tr_t1971, observed).
narrative_ontology:measurement(bw_embedded_liberalism_tr_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1973, 0.5).
narrative_ontology:measurement_basis(bw_embedded_liberalism_tr_t1973, observed).

% Extraction over time
narrative_ontology:measurement(bw_embedded_liberalism_be_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1944, 0.4).
narrative_ontology:measurement_basis(bw_embedded_liberalism_be_t1944, observed).
narrative_ontology:measurement(bw_embedded_liberalism_be_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1950, 0.44).
narrative_ontology:measurement_basis(bw_embedded_liberalism_be_t1950, observed).
narrative_ontology:measurement(bw_embedded_liberalism_be_t1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1958, 0.5).
narrative_ontology:measurement_basis(bw_embedded_liberalism_be_t1958, observed).
narrative_ontology:measurement(bw_embedded_liberalism_be_t1965, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1965, 0.56).
narrative_ontology:measurement_basis(bw_embedded_liberalism_be_t1965, observed).
narrative_ontology:measurement(bw_embedded_liberalism_be_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1968, 0.62).
narrative_ontology:measurement_basis(bw_embedded_liberalism_be_t1968, observed).
narrative_ontology:measurement(bw_embedded_liberalism_be_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1971, 0.58).
narrative_ontology:measurement_basis(bw_embedded_liberalism_be_t1971, observed).
narrative_ontology:measurement(bw_embedded_liberalism_be_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1973, 0.5).
narrative_ontology:measurement_basis(bw_embedded_liberalism_be_t1973, observed).

% Suppression requirement over time
narrative_ontology:measurement(bw_embedded_liberalism_su_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1944, 0.3).
narrative_ontology:measurement_basis(bw_embedded_liberalism_su_t1944, observed).
narrative_ontology:measurement(bw_embedded_liberalism_su_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1950, 0.42).
narrative_ontology:measurement_basis(bw_embedded_liberalism_su_t1950, observed).
narrative_ontology:measurement(bw_embedded_liberalism_su_t1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1958, 0.55).
narrative_ontology:measurement_basis(bw_embedded_liberalism_su_t1958, observed).
narrative_ontology:measurement(bw_embedded_liberalism_su_t1965, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1965, 0.62).
narrative_ontology:measurement_basis(bw_embedded_liberalism_su_t1965, observed).
narrative_ontology:measurement(bw_embedded_liberalism_su_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1968, 0.68).
narrative_ontology:measurement_basis(bw_embedded_liberalism_su_t1968, observed).
narrative_ontology:measurement(bw_embedded_liberalism_su_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1971, 0.45).
narrative_ontology:measurement_basis(bw_embedded_liberalism_su_t1971, observed).
narrative_ontology:measurement(bw_embedded_liberalism_su_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1973, 0.25).
narrative_ontology:measurement_basis(bw_embedded_liberalism_su_t1973, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, enforcement_mechanism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__sovereignty_defense).

% DUAL FORMULATION NOTE:
% The colloquial label 'Bretton Woods system' covers three structurally distinct claims about what the arrangement constrained. This story (keynesian_embedded_liberalism) authors epsilon for the standing 1944-73 arrangement read as a constraint on international capital protecting domestic policy space, with finance in the victim set. The neoliberal_convertibility sibling reads the same treaty substrate as a constraint on government intervention enabling free capital markets — victim and beneficiary sets inverted. The sovereignty_defense sibling reads it as a constraint on external monetary discipline preserving national monetary sovereignty. All three share the kernel (the Articles of Agreement) but assign different victim/beneficiary structures over the same arrangement; per the epsilon-invariance principle they are authored as separate linked stories and must not be merged, since merging would average incompatible victim sets into a single epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
