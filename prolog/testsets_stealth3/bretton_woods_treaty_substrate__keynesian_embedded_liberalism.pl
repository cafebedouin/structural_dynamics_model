% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Bretton Woods Capital-Control Substrate (Embedded Liberalism Reading)
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   The Bretton Woods Articles of Agreement (July 1944) established
 *   fixed-but-adjustable currency parities anchored by dollar-gold
 *   convertibility, an International Monetary Fund to pool crisis liquidity
 *   and approve parity changes, and — central to this reading — the full
 *   legitimacy of capital-account controls. This story instantiates the
 *   keynesian_embedded_liberalism reading of that treaty substrate: the
 *   arrangement as a deliberate subordination of international capital to
 *   domestic democratic policy, built so governments could pursue full
 *   employment and welfare-state construction without a capital-flight veto.
 *   Under this reading the standing 1944-73 arrangement carries a genuine
 *   coordination function (parity stability, trade recovery, pooled
 *   liquidity, no repetition of the 1930s) and a deliberate asymmetric burden
 *   (private cross-border finance is the designed target of the controls).
 *   The epsilon authored here is this reading's assessment of the standing
 *   arrangement by its own lights — not of the liberalized order this reading
 *   prefers. Sibling readings of the same kernel are separate constraints,
 *   linked in network.affects_constraints. KEY AGENTS (by structural
 *   relationship): - us_treasury_and_federal_reserve: agenda-setting anchor
 *   (institutional/arbitrage) — issues the reserve currency, pledges gold
 *   convertibility, collects seigniorage, and alone can dissolve the system
 *   unilaterally - european_welfare_state_governments: primary beneficiary
 *   (organized/constrained) — run full-employment policy behind exchange
 *   controls - domestic_electorates: protected beneficiary
 *   (organized/trapped) — receive the employment and welfare the shield funds
 *   - international_finance_institutions: primary target
 *   (powerful/constrained) — merchant banks, investment houses, corporate
 *   treasuries bearing deliberate mobility restriction - export_import_firms:
 *   dual beneficiary/payer (organized/constrained) — parity stability is
 *   revenue, exchange rationing is overhead - foreign_central_banks: dual
 *   beneficiary/payer (institutional/constrained) — hold dollar reserves
 *   against the gold pledge, absorb the adjustment burden -
 *   offshore_eurodollar_participants: excluded circumventors
 *   (powerful/arbitrage) — unregulated offshore dollar market hollowing the
 *   controls from outside - imf_board_of_governors: administrator
 *   (institutional/identity_locked) — approves parities, lends, certifies
 *   controls - imf_research_staff: analytical observer
 *   (analytical/analytical) — sees the whole ledger of accumulation, debt,
 *   and evasion
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.68).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.42).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods Capital-Control Substrate (Embedded Liberalism Reading)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '77f9323a-7dd3-4f22-bea7-fa97bb458a23').
narrative_ontology:cs_kernel_codification('77f9323a-7dd3-4f22-bea7-fa97bb458a23', fixed_text).
narrative_ontology:cs_authority_grounding('77f9323a-7dd3-4f22-bea7-fa97bb458a23', lineage).
narrative_ontology:cs_interpretation_layer_present('77f9323a-7dd3-4f22-bea7-fa97bb458a23').
narrative_ontology:cs_reading_relation('77f9323a-7dd3-4f22-bea7-fa97bb458a23', bretton_woods_treaty_substrate__neoliberal_convertibility, forecloses).
narrative_ontology:cs_reading_relation('77f9323a-7dd3-4f22-bea7-fa97bb458a23', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('77f9323a-7dd3-4f22-bea7-fa97bb458a23', foundational, domestic_policy_autonomy_over_capital_mobility).
narrative_ontology:cs_axiom_status(domestic_policy_autonomy_over_capital_mobility, holdable).
narrative_ontology:cs_axiom_grounding('77f9323a-7dd3-4f22-bea7-fa97bb458a23', domestic_policy_autonomy_over_capital_mobility, instrumental).
narrative_ontology:cs_axiom('77f9323a-7dd3-4f22-bea7-fa97bb458a23', secondary, capital_controls_are_legitimate_instruments).
narrative_ontology:cs_axiom_status(capital_controls_are_legitimate_instruments, holdable).
narrative_ontology:cs_axiom_grounding('77f9323a-7dd3-4f22-bea7-fa97bb458a23', capital_controls_are_legitimate_instruments, conventional).
narrative_ontology:cs_reference_frame('77f9323a-7dd3-4f22-bea7-fa97bb458a23', embedded_liberalism_compact).
narrative_ontology:cs_drift_state('77f9323a-7dd3-4f22-bea7-fa97bb458a23', generalized_float_1973, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('77f9323a-7dd3-4f22-bea7-fa97bb458a23', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, us_treasury_and_federal_reserve).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, european_welfare_state_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_electorates).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, export_import_firms).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, foreign_central_banks).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, export_import_firms).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, foreign_central_banks).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, keynes_clearing_union_principles).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, welfare_state_full_employment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the reserve currency and pledges gold convertibility at thirty-five dollars an ounce, anchoring every other member's parity. Sets the system's de facto agenda through quota weight and veto leverage, and finances deficits abroad by issuing dollar liabilities other members willingly hold. When defending the pledge conflicts with domestic spending priorities, it can suspend convertibility unilaterally, as it did in August 1971 — an exit no other member possesses.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, us_treasury_and_federal_reserve, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, us_treasury_and_federal_reserve, beneficiary).

% Run full-employment budgets, nationalizations, and wage settlements behind a wall of exchange controls that keeps savings at home and speculative attacks out. They maintain declared parities, seek Fund approval for adjustments, and draw on Fund credit when seasons turn bad. Leaving the arrangement would expose their fiscal programs to capital flight and their exporters to exchange chaos, so they stay inside and defend the rules that protect them.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, european_welfare_state_governments, beneficiary,
    organized, generational, constrained, continental).

% Receive the employment, housing, and welfare programs that protected policy space makes fundable, and vote for the governments that deliver them. They cannot relocate their livelihoods across borders to escape domestic monetary conditions, and they experience currency crises mainly as unemployment and price shocks rather than as portfolio events.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_electorates, beneficiary,
    organized, biographical, trapped, national).

% Merchant banks in the City, investment houses on Wall Street, and corporate treasuries that move money across borders for trade, lending, and speculation. Exchange controls cap what they may buy, sell, or repatriate; parity stability removes the exchange risk they are paid to carry; and the Articles treat their cross-border ambitions as a hazard to be contained rather than a service to be hosted. Their legal routes out are narrow, so they build unofficial ones — leads and lags, transfer pricing, and deposits booked outside national jurisdictions.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_institutions, payer,
    powerful, biographical, constrained, global).

% Ship goods across borders under parities that rarely move, which makes contracts priceable and hedging unnecessary. They also queue for import licenses and surrender export proceeds under exchange-control allocation, absorbing delays when hard currency runs short. Parity stability is their revenue; rationing is their overhead.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, export_import_firms, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, export_import_firms, payer).

% Hold dollar reserves against a gold pledge, intervene to hold their own parities, and finance the reserve center's deficits by accumulating its liabilities. They gain deep liquid markets and crisis credit, but they carry the trust risk: if the anchor suspends convertibility, their reserves lose the backing they were promised. Demanding gold en masse, as France periodically does, is their only lever, and using it threatens the system their own reserves depend on.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, foreign_central_banks, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, foreign_central_banks, payer).

% Book dollar deposits and lend outside United States jurisdiction, beyond the reach of American reserve requirements and every member's exchange controls. They exist because the controls create demand for an uncontrolled venue, and they grow every time onshore rules tighten. They were never seated at the treaty table and answer to no parity obligation, yet their market size becomes one of the system's largest single facts by the late 1960s.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, offshore_eurodollar_participants, excluded,
    powerful, immediate, arbitrage, global).

% Administers the Articles: approves parity changes, monitors member balances, lends against conditionality, and certifies when controls are justified. Its authority runs through quota-weighted votes in which the reserve center holds decisive weight. The institution's identity is the mandate itself — it cannot walk away from the regime it exists to run.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_board_of_governors, agenda_setter,
    institutional, generational, identity_locked, global).

% Compile balance-of-payments statistics, publish surveillance reports, and document the widening gap between the Articles' letter and members' practice. They see the whole ledger — who accumulates, who owes, who evades — and their analyses circulate to every member government.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_research_staff, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, us_treasury_and_federal_reserve).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes exchange-rate parities against a common anchor and pools crisis liquidity through the Fund, solving the interwar problems of competitive devaluation and retaliatory currency blocs; capital-account controls are part of the design, keeping speculative flows from dictating domestic monetary and fiscal choices.
% TRANSFER_FUNCTION: Moves policy autonomy and crisis insurance toward member governments, financed by the reserve center's willingness to issue liabilities others hold; moves forgone cross-border mobility and arbitrage returns away from private finance; concentrates seigniorage on the reserve-currency issuer.
% ABSENT_VOICES: Private international finance was present at the founding only as the designated adversary — Keynes designed against it, not with it — and offshore market participants did not yet exist to speak. Most of the colonized world had no seat in 1944; India attended as an exception. Domestic electorates entered only indirectly, through the governments that spoke for them.
% DISAPPEARANCE_RATIONALE: Remove the parity grid, the gold anchor, and the legitimacy of controls overnight — say, in 1960 — and speculative flows immediately test every parity; governments attempting expansionary budgets face capital flight and forced reversal; the welfare-state buildout loses its financing shield; trade reverts to exchange uncertainty or rival currency blocs. The entire postwar settlement of open trade plus domestic intervention depends on the arrangement.
% FOUNDING_PROBLEM: The interwar monetary breakdown: competitive devaluations, capital flights that toppled governments and budgets, collapsed trade, and the political conclusion drawn in 1944 that unrestrained short-term capital movements are incompatible with democratic full-employment policy.
% FOUNDING_PROBLEM_CORROBORATION: Governments and Fund staff attest the danger stayed live through the interval — the 1967 sterling crisis and the 1968 gold rush show speculative attack remained real wherever controls thinned. Against them, academic monetary economists (the Friedman floating-rate case from 1953) and the financial industry itself attest the founding problem was substantially solved and the controls obsolete; the absence of any 1930s-style collapse as controls loosened in the 1960s supports their reading. Corroboration therefore exists on both sides, from outside the beneficiary set.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.68 (interval end): the controls were designed to deny finance its preferred activity, and by the late 1960s the arrangement's burdens had broadened — deficit members absorbed harsh adjustment (the post-1967 sterling austerity) while the reserve center financed itself by issuing liabilities. It is not higher because the coordination delivery was real and valued: trade volumes boomed under stable parities, Fund credit cushioned crises, and no 1930s-style collapse recurred. Suppression (0.42 end-state) is a raw structural property, unscaled by power or scope: legal prohibition and rationing enforced compliance, but consent was broad and offshore exits existed. Theater_ratio (0.34) rises across the interval as the gold-window pretense and par-value fiction outlive their function after 1968, while core functions still operated earlier. Accessibility_collapse (0.38) is low because alternatives never vanished — floating-rate advocacy, bilateral arrangements, and the offshore market persisted visibly. Resistance (0.62) is high: finance lobbied and evaded continuously, academics attacked the parity rationale from 1953, and member states tested discipline repeatedly until the anchor withdrew. The three temporal series share one grid (1944, 1950, 1956, 1962, 1968, 1971, 1973); suppression_requirement is authored because enforcement capacity genuinely traces a maturity-then-decay arc rather than a static picture. Receipt surface: gains concentrate demonstrably on the anchor seat (seigniorage, deficit finance), so gain_flow names us_treasury_and_federal_reserve; removal proved cheap for that seat — executed unilaterally in August 1971 — so fixing_cost is cheap.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats compute differently from identical facts. From international_finance_institutions the arrangement is a confiscation: mobility restricted, returns capped, cross-border ambition treated as destabilizing. From european_welfare_state_governments and domestic_electorates the same clauses are protective infrastructure: the reason a government can nationalize industries and run full employment without a run on its currency. The anchor seat is unique in experiencing both faces — the system serves it (seigniorage, deficit finance) until the discipline binds it too (gold drain, 1971), at which point its arbitrage-grade exit ends the arrangement for everyone. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the anchor seat sits nearest the beneficiary end (designer, enforcer, and largest collector), welfare governments and electorates next (protected policy space, no exit sought), traders and foreign central banks mid-range (dual beneficiary/payer declarations — stability revenue against rationing overhead, liquidity against trust risk). The victim declaration drives the finance seat toward the full-target end, reinforced by constrained exit: legal routes out were narrow, and the offshore workaround took two decades to build. Offshore participants are excluded rather than coordinated — they sit outside the beneficiary/victim derivation entirely, which is itself the structural fact: the arrangement's enforcement perimeter is what defines them. No directionality overrides are authored; the derivation from declarations and exit options matches this reading's structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar monetary chaos and capital-flight vetoes on democratic policy — receded decisively by the late 1950s: convertibility was restored, trade boomed, and no competitive-devaluation spiral returned. What persisted afterward was defended by second-order justifications (trade stability, liquidity provision) and institutional momentum. The arrangement did not atrophy into inertial performance: theater rose, but the structure retained real enforcement capacity until the anchor state withdrew it, and it then dissolved within twenty-six months rather than persisting as an empty shell. Mandatrophy was resolved by dissolution, not decay. The tangled_rope classification keeps both truths legible: the coordination was genuine (blocking a snare misread, despite a named victim set), and the subordination of finance was deliberate design rather than incidental friction (blocking a rope misread, despite real collective benefits).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading does the treaty substrate''s actual structure support — this one (finance targeted, states protected) or neoliberal_convertibility (states targeted, finance enabled)?',
    'Archival and doctrinal analysis of the Articles'' operative provisions: which clauses bound whom, which obligations actually bit in practice (Article VIII versus Article VI usage, parity-change approvals, control justifications), and how the Fund''s Executive Board adjudicated disputes.',
    'If the binding clauses protected capital mobility rather than policy space, the victim and beneficiary sets reverse, epsilon moves toward the neoliberal reading''s value, and this story''s tangled_rope structure collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Kernel-level contest over which party the substrate constrains.').

omega_variable(
    finance_seat_directionality_contest,
    'Is the finance seat''s high directionality a property of the arrangement or an artifact of this reading''s framing — would a sovereignty_defense or neoliberal author place finance nearer the beneficiary end?',
    'Cross-reading comparison: author the sibling stories and compare computed per-seat classifications for the same historical actors under each reading''s declarations.',
    'If finance computes as beneficiary under a sibling reading, the family exhibits maximal seat divergence and the kernel contest is confirmed as directional rather than factual; if all readings agree, the contest is about evaluation only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finance_seat_directionality_contest, conceptual, 'Where the readings disagree: the directionality of the international-finance seat.').

omega_variable(
    eurodollar_leakage_magnitude,
    'How much did offshore eurocurrency markets reduce the effective burden the controls placed on international finance by the late 1960s?',
    'BIS eurocurrency market size series versus onshore control coverage estimates; measure the share of cross-border capital flow routed outside national jurisdictions.',
    'Large leakage means late-interval extractiveness on finance is overstated and the measured rise reflects burden-shifting onto deficit members rather than intensifying finance extraction; small leakage supports the monotonic rise as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eurodollar_leakage_magnitude, empirical, 'Offshore evasion''s effect on the extraction series.').

omega_variable(
    collapse_vindication_or_failure,
    'Did the 1971-73 collapse vindicate the arrangement (its problem solved, retired honorably) or refute it (unable to survive the pressures it was built to manage)?',
    'Counterfactual economic history: whether floating rates plus restored capital mobility reproduced interwar-scale chaos (they did not at 1970s scale, but later crises reopened the question), and archival study of whether the collapse was chosen or forced.',
    'A vindication reading pushes the story toward a scaffold-like interpretation (transitional support successfully withdrawn); a failure reading confirms a tangled_rope that broke under load rather than completed its term.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collapse_vindication_or_failure, empirical, 'Whether the regime''s end was completion or collapse.').

omega_variable(
    seigniorage_concentration,
    'What share of the arrangement''s total benefit flowed to the reserve center through liability issuance rather than diffusing across member governments as policy space?',
    'Estimate reserve-center seigniorage (growth in foreign-held dollar liabilities minus gold outflows) against measured policy-space value across members.',
    'High concentration strengthens the case that the agenda-setting seat captured the gains (the receipt surface already names it); low concentration supports a more genuinely mutual coordination reading and weakens the capture flavor of the receipt verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_concentration, empirical, 'Distribution of the arrangement''s gains between the anchor and the membership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1944, 0.1).
narrative_ontology:measurement_basis(bret_tr_t1944, observed).
narrative_ontology:measurement(bret_tr_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1950, 0.12).
narrative_ontology:measurement_basis(bret_tr_t1950, observed).
narrative_ontology:measurement(bret_tr_t1956, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1956, 0.15).
narrative_ontology:measurement_basis(bret_tr_t1956, observed).
narrative_ontology:measurement(bret_tr_t1962, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1962, 0.19).
narrative_ontology:measurement_basis(bret_tr_t1962, observed).
narrative_ontology:measurement(bret_tr_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1968, 0.26).
narrative_ontology:measurement_basis(bret_tr_t1968, observed).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1971, 0.31).
narrative_ontology:measurement_basis(bret_tr_t1971, observed).
narrative_ontology:measurement(bret_tr_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1973, 0.34).
narrative_ontology:measurement_basis(bret_tr_t1973, observed).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1944, 0.5).
narrative_ontology:measurement_basis(bret_be_t1944, observed).
narrative_ontology:measurement(bret_be_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1950, 0.53).
narrative_ontology:measurement_basis(bret_be_t1950, observed).
narrative_ontology:measurement(bret_be_t1956, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1956, 0.56).
narrative_ontology:measurement_basis(bret_be_t1956, observed).
narrative_ontology:measurement(bret_be_t1962, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1962, 0.59).
narrative_ontology:measurement_basis(bret_be_t1962, observed).
narrative_ontology:measurement(bret_be_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1968, 0.63).
narrative_ontology:measurement_basis(bret_be_t1968, observed).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1971, 0.66).
narrative_ontology:measurement_basis(bret_be_t1971, observed).
narrative_ontology:measurement(bret_be_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1973, 0.68).
narrative_ontology:measurement_basis(bret_be_t1973, observed).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1944, 0.55).
narrative_ontology:measurement_basis(bret_su_t1944, observed).
narrative_ontology:measurement(bret_su_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1950, 0.62).
narrative_ontology:measurement_basis(bret_su_t1950, observed).
narrative_ontology:measurement(bret_su_t1956, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1956, 0.66).
narrative_ontology:measurement_basis(bret_su_t1956, observed).
narrative_ontology:measurement(bret_su_t1962, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1962, 0.64).
narrative_ontology:measurement_basis(bret_su_t1962, observed).
narrative_ontology:measurement(bret_su_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1968, 0.56).
narrative_ontology:measurement_basis(bret_su_t1968, observed).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1971, 0.47).
narrative_ontology:measurement_basis(bret_su_t1971, observed).
narrative_ontology:measurement(bret_su_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1973, 0.42).
narrative_ontology:measurement_basis(bret_su_t1973, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resource_allocation).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__sovereignty_defense).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Bretton Woods system' covers at least three structurally distinct claims about the same treaty substrate. Per the epsilon-invariance principle these are authored as separate stories in one constraint family, linked by affects_constraints: this file (embedded liberalism: finance targeted, states protected, controls legitimate), neoliberal_convertibility (states targeted, finance enabled, controls as violations), and sovereignty_defense (external discipline targeted, national monetary autonomy protected). The treaty text is upstream of all three readings; this reading shaped the sovereignty_defense reading's legitimacy conditions while standing in a foreclosure relation to the neoliberal reading, since their foundational priority orderings cannot coexist in one framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
