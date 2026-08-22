% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__overdetermined_composite_reading, []).

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
 *   constraint_id: monetary_anchor_principle__overdetermined_composite_reading
 *   human_readable: Bretton Woods Gold-Exchange Anchor — Overdetermined Composite Reading
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   The Bretton Woods gold-exchange standard (1944-1971) pegged the dollar to
 *   gold at $35 an ounce, other members' currencies to the dollar, and
 *   disciplined national monetary policy through convertibility — the
 *   monetary anchor this story is about. This file instantiates one reading
 *   of that anchor's end: that collapse was overdetermined by four
 *   independent structural streams (the Triffin liquidity dilemma,
 *   Vietnam-era US fiscal deficits, the Keynesian policy consensus
 *   subordinating external balance to full employment, and
 *   technologically-driven capital mobility that made controls leaky), making
 *   the anchor's death inevitable by the late 1960s and reducing the August
 *   1971 closure to recognition of an already-complete transition. The
 *   claim/metric split is deliberate: claimed_type is authored from the
 *   reading's structural account — a hybrid arrangement with a genuine
 *   coordination function (stable parities, liquidity provision, adjustment
 *   credit) entangled with a real transfer (seigniorage to the reserve
 *   issuer, adjustment burden on the deficit periphery) — while the metrics
 *   describe the arrangement's actual operation across its life. Sibling
 *   readings are separate stories in the same constraint family, linked
 *   through network.affects_constraints; their structural deltas are recorded
 *   in the omegas.
 *
 * KEY AGENTS:
 *   - us_treasury_reserve_authority: primary beneficiary and agenda-setter (institutional/arbitrage) — issues the reserve currency, sets and defends the $35 gold price, settles deficits in its own paper, uniquely exempt from adjustment
 *   - foreign_central_banks: primary target (organized/trapped) — compelled to accumulate dollar reserves to hold their pegs; bear the devaluation risk realized in 1971-73
 *   - member_state_finance_ministries: secondary beneficiary (institutional/constrained) — gain stable-rate trade and fiscal space under the adjustable peg; accept conditionality when drawing
 *   - deficit_country_populations: target (powerless/trapped) — absorb deflationary packages (wage freezes, spending cuts) designed in forums they do not sit in
 *   - surplus_country_governments: target (powerful/constrained) — absorb imported inflation or revalue against exporter resistance; West Germany above all
 *   - dollar_inflation_savers: target (moderate/constrained) — bear the erosion of nominal holdings as US inflation accelerates 1965-1970
 *   - fixed_rate_exporters: secondary beneficiary (organized/constrained) — price cross-border contracts under rarely-moving parities
 *   - currency_speculators: excluded constituency (powerful/arbitrage) — one-way-bet traders whose activity the control machinery exists to blunt; no governance seat
 *   - imf_par_value_administration: administrator (institutional/identity_locked) — registers parities, lends against conditionality, patches the system rather than declaring the kernel broken
 *   - monetarist_critics: analytical observer (analytical/analytical) — diagnose the broken adjustment mechanism from 1953 onward; hold no enforcement power and no rents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.76).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.72).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Bretton Woods Gold-Exchange Anchor — Overdetermined Composite Reading").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, '552224a6-f24a-4c19-b9d8-e2bfd65293bb').
narrative_ontology:cs_kernel_codification('552224a6-f24a-4c19-b9d8-e2bfd65293bb', formalized).
narrative_ontology:cs_authority_grounding('552224a6-f24a-4c19-b9d8-e2bfd65293bb', extraction).
narrative_ontology:cs_interpretation_layer_present('552224a6-f24a-4c19-b9d8-e2bfd65293bb').
narrative_ontology:cs_reading_relation('552224a6-f24a-4c19-b9d8-e2bfd65293bb', monetary_anchor_principle__punctuated_swap_reading, influences).
narrative_ontology:cs_reading_relation('552224a6-f24a-4c19-b9d8-e2bfd65293bb', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('552224a6-f24a-4c19-b9d8-e2bfd65293bb', foundational, structural_overdetermination_of_regime_collapse).
narrative_ontology:cs_axiom_status(structural_overdetermination_of_regime_collapse, holdable).
narrative_ontology:cs_axiom_grounding('552224a6-f24a-4c19-b9d8-e2bfd65293bb', structural_overdetermination_of_regime_collapse, empirically_contingent).
narrative_ontology:cs_axiom('552224a6-f24a-4c19-b9d8-e2bfd65293bb', secondary, functional_death_precedes_formal_death).
narrative_ontology:cs_axiom_status(functional_death_precedes_formal_death, holdable).
narrative_ontology:cs_axiom_grounding('552224a6-f24a-4c19-b9d8-e2bfd65293bb', functional_death_precedes_formal_death, empirically_contingent).
narrative_ontology:cs_reference_frame('552224a6-f24a-4c19-b9d8-e2bfd65293bb', multi_stream_structural_equilibrium).
narrative_ontology:cs_drift_state('552224a6-f24a-4c19-b9d8-e2bfd65293bb', late_1960s_crisis_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('552224a6-f24a-4c19-b9d8-e2bfd65293bb', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, us_treasury_reserve_authority).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, member_state_finance_ministries).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, fixed_rate_exporters).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, foreign_central_banks).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, surplus_country_governments).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, deficit_country_populations).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, dollar_inflation_savers).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, triffin_dilemma_thesis).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, structural_overdetermination_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and defends the dollar's gold price at $35 an ounce, issues the reserve currency all other members hold, and decides whether to cover external deficits with gold or with additional dollar liabilities. Because other members must accept dollars to peg their currencies, it can settle its external deficits — including Vietnam War spending — in its own paper, and it alone is exempt from the deflationary adjustment its own Articles prescribe for deficit countries. Its exit from balance-of-payments discipline is the privilege of issuing what others are obliged to hold.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, us_treasury_reserve_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Run postwar reconstruction and then full-employment fiscal policy inside a system that guarantees stable exchange rates, supplies IMF credit for temporary shortfalls, and permits occasional devaluation as a last-resort escape valve. They gain the trade environment and the fiscal space; in return they accept IMF conditionality when they draw, and must defend their parity with deflation when markets doubt them.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, member_state_finance_ministries, beneficiary,
    institutional, biographical, constrained, national).

% Manufacturers and traders who price contracts across borders under parities that rarely move. Exchange-rate certainty lets them build export capacity and long-term supply chains without hedging costs; they lobby for parity stability and against both devaluation and revaluation that would disturb their cost structures.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, fixed_rate_exporters, beneficiary,
    organized, biographical, constrained, continental).

% Accumulate dollar reserves as the operating cost of pegging their currencies: to hold their parity they must buy dollars whenever their currencies strengthen, so their dollar holdings grow whether or not they want them. They may present dollars for gold at $35, but doing so drains the system's gold and invites political pressure from Washington; dumping dollars would break their own pegs. They bear the devaluation risk on reserves they did not choose to accumulate — risk realized when the dollar is devalued and floated after 1971.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, foreign_central_banks, payer,
    organized, generational, trapped, national).

% Governments of strong-currency countries (West Germany above all) that absorb US inflation as dollars flow in, and face a choice between revaluing — which their exporters resist — or accumulating more dollar claims whose value erodes. They are pressed at every G10 meeting to expand, revalue, or absorb; their policy autonomy is bounded by the reserve issuer's domestic priorities.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, surplus_country_governments, payer,
    powerful, biographical, constrained, national).

% Workers and households in weak-currency countries (Britain 1966-67, repeatedly) who absorb the deflationary packages — wage freezes, spending cuts, credit squeeze — that parity defense requires. They have no seat in the G10 or IMF negotiations that design the packages and no exit from their own currency.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, deficit_country_populations, payer,
    powerless, immediate, trapped, national).

% Households and institutions holding dollar-denominated deposits and bonds as US inflation accelerates from about one percent in the early 1960s toward six percent by 1970. The anchor's promise of price stability erodes under the reserve issuer's deficits; they cannot reprice existing nominal holdings and bear the erosion directly.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, dollar_inflation_savers, payer,
    moderate, biographical, constrained, national).

% Traders in currencies and gold who bet against parities they judge unsustainable. The peg system hands them one-way bets — limited loss when a defense succeeds, large gain when a devaluation comes — and the control machinery (Interest Equalization Tax, credit restraint programs, gold pool operations) exists to blunt them. They have no seat in any governance forum; their only voice is their positioning, which every crisis meeting of the 1960s was convened to counter.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, currency_speculators, excluded,
    powerful, immediate, arbitrage, global).

% Administers the par-value system: registers parities, lends against conditionality to deficit members, and certifies adjustment programs. Its staff and mandate are constituted by the par-value Articles themselves; through the 1960s it patches the system (SDR creation in 1968, standby arrangements) rather than declaring the kernel broken, and when the par-value system ends the institution must reinvent its purpose rather than simply close.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, imf_par_value_administration, agenda_setter,
    institutional, generational, identity_locked, global).

% Academic economists — Friedman's Chicago group foremost — who argue from 1953 onward that the adjustment mechanism is already broken: pegs held by controls rather than price flexibility. They publish the case for floating, diagnose the dollar glut and the reserve-currency logic years before the closure, and hold no enforcement power and no rents.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, monetarist_critics, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__overdetermined_composite_reading, us_treasury_reserve_authority).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__overdetermined_composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interwar problem: fixed-but-adjustable parities eliminate competitive devaluation cycles; the dollar-gold link supplies international liquidity and a common price anchor; the IMF supplies short-term adjustment credit so temporary deficits need not force deflation; trade expands without exchange-rate risk.
% TRANSFER_FUNCTION: Transfers seigniorage and real resources from foreign reserve holders to the reserve issuer — the United States finances external deficits, including the Vietnam War, with dollar liabilities that peg-holding central banks are compelled to accumulate — and transfers the adjustment burden onto deficit countries' populations through deflationary packages, while granting member governments fiscal freedom under the adjustable peg.
% ABSENT_VOICES: Floating-rate advocates and speculators have no governance seat — the G10, IMF, and gold pool forums contain only officials committed to parities; surplus-country households absorbing imported inflation are unrepresented in the gold pool's operations; and no democratic body ever ratifies gold's monetary role, which central bankers defend alone.
% DISAPPEARANCE_RATIONALE: After the window closes in August 1971 the world rearranges within two years: parities float, the Smithsonian and Jamaican settlements formalize the float, capital controls unwind over the following decade, inflation accelerates across the OECD, and the eurodollar market becomes the system's true liquidity mechanism — every arrangement that depended on the anchor is rebuilt around its absence.
% FOUNDING_PROBLEM: Interwar monetary chaos: the gold standard's collapse in the Depression, the competitive devaluations and beggar-thy-neighbor currency wars of the 1930s, and the trade destruction they caused. Bretton Woods (1944) was designed to deliver exchange-rate stability and international liquidity without the old gold standard's deflationary straitjacket.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Friedman's 1953 case for flexible rates and the Chicago critique attested the adjustment mechanism was being replaced by controls; Rueff and the French treasury's 1965 convertibility attack attested, from outside the US seat, that the reserve issuer's discipline had lapsed; Triffin's own 1960 congressional testimony and later economic historiography (Eichengreen, Bordo, James) date the system's functional exhaustion to the late 1960s. No party outside the arrangement's beneficiaries defends the claim that the founding problem was still live in 1971 — and the closure itself is the agenda-setter's implicit admission.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__overdetermined_composite_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.76 (interval end): by the late 1960s the arrangement moved real resources to the reserve issuer — the US financed Vietnam-era external deficits with dollar liabilities that peg-holding central banks were compelled to accumulate, and the 1971-73 devaluations realized the loss on those holdings — while the Articles' adjustment burden fell on deficit countries and never on the issuer. Suppression is 0.72: persistence required an enforcement apparatus built up over two decades — the London Gold Pool (1961), the Interest Equalization Tax (1964), voluntary then mandatory foreign credit restraint (1965-68), BIS swap networks, IMF conditionality — because participant preference no longer held the system together. Theater is 0.58: after the March 1968 two-tier gold settlement the official $35 price survived only as a central-bank-only fiction while the London market priced gold above $40; discipline was performed rather than practiced — but the pre-1965 functions (trade expansion under stable parities, adjustment credit that worked for temporary deficits) were real, so theater is high without being dominant. Accessibility_collapse is 0.45: alternatives never fully closed — floating rates had been argued since Friedman (1953), revaluation and SDR creation were live options, and the system's end demonstrates the alternative became accessible the moment pressures converged; the anchor held through enforcement, not through the absence of alternatives. Resistance is 0.62: de Gaulle's 1965 convertibility demand, the Rueff critique, sterling crises (1966-67), German revaluations (1961, 1969), and successive speculative runs constitute sustained, organized resistance from both states and markets. The dynamic is a ratchet, not a cycle: each crisis (the 1960 gold run, the 1968 pool collapse) permanently raised enforcement intensity and theater; suppression and theater rise monotonically with a step at 1968. Suppression here is structural throughout — capital controls and credit restraint — with no internalized component, so no interpersonal suppression-ambiguity omega arises. Deficit-country populations had no cross-border coalition mechanism; their resistance ran through domestic politics, where it fed the very Keynesian consensus that accelerated the anchor's dissolution — the constraint's weakest seat thus contributed to its end rather than to its defense.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the trapped payer seats compute different arrangements from the same structure. From the US Treasury's position the system is architecture it built and a privilege it enjoys: it sets the gold price, supplies the world's liquidity, and is the only member exempt from the deflationary adjustment its own Articles prescribe. From the foreign central banks' position the same structure is compelled financing: they accumulate dollars to hold their pegs, may not dump them without breaking those pegs, and watch the reserves' value melt at the devaluations they were told would never come. Among formally equal member governments, power diverges with reserve position: surplus countries (Germany) could revalue or accumulate; deficit countries faced conditionality and deflation; the US alone settled in its own paper — equal formal standing, radically different exit. The IMF seat is identity-locked to the par-value kernel — its mandate is the Articles — and patches the system (SDRs 1968, standbys) rather than declaring it broken, which is why the formal end arrives from Washington, not from the administrator. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The US reserve authority sits at the beneficiary end: it collects seigniorage, sets the rules, and holds arbitrage-grade exit — the option to settle in its own currency that no other member has. Member finance ministries sit low: stable-rate trade, IMF credit, and devaluation-as-escape-valve are real gains, though the deficit/surplus split differentiates within the class. Foreign central banks sit at the target end: trapped by their own pegs into compelled dollar accumulation, bearing devaluation risk they did not choose. Deficit-country populations and dollar-inflation savers sit high: they bear the deflationary packages and the nominal-erosion costs directly, with no exit. Surplus-country governments bear imported inflation and revaluation pressure — targets, though powerful ones with partial exit through revaluation. Fixed-rate exporters are genuine coordination beneficiaries. The excluded speculative seat is genuinely two-sided — the peg's rigidity hands speculators one-way bets while the control machinery taxes them — and no directionality override is authored because the net position is honestly near-symmetric; an omega records the question. The beneficiary/victim declarations map to real flows: seigniorage to Washington, adjustment to London and the deficit periphery, devaluation losses to every central bank holding dollars in August 1971.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar chaos, competitive devaluation, dollar shortage — was solved or superseded by the late 1950s: convertibility was restored (1958), trade rebuilt, and the dollar shortage became a dollar glut. The arrangement then persisted thirteen more years on enforcement and theater, which is the zombie signature: founding_problem_status dead paired with disappearance_verdict world_rearranges flags the capture/zombie condition, cross-checked against the rising theater series. The classification prevents two mislabels: reading the late-1960s system as pure extraction would erase the real coordination delivered 1944-1965 (trade expansion under stable parities, adjustment credit that worked when deficits were temporary); reading it as pure coordination would erase the seigniorage transfer and the asymmetric adjustment that financed another country's war. The entangled form captures both halves, and the overdetermined reading explains the absence of an orderly fix: each stream's removal required addressing the others — no single lever (gold revaluation alone, controls alone, fiscal austerity alone) could hold — which is why the mandate outlived its function without a managed sunset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_index_overdetermined,
    'This constraint is one reading of the monetary_anchor_principle kernel (overdetermined_composite_reading): how would instantiating punctuated_swap_reading or triffin_inevitability_reading change the structural classification of the same transition?',
    'Generate the sibling stories and compare: the punctuated reading relocates agency to the August 1971 decision (lower epsilon on the pre-1971 arrangement, contingency restored); the Triffin reading reduces the causal set to the reserve-currency dilemma alone (single-mechanism epsilon). Classification divergence across the family is the measurement.',
    'If the punctuated reading computes with low extraction on the pre-1971 arrangement, or the Triffin reading computes as near-mountain single-stream inevitability, this reading''s tangled_rope claim and high epsilon are reading-indexed rather than topic-indexed — the corpus must not merge the three into one verdict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_index_overdetermined, conceptual, 'Committer structure: this story is one of three sibling readings of the monetary-anchor kernel; the others are separate constraints with their own epsilon.').

omega_variable(
    counterfactual_joint_sufficiency,
    'Were the four structural streams jointly sufficient — could removing any one (no Vietnam escalation; capital controls that actually bound; a gold-price revaluation in 1965; a fiscal-consensus break) have preserved the anchor past 1971?',
    'Counterfactual economic history: model global gold flows and US reserve coverage under each single-stream removal; test whether any single-lever path keeps gold coverage of dollar liabilities above the confidence threshold through the 1970s.',
    'If one stream''s removal suffices, the collapse is contingent and epsilon on the standing arrangement falls — the anchor was recoverable and closer to coordination-with-policy-failure than entangled extraction; if none suffices, overdetermination and the high epsilon stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_joint_sufficiency, empirical, 'Whether the composite was jointly sufficient or any single lever could have held the anchor.').

omega_variable(
    victim_abstraction_mapping,
    'The reading''s own framing names ''monetary discipline'' as the victim — an abstraction. Does the actor-mapped victim set (reserve holders, adjustment bearers, inflation-exposed savers) capture the harm the reading intends, or does the dominant harm land only after 1971, outside epsilon''s referent?',
    'Measure inflation-tax incidence and reserve-devaluation losses across 1965-1974 and attribute them to the standing arrangement versus the successor float; if most realized harm is post-closure, reattribute it to the successor regime''s stories.',
    'If reserve-devaluation losses dominate (realized at the 1971-73 devaluations of compelled dollar holdings), the actor mapping holds and high epsilon stands; if post-collapse inflation dominates, part of this story''s epsilon belongs downstream and the standing arrangement''s epsilon falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_abstraction_mapping, conceptual, 'Whether the reading''s abstract victim (monetary discipline) maps to actors inside the standing arrangement.').

omega_variable(
    late_period_theater_or_function,
    'After the March 1968 two-tier gold settlement the anchor was functionally dead, yet the system persisted three more years — did the swap-network and IMF machinery still deliver coordination value, or was late-period maintenance pure performance?',
    'Isolate the trade-financing and liquidity value of BIS swap lines and IMF standbys in 1968-1971 from the gold anchor''s defense operations; if the machinery cleared trade independently of the anchor, function persisted.',
    'If function persisted, theater_ratio is overstated at the end and the end-state remains a working hybrid rather than decaying toward inertial maintenance; if not, the constraint was already mostly performance by 1971 and the inertial reading of the final phase gains force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(late_period_theater_or_function, empirical, 'Whether 1968-1971 maintenance was theatrical or still functional.').

omega_variable(
    speculator_net_position,
    'The excluded speculative seat is genuinely two-sided: the peg''s rigidity handed speculators one-way devaluation bets while the control machinery (Interest Equalization Tax, credit restraint) taxed their arbitrage — is their net structural position beneficiary-side, target-side, or symmetric?',
    'Aggregate speculative profit-and-loss across the 1960s crises (1960 gold run, 1967 sterling, 1969 DM revaluation, 1971 dollar) net of control costs; the sign and magnitude settle the side.',
    'If speculators net out as beneficiaries, the enforcement machinery was subsidizing them via one-way bets and the discipline was partly theater; if as targets, the controls were suppression of the system''s only price-discovery mechanism and effective extraction on that seat is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speculator_net_position, empirical, 'Net structural position of the excluded speculative seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(mone_tr_t0, observed).
narrative_ontology:measurement(mone_tr_t4, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement_basis(mone_tr_t4, observed).
narrative_ontology:measurement(mone_tr_t8, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement_basis(mone_tr_t8, observed).
narrative_ontology:measurement(mone_tr_t12, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement_basis(mone_tr_t12, observed).
narrative_ontology:measurement(mone_tr_t16, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement_basis(mone_tr_t16, observed).
narrative_ontology:measurement(mone_tr_t20, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(mone_tr_t20, observed).
narrative_ontology:measurement(mone_tr_t24, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement_basis(mone_tr_t24, observed).
narrative_ontology:measurement(mone_tr_t27, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 27, 0.58).
narrative_ontology:measurement_basis(mone_tr_t27, observed).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(mone_be_t0, observed).
narrative_ontology:measurement(mone_be_t4, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement_basis(mone_be_t4, observed).
narrative_ontology:measurement(mone_be_t8, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement_basis(mone_be_t8, observed).
narrative_ontology:measurement(mone_be_t12, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement_basis(mone_be_t12, observed).
narrative_ontology:measurement(mone_be_t16, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement_basis(mone_be_t16, observed).
narrative_ontology:measurement(mone_be_t20, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(mone_be_t20, observed).
narrative_ontology:measurement(mone_be_t24, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement_basis(mone_be_t24, observed).
narrative_ontology:measurement(mone_be_t27, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 27, 0.76).
narrative_ontology:measurement_basis(mone_be_t27, observed).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t0, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(mone_su_t0, observed).
narrative_ontology:measurement(mone_su_t4, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 4, 0.25).
narrative_ontology:measurement_basis(mone_su_t4, observed).
narrative_ontology:measurement(mone_su_t8, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement_basis(mone_su_t8, observed).
narrative_ontology:measurement(mone_su_t12, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 12, 0.32).
narrative_ontology:measurement_basis(mone_su_t12, observed).
narrative_ontology:measurement(mone_su_t16, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement_basis(mone_su_t16, observed).
narrative_ontology:measurement(mone_su_t20, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(mone_su_t20, observed).
narrative_ontology:measurement(mone_su_t24, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement_basis(mone_su_t24, observed).
narrative_ontology:measurement(mone_su_t27, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 27, 0.72).
narrative_ontology:measurement_basis(mone_su_t27, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, resource_allocation).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, triffin_inevitability_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, post_1971_floating_regime).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, eurodollar_market_expansion).

% DUAL FORMULATION NOTE:
% Constraint family: three readings of the monetary_anchor_principle kernel — this story (overdetermined_composite_reading), punctuated_swap_reading, and triffin_inevitability_reading — decompose the single colloquial label 'the end of Bretton Woods' into three structurally distinct claims with distinct epsilon. This reading dates functional death to the late 1960s via four jointly-sufficient streams and authors high epsilon on the standing arrangement; the punctuated reading relocates agency to a single August 1971 decision; the Triffin reading reduces the causal set to the reserve-currency dilemma alone. Each story links the others here; epsilon is authored per reading, never averaged across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
