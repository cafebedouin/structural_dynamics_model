% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Bretton Woods Substrate — Neoliberal Convertibility Reading
 *   domain: international political economy / monetary history / institutional design
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Bretton Woods treaty
 *   substrate: the neoliberal_convertibility reading, under which the
 *   arrangement constrains government intervention in capital flows so that
 *   free capital markets can operate, capital controls count as violations
 *   rather than tools, and national policy autonomy sits on the paying side
 *   while international finance sits on the collecting side. The same treaty
 *   articles sustain two sibling constraints authored elsewhere:
 *   keynesian_embedded_liberalism (the arrangement constrains international
 *   capital to protect domestic policy space — controls are tools) and
 *   sovereignty_defense (the arrangement constrains external monetary
 *   discipline to preserve national monetary sovereignty). The three readings
 *   carry materially different epsilon values over the same referent
 *   arrangement: the Keynesian reading authors epsilon against mobile capital
 *   as target; the sovereignty reading authors epsilon against external
 *   discipline as target; this reading authors epsilon for the standing
 *   liberalization-and-conditionality arrangement with national policy
 *   autonomy as its declared victim class. The stories are linked through
 *   network.affects_constraints as one constraint family; the divergence
 *   among them is the corpus datum, not an error to be reconciled.
 *
 * KEY AGENTS:
 *   - - international_finance: Primary beneficiary (powerful/arbitrage) — collects mobility, risk-shifting, and bailout backstops
 *   - - us_reserve_currency_issuer: Dual-positioned beneficiary and enforcer (institutional/arbitrage) — collects seigniorage and discipline leverage it does not itself bear
 *   - - imf_surveillance_apparatus: Agenda setter (institutional/identity_locked) — administers surveillance, conditionality, and authoritative interpretation
 *   - - deficit_debtor_governments: Primary payer (moderate/trapped) — surrenders policy autonomy under conditionality and market discipline
 *   - - nonreserve_central_banks: Payer with fused professional identity (organized/identity_locked) — maintains orthodoxy against domestic conditions
 *   - - austerity_state_electorates: Payer and excluded voice (powerless/trapped) — bears program costs without setting terms
 *   - - capital_control_advocates: Excluded voice (moderate/constrained) — proposes measures the framework classes as breaches
 *   - - ipe_monetary_historians: Analytical observer (analytical/analytical) — holds the archival record the readings contest over
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.58).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.61).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.39).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.39).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods Substrate — Neoliberal Convertibility Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international political economy / monetary history / institutional design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, '51ee4139-4a48-4237-a4b0-6d1b6722daf5').
narrative_ontology:cs_kernel_codification('51ee4139-4a48-4237-a4b0-6d1b6722daf5', formalized).
narrative_ontology:cs_authority_grounding('51ee4139-4a48-4237-a4b0-6d1b6722daf5', extraction).
narrative_ontology:cs_interpretation_layer_present('51ee4139-4a48-4237-a4b0-6d1b6722daf5').
narrative_ontology:cs_reading_relation('51ee4139-4a48-4237-a4b0-6d1b6722daf5', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, forecloses).
narrative_ontology:cs_reading_relation('51ee4139-4a48-4237-a4b0-6d1b6722daf5', bretton_woods_treaty_substrate__sovereignty_defense, forecloses).
narrative_ontology:cs_axiom('51ee4139-4a48-4237-a4b0-6d1b6722daf5', foundational, capital_mobility_presumptive_right).
narrative_ontology:cs_axiom_status(capital_mobility_presumptive_right, holdable).
narrative_ontology:cs_axiom_grounding('51ee4139-4a48-4237-a4b0-6d1b6722daf5', capital_mobility_presumptive_right, instrumental).
narrative_ontology:cs_axiom('51ee4139-4a48-4237-a4b0-6d1b6722daf5', foundational, policy_autonomy_yields_to_convertibility).
narrative_ontology:cs_axiom_status(policy_autonomy_yields_to_convertibility, holdable).
narrative_ontology:cs_axiom_grounding('51ee4139-4a48-4237-a4b0-6d1b6722daf5', policy_autonomy_yields_to_convertibility, conventional).
narrative_ontology:cs_reference_frame('51ee4139-4a48-4237-a4b0-6d1b6722daf5', open_capital_account_normality).
narrative_ontology:cs_drift_state('51ee4139-4a48-4237-a4b0-6d1b6722daf5', post_2012_imf_institutional_view, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('51ee4139-4a48-4237-a4b0-6d1b6722daf5', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_corporations).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, us_reserve_currency_issuer).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, deficit_debtor_governments).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, nonreserve_central_banks).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, austerity_state_electorates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Global banks, asset managers, currency dealers, and sovereign-bond investors move capital across borders under uniform convertibility rules, price sovereign risk continuously, and rely on official crisis lending to make their emerging-market exposures safe at the margin. Exit is cheap: portfolios relocate to whichever jurisdiction offers better risk-adjusted returns, often within hours, and relocation never forfeits access to the system itself.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance, beneficiary,
    powerful, generational, arbitrage, global).

% Run treasury operations, supply-chain financing, and profit repatriation across dozens of currencies. Convertibility rules and predictable exchange arrangements lower hedging costs and permit internal capital allocation unconstrained by national balance-of-payments management. When national rules turn unfavorable, booking locations and production footprints can be shifted.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_corporations, beneficiary,
    powerful, biographical, arbitrage, global).

% Issues the dominant reserve currency, controls dollar swap-line access, and informally steers the International Monetary Fund. Collects seigniorage, borrows externally in its own currency at privileged rates, and converts financial centrality into sanctions leverage. Its commitments anchor the whole payments system, which insulates it from the adjustment pressures applied to every other participant.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, us_reserve_currency_issuer, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__neoliberal_convertibility, us_reserve_currency_issuer, agenda_setter).

% Administers Article IV surveillance, lends under conditionality, certifies national policy frameworks through technical assistance, and issues the authoritative interpretations of what the treaty articles require as circumstances change. Its budget, staffing, and standing scale with the lending-and-review cycle it administers. Walking away from the mandate would mean dissolving the institution; its self-conception is that it is the neutral steward of the articles rather than an interested party.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_surveillance_apparatus, agenda_setter,
    institutional, generational, identity_locked, global).

% Run persistent external deficits, borrow in currencies they do not issue, and accept fiscal consolidation, privatization schedules, and monetary tightening as loan conditions. Once indebted, continued market access depends on remaining inside the framework; unilateral default or reimposing controls risks funding cutoff, litigation, and years of exclusion from correspondent banking relationships.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, deficit_debtor_governments, payer,
    moderate, biographical, trapped, regional).

% Manage currencies they cannot issue, hold reserves denominated in other states' liabilities, and defend credibility through orthodox policy even when domestic conditions argue for the opposite. Professional standing, peer evaluation, and career trajectories inside the central-banking community reward maintaining that orthodoxy. Leaving the framework means reserve losses, currency crisis, and loss of the professional identity that constitutes the institution.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, nonreserve_central_banks, payer,
    organized, generational, identity_locked, global).

% Bear the domestic consequences of externally negotiated programs: reduced public services, higher taxes, unemployment, and pension cuts. Program terms are negotiated between finance ministries, central banks, and official creditors before electoral processes engage; voters choose among governments after the terms are set, not over the terms themselves. Emigration is the only individual exit, and it is available mainly to the skilled minority.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, austerity_state_electorates, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__neoliberal_convertibility, austerity_state_electorates, excluded).

% Economists, officials, and policymakers who argue for managed capital flows, transaction taxes, or temporary controls on inflows and outflows. Their proposals sit outside the negotiating rooms where program terms and surveillance standards are set, and attempts to act on them are treated as lapses in good standing — though several states have deployed such measures successfully during crises and resumed market access afterward.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_control_advocates, excluded,
    moderate, biographical, constrained, regional).

% Reconstruct the 1944 negotiation record, trace how the treaty articles' operative meaning shifted across successive decades, and test the rival interpretations of the arrangement against archival evidence and outcome data. They collect no lending fees, pay no programs, and administer nothing; their product is the documented record the competing readings cite.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, ipe_monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__neoliberal_convertibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides common convertibility rules, a reserve-asset hierarchy, official crisis lending, and exchange-rate predictability so that cross-border capital can be priced, cleared, and recovered without bespoke negotiation between every pair of states — the underlying payments and liquidity infrastructure of multilateral trade and finance.
% TRANSFER_FUNCTION: Moves policy discretion from national governments — especially deficit states — to official creditors and standard-setting bodies; moves bailout financing from official institutions to distressed debtors conditional on austerity and structural reform; and moves seigniorage, stabilization rents, and risk-shifting benefits toward the reserve issuer and globally mobile capital.
% ABSENT_VOICES: Domestic electorates of program countries are absent from the rooms where terms are set; capital-control advocates are absent from surveillance and standard-setting bodies; and the Keynesian negotiators whose original design explicitly protected the right to control capital movements have no seat in the interpretation the apparatus now enforces.
% DISAPPEARANCE_RATIONALE: If the convertibility-discipline arrangement vanished overnight, reserve portfolios, trade invoicing, swap networks, and sovereign debt contracts would all lose their coordinating frame simultaneously: states would scramble into bilateral clearing arrangements or currency blocs, creditors would reprice or withdraw from cross-border exposure, and the current division of monetary labor between the reserve issuer and everyone else would dissolve into a renegotiated patchwork.
% FOUNDING_PROBLEM: The interwar payments chaos: competitive devaluations, beggar-thy-neighbor trade policy, the collapsed gold standard, and discriminatory bilateral clearing blocks that strangled multilateral trade. The 1944 articles were built to restore stable exchange rates and multilateral payments while — in the original design — explicitly safeguarding each state's right to manage capital movements.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of the interwar period, working outside any benefiting party, abundantly corroborate that the founding problem was real and severe. What no one outside the beneficiary set attests is this reading's specific claim that the live problem today is government intervention impeding free capital markets: that reframing is argued principally by the institutions and industry the arrangement funds, while heterodox economists and several crisis-era governments dispute it from outside.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is substantial but not total (0.58 at interval end) because the arrangement retains real coordination output — crisis lending, reserve pooling, payments predictability — alongside the autonomy transfer. Suppression (0.61) reflects enforcement that operates through conditionality, surveillance certification, and creditor market discipline rather than border guards; it is a raw structural property and is deliberately not scaled by scope in authoring, though the engine scales effective extraction. Theater (0.39) has risen steadily since the gold anchor and fixed parities died in 1971: mutual-assessment exercises, communiqué discipline, and surveillance rituals increasingly substitute for the original functional core. Accessibility_collapse (0.52) is mid-range because alternatives are stigmatized but demonstrably usable — Malaysia 1998, Iceland 2008, and the IMF's own 2012 institutional view concede that managed capital flows can work. Resistance (0.55) is real and recurring: Asian-crisis backlash, the Chiang Mai Initiative, the New Development Bank, and the partial rehabilitation of capital-flow management all register as active pushback. The suppression_requirement series is authored deliberately rather than defaulted: enforcement capacity was consciously built up from the weak 1944 machinery to the conditionality peak of 1997, then partially receded after successive crises dented the enforcement consensus — that trajectory is the story's enforcement history. All three tracked series share one eight-point grid (1944–2024) so the engine samples a complete row at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural facts. From the international_finance and reserve-issuer seats, the arrangement presents as the infrastructure that makes global capital markets possible — coordination-heavy, worth defending, with the discipline imposed on others experienced as background order rather than cost. From the deficit-debtor-government and electorate seats, the same structure presents as a machine that converts domestic policy autonomy into creditor protection — extraction-dominated, with the coordination benefits remote and conditional. The IMF seat experiences the arrangement as neutral stewardship and would compute the mildest profile of any participant seat; its identity_locked exit explains why the neutrality frame is maintained even as its own research department documents the legitimacy of the tools its programs penalize. The engine computes these divergent per-seat classifications from the structural data; this story's claimed type adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   International finance and multinational corporations derive directionality near the beneficiary pole: they appear in the beneficiaries array with arbitrage-grade exit, so effective extraction inverts toward subsidy for them. The US reserve issuer also derives low d despite its enforcement role — it collects the largest single stream of gains (seigniorage, privileged borrowing, leverage) while being structurally immune to the discipline it administers. Deficit-debtor governments derive high d: declared victims, trapped exit once indebted. Nonreserve central banks sit nearest the full-target end because identity_locked exit amplifies their d — they cannot exit without dissolving the professional identity that constitutes them. Austerity-state electorates derive high d with minimal power, which is precisely the coalition-risk configuration the analysis should flag: individually powerless, they bear concentrated costs that are collectively enormous. The IMF apparatus derives a middling d from its coordinator role; its situation text records the capture dynamic (budget and standing scale with the lending cycle) so downstream analysis can weigh whether its effective position is more interested than the neutrality claim suggests.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar payments chaos — was substantially solved by the 1960s, and the arrangement's original functional core (gold anchor, fixed parities, scarce-currency clause) has been dead since 1971. What persists is a reframed mandate enforced by a growing interpretive and conditional apparatus, with theater_ratio climbing from 0.12 to 0.39 across the interval as performative surveillance substitutes for the departed functions. The classification discipline matters in both directions here: declaring the genuine coordination residue (crisis lending, reserve pooling) prevents mislabeling the arrangement as pure extraction, while declaring the victims and enforcement machinery prevents the beneficiary framing from laundering the autonomy transfer as mere technical coordination. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: the parties genuinely dispute whether the live problem is government intervention (this reading), volatile capital (the Keynesian sibling), or external discipline (the sovereignty sibling), so no zombie flag fires — but the contested status itself is the signal that the mandate survives by reinterpretation rather than by its founding function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates only the neoliberal_convertibility reading of the bretton_woods_treaty_substrate kernel — are the sibling readings (keynesian_embedded_liberalism, sovereignty_defense) distinct constraints with their own epsilon values and victim sets, rather than corrections to this one?',
    'Comparative classification across the three family stories: if each reading yields a stable, internally consistent classification over the same referent arrangement with different victim sets, the readings are distinct constraints; if one reading''s structural data subsumes the others without remainder, the decomposition was spurious.',
    'Adopting the keynesian sibling moves international finance from beneficiary to target and removes national policy autonomy from the victim set; adopting the sovereignty sibling replaces both with external-discipline bodies as target. Classification of this story is valid only within this reading''s commitments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: one reading of a contested kernel, siblings are other files.').

omega_variable(
    capital_controls_tool_or_violation,
    'Is the normative status of capital controls — violation versus legitimate tool — resolvable within this reading, or is it the irreducible location of the kernel dispute?',
    'Track whether crisis deployments of controls (Malaysia 1998, Iceland 2008, Greece-era debates) are absorbed into the apparatus''s interpretation as exceptions or force formal revision of the liberalization norm; the IMF''s 2012 institutional view is the leading indicator.',
    'If controls are rehabilitated as tools, this reading''s victim set collapses and the constraint migrates toward the keynesian sibling''s structure; if the violation framing hardens, suppression and extraction rise together and the payer seats deepen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_controls_tool_or_violation, conceptual, 'The located disagreement of the kernel: the normative status of the regulated act.').

omega_variable(
    naturalness_of_open_capital_order,
    'Is free capital mobility the natural default order of international finance (as this reading''s framing presupposes), or a constructed post-1980s overlay on an arrangement whose 1944 design explicitly protected the right to control capital?',
    'Archival and doctrinal comparison: the 1944 negotiation record, the transitional Article XIV period, and the sequencing of liberalization pressure through the OECD codes, the IMF''s jurisdictional evolution, and bilateral investment treaties establish whether openness was designed-in or retrofitted.',
    'If the open-capital order is constructed and late, the reading''s genealogy weakens, the emerges-naturally style claim fails, and the arrangement''s persistence rests on enforcement rather than inevitability — raising the weight of the enforcement-dependent classification path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_open_capital_order, empirical, 'Whether the liberalization norm is natural default or constructed retrofit.').

omega_variable(
    conditionality_stabilization_vs_extraction,
    'Does program conditionality stabilize debtor economies (a genuine coordination service whose costs are the price of liquidity) or transfer losses from creditors to debtor populations (extraction through the lending channel)?',
    'Outcome evaluation of program cohorts against matched non-program counterfactuals: growth, employment, and distributional trajectories under comparable pre-program conditions, decomposing creditor recovery rates from debtor welfare outcomes.',
    'If stabilization dominates, part of the measured extraction is the price of the coordination itself and the payer seats'' effective burden falls; if loss-transfer dominates, the lending channel is an extraction mechanism and the arrangement slides toward the pure-extraction boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_stabilization_vs_extraction, empirical, 'Whether the conditionality channel coordinates or extracts.').

omega_variable(
    market_discipline_vs_treaty_enforcement,
    'How much of the observed suppressive force on national policy autonomy is produced by the formal treaty-and-institution machinery versus by private creditor repricing that would persist even if the formal apparatus dissolved?',
    'Natural experiments where formal enforcement lapsed or was refused (defaults followed by rapid market re-access, program rejections followed by autonomous recovery) compared against cases where formal enforcement was decisive.',
    'If market discipline carries most of the load, the formal apparatus''s suppression contribution is smaller than its theater suggests and the piton-side symptoms in the surveillance layer are confirmed; if formal enforcement is decisive, the apparatus is the operative suppressive structure and its capture dynamics matter correspondingly more.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_discipline_vs_treaty_enforcement, empirical, 'Attribution of suppressive force between formal machinery and private market discipline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1944, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1944, 0.12).
narrative_ontology:measurement(bret_tr_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1958, 0.16).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1971, 0.26).
narrative_ontology:measurement(bret_tr_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1985, 0.31).
narrative_ontology:measurement(bret_tr_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1997, 0.37).
narrative_ontology:measurement(bret_tr_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2008, 0.43).
narrative_ontology:measurement(bret_tr_t2016, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2016, 0.41).
narrative_ontology:measurement(bret_tr_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2024, 0.39).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1944, 0.28).
narrative_ontology:measurement(bret_be_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1958, 0.34).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1971, 0.46).
narrative_ontology:measurement(bret_be_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1985, 0.57).
narrative_ontology:measurement(bret_be_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1997, 0.67).
narrative_ontology:measurement(bret_be_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2008, 0.63).
narrative_ontology:measurement(bret_be_t2016, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2016, 0.6).
narrative_ontology:measurement(bret_be_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1944, 0.22).
narrative_ontology:measurement(bret_su_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1958, 0.3).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1971, 0.42).
narrative_ontology:measurement(bret_su_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1985, 0.54).
narrative_ontology:measurement(bret_su_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1997, 0.71).
narrative_ontology:measurement(bret_su_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2008, 0.65).
narrative_ontology:measurement(bret_su_t2016, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2016, 0.62).
narrative_ontology:measurement(bret_su_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2024, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, global_infrastructure).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, sovereignty_defense).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the bretton_woods_treaty_substrate kernel per the epsilon-invariance principle: 'what Bretton Woods constrains' is a colloquial label covering three structurally distinct claims with different binding targets, different victim sets, and materially different epsilon values. This story (neoliberal_convertibility) authors epsilon for the standing liberalization-conditionality arrangement with national policy autonomy as victim; keynesian_embedded_liberalism authors epsilon for the same referent with international capital as target; sovereignty_defense authors epsilon with external monetary discipline as target. Each story links the other two via network.affects_constraints; upstream-downstream citation pressure runs from the archival record (ipe_monetary_historians' domain) toward whichever reading currently holds the interpretive apparatus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
