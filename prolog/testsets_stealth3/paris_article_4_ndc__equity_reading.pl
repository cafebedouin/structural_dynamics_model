% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__equity_reading, []).

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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: Paris Article 4 NDCs — CBDR Equity Reading
 *   domain: international climate governance / treaty law / political economy
 *
 * SUMMARY:
 *   Paris Agreement Article 4 makes nationally determined contributions
 *   self-determined; the equity reading holds that their interpretation must
 *   nevertheless run through Common But Differentiated Responsibilities and
 *   Respective Capabilities (CBDR-RC), requiring structural distinctions
 *   between developed and developing states. Under this reading the operative
 *   arrangement is a differentiated bargain: developed (Annex II) states
 *   carry binding-side mitigation constraints, finance and
 *   technology-transfer obligations, and ratcheting expectations, while
 *   developing states retain policy space, and equity coalitions (G77/China,
 *   LMDC) hold an effective veto over supranational enforcement design
 *   through the COP consensus rule. This file instantiates ONE reading of the
 *   contested kernel paris_article_4_ndc; the sovereigntist and supranational
 *   readings are separate constraints linked via network.affects_constraints,
 *   and this story's epsilon — 0.58, moderate, asymmetrically distributed —
 *   is authored for the CBDR-structured arrangement as this reading itself
 *   assesses it, not for the siblings' arrangements and not averaged across
 *   them. The claimed type (tangled_rope) and the metrics are authored
 *   independently: the claim states the structure I believe true; the metrics
 *   describe the operation I believe observable.
 *
 * KEY AGENTS:
 *   - - g77_developing_coalition: Primary beneficiary and veto holder (organized/constrained) — collects differentiation and blocks enforcement designs it opposes
 *   - - major_emerging_economies: Largest single beneficiary (powerful/arbitrage) — retains policy space and carbon headroom
 *   - - least_developed_countries: Beneficiary dependent on transfers (organized/trapped)
 *   - - small_island_developing_states: Ambivalent beneficiary (organized/trapped) — collects finance, would trade part of the shield for accountability on all major emitters
 *   - - annex_ii_developed_states: Primary payer and co-administrator (institutional/constrained) — bears binding-side constraints and transfer obligations
 *   - - developed_economy_households: Ultimate fiscal payer (moderate/constrained)
 *   - - unfccc_secretariat: Administrator (institutional/constrained) — implements the reading COP consensus supplies
 *   - - supranational_enforcement_advocates: Excluded challenger (organized/constrained)
 *   - - future_generations: Excluded bearer of outcomes (powerless/trapped)
 *   - - climate_treaty_analysts: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.58).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.6).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "Paris Article 4 NDCs — CBDR Equity Reading").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international climate governance / treaty law / political economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, 'a019668e-3e0e-4f2f-b81d-a1a19fa2adbf').
narrative_ontology:cs_kernel_codification('a019668e-3e0e-4f2f-b81d-a1a19fa2adbf', fixed_text).
narrative_ontology:cs_authority_grounding('a019668e-3e0e-4f2f-b81d-a1a19fa2adbf', lineage).
narrative_ontology:cs_interpretation_layer_present('a019668e-3e0e-4f2f-b81d-a1a19fa2adbf').
narrative_ontology:cs_reading_relation('a019668e-3e0e-4f2f-b81d-a1a19fa2adbf', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a019668e-3e0e-4f2f-b81d-a1a19fa2adbf', paris_article_4_ndc__supranational_reading, influences).
narrative_ontology:cs_axiom('a019668e-3e0e-4f2f-b81d-a1a19fa2adbf', foundational, structural_developed_developing_distinction_required).
narrative_ontology:cs_axiom_status(structural_developed_developing_distinction_required, holdable).
narrative_ontology:cs_axiom_grounding('a019668e-3e0e-4f2f-b81d-a1a19fa2adbf', structural_developed_developing_distinction_required, deontological).
narrative_ontology:cs_axiom('a019668e-3e0e-4f2f-b81d-a1a19fa2adbf', secondary, right_to_development_precedes_symmetric_mitigation).
narrative_ontology:cs_axiom_status(right_to_development_precedes_symmetric_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('a019668e-3e0e-4f2f-b81d-a1a19fa2adbf', right_to_development_precedes_symmetric_mitigation, deontological).
narrative_ontology:cs_reference_frame('a019668e-3e0e-4f2f-b81d-a1a19fa2adbf', cbdr_structured_differentiation_regime).
narrative_ontology:cs_drift_state('a019668e-3e0e-4f2f-b81d-a1a19fa2adbf', contemporary_post_paris_implementation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a019668e-3e0e-4f2f-b81d-a1a19fa2adbf', '2026-08-20T12:00:00Z').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, g77_developing_coalition).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, major_emerging_economies).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, small_island_developing_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, annex_ii_developed_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_economy_households).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, cbdr_rc_principle).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, historical_responsibility_attribution).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, climate_justice_differential_duties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The negotiating bloc of 130+ developing states insists that NDC interpretation run through CBDR-RC with structural developed/developing distinctions. Finance eligibility, technology-transfer claims, and policy space flow to its members, and it exercises an effective veto over enforcement design through the COP consensus rule, blocking accountability language it reads as eroding differentiation. Leaving would mean abandoning the differentiation shield the bloc exists to defend, so it stays and litigates the text COP after COP.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, g77_developing_coalition, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, g77_developing_coalition, agenda_setter).

% The LDC Group coordinates as a bloc but its members carry little individual leverage. They receive adaptation finance, capacity-building flows, and the lightest differentiation tier, and they depend on those flows and on their regime voice; exit would cost them both without reducing their exposure. They defend structural differentiation as the regime's fairness core while pressing for the finance to actually arrive.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, least_developed_countries, beneficiary,
    organized, generational, trapped, regional).

% Large developing emitters retain policy space under structural distinction: no binding mitigation constraint, continued atmospheric headroom as developed emissions ratchet down, and eligibility for finance and technology flows. They hold bilateral leverage outside the treaty and move between defending the equity frame in the COP and striking great-power deals outside it. Accepting the supranational sibling's symmetric obligations is the exit they currently avoid.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, major_emerging_economies, beneficiary,
    powerful, generational, arbitrage, global).

% AOSIS members collect finance and loss-and-damage flows and benefit from developed-state obligations, but their position is double-edged: they press for maximal ambition and have repeatedly accepted accountability language that applies to all major emitters, so the veto that shields large developing emitters does not fully speak for them. Exit is physically unavailable to them — impacts follow regardless of what the treaty says.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, small_island_developing_states, beneficiary,
    organized, generational, trapped, regional).

% OECD donors and the EU carry the binding side of the bargain: ratcheting mitigation expectations toward net-zero, quantified finance and technology-transfer obligations, and the reporting burden of the transparency framework, while co-administering the regime through COP presidencies, technical bodies, and secretariat funding. Formal withdrawal exists — one party used it and returned — but carries diplomatic cost, so they work the text and the consensus floor instead, contesting the binary distinction's fitness and the scale of the finance ask.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, annex_ii_developed_states, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, annex_ii_developed_states, agenda_setter).

% Taxpayers and energy consumers in developed states ultimately fund the public-finance transfers and absorb mitigation costs through prices, industrial adjustment, and fiscal choices. They touch the constraint only indirectly, through domestic politics that constrains what their governments can pledge and deliver, and they cannot exit the obligation their governments accepted.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_economy_households, payer,
    moderate, biographical, constrained, national).

% Administers NDC submission, synthesis, and the transparency framework, and implements whatever differentiation reading COP consensus supplies. Its function and budget depend on the regime continuing, so it smooths interpretive disputes rather than forcing them, and it has no life outside the arrangement it services.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% Parts of the European Parliament, climate litigation movements, and transnational NGOs hold that self-determined pledges without international accountability cannot deliver 1.5C. The consensus rule plus the equity veto keeps binding-accountability proposals off the decision floor, so they operate through advisory opinions, domestic courts, and side declarations at the regime's margins.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, supranational_enforcement_advocates, excluded,
    organized, generational, constrained, global).

% People not yet born bear the consequences of whatever ambition level the differentiated bargain produces. They hold no seat in the COP and no vote in the consensus that sets differentiation, and nothing in the arrangement gives them voice or exit.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).

% Legal scholars and regime analysts track how differentiation has evolved across COP decisions and who is coordinated and who pays under each reading of Article 4. They collect no rents and bear no obligations under the constraint.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, climate_treaty_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__equity_reading, major_emerging_economies).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__equity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the universal-participation problem: climate stabilization requires every major emitter inside one regime, and the Kyoto record showed that symmetric binding commitments drive large-emitter exit and ratification failure. CBDR-structured differentiation is the bargain that bought near-universal membership — developed-state obligations and finance in exchange for developing-state participation — and it coordinates trust by making burden-sharing track declared responsibility and capability.
% TRANSFER_FUNCTION: Moves mitigation burden and public finance from developed states toward developing states: quantified finance pledges (USD 100 billion per year, re-scaled at USD 300 billion under the NCQG), technology transfer, capacity building, and — the largest implicit transfer — atmospheric carbon space, via timelines that let developing-country emissions grow while developed-country emissions ratchet down.
% ABSENT_VOICES: Supranational enforcement advocates would bind all major emitters with international accountability and are kept off the decision floor by the consensus rule the equity veto operates through. Future generations hold no seat at all. Inside the developing-state blocs, voices willing to trade policy space for finance-plus-accountability (some AOSIS and LDC positions) are outvoted by the LMDC line that speaks for the coalition.
% DISAPPEARANCE_RATIONALE: If CBDR-structured interpretation vanished overnight, developing-state parties would treat the regime's bargain as broken — differentiation is the consideration they entered for. Finance flows would be renegotiated or withheld, major emitters would reassess participation, and the regime would reorganize around either the sovereigntist reading (pledges without structure) or the supranational reading (binding targets with accountability); either way the current allocation of burden and carbon space rearranges.
% FOUNDING_PROBLEM: Post-Kyoto regime design: how to bring universal participation to climate mitigation when historical responsibility and capability are radically asymmetric and the Kyoto model — binding developed-state targets with no developing-state obligations — had failed to cover the majority of world emissions growth.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the developing-state beneficiary set: developed-state negotiators' contemporaneous accounts record that the US and EU accepted differentiation explicitly to secure universal coverage after Copenhagen's failure; the international environmental law literature treats CBDR as the price of universality; and the UNFCCC negotiation history from the Bali Action Plan through Paris documents the bargain. The parties dispute whether the problem still has its original shape — not what the arrangement was built for; no party claims the participation problem was never the founding problem.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__equity_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.58) because the arrangement couples a real coordination function — universal participation, the explicit lesson of Kyoto's symmetric-binding failure — with a substantial one-way transfer: quantified finance obligations, technology-transfer duties, and binding-side mitigation constraints fall on developed states while developing states take on no symmetric constraint. Suppression (0.60) is authored as a raw structural property and is not scaled by power or scope — the engine scales only extractiveness: the arrangement persists because the consensus rule lets equity coalitions block supranational enforcement alternatives and because developed-state exit carries costs that hold parties in place; the coercion here is procedural lock-in, not force. Theater ratio (0.40) reflects a real transfer and transparency machinery increasingly wrapped in reaffirmation rhetoric: delivered finance persistently trails pledges, and ambition-gap language has become a performative genre. Accessibility collapse is low-moderate (0.45) because both sibling readings remain live, organized alternatives — the constraint blocks them procedurally rather than rendering them unthinkable. Resistance (0.60) is high because the payer bloc actively contests the binary distinction's continuing fitness and the scale of its obligations, COP after COP. The measurement series share one nine-point grid (t=0 is 1992, t=33 is 2025) across all three tracked metrics. Trajectories: extraction jumps at Kyoto's binding asymmetry, dips as Kyoto's coverage narrowed, then rises again as Paris-era finance obligations quantified and grew; theater dips at Paris's real bargain and climbs with pledge inflation; suppression_requirement rises post-Paris as the constraint's enforcement work shifts from administering differentiation to defending it against the supranational alternative. Values smooth over the two-to-four-year COP negotiation cycle; the cycle's reaffirmation-contest oscillation is averaged into the trend rather than resolved into a separate cyclical series here.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the G77/LDC seat the arrangement is the regime's legitimacy core: differentiation is the consideration for participation, and the veto is what keeps the bargain enforceable against erosion. From the Annex II seat the same structure is an open-ended obligation whose enforcement others can block and whose scope others help define — coordination experienced as conscription. Major emerging economies experience it as a shield with option value: they defend the equity frame while holding bilateral leverage outside it. Small island states are the ambivalent seat: they collect the finance and benefit from developed-state obligations, but they would trade part of their shield for accountability applied to all major emitters, which is why the veto they nominally benefit from does not fully speak for them. The payer bloc is not powerless — Annex II states coordinate (EU, Umbrella Group) and can force agenda items on differentiation's evolution, which is why resistance rather than exit is their observed behavior. The engine computes per-seat classifications from these structural positions; this commentary explains the asymmetry, it does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the G77 coalition, LDCs, SIDS, and major emerging economies receive the constraint's flows (finance eligibility, policy space, carbon headroom) and bear no symmetric obligation, so they derive near the beneficiary end — emerging economies nearest of all, since arbitrage-grade exit options (bilateral deals, bloc-switching leverage) sit at the extreme low end. Annex II developed states and their households map to high directionality: they bear the transfer obligations and binding-side constraints, with exit limited to costly withdrawal. The UNFCCC secretariat declares no beneficiary or victim position and falls to the canonical fallback near symmetric. No directionality overrides are authored: the derivation from beneficiary/victim data plus exit options captures every seat's relationship to this constraint, including the G77's agenda-setting secondary role, which shapes enforcement design without changing who the constraint extracts from.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — universal participation under radical asymmetry of responsibility and capability — is contested, not dead, so no mandatrophy resolution is declared. The tangled_rope classification is what prevents mislabeling in both directions: a rope reading would erase the asymmetric extraction (the one-way transfer and the veto shield over enforcement), while a snare reading would erase the genuine coordination achievement (near-universal membership that no symmetric design has ever produced). The classification stays honest about the hybrid: coordination and extraction run through the same structure and require active enforcement to hold. The binary-distinction omega tracks the mandatrophy-adjacent risk: if capability convergence makes the developed/developing binary fail the reading's own justificatory distribution, the coordination function thins while the shield persists — at that point the arrangement would drift toward snare (differentiation as entrenched interest) and the founding problem would be dead in its original form while the arrangement persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the equity reading of the paris_article_4_ndc kernel; how would the beneficiary/victim structure and epsilon shift under the sovereigntist or supranational sibling readings?',
    'Generate and compare the sibling reading stories (paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__supranational_reading) against this one; the disagreement is located in whether Article 4 interpretation is purely voluntary, CBDR-structured, or symmetrically binding with international accountability.',
    'Under the sovereigntist reading the transfer and binding structure dissolves (epsilon falls, the victim set empties); under the supranational reading obligations symmetrize (the victim set expands to major developing emitters and the equity veto disappears). This story''s metrics are valid only for the equity reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame omega: this constraint is one reading of a contested treaty kernel; sibling readings instantiate different constraints.').

omega_variable(
    binary_distinction_fitness,
    'Does the binary developed/developing structural distinction still track the underlying distribution of historical responsibility and capability, given that major developing emitters now lead annual emissions and several exceed developed-state income levels?',
    'Updated responsibility-capability indices and graduated-differentiation proposals (evolving Annex assignments, income-and-emissions-threshold graduation) tested against actual COP negotiation behavior.',
    'If the binary no longer tracks, the equity reading''s constraint increasingly shields states its own justice premise no longer covers; the extraction asymmetry inverts toward the powerless within developing states and the constraint drifts from tangled_rope toward snare (differentiation as entrenched interest).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binary_distinction_fitness, empirical, 'Whether the structural distinction the reading requires still tracks its own justificatory distribution.').

omega_variable(
    equity_veto_capture_ambiguity,
    'Does the equity coalition''s veto over enforcement design protect the participation bargain, or has it been captured by major emitters using equity framing to avoid obligations the most vulnerable would accept?',
    'Compare LMDC veto positions against AOSIS and LDC positions on accountability measures across COP decisions; if the most vulnerable blocs would accept enforcement the veto blocks, capture is indicated.',
    'If captured, the veto is an extraction-shielding mechanism rather than a coordination-preserving one; the constraint''s coordination function thins relative to its extraction and reclassification pressure toward snare follows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_veto_capture_ambiguity, empirical, 'Whether the equity veto is coordination-preserving or captured by the largest shielded emitters.').

omega_variable(
    finance_delivery_decay,
    'Are developed-state finance and technology-transfer obligations being delivered at the level the equity reading''s constraint requires, or is delivery decaying into pledge rhetoric?',
    'OECD and UNFCCC Standing Committee on Finance accounting: delivered versus pledged flows, grant-versus-loan composition, adaptation share.',
    'Sustained shortfall raises theater_ratio and models drift toward piton (theatrical reaffirmation of differentiation without functioning transfer); delivery at scale supports the tangled_rope reading with a real transfer function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(finance_delivery_decay, empirical, 'Whether the transfer function is real or decaying into theatrical reaffirmation.').

omega_variable(
    cbdr_interpretive_bridge_coherence,
    'Is CBDR-structured interpretation of self-determined NDCs coherent — can a structural distinction govern interpretation of pledges the text makes purely domestic — or does the bridge smuggle in bindingness the Paris text does not contain?',
    'Conceptual analysis of Article 4''s equity references (4.3, 4.4, 4.19) against the self-differentiation architecture; test whether fairness reviews can operationalize structural distinction without converting NDCs into negotiated targets.',
    'If incoherent, the equity reading collapses toward the supranational sibling (needing negotiated targets to make distinctions bite) or the sovereigntist sibling (distinctions unenforceable in interpretation); if coherent, this story''s structure stands as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cbdr_interpretive_bridge_coherence, conceptual, 'Whether the reading''s core interpretive move — structural distinction over self-determined pledges — is internally coherent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 0, 33).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ndc_equity_reading_tr_t0, paris_article_4_ndc__equity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(ndc_equity_reading_tr_t0, observed).
narrative_ontology:measurement(ndc_equity_reading_tr_t5, paris_article_4_ndc__equity_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(ndc_equity_reading_tr_t5, observed).
narrative_ontology:measurement(ndc_equity_reading_tr_t10, paris_article_4_ndc__equity_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(ndc_equity_reading_tr_t10, observed).
narrative_ontology:measurement(ndc_equity_reading_tr_t15, paris_article_4_ndc__equity_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(ndc_equity_reading_tr_t15, observed).
narrative_ontology:measurement(ndc_equity_reading_tr_t20, paris_article_4_ndc__equity_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(ndc_equity_reading_tr_t20, observed).
narrative_ontology:measurement(ndc_equity_reading_tr_t23, paris_article_4_ndc__equity_reading, theater_ratio, 23, 0.25).
narrative_ontology:measurement_basis(ndc_equity_reading_tr_t23, observed).
narrative_ontology:measurement(ndc_equity_reading_tr_t26, paris_article_4_ndc__equity_reading, theater_ratio, 26, 0.28).
narrative_ontology:measurement_basis(ndc_equity_reading_tr_t26, observed).
narrative_ontology:measurement(ndc_equity_reading_tr_t29, paris_article_4_ndc__equity_reading, theater_ratio, 29, 0.35).
narrative_ontology:measurement_basis(ndc_equity_reading_tr_t29, observed).
narrative_ontology:measurement(ndc_equity_reading_tr_t33, paris_article_4_ndc__equity_reading, theater_ratio, 33, 0.4).
narrative_ontology:measurement_basis(ndc_equity_reading_tr_t33, observed).

% Extraction over time
narrative_ontology:measurement(ndc_equity_reading_be_t0, paris_article_4_ndc__equity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(ndc_equity_reading_be_t0, observed).
narrative_ontology:measurement(ndc_equity_reading_be_t5, paris_article_4_ndc__equity_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement_basis(ndc_equity_reading_be_t5, observed).
narrative_ontology:measurement(ndc_equity_reading_be_t10, paris_article_4_ndc__equity_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(ndc_equity_reading_be_t10, observed).
narrative_ontology:measurement(ndc_equity_reading_be_t15, paris_article_4_ndc__equity_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement_basis(ndc_equity_reading_be_t15, observed).
narrative_ontology:measurement(ndc_equity_reading_be_t20, paris_article_4_ndc__equity_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(ndc_equity_reading_be_t20, observed).
narrative_ontology:measurement(ndc_equity_reading_be_t23, paris_article_4_ndc__equity_reading, base_extractiveness, 23, 0.52).
narrative_ontology:measurement_basis(ndc_equity_reading_be_t23, observed).
narrative_ontology:measurement(ndc_equity_reading_be_t26, paris_article_4_ndc__equity_reading, base_extractiveness, 26, 0.54).
narrative_ontology:measurement_basis(ndc_equity_reading_be_t26, observed).
narrative_ontology:measurement(ndc_equity_reading_be_t29, paris_article_4_ndc__equity_reading, base_extractiveness, 29, 0.56).
narrative_ontology:measurement_basis(ndc_equity_reading_be_t29, observed).
narrative_ontology:measurement(ndc_equity_reading_be_t33, paris_article_4_ndc__equity_reading, base_extractiveness, 33, 0.58).
narrative_ontology:measurement_basis(ndc_equity_reading_be_t33, observed).

% Suppression requirement over time
narrative_ontology:measurement(ndc_equity_reading_su_t0, paris_article_4_ndc__equity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(ndc_equity_reading_su_t0, observed).
narrative_ontology:measurement(ndc_equity_reading_su_t5, paris_article_4_ndc__equity_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement_basis(ndc_equity_reading_su_t5, observed).
narrative_ontology:measurement(ndc_equity_reading_su_t10, paris_article_4_ndc__equity_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(ndc_equity_reading_su_t10, observed).
narrative_ontology:measurement(ndc_equity_reading_su_t15, paris_article_4_ndc__equity_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(ndc_equity_reading_su_t15, observed).
narrative_ontology:measurement(ndc_equity_reading_su_t20, paris_article_4_ndc__equity_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(ndc_equity_reading_su_t20, observed).
narrative_ontology:measurement(ndc_equity_reading_su_t23, paris_article_4_ndc__equity_reading, suppression_requirement, 23, 0.55).
narrative_ontology:measurement_basis(ndc_equity_reading_su_t23, observed).
narrative_ontology:measurement(ndc_equity_reading_su_t26, paris_article_4_ndc__equity_reading, suppression_requirement, 26, 0.57).
narrative_ontology:measurement_basis(ndc_equity_reading_su_t26, observed).
narrative_ontology:measurement(ndc_equity_reading_su_t29, paris_article_4_ndc__equity_reading, suppression_requirement, 29, 0.58).
narrative_ontology:measurement_basis(ndc_equity_reading_su_t29, observed).
narrative_ontology:measurement(ndc_equity_reading_su_t33, paris_article_4_ndc__equity_reading, suppression_requirement, 33, 0.6).
narrative_ontology:measurement_basis(ndc_equity_reading_su_t33, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, resource_allocation).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__supranational_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Paris Article 4 NDC obligations' decomposes into three structurally distinct claims about what NDCs are — voluntary self-determined pledges (sovereigntist_reading), CBDR-structured differentiated interpretation (this file, equity_reading), and binding ratcheting commitments with international accountability (supranational_reading). Each carries its own epsilon, beneficiary/victim structure, and type per the epsilon-invariance principle. This reading coexists with the sovereigntist reading (developing-state parties commonly hold both: domestic determination of pledge content plus CBDR-structured differentiation of obligations) and structurally influences the supranational reading (the equity veto shapes the supranational reading's legitimacy conditions and blocks its enforcement machinery without logically foreclosing a CBDR-differentiated version of it).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
