% ============================================================================
% CONSTRAINT STORY: cbdr_principle__historical_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__historical_responsibility_reading, []).

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
 *   constraint_id: cbdr_principle__historical_responsibility_reading
 *   human_readable: CBDR Historical-Responsibility Reading: Binding Differentiated Mitigation plus Loss/Damage Finance
 *   domain: international/treaty-law/development-economics
 *
 * SUMMARY:
 *   This story instantiates the historical-responsibility reading of the CBDR
 *   (Common But Differentiated Responsibilities) principle: the climate
 *   regime requires legally binding, economy-wide emissions reductions from
 *   industrialized (annex-listed) nations, scaled to their share of
 *   cumulative historical emissions, plus financing for loss and damage in
 *   climate-vulnerable developing nations. The arrangement this reading
 *   demands and partially obtained — the Kyoto architecture of binding annex
 *   differentiation plus the finance obligations that accreted around it — is
 *   the standing arrangement this story's epsilon assesses; the reading's own
 *   endorsement of the transfer is a normative position about that
 *   arrangement, not a different arrangement. This file is one reading of the
 *   shared cbdr_principle kernel; the voluntary-commitment reading is a
 *   separate constraint file (linked in network.affects_constraints) with its
 *   own epsilon, victim set, and classification — the contest between
 *   readings is recorded in the omega variables, not hedged into this
 *   constraint's values. KEY AGENTS (by structural relationship): -
 *   annex_i_industrialized_nations: Primary target
 *   (institutional/constrained) — bears binding reduction obligations and the
 *   finance/loss-damage transfer; also co-authors the regime's operating
 *   rules, a dual position that keeps them from being a pure payer -
 *   major_emerging_economies: Primary beneficiary (institutional/mobile) —
 *   receives emissions headroom, technology transfer, and finance; cannot be
 *   compelled by the regime's instruments -
 *   climate_vulnerable_developing_states: Intended beneficiary
 *   (organized/trapped) — holds the loss-and-damage claim; geography fixes
 *   their exposure and delivery repeatedly falls short -
 *   least_developed_countries: Intended beneficiary (organized/trapped) —
 *   first in line for adaptation finance by design, last served in delivery -
 *   developed_nation_energy_intensive_industries: Secondary target
 *   (powerful/arbitrage) — bears compliance costs and exits by relocating
 *   production to non-annex jurisdictions -
 *   multilateral_climate_fund_intermediaries: Incidental beneficiary
 *   (institutional/mobile) — retains overhead and shapes disbursement on the
 *   transfer flow - unfccc_secretariat: Administrator
 *   (institutional/constrained) — runs the regime's procedural machinery
 *   regardless of whether the obligations beneath it bind anyone -
 *   future_generations: Excluded party (powerless/trapped) — bears the
 *   outcomes, holds no seat - independent_climate_finance_trackers:
 *   Analytical observer (analytical/analytical) — audits the
 *   pledge-to-delivery record from outside the benefiting parties
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, 0.45).
domain_priors:suppression_score(cbdr_principle__historical_responsibility_reading, 0.22).
domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__historical_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__historical_responsibility_reading, "CBDR Historical-Responsibility Reading: Binding Differentiated Mitigation plus Loss/Damage Finance").
narrative_ontology:topic_domain(cbdr_principle__historical_responsibility_reading, "international/treaty-law/development-economics").

domain_priors:requires_active_enforcement(cbdr_principle__historical_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__historical_responsibility_reading, 'a966eb7b-3377-4d0c-bb3b-f0a13360f5f4').
narrative_ontology:cs_kernel_codification('a966eb7b-3377-4d0c-bb3b-f0a13360f5f4', fixed_text).
narrative_ontology:cs_authority_grounding('a966eb7b-3377-4d0c-bb3b-f0a13360f5f4', practice).
narrative_ontology:cs_interpretation_layer_present('a966eb7b-3377-4d0c-bb3b-f0a13360f5f4').
narrative_ontology:cs_reading_relation('a966eb7b-3377-4d0c-bb3b-f0a13360f5f4', cbdr_principle__voluntary_commitment_reading, coexists_with).
narrative_ontology:cs_axiom('a966eb7b-3377-4d0c-bb3b-f0a13360f5f4', foundational, cumulative_emissions_create_remedial_duty).
narrative_ontology:cs_axiom_status(cumulative_emissions_create_remedial_duty, holdable).
narrative_ontology:cs_axiom_grounding('a966eb7b-3377-4d0c-bb3b-f0a13360f5f4', cumulative_emissions_create_remedial_duty, deontological).
narrative_ontology:cs_axiom('a966eb7b-3377-4d0c-bb3b-f0a13360f5f4', foundational, differentiation_requires_binding_legal_form).
narrative_ontology:cs_axiom_status(differentiation_requires_binding_legal_form, holdable).
narrative_ontology:cs_axiom_grounding('a966eb7b-3377-4d0c-bb3b-f0a13360f5f4', differentiation_requires_binding_legal_form, instrumental).
narrative_ontology:cs_reference_frame('a966eb7b-3377-4d0c-bb3b-f0a13360f5f4', binding_annex_differentiation).
narrative_ontology:cs_drift_state('a966eb7b-3377-4d0c-bb3b-f0a13360f5f4', post_paris_consensus_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a966eb7b-3377-4d0c-bb3b-f0a13360f5f4', '').
narrative_ontology:cs_kernel_id(cbdr_principle__historical_responsibility_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, climate_vulnerable_developing_states).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, major_emerging_economies).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, multilateral_climate_fund_intermediaries).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, annex_i_industrialized_nations).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nation_energy_intensive_industries).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, polluter_pays_principle).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, climate_justice_remedial_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Industrialized treaty parties listed in the convention's annexes. They accept economy-wide emissions budgets under this arrangement, contribute the bulk of climate finance and loss/damage payments, and host the industries and taxpayers who ultimately bear those costs. Several declined the binding instruments — the United States never ratified Kyoto, Canada withdrew in 2011, Japan and Russia declined Doha-period targets — so the binding core covers a shrinking share of the bloc. Exit is lawful but costly: diplomatic isolation, loss of standing in a regime they largely designed, and no escape from the physical climate impacts that motivated the regime. They also co-author the operating rules: fund governance, accounting standards, and market mechanisms are negotiated largely on their terms.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, annex_i_industrialized_nations, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, annex_i_industrialized_nations, agenda_setter).

% Large, rapidly industrializing parties (China, India, Brazil, Indonesia among them) classified as developing under the convention. Under this arrangement they take no binding reduction obligations, retain full emissions headroom for growth, and receive technology transfer and finance. They are also the largest current and, increasingly, largest cumulative emitters, which makes their exemption the arrangement's most strained element. They cannot be compelled — no instrument binds them — and they demonstrated at Paris that they can reshape the regime's terms when differentiation moves against them.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, major_emerging_economies, beneficiary,
    institutional, generational, mobile, global).

% Small island states and drought- and coastal-exposed nations organized as AOSIS and the G77's vulnerable-country caucuses. They face existential loss from warming they contributed almost nothing to, and their claim — compensation for loss and damage plus adaptation finance — is this arrangement's moral core. They cannot exit the harm: geography fixes their exposure, and no alternative forum delivers adaptation resources at scale. Their leverage is moral and procedural rather than material; they act only in coalition, and the resources they are owed have repeatedly arrived late, small, and loan-weighted.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_vulnerable_developing_states, beneficiary,
    organized, biographical, trapped, global).

% The UN's least-developed-country group. First in line for adaptation finance by the arrangement's design and last in actual delivery: adaptation flows to LDCs run at a fraction of assessed needs, much of it as loans, much of it through intermediaries that retain overhead. Their negotiating position depends entirely on the differentiated-obligations claim; a uniform-obligations regime would price them out of participation entirely.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, least_developed_countries, beneficiary,
    organized, biographical, trapped, regional).

% Cement, steel, chemicals, and fossil-fuel interests in industrialized economies. They bear compliance costs through energy prices and production limits, and they organized the domestic resistance that shaped the arrangement's limits — the Byrd–Hagel resolution preceded US non-ratification. Their exit is arbitrage: production relocates to non-annex jurisdictions without comparable obligations, a flow the arrangement's design anticipates and its critics cite as its central leak.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nation_energy_intensive_industries, payer,
    powerful, immediate, arbitrage, global).

% The GCF, GEF, Adaptation Fund trustees, and World Bank-administered channels that move finance from contributors to recipients. They retain fees and administrative overhead, shape disbursement conditions, and are governed by boards on which contributor states hold weight. Their position persists across regime designs — they administered the binding era's funds and now administer the voluntary era's.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, multilateral_climate_fund_intermediaries, beneficiary,
    institutional, biographical, mobile, global).

% The permanent bureaucracy of the climate regime. It compiles national communications, services the negotiating rounds, administers the compliance machinery, and maintains the ledger through which obligations are recorded and audited. It holds no independent material power; its continuity depends on the regime's continuation, and its procedural activity continues at full volume regardless of whether the obligations beneath it bind anyone.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% The parties not present. They inherit the emissions trajectories the arrangement permits or prevents, the debts the finance obligations create or forgive, and the adaptation deficits the delivery gaps leave. They hold no seat in the COP process; their interests enter only through the arguments other parties choose to make on their behalf.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).

% OECD, ODI, and academic accounting efforts that reconcile what contributors pledge against what recipients receive. They publish the delivery gap, the loan-grant mix, and the accounting discrepancies (mobilized versus provided, grant-equivalent values) that the regime's self-reporting smooths over. They hold no enforcement power; their product is the record against which the arrangement's claims are checked.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, independent_climate_finance_trackers, observer,
    analytical, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cbdr_principle__historical_responsibility_reading, major_emerging_economies).
narrative_ontology:fixing_cost_class(cbdr_principle__historical_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the participation problem of a global commons regime: mitigation effort is allocated across parties by historical contribution and capacity, so the largest historical emitters accept binding budgets while the rest of the world joins without binding limits, and a finance channel moves adaptation and loss-and-damage resources to the most exposed parties. Universal membership is preserved by making the obligation set asymmetric.
% TRANSFER_FUNCTION: Moves binding mitigation effort onto developed nations' own economies (the constraint side) and moves money, technology, and emissions headroom from developed nations to developing ones (the finance and differentiation side). Within the finance flow, multilateral intermediaries retain overhead, contributor states shape disbursement, and the largest material receipts have concentrated in middle-income emerging economies.
% ABSENT_VOICES: Future generations hold no seat; climate-displaced communities within developing nations are represented only through their states; households and firms in developed nations bear the costs through taxation and energy prices but appear at the COP only via their governments' negotiating positions; and the populations of non-party states (the US during Kyoto's first period) were bound by nothing their government had not ratified.
% DISAPPEARANCE_RATIONALE: The regime's coalitions (G77, AOSIS, the LDC group), its finance architecture, and its negotiating logic are organized around the differentiated-obligations claim. Overnight removal would force an immediate choice between uniform binding obligations (which developing nations would refuse) and purely voluntary action (which vulnerable states would read as abandonment); the finance channel would collapse into discretionary aid; and the annex-based coalition structure that has organized three decades of negotiation would dissolve and re-form around capacity-only or voluntarist lines.
% FOUNDING_PROBLEM: The Rio bargain of 1992: a global commons problem in which the largest cumulative emitters and the wealthiest states had to act, while the majority of humanity — low historical contribution, low capacity, high exposure — would not accept symmetric obligations. Differentiation by historical responsibility and capacity was the price of universal membership; this reading holds the strong form of that bargain (binding, proportional, plus loss/damage), which the Kyoto Protocol partially embodied.
% FOUNDING_PROBLEM_CORROBORATION: Developed-nation treaty conduct corroborates the genealogy from outside the beneficiary set: the United States ratified the UNFCCC's differentiated framework in 1992 while the Senate resolved 95–0 (Byrd–Hagel) against its binding extension, and Canada withdrew from Kyoto rather than meet compliance costs — both attest that differentiation was the price of participation and that its binding form was the contested part. Treaty historians (Bodansky, Rajamani) and the negotiated record of Rio and Kyoto corroborate the participation-deadlock founding problem. No source outside the beneficiary set attests that the loss-and-damage component was part of the original founding problem: it entered the regime's texts in 2007 (Bali), 2013 (Warsaw), and 2022, and its proponents read it back into the historical record — that anachronism is itself signal about the reading's development.
narrative_ontology:disappearance_verdict(cbdr_principle__historical_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__historical_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__historical_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cbdr_principle__historical_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__historical_responsibility_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__historical_responsibility_reading_tests).
:- end_tests(cbdr_principle__historical_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon (0.45 at interval end, peaking 0.60 during Kyoto's first commitment period) records the magnitude of the arrangement's compulsory transfers and binding constraints — what it moves and compels — assessed on this reading's own lights. The reading holds the transfer is owed remediation for cumulative harm; that endorsement is normative and does not shrink the measured magnitude of what the arrangement compels from developed nations, nor does it erase the reading's own critique of the operation's failure modes: loan-weighted finance, intermediary overhead, carbon-market offsets that let developed nations avoid domestic cuts, and a delivery gap that leaves the intended beneficiaries holding a promise rather than a payment. Suppression is low (0.22) because exits are real and have been taken — the US never ratified Kyoto, Canada withdrew, Japan and Russia declined Doha targets — and because developing nations were never bound at all. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: it rose through the Marrakesh rule-building and the compliance machinery of the first commitment period (peak 0.62 in 2009), then decayed through Doha's shrinkage, the Paris pivot to voluntary contributions, and the loss/damage fund's voluntary capitalization (0.22 by 2023) — an enforcement intensification followed by enforcement decay, not a static picture. Theater_ratio rises monotonically (0.28 to 0.68) because the binding core eroded while procedural activity continued at full volume: annual reaffirmations, target-setting over a shrinking covered share, and a fund agreed with ceremony and capitalized at a fraction of assessed need. Accessibility_collapse is low (0.30) because the alternative arrangement — self-differentiated voluntary contributions — is fully available and was actually adopted; resistance is high (0.72) with documented episodes: the Byrd–Hagel resolution (95–0), non-ratification, withdrawal, and two decades of blocked finance decisions. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is context-scaled downstream. All three series share one time grid (1997, 2001, 2005, 2009, 2012, 2015, 2019, 2023) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute differently, and the divergence is structural rather than notional. From the developed-nation seat, the arrangement is a compelled cost co-authored by themselves — binding budgets plus a transfer they resisted at every stage and partially exited; their dual position (payer and agenda-setter) means the rules they pay under are ones they largely wrote. From the major-emerging-economy seat, the arrangement is a subsidy: headroom, technology, and finance with no compulsion attached and a demonstrated ability to reshape terms when differentiation moves against them. From the vulnerable-state seat, the arrangement is a promise whose non-delivery they bear — their computed experience of the same structure is dominated by the delivery gap, not the transfer. The engine computes per-seat types from these structural positions; the divergence between seats is the measurement, not an artifact to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Developed nations and their energy-intensive industries are declared victims and carry payer roles, placing them near the target end; their exit options differentiate them — industries hold arbitrage-grade exit (relocation to non-annex jurisdictions), which damps their effective burden relative to the states themselves, whose exit is lawful but costly and who remain exposed to the physical harms that motivated the regime. Major emerging economies and the vulnerable-state groups are declared beneficiaries with low directionality; the vulnerable states' trapped exit reflects their inability to exit the harm rather than any compulsion by the arrangement. Fund intermediaries sit nearest the beneficiary end: they collect on the flow without bearing its obligations. No directionality overrides are authored: the derivation from role declarations plus exit options captures the structure, and the one genuinely dual-positioned agent (developed nations as payer and co-author) is encoded structurally via secondary_role rather than a numeric override — their co-authorship is what keeps their derived position below that of a pure payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the participation deadlock of a global commons regime — is not dead: universal participation remains necessary, and the differentiation claim is still what holds the G77, AOSIS, and LDC coalitions together. But the founding vehicle of the binding form, the Kyoto architecture, is functionally dead, and the arrangement now persists largely as procedural maintenance around a voluntary-era regime. This is why founding_problem_status is authored 'contested' rather than 'dead': the beneficiaries attest the founding problem (historical debt unaddressed) is live, while the regime's operative treaty has moved to the sibling reading's architecture. The classification prevents two mislabelings. It is not a snare: the coordination function is real (differentiation was and remains the price of universal membership), the transfer runs toward the harmed party, and this reading endorses it as remediation. It is not a rope: the transfer is compulsory in form, contested in fact, resisted by those it compels, and exits have been taken. The tangled-rope claim sits between, and the rising theater series records the drift toward inertial persistence that a later re-authoring may need to classify differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cbdr_kernel_reading_contest,
    'This constraint is one reading of the cbdr_principle kernel — the historical_responsibility_reading (binding, cumulative-emissions-proportional reductions from developed nations plus loss/damage finance). The sibling reading (cbdr_principle__voluntary_commitment_reading: voluntary nationally determined contributions with technology transfer as the primary developed-nation obligation) would structurally remove developed nations from the victim set (no binding cuts, no compulsory finance), re-situate developing nations under self-differentiated contributions rather than exemption, and replace the finance-transfer obligation with a technology-transfer expectation. Where is the disagreement located, and which reading does the regime actually instantiate?',
    'Treaty-level adjudication: a COP consensus decision or amendment fixing what CBDR requires in legal form, or de facto settlement of the question by which architecture the regime''s finance and mitigation instruments actually run on.',
    'If the voluntary reading is adopted, the victim and beneficiary sets of this constraint invert and the compulsory transfer disappears; if this reading is adopted, the sibling''s arrangement is a diluted non-implementation. The two readings are separate constraint files and must never be averaged into one epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cbdr_kernel_reading_contest, conceptual, 'Kernel-level contest: which reading of CBDR''s differentiation requirement the regime instantiates.').

omega_variable(
    cumulative_emissions_attribution,
    'How are cumulative historical emissions attributed for the proportionality requirement — territorial production accounting, consumption-based accounting, which baseline year, and does ''developed'' mean the frozen 1992 annex lists or a dynamic classification?',
    'An agreed accounting methodology in a COP decision or scientific body ruling, with membership criteria for the developed-nation set.',
    'Territorial accounting concentrates obligations on early industrializers; consumption-based accounting shifts weight toward import-dependent developed economies and export-manufacturing emerging economies. A dynamic classification would move major emerging economies toward the victim set as their cumulative share grows; a frozen classification preserves the current sets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cumulative_emissions_attribution, empirical, 'Attribution methodology and membership criteria for the proportional-obligation formula.').

omega_variable(
    finance_delivery_gap,
    'Are the finance and loss/damage obligations actually being discharged — pledged versus delivered volumes, grant-equivalent versus loan-weighted accounting, and how much of claimed delivery is reclassified existing aid?',
    'OECD and UNFCCC Standing Committee on Finance reconciliations using grant-equivalent accounting, audited against recipient-side records.',
    'If delivery is systematically short and loan-weighted, the intended beneficiaries'' benefit is largely notional and their computed seat shifts from subsidized to unfulfilled claimant, pushing the arrangement''s observed operation toward theatrical maintenance of a promise rather than its discharge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finance_delivery_gap, empirical, 'Whether the transfer obligations are discharged in real, grant-equivalent resources.').

omega_variable(
    eroding_exemption_consistency,
    'Is this reading internally consistent as major emerging economies'' cumulative emissions rival or exceed historical developed-nation emissions — does the reading''s own cumulative-causation logic extend binding obligations to them, or does ''developed'' freeze at the 1992 annexes?',
    'Doctrinal development within the G77 and climate-justice scholarship: whether dynamic historical responsibility is adopted or the annex freeze is defended as settled differentiation.',
    'A dynamic reading expands the victim set to include major emerging economies and strengthens the arrangement''s coherence but fractures the negotiating coalition that sustains it; a frozen reading preserves coalition unity at the cost of an internal consistency challenge the sibling reading exploits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eroding_exemption_consistency, conceptual, 'Whether the reading''s cumulative-causation premise applies dynamically or freezes at 1992 membership.').

omega_variable(
    enforcement_return_path,
    'Can binding enforcement of the differentiated obligations return — via the ICJ advisory opinion on state climate obligations (July 2025), ITLOS rulings, domestic climate litigation, or a future COP restoring compulsory finance — or is the decay series terminal?',
    'Track whether advisory-opinion holdings and subsequent litigation produce justiciable, enforceable differentiated obligations, and whether any COP re-establishes compulsory contribution schedules.',
    'An enforcement return reverses the suppression and theater trajectories and reconstitutes the arrangement''s binding core; failure to convert advisory opinions into enforceable obligations completes the drift toward ceremonial persistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_return_path, empirical, 'Whether the enforcement decay of the binding-differentiation core is reversible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__historical_responsibility_reading, 1997, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_tr_t1997, cbdr_principle__historical_responsibility_reading, theater_ratio, 1997, 0.28).
narrative_ontology:measurement(cbdr_tr_t2001, cbdr_principle__historical_responsibility_reading, theater_ratio, 2001, 0.36).
narrative_ontology:measurement(cbdr_tr_t2005, cbdr_principle__historical_responsibility_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(cbdr_tr_t2009, cbdr_principle__historical_responsibility_reading, theater_ratio, 2009, 0.48).
narrative_ontology:measurement(cbdr_tr_t2012, cbdr_principle__historical_responsibility_reading, theater_ratio, 2012, 0.58).
narrative_ontology:measurement(cbdr_tr_t2015, cbdr_principle__historical_responsibility_reading, theater_ratio, 2015, 0.62).
narrative_ontology:measurement(cbdr_tr_t2019, cbdr_principle__historical_responsibility_reading, theater_ratio, 2019, 0.65).
narrative_ontology:measurement(cbdr_tr_t2023, cbdr_principle__historical_responsibility_reading, theater_ratio, 2023, 0.68).

% Extraction over time
narrative_ontology:measurement(cbdr_be_t1997, cbdr_principle__historical_responsibility_reading, base_extractiveness, 1997, 0.34).
narrative_ontology:measurement(cbdr_be_t2001, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2001, 0.42).
narrative_ontology:measurement(cbdr_be_t2005, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(cbdr_be_t2009, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2009, 0.6).
narrative_ontology:measurement(cbdr_be_t2012, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2012, 0.55).
narrative_ontology:measurement(cbdr_be_t2015, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2015, 0.47).
narrative_ontology:measurement(cbdr_be_t2019, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2019, 0.43).
narrative_ontology:measurement(cbdr_be_t2023, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2023, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_su_t1997, cbdr_principle__historical_responsibility_reading, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(cbdr_su_t2001, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2001, 0.42).
narrative_ontology:measurement(cbdr_su_t2005, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(cbdr_su_t2009, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2009, 0.62).
narrative_ontology:measurement(cbdr_su_t2012, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2012, 0.5).
narrative_ontology:measurement(cbdr_su_t2015, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2015, 0.34).
narrative_ontology:measurement(cbdr_su_t2019, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2019, 0.26).
narrative_ontology:measurement(cbdr_su_t2023, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2023, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__historical_responsibility_reading, resource_allocation).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, cbdr_principle__voluntary_commitment_reading).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, climate_loss_damage_fund).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the cbdr_principle kernel. The colloquial label 'CBDR' covers two structurally distinct claims with different epsilons, victim sets, and enforcement profiles: this file (historical_responsibility_reading — binding proportional obligations plus compulsory loss/damage finance; developed nations in the victim set; tangled-rope structure with active enforcement required) and cbdr_principle__voluntary_commitment_reading (voluntary NDCs with technology transfer as the developed-nation obligation; no compulsory transfer; different enforcement profile entirely). The upstream reading shaped the downstream one historically: the binding reading's operation (Kyoto) generated the compliance failures and coalition strains that the voluntary reading was built to resolve, so each story links the other in affects_constraints. The loss/damage fund edge records that this reading's finance component structurally feeds the fund's design and legitimacy claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
