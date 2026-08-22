% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Paris Article 4 NDC Regime — Supranational Reading (Binding Ratchet with International Accountability)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates the SUPRANATIONAL READING of the Paris Article 4
 *   NDC kernel: NDCs construed as legally binding commitments whose content
 *   must ratchet upward every five years toward net-zero, policed by an
 *   international accountability architecture (enhanced transparency
 *   framework, global stocktake, compliance committee, institutionalized
 *   climate-finance obligations). Assessed by the reading's own lights, the
 *   arrangement it describes is a high-epsilon system: carbon-intensive
 *   industries face scheduled regulatory extinction, fossil-exporting states
 *   face a published devaluation of their rent base, developed treasuries
 *   face institutionalized North-South transfers, and non-performance carries
 *   reputational and financial consequence. The same structure performs
 *   coordination no voluntary scheme achieves — it is the reading's answer to
 *   the free-rider problem. Per the epsilon-invariance principle, the sibling
 *   readings (sovereigntist_reading: voluntary self-determined pledges;
 *   equity_reading: CBDR-differentiated obligation) are SEPARATE constraints
 *   with their own epsilon values, victim sets, and enforcement structures;
 *   they are linked via network.affects_constraints, not folded into this
 *   story. The epsilon referent here is the binding-accountability
 *   arrangement this reading holds to be in force — never the voluntary
 *   arrangement the sovereigntist sibling would describe. Claim and metrics
 *   are authored independently: the claimed type is what I believe
 *   structurally true; the metrics describe the arrangement's operation as
 *   this reading presents it.
 *
 * KEY AGENTS:
 *   - - unfccc_regime_bodies: Agenda-setter and institutional beneficiary (institutional/identity_locked) — administers review machinery, collects budget and procedural authority
 *   - - renewable_energy_sectors: Beneficiary (organized/mobile) — demand expansion guaranteed by the ambition cycle
 *   - - climate_vulnerable_states: Beneficiary (organized/trapped) — finance and adaptation recipients with existential stake in continuation
 *   - - carbon_intensive_industries: Primary target (powerful/constrained) — bears compliance costs and scheduled contraction
 *   - - fossil_fuel_exporting_states: Primary target (powerful/trapped) — stranded rent base on a published schedule
 *   - - developed_state_taxpayers: Payer with secondary beneficiary position (moderate/constrained) — funds the transfers, claims the deferred benefits
 *   - - coal_region_workforce: Excluded voice (powerless/trapped) — bears closure costs without a seat
 *   - - independent_assessment_bodies: Analytical observer (analytical/analytical) — reputational-pressure channel, no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.72).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.62).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris Article 4 NDC Regime — Supranational Reading (Binding Ratchet with International Accountability)").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, '4c5d19e1-f2b4-4fb6-87a0-a3e36003a401').
narrative_ontology:cs_kernel_codification('4c5d19e1-f2b4-4fb6-87a0-a3e36003a401', fixed_text).
narrative_ontology:cs_authority_grounding('4c5d19e1-f2b4-4fb6-87a0-a3e36003a401', lineage).
narrative_ontology:cs_interpretation_layer_present('4c5d19e1-f2b4-4fb6-87a0-a3e36003a401').
narrative_ontology:cs_reading_relation('4c5d19e1-f2b4-4fb6-87a0-a3e36003a401', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('4c5d19e1-f2b4-4fb6-87a0-a3e36003a401', paris_article_4_ndc__equity_reading, influences).
narrative_ontology:cs_axiom('4c5d19e1-f2b4-4fb6-87a0-a3e36003a401', foundational, ndcs_legally_binding_with_consequences).
narrative_ontology:cs_axiom_status(ndcs_legally_binding_with_consequences, holdable).
narrative_ontology:cs_axiom_grounding('4c5d19e1-f2b4-4fb6-87a0-a3e36003a401', ndcs_legally_binding_with_consequences, conventional).
narrative_ontology:cs_axiom('4c5d19e1-f2b4-4fb6-87a0-a3e36003a401', foundational, five_year_ratchet_obligatory).
narrative_ontology:cs_axiom_status(five_year_ratchet_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('4c5d19e1-f2b4-4fb6-87a0-a3e36003a401', five_year_ratchet_obligatory, instrumental).
narrative_ontology:cs_reference_frame('4c5d19e1-f2b4-4fb6-87a0-a3e36003a401', binding_ratchet_net_zero_trajectory).
narrative_ontology:cs_drift_state('4c5d19e1-f2b4-4fb6-87a0-a3e36003a401', post_first_global_stocktake, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4c5d19e1-f2b4-4fb6-87a0-a3e36003a401', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, unfccc_regime_bodies).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, renewable_energy_sectors).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, climate_vulnerable_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, developed_state_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, developed_state_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the treaty's review machinery: convene the governing sessions, operate the transparency framework, run the periodic global stocktake, and staff the compliance committee. Collect assessed contributions and administrative budget from member states and accumulate procedural authority over how national climate plans are presented, reviewed, and compared. The secretariat's staffing, budget, and diplomatic standing are constituted by the review process itself; exiting the arrangement would dissolve the organization's mandate along with it.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, unfccc_regime_bodies, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, unfccc_regime_bodies, beneficiary).

% Sell into demand that the ratcheting obligation guarantees to expand: every tightened national plan converts into procurement mandates, auction volumes, and grid-access priority for wind, solar, storage, and electrification suppliers. Capital is mobile across sectors and jurisdictions, so participation is a portfolio choice, but the sector's growth projections are indexed to the ambition cycle continuing and tightening.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, renewable_energy_sectors, beneficiary,
    organized, biographical, mobile, global).

% Receive finance, technology-transfer provisions, and adaptation funding channeled through the regime, and hold moral leverage in stocktake proceedings disproportionate to their share of emissions. Exposure to physical climate impacts makes the arrangement's continuation existential for them: they cannot exit the atmosphere or the diplomatic process that promises compensation and mitigation. They also accept plan-submission and review burdens, and their development space narrows as the ratchet applies to all parties.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, climate_vulnerable_states, beneficiary,
    organized, generational, trapped, global).

% Produce cement, steel, chemicals, and hydrocarbons under a regulatory trajectory that schedules their product lines for contraction and eventual phase-out. Costs arrive as carbon pricing, performance standards, disclosure mandates, and border adjustments on embodied emissions. Assets are long-lived and site-specific, so relocation is partial and retirement is booked as stranded capital; litigation, lobbying, and jurisdiction-shopping are the available counters.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    powerful, biographical, constrained, global).

% Depend on hydrocarbon rents for a large share of fiscal revenue and export earnings. The ratchet trajectory devalues the core national asset base on a published schedule, and diversification programs move slower than the ambition cycle. They retain obstruction leverage inside consensus procedures but cannot withdraw from the demand-side expectations the regime sets for their customers.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_states, payer,
    powerful, generational, trapped, global).

% Fund the finance, technology, and adaptation transfers through national budgets and absorb residual transition costs passed through energy prices. They also hold claims on the arrangement's benefits — avoided climate damage, cleaner air, new industry — which arrive on longer horizons than the appropriations. Fiscal priorities are set domestically; the transfer obligation is internationally framed but domestically paid.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, developed_state_taxpayers, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, developed_state_taxpayers, beneficiary).

% Concentrated employment in mining and coal-fired generation faces scheduled closure under national plans submitted to satisfy the ratchet. Just-transition funds exist on paper but disbursement lags closure. These workers hold no delegation seat, no veto, and limited geographic mobility; retraining programs assume local labor markets that do not exist.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, coal_region_workforce, excluded,
    powerless, immediate, trapped, national).

% Track pledged versus projected emissions through emissions-gap and country-assessment consortia, publish scorecards that feed reputational pressure, and supply the factual baseline for stocktake deliberations. They hold no enforcement power; their influence runs entirely through publication and the credibility of their methods.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, independent_assessment_bodies, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__supranational_reading, diffuse).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__supranational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the transboundary free-rider problem in greenhouse-gas abatement: absent binding, mutually scrutinized, escalating commitments, each state's dominant strategy is to let others cut first. The arrangement converts unilateral discretion into reciprocal verifiable obligation and supplies a common accounting and review surface — transparency framework, global stocktake, compliance procedure — that no bilateral or purely voluntary scheme provides.
% TRANSFER_FUNCTION: Moves decarbonization obligation and compliance cost onto carbon-intensive industries and fossil-fuel-exporting states; moves finance, technology, and adaptation resources from developed-state treasuries to developing states as an institutionalized compensatory flow; moves scrutiny authority over national energy policy upward to multilateral review bodies.
% ABSENT_VOICES: Fossil-producing region workforces and communities bear closure costs with no formal seat in governing proceedings — represented only through national delegations that frequently oppose their interests. Future generations affected by today's ratchet trajectory appear only as rhetorical referents. Non-party and withdrawing states lose standing in the process that prices their exports, since border-adjustment mechanisms apply without their consent.
% DISAPPEARANCE_RATIONALE: If binding ratcheting commitments with international accountability vanished overnight, the regime would revert to pledge-and-review: transparency reviews would lose consequence, finance flows would lose their anchoring frame, carbon-intensive asset-retirement schedules would extend, and the entire conference, review, and compliance architecture would dissolve into diplomacy without teeth. The energy investment cycle would rearrange around the removed expectation of escalating obligation.
% FOUNDING_PROBLEM: Designing international climate commitments strong enough to overcome free-riding after two failures: Kyoto's rigid top-down targets collapsed for lack of ratification breadth, and Copenhagen's pledge model collapsed for lack of accountability. The supranational reading's founding problem was making nationally determined pledges genuinely binding — procedural obligation, mandatory progression, and consequence for non-performance.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: UNEP Emissions Gap assessments and IPCC synthesis reports attest that the ambition shortfall the ratchet was built to close remains live; treaty-effectiveness scholarship independent of regime institutions documents that no material sanction has yet been applied to a non-performing party, corroborating the dispute over whether bindingness exists in operation or only in text. Petrostate delegations and industry associations — adversarial to this reading — likewise attest the accountability machinery lacks coercive force. Corroboration here runs against the reading's own claim, which is itself signal that the founding problem's solution is disputed rather than settled.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__supranational_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.72 at interval end) because the ratchet's costs are concentrated and scheduled: sectoral phase-outs, border adjustments, and finance obligations fall on identifiable seats while the obligation escalates by design every cycle. Suppression (0.62) reflects the enforcement machinery the reading posits — transparency review, compliance procedure, reputational and financial consequence, trade-side instruments — and is authored as a raw structural property, unscaled by power or scope; only extractiveness is scaled downstream. Theater ratio DECLINES across the interval (0.42 to 0.30) under this reading: early pledge-review activity carried substantial performative content, but the rulebook finalization, the first global stocktake, and the diffusion of border-adjustment instruments convert review into consequential procedure. Accessibility collapse is moderate (0.45): the withdrawal door formally stays open, and the voluntary framing persists in discourse as a live alternative — the reading suppresses it rhetorically and procedurally, not physically. Resistance is substantial (0.60): petrostate obstruction in consensus procedures, the withdrawal-and-return episode of a major emitter, and developing-state complaints over review burdens are all recorded resistance. The measurement series run on ONE shared time grid (points 0, 3, 6, 9, 12, 15) with every tracked metric authored at every point; points 12 and 15 carry projected basis. Dynamics are monotonic ratchet-hardening, not cyclical — no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same structure. From the regime bodies' position the arrangement is the coordination achievement they administer and staff; from the carbon-intensive and exporter seats it is a scheduled expropriation of asset value with constrained exit. Two same-class victims diverge sharply: carbon_intensive_industries (powerful, constrained) retain litigation, lobbying, and partial relocation, while coal_region_workforce (powerless, trapped) bears closure with no seat and no mobility — the same obligation, radically different effective positions, driven by exit options rather than nominal class. Coalition potential among the dispersed payers exists in principle (consumer interests, workforce organizations, affected industries) but is currently unrealized; the analysis flags it rather than assuming its absence is structural. The regime bodies' identity lock is institutional: the secretariat has become its review function, so the option of relaxing the machinery it administers is not a live internal position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: unfccc_regime_bodies (agenda-setter collecting budget and authority), renewable_energy_sectors (guaranteed demand), and climate_vulnerable_states (finance and adaptation recipients) all sit near the subsidized end. Victim declarations drive high directionality: carbon_intensive_industries, fossil_fuel_exporting_states, and developed_state_taxpayers sit near the target end, sharpened by constrained or trapped exit. One nuance: climate_vulnerable_states are trapped AND beneficiaries — exit modulation sharpens directionality for TARGETS, it does not invert beneficiary-side subsidy, so their derived directionality stays low despite the trapped atom; the trap concerns their physical and diplomatic position, not their net position under the arrangement. Developed_state_taxpayers are genuinely dual-positioned (pay the transfers, claim the deferred benefits), which their secondary_role records. No directionality_overrides are authored: the derivation from declared roles, power atoms, and exit options captures every seat's relationship without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — overcoming free-riding after Kyoto and Copenhagen — is contested rather than dead: adversaries and independent assessments agree the ambition gap persists, so the arrangement has not outlived its stated function, and no mandatrophy resolution is declared. Classification discipline cuts both ways. Mislabeling this as pure extraction (snare) would erase the genuine coordination function that even the arrangement's adversaries acknowledge it performs — the free-rider solution is primary, not cover. Mislabeling it as pure coordination (rope) would erase the identifiable victim set and the asymmetric burden schedule that require active enforcement to hold. The tangled-rope reading holds both: coordination function and asymmetric extraction through the same structure. The absence of a sunset clause is deliberate — the ratchet is open-ended by design, each cycle renewing the mandate, which distinguishes this arrangement from transitional scaffolding even though the net-zero endpoint gives it a teleological horizon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the paris_article_4_ndc kernel governs — binding supranational commitment (this story), voluntary sovereigntist pledge, or CBDR-differentiated obligation? The readings instantiate structurally different constraints with different epsilon, victim sets, and enforcement structures.',
    'Authoritative interpretive events: ICJ/ITLOS advisory opinions on states'' climate obligations, governing-body decision practice on consequence regimes, and sustained state practice on whether non-performance carries material cost.',
    'Adopting the sovereigntist sibling collapses sanctioned enforcement and institutionalized transfers out of the structure — epsilon drops sharply and the victim set thins. Adopting the equity sibling replaces the uniform ratchet with differentiated burden classes and redistributes the victim set between developed and developing seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This constraint is one reading of the paris_article_4_ndc kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    bindingness_practice_gap,
    'Does the accountability machinery impose material consequences for non-performance, or does the supranational reading describe the treaty''s aspiration rather than its operation?',
    'Sanction-incidence audit across completed ratchet cycles: cases where missed targets, late submissions, or finance shortfalls produced financial, trade, or legal cost to the non-performing party.',
    'If no material consequence has ever landed, the authored suppression overstates operative coercion and epsilon indexes the reading''s aspiration rather than the arrangement''s operation; recomputation would pull the profile toward the sovereigntist sibling''s low-enforcement shape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bindingness_practice_gap, empirical, 'Gap between the reading''s bindingness premise and observed enforcement practice.').

omega_variable(
    transfer_institutionalization_stability,
    'Are North-to-South finance and technology transfers a structural feature of this reading''s arrangement, or contingent political bargaining that each budget cycle can revoke without procedural cost?',
    'Track delivery against pledged baselines across successive stocktake cycles and ministerial declarations; test whether shortfalls trigger any procedural consequence inside the review machinery.',
    'If transfers prove revocable without cost, climate_vulnerable_states become nominal-only beneficiaries, the beneficiary structure thins, and the arrangement drifts toward performative maintenance — rising theater ratio and a weakened coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_institutionalization_stability, empirical, 'Stability of the institutionalized wealth-transfer component of the arrangement.').

omega_variable(
    extinction_design_vs_emergence,
    'Is the regulatory extinction of carbon-intensive industries a designed target of the ratchet''s drafting, or an emergent side effect of aggregate ambition that happens to concentrate on them?',
    'Textual and decision-record analysis: whether phase-out schedules, border adjustments, and performance standards were drafted with explicit sectoral targeting, versus emerging from economy-wide caps and stocktake outcomes.',
    'Designed targeting concentrates directionality on those seats and sharpens the coordination/extraction asymmetry; emergent concentration suggests diffuse burden-sharing and a computed classification nearer the coordination pole.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extinction_design_vs_emergence, conceptual, 'Whether the victim set is designed into the instrument or emergent from its operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__supranational_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pari_tr_t3, paris_article_4_ndc__supranational_reading, theater_ratio, 3, 0.39).
narrative_ontology:measurement(pari_tr_t6, paris_article_4_ndc__supranational_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement(pari_tr_t9, paris_article_4_ndc__supranational_reading, theater_ratio, 9, 0.34).
narrative_ontology:measurement(pari_tr_t12, paris_article_4_ndc__supranational_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(pari_tr_t15, paris_article_4_ndc__supranational_reading, theater_ratio, 15, 0.3).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__supranational_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(pari_be_t3, paris_article_4_ndc__supranational_reading, base_extractiveness, 3, 0.59).
narrative_ontology:measurement(pari_be_t6, paris_article_4_ndc__supranational_reading, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(pari_be_t9, paris_article_4_ndc__supranational_reading, base_extractiveness, 9, 0.66).
narrative_ontology:measurement(pari_be_t12, paris_article_4_ndc__supranational_reading, base_extractiveness, 12, 0.69).
narrative_ontology:measurement(pari_be_t15, paris_article_4_ndc__supranational_reading, base_extractiveness, 15, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__supranational_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(pari_su_t3, paris_article_4_ndc__supranational_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(pari_su_t6, paris_article_4_ndc__supranational_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(pari_su_t9, paris_article_4_ndc__supranational_reading, suppression_requirement, 9, 0.58).
narrative_ontology:measurement(pari_su_t12, paris_article_4_ndc__supranational_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(pari_su_t15, paris_article_4_ndc__supranational_reading, suppression_requirement, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__equity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Paris NDCs' decomposes, per the epsilon-invariance principle, into three structurally distinct claims that share one treaty text but differ on legal character, burden distribution, and victim set. This story (supranational_reading) authors high epsilon for the binding-accountability arrangement it holds to be in force; sovereigntist_reading authors low epsilon for a voluntary pledge-and-review arrangement with no sanctioned enforcement; equity_reading authors epsilon for a CBDR-differentiated arrangement with a redistributed victim set. The supranational reading sits upstream of the equity reading (its accountability machinery conditions how differentiation can operate) and in direct logical contradiction with the sovereigntist reading (binding versus voluntary cannot both characterize the same instrument within one party's framework). All three files cross-link via network.affects_constraints; none folds another's contest into its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
