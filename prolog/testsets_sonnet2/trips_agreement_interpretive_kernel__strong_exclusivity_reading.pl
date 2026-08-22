% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__strong_exclusivity_reading, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Strong Exclusivity Reading: Uniform Patent Protection as Innovation Incentive
 *   domain: international_trade_law/public_health_policy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the strong-exclusivity reading of the TRIPS
 *   interpretive kernel: the claim that the treaty text mandates high,
 *   uniform patent protection with only narrowly-construed flexibilities,
 *   justified as necessary to incentivize pharmaceutical R&D investment. This
 *   reading is one of at least three live constructions of the same kernel
 *   text — the public_health_flexibility_reading (broad compulsory licensing)
 *   and dispute_settlement_interpretive_authority (panel primacy over textual
 *   meaning) are separate constraint stories, not alternative framings of
 *   this one. Under this reading's own lights, the standing arrangement —
 *   patent holders exercising 20-year exclusivity with compulsory licensing
 *   treated as an emergency-only exception — is the referent for ε, not the
 *   flexibility regime this reading opposes. TRIPS-plus bilateral agreements
 *   (post-2001) and the practical narrowing of Doha Declaration flexibilities
 *   in dispute practice have progressively hardened the exclusivity reading's
 *   grip, which the temporal series reflects as rising extraction and rising
 *   suppression (enforcement infrastructure — investor-state dispute
 *   mechanisms, USTR Special 301 pressure — maturing over the interval).
 *
 * KEY AGENTS:
 *   - multinational_pharmaceutical_patent_holders: Primary beneficiary (institutional/arbitrage) — collects exclusivity rents, shapes dispute litigation strategy
 *   - high_income_state_trade_negotiators: Co-agenda-setter (institutional/arbitrage) — authored and defends the strong reading in treaty forums
 *   - low_income_state_health_ministries: Primary target (moderate/constrained) — bears drug procurement costs, faces retaliation risk for aggressive licensing
 *   - patients_without_treatment_access: Ultimate victim (powerless/trapped) — bears the human cost with no direct voice
 *   - wto_dispute_settlement_panels: Analytical/adjudicative seat — operationalizes whichever reading prevails case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.78).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.72).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Strong Exclusivity Reading: Uniform Patent Protection as Innovation Incentive").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/public_health_policy/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '263d5db0-eed8-4a59-96da-b88f3a033d30').
narrative_ontology:cs_kernel_codification('263d5db0-eed8-4a59-96da-b88f3a033d30', fixed_text).
narrative_ontology:cs_authority_grounding('263d5db0-eed8-4a59-96da-b88f3a033d30', extraction).
narrative_ontology:cs_interpretation_layer_present('263d5db0-eed8-4a59-96da-b88f3a033d30').
narrative_ontology:cs_reading_relation('263d5db0-eed8-4a59-96da-b88f3a033d30', trips_agreement_interpretive_kernel__public_health_flexibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('263d5db0-eed8-4a59-96da-b88f3a033d30', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('263d5db0-eed8-4a59-96da-b88f3a033d30', foundational, uniform_exclusivity_necessary_for_innovation).
narrative_ontology:cs_axiom_status(uniform_exclusivity_necessary_for_innovation, holdable).
narrative_ontology:cs_axiom_grounding('263d5db0-eed8-4a59-96da-b88f3a033d30', uniform_exclusivity_necessary_for_innovation, empirically_contingent).
narrative_ontology:cs_axiom('263d5db0-eed8-4a59-96da-b88f3a033d30', foundational, compulsory_licensing_is_narrow_emergency_exception).
narrative_ontology:cs_axiom_status(compulsory_licensing_is_narrow_emergency_exception, holdable).
narrative_ontology:cs_axiom_grounding('263d5db0-eed8-4a59-96da-b88f3a033d30', compulsory_licensing_is_narrow_emergency_exception, conventional).
narrative_ontology:cs_reference_frame('263d5db0-eed8-4a59-96da-b88f3a033d30', uruguay_round_uniform_protection_bargain).
narrative_ontology:cs_drift_state('263d5db0-eed8-4a59-96da-b88f3a033d30', post_doha_declaration_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('263d5db0-eed8-4a59-96da-b88f3a033d30', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_patent_holders).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_state_trade_negotiators).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, biomedical_research_investors).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_state_health_ministries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_manufacturers).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_without_treatment_access).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patent_exclusivity_drives_pharmaceutical_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold 20-year patent monopolies on medicines under this reading's construction of TRIPS, set list prices without price-control competition, and actively lobby trade delegations and WTO dispute panels to narrow the scope of compulsory licensing and parallel-import carve-outs. They fund the innovation-incentive research cited in defense of the strong reading and finance enforcement litigation against states that invoke flexibilities broadly.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_patent_holders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_patent_holders, agenda_setter).

% Negotiated and continue to defend the strong-exclusivity text in TRIPS review rounds and bilateral trade agreements (TRIPS-plus provisions), treating narrow flexibilities as the correct reading of the founding bargain. They face domestic pharmaceutical-sector pressure to hold the line and can walk away from any renegotiation without losing market access themselves.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_state_trade_negotiators, agenda_setter,
    institutional, generational, arbitrage, global).

% Allocate capital toward pharmaceutical R&D on the expectation that strong, narrowly-flexible patent exclusivity will preserve return horizons. Can redirect capital to other sectors if the reading weakens, but currently capture returns from the high-price regime this reading sustains.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, biomedical_research_investors, beneficiary,
    organized, biographical, mobile, global).

% Must procure patented medicines at prices set by exclusive rights-holders, and face WTO dispute exposure if they invoke compulsory licensing beyond what this reading treats as the narrow textual exception (national emergency, non-commercial use, case-by-case authorization). Trade retaliation threats and diplomatic pressure from high-income states constrain how aggressively they use even the licensing routes technically available.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_state_health_ministries, payer,
    moderate, biographical, constrained, national).

% Are legally barred from producing lower-cost equivalents of patented medicines until patent expiry, except in the narrow compulsory-licensing windows this reading construes minimally. Their capacity to supply generics to export markets is throttled by TRIPS Article 31bis's administrative complexity, which this reading treats as adequate rather than as a functional barrier.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_manufacturers, payer,
    moderate, biographical, constrained, regional).

% Cannot afford or access patented medicines priced under this exclusivity regime and have no individual capacity to invoke trade law; they depend entirely on state-level compulsory licensing decisions that this reading discourages states from making aggressively. Exit for them means going without treatment.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_without_treatment_access, payer,
    powerless, immediate, trapped, national).

% Represent patient and low-income-state interests in TRIPS review forums but hold no formal vote in WTO dispute panels or treaty-text interpretation; their access-to-medicines framing is treated by this reading's institutional apparatus as advocacy input rather than binding interpretive authority.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_advocacy_coalitions, excluded,
    organized, generational, constrained, global).

% Adjudicate disputes over TRIPS compliance and, in doing so, either ratify or narrow this reading's construction of flexibilities case by case; their rulings carry binding trade-retaliation consequences that operationalize whichever textual reading prevails in a given dispute.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_settlement_panels, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_settlement_panels, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__strong_exclusivity_reading, multinational_pharmaceutical_patent_holders).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__strong_exclusivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, predictable global floor for patent protection so that pharmaceutical R&D investors across many jurisdictions can rely on a single minimum exclusivity term rather than negotiating protection country by country — solving a genuine cross-border coordination problem for capital allocation toward drug development.
% TRANSFER_FUNCTION: Moves willingness-to-pay for medicines from patients and health systems in patent-observing states to patent-holding firms and their investors, via price premiums sustained by exclusivity and narrowly-construed compulsory licensing exceptions.
% ABSENT_VOICES: Public health advocacy coalitions and low-income-state patient populations raise access concerns in TRIPS Council review sessions and Doha Declaration follow-up but hold no formal seat in WTO dispute panel adjudication, where the operative interpretation of 'flexibility' is actually settled.
% DISAPPEARANCE_RATIONALE: If the strong-exclusivity reading of TRIPS text vanished — if panels and negotiators uniformly adopted the broad-flexibility reading instead — patent enforcement against generic manufacturers in the Global South would collapse, drug prices in enforcement jurisdictions would fall sharply, and pharmaceutical R&D capital allocation models built on current exclusivity assumptions would need to be revised; multinational patent holders' revenue projections depend materially on this reading prevailing in dispute outcomes.
% FOUNDING_PROBLEM: The 1994 TRIPS negotiators sought to solve free-riding on pharmaceutical R&D: without enforceable global minimum patent terms, firms feared that low-protection jurisdictions would undercut returns needed to fund costly, risky drug development, so uniform strong protection was framed as necessary to sustain the innovation pipeline.
% FOUNDING_PROBLEM_CORROBORATION: Patent holders and high-income trade negotiators attest the innovation-incentive problem remains live, citing R&D cost figures. Independent economists (e.g. work cited in WHO/WTO/WIPO joint studies) and the Doha Declaration's own text attest that uniform strong exclusivity was never empirically demonstrated as necessary versus targeted, needs-based protection — suggesting the founding problem's strong-uniformity solution may be overbroad relative to the problem it was built to solve, a reading contested from outside the beneficiary set.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) because the reading's practical effect is systematic transfer from price-constrained health systems and patients to patent-holding firms, sustained by treating compulsory licensing as exceptional rather than routine. Suppression is authored substantial (0.72) because trade retaliation threats, Special 301 watch-list pressure, and TRIPS-plus bilateral ratchets actively narrow the flexibilities the base text nominally permits — this is coercive infrastructure, not passive market operation. Theater ratio is moderate-low (0.28): the innovation-incentive function is not fabricated — R&D investment decisions genuinely respond to exclusivity expectations — but a growing share of enforcement activity (post-2001 TRIPS-plus provisions, aggressive dispute litigation) defends exclusivity margins beyond what the innovation-incentive rationale alone would require, and that margin is what the rising theater_ratio and suppression_requirement series track.
 *
 * DIRECTIONALITY LOGIC:
 *   Patent holders and high-income negotiators sit near the full-beneficiary end: they set the reading's terms, collect its rents, and retain arbitrage-grade exit (capital and diplomatic mobility) if the regime shifts. Low-income health ministries and generic manufacturers sit toward the target end — constrained exit, real costs, real resistance (Doha Declaration advocacy, generic-sector litigation). Patients without treatment access sit at the full-target extreme: trapped exit options, immediate time horizon, powerless structural position, and no independent capacity to invoke any of the flexibilities that formally exist. The derivation chain (beneficiary/victim + exit) produces this ordering without needing an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — R&D free-riding absent enforceable minimum protection — was plausibly live in 1994. The mandatrophy question this reading forces is whether uniform strong exclusivity remains the right-sized solution to that problem, or whether the reading has calcified into rent-preservation dressed as innovation policy. The founding_problem_status is authored contested rather than dead, because the innovation-incentive claim is not obviously false — but the corroboration trail shows the strong-uniformity solution was never independently validated as necessary versus a narrower, needs-differentiated protection regime, which is exactly the gap the sibling flexibility reading exploits structurally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_incentive_empirical_necessity,
    'Is uniform strong patent exclusivity actually necessary to sustain pharmaceutical R&D investment, or would a narrower, needs-differentiated protection regime (as the public_health_flexibility_reading would permit) sustain comparable innovation at lower access cost?',
    'Comparative empirical studies of R&D investment responsiveness to protection-term variation across therapeutic categories and income tiers; natural experiments from jurisdictions that have exercised compulsory licensing without triggering measurable R&D contraction.',
    'If the narrower regime sustains comparable innovation, the strong-exclusivity reading''s coordination justification collapses into pure rent extraction with an innovation-incentive cover story; if not, the coordination function is genuinely load-bearing and the tangled_rope classification''s coordination half is well-founded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_empirical_necessity, empirical, 'Whether uniform strong exclusivity is empirically necessary for the innovation function it claims to serve.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading''s disagreement with the public_health_flexibility_reading live — in the treaty text''s plain language, in negotiating-history intent, or in downstream dispute-panel practice?',
    'Textual and negotiating-history analysis of TRIPS Articles 7, 8, 27, 30, and 31, cross-referenced against the actual pattern of WTO panel rulings since 1995 and the 2001 Doha Declaration''s interpretive gloss.',
    'If the disagreement is primarily textual, the two readings are genuinely incompatible constructions of the same words (closer to forecloses); if it is primarily a matter of panel practice and political will, the readings coexist as live alternative constructions that different tribunals could adopt in different disputes (coexists_with, as declared here).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating whether the strong-exclusivity and flexibility readings differ at the textual, intent, or enforcement-practice level.').

omega_variable(
    trips_plus_ratchet_reading_capture,
    'Does the proliferation of bilateral TRIPS-plus agreements (post-2001) represent this reading exporting itself beyond the multilateral kernel, or does it represent independent bilateral leverage that happens to align with this reading?',
    'Tracing whether TRIPS-plus provisions cite or invoke the multilateral TRIPS text''s interpretive ambiguity as negotiating leverage, versus whether they arise from bilateral trade-power asymmetry independent of the kernel dispute.',
    'If TRIPS-plus provisions are the strong-exclusivity reading''s downstream export mechanism, this reading''s effective extraction is understated by looking at multilateral TRIPS practice alone — the true ε may be higher when bilateral ratchets are included.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trips_plus_ratchet_reading_capture, conceptual, 'Whether bilateral TRIPS-plus agreements are this reading''s extraction mechanism or an independent leverage channel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement(trip_tr_t2007, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2007, 0.21).
narrative_ontology:measurement(trip_tr_t2013, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2013, 0.24).
narrative_ontology:measurement(trip_tr_t2019, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2019, 0.26).
narrative_ontology:measurement(trip_tr_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2001, 0.6).
narrative_ontology:measurement(trip_be_t2007, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2007, 0.66).
narrative_ontology:measurement(trip_be_t2013, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2013, 0.71).
narrative_ontology:measurement(trip_be_t2019, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2019, 0.75).
narrative_ontology:measurement(trip_be_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement(trip_su_t2007, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2007, 0.6).
narrative_ontology:measurement(trip_su_t2013, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2013, 0.64).
narrative_ontology:measurement(trip_su_t2019, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2019, 0.68).
narrative_ontology:measurement(trip_su_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resource_allocation).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the trips_agreement_interpretive_kernel. strong_exclusivity_reading (this file) authors ε=0.78 for the arrangement as strong-exclusivity advocates and enforcers experience it operating — high extraction sustained by narrow compulsory-licensing construction. public_health_flexibility_reading authors a substantially different ε and beneficiary/victim structure for the same underlying text, treating broad compulsory licensing as the text's actual, intended safeguard. dispute_settlement_interpretive_authority is orthogonal to both substantive readings — it locates interpretive power in the WTO panel process itself rather than in either textual construction. All three are ε-invariant individually; none averages or hedges across the others. Each is linked here via affects_constraints because WTO panel rulings (dispute_settlement_interpretive_authority) determine which substantive reading (this one or its sibling) actually governs enforcement outcomes in any given dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
