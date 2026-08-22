% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: TRIPS Agreement — Strong Exclusivity Reading (High Uniform Patent Protection)
 *   domain: international_trade_law/public_health_policy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the strong-exclusivity reading of the TRIPS
 *   interpretive kernel: the view, held by originator pharmaceutical firms
 *   and high-income state trade negotiators, that TRIPS text mandates high
 *   uniform patent protection with narrow, exceptionally-justified
 *   flexibilities, in order to preserve the innovation incentive that funds
 *   costly drug R&D. Under this reading, compulsory licensing and parallel
 *   importation are read as constrained emergency valves, not routine tools,
 *   and the 2001 Doha Declaration's 'flexibility' language is treated as
 *   clarifying rather than expanding the baseline exclusivity commitment.
 *   This is one of three linked readings of a single contested kernel
 *   (trips_agreement_interpretive_kernel); the sibling
 *   public_health_flexibility_reading authors the same treaty text with a
 *   different ε, a different beneficiary/victim structure, and a different
 *   classification — that divergence is the point of decomposing rather than
 *   averaging.
 *
 * KEY AGENTS:
 *   - originator_pharmaceutical_firms: Primary beneficiary (institutional/arbitrage) — collects exclusivity rents under narrow-flexibility construction
 *   - high_income_state_trade_negotiators: Agenda-setter (institutional/arbitrage) — drafts and defends the reading in WTO and bilateral fora
 *   - low_income_state_governments: Primary target (moderate/constrained) — bears compliance costs and diplomatic pressure for deviation
 *   - patients_in_low_and_middle_income_countries: Primary victim (powerless/trapped) — bears price consequences directly
 *   - generic_drug_manufacturers: Secondary target (moderate/constrained) — market entry blocked
 *   - wto_dispute_settlement_panels: Secondary institutional actor (institutional/analytical) — adjudicates which reading governs a given dispute
 *   - public_health_advocacy_coalitions: Excluded voice (organized/constrained) — argues for the sibling reading from outside the formal process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.72).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.68).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Agreement — Strong Exclusivity Reading (High Uniform Patent Protection)").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/public_health_policy/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'a7063ba1-e994-464c-9f7d-98a55671b640').
narrative_ontology:cs_kernel_codification('a7063ba1-e994-464c-9f7d-98a55671b640', fixed_text).
narrative_ontology:cs_authority_grounding('a7063ba1-e994-464c-9f7d-98a55671b640', extraction).
narrative_ontology:cs_interpretation_layer_present('a7063ba1-e994-464c-9f7d-98a55671b640').
narrative_ontology:cs_reading_relation('a7063ba1-e994-464c-9f7d-98a55671b640', trips_agreement_interpretive_kernel__public_health_flexibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7063ba1-e994-464c-9f7d-98a55671b640', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('a7063ba1-e994-464c-9f7d-98a55671b640', foundational, exclusivity_scope_proportionate_to_innovation_incentive).
narrative_ontology:cs_axiom_status(exclusivity_scope_proportionate_to_innovation_incentive, holdable).
narrative_ontology:cs_axiom_grounding('a7063ba1-e994-464c-9f7d-98a55671b640', exclusivity_scope_proportionate_to_innovation_incentive, instrumental).
narrative_ontology:cs_axiom('a7063ba1-e994-464c-9f7d-98a55671b640', foundational, compulsory_licensing_as_exceptional_not_routine).
narrative_ontology:cs_axiom_status(compulsory_licensing_as_exceptional_not_routine, holdable).
narrative_ontology:cs_axiom_grounding('a7063ba1-e994-464c-9f7d-98a55671b640', compulsory_licensing_as_exceptional_not_routine, conventional).
narrative_ontology:cs_reference_frame('a7063ba1-e994-464c-9f7d-98a55671b640', uruguay_round_negotiated_baseline).
narrative_ontology:cs_drift_state('a7063ba1-e994-464c-9f7d-98a55671b640', post_doha_declaration_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a7063ba1-e994-464c-9f7d-98a55671b640', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, originator_pharmaceutical_firms).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_state_trade_negotiators).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_state_governments).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_low_and_middle_income_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold patents on new medicines and rely on the strong-exclusivity reading of TRIPS to secure 20-year market exclusivity across member states, blocking generic entry during the patent term. They lobby actively for narrow construction of Article 31 compulsory licensing grounds and Article 30 exceptions, and fund the trade-negotiator infrastructure that maintains this reading in dispute settlement.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, originator_pharmaceutical_firms, beneficiary,
    institutional, generational, arbitrage, global).

% Draft and defend the strong-exclusivity reading in WTO fora and bilateral trade agreements (TRIPS-plus provisions), treating high uniform protection as the textually correct baseline and public-health flexibilities as narrow exceptions requiring exceptional justification. They administer this reading through USTR Special 301 pressure and EU trade chapters, and can revise their negotiating posture but bear none of the access costs themselves.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_state_trade_negotiators, agenda_setter,
    institutional, generational, arbitrage, global).

% Must implement TRIPS-compliant patent law to retain WTO membership benefits and avoid trade retaliation, even where domestic public-health need calls for earlier generic entry. Invoking compulsory licensing under this reading is treated as an extraordinary, contestable act inviting diplomatic and trade pressure, not a routine flexibility — exit means accepting isolation from preferential trade arrangements.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_state_governments, payer,
    moderate, biographical, constrained, national).

% Face originator prices for patented medicines (antiretrovirals, cancer therapies, hepatitis C cures) that are unaffordable relative to local income, with no ability to access earlier generic versions where the strong-exclusivity reading holds. Their only leverage is state action they cannot compel and cross-border activism they cannot fund.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_low_and_middle_income_countries, payer,
    powerless, immediate, trapped, national).

% Are barred from manufacturing and exporting bioequivalent versions of patented drugs until patent expiry under this reading's narrow construction of compulsory licensing grounds, even where they have the manufacturing capacity to supply low-income markets immediately. They can lobby for TRIPS flexibilities and litigate at the margins but cannot manufacture without licensing.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_manufacturers, payer,
    moderate, biographical, constrained, global).

% Adjudicate disputes over which reading of TRIPS text governs a given national measure, drawing on prior panel reports and the Doha Declaration. Their rulings can entrench or loosen the strong-exclusivity reading depending on how narrowly they construe Articles 30 and 31, making them a secondary site of contest over which reading of the kernel prevails in a given case.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_settlement_panels, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_settlement_panels, agenda_setter).

% Argue for the broad flexibility reading (compulsory licensing, parallel imports, Doha-style public health carve-outs) but have no formal seat in WTO dispute panels or TRIPS Council negotiations; they operate through amicus submissions, shaming campaigns, and pressure on national governments rather than direct participation in the interpretive process this reading controls.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_advocacy_coalitions, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__strong_exclusivity_reading, originator_pharmaceutical_firms).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__strong_exclusivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable, harmonized patent standard across WTO members so that pharmaceutical firms can plan R&D investment against a stable global exclusivity horizon rather than negotiating protection country by country.
% TRANSFER_FUNCTION: Moves the price of pharmaceutical access from originator firms' R&D-recoupment ledger onto low-income state health budgets and individual patients, by extending market exclusivity and narrowing the conditions under which cheaper generic supply can lawfully enter.
% ABSENT_VOICES: Patients in the low-income countries who bear the price consequences most directly have no seat in TRIPS Council negotiations or WTO dispute panels; public health advocacy coalitions attempt to represent this interest but do so from outside the formal treaty-interpretation process, which this reading's proponents control.
% DISAPPEARANCE_RATIONALE: If the strong-exclusivity reading were displaced overnight by the flexibility reading becoming dominant, compulsory licensing would become a routine administrative tool rather than an exceptional, diplomatically costly act; generic manufacturers would enter markets years earlier; originator firms would face earlier price competition and would very likely restructure R&D-investment expectations around a shorter effective exclusivity window.
% FOUNDING_PROBLEM: In the pre-TRIPS era, many states (including some now-industrialized ones) offered no or weak pharmaceutical patent protection, which originator firms and their home governments argued undercut the incentive to invest in costly, risky drug R&D and allowed 'free-riding' by process-patent-only jurisdictions.
% FOUNDING_PROBLEM_CORROBORATION: Originator firms and high-income state negotiators attest the innovation-incentive problem remains live and requires strong uniform protection. Independent health economists, the WHO, and the 2001 Doha Declaration's own text (negotiated by the full WTO membership, not just originator-aligned parties) attest that the innovation-incentive rationale does not require the narrow-flexibility construction this reading insists on — the broad-flexibility reading claims the same founding problem is compatible with routine compulsory licensing, meaning the 'narrow flexibilities' feature is a policy choice layered onto the founding problem, not a requirement flowing from it.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.72, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored high (0.72 by 2025) because under this reading the price premium patients and states pay tracks the exclusivity term and the narrowness of the compulsory-licensing exception, not any independently verified R&D cost recovery figure — the reading itself asserts the exclusivity is proportionate to the innovation incentive, but the metric tracks what the arrangement actually extracts, which has risen as TRIPS-plus bilateral provisions (data exclusivity, patent term extensions) have layered onto the WTO floor. Suppression (0.68) reflects that deviation from this reading invites trade retaliation risk (Special 301, WTO panel proceedings), not persuasion. Theater ratio is comparatively low (0.28) because the enforcement machinery (patent offices, TRIPS Council review, dispute panels) performs a genuine coordination function — harmonized filing and disclosure standards are real — even as the exclusivity-scope component of that same machinery does the extractive work.
 *
 * DIRECTIONALITY LOGIC:
 *   Originator firms and high-income negotiators sit near the full-beneficiary end: they collect the exclusivity rent (firms) or control the interpretive agenda without bearing implementation cost (negotiators). Low-income states sit as constrained targets — nominal sovereign power but exit means forfeiting WTO-linked trade benefits. Patients are the least powerful and most trapped: no seat in the interpretive process, no alternative supply channel while the narrow reading holds, and immediate time horizon (illness does not wait for a Doha-style declaration). Generic manufacturers are commercially motivated payers with some lobbying capacity but no manufacturing right until licensing terms open. This is exactly the seat-divergence the Tangled Rope classification is meant to hold: real coordination benefit (harmonized filing, predictable global IP baseline) coexists with asymmetric extraction (patients and low-income states pay through the same structure that gives originator firms their planning certainty).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — underinvestment in drug R&D absent enforceable exclusivity — was real in 1995 and remains partially live (novel therapeutics are still costly to develop). But the strong-exclusivity reading's narrow construction of flexibilities is not required by that founding problem; the Doha Declaration itself, negotiated by the full WTO membership, establishes that the same founding problem is compatible with a much broader flexibility regime. This is the seam the sibling public_health_flexibility_reading occupies. Treating narrow flexibility as inseparable from the innovation incentive is the move this story's classification exists to test — labeling the entire treaty a Mountain (necessary natural consequence of innovation economics) would hide the tangled-rope structure; labeling it a pure Snare would erase the genuine, still-partially-live coordination function (harmonized global patent filing, reduced duplication of examination). Tangled Rope holds both facts simultaneously without dissolving either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_incentive_empirical_warrant,
    'Does the narrow-flexibility construction of TRIPS actually produce more pharmaceutical R&D investment than the broad-flexibility construction would, or is the innovation-incentive claim doing more rhetorical than causal work?',
    'Comparative analysis of R&D investment trajectories in therapeutic areas before and after TRIPS-plus provisions, controlled for market size, against jurisdictions that adopted broader compulsory-licensing practice (e.g., post-2001 African CL invocations) without observable R&D contraction.',
    'If narrow construction shows no marginal R&D benefit over broad construction, the strong-exclusivity reading''s extraction is unmoored from its own stated justification, strengthening the case that this reading functions as rent-preservation dressed as innovation policy. If a genuine marginal benefit is found, the coordination function this story attributes to the reading is more substantial than the sibling reading credits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_empirical_warrant, empirical, 'Whether narrow TRIPS flexibility construction causally increases pharmaceutical R&D relative to broad construction.').

omega_variable(
    kernel_disagreement_locus,
    'Where exactly does this reading structurally diverge from the public_health_flexibility_reading — is it the text of Articles 30-31 themselves, or the interpretive weight given to the Doha Declaration as clarifying versus expanding that text?',
    'Textual and negotiating-history analysis of whether the 2001 Doha Declaration was intended by its drafters as an authoritative interpretation (binding gloss) or a political statement without formal interpretive force under WTO law.',
    'If Doha is authoritative interpretation, the strong-exclusivity reading''s narrow construction is harder to sustain as the textually correct baseline reading, favoring the sibling reading''s classification. If Doha is non-binding political statement, this reading''s narrow construction retains stronger textual footing, and the extraction it authors is closer to what the treaty text itself compels rather than a chosen interpretive overreach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_locus, conceptual, 'Whether the sibling readings diverge on treaty text itself or on the interpretive status of the Doha Declaration.').

omega_variable(
    generic_manufacturing_capacity_counterfactual,
    'Absent this reading''s narrow construction, would generic manufacturers in fact have supplied low-income markets promptly, or do manufacturing capacity, regulatory approval timelines, and quality-assurance infrastructure constrain access independently of the patent regime?',
    'Case study of jurisdictions that issued compulsory licenses (Thailand, India, Rwanda-Canada Paragraph 6 mechanism) and measured time-to-market for generic alternatives against theoretical manufacturing capacity.',
    'If non-patent constraints (manufacturing capacity, regulatory approval) are the binding constraint rather than the patent exclusivity itself, the victim-side extraction this story attributes to the strong-exclusivity reading is partially overstated — some of the harm is attributable to independent infrastructure gaps, not the reading''s interpretive choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generic_manufacturing_capacity_counterfactual, empirical, 'Whether patent exclusivity or independent manufacturing/regulatory capacity is the binding constraint on generic drug access timing.').


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
narrative_ontology:measurement(trip_tr_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(trip_tr_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2020, 0.26).
narrative_ontology:measurement(trip_tr_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(trip_be_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(trip_be_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(trip_be_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2001, 0.52).
narrative_ontology:measurement(trip_su_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(trip_su_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2020, 0.66).
narrative_ontology:measurement(trip_su_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.1).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of trips_agreement_interpretive_kernel. strong_exclusivity_reading (this story, Tangled Rope) and public_health_flexibility_reading (sibling, expected Rope-leaning or lower-extraction Tangled Rope) author the same treaty text with different beneficiary/victim structures and different ε — they are not the same constraint viewed from two angles but two structurally distinct constraints sharing a kernel. dispute_settlement_interpretive_authority is the procedural layer determining WHICH substantive reading a given WTO panel enforces in a given dispute; it structurally influences both substantive readings by determining which one gets institutional teeth in any particular case.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
