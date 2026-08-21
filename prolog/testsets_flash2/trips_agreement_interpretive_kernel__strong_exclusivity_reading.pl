% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Agreement: Strong Patent Exclusivity Reading
 *   domain: international_trade_law/public_health_policy/intellectual_property_regime
 *
 * SUMMARY:
 *   This constraint represents the 'strong exclusivity' reading of the TRIPS
 *   Agreement, which interprets the text as mandating high uniform patent
 *   protections with narrow flexibilities. This reading prioritizes
 *   incentivizing pharmaceutical innovation through robust IP rights, often
 *   at the expense of public health access in developing countries. It is one
 *   reading of the broader TRIPS Agreement kernel, which is subject to
 *   ongoing interpretive contestation.
 *
 * KEY AGENTS:
 *   - pharmaceutical_innovators: Primary beneficiary (institutional/arbitrage)
 *   - developed_nations: Agenda-setter (institutional/mobile)
 *   - low_income_states: Payer (powerless/trapped)
 *   - patients_in_developing_countries: Payer (powerless/trapped)
 *   - wto_dispute_panels: Agenda-setter (institutional/constrained)
 *   - public_health_advocates: Excluded (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.85).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.78).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Agreement: Strong Patent Exclusivity Reading").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '2a42acc9-8f67-4962-aee7-f2b1ab30d707').
narrative_ontology:cs_kernel_codification('2a42acc9-8f67-4962-aee7-f2b1ab30d707', fixed_text).
narrative_ontology:cs_authority_grounding('2a42acc9-8f67-4962-aee7-f2b1ab30d707', lineage).
narrative_ontology:cs_interpretation_layer_present('2a42acc9-8f67-4962-aee7-f2b1ab30d707').
narrative_ontology:cs_reading_relation('2a42acc9-8f67-4962-aee7-f2b1ab30d707', trips_agreement_interpretive_kernel__public_health_flexibility_reading, influences).
narrative_ontology:cs_reading_relation('2a42acc9-8f67-4962-aee7-f2b1ab30d707', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, coexists_with).
narrative_ontology:cs_axiom('2a42acc9-8f67-4962-aee7-f2b1ab30d707', foundational, ip_rights_as_fundamental_incentive).
narrative_ontology:cs_axiom_status(ip_rights_as_fundamental_incentive, holdable).
narrative_ontology:cs_axiom_grounding('2a42acc9-8f67-4962-aee7-f2b1ab30d707', ip_rights_as_fundamental_incentive, instrumental).
narrative_ontology:cs_axiom('2a42acc9-8f67-4962-aee7-f2b1ab30d707', foundational, minimal_deviation_from_patent_monopoly).
narrative_ontology:cs_axiom_status(minimal_deviation_from_patent_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('2a42acc9-8f67-4962-aee7-f2b1ab30d707', minimal_deviation_from_patent_monopoly, conventional).
narrative_ontology:cs_reference_frame('2a42acc9-8f67-4962-aee7-f2b1ab30d707', strong_ip_protection_framework).
narrative_ontology:cs_drift_state('2a42acc9-8f67-4962-aee7-f2b1ab30d707', contemporary_public_health_crises, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2a42acc9-8f67-4962-aee7-f2b1ab30d707', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_innovators).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_nations).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_developing_countries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from extended patent monopolies and high prices for patented medicines, incentivizing R&D. They actively lobby for strict interpretation and enforcement of TRIPS provisions.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_innovators, beneficiary,
    institutional, generational, arbitrage, global).

% Advocate for strong IP protection, aligning with their domestic pharmaceutical industries. They use diplomatic and trade pressure to ensure compliance with this reading of TRIPS.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_nations, agenda_setter,
    institutional, generational, mobile, global).

% Bear the cost of high drug prices and limited access to generic medicines, straining public health budgets and impacting patient outcomes. Their ability to use flexibilities like compulsory licensing is severely constrained by this reading.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states, payer,
    powerless, immediate, trapped, global).

% Are direct victims of high drug prices, often lacking access to essential medicines due to patent protection. They have virtually no exit options from this structural constraint.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_developing_countries, payer,
    powerless, immediate, trapped, local).

% Interpret the TRIPS agreement and rule on disputes, often favoring strong IP protection in line with this reading, backed by trade retaliation mechanisms. Their rulings shape the practical application of the agreement.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_panels, agenda_setter,
    institutional, biographical, constrained, global).

% Argue for broader interpretation of public health flexibilities but face significant institutional barriers and lobbying power from IP holders. Their voice is often marginalized in formal dispute resolution.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_innovators).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__strong_exclusivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global baseline for intellectual property protection, aiming to harmonize patent laws across WTO member states to facilitate international trade and investment in innovation.
% TRANSFER_FUNCTION: Transfers significant economic value from consumers and public health systems in low-income countries to pharmaceutical companies and developed nations through patent-derived monopoly pricing.
% ABSENT_VOICES: Public health advocates, patient groups, and generic drug manufacturers are often excluded from the core interpretive processes of the WTO dispute settlement, where the strong exclusivity reading is solidified. They would argue for a more balanced interpretation prioritizing access.
% DISAPPEARANCE_RATIONALE: If the strong exclusivity reading of TRIPS vanished, low-income countries would immediately expand generic production and parallel imports, drastically lowering drug prices. Pharmaceutical R&D models would shift, potentially towards public funding or prize systems, and global health equity would improve significantly.
% FOUNDING_PROBLEM: Lack of uniform global IP protection was perceived to stifle innovation and create unfair competition, particularly for industries with high R&D costs like pharmaceuticals.
% FOUNDING_PROBLEM_CORROBORATION: Pharmaceutical innovators and developed nations continue to assert that strong IP is essential for R&D, citing the high costs and risks of drug development. However, public health organizations and economists (outside the benefiting parties) contest this, arguing that the current regime primarily serves rent extraction, not optimal innovation.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) reflects the significant economic rents captured by pharmaceutical companies due to extended patent monopolies. Suppression (0.78) is high due to the binding nature of WTO rulings and the threat of trade sanctions, which severely limit the ability of low-income states to implement public health flexibilities. Theater ratio is low (0.15) because the enforcement mechanisms are genuinely functional in upholding IP rights, rather than being merely performative. The claimed type is 'tangled_rope' because it provides a coordination function (global IP harmonization) but also involves significant asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pharmaceutical innovators and developed nations, this reading of TRIPS is a legitimate 'rope' that coordinates global innovation and trade. However, from the perspective of low-income states and patients, it operates as a 'snare' or 'tangled_rope' that extracts wealth and limits access to essential medicines. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical innovators and developed nations are clear beneficiaries, as the constraint directly subsidizes their R&D models and economic interests (low directionality). Low-income states and patients are clear targets, bearing the costs of high drug prices and restricted generic access (high directionality). WTO dispute panels, while acting as agenda-setters, often lean towards interpretations that reinforce strong IP, thus indirectly benefiting the innovators and developed nations.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'rope' (as claimed by some beneficiaries) by highlighting the substantial extraction and suppression involved. It also avoids mislabeling it as a pure 'snare' by acknowledging the genuine, albeit contested, coordination function of global IP harmonization. The 'tangled_rope' classification captures the hybrid nature, where a coordination mechanism is leveraged for asymmetric extraction, and its persistence relies on active enforcement rather than pure collective benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_health_flexibility_scope,
    'Does the TRIPS Agreement text inherently allow for broad public health flexibilities (e.g., compulsory licensing, parallel imports), or are these strictly limited exceptions?',
    'Further WTO Appellate Body rulings or a new round of multilateral negotiations explicitly clarifying the scope of public health safeguards.',
    'If flexibilities are found to be broad, the ''strong exclusivity'' reading would be weakened, potentially shifting the constraint towards a more balanced ''rope'' or even ''scaffold'' if the intent is truly transitional. If they are confirmed as narrow, the ''snare'' aspects of this reading would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_health_flexibility_scope, conceptual, 'Ambiguity in the TRIPS text regarding public health safeguards.').

omega_variable(
    innovation_incentive_efficacy,
    'To what extent do high uniform patent protections actually incentivize pharmaceutical innovation for global health needs, versus primarily enabling rent extraction?',
    'Empirical studies comparing innovation rates and types under different IP regimes, and analysis of R&D spending vs. marketing/profit margins in the pharmaceutical industry.',
    'If the link between strong IP and innovation for global health is weak, the ''coordination'' justification for this reading would be undermined, pushing it closer to a ''snare''. If strong, it would reinforce the ''tangled_rope'' classification by validating the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_efficacy, empirical, 'The effectiveness of strong IP in driving socially beneficial innovation.').

omega_variable(
    dispute_settlement_bias,
    'Is the WTO dispute settlement mechanism inherently biased towards interpretations that favor strong IP protection, or does it apply the TRIPS text neutrally?',
    'Analysis of historical dispute rulings, dissenting opinions, and the backgrounds of panel members. Examination of the influence of lobbying by IP-intensive industries on WTO processes.',
    'If a systemic bias is confirmed, the ''suppression'' metric for low-income states would be effectively higher, as their legal avenues for challenging the strong exclusivity reading are compromised. This would push the constraint further towards a ''snare'' from their perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dispute_settlement_bias, empirical, 'Potential bias in WTO dispute resolution towards strong IP.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(trip_tr_t2000, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(trip_tr_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2005, 0.17).
narrative_ontology:measurement(trip_tr_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2010, 0.16).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(trip_tr_t2024, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 1995, 0.7).
narrative_ontology:measurement(trip_be_t2000, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(trip_be_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(trip_be_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2015, 0.83).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(trip_be_t2024, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(trip_su_t2000, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(trip_su_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(trip_su_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2010, 0.76).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2020, 0.78).
narrative_ontology:measurement(trip_su_t2024, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel__public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the TRIPS Agreement interpretive kernel. This 'strong exclusivity' reading directly influences the practical application of the 'public health flexibility' reading and is adjudicated by the 'dispute settlement interpretive authority'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
