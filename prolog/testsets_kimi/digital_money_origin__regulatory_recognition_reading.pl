% ============================================================================
% CONSTRAINT STORY: digital_money_origin__regulatory_recognition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__regulatory_recognition_reading, []).

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
 *   constraint_id: digital_money_origin__regulatory_recognition_reading
 *   human_readable: State Regulatory Recognition as Monetary Origin
 *   domain: monetary_history/institutional_economics
 *
 * SUMMARY:
 *   This constraint treats digital money as having emerged only when monetary
 *   authorities formally incorporated it into statistical aggregates and
 *   regulatory frameworks. The reading is contested by alternative narratives
 *   locating origin in conceptual feasibility or first social holding.
 *   Structurally, the constraint creates a legal-regulatory perimeter that
 *   benefits incumbent financial institutions by imposing licensing and
 *   compliance costs on unregulated innovators. It is claimed as a
 *   tangled_rope because the statistical and prudential coordination is real,
 *   while the asymmetric extraction through incumbent-favoring barriers is
 *   equally real.
 *
 * KEY AGENTS:
 *   - Central banks and regulators (agenda_setter, institutional/constrained): define and enforce the regulatory perimeter
 *   - Incumbent financial institutions (beneficiary, powerful/constrained): capture rents from licensed market access
 *   - Unregulated innovators (payer, moderate/constrained): bear compliance costs and exclusion from monetary status
 *   - Crypto payment networks (excluded, powerless/trapped): structurally barred from recognition and consultation
 *   - Monetary economists (observer, analytical): provide competing analytical frames for monetary origin
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.65).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.71).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "State Regulatory Recognition as Monetary Origin").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "monetary_history/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, '8992bd9c-55da-4e7d-b7c3-cb194779de6a').
narrative_ontology:cs_kernel_codification('8992bd9c-55da-4e7d-b7c3-cb194779de6a', formalized).
narrative_ontology:cs_authority_grounding('8992bd9c-55da-4e7d-b7c3-cb194779de6a', lineage).
narrative_ontology:cs_interpretation_layer_present('8992bd9c-55da-4e7d-b7c3-cb194779de6a').
narrative_ontology:cs_reading_relation('8992bd9c-55da-4e7d-b7c3-cb194779de6a', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('8992bd9c-55da-4e7d-b7c3-cb194779de6a', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_axiom('8992bd9c-55da-4e7d-b7c3-cb194779de6a', foundational, state_recognition_constitutes_monetary_origin).
narrative_ontology:cs_axiom_status(state_recognition_constitutes_monetary_origin, holdable).
narrative_ontology:cs_axiom_grounding('8992bd9c-55da-4e7d-b7c3-cb194779de6a', state_recognition_constitutes_monetary_origin, conventional).
narrative_ontology:cs_axiom('8992bd9c-55da-4e7d-b7c3-cb194779de6a', secondary, regulatory_frameworks_enable_financial_stability).
narrative_ontology:cs_axiom_status(regulatory_frameworks_enable_financial_stability, holdable).
narrative_ontology:cs_axiom_grounding('8992bd9c-55da-4e7d-b7c3-cb194779de6a', regulatory_frameworks_enable_financial_stability, instrumental).
narrative_ontology:cs_reference_frame('8992bd9c-55da-4e7d-b7c3-cb194779de6a', state_monetary_sovereignty_framework).
narrative_ontology:cs_drift_state('8992bd9c-55da-4e7d-b7c3-cb194779de6a', post_crypto_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8992bd9c-55da-4e7d-b7c3-cb194779de6a', '').
narrative_ontology:cs_kernel_id(digital_money_origin__regulatory_recognition_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unregulated_innovators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define digital money into existence for official purposes by incorporating it into monetary aggregates (M1/M2) and erecting licensing, reporting, and prudential frameworks. They enforce compliance through banking law and coordinate internationally via standard-setting bodies.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, central_banks_and_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Receive regulatory legitimacy for their digital payment products while money-transmitter licensing, capital requirements, and statistical reporting burdens structurally disadvantage non-bank competitors. They participate in rule-making consultations and capture the rents of a licensed market.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, beneficiary,
    powerful, generational, constrained, global).

% Develop digital payment and currency technologies but face banking-license requirements and statistical exclusion that prevent their products from achieving monetary status unless they incumbize. They bear compliance costs, legal ambiguity, or outright exclusion from the payment rails.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_innovators, payer,
    moderate, biographical, constrained, national).

% Operate decentralized digital payment systems that are formally excluded from monetary aggregates and regulatory recognition. They are structurally barred from the drafting process and their existence is treated as non-monetary or illicit by the framework.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, crypto_payment_networks, excluded,
    powerless, biographical, trapped, global).

% Study and debate competing origin claimsâstate recognition, technological feasibility, or social practice. They provide analytical frameworks that legitimate or contest the regulatory reading without being bound by its enforcement.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:fixing_cost_class(digital_money_origin__regulatory_recognition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregating digital money into macroeconomic statistics and establishing prudential regulatory standards so that digital payment instruments are interoperable, measurable, and subject to financial stability oversight.
% TRANSFER_FUNCTION: Moves regulatory legitimacy and market access from unregulated issuers to licensed financial institutions, while concentrating the authority to define money in state monetary authorities.
% ABSENT_VOICES: Users of excluded digital money systems and non-bank fintech innovators are largely absent from the regulatory drafting process; they would contest the premise that state recognition is constitutive of monetary reality.
% DISAPPEARANCE_RATIONALE: If the regulatory recognition framework vanished, incumbent banks would lose their licensing advantage, unregulated innovators could enter payment markets without compliance barriers, and the statistical visibility of digital money would fragmentâthe monetary hierarchy would reorganize around market adoption rather than state classification.
% FOUNDING_PROBLEM: Digital money outside regulatory frameworks created macroeconomic blind spots, tax compliance gaps, and consumer protection risks in an unregulated payment frontier.
% FOUNDING_PROBLEM_CORROBORATION: Monetary authorities and international financial institutions (BIS, IMF) attest that statistical inclusion solves macroeconomic visibility. Fintech trade associations and cryptocurrency developers outside the beneficiary set attest that the problem was exaggerated to justify incumbent-protecting barriers; independent legal scholarship documents regulatory capture dynamics.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__regulatory_recognition_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__regulatory_recognition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__regulatory_recognition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is authored at moderately high levels because the constraint moves substantial resources and market access from unregulated actors to licensed incumbents through active legal barriers. Suppression (0.71) exceeds extraction because the framework's persistence depends on ongoing enforcement of banking laws and money-transmitter statutes. Theater_ratio (0.38) reflects that a meaningful share of regulatory activity has shifted toward performative compliance and box-checking rather than genuine risk mitigation. Accessibility_collapse (0.48) captures that gray-market alternatives persist but are increasingly risky and delegitimized. Resistance (0.55) reflects sustained contestation from fintech and cryptocurrency communities. Measurements share a single time grid to prevent spurious transition dating.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (monetary authorities) should compute toward coordination-heavy types because the framework solves genuine statistical aggregation and stability oversight problems. The payer seat (unregulated innovators) should compute toward extraction-heavy types because the same framework strips them of legitimacy and loads them with compliance costs. The beneficiary seat (incumbent banks) sits betweenâgaining rents but also bearing regulatory constraints. The engine derives this divergence from the same structural data rather than from tuned metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and regulators have low directionality (d near 0.0) because the constraint expresses their authority and mandate. Incumbent financial institutions have moderately low directionality (d ~0.25) because they capture rents but are also regulated by the same framework. Unregulated innovators have high directionality (d ~0.85) because the constraint extracts compliance costs and market access from them. Crypto payment networks, though excluded, would register even higher d if treated as targets of suppression. The derivation follows beneficiary/victim declarations and exit modulation without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 genealogy prevents mislabeling this constraint as pure extraction (snare) by documenting a contested but genuine founding problem: macroeconomic visibility and consumer protection in an unregulated digital frontier. However, the founding_problem_status is contested, and corroboration comes partly from outside the beneficiary set, signaling that the mandate may have outlived its original justification or been captured. The temporal measurements show extraction rising over time while the theater ratio climbs, consistent with coordination atrophying into rent-seeking. If the founding problem were unequivocally dead, the constraint would drift toward piton or snare; its contested status keeps it in tangled_rope territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does the regulatory_recognition_reading of digital money origin foreclose alternative origin narratives, or merely coexist with them as a state-centric framing?',
    'Discourse analysis of monetary economics curricula and central bank communications: if state recognition is treated as definitionally necessary for monetary status, the reading forecloses siblings; if treated as one institutional milestone among many, it coexists.',
    'If foreclosure is documented, the constraint functions as a stronger commitment system with higher extraction potential; if coexistence, extraction is moderated by competing narratives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural relationship of this kernel reading to sibling origin narratives').

omega_variable(
    regulatory_barrier_necessity,
    'Are the legal and regulatory barriers defining this reading necessary for monetary and financial stability, or do they primarily serve incumbent protection?',
    'Comparative natural experiment across jurisdictions with varying licensing intensity: measure relationship between barrier strictness and stability outcomes, controlling for income and financial depth.',
    'If barriers show no stability return, the coordination story is cover and the constraint shifts toward snare; if returns are demonstrable, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_barrier_necessity, empirical, 'Whether regulatory barriers serve coordination or extraction').

omega_variable(
    enforcement_intensification,
    'Does the rising extraction trajectory reflect genuine regulatory learning about digital money risks, or an enforcement ratchet protecting entrenched interests?',
    'Trace the regulatory agenda-setting process through legislative history and lobbying disclosures; correlate extraction metric increases with incumbent campaign contributions or revolving-door appointments.',
    'If correlated with incumbent influence, the temporal drift signals capture rather than public-interest evolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_intensification, empirical, 'Source of rising extractiveness over the measured interval').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dm_reg_rec_tr_t0, digital_money_origin__regulatory_recognition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dm_reg_rec_tr_t5, digital_money_origin__regulatory_recognition_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(dm_reg_rec_tr_t10, digital_money_origin__regulatory_recognition_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(dm_reg_rec_tr_t15, digital_money_origin__regulatory_recognition_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(dm_reg_rec_tr_t20, digital_money_origin__regulatory_recognition_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(dm_reg_rec_tr_t25, digital_money_origin__regulatory_recognition_reading, theater_ratio, 25, 0.36).
narrative_ontology:measurement(dm_reg_rec_tr_t30, digital_money_origin__regulatory_recognition_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(dm_reg_rec_be_t0, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dm_reg_rec_be_t5, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(dm_reg_rec_be_t10, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(dm_reg_rec_be_t15, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(dm_reg_rec_be_t20, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(dm_reg_rec_be_t25, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(dm_reg_rec_be_t30, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(dm_reg_rec_su_t0, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(dm_reg_rec_su_t5, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(dm_reg_rec_su_t10, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(dm_reg_rec_su_t15, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(dm_reg_rec_su_t20, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(dm_reg_rec_su_t25, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 25, 0.69).
narrative_ontology:measurement(dm_reg_rec_su_t30, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, first_held_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the digital_money_origin kernel. The kernel decomposes into three structurally distinct constraints because the natural-language label 'when digital money emerged' conflates claims with different epsilon values, beneficiary structures, and empirical status. Each reading has its own constraint_id, stakeholders, and classification, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
