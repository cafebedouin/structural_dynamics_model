% ============================================================================
% CONSTRAINT STORY: digital_money_origin__regulatory_recognition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Regulatory Recognition Threshold for Digital Money
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story captures the regulatory_recognition_reading of the
 *   digital_money_origin kernel: the claim that digital money 'emerged' only
 *   when monetary authorities formally incorporated it into statistical
 *   aggregates (M1/M2/M3) and regulatory frameworks (licensing, settlement
 *   access, reporting). This is the latest-origin reading — it places
 *   emergence at the moment of state recognition, not at technical conception
 *   or first practical use. The constraint is the regulatory perimeter
 *   itself: the legal/statistical boundary that determines what counts as
 *   money. It operates as a tangled rope — genuine coordination (monetary
 *   policy needs a measurable money supply) combined with asymmetric
 *   extraction (incumbents benefit from barriers that exclude innovators).
 *   The measurement series shows rising extractiveness and suppression from
 *   1970–2020 as the regulatory perimeter hardened around the incumbents'
 *   business model while technology lowered the natural cost of issuing
 *   digital monetary instruments.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.65).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.75).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Regulatory Recognition Threshold for Digital Money").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, 'f53b5675-480f-498d-9287-562a123c87b0').
narrative_ontology:cs_kernel_codification('f53b5675-480f-498d-9287-562a123c87b0', formalized).
narrative_ontology:cs_authority_grounding('f53b5675-480f-498d-9287-562a123c87b0', extraction).
narrative_ontology:cs_interpretation_layer_present('f53b5675-480f-498d-9287-562a123c87b0').
narrative_ontology:cs_reading_relation('f53b5675-480f-498d-9287-562a123c87b0', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('f53b5675-480f-498d-9287-562a123c87b0', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_axiom('f53b5675-480f-498d-9287-562a123c87b0', foundational, legal_recognition_constitutes_monetary_existence).
narrative_ontology:cs_axiom_status(legal_recognition_constitutes_monetary_existence, holdable).
narrative_ontology:cs_axiom_grounding('f53b5675-480f-498d-9287-562a123c87b0', legal_recognition_constitutes_monetary_existence, conventional).
narrative_ontology:cs_axiom('f53b5675-480f-498d-9287-562a123c87b0', secondary, regulatory_perimeter_defines_monetary_boundary).
narrative_ontology:cs_axiom_status(regulatory_perimeter_defines_monetary_boundary, holdable).
narrative_ontology:cs_axiom_grounding('f53b5675-480f-498d-9287-562a123c87b0', regulatory_perimeter_defines_monetary_boundary, conventional).
narrative_ontology:cs_reference_frame('f53b5675-480f-498d-9287-562a123c87b0', regulatory_monetary_order).
narrative_ontology:cs_drift_state('f53b5675-480f-498d-9287-562a123c87b0', contemporary_fintech_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f53b5675-480f-498d-9287-562a123c87b0', '').
narrative_ontology:cs_kernel_id(digital_money_origin__regulatory_recognition_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unregulated_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, general_public_users).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, general_public_users).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, financial_stability_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central banks and financial regulators define what counts as money in official statistics (M1, M2, M3) and grant licenses for payment systems. They enforce the regulatory perimeter through reporting requirements, capital rules, and access to settlement infrastructure. Their mandate is monetary stability, but their position lets them set the boundary between 'money' and 'not-money'.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Established banks and card networks benefit from regulatory barriers that raise entry costs for competitors. They hold privileged access to central bank settlement, deposit insurance, and the statistical aggregates that define the monetary system. The regulatory recognition constraint protects their franchise value and pricing power in payments and credit.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Fintech startups, crypto projects, and novel payment systems face high compliance costs, licensing uncertainty, and exclusion from official monetary statistics. They must either seek regulatory approval (slow, expensive, uncertain) or operate outside the perimeter (limited scalability, no deposit insurance, statistical invisibility). The constraint extracts time, capital, and legitimacy from them.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_innovators, payer,
    moderate, biographical, constrained, global).

% Users gain stability, deposit insurance, and interoperability from the regulated monetary system. They also bear the cost of slower innovation, higher fees, and reduced choice when regulatory barriers protect incumbents. Their exit is constrained by network effects and legal tender laws.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, general_public_users, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, general_public_users, payer).

% Communities using local currencies, mutual credit, or crypto-native systems operate outside regulatory recognition. They would argue that money emerges from social agreement, not state sanction, but their voice is absent from statistical aggregates and policy forums. Their exclusion is structural — the regulatory framework does not have a seat for them.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, alternative_monetary_communities, excluded,
    powerless, generational, identity_locked, local).

% Scholars who study the history of money and the evolution of monetary systems. They observe the contest between readings of when digital money 'really' emerged and analyze the structural consequences of each origin narrative for current policy.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_historians_economists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, authoritative definition of the money supply for monetary policy, financial stability monitoring, and cross-border comparability. Standardizes what counts as money so that central banks can steer inflation, regulators can supervise risk, and statisticians can produce consistent aggregates.
% TRANSFER_FUNCTION: Moves regulatory compliance costs, licensing fees, and competitive advantage from unregulated innovators to incumbent institutions. The regulatory perimeter acts as a toll gate: innovators pay in time, capital, and restricted market access; incumbents collect in protected market position and lower competitive pressure.
% ABSENT_VOICES: Alternative monetary communities (local currency networks, crypto-native economies, mutual credit systems) and unlicensed innovators in jurisdictions with restrictive regimes. They are excluded because the regulatory framework only recognizes entities that seek its permission; there is no 'voice' channel for those who reject the premise that permission is needed.
% DISAPPEARANCE_RATIONALE: If the regulatory recognition constraint vanished overnight, the boundary between 'money' and 'not-money' would become contested in practice. Multiple parallel monetary systems would compete for acceptance, statistical aggregates would fragment, central banks would lose a clean policy transmission channel, and incumbents would face immediate competitive pressure from unlicensed alternatives. The monetary order would reorganize around usage and trust rather than legal designation.
% FOUNDING_PROBLEM: The breakdown of metallic standards and the rise of book-entry banking created a need for authorities to define and measure the money supply in a fiat system. Without a clear statistical boundary, monetary policy had no reliable target, and financial stability monitoring had no perimeter.
% FOUNDING_PROBLEM_CORROBORATION: Central bank histories and BIS documents attest the problem was live in the 1970s–1990s when aggregates were formalized. Fintech advocates and some monetary economists (e.g., Selgin, White, and the 'free banking' school) argue the problem is substantially solved by technology — real-time ledger data makes statistical recognition separable from regulatory permission — and the constraint now persists as barrier maintenance. No single corroborating source outside the beneficiary set is universally accepted; the contest is the signal.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__regulatory_recognition_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.65) reflects the growing gap between the marginal cost of digital issuance (near zero) and the regulatory cost imposed on non-incumbents. Suppression (0.75) is high because the constraint actively excludes — unlicensed systems cannot access settlement, cannot appear in aggregates, and face enforcement risk. Theater ratio (0.4) acknowledges the real coordination function (policy needs a measurable aggregate) while noting that a rising share of regulatory complexity serves barrier maintenance, not measurement. Accessibility collapse (0.65) and resistance (0.7) capture that alternatives exist (crypto, stablecoins, local currencies) but face structural suppression, and innovators actively resist through lobbying, regulatory arbitrage, and parallel infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   From the authority/incumbent seat, the constraint is a rope — genuine coordination for monetary stability. From the innovator seat, it is a snare — the coordination story is cover for rent extraction. The engine computes this divergence from the structural data; the authored claim (tangled_rope) acknowledges both functions are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities are the agenda-setters (d near beneficiary end — they write the rules and hold the settlement keys). Incumbent institutions are beneficiaries (d ~0.15 — they collect rents from the barrier). Unregulated innovators are payers (d ~0.85 — they bear compliance costs and exclusion). General public sits near symmetric (d ~0.5 — genuine stability benefit, diffuse innovation cost). Alternative communities are excluded (not in the directionality derivation — they are outside the constraint's formal scope but structurally affected). Observers are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defining the money supply for fiat policy) was live in the 1970s. Technology has since made real-time, permissionless measurement possible (blockchain analytics, open banking APIs, high-frequency transaction data). The constraint persists because the regulatory perimeter now protects incumbent franchise value, not because the measurement problem remains unsolved. This is mandatrophy: the mandate (measure the money supply) has outlived its original technical necessity, but the constraint (regulatory recognition as the gate) remains and has acquired extractive function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the regulatory_recognition_reading a distinct constraint from the other two readings of digital_money_origin, or a different measurement of the same constraint?',
    'Apply the ε-invariance test: if changing the ''origin moment'' definition changes the beneficiary/victim structure, extractiveness profile, and enforcement requirements, they are distinct constraints. The kernel context asserts they are distinct — this omega documents that commitment and its test.',
    'If distinct, each reading gets its own ε, stakeholders, and classification. If same constraint, the framework must model origin-moment as a measurement parameter (which DP-001 forbids). The kernel contest structure assumes distinctness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three origin narratives instantiate three constraints or one constraint with three observables.').

omega_variable(
    sibling_delta_coordination_function,
    'Does the became_thinkable_reading have a coordination function (conceptual standardization) and the first_held_reading have a coordination function (network bootstrapping), and if so, how do their extraction profiles differ from this reading''s regulatory coordination?',
    'Author the two sibling constraint stories and compare their base_properties. The coordination function of each reading maps to a different coordination_type: became_thinkable → information_standard; first_held → attachment_coordination or resource_allocation; regulatory_recognition → enforcement_mechanism.',
    'If sibling constraints have genuinely different coordination types and extraction profiles, the kernel family exhibits structural diversification — the ''origin of digital money'' is not a single coordination problem solved three ways, but three different problems each claiming the origin label.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_delta_coordination_function, empirical, 'Whether the three readings correspond to three structurally distinct coordination problems.').

omega_variable(
    regulatory_capture_vs_stability,
    'Is the rising extractiveness (0.35→0.65) and suppression (0.4→0.75) over 1970–2020 driven by genuine stability needs (e.g., shadow banking risk) or by incumbent capture of the regulatory process?',
    'Counterfactual: if a regulator today designed a money-supply measurement system from scratch with current technology (distributed ledgers, API standards, real-time reporting), would it require the same licensing perimeter? Compare with jurisdictions that have open banking / data-sharing mandates (UK, EU PSD2).',
    'If stability-driven, the constraint remains tangled_rope with a large coordination component. If capture-driven, it trends toward snare — the coordination story becomes thinner cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_vs_stability, preference, 'Whether the constraint''s trajectory reflects adaptive coordination or regulatory capture.').

omega_variable(
    statistical_visibility_as_coordination,
    'Does appearance in official monetary aggregates (M1/M2/M3) genuinely coordinate economic activity (by enabling policy), or does it primarily confer legitimacy rents on included instruments?',
    'Natural experiment: when a new instrument class is added to aggregates (e.g., money market funds in the 1990s, stablecoins if ever added), does its usage expand because of policy transmission improvements, or because of the legitimacy signal? Measure adoption curves before/after statistical inclusion controlling for other factors.',
    'If statistical visibility primarily confers legitimacy rents, the coordination function is smaller than claimed — the constraint extracts more than it coordinates. If it genuinely improves policy transmission, the coordination function is substantial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(statistical_visibility_as_coordination, empirical, 'Whether statistical recognition coordinates or merely legitimates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1970, digital_money_origin__regulatory_recognition_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(digi_tr_t1980, digital_money_origin__regulatory_recognition_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(digi_tr_t1990, digital_money_origin__regulatory_recognition_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(digi_tr_t2010, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(digi_tr_t2020, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(digi_be_t1970, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(digi_be_t1980, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(digi_be_t2010, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(digi_be_t2020, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1970, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(digi_su_t1980, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(digi_su_t2010, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(digi_su_t2020, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(digital_money_origin__regulatory_recognition_reading, 0.1).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__first_held_reading).

% DUAL FORMULATION NOTE:
% This reading (regulatory_recognition) is the downstream member of the digital_money_origin kernel family. The became_thinkable_reading (conceptual origin) and first_held_reading (practical origin) are upstream — they establish the conceptual and practical possibility that the regulatory reading then gates. The upstream readings influence this one: if digital money was never thinkable or never held, there would be nothing to regulate. This reading does not foreclose the others (they can coexist as earlier milestones) but it creates structural pressure on them by claiming the 'true' origin label for policy purposes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
