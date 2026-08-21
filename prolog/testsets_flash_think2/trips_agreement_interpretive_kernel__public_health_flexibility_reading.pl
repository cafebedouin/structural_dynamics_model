% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__public_health_flexibility_reading, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__public_health_flexibility_reading
 *   human_readable: TRIPS Public Health Flexibility Interpretation
 *   domain: international_trade_law/public_health_policy/intellectual_property_regime
 *
 * SUMMARY:
 *   This constraint story instantiates the
 *   'public_health_flexibility_reading' of the TRIPS Agreement interpretive
 *   kernel. This reading emphasizes that the TRIPS Agreement, particularly
 *   after the 2001 Doha Declaration, embeds broad flexibilities for WTO
 *   members to protect public health and promote access to medicines, notably
 *   through compulsory licensing and parallel imports. The metrics reflect a
 *   system that, while still involving some extraction from IP holders,
 *   actively coordinates public health access and requires enforcement to
 *   counter pressures for stronger IP exclusivity. The claimed type
 *   'tangled_rope' acknowledges both the genuine coordination function for
 *   public health and the asymmetric extraction from pharmaceutical patent
 *   holders whose maximal claims are curtailed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.45).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.6).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Public Health Flexibility Interpretation").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '4cd2909c-fbe9-499c-9a66-f6a6861ec9a9').
narrative_ontology:cs_kernel_codification('4cd2909c-fbe9-499c-9a66-f6a6861ec9a9', fixed_text).
narrative_ontology:cs_authority_grounding('4cd2909c-fbe9-499c-9a66-f6a6861ec9a9', lineage).
narrative_ontology:cs_interpretation_layer_present('4cd2909c-fbe9-499c-9a66-f6a6861ec9a9').
narrative_ontology:cs_reading_relation('4cd2909c-fbe9-499c-9a66-f6a6861ec9a9', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_reading_relation('4cd2909c-fbe9-499c-9a66-f6a6861ec9a9', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('4cd2909c-fbe9-499c-9a66-f6a6861ec9a9', foundational, public_health_is_a_human_right).
narrative_ontology:cs_axiom_status(public_health_is_a_human_right, holdable).
narrative_ontology:cs_axiom_grounding('4cd2909c-fbe9-499c-9a66-f6a6861ec9a9', public_health_is_a_human_right, deontological).
narrative_ontology:cs_axiom('4cd2909c-fbe9-499c-9a66-f6a6861ec9a9', foundational, flexibilities_are_inherent_to_trips).
narrative_ontology:cs_axiom_status(flexibilities_are_inherent_to_trips, holdable).
narrative_ontology:cs_axiom_grounding('4cd2909c-fbe9-499c-9a66-f6a6861ec9a9', flexibilities_are_inherent_to_trips, conventional).
narrative_ontology:cs_reference_frame('4cd2909c-fbe9-499c-9a66-f6a6861ec9a9', doha_declaration_spirit).
narrative_ontology:cs_drift_state('4cd2909c-fbe9-499c-9a66-f6a6861ec9a9', contemporary_pandemic_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4cd2909c-fbe9-499c-9a66-f6a6861ec9a9', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries_developing_countries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_developing_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, developed_country_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These ministries gain leverage to issue compulsory licenses and facilitate parallel imports, enabling access to essential medicines at lower costs for their populations. Their ability to exit the TRIPS framework is limited, but the flexibilities provide crucial internal maneuvering room.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries_developing_countries, beneficiary,
    organized, generational, constrained, national).

% These manufacturers benefit from the ability to produce and export generic versions of patented medicines under compulsory licenses, expanding their market and public health impact. They face legal challenges but operate within the framework of these flexibilities.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers, beneficiary,
    powerful, biographical, mobile, global).

% These companies bear the cost of reduced market exclusivity and pricing power in developing countries due to compulsory licensing and parallel imports. They actively lobby against broad interpretations of flexibilities and engage in legal challenges.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders, payer,
    institutional, generational, arbitrage, global).

% These individuals are the ultimate beneficiaries of increased access to affordable essential medicines, often in life-or-death situations. Their options are severely limited without the flexibilities provided by this interpretation.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_developing_countries, beneficiary,
    powerless, immediate, trapped, national).

% This body adjudicates disputes related to TRIPS, including the application of public health flexibilities. Its interpretations can significantly impact the scope and enforceability of these provisions, acting as a key arbiter.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, analytical, global).

% These governments often balance domestic pharmaceutical industry interests with global public health concerns. While they may advocate for strong IP protection, they also participate in the WTO framework that includes these flexibilities, sometimes facing pressure to support public health initiatives.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, developed_country_governments, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, developed_country_governments, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries_developing_countries).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__public_health_flexibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global intellectual property regime with the imperative of public health access, allowing WTO members to protect public health and promote access to medicines while adhering to TRIPS obligations.
% TRANSFER_FUNCTION: Facilitates the transfer of essential medicines and medical technologies from patent-protected markets to developing countries by enabling generic production and parallel imports, effectively transferring pricing power and market share from patent holders to public health systems and generic manufacturers.
% ABSENT_VOICES: The voices of marginalized communities and those without access to essential medicines would be amplified, demanding even broader and more automatic application of flexibilities, but they are often excluded from high-level trade negotiations.
% DISAPPEARANCE_RATIONALE: If the public health flexibilities interpretation vanished, developing countries would face insurmountable barriers to accessing affordable medicines, leading to widespread public health crises and a complete reorganization of global pharmaceutical supply chains and pricing structures.
% FOUNDING_PROBLEM: The original TRIPS Agreement, by mandating strong IP protection, created a conflict with the public health needs of developing countries, particularly regarding access to essential medicines for diseases like HIV/AIDS.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations (WHO, MSF), developing country governments, and independent academic analyses corroborate that the founding problem of access to medicines remains live, especially during global health crises. Pharmaceutical patent holders and some developed country governments argue the problem is largely addressed by voluntary licensing and aid programs.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).
:- end_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting that while public health access is prioritized, IP holders still retain significant rights and market power. Suppression (0.60) is substantial because the application of flexibilities often requires active legal and political defense against challenges from patent holders and their supporting governments. Theater ratio (0.15) is low, as the flexibilities are actively utilized and contested, indicating a functional rather than performative aspect. Accessibility collapse (0.40) is moderate, as flexibilities open alternatives but do not eliminate all barriers to access. Resistance (0.50) is also moderate, reflecting ongoing pushback from pharmaceutical companies against the broad application of these flexibilities. The dip in extractiveness and suppression around 2001 and 2020 reflects the impact of the Doha Declaration and the COVID-19 pandemic, respectively, which strengthened the public health interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health advocates and developing countries, this interpretation is a vital coordination mechanism to save lives. From the perspective of pharmaceutical patent holders, it represents an erosion of their intellectual property rights and a disincentive for innovation. The engine's classification will highlight this divergence, showing a beneficial outcome for public health seats and an extractive one for IP holders.
 *
 * DIRECTIONALITY LOGIC:
 *   Health ministries, generic manufacturers, and patients in developing countries are beneficiaries, gaining access to affordable medicines. Pharmaceutical patent holders are payers, experiencing reduced market exclusivity and pricing power. The WTO Dispute Settlement Body and developed country governments act as agenda-setters, shaping the interpretation and enforcement of these flexibilities, often balancing competing interests.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (ignoring the extraction from IP holders) or a pure Snare (ignoring the genuine public health coordination). The ongoing contestation and active enforcement demonstrate that its mandate is live, though its application is constantly negotiated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trips_kernel_reading_identity,
    'Is this constraint a genuine ''public_health_flexibility_reading'' of the TRIPS Agreement, or is it merely a temporary political concession within a fundamentally ''strong_exclusivity'' framework?',
    'Analysis of long-term trends in WTO dispute panel rulings, national legislative actions, and the frequency/scope of compulsory licensing and parallel imports over several decades, independent of specific health crises.',
    'If it''s a genuine reading, the current classification holds. If it''s a temporary concession, its underlying structure might be closer to a Snare or a more extractive Tangled Rope, with public health benefits being transient and easily suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trips_kernel_reading_identity, conceptual, 'Ambiguity regarding the fundamental nature and permanence of public health flexibilities within TRIPS.').

omega_variable(
    strong_exclusivity_reading_delta,
    'How would the structural properties of this constraint change if the ''strong_exclusivity_reading'' were to become dominant?',
    'Hypothetical re-evaluation of extractiveness, suppression, and beneficiary/victim sets under a scenario where compulsory licensing and parallel import flexibilities are narrowly interpreted or effectively nullified.',
    'Under a dominant ''strong_exclusivity_reading'', extractiveness from public health actors would significantly increase, suppression of generic competition would rise, and pharmaceutical patent holders would shift from ''payer'' to ''beneficiary'' with higher directionality, likely reclassifying the constraint towards a Snare or a more extractive Tangled Rope for public health seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strong_exclusivity_reading_delta, conceptual, 'Impact of an alternative TRIPS interpretation on constraint structure.').

omega_variable(
    disagreement_location_flexibility_scope,
    'Is the core disagreement over TRIPS flexibilities located in the interpretation of the text''s explicit provisions, or in the underlying normative principles (e.g., public health vs. property rights)?',
    'Detailed legal and philosophical analysis of WTO panel reports, national court decisions, and academic commentary, identifying whether arguments primarily hinge on textual ambiguity or on competing ethical/economic frameworks.',
    'If textual, resolution might come from clearer legal drafting or authoritative interpretation. If normative, the contest is deeper, making the constraint inherently more ''contested'' and resistant to purely legal resolution, potentially leading to persistent ''tangled_rope'' dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_flexibility_scope, conceptual, 'Location of the interpretive disagreement regarding TRIPS flexibilities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(trip_tr_t2008, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2008, 0.12).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2020, 0.08).
narrative_ontology:measurement(trip_tr_t2024, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2001, 0.4).
narrative_ontology:measurement(trip_be_t2008, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2008, 0.42).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2020, 0.38).
narrative_ontology:measurement(trip_be_t2024, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement(trip_su_t2008, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(trip_su_t2024, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the TRIPS Agreement interpretive kernel, focusing on public health flexibilities. It is structurally distinct from the 'strong_exclusivity_reading' and the 'dispute_settlement_interpretive_authority' reading, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
