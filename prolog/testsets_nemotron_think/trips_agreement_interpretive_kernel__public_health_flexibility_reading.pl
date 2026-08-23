% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: trips_agreement_interpretive_kernel__public_health_flexibility_reading
 *   human_readable: TRIPS Public Health Flexibilities Reading (Broad Compulsory Licensing & Parallel Imports)
 *   domain: international_trade_law/public_health_policy/intellectual_property
 *
 * SUMMARY:
 *   This constraint story captures the public health flexibility reading of
 *   the TRIPS Agreement — the interpretation that Articles 31, 31bis, and the
 *   Doha Declaration embed broad compulsory licensing and parallel import
 *   flexibilities to protect public health access. The reading operates as a
 *   legal permission structure: it authorizes governments to override patent
 *   monopolies for public health, creating a coordination mechanism for
 *   affordable medicine supply. The claimed type is tangled_rope because the
 *   flexibility regime performs genuine coordination (solving the market
 *   failure where monopoly pricing excludes poor populations) while
 *   simultaneously extracting value from patent holders (eroding their
 *   exclusive returns in affected markets). The extraction is not the purpose
 *   but a structural consequence of the coordination function. Requires
 *   active enforcement through national implementation legislation, WTO
 *   notification procedures, and the paragraph 6 export system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.45).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.15).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Public Health Flexibilities Reading (Broad Compulsory Licensing & Parallel Imports)").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade_law/public_health_policy/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '3b19f706-189b-403e-804c-b290506f129f').
narrative_ontology:cs_kernel_codification('3b19f706-189b-403e-804c-b290506f129f', formalized).
narrative_ontology:cs_authority_grounding('3b19f706-189b-403e-804c-b290506f129f', lineage).
narrative_ontology:cs_interpretation_layer_present('3b19f706-189b-403e-804c-b290506f129f').
narrative_ontology:cs_reading_relation('3b19f706-189b-403e-804c-b290506f129f', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b19f706-189b-403e-804c-b290506f129f', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, coexists_with).
narrative_ontology:cs_axiom('3b19f706-189b-403e-804c-b290506f129f', foundational, compulsory_licensing_broadly_construed).
narrative_ontology:cs_axiom_status(compulsory_licensing_broadly_construed, holdable).
narrative_ontology:cs_axiom_grounding('3b19f706-189b-403e-804c-b290506f129f', compulsory_licensing_broadly_construed, conventional).
narrative_ontology:cs_axiom('3b19f706-189b-403e-804c-b290506f129f', foundational, parallel_imports_permitted_under_exhaustion).
narrative_ontology:cs_axiom_status(parallel_imports_permitted_under_exhaustion, holdable).
narrative_ontology:cs_axiom_grounding('3b19f706-189b-403e-804c-b290506f129f', parallel_imports_permitted_under_exhaustion, conventional).
narrative_ontology:cs_reference_frame('3b19f706-189b-403e-804c-b290506f129f', doha_declaration_2001_flexibility_affirmation).
narrative_ontology:cs_drift_state('3b19f706-189b-403e-804c-b290506f129f', post_covid19_trips_waiver_debate, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3b19f706-189b-403e-804c-b290506f129f', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries_developing_countries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_developing_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, doha_declaration_public_health).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_article_31_flexibilities).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, parallel_imports_exhaustion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce affordable generic versions of patented medicines using compulsory licenses; gain market access in developing countries and export markets under paragraph 6 system; their business model depends on the broad reading of TRIPS flexibilities being legally secure.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers, beneficiary,
    powerful, biographical, mobile, global).

% Issue compulsory licenses for essential medicines, negotiate prices with leverage of TRIPS flexibilities, import generics through parallel import channels; constrained by diplomatic pressure, trade retaliation threats, and limited domestic manufacturing capacity.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries_developing_countries, beneficiary,
    institutional, generational, constrained, national).

% Gain access to life-saving medicines at affordable prices when flexibilities are used; trapped by geography, poverty, and health system capacity — cannot exit to alternative health systems or pay monopoly prices.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_developing_countries, beneficiary,
    powerless, biographical, trapped, national).

% Face compulsory licensing eroding monopoly pricing in developing markets, parallel imports undercutting tiered pricing strategies, and precedent risk for broader flexibility use; constrained by reputational risk, shareholder pressure, and WTO dispute exposure if they challenge too aggressively.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders, payer,
    powerful, biographical, constrained, global).

% Adjudicate disputes over TRIPS interpretation; their rulings on Canada–Pharmaceutical Patents, EC–Tariff Preferences, and Australia–Plain Packaging shape the operational boundary of flexibilities; they set the agenda for what constitutes compliant implementation.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_panels, agenda_setter,
    institutional, generational, analytical, global).

% Monitor flexibility use, submit amicus briefs, campaign for access, document implementation gaps; mobile across jurisdictions and forums but excluded from formal WTO decision-making.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, civil_society_ngos, observer,
    organized, biographical, mobile, global).

% Lack domestic manufacturing capacity to use compulsory licensing; paragraph 6 export system is cumbersome and rarely used; would object to procedural barriers but have no voice in WTO councils or industry negotiations.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, least_developed_country_patients, excluded,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables affordable medicine access for public health crises by legally authorizing compulsory licensing and parallel imports, solving the coordination failure where patent monopolies block supply in low-income markets.
% TRANSFER_FUNCTION: Transfers monopoly rent value from patent-holding pharmaceutical companies to generic manufacturers and public health systems, lowering medicine prices for patients in developing countries.
% ABSENT_VOICES: Patients in least developed countries with no manufacturing capacity who cannot use compulsory licensing effectively; future patients who need new drugs whose R&D might be underfunded if flexibilities erode innovation incentives — both are structurally excluded from the WTO interpretive community.
% DISAPPEARANCE_RATIONALE: If the broad flexibility reading vanished overnight, compulsory licensing would revert to narrow domestic-emergency-only use, paragraph 6 export system would collapse, parallel imports would be blocked by national exhaustion regimes, and medicine prices in developing countries would rise to monopoly levels — millions would lose access.
% FOUNDING_PROBLEM: The HIV/AIDS crisis in sub-Saharan Africa (late 1990s–early 2000s) revealed that TRIPS-mandated patent protections blocked access to antiretrovirals priced at $10,000–$15,000 per patient-year while generic versions cost $300–$500.
% FOUNDING_PROBLEM_CORROBORATION: WHO, Médecins Sans Frontières, UNAIDS, and developing country governments (Brazil, India, South Africa, Thailand) attest the access crisis persists for cancer drugs, hepatitis C treatments, and COVID-19 tools; pharmaceutical industry associations (IFPMA, PhRMA) and some developed country governments contest that the founding problem is solved by tiered pricing and voluntary licenses.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.45) reflects the measurable transfer of monopoly rent from patent holders to generic producers and health systems — not maximal because flexibilities are used selectively, not universally. Suppression (0.15) is low because the constraint enables rather than coerces; the suppressive force runs opposite (strong_exclusivity_reading suppresses flexibilities). Theater ratio (0.1) is low — the legal mechanisms are functional, used in practice (Brazil HIV/AIDS, Thailand cancer drugs, Canada–Rwanda export). Accessibility collapse (0.4) is moderate: the reading makes full patent enforcement less available but doesn't eliminate it — voluntary licenses, tiered pricing, and donations remain alternatives. Resistance (0.7) is high from patent holders who litigate, lobby, and use diplomatic pressure to narrow flexibility use.
 *
 * PERSPECTIVAL GAP:
 *   From the patent holder seat, this constraint is a snare — coerced transfer of property value under cover of public health. From the health ministry seat, it is a rope — genuine coordination enabling life-saving access. From the WTO panel seat, it is a legal text to be interpreted. The engine computes this divergence from the structural power/exit asymmetries authored here; the claimed type (tangled_rope) reflects the authoring seat's judgment that both coordination and extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Generic manufacturers and health ministries are beneficiaries (d ~0.2–0.3) — they gain legal certainty and negotiating leverage. Patients in developing countries are beneficiaries with trapped exit (d ~0.15) — they gain access but cannot exit the constraint's boundaries. Patent holders are payers with constrained exit (d ~0.75) — they bear the value transfer but cannot exit the global patent system. WTO panels are agenda_setters with analytical exit (d ~0.5) — they interpret but don't collect rents. The derivation chain places patent holders near the target end because their property right is the object of the flexibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (HIV/AIDS access crisis) remains live — new diseases, new drugs, new access gaps. The flexibility regime has not atrophied; its use has expanded (cancer, hepatitis C, COVID-19). Mandatrophy is not resolved because the coordination problem persists and the extraction asymmetry is the mechanism of coordination, not a separable rent layer. If the founding problem were solved (universal affordable access), the regime would become a piton — but that horizon is not visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trips_text_genuine_flexibility_scope,
    'Does the TRIPS text genuinely embed broad compulsory licensing and parallel import flexibilities as a matter of treaty interpretation, or does the public health reading require creative construction beyond the textual baseline?',
    'Comparative analysis of Vienna Convention interpretation (Art. 31–33) applied to TRIPS Articles 6, 31, 31bis across WTO panel reports, the Doha Declaration, and subsequent practice; systematic coding of textual indicators for breadth vs. narrowness.',
    'If the text genuinely embeds broad flexibilities, the constraint is a Mountain (natural law of treaty text); if it requires creative construction, it is a Tangled Rope or Scaffold whose legitimacy depends on political consensus that could erode.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trips_text_genuine_flexibility_scope, conceptual, 'Whether the flexibility scope is textually intrinsic or politically constructed.').

omega_variable(
    flexibility_innovation_tradeoff_separability,
    'Are the public health coordination function and the innovation incentive extraction structurally separable, or does broad flexibility use necessarily degrade the R&D incentive structure that produces new medicines?',
    'Empirical analysis of R&D investment trends, pipeline data, and voluntary licensing patterns in therapeutic areas with heavy compulsory licensing use vs. those without; counterfactual modeling of innovation under alternative IP regimes.',
    'If separable, the tangled_rope classification holds — coordination without fatal extraction. If inseparable, the constraint may be a Scaffold (transitional) or the extraction may be existentially threatening to the innovation coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(flexibility_innovation_tradeoff_separability, empirical, 'Whether coordination and extraction are separable functions or a zero-sum tradeoff.').

omega_variable(
    suppression_mechanism_patent_holders,
    'Is the constraint on patent holders structural (legal loss of exclusivity) or internalized (industry self-censorship, voluntary licensing to avoid compulsory licenses, reputational internalization)?',
    'Track voluntary licensing rates, tiered pricing adoption, and patent non-enforcement pledges in jurisdictions with active flexibility use vs. those without; survey industry decision-makers on behavioral drivers.',
    'If internalized suppression dominates, the effective extraction on patent holders is higher than the legal measure suggests — the constraint operates psychologically beyond its formal scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_patent_holders, empirical, 'Structural vs. internalized suppression on patent holder behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(trip_tr_t5, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 5, 0.08).
narrative_ontology:measurement(trip_tr_t10, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(trip_tr_t15, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(trip_tr_t20, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(trip_tr_t25, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(trip_tr_t30, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(trip_be_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(trip_be_t5, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(trip_be_t10, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(trip_be_t15, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(trip_be_t20, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(trip_be_t25, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 25, 0.44).
narrative_ontology:measurement(trip_be_t30, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(trip_su_t5, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 5, 0.1).
narrative_ontology:measurement(trip_su_t10, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(trip_su_t15, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 15, 0.13).
narrative_ontology:measurement(trip_su_t20, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement(trip_su_t25, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement(trip_su_t30, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.15).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% Part of the TRIPS interpretive kernel family. This reading (public_health_flexibility) and strong_exclusivity_reading offer contradictory substantive interpretations of the same treaty text. The dispute_settlement_interpretive_authority reading governs which interpretation prevails in binding disputes. All three share the TRIPS text as kernel but differ on authority_grounding and drift_state.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__public_health_flexibility_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
