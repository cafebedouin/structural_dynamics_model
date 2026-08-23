% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__embedded_liberalism_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: NAFTA Jurisdictional Boundary - Embedded Liberalism Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   The embedded liberalism reading of NAFTA's jurisdictional boundary treats
 *   the treaty as a framework that balances market access commitments with
 *   legitimate domestic policy space for environmental, labor, and health
 *   regulation. The 'legitimate objectives' exception (Article 1105, 1106,
 *   and side agreements) is read as preserving regulatory autonomy when
 *   measures are non-discriminatory and not disguised trade barriers. This
 *   reading was dominant in the 1990s but has eroded as Chapter 11
 *   jurisprudence expanded investor protections — the Methanex, Metalclad,
 *   and Ethyl cases narrowed the regulatory space. The constraint is a
 *   tangled rope: genuine coordination (trade liberalization with regulatory
 *   carve-outs) coexists with asymmetric extraction (investors gain
 *   enforceable rights; regulators bear litigation costs and chill).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.45).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.4).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA Jurisdictional Boundary - Embedded Liberalism Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, '426aef20-96fa-4318-b33e-133af07b1c7f').
narrative_ontology:cs_kernel_codification('426aef20-96fa-4318-b33e-133af07b1c7f', formalized).
narrative_ontology:cs_authority_grounding('426aef20-96fa-4318-b33e-133af07b1c7f', lineage).
narrative_ontology:cs_interpretation_layer_present('426aef20-96fa-4318-b33e-133af07b1c7f').
narrative_ontology:cs_reading_relation('426aef20-96fa-4318-b33e-133af07b1c7f', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('426aef20-96fa-4318-b33e-133af07b1c7f', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('426aef20-96fa-4318-b33e-133af07b1c7f', foundational, legitimate_objectives_exception_preserves_regulatory_space).
narrative_ontology:cs_axiom_status(legitimate_objectives_exception_preserves_regulatory_space, holdable).
narrative_ontology:cs_axiom_grounding('426aef20-96fa-4318-b33e-133af07b1c7f', legitimate_objectives_exception_preserves_regulatory_space, conventional).
narrative_ontology:cs_axiom('426aef20-96fa-4318-b33e-133af07b1c7f', foundational, non_discriminatory_standard_as_boundary_condition).
narrative_ontology:cs_axiom_status(non_discriminatory_standard_as_boundary_condition, holdable).
narrative_ontology:cs_axiom_grounding('426aef20-96fa-4318-b33e-133af07b1c7f', non_discriminatory_standard_as_boundary_condition, conventional).
narrative_ontology:cs_reference_frame('426aef20-96fa-4318-b33e-133af07b1c7f', embedded_liberalism_compromise).
narrative_ontology:cs_drift_state('426aef20-96fa-4318-b33e-133af07b1c7f', post_nafta_chapter11_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('426aef20-96fa-4318-b33e-133af07b1c7f', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, trading_partners).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_sectors).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, labor_environmental_standards_bodies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, subnational_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_corporations).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, embedded_liberalism_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, non_discriminatory_standard).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, legitimate_objectives_exception).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain secure market access across NAFTA parties through binding commitments. They benefit from predictable rules and dispute settlement that protects their exporters' access. Can shift trade flows to other partners if disputes arise, giving them arbitrage-grade exit from any single dispute.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trading_partners, beneficiary,
    institutional, generational, arbitrage, continental).

% Use Chapter 11 investor-state dispute settlement to challenge domestic regulations that affect expected profits. Gain regulatory stability and compensation for regulatory change. Also bear compliance costs and reputational risk from high-profile disputes. Can relocate investment, giving mobile exit.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_corporations, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_corporations, payer).

% Face litigation costs, regulatory chill, and loss of policy autonomy when regulations are challenged as trade barriers. Must defend regulations in dispute settlement using public funds. Cannot exit the constraint — they are the domestic implementers of both trade obligations and domestic law. Exit is constrained by constitutional mandate.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies, payer,
    organized, biographical, constrained, national).

% See their standards challenged as disguised trade barriers under the 'legitimate objectives' test. Bear the burden of proving non-discrimination and necessity. Their exit is constrained — they exist to set standards, not to avoid trade law.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, labor_environmental_standards_bodies, payer,
    moderate, biographical, constrained, national).

% State/provincial/local regulations (environmental, labor, procurement) are subject to challenge but subnational governments have no standing in dispute settlement. They are trapped — bound by federal treaty commitments they did not negotiate and cannot exit.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, subnational_governments, payer,
    moderate, biographical, trapped, regional).

% Adjudicate the boundary between legitimate regulation and trade barrier. Their interpretations define the 'legitimate objectives' exception in practice. They are the active enforcement mechanism — without them the constraint has no bite. Analytical exit: they interpret, they do not bear the policy consequences.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, dispute_settlement_panels, agenda_setter,
    institutional, generational, analytical, continental).

% Labor unions, environmental groups, consumer advocates, indigenous communities — would object to regulatory chill and democratic deficit but have no formal standing in Chapter 11 or state-state dispute settlement. Amicus briefs are permitted but not guaranteed. Their exclusion is structural: the treaty was negotiated without them.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, civil_society_ngos, excluded,
    organized, biographical, constrained, continental).

% Produce the interpretive literature that frames the boundary debate. Track jurisprudence, develop doctrinal tests, critique outcomes. Pure analytical seat — they neither collect nor pay, but their work shapes how the constraint is understood and contested.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, legal_scholars_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles trade liberalization commitments with domestic regulatory autonomy by establishing a 'legitimate objectives' exception: environmental, labor, and health standards are compatible with trade obligations when applied non-discriminatorily and not as disguised restrictions.
% TRANSFER_FUNCTION: Moves regulatory autonomy and litigation risk from domestic regulators to trading partners and investors. When a regulation is challenged, the defending state bears legal costs and faces potential compensation awards; the challenging investor gains a damages remedy. The transfer runs from public regulatory capacity to private investor expectation.
% ABSENT_VOICES: Affected communities (fenceline communities bearing pollution, workers facing labor standard erosion), subnational governments (whose regulations are challenged without their consent), future generations (locked into regulatory chill). They are absent from treaty negotiation, dispute settlement, and the 'legitimate objectives' balancing test — which is framed as state-investor, not state-community.
% DISAPPEARANCE_RATIONALE: If the embedded liberalism balance vanished, two rearrangements compete: (1) capital supremacy reading would expand — investor protections swallow regulatory space, race-to-bottom dynamics accelerate; (2) sovereignty primacy reading would fragment trade — states withdraw or ignore rulings, trade agreements become aspirational. The balance itself is what holds the arrangement together.
% FOUNDING_PROBLEM: Post-WWII need to reconcile open trade with domestic policy space for full employment, social welfare, and democratic legitimacy — the 'embedded liberalism compromise' (Ruggie 1982). NAFTA extended this to North America with investor-state arbitration as the enforcement innovation.
% FOUNDING_PROBLEM_CORROBORATION: Ruggie's embedded liberalism scholarship (independent IR theory); ILO supervisory bodies and UN human rights treaty bodies (outside beneficiary set) document regulatory chill; corporate globalization advocates (US Chamber, BusinessEurope) attest the problem is solved and the arrangement is rent-seeking by states — no consensus across seats.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).
:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects moderate but real transfer: investors gain damages for regulatory change; states lose policy space and pay legal costs. Suppression (0.40) captures regulatory chill — states avoid or weaken regulations to avoid disputes — but alternatives persist (states still regulate, just more cautiously). Theater ratio (0.25) is low-moderate: the coordination function is real (trade flows increased, disputes are real adjudication), but a growing share of activity is performative compliance (regulatory impact assessments designed to survive challenge rather than optimize policy). Accessibility collapse (0.50) and resistance (0.45) are moderate: states and NGOs resist, alternative frameworks exist (WTO TBT/SPS, USMCA replacement), but the constraint's legal force is binding.
 *
 * PERSPECTIVAL GAP:
 *   From the dispute settlement panel seat, the constraint is genuine coordination: they apply the treaty text, balance the tests, produce reasoned awards. From the domestic regulator seat, the same structure operates as extraction: every new regulation triggers a litigation risk assessment, the 'legitimate objectives' test is applied narrowly, and the cost of defending is borne by the public purse. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) acknowledges both seats are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Trading partners and multinational corporations are structural beneficiaries (d near 0.15-0.25) — they collect market access and investment protections with arbitrage/mobile exit. Domestic regulatory agencies, standards bodies, and subnational governments are structural targets (d near 0.75-0.85) — they bear costs with constrained/trapped exit. Dispute settlement panels are agenda-setters (d ~ 0.5, analytical exit). Civil society is excluded (no standing). The directionality derivation from beneficiary/victim + exit captures this asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling trade and domestic policy space) is contested — some argue it's solved (trade is open, states still regulate), others that it's been captured (investor rights have expanded beyond the original compromise). The constraint persists not because the founding problem is live, but because the institutional machinery (Chapter 11, state-state dispute settlement) generates its own constituency: arbitrators, trade lawyers, investors who value the protections. This is classic mandatrophy — the mandate (balance) has atrophied, but the constraint persists through institutional inertia and beneficiary capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the embedded liberalism reading a genuine structural balance, or a transitional cover for the capital supremacy reading that emerges through jurisprudence?',
    'Longitudinal analysis of Chapter 11 awards: if regulatory defenses succeed at a stable rate, the balance is real; if success rate declines toward zero as jurisprudence accumulates, the reading was a cover.',
    'If cover, the constraint''s true ε is higher (capital supremacy); if genuine balance, ε remains moderate and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, empirical, 'Whether the reading''s structural delta (partial overlap, defensive authority) is stable or erodes toward capital supremacy.').

omega_variable(
    legitimate_objectives_boundary,
    'Where exactly does the ''legitimate objectives'' exception end and ''disguised restriction'' begin? Is the boundary determinate or inherently contestable?',
    'Doctrinal analysis of panel reports: if a consistent test emerges (necessity, proportionality, least-trade-restrictive), boundary is determinate; if panels split or shift tests case-by-case, boundary is contestable.',
    'A determinate boundary lowers extraction (predictability); a contestable boundary raises extraction (regulatory risk premium, chill).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimate_objectives_boundary, conceptual, 'The structural location of the regulatory autonomy boundary in this reading.').

omega_variable(
    litigation_cost_distribution,
    'Who actually bears the litigation costs of Chapter 11 disputes — the federal treasury, subnational governments, or regulatory agencies? And do costs fall disproportionately on poorer jurisdictions?',
    'Fiscal tracking of legal expenditures across NAFTA parties, disaggregated by government level and policy area. Compare per-capita cost burden.',
    'If costs concentrate on subnational/poorer jurisdictions, extraction is regressive — strengthens snare/tangled_rope asymmetry. If federal treasuries absorb costs, extraction is less asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(litigation_cost_distribution, empirical, 'Distributional incidence of the constraint''s enforcement costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 1994, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 1994, 0.1).
narrative_ontology:measurement(naft_tr_t1998, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 1998, 0.15).
narrative_ontology:measurement(naft_tr_t2002, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2002, 0.2).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2006, 0.22).
narrative_ontology:measurement(naft_tr_t2010, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2010, 0.23).
narrative_ontology:measurement(naft_tr_t2014, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2014, 0.24).
narrative_ontology:measurement(naft_tr_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(naft_tr_t2020, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2020, 0.25).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 1994, 0.25).
narrative_ontology:measurement(naft_be_t1998, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 1998, 0.3).
narrative_ontology:measurement(naft_be_t2002, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2002, 0.38).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2006, 0.42).
narrative_ontology:measurement(naft_be_t2010, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(naft_be_t2014, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2014, 0.45).
narrative_ontology:measurement(naft_be_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2018, 0.45).
narrative_ontology:measurement(naft_be_t2020, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 1994, 0.25).
narrative_ontology:measurement(naft_su_t1998, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 1998, 0.3).
narrative_ontology:measurement(naft_su_t2002, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2002, 0.35).
narrative_ontology:measurement(naft_su_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2006, 0.38).
narrative_ontology:measurement(naft_su_t2010, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(naft_su_t2014, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2014, 0.4).
narrative_ontology:measurement(naft_su_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2018, 0.4).
narrative_ontology:measurement(naft_su_t2020, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2020, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.1).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, usmca_jurisdictional_boundary).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, wto_tbt_agreement).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, wto_sps_agreement).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, ceta_investment_court_system).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the nafta_jurisdictional_boundary kernel. The capital_supremacy_reading and sovereignty_primacy_reading are sibling constraints with different ε values and beneficiary/victim structures. All three share the same treaty text as kernel but instantiate different constraints. The embedded_liberalism_reading is the middle position: partial overlap, moderate extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, institutional, 0.15).
constraint_indexing:directionality_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, powerful, 0.2).
constraint_indexing:directionality_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, organized, 0.75).
constraint_indexing:directionality_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
