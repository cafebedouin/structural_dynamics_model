% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__competitive_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__competitive_moat_reading, []).

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
 *   constraint_id: article17_erasure_right__competitive_moat_reading
 *   human_readable: GDPR Article 17 Erasure Right as Competitive Moat
 *   domain: technology_governance/data_protection_law/competition_policy
 *
 * SUMMARY:
 *   GDPR Article 17 (Right to Erasure) is widely framed as a fundamental
 *   privacy right. This reading — the competitive_moat_reading — treats the
 *   same legal text as a structural barrier that advantages incumbents. The
 *   constraint is not the right itself but the compliance infrastructure
 *   mandate it implies: 'reasonable steps' to erase data from all systems,
 *   including backups, derived data, and third-party shares. Incumbents
 *   (Google, Meta, Microsoft, enterprise SaaS) built this infrastructure
 *   early, shaped the guidance, and now enjoy near-zero marginal cost.
 *   Challengers face a fixed cost that can exceed seed funding. The
 *   coordination function (individual data control) is real; the extraction
 *   function (barrier to entry) is asymmetric. The engine will compute
 *   per-seat types from the structural data; this story authors the claim
 *   (tangled_rope) and metrics independently.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, 0.72).
domain_priors:suppression_score(article17_erasure_right__competitive_moat_reading, 0.65).
domain_priors:theater_ratio(article17_erasure_right__competitive_moat_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(article17_erasure_right__competitive_moat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__competitive_moat_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__competitive_moat_reading, "GDPR Article 17 Erasure Right as Competitive Moat").
narrative_ontology:topic_domain(article17_erasure_right__competitive_moat_reading, "technology_governance/data_protection_law/competition_policy").

domain_priors:requires_active_enforcement(article17_erasure_right__competitive_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__competitive_moat_reading, '3c317645-2b36-4601-a87b-cf35e7c1b4e1').
narrative_ontology:cs_kernel_codification('3c317645-2b36-4601-a87b-cf35e7c1b4e1', formalized).
narrative_ontology:cs_authority_grounding('3c317645-2b36-4601-a87b-cf35e7c1b4e1', lineage).
narrative_ontology:cs_interpretation_layer_present('3c317645-2b36-4601-a87b-cf35e7c1b4e1').
narrative_ontology:cs_reading_relation('3c317645-2b36-4601-a87b-cf35e7c1b4e1', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c317645-2b36-4601-a87b-cf35e7c1b4e1', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('3c317645-2b36-4601-a87b-cf35e7c1b4e1', foundational, compliance_cost_asymmetry_creates_moat).
narrative_ontology:cs_axiom_status(compliance_cost_asymmetry_creates_moat, holdable).
narrative_ontology:cs_axiom_grounding('3c317645-2b36-4601-a87b-cf35e7c1b4e1', compliance_cost_asymmetry_creates_moat, empirically_contingent).
narrative_ontology:cs_axiom('3c317645-2b36-4601-a87b-cf35e7c1b4e1', secondary, regulatory_floor_as_competitive_weapon).
narrative_ontology:cs_axiom_status(regulatory_floor_as_competitive_weapon, holdable).
narrative_ontology:cs_axiom_grounding('3c317645-2b36-4601-a87b-cf35e7c1b4e1', regulatory_floor_as_competitive_weapon, instrumental).
narrative_ontology:cs_reference_frame('3c317645-2b36-4601-a87b-cf35e7c1b4e1', individual_data_sovereignty_post_2018).
narrative_ontology:cs_drift_state('3c317645-2b36-4601-a87b-cf35e7c1b4e1', post_dma_eda_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3c317645-2b36-4601-a87b-cf35e7c1b4e1', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__competitive_moat_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, incumbent_tech_platforms).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, large_data_controllers).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, compliance_infrastructure_vendors).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, challenger_startups).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, small_data_controllers).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, new_market_entrants).
narrative_ontology:constraint_victim(article17_erasure_right__competitive_moat_reading, non_eu_competitors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, privacy_advocates).
narrative_ontology:constraint_beneficiary(article17_erasure_right__competitive_moat_reading, end_users).
narrative_ontology:constraint_vindicates(article17_erasure_right__competitive_moat_reading, data_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(article17_erasure_right__competitive_moat_reading, regulatory_compliance_as_quality_signal).
narrative_ontology:constraint_vindicates(article17_erasure_right__competitive_moat_reading, privacy_by_design_architecture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate mature erasure pipelines (automated lookup, cascade deletion, audit logging) built over a decade. They shaped the regulatory guidance through lobbying and participation in EDPB working groups. Their compliance cost per request is near-zero marginal; the fixed investment is sunk. They benefit when the bar stays high because it deters entrants who cannot amortize the build.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, incumbent_tech_platforms, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__competitive_moat_reading, incumbent_tech_platforms, beneficiary).

% Enterprise SaaS, ad-tech stacks, and data brokers with dedicated privacy engineering teams. They treat Article 17 compliance as a product feature and sales differentiator ('we are GDPR-ready'). Their scale makes the per-request cost trivial; they capture the trust premium without proportional expense.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, large_data_controllers, beneficiary,
    powerful, biographical, mobile, global).

% Vendors selling erasure-orchestration SaaS, consent-management platforms, and data-mapping tools. They profit from the mandate's complexity — the harder the compliance, the higher the license revenue. They lobby for granular guidance that expands the scope of 'reasonable steps.'
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, compliance_infrastructure_vendors, beneficiary,
    organized, biographical, mobile, global).

% Early-stage companies building data-intensive products (AI training, personalization, marketplace logs). They must implement erasure before product-market fit, diverting engineering weeks to plumbing that incumbents already have. Fundraising decks now include a 'GDPR compliance' line item; investors discount valuations for the capex. Exit means pivoting to non-personal-data models or accepting acquisition by an incumbent.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, challenger_startups, payer,
    moderate, immediate, constrained, global).

% Local e-commerce, community forums, niche apps, and research repositories. They lack in-house privacy engineers and cannot afford vendor contracts. Many rely on manual processes (spreadsheets, ticket queues) that scale poorly and expose them to fines. Some simply stop offering accounts to EU users — a de facto market exit.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, small_data_controllers, payer,
    powerless, immediate, trapped, regional).

% Founders evaluating whether to enter a data-dependent market. The Article 17 compliance floor appears in TAM models as a fixed cost that must be paid before revenue. Many choose not to enter; those who do often launch with geo-fencing that excludes the EEA, fragmenting the digital single market.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, new_market_entrants, excluded,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__competitive_moat_reading, new_market_entrants, payer).

% US, Asian, and LatAm platforms targeting EU users. They face the same build cost without the home-field advantage of early regulatory dialogue. Some adopt a 'Brussels effect' strategy — building to EU standard globally — which spreads the moat worldwide and reinforces incumbent advantage in their home markets too.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, non_eu_competitors, payer,
    moderate, biographical, constrained, global).

% National DPAs and the EDPB. They issue guidelines, enforce fines, and certify codes of conduct. Their institutional legitimacy depends on the regulation being effective; they resist narrowing the 'reasonable steps' test because that would look like deregulation. They are not captured but are structurally incentivized to maintain the compliance surface area.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__competitive_moat_reading, data_protection_authorities, observer).

% Civil-society groups (NOYB, EPIC, BEUC) that litigate erasure failures and push for stricter interpretation. They genuinely value the right and win cases that strengthen it. Their victories incidentally raise the compliance floor — a dynamic they acknowledge but treat as necessary for the right's credibility.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, privacy_advocates, beneficiary,
    organized, generational, arbitrage, global).

% Individuals exercising erasure requests. They gain real control over data remnants (old profiles, inference artifacts, shared logs). The right works for them — but they also lose access to services that withdraw from the EU or degrade free tiers to cover compliance costs. Their benefit is genuine but unevenly distributed.
narrative_ontology:constraint_stakeholder(article17_erasure_right__competitive_moat_reading, end_users, beneficiary,
    powerless, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides individuals a legally enforceable lever to delete personal data from corporate systems, limiting unbounded retention and enabling a baseline of data sovereignty across jurisdictions.
% TRANSFER_FUNCTION: Moves the cost of building and operating erasure infrastructure (data mapping, cascade deletion, verification, audit trails) from incumbent platforms — who amortize it over billions of users — to challengers and small controllers who must pay the full fixed cost before serving a single user.
% ABSENT_VOICES: Early-stage founders who never incorporate because the compliance floor exceeds their runway; academic researchers whose datasets become legally toxic; non-EU regulators who face pressure to mirror the standard without EU market access; users in developing markets who lose access to free services when providers geo-fence.
% DISAPPEARANCE_RATIONALE: If Article 17 vanished overnight, incumbent platforms would keep their erasure pipelines (they are now product features and trust signals), but challengers would launch without the fixed-cost barrier, vendors would pivot to voluntary privacy tooling, and the EEA market would see a wave of new entrants — the competitive landscape would reorganize within 12-18 months.
% FOUNDING_PROBLEM: Uncontrolled corporate data retention: companies kept personal data indefinitely by default, with no technical or legal mechanism for individuals to demand deletion, creating power asymmetry and privacy harm.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by pre-GDPR academic literature (Solove 2006, Hildebrandt 2008) and early DPAs' own reports. Competition authorities (UK CMA 2020, French Autorité 2021, EU Commission Digital Markets Act impact assessments) corroborate that the problem has been substantially addressed for incumbents but the arrangement now produces barrier effects they did not intend. Startup surveys (Atomico State of European Tech, Allied for Startups) attest the compliance cost asymmetry. No single seat speaks for all three.
narrative_ontology:disappearance_verdict(article17_erasure_right__competitive_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__competitive_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__competitive_moat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article17_erasure_right__competitive_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__competitive_moat_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__competitive_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article17_erasure_right__competitive_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article17_erasure_right__competitive_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the ratio of compliance cost to revenue for sub-scale entrants — often 15-30% of engineering budget in year one. Suppression (0.65) captures the technical and legal risk that deters market entry and forces geo-fencing. Theater (0.38) measures the growing share of compliance activity that is performative (vendor certifications, documentation for auditors) rather than functional erasure. Accessibility collapse (0.58) reflects that alternatives (not processing personal data, synthetic data, federated learning) exist but are technically immature for many use cases. Resistance (0.55) captures startup litigation, industry lobbying for carve-outs, and the emergence of 'privacy-light' product categories. All metrics measured on a shared annual grid 2018-2026.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent seat, Article 17 is a rope — a coordination mechanism they built and maintain. From the challenger seat, it is a snare — a barrier they cannot surmount without incumbent-scale capital. From the DPA seat, it is a scaffold — a transitional enforcement target that should normalize. From the privacy advocate seat, it is a mountain — a fundamental right. The engine computes these divergences; the authored claim (tangled_rope) reflects the structural hybridity: genuine coordination + asymmetric extraction + active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbents and large controllers are structural beneficiaries (d ≈ 0.1-0.2): they collect trust rents and competitive insulation. Compliance vendors are beneficiaries (d ≈ 0.15): they extract license revenue from the mandate. Challenger startups, small controllers, and new entrants are targets (d ≈ 0.8-0.95): they pay the full fixed cost with no amortization base. Non-EU competitors sit slightly lower (d ≈ 0.75) because some adopt the standard globally and capture scale benefits. DPAs are near-symmetric (d ≈ 0.5): they bear enforcement cost but gain institutional legitimacy. Privacy advocates are beneficiaries (d ≈ 0.2): they achieve policy goals. End users are near-symmetric (d ≈ 0.45): genuine right, diffuse indirect cost via service withdrawal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncontrolled retention) is substantially solved for incumbents — they have deletion pipelines. For challengers, the problem never existed (they had no legacy systems). The arrangement persists because the compliance floor has become a quality signal and moat. Mandatrophy is unresolved: the mandate's coordination function is live, but its extraction function has grown. The status 'contested' reflects this split.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading,
    'This constraint is one reading of the contested kernel ''article17_erasure_right''. How does the competitive_moat_reading''s ε differ from its siblings, and where is the structural disagreement located?',
    'Decompose the kernel into three constraint stories (this file, privacy_fundamental_reading, censorship_mechanism_reading). Compare their ε, beneficiary/victim sets, and claimed types. The disagreement is located in the compliance-infrastructure mandate: privacy_fundamental treats it as necessary cost of the right; censorship_mechanism treats it as a weapon; competitive_moat treats it as a barrier.',
    'If the three readings yield materially different ε and classifications, the kernel is structurally ambiguous and the label ''Article 17'' conflates distinct constraints. This validates the ε-invariance principle: one label, multiple constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'Commitment-structure framing: kernel_id=article17_erasure_right, reading_id=competitive_moat_reading, siblings=privacy_fundamental_reading+censorship_mechanism_reading').

omega_variable(
    compliance_cost_necessity,
    'Are the erasure infrastructure costs (data mapping, cascade deletion, third-party propagation) technically necessary for the right''s function, or are they inflated by regulatory guidance and vendor upselling?',
    'Technical audit of minimal viable erasure architecture vs. current regulatory expectations. Compare EDPB guidelines 05/2019 (Art. 17) with actual engineering implementations at different scales.',
    'If costs are technically necessary, the moat is an unavoidable consequence of the right (coordination cost). If inflated, the moat is partly constructed — a policy choice that could be narrowed without weakening the right.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_necessity, empirical, 'Whether the barrier height is inherent to the right or constructed by interpretation').

omega_variable(
    privacy_moat_separability,
    'Can the privacy benefit (individual control) be preserved while lowering the compliance floor for small controllers (e.g., tiered obligations, safe harbors, standardized APIs)?',
    'Policy experiments: UK''s ''data protection reform'' proposals, EU''s SME-friendly GDPR guidance, US state law tiered thresholds. Measure entry rates and erasure effectiveness in jurisdictions with tiered regimes.',
    'If separable, the moat is a policy failure, not a structural necessity. If inseparable, the trade-off is fundamental and the tangled_rope classification is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(privacy_moat_separability, conceptual, 'Whether the coordination and extraction components can be decoupled').

omega_variable(
    vendor_capture_feedback,
    'Do compliance-infrastructure vendors actively lobby for guidance that expands the scope of ''reasonable steps'' beyond what the right requires?',
    'Trace EDPB working-group participation, public consultations, and MEP lobbying records of major CMP and erasure-orchestration vendors. Correlate with guidance granularity increases.',
    'If vendors shape guidance to expand scope, the moat is actively maintained by a beneficiary coalition — strengthening the tangled_rope classification and identifying a capture vector.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vendor_capture_feedback, empirical, 'Whether beneficiaries actively maintain the extraction component').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__competitive_moat_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article17_moat_tr_t2018, article17_erasure_right__competitive_moat_reading, theater_ratio, 2018, 0.2).
narrative_ontology:measurement(article17_moat_tr_t2019, article17_erasure_right__competitive_moat_reading, theater_ratio, 2019, 0.24).
narrative_ontology:measurement(article17_moat_tr_t2020, article17_erasure_right__competitive_moat_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(article17_moat_tr_t2021, article17_erasure_right__competitive_moat_reading, theater_ratio, 2021, 0.31).
narrative_ontology:measurement(article17_moat_tr_t2022, article17_erasure_right__competitive_moat_reading, theater_ratio, 2022, 0.33).
narrative_ontology:measurement(article17_moat_tr_t2023, article17_erasure_right__competitive_moat_reading, theater_ratio, 2023, 0.35).
narrative_ontology:measurement(article17_moat_tr_t2024, article17_erasure_right__competitive_moat_reading, theater_ratio, 2024, 0.36).
narrative_ontology:measurement(article17_moat_tr_t2025, article17_erasure_right__competitive_moat_reading, theater_ratio, 2025, 0.37).
narrative_ontology:measurement(article17_moat_tr_t2026, article17_erasure_right__competitive_moat_reading, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(article17_moat_be_t2018, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2018, 0.45).
narrative_ontology:measurement(article17_moat_be_t2019, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2019, 0.52).
narrative_ontology:measurement(article17_moat_be_t2020, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(article17_moat_be_t2021, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2021, 0.63).
narrative_ontology:measurement(article17_moat_be_t2022, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2022, 0.67).
narrative_ontology:measurement(article17_moat_be_t2023, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2023, 0.7).
narrative_ontology:measurement(article17_moat_be_t2024, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2024, 0.71).
narrative_ontology:measurement(article17_moat_be_t2025, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2025, 0.715).
narrative_ontology:measurement(article17_moat_be_t2026, article17_erasure_right__competitive_moat_reading, base_extractiveness, 2026, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(article17_moat_su_t2018, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(article17_moat_su_t2019, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2019, 0.58).
narrative_ontology:measurement(article17_moat_su_t2020, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(article17_moat_su_t2021, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2021, 0.61).
narrative_ontology:measurement(article17_moat_su_t2022, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2022, 0.62).
narrative_ontology:measurement(article17_moat_su_t2023, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2023, 0.63).
narrative_ontology:measurement(article17_moat_su_t2024, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2024, 0.64).
narrative_ontology:measurement(article17_moat_su_t2025, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2025, 0.645).
narrative_ontology:measurement(article17_moat_su_t2026, article17_erasure_right__competitive_moat_reading, suppression_requirement, 2026, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__competitive_moat_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__competitive_moat_reading, 0.12).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, gdpr_article25_data_protection_by_design).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, eu_digital_markets_act_gatekeeper_obligations).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, uk_online_safety_act_age_verification).
narrative_ontology:affects_constraint(article17_erasure_right__competitive_moat_reading, california_ccpa_deletion_right).

% DUAL FORMULATION NOTE:
% Article 17 kernel decomposes into three constraint stories: competitive_moat_reading (this file, tangled_rope), privacy_fundamental_reading (rope/mountain), censorship_mechanism_reading (snare). All three share the legal text but differ in ε, beneficiary/victim structure, and enforcement dynamics. The competitive moat reading is downstream of the privacy fundamental reading (the right's legitimacy enables the mandate) and upstream of the censorship mechanism reading (the infrastructure built for erasure can be repurposed for takedown).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article17_erasure_right__competitive_moat_reading, institutional, 0.15).
constraint_indexing:directionality_override(article17_erasure_right__competitive_moat_reading, powerful, 0.2).
constraint_indexing:directionality_override(article17_erasure_right__competitive_moat_reading, moderate, 0.85).
constraint_indexing:directionality_override(article17_erasure_right__competitive_moat_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
