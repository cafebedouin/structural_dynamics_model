% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__expansive_secondary_objectives, []).

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
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Mandate Article 127 — Expansive Secondary Objectives Reading
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   The ECB's mandate under Article 127 TFEU establishes price stability as
 *   the primary objective but permits the ECB to 'support the general
 *   economic policies in the Union' including employment and growth 'without
 *   prejudice to the objective of price stability.' This expansive reading
 *   treats the 'without prejudice' clause as authorizing discretionary
 *   balancing — the ECB may operationally weight secondary objectives when
 *   its assessment indicates price stability is not threatened. This reading
 *   has expanded over time from a narrow crisis exception (post-2008,
 *   post-2012) to a standing feature of the strategic framework (2021
 *   strategy review). The constraint is structurally a tangled rope: it
 *   coordinates monetary-fiscal interaction in a currency union without
 *   fiscal union (genuine coordination function) while transferring resources
 *   from creditors to debtors through inflation tolerance (asymmetric
 *   extraction). Active enforcement is required — the orthodox reading is
 *   suppressed through institutional control of the mandate interpretation,
 *   appointment processes, and judicial deference.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.38).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.42).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.38).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Mandate Article 127 — Expansive Secondary Objectives Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, 'a27fb91a-ee70-4c3c-8c6f-219891a483c3').
narrative_ontology:cs_kernel_codification('a27fb91a-ee70-4c3c-8c6f-219891a483c3', fixed_text).
narrative_ontology:cs_authority_grounding('a27fb91a-ee70-4c3c-8c6f-219891a483c3', lineage).
narrative_ontology:cs_interpretation_layer_present('a27fb91a-ee70-4c3c-8c6f-219891a483c3').
narrative_ontology:cs_reading_relation('a27fb91a-ee70-4c3c-8c6f-219891a483c3', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('a27fb91a-ee70-4c3c-8c6f-219891a483c3', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('a27fb91a-ee70-4c3c-8c6f-219891a483c3', foundational, without_prejudice_authorizes_discretionary_balancing).
narrative_ontology:cs_axiom_status(without_prejudice_authorizes_discretionary_balancing, holdable).
narrative_ontology:cs_axiom_grounding('a27fb91a-ee70-4c3c-8c6f-219891a483c3', without_prejudice_authorizes_discretionary_balancing, conventional).
narrative_ontology:cs_axiom('a27fb91a-ee70-4c3c-8c6f-219891a483c3', foundational, employment_growth_are_operational_objectives_when_price_stability_not_threatened).
narrative_ontology:cs_axiom_status(employment_growth_are_operational_objectives_when_price_stability_not_threatened, holdable).
narrative_ontology:cs_axiom_grounding('a27fb91a-ee70-4c3c-8c6f-219891a483c3', employment_growth_are_operational_objectives_when_price_stability_not_threatened, instrumental).
narrative_ontology:cs_reference_frame('a27fb91a-ee70-4c3c-8c6f-219891a483c3', article_127_literal_text_as_constitutional_settlement).
narrative_ontology:cs_drift_state('a27fb91a-ee70-4c3c-8c6f-219891a483c3', post_2021_strategy_review, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a27fb91a-ee70-4c3c-8c6f-219891a483c3', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_workers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_debtors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, member_state_governments_seeking_fiscal_space).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council_members).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, creditor_nations_taxpayers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, inflation_averse_savers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, national_central_banks_constrained_by_mandate_interpretation).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, price_stability_purists_in_academy_and_policy).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, dual_mandate_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, employment_growth_as_operational_objective).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, without_prejudice_discretionary_balancing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets monetary policy for the eurozone. The expansive reading of Article 127(2) TFEU authorizes them to weigh employment and growth objectives when price stability is not threatened, using the 'without prejudice' clause as a discretionary balancing tool. They control the operational definition of 'price stability not threatened' and the weighting mechanism for secondary objectives.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, continental).

% Benefit from employment and growth considerations in monetary policy through lower unemployment, higher wage pressure, and stronger labor markets. Their voice is mediated through trade unions and political parties; they cannot directly exit the eurozone monetary framework but can pressure national governments.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_workers, beneficiary,
    organized, biographical, constrained, continental).

% Households, firms, and sovereigns with euro-denominated debt benefit from growth-friendly policy stances that ease debt service burdens. Their exit is constrained by eurozone membership and contract denomination; they benefit from the expansive reading's tolerance for above-target inflation during growth downturns.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_debtors, beneficiary,
    moderate, biographical, constrained, continental).

% Governments of member states (especially high-debt southern eurozone members) benefit from monetary policy that accommodates fiscal expansion and reduces sovereign borrowing costs. They appoint Governing Council members and influence the mandate interpretation through European Council positions, but cannot unilaterally exit the monetary union.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, member_state_governments_seeking_fiscal_space, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__expansive_secondary_objectives, member_state_governments_seeking_fiscal_space, agenda_setter).

% Taxpayers in creditor nations (Germany, Netherlands, Finland, etc.) bear the fiscal risk of monetary financing and potential inflation erosion of claims. They exercise influence through national governments and constitutional courts (e.g., German Bundesverfassungsgericht) but cannot exit the eurozone without massive economic disruption.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, creditor_nations_taxpayers, payer,
    institutional, generational, constrained, national).

% Households and institutions holding euro-denominated savings lose purchasing power when the expansive reading permits above-target inflation for employment/growth objectives. They have some exit via asset diversification (foreign currencies, real assets, inflation-linked bonds) but face transaction costs and regulatory constraints.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, inflation_averse_savers, payer,
    organized, biographical, mobile, continental).

% National central banks (Bundesbank, Banque de France, etc.) that institutionally identify with price stability orthodoxy are constrained by the Governing Council's expansive interpretation. Their staff and leadership have built professional identity around inflation targeting; dissent risks institutional marginalization. Exit would require national withdrawal from the Eurosystem.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, national_central_banks_constrained_by_mandate_interpretation, payer,
    institutional, generational, identity_locked, national).

% Economists, central bankers, and legal scholars who read Article 127 as requiring exclusive focus on price stability. They are excluded from operational decision-making but influence discourse through publications, testimony, and judicial amicus briefs. Their 'exit' is intellectual — maintaining the orthodox reading as a counter-narrative.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, price_stability_purists_in_academy_and_policy, excluded,
    moderate, civilizational, analytical, continental).

% Exercises democratic oversight through monetary dialogue hearings with the ECB President. Can request opinions, commission studies, and shape public narrative but has no binding authority over mandate interpretation. Observes the constraint's operation from a legitimacy-accountability perspective.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, european_parliament_economic_affairs_committee, observer,
    institutional, generational, analytical, continental).

% Ultimate arbiter of EU law including Treaty interpretation. Has upheld broad ECB discretion in cases like Gauweiler (OMT) and Weiss (PSPP) but has not definitively ruled on the expansive secondary objectives reading. Its future rulings could validate, constrain, or foreclose this reading.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, european_court_of_justice, observer,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__expansive_secondary_objectives, european_court_of_justice, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legitimate framework for the ECB to consider employment and growth impacts when setting policy, avoiding the paralysis of rigid single-target mandates during asymmetric shocks. Enables coordinated monetary-fiscal response within the eurozone's institutional architecture.
% TRANSFER_FUNCTION: Transfers real resources from creditors (savers, creditor-nation taxpayers) to debtors (borrowers, high-debt sovereigns, workers) via tolerance for above-target inflation and accommodative financial conditions. The transfer operates through the inflation tax and compressed real interest rates.
% ABSENT_VOICES: Small and medium enterprises in creditor nations that face higher input costs from accommodative policy; future generations who inherit the fiscal consequences of monetary financing; non-eurozone EU members affected by spillovers but without Governing Council representation.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished overnight and the ECB reverted to strict inflation targeting, sovereign borrowing spreads would widen immediately for high-debt members, fiscal consolidation pressure would intensify, unemployment would rise in periphery countries, and the political coalition sustaining the eurozone would fracture. The monetary-fiscal coordination architecture would collapse.
% FOUNDING_PROBLEM: The eurozone's asymmetric shock vulnerability: member states surrendered monetary sovereignty without a fiscal union, leaving them exposed to country-specific downturns with no national monetary policy response. The expansive reading was built to give the ECB operational flexibility to act as a de facto stabilizer for employment and growth when price stability is not immediately threatened.
% FOUNDING_PROBLEM_CORROBORATION: The ECB's own strategic reviews (2003, 2021) attest the problem remains live, citing persistent asymmetric shocks. The Five Presidents' Report (2015) and independent analyses by Bruegel, CEPR, and IMF staff corroborate the structural vulnerability but dispute whether the expansive reading is the appropriate solution versus fiscal union. The German Constitutional Court's PSPP ruling (2020) contested the reading's legitimacy from outside the beneficiary set.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).
:- end_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the real but bounded transfer from creditors to debtors via inflation tolerance — significant but constrained by the price stability anchor. Suppression (0.42) captures the institutional marginalization of orthodox voices within the Eurosystem and the difficulty of legal challenge (ECJ deference). Theater ratio (0.31) reflects that the price stability anchor is real and operationally binding (the ECB does target ~2% over the medium term) but a growing share of communication and analytical work serves to justify the expansive reading rather than pursue the primary objective. Accessibility collapse (0.48) is moderate: the orthodox reading remains a live alternative in courts, academia, and creditor nation politics. Resistance (0.54) is substantial: legal challenges, political opposition in creditor nations, and scholarly critique persist.
 *
 * PERSPECTIVAL GAP:
 *   From the Governing Council's seat, this is a rope — genuine coordination solving the eurozone's structural vulnerability. From creditor nation taxpayers' and orthodox NCBs' seats, it is a snare — extraction under a coordination cover story. From workers' and debtors' seats, it is a scaffold — transitional support that should become a fiscal union. The engine computes this divergence from the declared structural data; the claimed_type (tangled_rope) represents the author's structural judgment that both coordination and extraction are real and irreducible.
 *
 * DIRECTIONALITY LOGIC:
 *   The Governing Council (agenda_setter, institutional power, analytical exit) sits at the beneficiary end — it gains discretionary authority and institutional relevance. Workers and debtors (beneficiaries, organized/moderate power, constrained exit) receive transfers but cannot control the mechanism. Member state governments (dual beneficiary/agenda_setter, institutional power, constrained exit) both benefit and influence. Creditor nation taxpayers and inflation-averse savers (payers, institutional/organized power, constrained/mobile exit) bear the inflation tax. National central banks with orthodox identity (payers, institutional power, identity_locked exit) are structurally trapped — their professional identity fuses with price stability orthodoxy but they must implement expansive policy. Price stability purists (excluded, moderate power, analytical exit) are kept out of operational decisions but contest in discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (asymmetric shock vulnerability in a monetary union without fiscal union) remains live and contested. The arrangement has not atrophied — its coordination function is actively used (2020 pandemic response, 2022-23 inflation response with selective transmission protection). But the extraction dimension has grown (theater ratio rising from 0.12 to 0.31) as the 'without prejudice' clause has been stretched to justify persistent secondary objective weighting even when price stability is threatened. Mandatrophy is not resolved; the constraint is in active contested evolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    price_stability_threshold_ambiguity,
    'What operational threshold defines ''price stability not threatened'' — and who decides when it is met?',
    'ECB''s published reaction function analysis; ECJ ruling on a challenge to a specific policy decision justified under the expansive reading; academic consensus on the reaction function''s estimated parameters.',
    'If the threshold is effectively ''always met absent hyperinflation,'' the expansive reading becomes a snare (extraction with coordination cover). If the threshold is tight and objectively verifiable, the reading remains a genuine tangled rope. The threshold''s operationalization determines the extraction-coordination boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_stability_threshold_ambiguity, conceptual, 'Whether the ''without prejudice'' condition is a binding constraint or a nominal fig leaf.').

omega_variable(
    mandate_interpretation_as_institutional_power,
    'Does the expansive reading primarily serve the eurozone''s macroeconomic stability, or does it primarily serve the ECB''s institutional power and the Governing Council''s discretionary authority?',
    'Counterfactual analysis: would a fiscal union with a narrow mandate achieve better employment/growth outcomes with less extraction? Comparison of ECB forecasting errors under expansive vs. orthodox regimes.',
    'If institutional power is the primary driver, the constraint is a snare masquerading as a tangled rope. If macroeconomic outcomes are demonstrably superior, the coordination function is genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_interpretation_as_institutional_power, preference, 'The teleological ambiguity at the heart of mandate interpretation disputes.').

omega_variable(
    kernel_reading_structural_delta,
    'How does this reading''s beneficiary/victim structure structurally differ from the sibling readings?',
    'Comparative analysis of the three readings'' stakeholder mappings: orthodox reading has narrow beneficiaries (price stability purists, creditor nations) and no operational victims; climate_incorporation reading adds climate-vulnerable populations as beneficiaries and fossil-intensive sectors as victims; this reading adds workers/debtors as beneficiaries and creditors/savers as victims.',
    'Documents the kernel''s structural fracture lines for the commitment-system engine. If the beneficiary/victim sets are mutually exclusive across readings, the kernel is irreducibly contested (distributed authority). If they overlap substantially, the dispute is about weighting, not structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Structural delta between this reading and its kernel siblings for commitment-system classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 1999, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_mandate_expansive_tr_t1999, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 1999, 0.12).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2003, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2003, 0.15).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2008, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2012, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2012, 0.28).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2015, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2015, 0.31).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2020, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2024, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2024, 0.31).

% Extraction over time
narrative_ontology:measurement(ecb_mandate_expansive_be_t1999, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 1999, 0.18).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2003, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2003, 0.22).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2008, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2008, 0.31).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2012, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2012, 0.38).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2015, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2020, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2024, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(ecb_mandate_expansive_su_t1999, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 1999, 0.25).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2003, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2003, 0.3).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2008, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2008, 0.38).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2012, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2012, 0.45).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2015, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2020, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2024, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__expansive_secondary_objectives, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__climate_incorporation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, eu_fiscal_rules_stability_growth_pact).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, eu_economic_governance_framework_2024).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_monetary_policy_transmission_protection_instrument).

% DUAL FORMULATION NOTE:
% Part of the ecb_mandate_article_127 constraint family. Three readings of the same Treaty article: orthodox_price_stability (Mountain claim, low extraction), expansive_secondary_objectives (this story, Tangled Rope, moderate extraction), climate_incorporation (Tangled Rope, distinct beneficiary/victim set). All three share the kernel (Article 127 TFEU text) but instantiate different constraints with different ε, beneficiaries, victims, and suppression profiles. The upstream Mountain claim (orthodox) is cited as legitimacy cover for the downstream extractive readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_mandate_article_127__expansive_secondary_objectives, institutional, 0.15).
constraint_indexing:directionality_override(ecb_mandate_article_127__expansive_secondary_objectives, organized, 0.25).
constraint_indexing:directionality_override(ecb_mandate_article_127__expansive_secondary_objectives, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
