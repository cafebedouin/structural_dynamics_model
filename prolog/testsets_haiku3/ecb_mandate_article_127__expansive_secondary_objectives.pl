% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Mandate Article 127: Expansive Secondary Objectives Reading
 *   domain: monetary_policy/constitutional_law/institutional_governance
 *
 * SUMMARY:
 *   Article 127 TFEU establishes the ECB's primary mandate: 'the maintenance
 *   of price stability.' It adds that the ECB shall 'support the general
 *   economic policies of the Union' in pursuit of 'high employment and
 *   sustainable, non-inflationary growth' — secondary objectives that operate
 *   'without prejudice to the objective of price stability.' This constraint
 *   instantiates one reading: the expansive interpretation that 'without
 *   prejudice' authorizes discretionary weighting of secondary objectives
 *   whenever price stability is not threatened. Under this reading, the ECB
 *   holds the structural authority to balance employment, growth, and
 *   financial stability against inflation control, using its own judgment
 *   about what 'not threatened' means. Orthodox and climate-incorporation
 *   readings contest this interpretation. The constraint is fundamentally
 *   about treaty scope — what the legal instrument permits — not about actual
 *   conduct. Operationality (whether the ECB actually deploys
 *   secondary-objective weight) is captured in separate omegas and
 *   measurements, not in the core constraint definition.
 *
 * KEY AGENTS:
 *   - ECB Governing Council: institutional agenda-setter, formulates policy under the reading's authorization structure
 *   - Eurozone workers (organized labor): structural beneficiary when secondary objectives weight employment
 *   - Eurozone debtors: structural beneficiary when secondary objectives permit accommodative stance
 *   - Member states (especially fiscal-constrained periphery): secondary beneficiary, gains fiscal room when ECB secondary objectives reduce austerity pressure
 *   - Inflation-sensitive savers: structural victim, value of savings erodes if secondary objectives authorized accommodative stance
 *   - Creditors holding fixed-rate euro instruments: structural victim, real returns compress if secondary objectives sustain higher inflation
 *   - Orthodox inflation-targeting states (Bundesbank constituency, price-stability maximizers): victim of suppressed alternatives (constrained from pursuing tighter policy)
 *   - Climate policymakers: excluded voice, would argue for climate-risk integration as tertiary objective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.42).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.38).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.42).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Mandate Article 127: Expansive Secondary Objectives Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary_policy/constitutional_law/institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, 'c50a2019-46b5-42ff-bae1-828170339a52').
narrative_ontology:cs_kernel_codification('c50a2019-46b5-42ff-bae1-828170339a52', fixed_text).
narrative_ontology:cs_authority_grounding('c50a2019-46b5-42ff-bae1-828170339a52', lineage).
narrative_ontology:cs_interpretation_layer_present('c50a2019-46b5-42ff-bae1-828170339a52').
narrative_ontology:cs_reading_relation('c50a2019-46b5-42ff-bae1-828170339a52', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('c50a2019-46b5-42ff-bae1-828170339a52', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('c50a2019-46b5-42ff-bae1-828170339a52', foundational, discretionary_secondary_balancing).
narrative_ontology:cs_axiom_status(discretionary_secondary_balancing, holdable).
narrative_ontology:cs_axiom_grounding('c50a2019-46b5-42ff-bae1-828170339a52', discretionary_secondary_balancing, conventional).
narrative_ontology:cs_axiom('c50a2019-46b5-42ff-bae1-828170339a52', foundational, price_stability_not_threatened_as_permission_trigger).
narrative_ontology:cs_axiom_status(price_stability_not_threatened_as_permission_trigger, holdable).
narrative_ontology:cs_axiom_grounding('c50a2019-46b5-42ff-bae1-828170339a52', price_stability_not_threatened_as_permission_trigger, conventional).
narrative_ontology:cs_reference_frame('c50a2019-46b5-42ff-bae1-828170339a52', discretionary_secondary_objectives_framework).
narrative_ontology:cs_drift_state('c50a2019-46b5-42ff-bae1-828170339a52', contemporary_post_pandemic_tightening, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c50a2019-46b5-42ff-bae1-828170339a52', '2026-06-19T00:00:00Z').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, workers_employed_in_eurozone).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, debtors_indebted_in_euros).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, member_states_fiscal_capacity).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, inflation_sensitive_savers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, creditors_holding_fixed_euros).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_inflation_targeting_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_inflation_maximizer_states).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, monetary_policy_redistributive_function).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, employment_as_constitutional_good).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, discretionary_central_banking_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formulates monetary policy under Article 127 TFEU. The expansive reading grants them authority to discretionarily weight secondary objectives (employment, growth, financial stability) when price stability is not threatened. They interpret the treaty, set the 'not threatened' threshold operationally, and communicate policy rationale. Their choices reverberate across 20 member states. They exit through reinterpretation (shift toward orthodox reading) or resignation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, continental).

% Benefit structurally when secondary objectives weight employment and growth. Accommodative ECB policy (QE, low rates, forward guidance supporting employment) expands job availability and wage growth. They are structurally locked into euro membership; exit would require emigration. They depend on the ECB's discretionary interpretation to weigh their welfare.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, workers_employed_in_eurozone, beneficiary,
    moderate, biographical, constrained, continental).

% Benefit when secondary objectives permit accommodative stance: lower real interest rates, slower debt accumulation in real terms, capital gains on equity holdings. Include households with mortgages, corporations with euro-denominated debt, and sovereigns with euro liabilities. Trapped in euro denominations; exit requires asset redenomination or default.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, debtors_indebted_in_euros, beneficiary,
    moderate, biographical, constrained, continental).

% Benefit when ECB secondary-objective weighting sustains accommodative policy, reducing austerity pressure. Governments with large debts or cyclical deficits (peripheral member states especially) gain fiscal space. ECB accommodation reduces bond yields, making sovereigns' refinancing cheaper. Can exit via EU exit or via appeal for different ECB interpretation, but both are costly.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, member_states_fiscal_capacity, beneficiary,
    powerful, generational, mobile, continental).

% Bear the cost when secondary objectives permit higher inflation as the price of employment support or financial stability. Savers with fixed-rate savings, pensions indexed to nominal rather than real returns, and elderly on fixed incomes suffer erosion of real wealth. They can redenominate into foreign currency or cryptocurrencies (constrained exit), but cannot leave the euro system unilaterally.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, inflation_sensitive_savers, payer,
    moderate, biographical, constrained, continental).

% Bear real-return compression if secondary objectives authorize higher inflation. Bondholders, creditors to sovereigns and corporations, foreign central banks with euro reserves all lose purchasing power if accommodative policy persists. Can shift portfolios to other currencies or asset classes (mobile exit), but Europe-focused institutions face costs.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, creditors_holding_fixed_euros, payer,
    powerful, biographical, mobile, continental).

% Lose structural discretion under the expansive reading. States (Germany, Netherlands, and their allied constituencies) that prioritize price stability and object to secondary-objective operationalization find their preferred policy path (tighter, more restrictive) suppressed by the reading. Can appeal for orthodox reinterpretation, but face institutional and political headwinds. Exit via EU departure is costly; can shift toward fiscal policy or regulatory leverage instead.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_inflation_maximizer_states, payer,
    powerful, generational, mobile, continental).

% Would argue for climate-risk integration as a tertiary objective (alongside employment and growth). Structurally excluded from the secondary-objective set under this reading — climate appears at the margins (collateral framework adjustments, rhetoric) but not as an operational objective. The expansive reading permits climate incorporation (influences the climate reading), but does not mandate it. Can lobby for formal treaty amendment or for ECB practice shift, but institutional path is lengthy.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, climate_policymakers, excluded,
    organized, generational, constrained, continental).

% Constitutes the epistemic seat from which the orthodox reading is defended. Economists, legal scholars, and think tanks argue that Article 127 requires exclusive price-stability focus and that secondary objectives violate the constraint by design. They contest the expansive reading's interpretation and document perceived policy deviations. No material stakes, but legitimacy-shaping role.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, academic_orthodox_constituency, observer,
    organized, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__expansive_secondary_objectives, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates monetary policy across 20 member states with divergent inflation preferences, growth targets, and distributional priorities. Without a supranational monetary authority empowered to trade off these objectives, member states would pursue conflicting policies (some tight, some loose), fragmenting the monetary union. The constraint establishes a single, discretionary arbiter (the ECB) authorized to balance competing EU-level objectives (price stability, employment, growth, financial stability) without requiring unanimous agreement on every decision.
% TRANSFER_FUNCTION: Moves real income from creditors, savers with fixed-rate assets, and inflation-averse constituencies TO debtors, workers, and fiscal-constrained member states, through the ECB's discretionary weighting of secondary objectives toward accommodation. The transfer is executed via asset purchases (QE), forward guidance signaling low future rates, and collateral adjustments that affect borrowing costs and asset valuations. The magnitude of transfer depends on how much the ECB operationalizes secondary objectives versus prioritizing price stability.
% ABSENT_VOICES: Climate policymakers are excluded — climate risk is not an operational objective in the secondary-objective set, though the reading permits future incorporation. Orthodox inflation-maximizer states (Germany, Netherlands, and allied constituencies) are nominally included but structurally suppressed by the reading's authorization of discretionary secondary-objective weighting.
% DISAPPEARANCE_RATIONALE: If the expansive secondary-objectives reading disappeared and the orthodox price-stability reading took exclusive hold, ECB policy would shift substantially: lower accommodation during crises, tighter collateral standards, lower tolerance for inflation outcomes, reduced asset purchase programs. Eurozone inflation would likely trend lower and more stable; employment and growth would suffer; member-state fiscal pressures would intensify; the distribution of real income would shift from debtors to creditors. The constraint's disappearance would not eliminate coordination (the ECB would still exist), but would alter its bias dramatically.
% FOUNDING_PROBLEM: Early monetary union (1999 onwards) required a unified policy framework to prevent competitive devaluations, coordinate responses to asymmetric shocks, and maintain price stability in a multi-state system. The ECB was granted price-stability primacy. However, the eurozone proved heterogeneous in inflation tolerance and growth priorities — peripheral states had higher unemployment, higher debt burdens, and lower growth rates than core states. The secondary-objectives clause was included to permit the ECB flexibility to balance these divergent needs without requiring constant treaty renegotiation. The expansive reading interprets this flexibility as discretionary authority to operationalize employment and growth support when inflation space permits.
% FOUNDING_PROBLEM_CORROBORATION: The ECB (especially post-2008) attests the founding problem remains live and secondary objectives are operationally warranted. Member-state governments, labor unions, and fiscal-constraint-facing states corroborate that employment and growth support are necessary. Orthodox states (Bundesbank, Dutch central bank) and academic orthodox economists attest the founding problem was adequately solved by price-stability focus and secondary objectives should be restrained. International economic analyses (IMF, OECD) provide mixed corroboration — some support flexible secondary objectives in crisis contexts, others warn of long-run inflation costs. No dominant external consensus; corroboration is split by institutional and ideological lines.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.42 at interval end) because the constraint permits discretionary balancing without mandating it — permission structures are less extractive than coercive rules, and the ECB retains the option (not the obligation) to weight secondary objectives. However, extractiveness rises from 0.28 to 0.42 across the interval as the constraint's operational use accumulates evidence (2008-2025: QE, forward guidance, collateral framework adjustment all deployed under secondary-objective justification). Suppression is moderate (0.38) because orthodox interpretation remains live among member states and the ECB's own staff — the constraint actively suppresses this alternative by establishing a wider permission structure. Theater is low-moderate (0.28): the secondary objectives are genuinely operational in some policy phases (2012-2015 crisis response, 2020-2021 pandemic), but become less prominent in tighter regimes (post-2022 rate-hike phase). The measurements track both the deepening operationality and the cyclicality: extraction peaks around time=15 (peak operational deployment of secondary objectives in pandemic phase), then recedes as orthodox priorities reassert. Theater peaks with the same cycle (performative emphasis on secondary objectives in low-inflation phases, downplayed in high-inflation phases). Suppression remains elevated throughout because the reading's existence continuously forecloses member-state arguments for tighter policy under an exclusive-price-stability reading.
 *
 * PERSPECTIVAL GAP:
 *   The ECB Governing Council (agenda-setter seat) computes this as a coordination function — balancing multiple EU objectives within the constraint of EU treaty scope. Workers, debtors, and fiscal-constrained member states (beneficiary seats) compute it as a transfer mechanism — the discretionary weighting of secondary objectives redistributes welfare toward them. Orthodox states and creditor constituencies (victim/suppressed-alternative seats) compute it as extraction — the constraint permits the ECB to deploy accommodation they see as violating the primary mandate, extracting from them to benefit others. The engine computes per-seat types from these structural asymmetries; the authored metrics reflect the aggregate across all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   ECB Governing Council: d ≈ 0.15–0.25 (beneficiary through institutional power and discretionary authority, high exit — can reinterpret the constraint). Eurozone workers: d ≈ 0.65–0.75 (beneficiary through secondary-objective weight on employment, but exit is structural — cannot leave the eurozone system). Debtors: d ≈ 0.55–0.65 (beneficiary through accommodation, moderate exit if they can refinance outside euros). Member-state governments: d ≈ 0.35–0.45 (mixed — fiscal-constrained states benefit from reduced austerity pressure; orthodox states lose discretion). Savers and creditors: d ≈ 0.75–0.85 (targets — they bear inflation cost, trapped in euro). No directionality overrides needed; the structural data (beneficiary/victim + exit) drives the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordination of European monetary policy post-1999) is LIVE but its FUNCTION has partially attrophied. The original coordination need (preventing hyperinflation, stabilizing exchange rates post-Bretton Woods) remains real. But the constraint's operational loading has shifted: it now carries distributional objectives (employment support, fiscal transfer through ECB balance-sheet expansion) that are not coordination functions but extraction mechanisms. This is not mandatrophy proper (which would be total function loss + theatrical persistence) — it is a hybrid: genuine coordination core + grafted extraction function + suppression of alternatives. Classification as tangled_rope rather than pure snare reflects this: the coordination is real, the extraction is real, both are active. A piton candidate would show higher theater (performative emphasis without operational weight); measurements show actual operational deployment during crisis phases. The constraint is legitimately tangled rather than degraded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    price_stability_threshold_ambiguity,
    'What constitutes ''price stability not threatened'' — is it a binary gate (inflation <2%) or a continuous permission structure allowing secondary objectives to accumulate weight as inflation distance increases?',
    'Formal ECB guidance documents (OMs, working papers) specifying the operational threshold; comparing stated thresholds across policy cycles; observing how secondary objective weight scaled with inflation gap in actual decisions.',
    'A binary interpretation (orthodox reading) vs. a continuous permission structure (expansive reading) changes who can claim legitimacy to challenge ECB decisions. Continuous permission would amplify this reading''s extraction relative to orthodox, making the constraint more snare-like at low-inflation moments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_stability_threshold_ambiguity, conceptual, 'The operationalization of ''price stability not threatened'' as gate versus gradient.').

omega_variable(
    secondary_objectives_operational_status,
    'Are secondary objectives (employment, growth, financial stability) genuinely operational — feeding decision criteria, asset allocation, forward guidance — or merely rhetorical cover for decisions driven by primary price stability mandate?',
    'Econometric analysis of ECB decisions (asset purchase composition, policy rate moves, guidance shifts) versus counterfactual orthodox-mandate models; interviews with policy committee members on decision weight; comparing stated objective emphasis to observable conduct.',
    'If secondary objectives are operational, extraction is moderate (balancing function) and beneficiaries are real. If rhetorical, extraction is higher (discretion deployed covertly) and the constraint becomes snare-like despite stated coordination. The measured extractiveness rests on assumption of operationality; if false, chi rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_objectives_operational_status, empirical, 'Whether secondary objectives genuinely guide policy or provide post-hoc justification for primary-mandate decisions.').

omega_variable(
    reading_fidelity_to_treaty_text,
    'Does the treaty language ''without prejudice to the objective of price stability'' actually permit discretionary balancing, or does it merely state that secondary objectives must not CONTRADICT price stability (allowing subordination, not discretionary weighting)?',
    'Comparative textual analysis against other EU treaty passages using ''without prejudice''; legal scholarship on treaty interpretation conventions in EU constitutional law; ECJ jurisprudence on Article 127 scope.',
    'Textual interpretation determines the axiom ''discretionary_secondary_balancing'' status: if treaty permits discretion, axiom is holdable; if treaty only permits non-contradiction (subordination), axiom is overridden by the instrument itself. This is the core reading contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_fidelity_to_treaty_text, conceptual, 'Treaty text interpretation: discretionary balancing versus mandatory subordination.').

omega_variable(
    kernelreading_natural_law_ambiguity,
    'Is this constraint one reading of a single kernel (Article 127 TFEU), or does it describe two distinct constraints — one about what the treaty permits (the kernel) and one about how the ECB exercises that permission operationally?',
    'This is a structure-of-the-reading question: the engine routes through omega_C (committer framing). The answer is: one kernel (Article 127), one reading instantiated here (the expansive interpretation), the operational question (whether discretion is actually deployed) is an empirical question captured in the secondary_objectives_operational_status omega.',
    'No classification impact; this clarifies that the constraint is ABOUT the treaty''s scope (the permission structure), not about historical ECB conduct. If conduct diverged (e.g., treaty permits discretion but ECB never uses it), the constraint would still be snare-classified as authorizing extraction even if extraction were not deployed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernelreading_natural_law_ambiguity, conceptual, 'Kernel-vs-conduct boundary: this constraint instantiates the reading of treaty permission, not the observational history of discretion use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(ecb__tr_t0, observed).
narrative_ontology:measurement(ecb__tr_t5, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(ecb__tr_t5, observed).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(ecb__tr_t10, observed).
narrative_ontology:measurement(ecb__tr_t15, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(ecb__tr_t15, observed).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(ecb__tr_t20, observed).
narrative_ontology:measurement(ecb__tr_t25, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(ecb__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(ecb__be_t0, observed).
narrative_ontology:measurement(ecb__be_t5, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(ecb__be_t5, observed).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 10, 0.39).
narrative_ontology:measurement_basis(ecb__be_t10, observed).
narrative_ontology:measurement(ecb__be_t15, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 15, 0.43).
narrative_ontology:measurement_basis(ecb__be_t15, observed).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 20, 0.41).
narrative_ontology:measurement_basis(ecb__be_t20, observed).
narrative_ontology:measurement(ecb__be_t25, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 25, 0.42).
narrative_ontology:measurement_basis(ecb__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(ecb__su_t0, observed).
narrative_ontology:measurement(ecb__su_t5, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 5, 0.32).
narrative_ontology:measurement_basis(ecb__su_t5, observed).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(ecb__su_t10, observed).
narrative_ontology:measurement(ecb__su_t15, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(ecb__su_t15, observed).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 20, 0.39).
narrative_ontology:measurement_basis(ecb__su_t20, observed).
narrative_ontology:measurement(ecb__su_t25, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 25, 0.38).
narrative_ontology:measurement_basis(ecb__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__expansive_secondary_objectives, 0.18).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% Three constraint stories instantiate competing readings of Article 127 TFEU. Each reading fixes a different referent for the constraint: (1) ORTHODOX reading: Article 127 requires exclusive price-stability focus, secondary objectives must not be operationalized. (2) EXPANSIVE reading (this story): Article 127 permits discretionary secondary-objective weighting within price-stability guardrails. (3) CLIMATE reading: Article 127 requires climate-risk integration into policy operations. Each story carries its own ε, beneficiary/victim structure, and claimed type. They are NOT three measurements of one constraint; they are three distinct constraints grounded in competing interpretations of the same treaty text. The readings influence one another: the expansive reading creates structural space for the climate reading (a climate objective would sit alongside employment/growth in the secondary-objective set). The orthodox reading forecloses both (exclusive price-stability focus rules out secondary-objective operationality altogether). The network links enable the contamination-propagation system to track how one reading's strength affects the others' viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_mandate_article_127__expansive_secondary_objectives, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
