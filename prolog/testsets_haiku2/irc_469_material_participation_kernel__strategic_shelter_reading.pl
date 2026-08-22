% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strategic_shelter_reading, []).

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
 *   constraint_id: irc_469_material_participation_kernel__strategic_shelter_reading
 *   human_readable: IRC 469 Material Participation (Strategic Shelter Reading)
 *   domain: legal/regulatory/economic
 *
 * SUMMARY:
 *   IRC §469 (Passive Activity Loss Limitation) limits deductions of losses
 *   from passive real estate activities against wages and active business
 *   income. Under the strategic shelter reading, the statute's definition of
 *   'material participation' and 'active participation' is interpreted
 *   permissively: investors qualify by meeting hour-counting safe harbors
 *   (500+ hours, most significant participation test, or grouping elections
 *   under Treas. Reg. 1.469-4(e)). This reading is contested against the
 *   strict_gatekeeper_reading, which would require verifiable substantial
 *   personal labor with high documentation standards. The strategic shelter
 *   reading enables broad passive loss deductions and wealth preservation
 *   through leverage; the strict gatekeeper reading would limit deductions to
 *   investors with genuine operational involvement.
 *
 * KEY AGENTS:
 *   - Passive Loss Claiming Investors: High-income individuals owning real estate entities who structure material participation to claim losses against wages.
 *   - Tax Planning Professionals: Attorneys, CPAs, and real estate advisors who design and execute material participation strategies and hour-counting mechanics.
 *   - Wage Earning Taxpayers: W-2 earners without significant real estate holdings who bear effective tax rate increases to fund the revenue loss.
 *   - IRS Treasury Department: Administers the statute and publishes safe-harbor guidance; theoretically authoritative but structurally weaker than taxpayer interpretation.
 *   - Alternative Reading Advocates: Scholars, legislators, and Treasury officials advocating strict gatekeeper threshold (excluded from decision-making).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.68).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.52).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "IRC 469 Material Participation (Strategic Shelter Reading)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "legal/regulatory/economic").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, '2f4ffef8-0887-4bd0-9ad9-6aec0cd77aa3').
narrative_ontology:cs_kernel_codification('2f4ffef8-0887-4bd0-9ad9-6aec0cd77aa3', fixed_text).
narrative_ontology:cs_authority_grounding('2f4ffef8-0887-4bd0-9ad9-6aec0cd77aa3', extraction).
narrative_ontology:cs_interpretation_layer_present('2f4ffef8-0887-4bd0-9ad9-6aec0cd77aa3').
narrative_ontology:cs_reading_relation('2f4ffef8-0887-4bd0-9ad9-6aec0cd77aa3', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('2f4ffef8-0887-4bd0-9ad9-6aec0cd77aa3', foundational, statutory_language_permits_aggressive_participation).
narrative_ontology:cs_axiom_status(statutory_language_permits_aggressive_participation, holdable).
narrative_ontology:cs_axiom_grounding('2f4ffef8-0887-4bd0-9ad9-6aec0cd77aa3', statutory_language_permits_aggressive_participation, empirically_contingent).
narrative_ontology:cs_axiom('2f4ffef8-0887-4bd0-9ad9-6aec0cd77aa3', secondary, hour_counting_safe_harbors_constitute_participation).
narrative_ontology:cs_axiom_status(hour_counting_safe_harbors_constitute_participation, holdable).
narrative_ontology:cs_axiom_grounding('2f4ffef8-0887-4bd0-9ad9-6aec0cd77aa3', hour_counting_safe_harbors_constitute_participation, conventional).
narrative_ontology:cs_reference_frame('2f4ffef8-0887-4bd0-9ad9-6aec0cd77aa3', permissive_material_participation_framework).
narrative_ontology:cs_drift_state('2f4ffef8-0887-4bd0-9ad9-6aec0cd77aa3', contemporary_post_2008_crisis_enforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f4ffef8-0887-4bd0-9ad9-6aec0cd77aa3', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, passive_loss_claiming_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_planning_professionals).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, wage_earning_taxpayers).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, passive_investment_limited_partners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, limited_partners_non_materially_participant).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, limited_partners_non_materially_participant).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strategic_shelter_reading, taxpayer_intent_controls_classification).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strategic_shelter_reading, regulatory_detail_permits_aggressive_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% High-income individuals who own real estate entities and claim material participation deductions. Under the strategic shelter reading, they qualify by meeting hour-counting safe harbors (500+ hours, most significant participation, or grouping elections). This permits them to deduct passive losses against wages, dramatically reducing taxable income and enabling wealth preservation through leverage. Their exit option is arbitrage: if the strict reading prevails, they exit to alternative shelter strategies or accept passive loss limitations.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, passive_loss_claiming_investors, beneficiary,
    powerful, generational, arbitrage, national).

% Tax attorneys, CPAs, and real estate consultants who design material participation strategies. They benefit from demand for their expertise — the permissive threshold and technical hour-counting mechanics create steady consulting work. They shape interpretation through published guidance, tax authority engagement, testimony in legislative hearings, and representations in litigation. They have moderate exit: if the strict reading prevails, they pivot to advising on alternative strategies, but much of their real estate tax practice loses economic value.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_planning_professionals, agenda_setter,
    organized, biographical, mobile, national).

% W-2 earners without significant real estate or pass-through holdings. They cannot claim passive loss deductions and pay effective tax rates that subsidize the sheltering by passive-loss claimers at comparable income levels. They have no voice in the regulatory interpretation and cannot exit: their tax liability is a structural feature of their income source. Their burden rises as the strategic reading amplifies passive loss shelter.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, wage_earning_taxpayers, payer,
    moderate, biographical, constrained, national).

% Passive investors in real estate partnerships. When the managing partner claims material participation and shelters partnership losses, limited partners absorb loss allocations they cannot deduct against outside income. They gain real economic loss on their capital but cannot deduct it; they may receive phantom income allocations without corresponding cash distributions. Exit is constrained: they can redeem or sell their interest but face market friction and capital gains recognition.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, limited_partners_non_materially_participant, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strategic_shelter_reading, limited_partners_non_materially_participant, beneficiary).

% Administers IRC §469 and collects revenue. The strategic shelter reading reduces revenues by billions annually. Treasury publishes safe-harbor guidance (Treas. Reg. 1.469-5T) and audit priorities in response. Theoretically authoritative but structurally weaker than the taxpayer-friendly interpretation because the statute's language genuinely supports the permissive reading. Treasury position: the statute is permissive but legislative history and policy support narrower interpretation. IRS audit enforcement is inconsistent, balancing statutory language against policy intent.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, irs_treasury_department, observer,
    institutional, generational, analytical, national).

% Tax scholars, legislators, and Treasury policy officials who advocate the strict_gatekeeper_reading (material participation requires verifiable substantial labor). They cite legislative history, policy purpose, and concerns about high-income sheltering as warrant for a narrower reading. Their voices appear in academic commentary, congressional testimony, proposed legislation, and Treasury guidance disclaimers — but they occupy no seat in taxpayer compliance decisions or tax planning strategy design. They are structurally unable to enforce the strict reading because the statute's language supports the permissive one.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, alternative_reading_advocates, excluded,
    organized, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strategic_shelter_reading, passive_loss_claiming_investors).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strategic_shelter_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates passive real estate losses to investors who claim material participation, enabling multi-investor ownership structures where losses flow through to participating members without requiring full liability for partnership debts. Solves the coordination problem of how to distribute passive activity income and losses in partnership structures while maintaining investor limited liability.
% TRANSFER_FUNCTION: Transfers foregone federal income tax revenue from the general Treasury to passive-loss-claiming investors (via lower effective tax rates on wages and active income) and to tax planning professionals (via consulting demand for shelter design and documentation). The transfer magnitude: billions annually in foregone federal revenue, concentrated on high-income investors and distributed to tax professional networks.
% ABSENT_VOICES: Wage-earning taxpayers and non-materially-participating limited partners would argue for a narrower material participation threshold and stricter documentation standards; they are excluded from the interpretation process and have no seat at compliance-strategy tables. Alternative reading advocates (Treasury scholars, legislators) exist in academic and policy venues but lack authority over Section 469 administration. Their exclusion is structural: the interpretation is made through IRS guidance, taxpayer litigation strategy, and professional norm-setting — venues where beneficiaries have standing and excluded voices do not.
% DISAPPEARANCE_RATIONALE: If the permissive material participation standard disappeared and the statute reverted to a strict gatekeeper threshold, high-income real estate investors would lose billions in passive loss deductions (estimated $10–20 billion annually in foregone deductions based on IRS SOI data). Tax planning architecture would reorganize: investors would exit passive ownership in favor of active C-corporation structures or foreign entities; real estate partnerships would restructure to concentrate losses on genuinely active managers; demand for aggressive hour-counting consulting would collapse; effective tax rates for real estate investors would rise sharply relative to wage earners, narrowing the shelter-enabled wealth accumulation gap.
% FOUNDING_PROBLEM: IRC §469 (Passive Activity Loss Limitation, enacted 1986 Tax Reform Act) was enacted to prevent high-income earners from sheltering wages and active business income using losses from passive real estate investments they did not meaningfully manage. The statute's policy goal was to reserve passive activity loss deductions for investors with genuine operational involvement and participation.
% FOUNDING_PROBLEM_CORROBORATION: IRS Treasury scholarship (including IRS memoranda and legislative history annotations) attests the founding problem is live — high-income investors continue to shelter wages using aggressive passive loss claims, contrary to the statute's stated intent. Tax planning professionals counter that the statutory language (IRC §469(c)(1), 'actively participated'; Treas. Reg. 1.469-5T safe harbors) permits the shelter strategies they design. Academic tax scholars (citing GAO reports, Tax Foundation analyses, congressional testimony from 1990s–2020s) and Treasury policy advisors attest the problem persists and shelters continue unchecked, corroborating the assessment that the founding mandate is not met. The IRS has published guidance (e.g., IRS Notice 2009-71 on certain shelter arrangements) expressing concern about aggressive shelter planning, further corroborating that Treasury views the founding problem as unresolved.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.54 to 0.68 over the interval as shelter strategies mature, hour-counting mechanics become formalized in tax planning practice, and grouping elections proliferate. The constraint reaches a plateau at 0.68 (material participation saturation — most investors who want to shelter have engineered it). Theater ratio rises from 0.28 to 0.41 as documentation practices become increasingly formalistic: taxpayers maintain hour logs that conform to safe-harbor thresholds but may not reflect genuine operational involvement. Suppression is moderate (0.52) because the constraint's persistence depends on maintaining the permissive reading against periodic Treasury challenges and legislative pressure — active suppression of alternative readings through IRS safe-harbor guidance and taxpayer litigation success. Accessibility collapse is low (0.48) because the strict gatekeeper reading remains a live alternative in Treasury guidance, academic commentary, and legislative proposals; alternatives have not fully collapsed. Resistance is moderate-to-high (0.59) from wage earners and limited partners who bear the cost, but they lack institutional seats to enforce the strict reading.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (investors, tax professionals), the material participation constraint is genuine coordination solving multi-investor real estate ownership and enabling efficient loss allocation. From the payer seats (wage earners, non-materially-participating partners), the same structure operates as enforced extraction: they subsidize the sheltering through higher effective rates while having no say in the interpretation. The Treasury/IRS seat is conflicted: they are charged with administering the statute per its language (which supports the permissive reading) while Treasury policy preferences would favor the strict reading. This perspectival fracture is structural to the kernel contest itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Passive loss claiming investors occupy the beneficiary end of directionality (d near 0.0) — they collect the deductions and shape interpretation through professional networks. Tax planning professionals are agenda-setters (d near 0.25–0.35) — they set the technical standards but do not collect rents directly; their benefit flows through consulting demand. Wage earners and non-materially-participating limited partners occupy the target end (d near 0.85–1.0) — they bear the revenue loss without voice in interpretation. The IRS Treasury Department occupies a fractured position: theoretically authoritative (moderate power) but structurally unable to enforce the strict reading because the statute's language supports the permissive interpretation and taxpayers litigate successfully. Treasury's d is around 0.55–0.65 (moderate target, lacking leverage to change the interpretive equilibrium).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prevent high-income sheltering of wages using passive losses) is contested but not dead: Treasury scholarship, congressional testimony, and academic critics attest high-income sheltering continues despite the statute. The strategic shelter reading persists because the statute's language genuinely permits it, and beneficiaries have captured the interpretation machinery (tax professionals shape guidance, succeed in litigation, counsel compliance). The mandatrophy tension arises: the constraint persists precisely because its function (opening shelter rather than closing it) contradicts its nominal mandate. A strict gatekeeper reading would restore mandated function; the strategic shelter reading systematically inverts it. This is the core kernel contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hour_counting_materiality,
    'What constitutes credible evidence of ''material participation'' under Treas. Reg. 1.469-5T safe harbors — are contemporaneous hour logs sufficient, or does the regulation require evidence of genuine management involvement?',
    'Litigation testing hour-log documentation against operational reality (did the investor actually perform the reported hours, or merely document them in conformity with safe-harbor thresholds?); IRS audit adjustments to participation claims backed by detailed factual analysis.',
    'If hour logs alone satisfy the test, the permissive reading is robust and extractiveness remains high. If operational reality is required, many documented claims fail, and extractiveness falls toward the strict gatekeeper baseline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hour_counting_materiality, empirical, 'Whether hour-counting mechanics constitute genuine material participation or formalistic shelter.').

omega_variable(
    grouping_election_scope,
    'Do grouping elections under Treas. Reg. 1.469-4(e) permit taxpayers to aggregate passive real estate activities into a single activity eligible for material participation, or should each project be evaluated independently?',
    'Treasury guidance clarification or legislative amendment narrowing grouping scope; audit practice testing whether grouped activities share common management or are merely aggregated for deduction convenience.',
    'Broad grouping scope permits more taxpayers to cross the material participation threshold by concentrating hours on a subset of investments; narrow scope restricts grouping and raises the effective bar for participation claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grouping_election_scope, empirical, 'Whether grouping elections enable permissive shelter or require operational integration.').

omega_variable(
    statutory_language_vs_intent,
    'Is IRC §469(c)(1) and its ''active participation'' definition meant to embody the statutory language''s literal terms (permissive, hour-counting based) or the legislative intent from the 1986 Tax Reform Act (preventing high-income sheltering)?',
    'Appellate litigation applying statutory interpretation canons (textualism vs. purposivism); congressional clarification through committee reports or amendment; Treasury non-acquiescence to unfavorable case law.',
    'Textualist reading supports the strategic shelter interpretation; purposivist reading supports the strict gatekeeper reading. The choice fundamentally determines which reading is the ''true'' constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_language_vs_intent, conceptual, 'Whether the kernel is settled by statutory text or legislative purpose — the core reading boundary.').

omega_variable(
    suppression_mechanism_legality,
    'Is the suppression of the strict gatekeeper reading through strategic litigation (taxpayers bringing cases challenging aggressive IRS positions) and professional norm-setting (tax bar solidifying permissive interpretations) a legitimate exercise of statutory interpretation, or does it constitute evasion of the statute''s intent?',
    'Congressional reassessment of the passive loss regime and explicit statutory amendment narrowing material participation; Treasury shift to clear administrative non-acquiescence in permissive case law.',
    'If legitimate, the suppression is structural and the strategic reading remains stable. If illegitimate (intent-evasion), legislative remedy would rebalance toward the strict reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_legality, preference, 'Whether the interpretive suppression of the strict reading is normatively justified or constitutes regulatory capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(irc__tr_t0, observed).
narrative_ontology:measurement(irc__tr_t5, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(irc__tr_t5, observed).
narrative_ontology:measurement(irc__tr_t10, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(irc__tr_t10, observed).
narrative_ontology:measurement(irc__tr_t15, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(irc__tr_t15, observed).
narrative_ontology:measurement(irc__tr_t20, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(irc__tr_t20, observed).
narrative_ontology:measurement(irc__tr_t25, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(irc__tr_t25, observed).
narrative_ontology:measurement(irc__tr_t30, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(irc__tr_t30, observed).
narrative_ontology:measurement(irc__tr_t40, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(irc__tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(irc__be_t0, observed).
narrative_ontology:measurement(irc__be_t5, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(irc__be_t5, observed).
narrative_ontology:measurement(irc__be_t10, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(irc__be_t10, observed).
narrative_ontology:measurement(irc__be_t15, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(irc__be_t15, observed).
narrative_ontology:measurement(irc__be_t20, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(irc__be_t20, observed).
narrative_ontology:measurement(irc__be_t25, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(irc__be_t25, observed).
narrative_ontology:measurement(irc__be_t30, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(irc__be_t30, observed).
narrative_ontology:measurement(irc__be_t40, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(irc__be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(irc__su_t0, observed).
narrative_ontology:measurement(irc__su_t5, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement_basis(irc__su_t5, observed).
narrative_ontology:measurement(irc__su_t10, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement_basis(irc__su_t10, observed).
narrative_ontology:measurement(irc__su_t15, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(irc__su_t15, observed).
narrative_ontology:measurement(irc__su_t20, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(irc__su_t20, observed).
narrative_ontology:measurement(irc__su_t25, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement_basis(irc__su_t25, observed).
narrative_ontology:measurement(irc__su_t30, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(irc__su_t30, observed).
narrative_ontology:measurement(irc__su_t40, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(irc__su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(irc_469_material_participation_kernel__strategic_shelter_reading, 0.12).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel__strict_gatekeeper_reading).

% DUAL FORMULATION NOTE:
% The irc_469_material_participation_kernel decomposes into two constraint stories representing contested readings of the same statutory kernel (IRC §469). The strategic_shelter_reading (this story) interprets material participation permissively via safe-harbor hour-counting and grouping elections, enabling passive loss deductions and wealth preservation. The strict_gatekeeper_reading would require verifiable substantial personal labor with high documentation standards, narrowing the population eligible for material participation deductions. The two readings have opposite structural effects: strategic reading broadens access and reduces compliance friction; strict reading narrows access and raises documentation burden. They are linked via network.affects_constraints because the triumph of one reading would foreclose the viability of the other within any single coherent regulatory framework. Both remain live positions in contemporary tax law dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irc_469_material_participation_kernel__strategic_shelter_reading, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
