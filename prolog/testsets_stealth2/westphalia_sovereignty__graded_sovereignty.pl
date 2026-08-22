% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__graded_sovereignty, []).

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
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty: Capacity-Calibrated Intervention Legitimacy
 *   domain: international law/political theory/state systems
 *
 * SUMMARY:
 *   Since the early 1990s, a working substitute for categorical
 *   non-intervention has consolidated: territorial authority is treated as a
 *   measurable quantity, and the legitimacy of external action toward a
 *   territory is calibrated to its measured governing capacity. Peacekeeping
 *   mandates, governance-conditioned lending, benchmark reviews, and
 *   trusteeship-style administrations (Bosnia, Kosovo, Timor-Leste)
 *   operationalize a spectrum from full to nominal authority. The arrangement
 *   solves real problems — pooled crisis response, humanitarian access,
 *   development finance where domestic institutions cannot intermediate —
 *   while transferring decision authority from weak states to an evaluator
 *   class of Security Council powers, financial institutions, donor
 *   governments, and indicator publishers. This file is ONE READING of the
 *   westphalia_sovereignty kernel (graded_sovereignty); the sibling readings
 *   are separate constraints linked in network.affects_constraints. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   (genuine coordination function plus asymmetric terms) while the authored
 *   metrics describe substantially extractive, actively enforced operation —
 *   the engine measures that divergence; the claim is not reconciled to the
 *   metrics. KEY AGENTS (by structural relationship): -
 *   permanent_security_council_members: Agenda-setting authority
 *   (institutional/arbitrage) — authorizes or vetoes intervention mandates
 *   and writes capacity thresholds into resolutions -
 *   international_financial_institutions: Administrator and collector
 *   (institutional/arbitrage) — runs conditionality regimes, sets benchmark
 *   frameworks, collects interest and policy leverage -
 *   bilateral_donor_governments: Collecting participant (powerful/mobile) —
 *   channels governance-conditioned aid, redirects bilaterally when
 *   multilateral rules chafe - governance_indicator_publishers: Epistemic
 *   collector (organized/mobile) — produce the rankings that operationalize
 *   the capacity spectrum - fragile_states_under_international_oversight:
 *   Bearer of oversight costs (powerless/trapped) — accept missions, adjusted
 *   budgets, and benchmark review as the price of recognition and finance -
 *   heavily_indebted_post_colonial_states: Bearer of conditionality cycles
 *   (moderate/constrained) — repeated reform programs, limited agenda power,
 *   marginal creditor shopping -
 *   civilian_populations_of_intervened_territories: Bearer of intervention
 *   consequences (powerless/trapped) — displacement and disruption under
 *   mandates justified in their name - emerging_alternative_patrons: Excluded
 *   rival (powerful/arbitrage) — offer unconditional finance, barred from
 *   standard-setting - public_international_law_scholars: Analytical observer
 *   (analytical/analytical) — map the doctrine-practice gap
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.62).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.62).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.62).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty: Capacity-Calibrated Intervention Legitimacy").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international law/political theory/state systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, 'ee5bcba4-b086-4371-8748-0dc6506d1401').
narrative_ontology:cs_kernel_codification('ee5bcba4-b086-4371-8748-0dc6506d1401', formalized).
narrative_ontology:cs_authority_grounding('ee5bcba4-b086-4371-8748-0dc6506d1401', expertise).
narrative_ontology:cs_interpretation_layer_present('ee5bcba4-b086-4371-8748-0dc6506d1401').
narrative_ontology:cs_reading_relation('ee5bcba4-b086-4371-8748-0dc6506d1401', westphalia_sovereignty__westphalia_absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('ee5bcba4-b086-4371-8748-0dc6506d1401', westphalia_sovereignty__westphalia_conditional_responsibility, influences).
narrative_ontology:cs_axiom('ee5bcba4-b086-4371-8748-0dc6506d1401', foundational, territorial_authority_is_scalar).
narrative_ontology:cs_axiom_status(territorial_authority_is_scalar, holdable).
narrative_ontology:cs_axiom_grounding('ee5bcba4-b086-4371-8748-0dc6506d1401', territorial_authority_is_scalar, empirically_contingent).
narrative_ontology:cs_axiom('ee5bcba4-b086-4371-8748-0dc6506d1401', secondary, stewardship_duty_of_capable_powers).
narrative_ontology:cs_axiom_status(stewardship_duty_of_capable_powers, holdable).
narrative_ontology:cs_axiom_grounding('ee5bcba4-b086-4371-8748-0dc6506d1401', stewardship_duty_of_capable_powers, instrumental).
narrative_ontology:cs_reference_frame('ee5bcba4-b086-4371-8748-0dc6506d1401', capacity_graded_authority_spectrum).
narrative_ontology:cs_drift_state('ee5bcba4-b086-4371-8748-0dc6506d1401', contemporary_multipolar_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ee5bcba4-b086-4371-8748-0dc6506d1401', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, permanent_security_council_members).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, international_financial_institutions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, bilateral_donor_governments).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, governance_indicator_publishers).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, fragile_states_under_international_oversight).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, heavily_indebted_post_colonial_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, civilian_populations_of_intervened_territories).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, failed_states_doctrine).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, good_governance_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authorize, veto, or decline intervention mandates and write capacity thresholds into resolutions. Decide which measured deficits license action and which do not. Are structurally exempt from the evaluations they administer, and can route around adverse findings through veto power and parallel diplomacy. Collect discretionary authority over when external action toward a territory is legitimate.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, permanent_security_council_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Design and administer lending programs whose terms embed governance benchmarks, fiscal targets, and institutional reforms. Publish the assessments that rank borrowing governments' capacity. Collect interest, repayment streams, and durable policy leverage over program countries. Borrowers bear the terms; the institutions face no comparable external review of their own forecasting record.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, international_financial_institutions, beneficiary).

% Channel development assistance through governance-conditioned frameworks and tie disbursement to benchmark performance. Gain influence over recipient policy, procurement access, and alignment of recipient votes in international fora. Can redirect funds bilaterally or withdraw when multilateral rules constrain them, and face no reciprocal conditionality on their own domestic arrangements.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, bilateral_donor_governments, beneficiary,
    powerful, biographical, mobile, continental).

% Produce the composite indices and country ratings that operationalize the capacity spectrum for lenders, mandate authors, and donors. Collect funding, citation authority, and agenda-setting influence from the demand for measurement. Are not elected by, accountable to, or themselves ranked by the states they score.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, governance_indicator_publishers, beneficiary,
    organized, biographical, mobile, global).

% Governments of low-capacity territories accept oversight missions, internationally supervised budgets, and recurring benchmark review as the price of diplomatic recognition, concessional finance, and security deployment. Cannot exit the state system or relocate their territory. Receive protective and financial flows from the same structure that constrains them, and hold formal but not agenda-setting voice in the bodies that evaluate them.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, fragile_states_under_international_oversight, payer,
    powerless, biographical, trapped, national).

% Cycle through successive reform programs whose terms are set externally: privatization schedules, subsidy removals, fiscal ceilings. Retain more formal voice than collapsed-state counterparts and can marginally shop among creditors or delay programs, but cannot set the benchmark frameworks their compliance is measured against. Individually weak, with coalition options through groupings of similarly positioned states.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, heavily_indebted_post_colonial_states, payer,
    moderate, biographical, constrained, national).

% Bear the physical consequences of mandates executed in their name: displacement, disrupted services, occupation economies, and the volatility that follows transitions between mission phases. Are invoked as the protected party legitimizing external action but hold no seat in mandate design, benchmark setting, or exit timing. Mobility is limited to flight, usually internal or into neighboring states.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, civilian_populations_of_intervened_territories, payer,
    powerless, immediate, trapped, local).

% Offer finance, infrastructure, and security cooperation without governance conditionality, giving constrained governments an outside option the evaluation regime does not control. Are largely absent from the Western-led standard-setting fora where benchmarks and intervention criteria are drafted. Their growing market share erodes the leverage the conditionality system depends on.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, emerging_alternative_patrons, excluded,
    powerful, generational, arbitrage, continental).

% Document the widening distance between the Charter's formal text on domestic jurisdiction and the capacity-calibrated practice of mandates, conditionality, and supervised administration. Trace selective application across cases and publish the doctrinal reconstructions that future negotiations will inherit. Hold no enforcement role and bear none of the arrangement's costs.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, public_international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__graded_sovereignty, international_financial_institutions).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__graded_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework for international action toward territories whose governments cannot guarantee basic functions: pooled peacekeeping, humanitarian access corridors, development finance intermediation, and burden-sharing that a strictly inviolability-based rule cannot organize.
% TRANSFER_FUNCTION: Moves decision authority and policy autonomy from weak and indebted states to external evaluation and intervention authorities; moves financial resources inward under externally set conditions; moves status and epistemic authority upward to the evaluator institutions; moves the physical costs of intervention onto the populations of the territories concerned.
% ABSENT_VOICES: Civilian populations of overseen territories are absent from every room where benchmarks, mandate scopes, and exit timelines are set. Weak-state governments are formally present in general assemblies and boards but without agenda power. Alternative-patron powers are excluded from standard-setting altogether. Each would contest the calibration: populations on exit timing and mandate conduct, weak states on benchmark content, alternative patrons on the evaluation monopoly itself.
% DISAPPEARANCE_RATIONALE: If the capacity-calibrated framework vanished overnight, standing peacekeeping mandates, governance-conditioned lending, supervised administrations, and debt-relief compacts would lose their legitimating basis simultaneously. The system would rearrange around one of two poles: reversion toward strict inviolability, abandoning state-failure response to regional powers and chance, or openly improvised great-power intervention stripped of the evaluation vocabulary — either way, the current division of authority between evaluator class and overseed states would dissolve and be renegotiated.
% FOUNDING_PROBLEM: How should a system of formally equal sovereign states respond to territories whose governments collapse into atrocity or administrative vacuum, given that the categorical non-intervention rule leaves such situations legally unaddressable — the gap exposed by the Somalia and Rwanda failures of the early 1990s?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the African Union's constitutive act and its 'non-indifference' doctrine attest the capacity-deficit problem from a victim-side seat while disputing who should answer it; G77 ministerial declarations attest the problem's persistence while rejecting the current remedy's terms; operational reports from humanitarian organizations working in collapsed-state territories document the unmet need the arrangement claims to address; and International Law Commission commentary records the doctrinal gap. The beneficiary parties obviously attest the problem as well, but the corroboration above does not depend on them.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__graded_sovereignty, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__graded_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__graded_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.62 at interval end) because the terms of oversight are set by the evaluators: conditionality packages, benchmark definitions, and mandate scopes are not negotiated at parity, and the autonomy transferred from weak states is decoupled from the services returned. Suppression (0.62) is authored as a raw structural property — the engine alone scales extractiveness by directionality and scope — and reflects the machinery that keeps weak seats compliant: aid cutoffs, sanctions, exclusion from agenda-setting, and the veto gate. Theater is moderate (0.34): governance indicators and capacity reviews multiply faster than capacity-building outcomes, a growing share of activity ranks and reports rather than builds, yet peacekeeping and concessional finance remain functionally real. Accessibility_collapse is moderate-low (0.48): exits partially persist — alternative patrons, regional organizations, creditor shopping — but no state can leave the international system itself. Resistance is elevated (0.58): sustained sovereignty counter-discourse, G77 and African Union mobilization, and the growth of unconditional-financing rivals. The temporal series run on ONE shared grid (every tracked metric authored at every time point 0-35). The trajectory is rise-then-plateau-with-slight-retreat: extraction and enforcement machinery built through the 1990s and peaked in the post-9/11 intervention decade (t15-t20), then receded modestly as veto gridlock, intervention fatigue, and multipolar financing eroded the evaluation monopoly — the retreat is why the suppression_requirement series is authored at all: the story specifically tracks enforcement-capacity buildup and subsequent fragmentation, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats should compute differently. From the Security Council and financial-institution positions, the arrangement is responsible stewardship they built and staff: capacity assessment disciplines intervention discretion and pools burdens no single state would carry alone. From the fragile-state and indebted-state positions, the same structure operates as a hierarchy in which their domestic arrangements are permanently objects of external evaluation, their policy space is contractually mortgaged, and the evaluators are never themselves evaluated. The scholar seat records the third experience: selective application — capacity language deployed where interests align and ignored where they do not. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Permanent Security Council members sit nearest the beneficiary end: they collect discretionary authority and are structurally immune from the evaluations they authorize (arbitrage-grade exit from the rules they write). International financial institutions collect interest and policy leverage while administering the benchmarks — near-beneficiary with administrator duties. Donor governments and indicator publishers collect influence and epistemic authority at lower intensity. Fragile states, indebted states, and intervened populations sit near the target end: they bear the transfers of autonomy, fiscal terms, and physical consequences. The directionality_overrides entry corrects the derivation for the powerless seats: the same structure that extracts from them also delivers protective and financial flows (peacekeeping deployment, humanitarian corridors, concessional credit), so their realized directionality sits slightly inside full target (d=0.82 rather than ~1.0). Emerging alternative patrons are excluded rather than coordinated — their exclusion from standard-setting is part of what the enforcement machinery protects, and their growth is the visible force eroding the arrangement from outside.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how a system of formally equal states responds to territories whose governments cannot protect populations or perform basic functions — remains live: state fragility persists and new cases arrive continuously. No mandatrophy declaration is authored, and the R5 mismatch consumer finds status=live paired with verdict=world_rearranges, so no zombie flag fires. The classification discipline matters here in both directions: the genuine coordination function (pooled crisis response, development finance, humanitarian access that strict inviolability cannot deliver) blocks a pure-extraction reading, while the asymmetric terms, the immunity of the evaluators from their own metrics, and the enforced exclusion of alternative patrons block a pure-coordination reading. What remains is a structure that must be described as doing both at once — coordination and extraction through the same instruments — which is exactly the hybrid case the framework exists to distinguish from its neighbors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the graded_sovereignty reading of the westphalia_sovereignty kernel; which structural features would change if a sibling reading were adopted instead?',
    'Classify the sibling files (absolute_non_intervention, conditional_responsibility) against the same referent arrangement and diff their victim sets, beneficiary sets, and epsilon values.',
    'Under absolute_non_intervention the entire oversight apparatus becomes per se illegitimate and every intervened territory joins the victim set; under conditional_responsibility the continuous tiering collapses to discrete atrocity-threshold events and the standing evaluation bureaucracy loses its reason to exist. The disagreement is located in WHERE the sovereignty boundary sits: nowhere (categorical), at atrocity thresholds, or distributed continuously along a capacity spectrum.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: this story is one of three readings; sibling adoption restructures victim classes and dissolves or preserves the evaluation apparatus.').

omega_variable(
    evaluation_expertise_or_extraction,
    'Does the capacity-evaluation apparatus ground its authority in predictive expertise (indicators that track real governing capacity) or in interest (indicators constructed to license predetermined intervention and lending positions)?',
    'Out-of-sample predictive-validity audits of governance indicators against subsequent state performance; comparison of intervention and lending decisions against indicator readings and against the evaluators'' strategic and commercial interests.',
    'Genuine predictive expertise supports the tangled_rope reading (a real coordination service wrapped around asymmetric terms); constructed metrics shift the classification toward snare with the evaluator class as a concentrated capturer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evaluation_expertise_or_extraction, empirical, 'Whether evaluator authority is competence-grounded or interest-grounded.').

omega_variable(
    tiering_naturalness,
    'Is the observed hierarchy of state capacity a natural distribution that the arrangement merely registers, or a constructed tiering that the arrangement produces and maintains?',
    'Compare capacity trajectories of intensively evaluated states against matched unevaluated comparators across regime episodes (strict non-intervention eras, South-South cooperation windows); test whether evaluation intensity predicts divergence beyond initial conditions.',
    'If constructed, the arrangement''s beneficiaries help manufacture the deficits that justify continued oversight, strengthening the extraction attribution; if natural, the arrangement sits closer to a coordination mechanism over a pre-existing landscape.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tiering_naturalness, empirical, 'Naturalness of the capacity hierarchy the arrangement administers.').

omega_variable(
    weak_state_coalition_efficacy,
    'Can the numerically numerous but individually weak payer states convert coalition potential (G77, African Union, Non-Aligned Movement, BRICS alignment) into effective power over the evaluation regime?',
    'Track outcomes of coordinated weak-state initiatives (debt standstill proposals, indicator-reform demands, Security Council reform pushes) against elite-splitting counteroffers and side-payments.',
    'High coalition efficacy reduces effective suppression on the payer class and supports a coordination-ward reading; persistent elite-splitting confirms that the tiering depends on keeping the many weak seats uncoordinated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weak_state_coalition_efficacy, empirical, 'Coalition-conversion prospects for the weak-state payer class.').

omega_variable(
    oversight_population_net_position,
    'Do civilian populations of overseen territories net-benefit from the intervention and stewardship apparatus (protection, services, concessional finance) enough to sit partially on the receiving side of the structure?',
    'Within-case comparisons of protected versus abandoned crises; welfare and displacement data across mission-presence gradients.',
    'Net benefit pulls the population seat away from full-target directionality and softens the measured asymmetry; net harm confirms these populations as the arrangement''s deepest cost-bearers beneath the formal state-level accounting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oversight_population_net_position, empirical, 'Net position of the population seat under international oversight.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__graded_sovereignty, theater_ratio, 0, 0.18).
narrative_ontology:measurement(west_tr_t5, westphalia_sovereignty__graded_sovereignty, theater_ratio, 5, 0.22).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__graded_sovereignty, theater_ratio, 10, 0.27).
narrative_ontology:measurement(west_tr_t15, westphalia_sovereignty__graded_sovereignty, theater_ratio, 15, 0.31).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__graded_sovereignty, theater_ratio, 20, 0.34).
narrative_ontology:measurement(west_tr_t25, westphalia_sovereignty__graded_sovereignty, theater_ratio, 25, 0.33).
narrative_ontology:measurement(west_tr_t30, westphalia_sovereignty__graded_sovereignty, theater_ratio, 30, 0.32).
narrative_ontology:measurement(west_tr_t35, westphalia_sovereignty__graded_sovereignty, theater_ratio, 35, 0.34).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(west_be_t5, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(west_be_t15, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(west_be_t25, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 25, 0.63).
narrative_ontology:measurement(west_be_t30, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(west_be_t35, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(west_su_t5, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(west_su_t15, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(west_su_t25, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 25, 0.67).
narrative_ontology:measurement(west_su_t30, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(west_su_t35, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 35, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, resource_allocation).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, imf_structural_adjustment_conditionality).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Westphalian sovereignty'. The label conflates three structurally distinct claims with different epsilon values, victim sets, and enforcement profiles: categorical inviolability (absolute_non_intervention), threshold-forfeited inviolability (conditional_responsibility), and continuously graded authority (this file). Per the epsilon-invariance principle these are written as separate stories linked by network edges rather than one story with a measurement parameter. The absolute reading is the historical upstream baseline cited by all parties; this graded reading is upstream of the conditional reading in infrastructure terms — its capacity metrics and 'unable/unwilling' assessments are the evidentiary inputs conditional-responsibility determinations draw on — while remaining logically independent of it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__graded_sovereignty, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
