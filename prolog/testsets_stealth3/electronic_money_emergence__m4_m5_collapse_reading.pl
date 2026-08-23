% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__m4_m5_collapse_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: electronic_money_emergence__m4_m5_collapse_reading
 *   human_readable: M4/M5 Boundary as Retroactive Constructor of Electronic Money (Collapse Reading)
 *   domain: economic history / monetary theory / technology studies
 *
 * SUMMARY:
 *   The standing arrangement under contest is the official M4/M5-style
 *   statistical partition maintained by central bank statistics departments —
 *   the boundary-drawing practice that assigns payment instruments to
 *   monetary bins and publishes the resulting series. This story instantiates
 *   the m4_m5_collapse_reading of the kernel electronic_money_emergence: on
 *   this reading, the category 'electronic money' was constituted
 *   retroactively by the classification act itself, so there is no genuine
 *   emergence event to date; the 'emergence' the literature dates is an
 *   artifact of when the bins changed. Epsilon's referent is fixed
 *   accordingly: the standing classification arrangement, assessed by this
 *   reading's own lights — a moderately burdensome, diffusely costly,
 *   uncaptured maintenance practice. Per the epsilon-invariance principle
 *   this is one member of a constraint family: became_thinkable_reading
 *   (emergence at conceptual thinkability) and first_held_reading (emergence
 *   at first institutional dematerialized holding) are separate constraints
 *   with their own epsilon, beneficiary structures, and classifications,
 *   linked via network.affects_constraints. A reflexive note: the
 *   emergence-dating literature the kernel contests is itself downstream of
 *   the artifact this reading identifies — the sibling readings presuppose an
 *   event this reading denies. Claim and metrics are authored independently:
 *   the claimed type is stated from structural analysis of the arrangement;
 *   the metrics describe its observed operation; the engine computes per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - - central_bank_statistics_departments: Agenda-setter (institutional / identity_locked) — administers the classification and guards series continuity; fused with its own output
 *   - - reporting_credit_institutions: Primary bearer of recurring costs (organized / constrained) — files returns under the bins, voice without veto
 *   - - electronic_money_issuers: Dual-positioned seat (moderate / constrained) — bears designation costs, draws legitimation from the same designation
 *   - - monetary_data_vendors: Incidental gainer (moderate / arbitrage) — repackages the series commercially, uncommitted to its survival
 *   - - academic_monetary_economists: Secondary bearer (moderate / constrained) — inherits the artifact in empirical work, anchored by free authoritative data
 *   - - payment_network_operators: Excluded voice (powerful / arbitrage) — partitioned without representation, exited into proprietary data
 *   - - central_bank_methodology_committees: Analytical observer (institutional / analytical) — annotates the drift, lacks mandate to retire the framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.36).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.22).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, piton).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "M4/M5 Boundary as Retroactive Constructor of Electronic Money (Collapse Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic history / monetary theory / technology studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, '0f7728bf-bece-411a-980a-9998dec9a7c9').
narrative_ontology:cs_kernel_codification('0f7728bf-bece-411a-980a-9998dec9a7c9', formalized).
narrative_ontology:cs_authority_grounding('0f7728bf-bece-411a-980a-9998dec9a7c9', lineage).
narrative_ontology:cs_interpretation_layer_present('0f7728bf-bece-411a-980a-9998dec9a7c9').
narrative_ontology:cs_reading_relation('0f7728bf-bece-411a-980a-9998dec9a7c9', electronic_money_emergence__became_thinkable_reading, forecloses).
narrative_ontology:cs_reading_relation('0f7728bf-bece-411a-980a-9998dec9a7c9', electronic_money_emergence__first_held_reading, forecloses).
narrative_ontology:cs_axiom('0f7728bf-bece-411a-980a-9998dec9a7c9', foundational, statistical_categories_constitute_not_describe).
narrative_ontology:cs_axiom_status(statistical_categories_constitute_not_describe, holdable).
narrative_ontology:cs_axiom_grounding('0f7728bf-bece-411a-980a-9998dec9a7c9', statistical_categories_constitute_not_describe, empirically_contingent).
narrative_ontology:cs_axiom('0f7728bf-bece-411a-980a-9998dec9a7c9', foundational, emergence_dating_is_measurement_artifact).
narrative_ontology:cs_axiom_status(emergence_dating_is_measurement_artifact, holdable).
narrative_ontology:cs_axiom_grounding('0f7728bf-bece-411a-980a-9998dec9a7c9', emergence_dating_is_measurement_artifact, empirically_contingent).
narrative_ontology:cs_reference_frame('0f7728bf-bece-411a-980a-9998dec9a7c9', constitutive_measurement_convention).
narrative_ontology:cs_drift_state('0f7728bf-bece-411a-980a-9998dec9a7c9', contemporary_cbdc_stablecoin_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0f7728bf-bece-411a-980a-9998dec9a7c9', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistics_departments).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, monetary_data_vendors).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_issuers).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, reporting_credit_institutions).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, academic_monetary_economists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, academic_monetary_economists).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_issuers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compile and publish the official monetary aggregate series, assign newly appearing instruments to bins through technical notes, and defend the continuity of long historical series. Departmental standing and staff careers are bound to the authority of the published series; retiring a flagship aggregate is treated internally as institutional failure, and the rare discontinuation precedents are remembered as reputational damage episodes rather than liberations. Exit would mean dismantling the department's own centerpiece and implicitly re-labeling decades of its output.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistics_departments, agenda_setter,
    institutional, generational, identity_locked, continental).

% File periodic statistical returns classifying their liabilities and payment products under the official bins. Each firm's burden is small but recurring: classification staff time and audit exposure for mis-slotting instruments. Consultation procedures let banking associations comment on bin definitions, providing voice without veto. Exit is unavailable short of leaving the jurisdictions whose returns regimes cover them.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, reporting_credit_institutions, payer,
    organized, biographical, constrained, continental).

% Hold prepaid and wallet balances that the statistical framework slices into a designated money category. They carry reporting obligations and examination costs attached to that designation, and simultaneously draw standing from it: being counted as money eases partnership negotiations with banks and signals seriousness to regulators and investors. Leaving the designation would mean leaving the business.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_issuers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_issuers, beneficiary).

% License and repackage the official series into terminals and databases sold to banks, funds, and researchers. Product lines assume the series continue indefinitely; if publication stopped they would re-tool within a quarter, and several already sell alternative payment-flow datasets alongside the official ones.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_data_vendors, beneficiary,
    moderate, biographical, arbitrage, global).

% Use the official aggregates as ready-made, citable measures in empirical work. The data are free and authoritative, which anchors publication, yet the inherited category shapes findings: studies of when electronic money began take the statistical bins' birthdates as events in the world. Alternative constructions exist but carry citation friction and reviewer suspicion, and parts of the field built careers on aggregate-based modeling.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, academic_monetary_economists, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__m4_m5_collapse_reading, academic_monetary_economists, beneficiary).

% Operate card, wallet, and instant-payment rails whose products the statistical framework partitions without their participation. They were not represented when bin definitions were drawn and would argue for instrument-neutral flow measurement. Their response has been to build proprietary datasets outside the official apparatus rather than seek seats in the classification bodies.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, payment_network_operators, excluded,
    powerful, biographical, arbitrage, global).

% Convene periodically to review monetary statistics methods across jurisdictions, documenting instrument proliferation and recommending supplemental dashboards. They can annotate the framework but hold no mandate to retire or redraw the aggregates themselves.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_bank_methodology_committees, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__m4_m5_collapse_reading, diffuse).
narrative_ontology:fixing_cost_class(electronic_money_emergence__m4_m5_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one shared, continuous aggregation scheme so central banks, markets, and researchers can compare monetary stocks across time and institutions; it solved the once-real problem of incompatible national reporting formats and fast-moving instrument churn by assigning every liability to a fixed bin.
% TRANSFER_FUNCTION: Moves recurring classification-and-reporting labor from credit institutions and e-money issuers into the statistical apparatus, and moves epistemic authority — citation priority and presumptive reliability — to whatever is published under the official bins.
% ABSENT_VOICES: Endogenous-money scholars and the engineers who design payment instruments were absent from the working groups that drew the boundaries; both would contest that the bins track anything in the payments substrate. They sit outside the consultation circuit, which runs through the supervised reporting institutions.
% DISAPPEARANCE_RATIONALE: Material payments would not pause, but the epistemic layer would rearrange: long-run monetary comparisons would lose their spine mid-series, data vendors would scramble to stitch continuity, and literatures built on the bins would face re-basing. The dependence is real but confined to the measurement layer — an arrangement whose remaining grip is archival rather than functional.
% FOUNDING_PROBLEM: In the monetary-targeting decades, financial innovation migrated liabilities across liquidity lines faster than narrow aggregates could track them; statistical authorities extended the aggregate ladder toward M4, M5, and equivalents to keep money-growth targets operable and to catch new instruments — including the first stored-value and network-money experiments — inside the controlled measure.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: central-bank strategy reviews that formally abandoned monetary targets (the Bank of England in the 1980s, the Bundesbank tradition's late-1990s wind-down, the ECB's 2003 downgrade of its M3 reference value) and the policymaking memoirs surrounding those decisions all attest that the operative problem — hitting money-growth targets — ceased to organize policy. No party outside the statistical establishment argues the targeting problem remains live.
narrative_ontology:disappearance_verdict(electronic_money_emergence__m4_m5_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__m4_m5_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__m4_m5_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(electronic_money_emergence__m4_m5_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__m4_m5_collapse_reading, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).
:- end_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and diffuse (0.36): recurring reporting labor and examination costs on classified institutions, plus epistemic misdirection for downstream users, with no seat collecting concentrated rents — the receipt surface affirmatively checks to diffuse. Suppression is low (0.22) and is authored as the raw structural property, unscaled by power or scope; its composition is roughly half structural (mandated returns, audit exposure) and half internalized (citation norms and reviewer expectations that bind researchers to the official series even where alternatives exist). Theater ratio is high (0.62): annual publications, harmonization exercises, and methodological notes continue at full liturgical tempo decades after the targeting function they served was abandoned. Accessibility collapse is moderate-low (0.40): Divisia indices, proprietary payment-flow data, and ad hoc aggregates are constructible and partially exist, but official status imposes a strong default. Resistance is moderate (0.35): Goodhart-law critique, endogenous-money objections, and the discontinuation precedents surface episodically without sustained campaign. The measurement series run on one shared seven-point grid for both tracked metrics, so every metric is authored at every examined time point; the theater trajectory climbs monotonically, tracking the abandonment of monetary targeting mid-interval, while extractiveness creeps upward in small steps as directive-era expansions added reporting populations. No suppression_requirement series is authored: the enforcement picture is static (returns regimes neither hardened nor decayed materially over the interval), so the scalar in base_properties carries it, per the static-enforcement rule. Dynamics are monotone, not cyclical; the end-state scalar values correspond to the final grid points. No metric was tuned toward a predicted verdict — the claimed type and the metrics were authored from independent assessments.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the keeper's chair (statistics departments), the arrangement is identity-bearing stewardship: retiring the aggregates would negate the department's own lifework, so the seat experiences continuation as duty rather than choice. From the payer chairs, the same structure is diffuse burden without veto — real costs, small per firm, unchallengeable through the consultation channel. From the vendor chair, it is benign standardization: a product feed, replaceable. From the researcher chair, it is both resource and distortion — free authoritative data that quietly imports a constitutive fiction into findings. Same-level lateral dynamics differentiate the three moderate seats despite identical power atoms: arbitrage-grade exit (vendors), citation-and-career friction (academics), and business-tied designation (issuers) produce divergent experiences from identical global standing — the differentiation is entirely constraint-specific exit structure, not power. Inter-institutionally, statistics departments and methodology committees inhabit adjacent institutional niches with different mandates: one maintains, the other may only annotate; the observer seat documents the drift it cannot arrest.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low-directionality end: monetary_data_vendors sit nearest the beneficiary pole, reinforced by arbitrage-grade exit. central_bank_statistics_departments derive low d from their beneficiary entry, but their administrator workload offsets the mandate-stability gain, parking them near symmetry — the characteristic near-symmetric administrator position where the cost of fixing exceeds what they bear. electronic_money_issuers straddle the axis (payer with beneficiary secondary): designation costs push toward target, legitimation pulls back, netting approximately symmetric — this is the coarsest point in the derivation, flagged in the issuer omega. Victims drive the high-d end: reporting_credit_institutions are pinned near full-target by constrained exit despite organized power; academic_monetary_economists sit moderately high, their mild citation-and-career lock doing the work exit options cannot. No directionality overrides are authored: beneficiary/victim declarations plus exit atoms suffice, and the known coarseness (the issuer seat) is documented as an omega rather than patched with an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping monetary-growth targets operable as innovation churned through liquidity categories — is dead, attested from outside the benefiting parties by the strategy reviews that abandoned targeting. Yet the arrangement persists on series-continuity ritual: the founding_problem_status (dead) crossed with disappearance_verdict (world_rearranges) produces the mismatch signature, cross-checked against the elevated theater ratio and the empty receipt surface. Classification discipline prevents two mislabels: not a snare, because no seat captures the gains (receipt affirmatively diffuse; the vendor's commerce is downstream sale of output, not receipt of the extracted burden); not a rope, because the coordination function has atrophied below live-use floors — what remains coordinates an archive, not a policy. Not a scaffold: no sunset machinery exists anywhere in the framework, and nobody is planning a transition. Mandatrophy is declared resolved: the mandate outlived its function, and what continues is maintenance of the maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (m4_m5_collapse_reading) of the kernel electronic_money_emergence; what would the corpus look like if a sibling reading (became_thinkable_reading, first_held_reading) were instantiated instead?',
    'Generate the sibling files and compare: each sibling relocates epsilon onto a real datable event (a thinkability threshold, a first institutional dematerialized holding), changing the beneficiary/victim structure and the computed classification.',
    'Sibling adoption replaces the no-event claim with event-anchored constraints whose epsilon attaches to institutional milestones; the piton-shaped artifact reading would coexist as a critique-layer constraint rather than the primary dating account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one reading of a contested kernel, routed here per the committer-frame rules.').

omega_variable(
    category_ontology_location,
    'Where is the disagreement located: do statistical categories DESCRIBE pre-existing monetary kinds, or CONSTITUTE them retroactively such that apparent emergence dates are artifacts of when the bins were drawn?',
    'Archival chronology: compare the documentary record of statistical working-group decisions (when instruments were assigned to bins) against independent evidence of instrument usage and self-description preceding classification. If instruments operated and were understood as money before the bins existed, the collapse reading weakens toward the first-held sibling.',
    'Resolution determines whether this constraint''s epsilon attaches to a classificatory practice (current file) or dissolves into the siblings'' event-anchored constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_ontology_location, conceptual, 'The descriptive-vs-constitutive dispute is the precise structural element on which the three readings diverge.').

omega_variable(
    residual_function_ambiguity,
    'Does broad-money aggregation retain any live analytic function (residual citations of M3-style reference values in policy analysis, inflation-dashboard work), or is remaining use purely ceremonial?',
    'Usage audit: trace citations of the upper aggregates in policy documents, forecasts, and market commentary over the last decade; distinguish load-bearing use from ritual reference.',
    'A live residual function would pull the arrangement toward a degraded-but-working coordination hybrid; confirmed ceremonial-only use consolidates the atrophied-function reading and the theater-heavy profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_function_ambiguity, empirical, 'Whether the founding function is fully dead or survives in vestigial analytic form.').

omega_variable(
    issuer_net_directionality,
    'Are electronic money issuers net bearers of the classification (compliance and examination costs dominate) or net gainers (official recognition as money dominates, easing partnerships and licensing)?',
    'Survey of issuers'' compliance cost accounts against measurable legitimation premiums (partnership terms, valuation effects of being counted in official money statistics).',
    'Shifts the derived directionality of the moderate seats around symmetry; a decisively net-beneficiary finding would weaken the asymmetric-cost structure this story authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(issuer_net_directionality, empirical, 'Net position of the dual-positioned issuer seat drives the coarsest part of the directionality derivation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t0, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(elec_tr_t6, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(elec_tr_t12, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(elec_tr_t18, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 18, 0.46).
narrative_ontology:measurement(elec_tr_t24, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 24, 0.54).
narrative_ontology:measurement(elec_tr_t30, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 30, 0.59).
narrative_ontology:measurement(elec_tr_t36, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 36, 0.62).

% Extraction over time
narrative_ontology:measurement(elec_be_t0, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 0, 0.27).
narrative_ontology:measurement(elec_be_t6, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 6, 0.29).
narrative_ontology:measurement(elec_be_t12, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 12, 0.31).
narrative_ontology:measurement(elec_be_t18, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 18, 0.32).
narrative_ontology:measurement(elec_be_t24, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 24, 0.34).
narrative_ontology:measurement(elec_be_t30, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(elec_be_t36, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 36, 0.36).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(electronic_money_emergence__m4_m5_collapse_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__m4_m5_collapse_reading, information_standard).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, first_held_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'emergence of electronic money' conflates three structurally distinct claims — a concept-history claim (became_thinkable), an institutional-event claim (first_held), and a measurement-constitution claim (this file). Each carries its own epsilon, victim set, and classification; the upstream dating literatures the first two readings instantiate are precisely what this reading identifies as downstream of the classificatory artifact. Linked bidirectionally through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
