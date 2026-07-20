% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strict_gatekeeper_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strict_gatekeeper_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: irc_469_material_participation_kernel__strict_gatekeeper_reading
 *   human_readable: IRC Â§469 Strict Material Participation Gatekeeper
 *   domain: tax_law/regulatory_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates the strict gatekeeper reading of IRC Â§469
 *   material participation, which treats the statute as requiring verifiable,
 *   substantial personal labor documented to a high evidentiary standard
 *   before passive real estate losses may be deducted against ordinary
 *   income. It is one of two competing readings of the same statutory kernel;
 *   the sibling strategic shelter reading interprets the same text as
 *   permitting aggressive hour-counting and grouping elections. The strict
 *   reading narrows the qualifying population, imposes high compliance
 *   friction, and results in rare allowance of passive loss deductions for
 *   ordinary income offsets. The IRS enforces this reading through
 *   examination campaigns, regulations, and litigation, while taxpayers bear
 *   the burden of proving participation through contemporaneous records.
 *
 * KEY AGENTS:
 *   - IRS LB&I Division: agenda_setter (institutional/constrained) â administers and enforces the strict material participation tests
 *   - US Treasury: beneficiary (institutional/analytical) â captures preserved revenue from denied deductions
 *   - Tax advisory industry: beneficiary (organized/mobile) â profits from compliance complexity and documentation burden
 *   - Sophisticated real estate investors: payer (powerful/constrained) â bear denied deductions but can restructure partially around the rules
 *   - Small rental property owners: payer (moderate/trapped) â lack resources to document or restructure, accumulating suspended losses
 *   - Congressional tax writers: observer (institutional/analytical) â could amend the statute but have not
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.74).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.78).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC Â§469 Strict Material Participation Gatekeeper").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax_law/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'efae2e7b-1f1a-4131-9ad9-63d4e163ccd8').
narrative_ontology:cs_kernel_codification('efae2e7b-1f1a-4131-9ad9-63d4e163ccd8', fixed_text).
narrative_ontology:cs_authority_grounding('efae2e7b-1f1a-4131-9ad9-63d4e163ccd8', lineage).
narrative_ontology:cs_interpretation_layer_present('efae2e7b-1f1a-4131-9ad9-63d4e163ccd8').
narrative_ontology:cs_reading_relation('efae2e7b-1f1a-4131-9ad9-63d4e163ccd8', irc_469_material_participation_kernel__strategic_shelter_reading, influences).
narrative_ontology:cs_axiom('efae2e7b-1f1a-4131-9ad9-63d4e163ccd8', foundational, substantial_verifiable_labor_required).
narrative_ontology:cs_axiom_status(substantial_verifiable_labor_required, holdable).
narrative_ontology:cs_axiom_grounding('efae2e7b-1f1a-4131-9ad9-63d4e163ccd8', substantial_verifiable_labor_required, conventional).
narrative_ontology:cs_axiom('efae2e7b-1f1a-4131-9ad9-63d4e163ccd8', secondary, anti_abuse_revenue_integrity).
narrative_ontology:cs_axiom_status(anti_abuse_revenue_integrity, holdable).
narrative_ontology:cs_axiom_grounding('efae2e7b-1f1a-4131-9ad9-63d4e163ccd8', anti_abuse_revenue_integrity, instrumental).
narrative_ontology:cs_reference_frame('efae2e7b-1f1a-4131-9ad9-63d4e163ccd8', statutory_anti_abuse_reference).
narrative_ontology:cs_drift_state('efae2e7b-1f1a-4131-9ad9-63d4e163ccd8', contemporary_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('efae2e7b-1f1a-4131-9ad9-63d4e163ccd8', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, us_treasury).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_advisory_industry).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, sophisticated_real_estate_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_rental_property_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the material participation tests through examination campaigns, regulations, and litigation positions. Requires contemporaneous time logs and substantial personal participation evidence to unlock passive loss deductions. Can adjust enforcement emphasis but is bound by the Internal Revenue Code, administrative procedure, and judicial review.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_lb_i, agenda_setter,
    institutional, generational, constrained, national).

% Collects additional income tax revenue preserved when the strict reading disallows passive real estate losses against ordinary income. The revenue accrues to the general fund; the strict gatekeeper function protects the tax base from shelter-driven erosion.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, us_treasury, beneficiary,
    institutional, generational, analytical, national).

% Designs entity structures, maintains contemporaneous time-tracking systems, and prepares documentation packages to satisfy strict material participation tests. The high documentation bar and interpretive complexity create specialized, recurring demand for tax compliance and planning services.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_advisory_industry, beneficiary,
    organized, biographical, mobile, national).

% Own substantial real estate portfolios and report passive activity losses. Must document 500-plus hours or satisfy other tests to deduct losses against ordinary income. Can afford restructuring and professional record-keeping but remain constrained by the statutory threshold; failure to document means deferred or lost deductions.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, sophisticated_real_estate_investors, payer,
    powerful, biographical, constrained, national).

% Own one or a few rental properties and often perform management labor informally. Lack resources to generate contemporaneous time logs or restructure into compliant entities. Their passive losses accumulate as suspended carryforwards with indefinite deferral, effectively trapping capital in a tax-disadvantaged posture.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_rental_property_owners, payer,
    moderate, biographical, trapped, national).

% Have the authority to amend IRC Â§469 to clarify or loosen material participation standards but have not intervened. Observe the interpretive contest between strict IRS enforcement and taxpayer attempts to navigate the permissive alternative reading.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, congressional_tax_writers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strict_gatekeeper_reading, us_treasury).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents high-income taxpayers from using passive real estate losses to shelter ordinary wage and portfolio income, preserving the progressivity of the income tax base and preventing synthetic tax shelter erosion of federal revenue.
% TRANSFER_FUNCTION: Moves tax revenue from real estate investors who cannot satisfy strict documentation and hour thresholds to the federal Treasury; moves compliance labor and advisory fees from investors to the tax preparation and planning industry.
% ABSENT_VOICES: Small rental property owners without formal bookkeeping, immigrant landlords, and part-time real estate operators who lack resources to generate contemporaneous time logs are effectively excluded from the administrative hearing and legislative process that shapes material participation guidance; their inability to document is treated as non-participation rather than evidence of a bar set too high.
% DISAPPEARANCE_RATIONALE: If the strict gatekeeper reading vanished overnight, millions of real estate investors would refile to claim passive loss deductions against ordinary income, the tax advisory industry's documentation-practice revenue would contract, and the Treasury would face a revenue gap requiring statutory amendment or rate adjustment.
% FOUNDING_PROBLEM: The Tax Reform Act of 1986 sought to stop wealthy taxpayers from generating paper real estate losses through limited partnerships and similar vehicles to offset salaries and portfolio income, draining the tax base.
% FOUNDING_PROBLEM_CORROBORATION: Congressional records from the 1986 Act cite revenue protection and anti-abuse intent; however, contemporary tax scholars and the Government Accountability Office note that the strict reading now captures many non-abusive, middle-income rental operators who were not the original target, while sophisticated taxpayers continue to navigate around the rules through entity structuring. The corroboration is split between original legislative history and current empirical critiques from outside the benefiting parties.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.74, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.74) is high because the strict reading denies valuable tax attributes to a broad swath of investors and imposes substantial compliance costs that decouple from the underlying economic activity. Suppression (0.78) is higher still because the constraint's persistence depends on active IRS examination, regulatory bar-setting, and the legal impossibility of unilaterally opting out of the tax recognition system. Theater ratio (0.45) reflects that a growing share of enforcement and planning activity is devoted to documentation ritual rather than substantive abuse prevention. The measurement series share a single time grid, showing monotonic increases as the strict reading matured from statutory enactment through regulatory hardening and contemporary campaign enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (IRS) experiences the constraint as necessary coordination against tax base erosion, while the payer seats experience it as enforced extraction of tax attributes and compliance labor. The beneficiary seat (Treasury) sees revenue preservation; the tax advisory industry sees a service market created by friction. Small property owners and sophisticated investors diverge even within the payer class: the former are trapped by documentation asymmetry, while the latter are merely constrained by restructuring costs. The engine will compute different per-seat classifications from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Treasury and tax advisory industry are structural beneficiaries: the Treasury collects revenue preserved by disallowance (low d, subsidy-like damping), and the advisory industry captures fees from compliance complexity (low d). Real estate investors are structural targets: they bear the direct cost of denied deductions and documentation burdens (high d). The small owner subgroup sits nearer the full-target end due to trapped exit options, while sophisticated investors are slightly less targeted due to constrained but available restructuring. The IRS as agenda-setter sits near the symmetric middle: it does not personally collect the revenue but enforces the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling the constraint as pure extraction (snare) because there is a genuine coordination function: preventing wealthy taxpayers from converting ordinary income into sheltered passive losses. It also prevents mislabeling as pure coordination (rope) because the asymmetric extraction is real and substantial â the documentation bar is set high enough to capture non-abusive small operators, and the advisory industry profits from the friction. The temporal measurements show extraction accumulation over the interval, consistent with a coordination mechanism that has been progressively weaponized for revenue preservation beyond its original anti-abuse scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irc_469_reading_incommensurability,
    'Does the strict gatekeeper reading of material participation represent the only defensible statutory construction, or is the strategic shelter reading equally grounded in the IRC Â§469 text?',
    'Definitive judicial resolution by the Supreme Court or Treasury regulations explicitly endorsing one reading as the sole permissible construction of the statutory kernel.',
    'If the strategic shelter reading is textually defensible, the strict reading''s high extraction is a policy choice masquerading as statutory fidelity; if the strict reading is the only textual reading, the extraction is inherent to the enacted law and the kernel is not underdetermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irc_469_reading_incommensurability, conceptual, 'Whether the strict and permissive readings are mutually exclusive constructions or one is textually mandated').

omega_variable(
    small_investor_documentary_asymmetry,
    'Does the strict documentation bar for material participation disproportionately exclude small rental property owners while permitting sophisticated investors to engineer compliance through entity grouping and professional record-keeping?',
    'Empirical study of audit outcomes and material participation test pass rates stratified by taxpayer income, entity complexity, and representation status.',
    'If yes, the constraint''s effective extraction is regressive within the payer class, intensifying the burden on lower-power payers despite the same nominal statutory rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_investor_documentary_asymmetry, empirical, 'Whether compliance asymmetry creates regressive extraction across payer power levels').

omega_variable(
    compliance_cost_vs_revenue_preservation,
    'Does the revenue preserved by the strict reading exceed the aggregate compliance costs imposed on taxpayers and the administrative costs of enforcement?',
    'Comprehensive cost-benefit analysis by the Joint Committee on Taxation or GAO incorporating deadweight loss, taxpayer time costs, and examination outlays.',
    'If compliance and enforcement costs exceed revenue preserved, the constraint operates as net social cost extraction rather than efficient coordination; if revenue dominates, the coordination function is proportionate to its price.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_cost_vs_revenue_preservation, empirical, 'Whether the constraint''s social cost exceeds its revenue benefit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(irc__tr_t8, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(irc__tr_t15, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(irc__tr_t23, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 23, 0.38).
narrative_ontology:measurement(irc__tr_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(irc__tr_t38, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 38, 0.45).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(irc__be_t8, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(irc__be_t15, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(irc__be_t23, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 23, 0.66).
narrative_ontology:measurement(irc__be_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(irc__be_t38, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 38, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(irc__su_t8, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(irc__su_t15, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(irc__su_t23, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 23, 0.7).
narrative_ontology:measurement(irc__su_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(irc__su_t38, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 38, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel__strategic_shelter_reading).

% DUAL FORMULATION NOTE:
% This constraint and its sibling are competing readings of the same IRC Â§469 statutory kernel. They are linked as a constraint family because the same natural-language label (material participation) conflates two structurally distinct interpretive regimes with different epsilon values, victim sets, and coordination functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
