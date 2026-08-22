% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__legalization_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State Regulated Drug Commerce Regime (Legalization Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   A state operates drug markets as lawful commerce under a licensing,
 *   testing, and taxation regime: producers and retailers operate by permit,
 *   products carry testing and labeling requirements, sales are age-gated and
 *   quantity-capped, and an enforcement arm prosecutes supply outside the
 *   permit system. This story instantiates ONE reading of the
 *   substance_control_authority kernel — the legalization_reading, under
 *   which the state's authority runs through market regulation rather than
 *   criminal sanction or purely clinical management. The epsilon referent is
 *   the standing legalized-and-regulated arrangement itself, assessed as it
 *   operates (including its taxes, licensing barriers, and enforcement
 *   against unlicensed supply), not the arrangement any sibling reading would
 *   install. Sibling readings are separate constraints with their own victim
 *   sets and epsilon values; see commentary.kernel_context and
 *   network.dual_formulation_note. KEY AGENTS (by structural relationship): -
 *   state_regulatory_agency: agenda setter (institutional/constrained) —
 *   administers licensing, testing standards, and enforcement -
 *   state_treasury: primary beneficiary (institutional/constrained) —
 *   collects excise and fee receipts - licensed_producers: beneficiary
 *   (powerful/mobile) — protected legal market access - adult_consumers:
 *   beneficiary/payer (moderate/constrained) — legal access against taxed
 *   prices - unlicensed_suppliers: primary target (organized/trapped) — bears
 *   enforcement, forfeiture, prosecution - users_exceeding_access_limits:
 *   secondary target (powerless/trapped) — penalized beyond caps -
 *   public_health_systems: secondary beneficiary (institutional/constrained)
 *   — earmarked funding, incident data - small_market_entrants: excluded
 *   (moderate/constrained) — priced out of licensure, absent from
 *   rule-setting - civil_liberties_monitors: analytical observer
 *   (organized/analytical) - treaty_oversight_bodies: inter-institutional
 *   observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.62).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.55).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Regulated Drug Commerce Regime (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, '1e8e7026-b587-44cf-a65b-b76fece8f9cd').
narrative_ontology:cs_kernel_codification('1e8e7026-b587-44cf-a65b-b76fece8f9cd', formalized).
narrative_ontology:cs_authority_grounding('1e8e7026-b587-44cf-a65b-b76fece8f9cd', distributed).
narrative_ontology:cs_reading_relation('1e8e7026-b587-44cf-a65b-b76fece8f9cd', substance_control_authority__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e8e7026-b587-44cf-a65b-b76fece8f9cd', substance_control_authority__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('1e8e7026-b587-44cf-a65b-b76fece8f9cd', foundational, regulated_lawful_commerce_superior_to_criminalization).
narrative_ontology:cs_axiom_status(regulated_lawful_commerce_superior_to_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('1e8e7026-b587-44cf-a65b-b76fece8f9cd', regulated_lawful_commerce_superior_to_criminalization, instrumental).
narrative_ontology:cs_axiom('1e8e7026-b587-44cf-a65b-b76fece8f9cd', secondary, qualified_adult_access_entitlement).
narrative_ontology:cs_axiom_status(qualified_adult_access_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('1e8e7026-b587-44cf-a65b-b76fece8f9cd', qualified_adult_access_entitlement, deontological).
narrative_ontology:cs_reference_frame('1e8e7026-b587-44cf-a65b-b76fece8f9cd', drugs_as_regulable_commerce).
narrative_ontology:cs_drift_state('1e8e7026-b587-44cf-a65b-b76fece8f9cd', post_implementation_review, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1e8e7026-b587-44cf-a65b-b76fece8f9cd', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, licensed_producers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, state_treasury).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, public_health_systems).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, adult_consumers).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, unlicensed_suppliers).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, users_exceeding_access_limits).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, adult_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and revises the licensing rules, product-testing standards, purchase limits, and inspection schedules; runs the enforcement arm that raids unlicensed operations and penalizes over-limit sales. Funded through licensing fees and legislative appropriations. Cannot drop the mandate or rewrite its own authority without new legislation.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_regulatory_agency, agenda_setter,
    institutional, generational, constrained, national).

% Receives excise tax receipts and licensing fees deposited into designated funds. After the first budgets are built on these revenues, the streams become line items that programs depend on; eliminating them would require replacing general-fund money.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_treasury, beneficiary,
    institutional, generational, constrained, national).

% Hold permits to cultivate, process, and sell; gain court-enforceable protection against unlicensed competitors and access to banking and contracts unavailable to unlicensed operators. Bear testing, tracking, and facility-compliance costs. Can relocate operations across jurisdictions or convert facilities to other uses.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, licensed_producers, beneficiary,
    powerful, biographical, mobile, national).

% Buy tested, labeled products at licensed shops within possession and purchase caps. Pay excise taxes and compliance-inflated retail prices. May shift some purchases to unlicensed sellers at lower prices and some legal risk.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, adult_consumers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__legalization_reading, adult_consumers, payer).

% Grow or sell outside the permit system. Subject to raid, asset forfeiture, and prosecution; prior convictions and capital requirements commonly bar them from licensure. Their equipment, product, and customer relationships are tied up in the unlicensed trade.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, unlicensed_suppliers, payer,
    organized, biographical, trapped, regional).

% Consume in amounts or settings beyond purchase caps and public-use rules; face civil fines and, for repeat distribution-scale possession, charges. Dependence narrows their practical alternatives regardless of what the rules allow.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, users_exceeding_access_limits, payer,
    powerless, immediate, trapped, local).

% Receive earmarked tax allocations for treatment and prevention, and gain incident reports tied to tested products. Carry the treatment caseload that follows whatever use levels the market settles at.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_systems, beneficiary,
    institutional, generational, constrained, national).

% Would open cultivation or retail operations but face application fees, capital requirements, and license caps that price them out; rule-setting consultations are attended by established license holders.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, small_market_entrants, excluded,
    moderate, biographical, constrained, national).

% Track the surveillance infrastructure the commerce rules ride on — purchase registries, seed-to-sale tracking, advertising limits — and publish assessments of penalty structures and data-sharing practices.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, civil_liberties_monitors, observer,
    organized, generational, analytical, national).

% Monitor whether the regulated-commerce scheme stays within international drug-control conventions that presume restriction; issue formal objections and request explanations when signatory states widen lawful supply.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, treaty_oversight_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__legalization_reading, state_treasury).
narrative_ontology:fixing_cost_class(substance_control_authority__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves product-safety and market-order problems that unregulated commerce cannot: standardized contaminant testing and labeling, age verification at point of sale, traceable supply chains enabling recalls, and a single collection point for excise revenue.
% TRANSFER_FUNCTION: Moves money from consumers to the treasury via excise taxes and licensing fees, and market share from unlicensed sellers to licensed firms; moves enforcement costs onto unlicensed operators through forfeiture and prosecution.
% ABSENT_VOICES: Unlicensed suppliers and purchasers beyond legal caps have no seat in rule-setting; people incarcerated under the prior restrictive regime rarely participate in designing the successor arrangement; advocates of unrestricted access are consulted marginally. Their objections surface mainly through litigation and elections.
% DISAPPEARANCE_RATIONALE: Drug commerce would continue overnight but reorganize entirely: quality testing and recalls would stop, tax streams funding treatment and inspection would end, licensed firms would lose enforceable market protection, and the enforcement apparatus would lose its object — every seat's arrangements depend on the scheme persisting in something like its current form.
% FOUNDING_PROBLEM: Unregulated drug commerce produced adulterated and mislabeled products, poisonings from unknown potency, and violent disputes over untaxed market share; the arrangement was built to move that commerce into inspectable, taxable, age-gated channels.
% FOUNDING_PROBLEM_CORROBORATION: Public-health surveillance outside the benefiting parties corroborates the problem: contaminant recalls and poisoning reports from unregulated supply persist wherever unlicensed channels operate, and independent market research documents the quality variance. Prohibition advocates dispute whether this arrangement solves the problem, but they corroborate that the underlying hazard exists.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__legalization_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at 0.62 at interval end: excise schedules ratchet upward once budgets depend on them, licensing fees rise with demand for permits, and compliance costs function as entry barriers — while the regime also delivers real testing, recall, and age-gating services. Suppression (0.55) is a raw structural property, unscaled by power or scope: it reflects the standing enforcement machinery aimed at unlicensed supply and over-limit possession, not participant preference; only extractiveness is scaled by directionality and scope downstream. Theater (0.30) tracks the growth of compliance paperwork relative to core quality functions. Accessibility_collapse (0.45): alternatives do not fully collapse — unlicensed channels and home cultivation persist — but the licensed channel captures majority share. Resistance (0.45): illicit operators resist enforcement, consumers resist via tax-driven arbitrage back to street supply, and civil-liberties monitors contest the surveillance layer. All three tracked metrics run on one shared seven-point grid (T=0..12) so temporal reads sample complete rows; the rising suppression_requirement series traces enforcement hardening against the persisting illicit channel, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   From the treasury and regulatory-agency seats the scheme computes as functioning coordination they administer and fund programs from; from the unlicensed-supplier seat the same statutes compute as pure enforcement with no coordination benefit reaching them; the adult-consumer seat straddles — tested products and legal access against taxed prices and purchase caps. The engine derives these per-seat classifications from the declared roles, power atoms, and exit options; the divergence between the administrator's view and the enforced-against view is the measurement, not an inconsistency to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (licensed_producers, state_treasury, public_health_systems, adult_consumers) place those seats near the subsidized end; victim declarations (unlicensed_suppliers, users_exceeding_access_limits) place those seats near the full-target end, amplified by trapped exit. adult_consumers carry a secondary payer role — their derived d sits nearer symmetric than a pure beneficiary's, reflecting taxed prices. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already differentiate every seat the derivation needs to distinguish; the one borderline case (public_health_systems, which also carries the treatment load) stays close enough to its derived value that an override would add noise rather than signal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — hazardous unregulated supply — remains live, so no mandatrophy declaration is authored and the R5 mismatch consumer finds status=live paired with verdict=world_rearranges: no zombie flag. The tangled-rope claim keeps both halves visible: reading the scheme as pure rope would erase the enforcement victims and the revenue-maximizing tax drift visible in the measurement series; reading it as pure snare would erase the load-bearing testing, recall, and age-gating functions that unregulated commerce lacks. The classification prevents both mislabels.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the substance_control_authority kernel — does classifying it independently of the prohibition and harm_reduction readings misrepresent the contested structure?',
    'Cross-reading comparison: compile all three sibling stories and compare computed types, victim sets, and effective extraction; divergence confirms the readings are distinct constraints rather than one constraint viewed differently.',
    'If the three readings collapse into one computable structure, the kernel needs re-decomposition; if they diverge as expected (users as criminals vs users as customers vs users as patients), each reading stands alone with its own stable epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one-of-three readings; sibling readings would replace the victim set and the mechanism of authority entirely.').

omega_variable(
    illicit_market_persistence,
    'Does regulated legal commerce actually eliminate illegal supply, or do taxation and compliance costs sustain a parallel illicit channel?',
    'Longitudinal price-gap analysis between licensed and street channels, seizure volumes, and licensed-market share surveys across the interval.',
    'If illicit supply persists at scale, the regime retains an ongoing enforcement target set and its coordination claim weakens toward entrenchment; if illicit share collapses, the victim set shrinks toward historical residue and the reading''s elimination premise is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(illicit_market_persistence, empirical, 'Whether the illegal-markets-eliminated premise of this reading holds in operation.').

omega_variable(
    use_volume_response,
    'How much does legal access increase population-level use and dependence, and does that shift the third-party protection burden this reading claims to carry?',
    'Pre/post epidemiological cohorts tracking prevalence, dependence incidence, and treatment admissions against comparable non-legalizing jurisdictions.',
    'Large use-volume increases would tighten access-control calibration, justify higher extraction framed as deterrence, and strengthen sibling readings'' critiques; negligible increases would stabilize the commerce frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_volume_response, empirical, 'Use-volume elasticity of the access-control settlement.').

omega_variable(
    licensing_concentration_capture,
    'Do licensing cost structures and market caps concentrate the industry into incumbents who then capture the rule-setter?',
    'Market concentration indices, license-transfer records, and lobbying-expenditure analysis correlated against rule amendments.',
    'Captured rule-setting would push the regime toward snare characteristics for excluded entrants and justify directionality overrides raising licensed_producers'' d above its derived value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_concentration_capture, empirical, 'Incumbent-capture trajectory of the licensing architecture.').

omega_variable(
    tax_calibration_referent,
    'Are excise rates calibrated to externality costs (the quality/access-control logic) or to revenue maximization (the extraction logic)?',
    'Fiscal records correlating rate schedules with revenue targets versus actuarial externality estimates; legislative history of rate amendments.',
    'Revenue-maximizing rates confirm the extraction component is deliberate rather than incidental, supporting the tangled-rope asymmetry; externality-calibrated rates would support a purer coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_calibration_referent, empirical, 'Referent of the tax schedule: externality pricing or revenue capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t2, substance_control_authority__legalization_reading, theater_ratio, 2, 0.16).
narrative_ontology:measurement_basis(subs_tr_t2, observed).
narrative_ontology:measurement(subs_tr_t4, substance_control_authority__legalization_reading, theater_ratio, 4, 0.19).
narrative_ontology:measurement_basis(subs_tr_t4, observed).
narrative_ontology:measurement(subs_tr_t6, substance_control_authority__legalization_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(subs_tr_t6, observed).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__legalization_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement_basis(subs_tr_t8, observed).
narrative_ontology:measurement(subs_tr_t10, substance_control_authority__legalization_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(subs_tr_t10, observed).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__legalization_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(subs_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t2, substance_control_authority__legalization_reading, base_extractiveness, 2, 0.44).
narrative_ontology:measurement_basis(subs_be_t2, observed).
narrative_ontology:measurement(subs_be_t4, substance_control_authority__legalization_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement_basis(subs_be_t4, observed).
narrative_ontology:measurement(subs_be_t6, substance_control_authority__legalization_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement_basis(subs_be_t6, observed).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__legalization_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement_basis(subs_be_t8, observed).
narrative_ontology:measurement(subs_be_t10, substance_control_authority__legalization_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(subs_be_t10, observed).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__legalization_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(subs_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t2, substance_control_authority__legalization_reading, suppression_requirement, 2, 0.38).
narrative_ontology:measurement_basis(subs_su_t2, observed).
narrative_ontology:measurement(subs_su_t4, substance_control_authority__legalization_reading, suppression_requirement, 4, 0.41).
narrative_ontology:measurement_basis(subs_su_t4, observed).
narrative_ontology:measurement(subs_su_t6, substance_control_authority__legalization_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement_basis(subs_su_t6, observed).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__legalization_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement_basis(subs_su_t8, observed).
narrative_ontology:measurement(subs_su_t10, substance_control_authority__legalization_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(subs_su_t10, observed).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__legalization_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement_basis(subs_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'drug policy' conflates three structurally distinct claims about state authority over substance markets. Per the epsilon-invariance principle they are authored as a three-story family sharing the substance_control_authority kernel: prohibition_reading (criminalization; epsilon assessed over the criminal-sanction arrangement), legalization_reading (this file; epsilon assessed over the regulated-commerce arrangement), and harm_reduction_reading (clinical management; epsilon assessed over the public-health arrangement). Each story links the others via network.affects_constraints; the upstream/downstream ordering among them is empirical, as each reading cites the others' failure modes as evidence for its own.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
