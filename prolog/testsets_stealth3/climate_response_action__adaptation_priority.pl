% ============================================================================
% CONSTRAINT STORY: climate_response_action__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__adaptation_priority, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_response_action__adaptation_priority
 *   human_readable: Adaptation-First Climate Response Regime
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   Since the Paris Agreement's Article 7 established a global adaptation
 *   goal, climate response has increasingly been organized as a
 *   protection-first regime: national adaptation plans, resilience
 *   infrastructure pipelines, and adaptation finance channels that treat a
 *   degree of warming as locked in and redirect resources toward defending
 *   exposed populations and assets. The arrangement coordinates real
 *   protective capacity — sea defenses, early-warning systems,
 *   drought-tolerant agriculture — while moving costs asymmetrically:
 *   developing-nation treasuries fund protection from narrow fiscal bases or
 *   borrow it, a roughly $350B annual North-South financing gap persists,
 *   protection quality tracks wealth, and the accepted warming trajectory
 *   hands uncompensated costs to future generations. This file authors ONE
 *   reading of the contested climate_response_action kernel — the
 *   adaptation_priority reading — as a clean epsilon-invariant constraint;
 *   the mitigation_priority and degrowth_transformation readings are separate
 *   stories linked through network.affects_constraints. Epsilon's referent is
 *   the standing adaptation-first arrangement itself, assessed by this
 *   reading's own protective commitments — never the mitigation-led or
 *   degrowth arrangements its critics would substitute. KEY AGENTS (by
 *   structural relationship): - multilateral_development_banks:
 *   agenda-setting administrator (institutional/arbitrage) — sets adaptation
 *   finance terms, collects lending income - high_income_nation_governments:
 *   dual-positioned beneficiary-payer (institutional/arbitrage) — pledges
 *   finance, protects own assets first -
 *   resilience_infrastructure_contractors: primary commercial beneficiary
 *   (organized/mobile) - insurance_sector: risk-pricing beneficiary
 *   (institutional/arbitrage) - developing_nation_treasuries: primary fiscal
 *   payer (moderate/constrained) - frontline_vulnerable_communities:
 *   residual-risk bearer (powerless/trapped) - future_generations: silent
 *   bearer of accepted warming costs (non-acting party) -
 *   loss_and_damage_advocates: excluded voice (organized/constrained) -
 *   ipcc_wgii_scientists: analytical observer
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__adaptation_priority, 0.64).
domain_priors:suppression_score(climate_response_action__adaptation_priority, 0.46).
domain_priors:theater_ratio(climate_response_action__adaptation_priority, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, extractiveness, 0.64).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__adaptation_priority, "Adaptation-First Climate Response Regime").
narrative_ontology:topic_domain(climate_response_action__adaptation_priority, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_action__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__adaptation_priority, '8306164d-781a-4a11-8f0f-30966e3e89ed').
narrative_ontology:cs_kernel_codification('8306164d-781a-4a11-8f0f-30966e3e89ed', formalized).
narrative_ontology:cs_authority_grounding('8306164d-781a-4a11-8f0f-30966e3e89ed', distributed).
narrative_ontology:cs_reading_relation('8306164d-781a-4a11-8f0f-30966e3e89ed', climate_response_action__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('8306164d-781a-4a11-8f0f-30966e3e89ed', climate_response_action__degrowth_transformation, influences).
narrative_ontology:cs_axiom('8306164d-781a-4a11-8f0f-30966e3e89ed', foundational, warming_lockin_acceptance_as_planning_basis).
narrative_ontology:cs_axiom_status(warming_lockin_acceptance_as_planning_basis, holdable).
narrative_ontology:cs_axiom_grounding('8306164d-781a-4a11-8f0f-30966e3e89ed', warming_lockin_acceptance_as_planning_basis, empirically_contingent).
narrative_ontology:cs_axiom('8306164d-781a-4a11-8f0f-30966e3e89ed', foundational, protection_of_vulnerable_populations_primacy).
narrative_ontology:cs_axiom_status(protection_of_vulnerable_populations_primacy, holdable).
narrative_ontology:cs_axiom_grounding('8306164d-781a-4a11-8f0f-30966e3e89ed', protection_of_vulnerable_populations_primacy, deontological).
narrative_ontology:cs_axiom('8306164d-781a-4a11-8f0f-30966e3e89ed', secondary, adaptation_financed_through_growth_economies).
narrative_ontology:cs_axiom_status(adaptation_financed_through_growth_economies, holdable).
narrative_ontology:cs_axiom_grounding('8306164d-781a-4a11-8f0f-30966e3e89ed', adaptation_financed_through_growth_economies, instrumental).
narrative_ontology:cs_reference_frame('8306164d-781a-4a11-8f0f-30966e3e89ed', resilience_investment_protection_framework).
narrative_ontology:cs_drift_state('8306164d-781a-4a11-8f0f-30966e3e89ed', contemporary_post_paris_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8306164d-781a-4a11-8f0f-30966e3e89ed', '').
narrative_ontology:cs_kernel_id(climate_response_action__adaptation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, resilience_infrastructure_contractors).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, multilateral_development_banks).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, high_income_nation_governments).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, insurance_sector).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, developing_nation_treasuries).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, frontline_vulnerable_communities).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, high_income_nation_governments).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, locked_in_warming_premise).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, resilience_dividend_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Channel the bulk of tracked international adaptation finance, largely as loans rather than grants, and set the terms under which developing nations access it. They design country platforms, appraise resilience projects, and collect interest and fees on adaptation lending portfolios. Their exposure is reputational and portfolio-based; they can reprice or redirect lending across regions.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, multilateral_development_banks, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, multilateral_development_banks, beneficiary).

% Pledge adaptation finance at annual negotiations while directing larger domestic budgets to protecting their own coastlines, agriculture, and infrastructure. Their firms win export-linked resilience contracts; their contributions fall short of assessed needs, leaving the financing gap open. Exit for them means renegotiating pledge frameworks, which they periodically do.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, high_income_nation_governments, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, high_income_nation_governments, payer).

% Design and build sea defenses, resilient water systems, and climate-proofed infrastructure under government and bank-financed contracts. Revenue scales with adaptation spending; they press for larger resilience budgets and can pursue projects across jurisdictions.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, resilience_infrastructure_contractors, beneficiary,
    organized, biographical, mobile, global).

% Reprice climate risk into premiums, expand coverage products where adaptation makes risk insurable, and withdraw from markets where it does not, shifting residual costs back to households and governments. Their underwriting data shapes which protections get built.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, insurance_sector, beneficiary,
    institutional, biographical, arbitrage, global).

% Must fund adaptation from narrow tax bases or external borrowing, diverting revenue from health, education, and existing debt service. They negotiate finance terms from positions of fiscal stress; declining the terms would leave their populations unprotected, so they accept loan-heavy packages.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, developing_nation_treasuries, payer,
    moderate, biographical, constrained, national).

% Live in exposed deltas, drylands, and informal coastal settlements. They receive whatever protection reaches them after allocation decisions made elsewhere, and bear residual flood, heat, and crop losses directly. Moving away means abandoning homes, livelihoods, and social ties, which most cannot do.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, frontline_vulnerable_communities, payer,
    powerless, immediate, trapped, regional).

% Will inherit the warming trajectory this arrangement accepts, along with the debts taken to build today's defenses and those defenses' maintenance liabilities. They take no part in current allocation decisions and cannot exit.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(climate_response_action__adaptation_priority, future_generations).

% Coalitions pressing for compensation for irreversible climate losses rather than protection-only spending. They hold standing in negotiation spaces, but their demands sit outside this arrangement's protection-first frame, so their proposals are deferred rather than adopted.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, loss_and_damage_advocates, excluded,
    organized, generational, constrained, global).

% Assess impacts, vulnerability, and adaptation limits across regions, publishing gap analyses that document shortfalls between estimated needs and delivered finance. They hold no allocation authority and depend on the arrangement only for research attention.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, ipcc_wgii_scientists, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(climate_response_action__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem that no exposed nation or household can self-provide engineered defenses, early-warning networks, drought-tolerant agriculture, or resilient infrastructure at scale: protection capacity is pooled, appraised, and delivered centrally through national plans and international finance channels.
% TRANSFER_FUNCTION: Moves capital from national budgets and development lenders into resilience projects; moves debt service from developing-nation treasuries to creditors; moves residual climate risk onto whoever is left unprotected after wealth-correlated allocation; moves accepted warming costs onto future generations.
% ABSENT_VOICES: Frontline communities are consulted late in allocation processes and rarely decisive; future generations have no seat at all; loss-and-damage advocates hold standing but their compensation demands are deferred outside the protection-first frame; mitigation-first and degrowth proponents contest the arrangement from outside it.
% DISAPPEARANCE_RATIONALE: If the adaptation-first arrangement vanished overnight, exposed nations would face unmanaged impacts with no defense pipelines, contractor sectors would lose their project books, adaptation lending channels would close, and insured asset markets would reprice chaotically; the world would rearrange around unbuffered climate damage and emergency response.
% FOUNDING_PROBLEM: The arrangement was built to solve a problem mitigation politics had stalled on: greenhouse gases already emitted commit the system to decades of further warming regardless of future emissions cuts, so someone must protect exposed populations and assets now, and adaptation offered a politically tractable frame when prevention agreements kept underdelivering.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group II impact and vulnerability assessments corroborate the live protective need from outside the benefiting parties, as do actuarial loss records and frontline community testimony; the UNEP Adaptation Gap Reports, produced independent of the finance recipients, attest annually that needs far exceed delivery.
narrative_ontology:disappearance_verdict(climate_response_action__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_action__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__adaptation_priority, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope on structure, not preference: the arrangement solves a genuine collective problem (no exposed nation or household can self-provide early warning, engineered defenses, or resilient seed systems at scale) AND moves costs asymmetrically through the same channels (loan-weighted finance, wealth-tracked protection quality, accepted-warming externalities), and it holds together only through active enforcement — treaty pledge cycles, bank appraisal regimes, national budget earmarks. Metrics are authored descriptively and independently of the claim. Extractiveness 0.64: the roughly $540B/yr universal-protection requirement against roughly $28B/yr delivered public finance leaves the difference as unfunded exposure or debt; the loan share of adaptation finance converts protection into repayment streams; protection quality correlates with wealth. Suppression 0.46 is authored RAW and UNSCALED — it reflects the coercive machinery holding allocation in place (conditional lending covenants, pledge-cycle discipline, budget earmarks), not scaled by scope or directionality; the engine owns any scaling. Theater_ratio 0.31: pledge announcements, loan-face-value accounting, and resilience branding are performative, but funded projects physically exist. Accessibility_collapse 0.38: mitigation-first and degrowth responses remain live alternatives; the adaptation frame forecloses neither, it competes with them for fiscal bandwidth. Resistance 0.57: loss-and-damage campaigns, grant-not-loans demands, and mitigation advocacy contest the arrangement continuously. Measurements run on ONE shared grid (t=0..10, mapping 2015-2025) with every tracked metric authored at every point; suppression_requirement is tracked because this story specifically traces enforcement-machinery build-up (NAP institutionalization, finance-tracking regimes, covenant conditionality), not merely extraction drift. Receipt surface: gain_flow is deliberately OMITTED — re-reading the stakeholder situations, several seats demonstrably accrue gains (banks collect lending income, contractors book revenue, insurers expand premia, donor firms win contracts) with no single predominant capturer; authoring 'diffuse' would affirmatively assert that no seat captures, which the situations falsify, and naming one seat would overstate concentration. fixing_cost is authored 'prohibitive': restructuring toward grant-based, needs-allocated finance runs into creditor portfolio interests, donor domestic budget politics, and consensus treaty procedure — costs exceeding any single fixer's benefit.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the agenda-setting lender seat the arrangement is a functioning protection pipeline it administers professionally; from the developing-nation treasury seat it is a repayment schedule attached to survival infrastructure; from the frontline-community seat it is whatever protection arrives after others allocate, plus the residual losses that do not arrive as protection; from the future-generations seat it is a pure inherited burden with no seat at the table. Same nominal policy, different computed types — the engine derives this divergence from power, exit, and role data, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: contractors (mobile, organized) sit nearest the subsidy end; banks and insurers collect fee and premium income while bearing little of the physical risk themselves; high-income nation governments are dual-positioned — declared beneficiary with a secondary payer role reflecting partial pledge payments — so their derived d sits low but not minimal. Victim declarations drive high d: treasuries (constrained exit — declining terms means abandoning their populations' protection) sit near full-target; frontline communities (trapped — mobility would cost homes and livelihoods) nearer still; future generations (trapped, non-acting) at the extreme. Excluded and observer seats carry no extraction position. Scope amplification applies through the engine: the arrangement operates globally, where verification of delivered-versus-pledged protection is weakest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — impacts arriving on any emissions trajectory, requiring protection nobody else provides — is live and externally corroborated (IPCC WGII assessments, actuarial loss data, frontline testimony), so no mandatrophy declaration is authored and the mismatch consumer should find status=live crossed with verdict=world_rearranges: no zombie flag. The classification guards both mislabels: reading the protective framing as pure rope ignores the debt-weighted finance and wealth-tracked protection that move costs through the same pipes; reading the extraction as snare-cover ignores that funded defenses, warnings, and seed systems genuinely reduce mortality and loss where they reach people. The lifecycle risk is theater drift: if pledge inflation and relabeled lending keep rising while delivery stagnates, the coordination half atrophies into performance — the theater_ratio series is the tripwire.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the adaptation_priority reading of the climate_response_action kernel; which structural features would flip under the mitigation_priority or degrowth_transformation sibling readings?',
    'Generate the sibling stories and compare victim sets, transfer functions, and epsilon over the shared referent; the divergence locates where the readings actually disagree.',
    'Under mitigation_priority the future_generations victim load shrinks and emitter-side payers appear; under degrowth_transformation the fiscal-capacity burden is reframed as a throughput constraint and the $540B annual figure loses its meaning as stated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this is one of three readings; sibling deltas are structural, not rhetorical.').

omega_variable(
    adaptation_finance_gap_closure,
    'Will the roughly $350B North-South financing gap close through grant-based transfers, or persist and be financed as debt?',
    'Track adaptation finance composition (grant versus loan share) and delivery-versus-pledge ratios across successive negotiation cycles using independently audited disbursement data.',
    'Grant closure lowers effective extraction on developing_nation_treasuries and softens the tangled_rope toward rope; debt persistence raises extraction and hardens the arrangement toward snare-flavored dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_finance_gap_closure, empirical, 'Whether the financing gap resolves as transfer or as leverage.').

omega_variable(
    protection_disparity_inherence,
    'Are protection disparities an inherent property of capacity-based adaptation, or a remediable distributional failure?',
    'Compare protection outcomes across jurisdictions with equal exposure but different fiscal capacity and finance access, controlling for hazard intensity.',
    'If inherent, the arrangement structurally perpetuates inequality and the extractiveness score understates it; if remediable, the arrangement is coordination with a distributive lag.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_disparity_inherence, conceptual, 'Whether wealth-tracked protection is constitutive or contingent.').

omega_variable(
    intergenerational_transfer_status,
    'Is accepting temperature rise a defensible intergenerational tradeoff (protection now outweighs prevention later), or extraction from future generations?',
    'Normative analysis of discount-rate choices combined with observed data on whether adaptation spending crowds out mitigation investment.',
    'An extraction reading strengthens the future_generations victim declaration and pushes classification toward snare; a residual-risk-management reading weakens it and supports the coordination half.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_transfer_status, conceptual, 'Status of the accepted-warming cost transfer across generations.').

omega_variable(
    adaptation_finance_accounting_ambiguity,
    'Do reported adaptation finance figures measure delivered protection, or relabeled and recounted flows such as loans booked at face value and double-counted projects?',
    'Independent audit of adaptation finance tracking against disbursement-level and outcome-level data.',
    'Inflated accounting raises the true theater_ratio above the authored 0.31 and masks undelivery; a corrected account would shift both measured theater and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_finance_accounting_ambiguity, empirical, 'Measurement integrity of the adaptation finance ledger.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__adaptation_priority, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_action__adaptation_priority, theater_ratio, 0, 0.21).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t2, climate_response_action__adaptation_priority, theater_ratio, 2, 0.23).
narrative_ontology:measurement_basis(clim_tr_t2, observed).
narrative_ontology:measurement(clim_tr_t4, climate_response_action__adaptation_priority, theater_ratio, 4, 0.26).
narrative_ontology:measurement_basis(clim_tr_t4, observed).
narrative_ontology:measurement(clim_tr_t6, climate_response_action__adaptation_priority, theater_ratio, 6, 0.28).
narrative_ontology:measurement_basis(clim_tr_t6, observed).
narrative_ontology:measurement(clim_tr_t8, climate_response_action__adaptation_priority, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(clim_tr_t8, observed).
narrative_ontology:measurement(clim_tr_t10, climate_response_action__adaptation_priority, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(clim_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_action__adaptation_priority, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t2, climate_response_action__adaptation_priority, base_extractiveness, 2, 0.55).
narrative_ontology:measurement_basis(clim_be_t2, observed).
narrative_ontology:measurement(clim_be_t4, climate_response_action__adaptation_priority, base_extractiveness, 4, 0.58).
narrative_ontology:measurement_basis(clim_be_t4, observed).
narrative_ontology:measurement(clim_be_t6, climate_response_action__adaptation_priority, base_extractiveness, 6, 0.6).
narrative_ontology:measurement_basis(clim_be_t6, observed).
narrative_ontology:measurement(clim_be_t8, climate_response_action__adaptation_priority, base_extractiveness, 8, 0.62).
narrative_ontology:measurement_basis(clim_be_t8, observed).
narrative_ontology:measurement(clim_be_t10, climate_response_action__adaptation_priority, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(clim_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_action__adaptation_priority, suppression_requirement, 0, 0.36).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t2, climate_response_action__adaptation_priority, suppression_requirement, 2, 0.38).
narrative_ontology:measurement_basis(clim_su_t2, observed).
narrative_ontology:measurement(clim_su_t4, climate_response_action__adaptation_priority, suppression_requirement, 4, 0.4).
narrative_ontology:measurement_basis(clim_su_t4, observed).
narrative_ontology:measurement(clim_su_t6, climate_response_action__adaptation_priority, suppression_requirement, 6, 0.42).
narrative_ontology:measurement_basis(clim_su_t6, observed).
narrative_ontology:measurement(clim_su_t8, climate_response_action__adaptation_priority, suppression_requirement, 8, 0.44).
narrative_ontology:measurement_basis(clim_su_t8, observed).
narrative_ontology:measurement(clim_su_t10, climate_response_action__adaptation_priority, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(clim_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__degrowth_transformation).

% DUAL FORMULATION NOTE:
% 'Climate response' decomposes into three structurally distinct arrangements (adaptation_priority, mitigation_priority, degrowth_transformation) with different epsilon values, victim sets, and transfer functions; this story authors only adaptation_priority. The upstream empirical premise shared across readings — climate-system inertia makes some further warming unavoidable — is cited differently by each: as justification for protection spending here, as urgency for emissions cuts in mitigation_priority, and as evidence of growth-system failure in degrowth_transformation. Family members link via affects_constraints; each file carries its own stable epsilon per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
