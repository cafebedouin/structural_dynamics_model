% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__integrated_reading, []).

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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: Integrated Alignment Priority Settlement (Dual Harm-Class Mandate)
 *   domain: technology governance/AI ethics/risk assessment
 *
 * SUMMARY:
 *   This story instantiates the integrated_reading of the
 *   ai_alignment_priority kernel: the standing arrangement under contest is
 *   the dual-mandate settlement now governing AI safety — safety institutes
 *   with paired evaluation and equity charters, funding portfolios split
 *   between capability-risk and deployment-harm work, and frameworks
 *   requiring labs to run both red-teaming and bias audits. Assessed by this
 *   reading's own lights, the settlement genuinely coordinates two
 *   communities that would otherwise war over the alignment label, yet it
 *   continues to impose real costs on both of its declared victim sets:
 *   deployment harms persist for present marginalized groups at reduced but
 *   nonzero rates, and capability risk continues to accumulate for future
 *   populations. Compliance burdens concentrate on labs, and the settlement's
 *   gains accrue disproportionately to incumbents through a fixed-cost moat.
 *   The sibling readings (existential_risk_reading, nearterm_harms_reading)
 *   are separate constraints with their own epsilon values and victim sets;
 *   they are linked through the network edge, not folded into this story. The
 *   claim/metric independence rule applies: the claimed type is what this
 *   reading believes is structurally true of the standing arrangement, and
 *   the metrics are what it believes is descriptively true — the engine
 *   computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - - frontier_ai_labs: Primary payer with captured offset ([powerful]/[constrained]) — bears dual compliance costs, recoups part through incumbent moat and legitimacy
 *   - - xrisk_research_community: Beneficiary with dilution costs ([organized]/[identity_locked]) — gained agenda share, ceded label monopoly
 *   - - algorithmic_justice_advocates: Beneficiary with dilution costs ([organized]/[identity_locked]) — gained standards-table seat, ceded lexical-priority claim
 *   - - present_marginalized_groups: Residual payer ([powerless]/[trapped]) — deployment harms persist under the settlement
 *   - - future_populations: Residual payer ([powerless]/[trapped]) — capability risk persists, represented only through proxies
 *   - - ai_funders_standards_bodies: Agenda setter ([institutional]/[mobile]) — writes the frameworks and portfolio weights that hold the settlement
 *   - - audit_red_team_vendors: Secondary beneficiary ([organized]/[mobile]) — the dual mandate created their services market
 *   - - small_developers_open_source: Excluded voice ([moderate]/[mobile]) — bears the heaviest relative compliance cost with no seat
 *   - - ai_governance_analysts: Analytical observer ([analytical]/[analytical]) — maps the settlement's operation across both harm classes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.57).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.45).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.57).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Integrated Alignment Priority Settlement (Dual Harm-Class Mandate)").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "technology governance/AI ethics/risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, '518d57a7-fdbd-43b2-bf0d-c927a3bd20b2').
narrative_ontology:cs_kernel_codification('518d57a7-fdbd-43b2-bf0d-c927a3bd20b2', distributed).
narrative_ontology:cs_authority_grounding('518d57a7-fdbd-43b2-bf0d-c927a3bd20b2', distributed).
narrative_ontology:cs_reading_relation('518d57a7-fdbd-43b2-bf0d-c927a3bd20b2', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('518d57a7-fdbd-43b2-bf0d-c927a3bd20b2', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_axiom('518d57a7-fdbd-43b2-bf0d-c927a3bd20b2', foundational, harm_classes_structurally_complementary).
narrative_ontology:cs_axiom_status(harm_classes_structurally_complementary, holdable).
narrative_ontology:cs_axiom_grounding('518d57a7-fdbd-43b2-bf0d-c927a3bd20b2', harm_classes_structurally_complementary, instrumental).
narrative_ontology:cs_axiom('518d57a7-fdbd-43b2-bf0d-c927a3bd20b2', secondary, joint_victim_representation_imperative).
narrative_ontology:cs_axiom_status(joint_victim_representation_imperative, holdable).
narrative_ontology:cs_axiom_grounding('518d57a7-fdbd-43b2-bf0d-c927a3bd20b2', joint_victim_representation_imperative, deontological).
narrative_ontology:cs_reference_frame('518d57a7-fdbd-43b2-bf0d-c927a3bd20b2', balanced_dual_harm_class_portfolio).
narrative_ontology:cs_drift_state('518d57a7-fdbd-43b2-bf0d-c927a3bd20b2', contemporary_frontier_scaling_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('518d57a7-fdbd-43b2-bf0d-c927a3bd20b2', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, xrisk_research_community).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, algorithmic_justice_advocates).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, audit_red_team_vendors).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, present_marginalized_groups).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, future_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, frontier_ai_labs).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, frontier_ai_labs).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, xrisk_research_community).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, algorithmic_justice_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and deploy frontier models; run red-teaming exercises against loss-of-control scenarios and bias audits against deployment discrimination because funders, procurement rules, and safety-institute access now require both. The dual obligation lands as fixed safety headcount and process cost that scales favorably with organization size. Leaving the settlement would mean losing safety-institute partnerships, procurement eligibility, and the legitimacy that comes with demonstrated dual coverage.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, frontier_ai_labs, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, frontier_ai_labs, beneficiary).

% Researchers and institutes focused on loss-of-control risk in advanced AI systems. They gained standing, dedicated funding lines, and policy access when the integrated settlement made capability risk half of the official alignment agenda; they also ceded sole ownership of the alignment label and submit to portfolio reviews that weigh deployment-harm work equally. Exit would mean abandoning the broader coalition and the funding channels that run through it.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, xrisk_research_community, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, xrisk_research_community, payer).

% Civil-society organizations and researchers focused on discriminatory and extractive deployments of AI systems. The settlement gave their agenda a seat inside alignment governance through mandatory impact assessments and equity criteria in evaluations, while diluting their claim that present harms deserve first priority. Exit would mean returning to an adversarial posture outside the rooms where standards are written.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, algorithmic_justice_advocates, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, algorithmic_justice_advocates, payer).

% People subjected to algorithmic decisions in lending, hiring, housing, benefits, and policing. Deployment harms persist under the settlement at reduced but nonzero rates; remedies arrive through audit cycles and mandated reassessments rather than through their own voice. They cannot opt out of the decision systems that govern their access to basic goods.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, present_marginalized_groups, payer,
    powerless, biographical, trapped, national).

% People not yet born whose exposure to catastrophic loss-of-control outcomes depends on how capability development is governed now. The settlement allocates part of the safety portfolio to their protection, but their interests enter only through proxy institutions such as long-horizon funders, forecasters, and safety institutes, with no mechanism to register dissatisfaction directly.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_populations, payer,
    powerless, civilizational, trapped, universal).

% Philanthropic funders, public AI safety institutes, and standards organizations that write the evaluation frameworks, funding gates, and procurement criteria operationalizing the dual mandate. They convene both communities, set portfolio weights between capability-risk work and deployment-harm work, and revise frameworks on multi-year cycles. Their leverage comes from controlling which work counts as aligned.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_funders_standards_bodies, agenda_setter,
    institutional, generational, mobile, global).

% Commercial firms selling red-team exercises, bias audits, and compliance documentation to labs and deployers. The dual mandate created their market; revenue depends on the obligations remaining broad enough to require external expertise, and they can reorient service lines toward whichever harm class the next framework revision emphasizes.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, audit_red_team_vendors, beneficiary,
    organized, immediate, mobile, global).

% Small labs and open-source projects that ship models outside major procurement channels. Dual-mandate compliance costs fall hardest on them, and they hold no seat in the standards processes that set the obligations; their objection that the settlement entrenches incumbents reaches the table mainly through public comment periods.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, small_developers_open_source, excluded,
    moderate, biographical, mobile, global).

% Academic and think-tank researchers studying the alignment-priority debate itself. They map funding flows, publication patterns, and framework adoption across both harm classes, and publish comparisons that the other seats cite or contest.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_governance_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__integrated_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_alignment_priority__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coalition-splitting problem in AI safety: prevents the loss-of-control research community and the deployment-fairness community from warring over the alignment label and duplicating infrastructure, and creates a shared evaluation surface in which a single model assessment covers both capability risk and deployment discrimination.
% TRANSFER_FUNCTION: Moves funding, attention, and agenda share away from single-focus allocations toward balanced portfolios; moves compliance obligations onto labs and deployers in the form of paired red-teaming and audit requirements; moves legitimacy and standards-table access to whichever actors demonstrate coverage of both harm classes.
% ABSENT_VOICES: Small developers and open-source maintainers priced out of dual compliance are absent from standards-setting; communities subjected to algorithmic decisions are represented only through advocacy intermediaries rather than directly; capability researchers who regard the entire priority debate as premature have no formal seat.
% DISAPPEARANCE_RATIONALE: If the integrated settlement vanished overnight, the two research communities would resume contesting the alignment label, funding portfolios would polarize toward whichever camp held agenda-setting positions, shared evaluation infrastructure would fragment into separate capability-eval and fairness-audit tracks, and both harm classes would be covered worse than under the balanced mandate.
% FOUNDING_PROBLEM: The alignment field fragmented into two camps: one treating alignment as preventing catastrophic loss of control over advanced AI, the other treating it as preventing discriminatory and extractive deployments. Each camp claimed the alignment mantle and treated the other's agenda as a distraction, leaving both harm classes partially uncovered and consuming field-wide energy in a priority war.
% FOUNDING_PROBLEM_CORROBORATION: National AI safety institutes' published dual-mandate charters and evaluation reports corroborate that both harm classes remain live concerns; civil-society incident databases documenting discriminatory deployments and the independent forecasting literature documenting capability risk both attest the persistence of the founding problem from outside the benefiting parties.
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__integrated_reading, 0.57, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.57 at interval end) because both harm classes remain partially uncovered under the settlement — the integrated frame reduced extraction relative to the single-focus counterfactual but did not eliminate it, and compliance costs plus moat effects add a second extraction channel aimed at labs and excluded entrants. Suppression is moderate (0.45) and is authored as a raw structural property, unscaled: the settlement holds against single-focus pressure through funding gates, framework requirements, and coalition discipline rather than hard barriers — soft enforcement, but real, since defecting to a single-focus agenda costs a lab or institute its standing in the settlement. Theater ratio (0.32) reflects growing parallel box-checking: as dual mandates institutionalized, a rising share of red-teaming and audit activity became compliance documentation rather than integrated risk work. Accessibility_collapse is low (0.30) because the alternatives — the two single-focus framings — remain fully articulated, funded, and live; nothing collapses upon understanding this constraint. Resistance is substantial (0.60) because both specialist camps actively resist dilution of their priority claims, which is precisely why active enforcement is required. The measurement series run on one shared seven-point grid so every tracked metric is authored at every examined time point. The base_extractiveness trajectory is U-shaped rather than monotonic: integration initially reduced extraction (0.62 to 0.52 by midpoint) as coverage gaps closed, then crept back up (to 0.57) as theater, moat effects, and portfolio-stagnation set in — the settlement solved the founding problem partially and then began collecting coordination rents on top of the residual harms. The suppression_requirement series rises gently and plateaus, modeling enforcement machinery that matured during settlement formation and then stabilized; this is an enforcement-capacity story, which is why the series is authored at all.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat (funders and standards bodies), the settlement is a functioning portfolio that ended a destructive priority war and both communities now attend. From the two advocacy-community seats, the same settlement reads as a hard-won expansion of their agenda that simultaneously taxes their urgency claims. From the frontier-lab seat, it is a compliance burden partially recouped through moat advantages and procurement access. From the two victim seats, it is whatever portion of harm remains after the settlement's partial coverage — the marginalized-group seat experiences audit-cycle latency on present discrimination; the future-population seat experiences continued accumulation of unmitigated capability risk through proxy representation only. The excluded small-developer seat experiences the settlement purely as an entry barrier. The engine computes these divergences from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. present_marginalized_groups and future_populations are declared victims with trapped exit: they sit near the full-target end, and their effective extraction is amplified by the settlement's global-to-universal scope, which makes verification of actual harm-class coverage difficult. xrisk_research_community and algorithmic_justice_advocates are declared beneficiaries but carry genuine dilution costs via their secondary payer position — their derived directionality sits nearer the beneficiary end than a pure beneficiary's, reflecting that the settlement subsidizes them unevenly. audit_red_team_vendors sit nearest the beneficiary end: the mandate manufactures their revenue with negligible offsetting cost. frontier_ai_labs are declared payers whose moat capture and legitimacy gains pull them back from the full-target position — their net position is target-leaning but materially offset. ai_funders_standards_bodies sit near symmetric: they administer the settlement and collect legitimacy and convening power rather than direct revenue. No directionality_overrides are authored: the override surface keys on power atom, and this story contains same-atom agents with genuinely different positions (xrisk_research_community and audit_red_team_vendors are both organized but occupy different directional positions), so a per-atom override would misapply; the declared beneficiary/victim structure plus secondary roles carries the differentiation instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the settlement as a pure rope would erase the persistent extraction on both victim sets — the residual deployment harms, the accumulating capability risk, the moat effect on entrants — and would treat the settlement's self-description (complementary, not competing) as the verdict. Reading it as a pure snare would erase the genuine coordination function: shared evaluation infrastructure, coalition stability, and dual coverage that neither single-focus camp achieved in a decade of contest. The mandatrophy interview confirms the founding problem (the priority war leaving both harm classes uncovered) is still live, corroborated from outside the benefiting parties by safety-institute charters, civil-society incident databases, and the forecasting literature; mandatrophy is therefore not resolved, and the settlement's mandate has not outlived its function. The forward risk this classification flags is drift: if theater_ratio continues its rise while base_extractiveness climbs back toward its pre-settlement level, the settlement decays toward inertial performance — a dual mandate maintained in documentation while both harm classes revert to single-focus neglect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_vs_tradeoff,
    'Is the relationship between catastrophic-risk work and present-harm work actually complementary under scarcity, or does it exhibit real trade-offs that the integrated framing papers over?',
    'Portfolio studies tracking the marginal return of shifting funding and researcher time between capability-risk work and deployment-harm work, including interaction effects (e.g., whether capability insights improve audit tooling and whether deployment feedback improves risk models).',
    'If the relationship is genuinely zero-sum under scarcity, the integrated mandate masks a live allocation fight, effective extraction on both victim sets rises above the authored value, and the classification drifts snare-ward; if complementary, the coordination function is stronger than authored and extraction falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_vs_tradeoff, empirical, 'Whether the dual mandate coordinates a positive-sum portfolio or conceals a zero-sum allocation fight.').

omega_variable(
    integration_vs_box_checking,
    'Are dual-methodology requirements producing genuine methodological integration, or parallel box-checking in which red-team findings and bias-audit findings never inform each other?',
    'Audit whether red-team discoveries feed deployment-harm remediation and vice versa: cross-citation rates between the two literatures, joint remediation tickets, and whether evaluation reports integrate findings across harm classes or file them separately.',
    'If box-checking dominates, the authored theater_ratio understates functional decay and the constraint drifts toward inertial performance maintained for compliance appearance rather than coverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_vs_box_checking, empirical, 'Whether the dual mandate integrates methods or runs them as disconnected compliance rituals.').

omega_variable(
    kernel_weighting_disagreement_location,
    'This constraint is one reading of the kernel ai_alignment_priority; the disagreement with sibling readings (existential_risk_reading, nearterm_harms_reading) is located in the weighting axiom — whether one harm class lexically dominates or the two are structurally complementary. Which weighting is correct is not resolvable inside this story.',
    'Adopting a sibling reading changes the constraint structurally: the victim set narrows to one harm class, the methodology collapses to a single track, and epsilon redistributes onto the abandoned class. Resolution requires the separate stories for each sibling reading, not modification of this one.',
    'If a sibling reading displaced this one in governance practice, present_marginalized_groups or future_populations would drop out of the protected set entirely and the dual-methodology enforcement machinery would lose its justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_weighting_disagreement_location, conceptual, 'Committer structure: reading-indexed weighting axiom; sibling readings instantiate different constraints with different victim sets.').

omega_variable(
    future_population_representation_fidelity,
    'Can the interests of future populations be represented with any fidelity in present portfolio decisions, or does their inclusion function rhetorically to weight the allocation toward capability-risk work?',
    'Analysis of how future-population claims are operationalized: whether stated long-horizon concerns actually move near-term funding decisions, and whether proxy institutions'' revealed preferences track their declared representation of future interests.',
    'If representation is largely rhetorical, the victim declaration for future_populations inflates this reading''s claimed coverage and the effective balance tilts toward whichever harm class the proxy institutions privately prioritize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_population_representation_fidelity, conceptual, 'Whether future-population victimhood is substantively represented or rhetorically deployed.').

omega_variable(
    incumbent_moat_effect,
    'Does the dual-mandate compliance burden raise fixed costs that advantage large incumbent labs over smaller entrants, converting the settlement into a structural moat?',
    'Compare compliance cost curves across organization sizes: safety headcount, audit expenditure, and certification overhead as a fraction of compute and revenue, and entry rates before versus after dual-mandate frameworks took effect.',
    'A strong moat effect concentrates the settlement''s gains at frontier_ai_labs, raises effective extraction on excluded small developers, and pushes the classification toward capture-dominated territory; a weak effect supports the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_moat_effect, empirical, 'Whether dual-mandate compliance costs function as an incumbent-advantaging barrier.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__integrated_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ai_a_tr_t2, ai_alignment_priority__integrated_reading, theater_ratio, 2, 0.2).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__integrated_reading, theater_ratio, 4, 0.23).
narrative_ontology:measurement(ai_a_tr_t6, ai_alignment_priority__integrated_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__integrated_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__integrated_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__integrated_reading, theater_ratio, 12, 0.32).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__integrated_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(ai_a_be_t2, ai_alignment_priority__integrated_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__integrated_reading, base_extractiveness, 4, 0.54).
narrative_ontology:measurement(ai_a_be_t6, ai_alignment_priority__integrated_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__integrated_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__integrated_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__integrated_reading, base_extractiveness, 12, 0.57).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__integrated_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(ai_a_su_t2, ai_alignment_priority__integrated_reading, suppression_requirement, 2, 0.4).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__integrated_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(ai_a_su_t6, ai_alignment_priority__integrated_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__integrated_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__integrated_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__integrated_reading, suppression_requirement, 12, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, nearterm_harms_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI alignment priorities' decomposes into three structurally distinct readings of the kernel ai_alignment_priority. This story instantiates the integrated_reading (balanced dual harm-class mandate; moderate epsilon over both victim sets; dual methodology). existential_risk_reading instantiates a narrower constraint whose victim set is future populations alone and whose methodology is capability evaluation; nearterm_harms_reading instantiates one whose victim set is present marginalized groups alone and whose methodology is deployment auditing. Each reading carries its own epsilon, beneficiaries, and victims; they are linked here because the integrated settlement's resource-allocation structure directly conditions the funding environment both sibling readings operate in, and because each sibling is frequently cited as evidence for or against the integrated balance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
