% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: Near-Term Harms Reading of AI Risk Governance Priority
 *   domain: technological/political
 *
 * SUMMARY:
 *   The standing arrangement under contest is the allocation of AI risk
 *   governance attention, funding, and regulatory bandwidth that consolidated
 *   after the 2022-2023 frontier-model inflection: recurring international
 *   summit processes, chartered safety institutes, a lab-funded research
 *   ecosystem, and voluntary evaluation regimes, all organized around
 *   long-run catastrophic scenarios as the field's defining concern. Assessed
 *   from the near-term-harms seat — the reading instantiated here — this
 *   arrangement leaves demonstrated present harms (algorithmic discrimination
 *   in credit, hiring, benefits, and policing; election-scale misinformation;
 *   automation-driven labor displacement; expanding surveillance; Global
 *   South data and moderation supply chains) systematically under-mitigated
 *   while the entities deploying the systems escape binding constraints. The
 *   epsilon authored below is a property of this reading applied to that
 *   standing arrangement; the reading's endorsed alternative (harm-first
 *   allocation) is not the referent. Sibling readings of the same kernel are
 *   separate constraint files linked through the network section. KEY AGENTS
 *   (by structural relationship): - frontier_ai_developers: Primary
 *   beneficiary and co-agenda-setter (institutional/arbitrage) — collects
 *   deferred binding constraints on deployed systems -
 *   ai_safety_research_establishment: Secondary beneficiary
 *   (organized/identity_locked) — collects funding, status, and expert seats
 *   - intergovernmental_summit_processes: Agenda-setter
 *   (institutional/constrained) — formally convenes and charters the priority
 *   structure - global_south_populations: Primary target (powerless/trapped)
 *   — bears data-supply, moderation, and deployment externalities -
 *   algorithmically_discriminated_groups: Target (powerless/constrained) —
 *   bears misclassification and surveillance harms -
 *   automation_displaced_workers: Target (moderate/constrained) — bears labor
 *   displacement - civil_society_harm_advocates: Excluded voice
 *   (organized/constrained) — documents harms, holds marginal agenda weight -
 *   ai_ethics_audit_researchers: Analytical observer (analytical/analytical)
 *   — measures the harm/priority discrepancy
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.72).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.58).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "Near-Term Harms Reading of AI Risk Governance Priority").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "technological/political").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, '1e406012-de30-4838-996f-b65b370ee316').
narrative_ontology:cs_kernel_codification('1e406012-de30-4838-996f-b65b370ee316', distributed).
narrative_ontology:cs_authority_grounding('1e406012-de30-4838-996f-b65b370ee316', expertise).
narrative_ontology:cs_interpretation_layer_present('1e406012-de30-4838-996f-b65b370ee316').
narrative_ontology:cs_reading_relation('1e406012-de30-4838-996f-b65b370ee316', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e406012-de30-4838-996f-b65b370ee316', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('1e406012-de30-4838-996f-b65b370ee316', foundational, demonstrated_present_harms_take_regulatory_precedence).
narrative_ontology:cs_axiom_status(demonstrated_present_harms_take_regulatory_precedence, holdable).
narrative_ontology:cs_axiom_grounding('1e406012-de30-4838-996f-b65b370ee316', demonstrated_present_harms_take_regulatory_precedence, empirically_contingent).
narrative_ontology:cs_axiom('1e406012-de30-4838-996f-b65b370ee316', foundational, speculative_scenarios_cannot_outrank_verified_victims).
narrative_ontology:cs_axiom_status(speculative_scenarios_cannot_outrank_verified_victims, holdable).
narrative_ontology:cs_axiom_grounding('1e406012-de30-4838-996f-b65b370ee316', speculative_scenarios_cannot_outrank_verified_victims, deontological).
narrative_ontology:cs_reference_frame('1e406012-de30-4838-996f-b65b370ee316', demonstrated_harm_proportional_priority).
narrative_ontology:cs_drift_state('1e406012-de30-4838-996f-b65b370ee316', contemporary_post_frontier_scaling, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1e406012-de30-4838-996f-b65b370ee316', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, frontier_ai_developers).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, ai_safety_research_establishment).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, algorithmically_discriminated_groups).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, automation_displaced_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and deploy the large-scale models whose present-day outputs generate the harms in question. Fund a large share of the safety research ecosystem, place personnel in government advisory roles, and frame public risk communication around long-run catastrophic scenarios. When governance attention fixes on distant scenarios, binding rules for current deployments stay voluntary; when attention turns to present harms, they can redirect messaging or restructure products. They collect the value of deferred binding constraints on deployed systems.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, frontier_ai_developers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, frontier_ai_developers, agenda_setter).

% Institutes, university groups, and in-house teams that study catastrophic AI scenarios. They receive the majority of philanthropic and public funding earmarked for AI risk, publish in the field's flagship venues, and fill the expert seats at governance convenings. Their career capital, grant pipelines, and professional networks are built around the long-run scenario framing; pivoting to harm-measurement work would mean rebuilding credentials and funding relationships from scratch.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_safety_research_establishment, beneficiary,
    organized, biographical, identity_locked, global).

% Convene the recurring international meetings where AI risk priorities are formally announced, issue communiques defining which risks count as the field's business, and charter the evaluation and incident-sharing bodies that operationalize those priorities. Their agendas are shaped by the expert delegations and lab-affiliated research they depend on for technical input.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, intergovernmental_summit_processes, agenda_setter,
    institutional, generational, constrained, global).

% Host the data-labeling and content-moderation supply chains that train and police deployed models, absorb deployment externalities such as election-scale misinformation and extractive data practices, and hold the least representation in the venues where governance priorities are set. Their exposure is continuous and present-tense; participating in priority-setting conversations requires resources and standing they do not have.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_populations, payer,
    powerless, immediate, trapped, global).

% Experience denials, misclassifications, and heightened surveillance from deployed systems in lending, hiring, benefits administration, housing, and policing. Individual recourse exists case-by-case through complaints or litigation, but the systems keep operating while cases proceed, and no seat in the governance priority conversation represents them as a class.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, algorithmically_discriminated_groups, payer,
    powerless, biographical, constrained, national).

% Lose tasks, wages, and job categories as deployed automation scales. Some organize through unions and sector campaigns, giving them more voice than individual consumers of AI outputs, but transition support depends on policy attention that competes with the dominant risk framing for legislative bandwidth.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, automation_displaced_workers, payer,
    moderate, biographical, constrained, national).

% Document present harms, litigate discrimination cases, and campaign for binding rules on deployed systems. They attend governance convenings in marginal numbers, hold few agenda slots, and receive a small fraction of risk-related funding. Their objections register as one input among many rather than as a claim on the priority structure.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, civil_society_harm_advocates, excluded,
    organized, biographical, constrained, global).

% Measure bias rates, misinformation spread, labor-market effects, and surveillance expansion across deployed systems, and compare stated governance priorities against where harms and resources actually fall. They publish the discrepancy records the other seats argue over.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_ethics_audit_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__near_term_harms_reading, frontier_ai_developers).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real coordination problem: giving governments, labs, and researchers a shared vocabulary, shared evaluation protocols, and standing channels for discussing advanced-AI hazards, so hazard anticipation is not attempted separately and incompatibly by every jurisdiction.
% TRANSFER_FUNCTION: Moves governance attention, research funding, regulatory bandwidth, and moral urgency from present-harm mitigation affecting marginalized populations toward long-run catastrophic-scenario preparation; correspondingly moves the costs of lightly governed deployment onto the populations harmed by current systems.
% ABSENT_VOICES: Frontline affected communities — data annotation and content-moderation workers, people denied benefits or flagged by biased systems, workers displaced by deployed automation, Global South institutions hosting pilot deployments — are largely absent from summit tables, safety-institute boards, and expert advisory panels. Civil-society advocates attend in marginal numbers and hold few agenda slots; their objection would be that the priority structure spends the field's finite attention on scenarios distant from the harms their constituents file daily.
% DISAPPEARANCE_RATIONALE: If the priority structure vanished overnight, the funding and convening power currently organized around long-run scenarios would redistribute toward measured present harms — fairness audits, displacement compensation, deployment gating — and deployers would face nearer-term binding pressure; the safety research economy would reorganize around whoever holds the new funding mandate.
% FOUNDING_PROBLEM: After the 2022-2023 frontier-model inflection, no standing machinery existed for anticipating severe AI hazards: governments lacked evaluation capacity, labs faced no common disclosure expectations, and hazard discussion was ad hoc. The arrangement was built to close that anticipation gap before capabilities outran it.
% FOUNDING_PROBLEM_CORROBORATION: State-published AI safety reports and independent academic risk assessments — sources outside the lab and safety-institute beneficiary set — corroborate that the founding hazard problem remains real. Civil-society harm documentation and labor-organization records corroborate the complementary fact that the current weighting diverges from the distribution of verified harm. No source outside the benefiting parties attests that the present weighting itself is correct; the contest between this reading and its siblings is precisely over that weighting.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72) because the arrangement's operative transfer — governance capacity, funding, and moral urgency — flows away from populations bearing verified, ongoing harms toward scenarios none of the affected parties will live to litigate, while deployers convert the deflected scrutiny into regulatory latitude. Suppression (0.58) is authored as a raw structural property, unscaled by power or scope: the mechanism is agenda gatekeeping (who gets expert seats, whose evidence counts as risk), not legal coercion, so it sits below enforcement-machinery levels. Theater_ratio (0.48) reflects the growing share of arrangement activity that is pledge-signing, summitry, and voluntary-commitment maintenance relative to binding mitigation of measured harms. Accessibility_collapse is low (0.35): harm-first governance is a visible, articulated alternative that persists in legislative proposals, audit literatures, and civil-society platforms — the arrangement de-prioritizes it rather than eliminating it. Resistance (0.62) is sustained: labor campaigns, discrimination litigation, Global South data-worker organizing, and internal dissent within the safety community. All three temporal series share one grid (points 0-36 at step 6) so the engine samples every metric at every examined time point; the trajectories show enforcement machinery institutionalizing (suppression_requirement rising), deployments scaling against a fixed priority (base_extractiveness rising), and performative activity outgrowing functional mitigation (theater_ratio rising).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats compute differently from identical structural data. Global South populations and discriminated groups sit trapped or constrained with no substitute for the governance capacity that never reaches them — their effective burden is amplified toward the full-target end. Frontier developers hold arbitrage-grade exit: they can reframe, relocate product lines, or shift messaging, which dampens their experienced burden toward the subsidy side even as they collect the arrangement's principal gains. The research establishment occupies the distinctive middle: it collects funding and status (beneficiary-side position) yet is identity_locked — its professional self-concept, grant pipelines, and networks are constituted by the long-run framing, so it cannot cheaply defect even where its own harm data argues for rebalancing. Intergovernmental agenda-setters experience the arrangement as neutral administration, but their technical dependence on lab-affiliated expertise tilts their effective position toward the deployers'. The engine computes these per-seat classifications; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (frontier_ai_developers, ai_safety_research_establishment) drive derived directionality toward the beneficiary end: the arrangement subsidizes both — deferred binding constraints for the former, funded mandates and expert standing for the latter. Victim declarations (global_south_populations, algorithmically_discriminated_groups, automation_displaced_workers) drive directionality toward the target end, with trapped and constrained exit options holding them near full-target. No directionality overrides are authored: the beneficiary/victim structure plus exit options already differentiate every seat, and the two institutional-power agents (developers, summit processes) separate cleanly through their role declarations rather than needing a power-atom override that could not distinguish them. Global spatial scope on the payer seats raises verification difficulty, which the engine accounts for when scaling effective burden.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ensuring coordinated anticipation of severe AI hazards — remains live, so this is not a decayed mandate and no mandatrophy resolution is declared. The classification question is compositional: reading the arrangement as pure extraction would erase the genuine coordination it performs (shared evaluation protocols, incident channels, a common risk taxonomy that did not exist before 2023); reading it as pure coordination would erase the documented divergence between where harms fall and where governance capacity goes. The tangled_rope claim keeps both facts load-bearing and routes the dispute to the right question — not whether the arrangement functions, but who pays for its functioning. The theater_ratio series is the early-warning instrument for the decay path: if the founding problem were ever closed (hazards credibly bounded) while summit and pledge activity continued, the arrangement would cross into theatrical maintenance, and the rising theater trajectory authored here is the signature that such a transition would begin with.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the near_term_harms_reading of kernel ai_risk_governance_priority; what would adopting a sibling reading change structurally?',
    'Compare the compiled stories across the reading set: victim sets, epsilon distributions, and resource-flow edges per reading.',
    'The existential_risk_reading removes present-harm populations from the victim center and re-centers humanity-at-large under counterfactual scenarios; the bridge_reading merges both victim sets and dissolves the priority conflict into a framework-design question. Classification of THIS file is stable only within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a three-reading kernel.').

omega_variable(
    diversion_vs_complement,
    'Does long-run-scenario-dominant governance actually divert regulatory attention and resources away from present harms, or does it complement harm mitigation by building general capacity?',
    'Track legislative bandwidth, enforcement actions, and funding allocations attributable to each framing across the interval; natural experiments in jurisdictions that adopted harm-first statutes.',
    'If diversion dominates, the standing arrangement''s burden on present-harm victims is directly attributable to the priority structure; if complementarity dominates, part of the measured burden belongs to general underinvestment rather than this arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversion_vs_complement, empirical, 'Whether the dominant framing displaces or reinforces present-harm mitigation.').

omega_variable(
    harm_attribution_counterfactual,
    'How much of the measured present harm would persist under any plausible governance priority, versus how much is produced by the priority structure itself?',
    'Comparative-jurisdiction analysis and counterfactual modeling of harm rates under harm-first governance regimes.',
    'Determines whether the authored extractiveness reflects the arrangement''s causal contribution or baseline industry conduct that no priority structure would prevent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_attribution_counterfactual, empirical, 'Counterfactual share of present harm attributable to the priority weighting.').

omega_variable(
    exclusion_mechanism_structural_or_internalized,
    'Is the absence of affected communities from priority-setting structural (gatekept agendas, unfunded participation, credential barriers) or internalized (learned disengagement after repeated dismissal)?',
    'Post-inclusion trajectory: if invited communities sustain participation once funded seats and agenda weight exist, the absence was structural; if engagement decays despite genuine access, internalized disengagement contributes.',
    'If internalized, remedying formal access understates the burden the arrangement reproduces, and effective suppression exceeds what the structural measure captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_mechanism_structural_or_internalized, empirical, 'Structural versus internalized mechanism behind affected-community absence.').

omega_variable(
    priority_claim_scope,
    'Does this reading assert that long-run catastrophic risks are unreal, or only that verified present harms take precedence in resource allocation?',
    'Textual analysis of the reading''s advocacy corpus and its concrete policy proposals.',
    'If denial, the arrangement''s long-run component functions as pure deflection and the structure trends toward pure extraction; if precedence-only, the long-run function is genuine but overweighted and the hybrid character deepens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(priority_claim_scope, conceptual, 'Scope of the precedence claim: denial versus prioritization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.34).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 6, 0.37).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(ai_r_tr_t18, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 18, 0.42).
narrative_ontology:measurement(ai_r_tr_t24, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(ai_r_tr_t30, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 30, 0.46).
narrative_ontology:measurement(ai_r_tr_t36, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 36, 0.48).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(ai_r_be_t18, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(ai_r_be_t24, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(ai_r_be_t30, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(ai_r_be_t36, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 36, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 6, 0.46).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(ai_r_su_t18, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 18, 0.53).
narrative_ontology:measurement(ai_r_su_t24, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(ai_r_su_t30, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement(ai_r_su_t36, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 36, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI risk governance priorities' decomposes into three structurally distinct constraints — one per reading of the kernel — because the readings assign different victim sets, different epsilon profiles, and different resource-flow edges. This file is the near-term member: high epsilon on present deployment harms, victims drawn from affected populations, beneficiaries drawn from deployers and the funded research establishment. The existential sibling inverts the profile; the bridge sibling fuses the victim sets. Family members are linked through affects_constraints; the upstream/downstream relation is evidential — each documented present harm strengthens this reading's precedence claim and pressures the bridge reading's symmetry assumption (modeled as the influences edge in cs_structure.reading_relations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
