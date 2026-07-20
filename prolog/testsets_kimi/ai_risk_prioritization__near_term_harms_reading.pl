% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__near_term_harms_reading, []).

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
 *   constraint_id: ai_risk_prioritization__near_term_harms_reading
 *   human_readable: Near-Term AI Harms Prioritization Regime
 *   domain: AI Safety / Technology Governance / Risk Assessment
 *
 * SUMMARY:
 *   This constraint instantiates the near_term_harms_reading of the
 *   ai_risk_prioritization kernel. It treats AI risk as primarily manifesting
 *   in present, measurable harmsâalgorithmic discrimination, labor
 *   displacement, and surveillanceâdirected at marginalized populations.
 *   The constraint allocates legitimacy, funding, and regulatory attention
 *   toward justice interventions (bias audits, worker protections,
 *   surveillance regulation) while framing existential risk as a speculative
 *   distraction. It is authored as a tangled rope because it coordinates
 *   genuine collective action on documented harms while simultaneously
 *   extracting from existential risk researchers through discursive
 *   suppression and from marginalized populations through potential audit
 *   theater.
 *
 * KEY AGENTS:
 *   - fairness_researchers: Primary beneficiary (organized/constrained) â collects funding, status, and career structure from the near-term framing.
 *   - marginalized_populations: Dual-positioned payer/beneficiary (powerless/trapped) â named as beneficiaries but structurally pay through objectification in audits and continued exposure to unremedied harms.
 *   - existential_risk_researchers: Excluded victim (moderate/constrained) â bears suppression of funding and legitimacy; framed as speculative.
 *   - civil_rights_regulators: Agenda setter (institutional/analytical) â administers compliance mandates and expands regulatory jurisdiction.
 *   - tech_corporations: Secondary payer (powerful/mobile) â bears compliance costs and operational friction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.62).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.68).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "Near-Term AI Harms Prioritization Regime").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "AI Safety / Technology Governance / Risk Assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, '1de9320b-cd12-44db-a7b3-b73d99fd2dd8').
narrative_ontology:cs_kernel_codification('1de9320b-cd12-44db-a7b3-b73d99fd2dd8', distributed).
narrative_ontology:cs_authority_grounding('1de9320b-cd12-44db-a7b3-b73d99fd2dd8', distributed).
narrative_ontology:cs_reading_relation('1de9320b-cd12-44db-a7b3-b73d99fd2dd8', ai_risk_prioritization__existential_risk_reading, coexists_with).
narrative_ontology:cs_axiom('1de9320b-cd12-44db-a7b3-b73d99fd2dd8', foundational, present_harm_materiality).
narrative_ontology:cs_axiom_status(present_harm_materiality, holdable).
narrative_ontology:cs_axiom_grounding('1de9320b-cd12-44db-a7b3-b73d99fd2dd8', present_harm_materiality, empirically_contingent).
narrative_ontology:cs_axiom('1de9320b-cd12-44db-a7b3-b73d99fd2dd8', foundational, justice_intervention_priority).
narrative_ontology:cs_axiom_status(justice_intervention_priority, holdable).
narrative_ontology:cs_axiom_grounding('1de9320b-cd12-44db-a7b3-b73d99fd2dd8', justice_intervention_priority, deontological).
narrative_ontology:cs_reference_frame('1de9320b-cd12-44db-a7b3-b73d99fd2dd8', present_harm_mitigation_framework).
narrative_ontology:cs_drift_state('1de9320b-cd12-44db-a7b3-b73d99fd2dd8', post_generative_ai_proliferation, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1de9320b-cd12-44db-a7b3-b73d99fd2dd8', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, existential_risk_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, tech_corporations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conduct bias audits, develop fairness metrics, and publish on algorithmic accountability. Their careers, grants, and conference prestige depend on the near-term harms framing remaining dominant in AI ethics. They can exit to industry roles but face identity-lock to the justice-oriented research community and its funding networks.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_researchers, beneficiary,
    organized, biographical, constrained, global).

% Subject to algorithmic hiring, lending, and surveillance systems that the framework claims to regulate. They experience the harms that justify the constraint but rarely control audit agendas or receive the research funding generated in their name. Exit is blocked by the ubiquity of these systems and by racialized and class-based identity categories that cannot be opted out of.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, marginalized_populations, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, marginalized_populations, beneficiary).

% Research long-term risks from advanced AI. Under the near-term prioritization regime, their work is framed as speculative and deprioritized in funding and policy. They are present in the field but structurally excluded from priority-setting venues; their exit options are constrained by the concentration of relevant funding in near-term frameworks.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, existential_risk_researchers, excluded,
    moderate, civilizational, constrained, global).

% Set enforcement agendas around algorithmic accountability, mandating bias audits and transparency reports. They expand regulatory jurisdiction through the near-term harms frame and administer the compliance frameworks that operationalize it.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, civil_rights_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Deploy AI systems at scale and bear the costs of bias audits, surveillance regulations, and transparency mandates. They comply with the near-term framework while lobbying against specific provisions; their global footprint allows limited jurisdictional arbitrage.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, tech_corporations, payer,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__near_term_harms_reading, fairness_researchers).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates civil society, regulators, and researchers around measurable, present-day harms from deployed AI systems, creating shared vocabularies and audit standards for discrimination and surveillance.
% TRANSFER_FUNCTION: Moves research funding and policy legitimacy from speculative long-term risk research toward fairness and accountability research; moves compliance costs onto technology corporations; moves the visibility of algorithmic harm onto marginalized populations.
% ABSENT_VOICES: Existential risk researchers and long-termist philosophers are present in the field but structurally excluded from priority-setting tables; affected communities themselves are often spoken for rather than with in audit and policy design processes.
% DISAPPEARANCE_RATIONALE: If the near-term harms framing vanished overnight, funding flows would shift toward alignment and x-risk research, corporate compliance regimes would lose their primary justification structure, and the policy apparatus around algorithmic accountability would lose its organizing consensusâthe AI governance landscape would reorganize around whichever frame replaced it.
% FOUNDING_PROBLEM: AI systems were being deployed at scale without adequate assessment of their impacts on marginalized populations, producing measurable discrimination in hiring, lending, and criminal justice, while the research community focused on abstract technical performance.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations and affected community advocates attest the problem is still live. However, corporate AI ethics officers and some funders now corroborate that the founding problem has been partially captured by institutional incentives that prioritize audit theater over material redistribution; independent investigative journalism provides outside corroboration of ongoing harms.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__near_term_harms_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is high because the constraint channels substantial resources away from alternative risk frames and because the audit apparatus may substitute measurement for material justice. Suppression (0.68) is higher still: the constraint's persistence depends on actively framing existential risk as illegitimate or elite, and on corporate compliance that suppresses alternative governance models. Theater ratio (0.45) reflects significant performative activityâbias audits that generate papers without redistribution, 'ethics washing' that satisfies regulators without altering power. Accessibility collapse (0.58) captures the partial foreclosure of long-term risk and structural economic critiques from policy discourse. Resistance (0.55) reflects pushback from x-risk researchers, some tech corporations, and pluralist funders. The measurement series share a single time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The fairness researcher seat should compute toward rope: they see genuine coordination on measurable harms and justifiable boundary policing against speculative distraction. The marginalized population seat should compute toward snare or tangled rope: they see attention without power, measurement without redistribution, and identity-lock that prevents exit from racialized surveillance. The existential risk researcher seat should compute toward snare: they experience active suppression of their research agenda and denial of legitimacy. The engine derives these divergences from the same structural data via directionality and scope scaling.
 *
 * DIRECTIONALITY LOGIC:
 *   Fairness researchers are declared beneficiaries with moderate power and constrained exit, placing them near the beneficiary pole but not at zero. Marginalized populations are declared both beneficiaries and victims; their powerless/trapped profile amplifies effective extraction despite the nominal beneficiary label, because victim status plus zero exit dominates the derivation. Existential risk researchers are excluded and victimized with constrained exit, pushing them toward full target. Tech corporations are payers but powerful and mobile, dampening their effective extraction. Civil rights regulators are agenda setters with analytical exit, placing them near the beneficiary/administrator pole.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by separating the live coordination function (addressing documented algorithmic harm) from the extractive overlay (audit theater, boundary policing). The founding problemâunaccountable AI discriminationâis contested between live and captured: civil liberties groups say it is ongoing, while critics say the response has been captured by institutional incentives. The R5 genealogy shows a mandate that may be drifting from live problem-solving toward institutional self-maintenance, as indicated by the rising theater ratio. A mandatrophy-resolved verdict is not declared because the constraint still coordinates genuine harm reduction even as it extracts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marginalized_beneficiary_ambiguity,
    'Does the near-term harms prioritization structurally benefit marginalized populations, or does it instrumentalize their suffering to sustain a fairness research and audit industry?',
    'Track material redistribution and power transfer to affected communities versus research funding and corporate compliance spending over the same interval.',
    'If instrumental, the constraint''s extractiveness from marginalized populations is higher than its coordination value, pushing classification toward snare; if genuine, the coordination function is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_beneficiary_ambiguity, empirical, 'Whether marginalized populations are genuine beneficiaries or instrumentalized objects').

omega_variable(
    x_risk_suppression_nature,
    'Is the suppression of existential risk discourse a necessary boundary-maintaining function of the near-term frame, or an incidental side effect of resource competition?',
    'Examine funding and publication patterns: if near-term researchers actively block x-risk participation in shared venues, suppression is structural; if exclusion is passive resource competition, it is incidental.',
    'Structural suppression would confirm the tangled rope classification; incidental exclusion would lower the suppression score and suggest a rope classification with unfortunate side effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(x_risk_suppression_nature, conceptual, 'Structural versus incidental suppression of existential risk discourse').

omega_variable(
    kernel_reading_contest,
    'This constraint is the near_term_harms_reading of kernel ai_risk_prioritization; the sibling existential_risk_reading would reallocate victim and beneficiary sets toward future humanity and alignment researchers. Which reading''s axioms better match empirical reality?',
    'Comparative empirical assessment of present harm magnitude versus existential risk probability and timeline; cannot be resolved by fiat.',
    'Resolution would collapse the kernel to a single reading or establish irreducible pluralism; affects resource allocation across AI safety domains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer frame ambiguity between near-term and existential readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(ai_r_tr_t24, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 24, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(ai_r_be_t24, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 4, 0.3).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(ai_r_su_t24, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization__existential_risk_reading).

% DUAL FORMULATION NOTE:
% This constraint and ai_risk_prioritization__existential_risk_reading are dual readings of the same kernel (ai_risk_prioritization). They share the same empirical objectâAI riskâbut assign opposite priority structures, beneficiary/victim sets, and intervention portfolios. They should be evaluated as separate constraints with different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
