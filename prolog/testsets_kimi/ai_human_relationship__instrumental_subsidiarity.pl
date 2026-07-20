% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__instrumental_subsidiarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__instrumental_subsidiarity, []).

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
 *   constraint_id: ai_human_relationship__instrumental_subsidiarity
 *   human_readable: AI as Neutral Tool Governed by Law and Subsidiarity
 *   domain: political_theology/technology_ethics
 *
 * SUMMARY:
 *   The constraint models the governance arrangement in which artificial
 *   intelligence is treated as a morally neutral instrument whose legitimate
 *   use is determined by legal and ethical frameworks operating under the
 *   Catholic Social Teaching principle of subsidiarity. This reading of the
 *   ai_human_relationship kernel competes with incarnational_humanism (which
 *   demands relational, embodied solidarity) and technocratic_optimization
 *   (which reduces human value to productivity). The framework coordinates AI
 *   policy across jurisdictions but asymmetrically concentrates authority in
 *   regulatory and compliance institutions while imposing disproportionate
 *   costs on small innovators and marginalized communities.
 *
 * KEY AGENTS:
 *   - cst_magisterium: Primary agenda-setter (institutional/civilizational) â provides normative kernel and benefits from institutional vindication
 *   - regulatory_authorities: Secondary agenda-setter (institutional/generational) â administers and enforces the legal-ethical framework
 *   - dominant_tech_firms: Primary beneficiary (powerful/constrained) â captures regulatory moats while paying compliance costs
 *   - compliance_intermediaries: Tertiary beneficiary (moderate/constrained) â extracts economic rents from mandatory ethics and legal processes
 *   - small_innovators: Primary payer (moderate/constrained) â bears disproportionate compliance burden
 *   - marginalized_communities: Secondary payer (powerless/trapped) â subject to surveillance mechanisms and exclusion
 *   - incarnational_humanist_communities: Excluded voice (organized/constrained) â structurally marginalized in governance conversations
 *   - technocratic_advocates: Excluded voice (powerful/constrained) â anthropologically foreclosed from policy tables
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.68).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.63).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI as Neutral Tool Governed by Law and Subsidiarity").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "political_theology/technology_ethics").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, '34560326-c28d-4946-9660-cd593d5b0696').
narrative_ontology:cs_kernel_codification('34560326-c28d-4946-9660-cd593d5b0696', formalized).
narrative_ontology:cs_authority_grounding('34560326-c28d-4946-9660-cd593d5b0696', lineage).
narrative_ontology:cs_interpretation_layer_present('34560326-c28d-4946-9660-cd593d5b0696').
narrative_ontology:cs_reading_relation('34560326-c28d-4946-9660-cd593d5b0696', ai_human_relationship__technocratic_optimization, forecloses).
narrative_ontology:cs_reading_relation('34560326-c28d-4946-9660-cd593d5b0696', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_axiom('34560326-c28d-4946-9660-cd593d5b0696', foundational, technology_is_morally_neutral_instrument).
narrative_ontology:cs_axiom_status(technology_is_morally_neutral_instrument, holdable).
narrative_ontology:cs_axiom_grounding('34560326-c28d-4946-9660-cd593d5b0696', technology_is_morally_neutral_instrument, conventional).
narrative_ontology:cs_axiom('34560326-c28d-4946-9660-cd593d5b0696', foundational, subsidiarity_governs_ai_regulation).
narrative_ontology:cs_axiom_status(subsidiarity_governs_ai_regulation, holdable).
narrative_ontology:cs_axiom_grounding('34560326-c28d-4946-9660-cd593d5b0696', subsidiarity_governs_ai_regulation, conventional).
narrative_ontology:cs_reference_frame('34560326-c28d-4946-9660-cd593d5b0696', procedural_subsidiarity_and_neutrality).
narrative_ontology:cs_drift_state('34560326-c28d-4946-9660-cd593d5b0696', contemporary_ai_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('34560326-c28d-4946-9660-cd593d5b0696', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, regulatory_authorities).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, dominant_tech_firms).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, compliance_intermediaries).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, cst_magisterium).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, small_innovators).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, marginalized_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, dominant_tech_firms).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, subsidiarity_principle).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, technology_neutrality_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provides the normative kernel of subsidiarity and human dignity that grounds the regulatory framework. Benefits from institutional vindication when Catholic Social Teaching principles are encoded in secular AI governance. Cannot abandon the instrumental reading without destabilizing decades of magisterial engagement with technology.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, cst_magisterium, agenda_setter,
    institutional, civilizational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, cst_magisterium, beneficiary).

% Administer and enforce AI governance frameworks derived from subsidiarity principles. Set transparency requirements, impact assessments, and compliance standards. Bound by legal mandates and political accountability; abandoning the regulatory paradigm would require institutional self-dissolution.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, regulatory_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Capture regulatory moats against smaller competitors through compliance advantages. Pay substantial costs for ethics auditing and legal review but absorb them as market-share protection. Scale permits direct lobbying to shape rules; exiting the framework means abandoning major jurisdictions.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, dominant_tech_firms, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, dominant_tech_firms, payer).

% Provide mandatory ethics auditing, legal compliance consulting, and governance advisory services. Revenue depends directly on the complexity and territorial expansion of regulatory requirements. Exit would require complete retooling to non-regulatory advisory markets.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, compliance_intermediaries, beneficiary,
    moderate, biographical, constrained, global).

% Bear compliance costs disproportionate to revenue and headcount. Lack dedicated legal teams and ethics officers. Must absorb costs that threaten viability, seek acquisition by larger firms, or exit regulated markets. Innovation cycles slow to match bureaucratic review timelines.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, small_innovators, payer,
    moderate, biographical, constrained, national).

% Subject to transparency and surveillance mechanisms framed as protective safeguards. Frequently excluded from AI services when compliance costs render deployment uneconomical in their regions. Lack voice in framework design; experience the constraint as top-down imposition rather than empowerment.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, marginalized_communities, payer,
    powerless, immediate, trapped, local).

% Promote efficiency-maximization and productivity-based optimization as the proper telos of AI deployment. Their anthropological premises are structurally ruled out by the dignity-protective framework. Excluded from policy tables where ethical constraints are treated as non-negotiable boundaries.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, technocratic_advocates, excluded,
    powerful, biographical, constrained, global).

% Affirm that legal frameworks are insufficient and that AI must be ordered through relational solidarity, preferential option for the poor, and embodied community practices. Their voice is marginalized in governance conversations that channel all legitimacy through procedural compliance and legal transparency metrics.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, incarnational_humanist_communities, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__instrumental_subsidiarity, diffuse).
narrative_ontology:fixing_cost_class(ai_human_relationship__instrumental_subsidiarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates AI development across diverse jurisdictions and value systems by providing a shared procedural framework: impact assessments, transparency requirements, and subsidiarity-based governance that channels innovation away from unregulated harm toward legally defined human ends.
% TRANSFER_FUNCTION: Moves regulatory authority and compliance burden to legal-ethical oversight bodies; moves competitive advantage toward entities that can afford dedicated ethics and legal infrastructure; moves governance voice away from relational and embodied communities to procedural experts and compliance professionals.
% ABSENT_VOICES: Technocratic optimizers who measure human value by productivity; incarnational humanists who see legal frameworks as reductive substitutes for solidarity; small innovators who cannot afford compliance but are not seated at policy tables; affected populations whose experience of surveillance-as-protection is not represented on ethics review boards.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, AI governance would lose its CST-subsidiarity grounding and revert toward either unregulated market optimization or centralized technocratic efficiency maximization. Regulatory authority would fragment, compliance industries would collapse, and the specific protections for human dignity through legal transparency would disappear.
% FOUNDING_PROBLEM: Rapid AI development threatening to outpace moral and legal boundaries; risk of technology being deployed for ends contrary to human dignity; absence of shared governance frameworks capable of respecting local pluralism while protecting vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by Catholic Social Teaching documents and some secular human rights frameworks. Contested by libertarian tech advocates who deny the problem requires legal intervention, and by incarnational humanists who attest the problem is real but that legal frameworks are an insufficient response.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__instrumental_subsidiarity, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__instrumental_subsidiarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_human_relationship__instrumental_subsidiarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because the procedural framework creates significant compliance overhead that functions as a competitive moat and generates economic rents for legal-ethics intermediaries. Suppression (0.63) reflects the active exclusion of unregulated innovation and alternative governance models (technocratic and incarnational). Theater ratio (0.52) captures the growing share of activity devoted to performative compliance, ethics washing, and bureaucratic box-checking rather than substantive protection. Accessibility collapse (0.48) is moderate: alternatives are marginalized but not eliminated. Resistance (0.55) reflects ongoing pushback from libertarian, technocratic, and incarnational quarters. The claimed type is tangled_rope because the constraint possesses a genuine coordination function alongside asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (regulatory authorities, CST magisterium) experience the constraint as necessary coordination that prevents technological harm and vindicates their institutional mission. The beneficiary seats (dominant tech firms, compliance intermediaries) experience it as a source of competitive advantage or revenue. The payer seats (small innovators, marginalized communities) experience the same structure as extractive burden and surveillance. The excluded seats (technocratic advocates, incarnational humanists) experience it as silencing. The engine computes these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory authorities and the CST magisterium are agenda-setters who derive authority and institutional vindication (low d, subsidy via power consolidation). Dominant tech firms and compliance intermediaries are beneficiaries who capture regulatory moats and rents (low-to-moderate d). Small innovators and marginalized communities are payers who bear compliance costs and surveillance burdens with limited exit (high d). Technocratic and incarnational excluded parties sit at high d because the constraint actively suppresses their alternatives. The derivation follows from beneficiary/victim declarations and exit modulation: trapped and constrained exit amplifies directionality toward the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 genealogy interview shows a contested founding problem: rapid AI development without moral boundaries. The problem is corroborated as live by CST and human rights sources but contested by those who see legal frameworks as insufficient or unnecessary. The status is contested rather than dead, so mandatrophy is not resolved. The constraint is classified as tangled_rope rather than snare because it retains a coordination function (genuine harm prevention, shared governance vocabulary) that a pure extraction model would mislabel. If the coordination function atrophied entirely and only compliance theater remained, it would degrade toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidiarity_centralization_tension,
    'Does the actual governance architecture under this reading respect subsidiarity, or has it drifted toward centralized regulatory accumulation?',
    'Comparative jurisdictional analysis measuring regulatory granularity against the principle that decisions should occur at the lowest competent level.',
    'If centralized, the reading has become a vehicle for technocratic extraction wearing CST language; if genuinely subsidiary, the coordination function remains authentic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_centralization_tension, empirical, 'Whether subsidiarity is practiced or only invoked rhetorically').

omega_variable(
    technology_neutrality_falsifiability,
    'Is the claim that AI is a morally neutral instrument empirically sustainable, or do algorithmic systems encode structural biases that make neutrality a constructive fiction?',
    'Audits of deployed AI systems for embedded value-laden design choices and disparate impact across populations.',
    'If falsified, the foundational axiom of this reading collapses and the constraint reverts to a cover story for governance capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_neutrality_falsifiability, empirical, 'Whether AI neutrality thesis survives empirical contact').

omega_variable(
    legal_framework_sufficiency,
    'Can procedural legal frameworks alone secure human dignity in AI deployment, or do they necessarily require complement by relational, incarnational practices?',
    'Comparative outcome studies between jurisdictions relying primarily on legal compliance versus those integrating solidarity-based, community-embedded oversight.',
    'If legal frameworks are insufficient, this reading is structurally incomplete and functions as a performative substitute for genuine protection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legal_framework_sufficiency, conceptual, 'Whether procedural governance is sufficient for dignity protection').

omega_variable(
    kernel_reading_instrumental_subsidiarity,
    'This constraint is the instrumental_subsidiarity reading of the ai_human_relationship kernel. How would the structural classification change if the incarnational_humanism reading or technocratic_optimization reading were adopted instead?',
    'Construct sibling constraint stories for each reading and compare base extractiveness, beneficiary/victim structures, and directionality distributions.',
    'The incarnational reading would likely reduce extractiveness by de-emphasizing compliance intermediaries; the technocratic reading would eliminate the dignity-protective coordination function entirely. Current classification as tangled_rope is specific to this reading''s procedural-legal architecture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instrumental_subsidiarity, conceptual, 'Structural delta across kernel readings').

omega_variable(
    axiom_foreclosure_ambiguity,
    'Does the axiom that human dignity is secured by procedural governance logically foreclose the technocratic_optimization reading''s claim that human value is measured by productivity?',
    'Formal analysis of whether these anthropological premises can be held simultaneously in a single normative framework.',
    'If genuinely foreclosed, the engine should compute contradiction; if merely competing, they coexist as rival policy frameworks. Affects whether the relation should be forecloses or coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_foreclosure_ambiguity, conceptual, 'Logical relation between rival anthropological axioms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_instr_subs_tr_t0, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_instr_subs_tr_t8, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 8, 0.28).
narrative_ontology:measurement(ai_instr_subs_tr_t16, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 16, 0.35).
narrative_ontology:measurement(ai_instr_subs_tr_t24, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 24, 0.42).
narrative_ontology:measurement(ai_instr_subs_tr_t32, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 32, 0.48).
narrative_ontology:measurement(ai_instr_subs_tr_t40, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(ai_instr_subs_be_t0, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_instr_subs_be_t8, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(ai_instr_subs_be_t16, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(ai_instr_subs_be_t24, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(ai_instr_subs_be_t32, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(ai_instr_subs_be_t40, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_instr_subs_su_t0, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_instr_subs_su_t8, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(ai_instr_subs_su_t16, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(ai_instr_subs_su_t24, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(ai_instr_subs_su_t32, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(ai_instr_subs_su_t40, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 40, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__technocratic_optimization).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__incarnational_humanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_human_relationship kernel. The kernel conflates three structurally distinct claims about technology and human value: instrumental neutrality with procedural governance (this story), efficiency-maximizing technocracy (technocratic_optimization), and relational incarnational solidarity (incarnational_humanism). Each reading has distinct epsilon, beneficiary structures, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
