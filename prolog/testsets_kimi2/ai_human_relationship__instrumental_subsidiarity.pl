% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__instrumental_subsidiarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
 *   human_readable: AI as Neutral Tool under Subsidiary Governance
 *   domain: political-theology/technology-ethics
 *
 * SUMMARY:
 *   This constraint story models the institutionalized arrangement arising
 *   from the instrumental_subsidiarity reading of the ai_human_relationship
 *   kernel, prevalent in Catholic social teaching and technology ethics
 *   discourse. The arrangement treats artificial intelligence as a morally
 *   neutral tool whose harms are preventable through proper legal governance,
 *   ethical oversight, and subsidiarityâdevolving responsibility to the
 *   lowest competent authority. The framework coordinates continued AI
 *   development by promising human-centered oversight, while structurally
 *   diffusing moral and legal responsibility away from developers toward
 *   users, local institutions, and regulators. Key agents include AI
 *   developers who gain moral distance from harms, regulatory and
 *   legal-ethics professionals who gain jurisdiction and professional
 *   standing, and affected populations who bear the costs of systems
 *   attributed to misuse rather than design. The constraint is claimed as
 *   tangled_rope: it possesses a genuine coordination function (governance
 *   framework enabling innovation with oversight) yet exhibits asymmetric
 *   extraction (responsibility diffusion and procedural capture).
 *
 * KEY AGENTS:
 *   - regulatory_bureaucracies (institutional/constrained): Primary agenda-setter â sets governance frameworks, gains jurisdiction and mission from the regulatory mandate
 *   - ai_developers (powerful/arbitrage): Primary beneficiary â collects moral and legal distance from harms through the neutral-tool framing
 *   - legal_ethics_professionals (organized/mobile): Secondary beneficiary â collects professional revenue from compliance and ethics interpretation
 *   - affected_populations (powerless/trapped): Primary payer â bears harms attributed to misuse or local governance failure rather than system design
 *   - civil_society_theologians (organized/analytical): Analytical observer â critiques the divergence between the framework's dignity claims and its material effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.62).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.58).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI as Neutral Tool under Subsidiary Governance").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "political-theology/technology-ethics").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, '34207e08-74ea-449a-8455-b8377d682a00').
narrative_ontology:cs_kernel_codification('34207e08-74ea-449a-8455-b8377d682a00', distributed).
narrative_ontology:cs_authority_grounding('34207e08-74ea-449a-8455-b8377d682a00', lineage).
narrative_ontology:cs_interpretation_layer_present('34207e08-74ea-449a-8455-b8377d682a00').
narrative_ontology:cs_reading_relation('34207e08-74ea-449a-8455-b8377d682a00', ai_human_relationship__technocratic_optimization, influences).
narrative_ontology:cs_reading_relation('34207e08-74ea-449a-8455-b8377d682a00', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_axiom('34207e08-74ea-449a-8455-b8377d682a00', foundational, technology_morally_neutral).
narrative_ontology:cs_axiom_status(technology_morally_neutral, holdable).
narrative_ontology:cs_axiom_grounding('34207e08-74ea-449a-8455-b8377d682a00', technology_morally_neutral, deontological).
narrative_ontology:cs_axiom('34207e08-74ea-449a-8455-b8377d682a00', foundational, subsidiarity_as_procedural_safeguard).
narrative_ontology:cs_axiom_status(subsidiarity_as_procedural_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('34207e08-74ea-449a-8455-b8377d682a00', subsidiarity_as_procedural_safeguard, deontological).
narrative_ontology:cs_reference_frame('34207e08-74ea-449a-8455-b8377d682a00', instrumental_subsidiarity_ideal).
narrative_ontology:cs_drift_state('34207e08-74ea-449a-8455-b8377d682a00', contemporary_ai_governance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('34207e08-74ea-449a-8455-b8377d682a00', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, ai_developers).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, regulatory_bureaucracies).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, legal_ethics_professionals).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, affected_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the neutral-tool framing which locates moral responsibility in use-cases and governance rather than in design choices. This allows continued development and deployment with minimal substantive constraint, while participating in ethics discourse that affirms their social license.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ai_developers, beneficiary,
    powerful, biographical, arbitrage, global).

% Set and enforce the governance frameworks that classify AI as a neutral instrument requiring legal-ethical oversight. Gain jurisdiction, budget, and institutional mission from the regulatory mandate. Their authority depends on maintaining the framing that AI is governable through procedural means.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, regulatory_bureaucracies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, regulatory_bureaucracies, beneficiary).

% Derive professional standing and revenue from interpreting, auditing, and certifying AI systems against ethical and legal frameworks. The proceduralization of AI governance expands their role as necessary intermediaries between developers and the public.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, legal_ethics_professionals, beneficiary,
    organized, biographical, mobile, national).

% Experience harms from AI systems including bias, exclusion, surveillance, and labor displacement, yet the neutral-tool framing attributes these injuries to misuse, inadequate local governance, or user error rather than to structural design choices. They lack recourse against developers and are told the remedy lies in better local procedure.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, affected_populations, payer,
    powerless, immediate, trapped, local).

% Observe and critique whether the instrumental-subsidiarity framework genuinely protects human dignity or merely proceduralizes harm. They document the divergence between the framework's claims and its material effects on vulnerable populations.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, civil_society_theologians, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__instrumental_subsidiarity, diffuse).
narrative_ontology:fixing_cost_class(ai_human_relationship__instrumental_subsidiarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a governance framework for AI development that assigns responsibility to users and regulators rather than creators, enabling continued technological innovation while promising human oversight through legal and ethical procedures.
% TRANSFER_FUNCTION: Moves moral and legal responsibility for AI harms from developers to users and local governance bodies; moves regulatory authority and compliance revenue to state bureaucracies and ethics professionals.
% ABSENT_VOICES: Communities experiencing structural harm from AI systems whose injuries are attributed to misuse rather than design; theologians arguing that technology is never neutral because it always embodies particular anthropologies and power relations; future generations who cannot contest the framing.
% DISAPPEARANCE_RATIONALE: If the instrumental-subsidiarity framing disappeared, regulatory authority would recentralize or dissolve, developers would face direct liability for system design, and the compliance industry around AI ethics would shrink. The distribution of responsibility and power across these seats would reorganize around either pure market optimization or stronger substantive constraints.
% FOUNDING_PROBLEM: Rapid AI development without governance mechanisms, risking harms to human dignity, social cohesion, and the common good; the need for a framework that permits innovation while protecting human ends.
% FOUNDING_PROBLEM_CORROBORATION: Catholic social teaching magisterium and some regulatory bodies attest the founding problem is live. Critics from affected communities and incarnational-humanist theologians attest the problem has mutated: the current arrangement no longer solves the founding problem but instead proceduralizes harm. Independent civil society organizations outside the benefiting parties document ethics washing and responsibility diffusion.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__instrumental_subsidiarity, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because the neutral-tool framing systematically diffuses responsibility from those who design AI systems to those who use them or govern them, constituting a structural transfer of liability and moral burden. Suppression (0.58) is moderate-high: the framing suppresses structural critique by treating harms as implementation failures solvable by more procedure, and by invoking subsidiarity to localize blame. Theater_ratio (0.55) reflects the significant performative component of AI ethics and transparency requirements that create visible compliance without altering core system logic. Accessibility_collapse (0.48) is moderate: alternative framings (incarnational humanism, direct developer liability) exist but are marginalized in policy discourse. Resistance (0.52) captures growing civil society and theological critique of ethics-washing and proceduralism. The temporal series show extraction and theater rising together as the governance framework matured, indicating Goodhart drift toward compliance theater.
 *
 * PERSPECTIVAL GAP:
 *   The regulatory bureaucracy seat experiences this constraint as genuine coordination: it solves the problem of ungoverned AI by providing a framework for oversight. The affected populations seat experiences the same structure as extraction: their injuries are routed through local complaint procedures and attributed to inadequate use or governance, never to design. The developers seat experiences subsidy: the neutral-tool premise removes them from the causal chain of harm. These divergences are structurally determined by each seat's directionality toward the constraint, not by disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (ai_developers, regulatory_bureaucracies, legal_ethics_professionals) sit near the low-d end: the constraint subsidizes their positions by diffusing liability, expanding jurisdiction, or creating professional markets. The payer (affected_populations) sits near high-d: the constraint extracts by localizing blame and removing structural recourse. Civil society theologians sit near d=0.5 as symmetric observers: they neither collect nor pay directly, but analyze the structure. No overrides are needed because the structural derivation from beneficiary/victim declarations plus exit options captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two errors. First, it prevents reading the governance framework as pure rope (coordination without extraction): the responsibility diffusion to affected populations is not a side effect but a structural feature of the neutral-tool framing. Second, it prevents reading the arrangement as pure snare (extraction without coordination): the governance framework does solve real coordination problems around safety standards, interoperability, and cross-border accountability, even as it extracts. The scaffold classification is rejected because no credible sunset clause existsâthe regulatory apparatus is self-perpetuating. The piton classification is rejected because concentrated beneficiaries (developers, regulators, professionals) actively maintain and expand the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_neutrality_ambiguity,
    'Is the claim that AI is a morally neutral tool a genuine metaphysical insight about artifacts, or a constructed framing that diffuses responsibility from developers to users and regulators?',
    'Comparative analysis of liability regimes: if neutral-tool jurisdictions show systematically lower developer liability and higher user liability for identical harms, the framing functions as responsibility diffusion.',
    'If responsibility diffusion, extractiveness is higher than the coordination framing suggests; if genuine metaphysical insight, the extraction is the price of correct ontology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_neutrality_ambiguity, conceptual, 'Ambiguity over whether technology neutrality is ontological claim or responsibility mechanism').

omega_variable(
    subsidiarity_inversion_risk,
    'Does the subsidiarity principle in AI governance empower local communities against centralized power, or does it invert subsidiarity by assigning oversight to the level least capable of resisting corporate capture?',
    'Cross-jurisdictional comparison of AI governance outcomes: localities with strong civil society versus those with weak capacity, measuring whether subsidiarity produces better protection or regulatory arbitrage.',
    'If inversion is dominant, the constraint extracts from local communities by assigning them impossible oversight burdens; if empowerment is dominant, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_inversion_risk, empirical, 'Empirical test of whether subsidiarity empowers or burdens local communities').

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the instrumental_subsidiarity reading of the ai_human_relationship kernel. How would the structural classification change if the incarnational_humanism reading were adopted instead?',
    'Comparative structural analysis of the incarnational_humanism reading as a separate constraint story.',
    'The incarnational reading would likely reclassify ai_developers as payers rather than beneficiaries, and would elevate affected_populations to central beneficiaries, potentially shifting the constraint type toward scaffold or rope depending on enforcement requirements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Structural delta between this reading and sibling incarnational humanism reading').

omega_variable(
    procedural_ethics_sufficiency,
    'Can procedural ethics frameworks (transparency, auditing, impact assessments) adequately safeguard human dignity when AI systems encode structurally harmful optimization logic, or do they merely theatricalize extraction?',
    'Longitudinal study comparing jurisdictions with procedural ethics regimes versus substantive design constraints, measuring actual harm reduction.',
    'If procedural ethics is insufficient, theater_ratio understates the performative component; the constraint extracts through ethics theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_ethics_sufficiency, empirical, 'Whether procedural governance actually reduces harm or performs compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_inst_sub_tr_t0, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_inst_sub_tr_t5, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 5, 0.28).
narrative_ontology:measurement(ai_inst_sub_tr_t10, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 10, 0.35).
narrative_ontology:measurement(ai_inst_sub_tr_t15, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 15, 0.42).
narrative_ontology:measurement(ai_inst_sub_tr_t20, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 20, 0.48).
narrative_ontology:measurement(ai_inst_sub_tr_t25, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 25, 0.52).
narrative_ontology:measurement(ai_inst_sub_tr_t30, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(ai_inst_sub_be_t0, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ai_inst_sub_be_t5, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(ai_inst_sub_be_t10, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(ai_inst_sub_be_t15, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(ai_inst_sub_be_t20, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(ai_inst_sub_be_t25, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 25, 0.59).
narrative_ontology:measurement(ai_inst_sub_be_t30, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_inst_sub_su_t0, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_inst_sub_su_t5, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(ai_inst_sub_su_t10, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(ai_inst_sub_su_t15, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(ai_inst_sub_su_t20, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(ai_inst_sub_su_t25, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 25, 0.56).
narrative_ontology:measurement(ai_inst_sub_su_t30, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__technocratic_optimization).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__incarnational_humanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the ai_human_relationship kernel. The instrumental_subsidiarity reading treats technology as neutral and governable through procedural ethics and subsidiarity. It decomposes from the incarnational_humanism reading (which insists on substantive ordering to the common good and preferential option for the poor) and the technocratic_optimization reading (which treats human value as reducible to productivity metrics). Each reading carries a distinct epsilon, beneficiary structure, and directionality profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
