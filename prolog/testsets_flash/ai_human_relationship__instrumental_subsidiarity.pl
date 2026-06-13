% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__instrumental_subsidiarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_human_relationship__instrumental_subsidiarity
 *   human_readable: AI as Neutral Tool to be Regulated (Instrumental Subsidiarity Reading)
 *   domain: political_theology/technology_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'instrumental subsidiarity' reading of the
 *   AI-human relationship, rooted in Catholic Social Teaching. It posits AI
 *   as a morally neutral tool whose ethical implications arise from its
 *   design, deployment, and governance. The constraint emphasizes human
 *   responsibility in regulating AI through legal and ethical frameworks,
 *   with subsidiarity serving as a procedural safeguard to protect human
 *   dignity and local autonomy. The goal is to ensure technology serves human
 *   ends, rather than dominating them, through proper governance and
 *   transparency.
 *
 * KEY AGENTS:
 *   - regulators: Agenda-setter (institutional/analytical) — defines and enforces ethical and legal boundaries for AI.
 *   - ethical_ai_developers: Beneficiary (organized/mobile) — benefits from clear guidelines and public trust, but must adhere to regulations.
 *   - human_rights_advocates: Beneficiary (organized/analytical) — benefits from the protection of human dignity and ethical frameworks, acts as a watchdog.
 *   - ai_users: Beneficiary (moderate/biographical) — benefits from safer, more ethically aligned AI systems.
 *   - unregulated_ai_actors: Excluded (powerful/arbitrage) — would resist regulation that limits profit or autonomy, but are outside the legitimate conversation of this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.3).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.2).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.3).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI as Neutral Tool to be Regulated (Instrumental Subsidiarity Reading)").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "political_theology/technology_ethics").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, '20b5944d-6a77-4b09-a25c-e452271f1860').
narrative_ontology:cs_kernel_codification('20b5944d-6a77-4b09-a25c-e452271f1860', formalized).
narrative_ontology:cs_authority_grounding('20b5944d-6a77-4b09-a25c-e452271f1860', lineage).
narrative_ontology:cs_interpretation_layer_present('20b5944d-6a77-4b09-a25c-e452271f1860').
narrative_ontology:cs_reading_relation('20b5944d-6a77-4b09-a25c-e452271f1860', ai_human_relationship__technocratic_optimization, coexists_with).
narrative_ontology:cs_reading_relation('20b5944d-6a77-4b09-a25c-e452271f1860', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_axiom('20b5944d-6a77-4b09-a25c-e452271f1860', foundational, technology_is_morally_neutral).
narrative_ontology:cs_axiom_status(technology_is_morally_neutral, holdable).
narrative_ontology:cs_axiom_grounding('20b5944d-6a77-4b09-a25c-e452271f1860', technology_is_morally_neutral, deontological).
narrative_ontology:cs_axiom('20b5944d-6a77-4b09-a25c-e452271f1860', foundational, human_dignity_protected_via_law).
narrative_ontology:cs_axiom_status(human_dignity_protected_via_law, holdable).
narrative_ontology:cs_axiom_grounding('20b5944d-6a77-4b09-a25c-e452271f1860', human_dignity_protected_via_law, conventional).
narrative_ontology:cs_reference_frame('20b5944d-6a77-4b09-a25c-e452271f1860', subsidiarity_as_procedural_safeguard).
narrative_ontology:cs_drift_state('20b5944d-6a77-4b09-a25c-e452271f1860', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('20b5944d-6a77-4b09-a25c-e452271f1860', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, regulators).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, ethical_ai_developers).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, human_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, ai_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Government bodies and international organizations tasked with creating and enforcing laws and ethical guidelines for AI development and deployment. They aim to protect public interest and human rights.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, regulators, agenda_setter,
    institutional, generational, analytical, national).

% Companies and researchers committed to developing AI in alignment with ethical principles and legal frameworks. They benefit from clear rules, public trust, and a level playing field, but bear compliance costs.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ethical_ai_developers, beneficiary,
    organized, biographical, mobile, global).

% Non-governmental organizations and civil society groups that monitor AI development, advocate for human-centric AI, and push for stronger ethical and legal safeguards. They benefit from the existence of regulatory frameworks.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, human_rights_advocates, beneficiary,
    organized, generational, analytical, global).

% Individuals who interact with AI systems in their daily lives. They benefit from AI systems that are designed and deployed with ethical considerations and legal protections, reducing risks of harm or discrimination.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ai_users, beneficiary,
    moderate, biographical, constrained, global).

% Entities that seek to develop and deploy AI without adherence to ethical guidelines or legal restrictions, often prioritizing profit or power. They are excluded from the legitimate discourse of this reading and would resist its enforcement.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, unregulated_ai_actors, excluded,
    powerful, immediate, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__instrumental_subsidiarity, regulators).
narrative_ontology:fixing_cost_class(ai_human_relationship__instrumental_subsidiarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the development and deployment of AI technologies globally, ensuring they align with human values, dignity, and the common good, preventing unchecked technological determinism.
% TRANSFER_FUNCTION: Transfers responsibility for ethical outcomes from individual developers to a collective regulatory and ethical framework, and transfers resources (compliance costs) from developers to oversight bodies, in exchange for public trust and legitimacy.
% ABSENT_VOICES: Actors who believe AI should be entirely unregulated or who prioritize technological advancement and efficiency above all other concerns (e.g., 'technocratic_optimization' proponents) are largely absent from the framing of this constraint. They would argue that regulation stifles innovation and that the market is the best arbiter of ethical use.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, AI development would likely proceed with fewer ethical safeguards, potentially leading to increased harms, discrimination, and a loss of public trust. The relationship between humans and AI would become more technocratic, driven by efficiency and power, rather than human flourishing.
% FOUNDING_PROBLEM: The rapid advancement of AI technology without corresponding ethical and legal frameworks, leading to concerns about human autonomy, privacy, bias, and the potential for AI to undermine human dignity and social cohesion.
% FOUNDING_PROBLEM_CORROBORATION: The problem is widely attested by international bodies (e.g., UN, UNESCO), academic ethicists, and civil society organizations, who consistently highlight ongoing challenges in AI governance and the need for robust ethical frameworks. This corroboration comes from outside the direct beneficiaries of the regulatory structure.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__instrumental_subsidiarity_tests).
:- end_tests(ai_human_relationship__instrumental_subsidiarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it aims for genuine coordination (ethical AI development, human dignity protection) with relatively low inherent extraction. The extractiveness (0.3) is primarily the cost of compliance and oversight. Suppression (0.2) is low, reflecting a reliance on shared ethical principles and legal frameworks rather than overt coercion. Theater ratio (0.1) is low, as the stated purpose of ethical governance is largely aligned with actual practice within this reading. The values reflect an aspirational state where regulation effectively guides AI development.
 *
 * PERSPECTIVAL GAP:
 *   Regulators and human rights advocates would experience this as a beneficial coordination mechanism, ensuring AI serves the common good. Ethical AI developers would see it as a necessary framework for responsible innovation. Unregulated AI actors, however, would perceive it as an extractive and suppressive force limiting their freedom and profit, highlighting the tension between ethical governance and unbridled technological expansion.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulators and human rights advocates are beneficiaries (d near 0.0) as they gain legitimacy and achieve their mission through this framework. Ethical AI developers are also beneficiaries (d near 0.2) as clear rules foster trust and a stable operating environment, despite compliance costs. AI users are diffuse beneficiaries. Unregulated AI actors, though not explicitly victims, are targets of the regulatory force (d near 1.0) as their preferred mode of operation is curtailed.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine efforts at ethical AI governance as pure extraction. By framing AI as a neutral tool, it avoids the pitfall of either uncritical embrace (technocratic_optimization) or outright rejection (which might be a feature of some incarnational_humanism readings if they become Luddite). The focus on regulation and subsidiarity aims to keep the constraint adaptive and responsive to evolving technological realities, preventing mandatrophy by ensuring its mandate remains relevant to human flourishing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine instantiation of the ''instrumental_subsidiarity'' reading of the ''ai_human_relationship'' kernel, or does it implicitly lean towards ''technocratic_optimization''?',
    'Analysis of regulatory outcomes: if regulations prioritize efficiency over human dignity safeguards, it indicates a drift towards ''technocratic_optimization''.',
    'If it leans towards ''technocratic_optimization'', the effective extractiveness and suppression would be higher, reclassifying it towards a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity in the practical application of instrumental subsidiarity versus efficiency.').

omega_variable(
    subsidiarity_enforcement_efficacy,
    'How effectively can the principle of subsidiarity be enforced in complex, global AI development and deployment, given the power asymmetries?',
    'Empirical study of AI governance models: assess the actual decentralization of decision-making and protection of local autonomy in AI systems.',
    'If subsidiarity proves difficult to enforce, the constraint''s ability to protect human dignity is weakened, increasing potential for extraction and suppression, pushing it towards a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subsidiarity_enforcement_efficacy, empirical, 'Efficacy of subsidiarity as a safeguard in AI governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 2020, 2040).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(ai_h_be_t2020, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2020, 0.25).
narrative_ontology:measurement(ai_h_be_t2025, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2025, 0.28).
narrative_ontology:measurement(ai_h_be_t2030, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2030, 0.3).
narrative_ontology:measurement(ai_h_be_t2035, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2035, 0.32).
narrative_ontology:measurement(ai_h_be_t2040, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2040, 0.33).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t2020, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2020, 0.15).
narrative_ontology:measurement(ai_h_su_t2025, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2025, 0.18).
narrative_ontology:measurement(ai_h_su_t2030, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2030, 0.2).
narrative_ontology:measurement(ai_h_su_t2035, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2035, 0.22).
narrative_ontology:measurement(ai_h_su_t2040, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2040, 0.23).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_human_relationship__instrumental_subsidiarity, 0.1).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_human_relationship' kernel, focusing on AI as a neutral tool to be regulated. Other readings, such as 'technocratic_optimization' and 'incarnational_humanism', represent different structural claims about AI's nature and its relationship to humanity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
