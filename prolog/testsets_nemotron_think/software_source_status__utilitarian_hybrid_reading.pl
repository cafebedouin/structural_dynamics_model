% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__utilitarian_hybrid_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: software_source_status__utilitarian_hybrid_reading
 *   human_readable: Utilitarian Hybrid Licensing Policy: Context-Dependent Welfare Maximization
 *   domain: economic/technological/political
 *
 * SUMMARY:
 *   The utilitarian hybrid reading of software source status proposes that
 *   licensing should be decided by aggregate welfare maximization in each
 *   context, not by categorical moral or property claims. This constraint
 *   story analyzes the policy framework that would instantiate this reading:
 *   a governance regime where infrastructure (OS, runtimes, protocols)
 *   defaults open because openness maximizes welfare there, while specialized
 *   high-R&D tools (EDA, CAD, scientific simulation) may legitimately remain
 *   proprietary because the welfare calculus favors sustainable investment
 *   over open access. The constraint is a tangled rope: it genuinely
 *   coordinates by providing a neutral decision criterion that escapes the
 *   free/proprietary trench warfare, but it also extracts asymmetrically —
 *   proprietary infrastructure vendors lose lock-in rents, while specialized
 *   open-source maintainers face proprietary competition justified by the
 *   same calculus. Active enforcement is required (policy authorities must
 *   adjudicate welfare claims case by case). No categorical victim class
 *   exists; victimhood is context-dependent and reciprocal.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.28).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.18).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Utilitarian Hybrid Licensing Policy: Context-Dependent Welfare Maximization").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "economic/technological/political").

domain_priors:requires_active_enforcement(software_source_status__utilitarian_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, 'c1886853-7edd-43e6-8429-957dd435921d').
narrative_ontology:cs_kernel_codification('c1886853-7edd-43e6-8429-957dd435921d', formalized).
narrative_ontology:cs_authority_grounding('c1886853-7edd-43e6-8429-957dd435921d', expertise).
narrative_ontology:cs_interpretation_layer_present('c1886853-7edd-43e6-8429-957dd435921d').
narrative_ontology:cs_reading_relation('c1886853-7edd-43e6-8429-957dd435921d', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1886853-7edd-43e6-8429-957dd435921d', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('c1886853-7edd-43e6-8429-957dd435921d', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('c1886853-7edd-43e6-8429-957dd435921d', foundational, aggregate_welfare_maximization).
narrative_ontology:cs_axiom_status(aggregate_welfare_maximization, holdable).
narrative_ontology:cs_axiom_grounding('c1886853-7edd-43e6-8429-957dd435921d', aggregate_welfare_maximization, empirically_contingent).
narrative_ontology:cs_axiom('c1886853-7edd-43e6-8429-957dd435921d', secondary, context_dependent_licensing_optimality).
narrative_ontology:cs_axiom_status(context_dependent_licensing_optimality, holdable).
narrative_ontology:cs_axiom_grounding('c1886853-7edd-43e6-8429-957dd435921d', context_dependent_licensing_optimality, instrumental).
narrative_ontology:cs_reference_frame('c1886853-7edd-43e6-8429-957dd435921d', welfare_optimization_framework).
narrative_ontology:cs_drift_state('c1886853-7edd-43e6-8429-957dd435921d', contemporary_policy_debate, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c1886853-7edd-43e6-8429-957dd435921d', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, software_users).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, open_source_infrastructure_maintainers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, specialized_proprietary_tool_vendors).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, proprietary_infrastructure_vendors).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, specialized_open_source_maintainers).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, welfare_economics_applied_to_ip).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, contextual_licensing_optimality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience licensing terms across all software they use. Benefit when infrastructure is open (interoperability, no vendor lock-in, community support) and when specialized tools are proprietary (sustained investment, professional support, advanced features). Can switch between ecosystems but face switching costs.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, software_users, beneficiary,
    organized, biographical, mobile, global).

% Build and maintain foundational software (OS kernels, language runtimes, databases, web servers). Gain from policy favoring open licensing for infrastructure: network effects, contributor inflow, institutional adoption. Also help set norms through foundation governance. Can move projects between licenses but community fragmentation is a real cost.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, open_source_infrastructure_maintainers, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, open_source_infrastructure_maintainers, agenda_setter).

% Sell complex domain-specific tools (EDA, CAD, simulation, specialized analytics). Policy justifies their proprietary model as welfare-maximizing for high-R&D, narrow-audience tools where open source cannot sustain investment. Benefit from legitimate proprietary space but face pressure to prove welfare case. Exit constrained by specialized market position.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, specialized_proprietary_tool_vendors, beneficiary,
    powerful, biographical, constrained, global).

% Currently extract rents from infrastructure lock-in (OS, cloud platforms, middleware). Policy shifts infrastructure toward open licensing, threatening their extraction model. Must either open-source infrastructure layers or justify proprietary model via welfare argument — difficult for commoditized infrastructure. Exit constrained by legacy installed base and enterprise contracts.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, proprietary_infrastructure_vendors, payer,
    institutional, generational, constrained, global).

% Maintain open-source tools in specialized domains (scientific computing, niche creative tools). Policy may justify proprietary alternatives where open source cannot sustain quality, creating competitive pressure. Lose contributor mindshare and funding to proprietary rivals. Exit constrained by domain expertise and community ties.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, specialized_open_source_maintainers, payer,
    moderate, biographical, constrained, global).

% Government bodies (competition authorities, standards agencies, research funders) that implement welfare-based licensing policy. Commission economic analyses, set procurement rules, enforce competition law. Their decisions determine which contexts favor which model. Not directly extracting but shape the playing field.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, policy_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Hold that software freedom is a non-negotiable ethical requirement; any proprietary licensing is injustice. Excluded from utilitarian calculus because their objection is deontological, not welfare-based. Would object to any policy that legitimizes proprietary software. Identity-locked: their self-concept is constituted by this commitment.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, freedom_imperative_advocates, excluded,
    organized, civilizational, identity_locked, global).

% Hold that creators have absolute right to restrict access/modification; any mandatory openness is theft. Excluded because utilitarian calculus can override property claims for welfare gains. Would object to policy that favors open infrastructure. Identity-locked: worldview fuses property rights with creative legitimacy.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, property_rights_advocates, excluded,
    organized, civilizational, identity_locked, global).

% Provide the analytical framework: measure consumer/producer surplus, innovation incentives, deadweight loss from monopoly vs. underinvestment from weak appropriability. Their models inform but don't dictate policy. See full structure; no skin in the game.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, welfare_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the fragmentation of software licensing into ideological camps by providing a single decision criterion: choose the licensing model that maximizes aggregate welfare in each context. Replaces moral absolutism with context-sensitive optimization.
% TRANSFER_FUNCTION: Moves licensing authority from ideological defaults (always-open or always-proprietary) to context-dependent welfare calculation. In infrastructure contexts, transfers control from proprietary vendors to open ecosystems. In specialized-tool contexts, transfers legitimacy from open-by-default to proprietary-by-justification. No single directional transfer; the transfer vector flips by domain.
% ABSENT_VOICES: Freedom-imperative advocates (deontological objectors to any proprietary) and property-rights advocates (deontological objectors to any mandated openness) are structurally excluded — their objections are categorical, not welfare-conditional, so the utilitarian framework has no seat for them. They remain active in public discourse but outside the policy calculus.
% DISAPPEARANCE_RATIONALE: If welfare-based licensing policy vanished, the field would revert to the current ideological trench warfare: infrastructure stays proprietary-by-default (vendor lock-in persists), specialized tools face pressure to open-source (undermining sustainable R&D), and no neutral criterion exists to resolve disputes. The mixed ecosystem equilibrium would collapse to whichever ideology captures the next policy cycle.
% FOUNDING_PROBLEM: Software licensing polarized into two absolutist camps: free software fundamentalism (all software must be free) and proprietary maximalism (all code is property). This produced systematically suboptimal outcomes — infrastructure locked down despite massive social gains from openness, and specialized tools underfunded because open-source purity prevented sustainable business models. The utilitarian hybrid was proposed to escape this false dichotomy.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of software (e.g., Lerner & Tirole, von Krogh) document the polarization and its welfare costs from outside the benefiting parties. The FSF and proprietary trade associations each attest the problem is misdiagnosed — FSF says the problem is insufficient freedom, trade associations say the problem is insufficient property protection. No neutral third party corroborates the utilitarian framing as the *correct* resolution; the status remains contested.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__utilitarian_hybrid_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__utilitarian_hybrid_reading_tests).
:- end_tests(software_source_status__utilitarian_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.28) because the constraint primarily coordinates — it replaces ideological defaults with a decision procedure. The extraction that exists is the asymmetric loss: proprietary infrastructure vendors lose extraction rents (their victimhood), while specialized open-source maintainers lose contributor/funder mindshare to proprietary alternatives (their victimhood). Suppression is low (0.18) because the constraint doesn't ban models; it just shifts burden of proof. Theater is low (0.12) because the welfare calculus is genuinely used in policy (EU interoperability rules, US government open-source mandates, foundation governance). Accessibility collapse is moderate (0.35) because ideological alternatives remain live and vocal. Resistance is moderate (0.42) because both absolutist camps actively resist the utilitarian framework. The measurement grid shows gradual creep in all three metrics as the policy regime matures and edge cases harden into precedents.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (proprietary infrastructure vendors, specialized open-source maintainers) experience this as extraction — their established models are disrupted by a calculus they reject. The beneficiary seats (users, open infrastructure maintainers, specialized proprietary vendors) experience it as coordination — a rational framework replacing ideological gridlock. The agenda-setter seat (policy authorities) experiences it as administrative burden — case-by-case welfare adjudication is costly and politically fraught. The engine computes this divergence from the structural data; the claimed type (tangled_rope) reflects the author's judgment that both coordination and extraction are genuinely present.
 *
 * DIRECTIONALITY LOGIC:
 *   Software users and open-source infrastructure maintainers are structural beneficiaries (d near 0.1-0.2): they gain coordination value and avoid lock-in. Specialized proprietary vendors are beneficiaries in their domain (d ~0.2) but payers in infrastructure contexts. Proprietary infrastructure vendors are payers (d ~0.8): the constraint directly targets their extraction model. Specialized open-source maintainers are payers in their domain (d ~0.7): the constraint legitimates proprietary competition. Policy authorities sit near symmetric (d ~0.5): they administer the calculus but don't personally extract. Excluded ideological advocates are identity-locked (d ~0.9 for both): the constraint's legitimacy condition (welfare maximization) directly contradicts their constitutive commitments. Observers (welfare economists) are analytical (d=0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ideological polarization producing systematically suboptimal licensing) remains contested — not dead, not universally accepted as live. The mandate has not atrophied because the polarization persists and the utilitarian middle is still the only framework that escapes it. But the mandate is not resolved either: welfare calculation is notoriously manipulable, and both ideological camps continuously attempt to capture the calculus. The constraint persists as a live contested framework, not a settled arrangement. Mandatrophy is not resolved; the constraint continues to do work (coordination) while also extracting (asymmetric burden-shifting).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the utilitarian_hybrid_reading instantiate a single coherent constraint, or does it fracture into context-specific constraints (one for infrastructure, one for specialized tools) with different ε values?',
    'Empirical: examine whether policy adjudication uses a unified welfare function across contexts or distinct criteria per domain. If distinct criteria, the ε-invariance principle requires decomposition into separate constraint stories.',
    'If the reading fractures, each sub-constraint gets its own ε, beneficiaries, victims, and classification. The current single-story authoring would be an ε-invariance violation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the utilitarian hybrid is one constraint or a constraint family.').

omega_variable(
    welfare_calculus_manipulability,
    'How vulnerable is the welfare-maximization criterion to strategic manipulation by powerful actors (regulatory capture of the adjudication process)?',
    'Historical analysis of policy outcomes: track whether welfare justifications systematically favor incumbents with lobbying capacity vs. genuinely optimal outcomes.',
    'If highly manipulable, the constraint''s effective extraction is higher than authored — the coordination function becomes cover for capture. Reclassification toward snare becomes plausible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_calculus_manipulability, empirical, 'Strategic vulnerability of the welfare adjudication mechanism.').

omega_variable(
    context_boundary_definition,
    'Who defines the boundary between ''infrastructure'' (favors open) and ''specialized tools'' (may favor proprietary), and is that boundary itself a site of extraction?',
    'Case study of boundary disputes (e.g., is Kubernetes infrastructure or a specialized orchestration tool? Is a game engine infrastructure or specialized?). Trace which actors control the categorization.',
    'If boundary control concentrates extraction, the constraint has a hidden snare layer. The current authoring treats boundary as analytical; if it''s political, victims shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_boundary_definition, conceptual, 'Whether the context boundary is a neutral analytical category or a political lever.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 1998, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1998, software_source_status__utilitarian_hybrid_reading, theater_ratio, 1998, 0.08).
narrative_ontology:measurement(soft_tr_t2003, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2003, 0.09).
narrative_ontology:measurement(soft_tr_t2008, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(soft_tr_t2013, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2013, 0.11).
narrative_ontology:measurement(soft_tr_t2018, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2018, 0.11).
narrative_ontology:measurement(soft_tr_t2025, software_source_status__utilitarian_hybrid_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(soft_be_t1998, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 1998, 0.15).
narrative_ontology:measurement(soft_be_t2003, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2003, 0.18).
narrative_ontology:measurement(soft_be_t2008, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2008, 0.22).
narrative_ontology:measurement(soft_be_t2013, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2013, 0.25).
narrative_ontology:measurement(soft_be_t2018, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2018, 0.27).
narrative_ontology:measurement(soft_be_t2025, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1998, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 1998, 0.12).
narrative_ontology:measurement(soft_su_t2003, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2003, 0.14).
narrative_ontology:measurement(soft_su_t2008, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2008, 0.16).
narrative_ontology:measurement(soft_su_t2013, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2013, 0.17).
narrative_ontology:measurement(soft_su_t2018, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2018, 0.18).
narrative_ontology:measurement(soft_su_t2025, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 2025, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__utilitarian_hybrid_reading, 0.15).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__property_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the software_source_status kernel. The kernel decomposes into four constraint stories (this one plus three siblings) because each reading produces a structurally distinct constraint with different ε, beneficiaries, victims, and type. The utilitarian hybrid reading produces a tangled_rope with context-dependent victims; the freedom imperative produces a snare (categorical victims: proprietary vendors/users); the pragmatic development reading produces a rope (coordination via superior methodology); the property rights reading produces a mountain claim (natural property right) that likely triggers FSM as false summit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_source_status__utilitarian_hybrid_reading, institutional, 0.75).
constraint_indexing:directionality_override(software_source_status__utilitarian_hybrid_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
