% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Integrated AI Alignment Priority Framework
 *   domain: technology_ethics/ai_governance
 *
 * SUMMARY:
 *   The integrated reading of AI alignment priority emerged around 2022-2023
 *   as a synthesis attempt between two polarized governance camps: the
 *   existential risk community (prioritizing catastrophic loss of control)
 *   and the algorithmic justice community (prioritizing present
 *   discriminatory harms). The reading claims these are complementary — that
 *   red-teaming catches both bias and deception, that audit infrastructure
 *   serves both, that talent and funding can be shared. Structurally, it
 *   operates as a resource allocation framework that mediates between
 *   competing victim claims (present marginalized groups vs future
 *   populations) and competing beneficiary claims (labs wanting deployment
 *   freedom vs governance bodies wanting expanded mandate). The constraint is
 *   actively enforced through funding requirements (e.g., US executive order
 *   mandating both red-teaming and bias audits), international standards (ISO
 *   42001 covering both risk categories), and institutional mandates (AI
 *   Safety Institutes with dual portfolios).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.45).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.4).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Integrated AI Alignment Priority Framework").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "technology_ethics/ai_governance").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, '7d18afda-b87c-44e2-b3f4-36d05af9d8de').
narrative_ontology:cs_kernel_codification('7d18afda-b87c-44e2-b3f4-36d05af9d8de', distributed).
narrative_ontology:cs_authority_grounding('7d18afda-b87c-44e2-b3f4-36d05af9d8de', distributed).
narrative_ontology:cs_reading_relation('7d18afda-b87c-44e2-b3f4-36d05af9d8de', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d18afda-b87c-44e2-b3f4-36d05af9d8de', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_axiom('7d18afda-b87c-44e2-b3f4-36d05af9d8de', foundational, risk_categories_are_complementary).
narrative_ontology:cs_axiom_status(risk_categories_are_complementary, holdable).
narrative_ontology:cs_axiom_grounding('7d18afda-b87c-44e2-b3f4-36d05af9d8de', risk_categories_are_complementary, instrumental).
narrative_ontology:cs_axiom('7d18afda-b87c-44e2-b3f4-36d05af9d8de', foundational, shared_infrastructure_serves_both_risks).
narrative_ontology:cs_axiom_status(shared_infrastructure_serves_both_risks, holdable).
narrative_ontology:cs_axiom_grounding('7d18afda-b87c-44e2-b3f4-36d05af9d8de', shared_infrastructure_serves_both_risks, empirically_contingent).
narrative_ontology:cs_reference_frame('7d18afda-b87c-44e2-b3f4-36d05af9d8de', fragmented_governance_baseline).
narrative_ontology:cs_drift_state('7d18afda-b87c-44e2-b3f4-36d05af9d8de', post_integrated_frame_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7d18afda-b87c-44e2-b3f4-36d05af9d8de', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, ai_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, governance_bodies).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, safety_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, present_marginalized_groups).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, future_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, global_south_ai_practitioners).
narrative_ontology:constraint_vindicates(ai_alignment_priority__integrated_reading, integrated_risk_governance).
narrative_ontology:constraint_vindicates(ai_alignment_priority__integrated_reading, complementary_priorities_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major AI developers (frontier labs) set research priorities and resource allocation for alignment work. They benefit from the integrated framing because it legitimizes their safety investments while preserving deployment freedom. They can redirect capital across risk categories and have exit options through jurisdictional arbitrage and self-governance commitments.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_labs, agenda_setter,
    institutional, generational, arbitrage, global).

% International and national regulatory bodies (e.g., EU AI Office, US AI Safety Institute, UK AISI) administer the integrated framework. They benefit from expanded mandate covering both risk categories, which justifies budget and staffing. Their exit is constrained by treaty obligations and legislative mandates.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, governance_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Technical alignment researchers receive funding and institutional recognition from both existential risk and near-term harm research programs. The integrated framing expands the funding pool. They can move between research agendas and institutions with moderate friction.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, safety_researchers, beneficiary,
    organized, biographical, mobile, global).

% Communities currently harmed by deployed AI systems (algorithmic discrimination, surveillance, labor displacement, environmental costs). They bear the opportunity cost when governance resources shift toward speculative catastrophic risks. Their exit is blocked by structural dependency on AI-mediated systems (credit, employment, healthcare, policing).
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, present_marginalized_groups, payer,
    powerless, biographical, trapped, global).

% Future generations who would bear existential catastrophe if alignment fails. They are represented only by proxy advocates in current governance. They bear the cost of any dilution of catastrophic risk prevention. Exit is structurally impossible — they do not yet exist and cannot consent or dissent.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_populations, payer,
    powerless, civilizational, trapped, universal).

% AI researchers, developers, and civil society actors in the Global South who face both present deployment harms (extraction, digital colonialism) and exclusion from catastrophic risk governance forums. They pay through resource diversion to Northern-defined priorities and lack of representation. Exit is constrained by compute access dependency and publication venue gatekeeping.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, global_south_ai_practitioners, payer,
    moderate, biographical, constrained, global).

% Researchers and funders who prioritize catastrophic risk prevention above all. They are structurally excluded from the integrated framework's resource allocation because their priority demands disproportionate allocation to speculative risks. They maintain independent funding channels (philanthropic) and institutional homes (e.g., FHI, MIRI-aligned orgs).
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, existential_risk_advocates, excluded,
    organized, civilizational, mobile, global).

% Civil rights groups, algorithmic justice organizations, and affected-community advocates who prioritize present harms. They are excluded when integrated frameworks treat their concerns as 'already solved' or subordinate to long-termism. They retain mobilization capacity through legal advocacy, journalism, and grassroots organizing.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, nearterm_harms_advocates, excluded,
    organized, biographical, mobile, global).

% Independent scholars and meta-researchers who study AI governance portfolio allocation across risk categories. They do not collect rents from the constraint nor bear its direct costs. They observe the structural tension between the integrated framing's claimed complementarity and the resource competition it mediates.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates AI governance portfolio allocation across two distinct risk categories (catastrophic loss of control and present deployment harms) by asserting they are complementary rather than competing, enabling shared infrastructure (red-teaming, audits, evals) and joint funding streams.
% TRANSFER_FUNCTION: Moves research funding, policy attention, regulatory capacity, and talent between existential risk mitigation and near-term harm mitigation. Transfers legitimacy from established near-term harm work to speculative catastrophic risk work, and transfers urgency from catastrophic risk narratives to present harm remediation.
% ABSENT_VOICES: Future generations (no representation), Global South communities most exposed to AI extraction (excluded from governance forums), workers displaced by automation (organized labor largely absent from alignment forums), and ecosystems bearing compute environmental costs. These voices would object to resource allocation that treats their harms as secondary to speculative risks.
% DISAPPEARANCE_RATIONALE: If the integrated framing vanished, AI governance would fracture into two competing paradigms: one capturing most resources for catastrophic risk prevention (longtermist governance), the other for present harm remediation (algorithmic justice governance). Funding streams, regulatory mandates, and research agendas would bifurcate, eliminating the shared infrastructure (evals, red-teaming, audit standards) that currently serves both.
% FOUNDING_PROBLEM: Early AI governance (2015-2022) fragmented into two hostile camps: existential risk proponents who treated present harms as distractions, and near-term harm advocates who treated catastrophic risk as sci-fi diversion. This fragmentation wasted scarce governance capacity, duplicated infrastructure, and prevented unified political coalitions for AI regulation.
% FOUNDING_PROBLEM_CORROBORATION: The fragmentation thesis is attested by the existential risk camp (e.g., 2022 FLI pause letter signatories framing near-term work as 'distraction') and the near-term camp (e.g., 2023 ACM FAccT keynote framing longtermism as 'evading accountability'). Independent corroboration comes from OECD AI Policy Observatory (2023) documenting 'parallel governance tracks' and UNESCO (2022) noting 'insufficient bridging' between risk communities. No single party's self-assessment suffices.
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__integrated_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).
:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the integrated framework extracts compliance costs from AI labs (dual audit requirements), diverts advocacy capacity from specialized communities (forcing coalition-building), and imposes opportunity costs on both risk communities (neither gets full priority). Suppression is moderate (0.4) because exit is possible but costly: labs can jurisdiction-shop, advocates can maintain parallel infrastructures, but the integrated frame dominates major funding channels (government, philanthropic). Theater ratio (0.35) reflects performative 'both-sides' rhetoric in policy documents that masks continued resource competition. Accessibility collapse (0.5) and resistance (0.5) are moderate because alternative framings (pure existential, pure near-term) remain live and organized.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (labs, governance bodies), the constraint appears as a Rope: genuine coordination solving fragmentation, with moderate costs shared fairly. From the payer seats (marginalized groups, future populations, Global South), it appears as a Snare: the complementarity rhetoric masks resource diversion to the priorities of powerful actors. From the excluded seats, it appears as a Piton: a degraded compromise that satisfies no one's core demand but persists through institutional inertia. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   AI labs and governance bodies are structural beneficiaries (d ~0.2): they gain legitimacy, expanded mandate, and unified compliance infrastructure. Safety researchers are moderate beneficiaries (d ~0.35): expanded funding but diluted focus. Present marginalized groups are primary targets (d ~0.85): they bear opportunity costs when resources shift to speculative risks, and their exit is trapped by structural dependency. Future populations are targets (d ~0.9) but represented only by proxies — their extraction is opportunity cost of dilution. Global South practitioners are constrained targets (d ~0.7): they pay both exclusion costs and compliance costs. Excluded advocates (existential and near-term) sit outside the coordination but exert pressure on its boundaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmentation) was real in 2015-2022 but its status is contested: existential risk advocates claim fragmentation persists (near-term work still treated as distraction), near-term advocates claim the integrated frame is capture (longtermism colonizing governance), and integrated advocates claim synthesis is working. The constraint shows mandatrophy signals: the original coordination problem (fragmentation) may be solved, but the arrangement now serves to legitimate dual extraction from both victim sets. Theater ratio rising over the interval suggests Goodhart drift — complementarity metrics become proxy goals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the integrated reading instantiate a genuinely distinct constraint from its sibling readings, or is it a rhetorical synthesis that papers over irreconcilable resource allocation conflicts?',
    'Track actual funding flows and regulatory outputs over 5 years: if integrated frameworks produce joint resource allocation (shared evals, combined budgets), the reading is structurally distinct; if they produce sequential or siloed allocation, the reading is a cover story.',
    'If cover story, the constraint''s ε is higher than measured (extraction hidden by coordination rhetoric) and classification shifts toward snare; if genuine synthesis, ε is accurately moderate and tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether the integrated reading is a real coordination mechanism or a rhetorical fusion of competing constraints.').

omega_variable(
    future_population_representation,
    'Can future populations be meaningfully represented in current governance without extracting from present marginalized groups?',
    'Analyze governance proposals that claim to represent future interests: measure resource diversion from present harm remediation to speculative risk prevention. If diversion correlates with longtermist funding influence, representation is extractive.',
    'If representation is extractive, the victim set ''future_populations'' functions as a moral license for present extraction — the constraint becomes a snare for present_marginalized_groups. If non-extractive representation exists, the dual victim set is structurally coherent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_population_representation, conceptual, 'Whether the dual victim set (present + future) is a genuine coordination achievement or a moral licensing mechanism.').

omega_variable(
    complementarity_measurement,
    'What counts as evidence that catastrophic and present harm prevention are ''complementary'' rather than merely ''both funded''?',
    'Define complementarity operationally: shared methodological infrastructure (red-teaming that catches both bias and deception), shared talent pipelines, shared regulatory triggers. Measure overlap in actual practice, not stated intent.',
    'If complementarity is only nominal (separate programs under one banner), the coordination function is theater and theater_ratio is understated. If structurally real, the tangled_rope classification is warranted with genuine coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_measurement, empirical, 'Operationalizing the claimed complementarity between risk categories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__integrated_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_a_tr_t2, ai_alignment_priority__integrated_reading, theater_ratio, 2, 0.25).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__integrated_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(ai_a_tr_t6, ai_alignment_priority__integrated_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__integrated_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__integrated_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__integrated_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ai_a_be_t2, ai_alignment_priority__integrated_reading, base_extractiveness, 2, 0.32).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__integrated_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(ai_a_be_t6, ai_alignment_priority__integrated_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__integrated_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__integrated_reading, base_extractiveness, 10, 0.45).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ai_alignment_priority__integrated_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__integrated_reading, 0.15).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__nearterm_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form the ai_alignment_priority constraint family. The kernel is the priority question; each reading instantiates a different constraint with different ε, different victim sets, different coordination functions. The integrated reading claims to subsume the siblings' coordination functions; the siblings claim the integrated reading extracts from their constituencies. Network edges represent the structural influence: the integrated reading's resource allocation decisions directly affect the operating conditions of the sibling readings (funding, regulatory attention, talent flows).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__integrated_reading, powerless, 0.85).
constraint_indexing:directionality_override(ai_alignment_priority__integrated_reading, analytical, 0.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
