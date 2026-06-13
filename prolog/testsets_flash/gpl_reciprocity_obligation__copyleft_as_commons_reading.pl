% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Commons Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL reciprocity obligation, from the 'copyleft as commons'
 *   perspective, functions as an institutional technology designed to prevent
 *   the enclosure of the software commons. It mandates that any derivative
 *   work distributed must also be licensed under the GPL, ensuring that
 *   improvements and modifications remain part of the shared resource. This
 *   reading emphasizes the collective benefit of a perpetually expanding
 *   commons, viewing the 'viral' nature of the license not as a restriction
 *   on freedom, but as a mechanism for maintaining a shared pool of resources
 *   against individual attempts to privatize collective effort.
 *
 * KEY AGENTS:
 *   - software_commons: Primary beneficiary (institutional/arbitrage) — benefits from expansion and protection
 *   - proprietary_integrators: Primary victim (powerful/constrained) — bears the cost of mandatory reciprocity
 *   - downstream_users: Secondary beneficiary (moderate/mobile) — benefits from access to free and open software
 *   - exit_maximizers: Secondary victim (moderate/constrained) — bears the cost of inability to privatize contributions
 *   - free_software_foundation: Agenda setter (institutional/analytical) — administers and defends the GPL
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.45).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.6).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Reciprocity Obligation (Copyleft as Commons Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, '99f271dd-ea4d-4f29-b20f-1ce447f60cf1').
narrative_ontology:cs_kernel_codification('99f271dd-ea4d-4f29-b20f-1ce447f60cf1', fixed_text).
narrative_ontology:cs_authority_grounding('99f271dd-ea4d-4f29-b20f-1ce447f60cf1', lineage).
narrative_ontology:cs_interpretation_layer_present('99f271dd-ea4d-4f29-b20f-1ce447f60cf1').
narrative_ontology:cs_reading_relation('99f271dd-ea4d-4f29-b20f-1ce447f60cf1', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('99f271dd-ea4d-4f29-b20f-1ce447f60cf1', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_axiom('99f271dd-ea4d-4f29-b20f-1ce447f60cf1', foundational, collective_code_is_common_property).
narrative_ontology:cs_axiom_status(collective_code_is_common_property, holdable).
narrative_ontology:cs_axiom_grounding('99f271dd-ea4d-4f29-b20f-1ce447f60cf1', collective_code_is_common_property, deontological).
narrative_ontology:cs_axiom('99f271dd-ea4d-4f29-b20f-1ce447f60cf1', foundational, reciprocity_prevents_enclosure).
narrative_ontology:cs_axiom_status(reciprocity_prevents_enclosure, holdable).
narrative_ontology:cs_axiom_grounding('99f271dd-ea4d-4f29-b20f-1ce447f60cf1', reciprocity_prevents_enclosure, empirically_contingent).
narrative_ontology:cs_reference_frame('99f271dd-ea4d-4f29-b20f-1ce447f60cf1', perpetual_commons_expansion).
narrative_ontology:cs_drift_state('99f271dd-ea4d-4f29-b20f-1ce447f60cf1', contemporary_commercial_integration, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('99f271dd-ea4d-4f29-b20f-1ce447f60cf1', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_users).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, exit_maximizers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective body of GPL-licensed software and its associated community. It benefits from the mandatory reciprocity that ensures contributions remain part of the shared resource, preventing privatization and enclosure.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_non_agent(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons).

% Commercial entities that wish to incorporate GPL-licensed software into proprietary products without releasing their own modifications under the GPL. They bear the cost of the reciprocity obligation, either by avoiding GPL code or by complying and foregoing proprietary control over their derivatives.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Individuals and organizations who use GPL-licensed software. They benefit from the availability of high-quality, free, and open-source software, and the assurance that its future development will remain open.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_users, beneficiary,
    moderate, biographical, mobile, global).

% Developers or companies who contribute to GPL projects but would prefer to privatize their specific contributions or derivative works for commercial gain. They are prevented from 'exiting' the commons with their improvements due to the reciprocity clause.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, exit_maximizers, payer,
    moderate, immediate, constrained, global).

% The primary institutional body responsible for drafting, promoting, and defending the GPL. They actively enforce the license to ensure its terms are met, acting as stewards of the software commons.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, free_software_foundation, agenda_setter,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the collective development and maintenance of a shared software commons by ensuring that all derivative works remain open and accessible, preventing individual actors from privatizing collective effort.
% TRANSFER_FUNCTION: Transfers the 'right to privatize' derivative works from individual developers/companies back to the software commons, ensuring that all modifications and improvements are shared under the same reciprocal terms.
% ABSENT_VOICES: Proprietary software companies and developers who prioritize maximum control over their intellectual property are structurally excluded from integrating GPL-licensed code into their proprietary stacks without significant legal and business model adjustments. They would argue for more permissive licensing models.
% DISAPPEARANCE_RATIONALE: If the GPL reciprocity obligation vanished, the software commons would likely fragment as individual actors privatized their contributions, leading to a reduction in the shared pool of open-source software and a shift towards more proprietary ecosystems. The institutional technology for maintaining the commons would be gone.
% FOUNDING_PROBLEM: The problem of software enclosure, where collective efforts in software development were privatized by individual actors, leading to a 'tragedy of the commons' for shared codebases.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and many open-source advocates attest that the threat of enclosure remains live, citing ongoing commercial pressures and legal challenges to open-source principles. Independent legal scholars and economists also acknowledge the persistent tension between open and proprietary models, corroborating the ongoing relevance of the founding problem.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Rope because its primary function is to coordinate collective action (maintaining the commons) for the benefit of all participants, even if it imposes costs on those who wish to privatize. Extractiveness (0.45) is moderate: it extracts the 'right to privatize' from those who would enclose the commons. Suppression (0.6) is also moderate, as it actively suppresses proprietary integration through legal enforcement. Theater ratio is low (0.1) because the enforcement directly serves the stated function of maintaining the commons, with little performative overhead. Accessibility collapse is moderate (0.4) as alternatives (other licenses, proprietary development) exist, but the GPL's 'viral' nature makes it difficult to integrate GPL-licensed code into proprietary projects without adopting the GPL.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'software_commons' and 'downstream_users', the GPL is a beneficial coordination mechanism. However, 'proprietary_integrators' and 'exit_maximizers' experience it as a restrictive and extractive force. The engine's per-seat classification will reflect this divergence based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'software_commons' is a full beneficiary (d=0.0) as the constraint directly ensures its growth and protection. 'Downstream_users' are also beneficiaries (d=0.1) as they gain access to a rich ecosystem. 'Proprietary_integrators' are targets (d=0.9) as they are directly prevented from enclosing the commons. 'Exit_maximizers' are also targets (d=0.8) as their ability to privatize contributions is curtailed. The 'free_software_foundation' acts as an agenda setter, enforcing the rules for the benefit of the commons.
 *
 * MANDATROPHY ANALYSIS:
 *   The GPL's mandate to prevent commons enclosure remains highly relevant in the context of ongoing attempts to privatize open-source contributions. The classification as a Rope (or potentially Tangled Rope from some seats) prevents mislabeling it as a pure Snare, acknowledging its genuine coordination function while also recognizing the costs it imposes on specific actors. The 'contested' status of the founding problem reflects the ongoing debate about whether the threat of enclosure is still as severe as it was at GPL's inception, or if the license has become overly restrictive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gpl_kernel_reading_identity,
    'Is this constraint primarily understood as preventing commons enclosure, preserving user freedom, or restricting business models?',
    'Analysis of legal precedent, developer discourse, and economic impact studies focusing on the primary effect of GPL enforcement.',
    'If primarily understood as preserving user freedom, the classification might shift towards a purer ''Rope'' for individual users. If primarily seen as restricting business models, it might lean towards ''Tangled Rope'' or ''Snare'' for commercial entities. This reading (copyleft_as_commons_reading) emphasizes the institutional technology aspect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gpl_kernel_reading_identity, conceptual, 'This constraint is one reading of the ''gpl_reciprocity_obligation'' kernel, specifically the ''copyleft_as_commons_reading''. Sibling readings include ''copyleft_as_freedom_reading'' and ''copyleft_as_restriction_reading''. The disagreement is located in the primary normative justification and perceived impact of the reciprocity obligation.').

omega_variable(
    enforcement_cost_vs_benefit,
    'Does the cost of enforcing GPL compliance outweigh the benefit of preventing commons enclosure for the software commons?',
    'Empirical study of legal costs, developer time spent on compliance, and the economic value of contributions retained within the commons due to GPL.',
    'If enforcement costs are disproportionately high, the constraint''s ''theater_ratio'' might be higher, or its ''extractiveness'' might be re-evaluated as less efficient coordination. If benefits are clear and substantial, it reinforces the ''Rope'' or ''Tangled Rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_vs_benefit, empirical, 'Assessing the efficiency of GPL as an institutional technology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl__be_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(gpl__be_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gpl__su_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(gpl__su_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_as_restriction_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a family of readings of the GPL reciprocity obligation. Each reading emphasizes a different normative justification and structural impact, leading to distinct ε values and classifications. They are linked to reflect their shared origin in the GPL kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
