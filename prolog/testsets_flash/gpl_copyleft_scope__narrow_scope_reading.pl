% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__narrow_scope_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Copyleft Scope (Narrow Interpretation)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents a 'narrow scope' interpretation of GPL Section
 *   2(b), asserting that copyleft obligations extend only to direct
 *   derivative works, excluding mere aggregation, plugin architectures, and
 *   certain dynamic linking forms. This reading aligns with traditional
 *   copyright doctrine's emphasis on substantial similarity and direct
 *   derivation. It facilitates the integration of GPL-licensed components
 *   into larger, often proprietary, software systems, providing a
 *   coordination mechanism for mixed codebases. However, it weakens the
 *   expectations of strong copyleft advocates for universal code-sharing and
 *   makes enforcement against more subtle forms of code coupling rare.
 *
 * KEY AGENTS:
 *   - commercial_software_firms: Primary beneficiary (powerful/mobile) — retains flexibility for proprietary layers.
 *   - hybrid_software_developers: Beneficiary (moderate/mobile) — can mix licenses with less friction.
 *   - strong_copyleft_advocates: Primary victim (organized/constrained) — sees copyleft weakened.
 *   - open_source_foundations: Agenda setter (institutional/constrained) — administers GPL, but interpretation is contested.
 *   - software_users: Beneficiary (moderate/mobile) — benefits from broader software availability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.35).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.25).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Copyleft Scope (Narrow Interpretation)").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__narrow_scope_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, 'abb6fd1b-1772-41b8-8fdf-3bf053fe39e3').
narrative_ontology:cs_kernel_codification('abb6fd1b-1772-41b8-8fdf-3bf053fe39e3', fixed_text).
narrative_ontology:cs_authority_grounding('abb6fd1b-1772-41b8-8fdf-3bf053fe39e3', distributed).
narrative_ontology:cs_reading_relation('abb6fd1b-1772-41b8-8fdf-3bf053fe39e3', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('abb6fd1b-1772-41b8-8fdf-3bf053fe39e3', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('abb6fd1b-1772-41b8-8fdf-3bf053fe39e3', foundational, derivative_work_follows_traditional_copyright).
narrative_ontology:cs_axiom_status(derivative_work_follows_traditional_copyright, holdable).
narrative_ontology:cs_axiom_grounding('abb6fd1b-1772-41b8-8fdf-3bf053fe39e3', derivative_work_follows_traditional_copyright, conventional).
narrative_ontology:cs_axiom('abb6fd1b-1772-41b8-8fdf-3bf053fe39e3', secondary, mere_aggregation_is_not_derivation).
narrative_ontology:cs_axiom_status(mere_aggregation_is_not_derivation, holdable).
narrative_ontology:cs_axiom_grounding('abb6fd1b-1772-41b8-8fdf-3bf053fe39e3', mere_aggregation_is_not_derivation, conventional).
narrative_ontology:cs_reference_frame('abb6fd1b-1772-41b8-8fdf-3bf053fe39e3', traditional_copyright_framework).
narrative_ontology:cs_drift_state('abb6fd1b-1772-41b8-8fdf-3bf053fe39e3', contemporary_software_architectures, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('abb6fd1b-1772-41b8-8fdf-3bf053fe39e3', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_software_firms).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, hybrid_software_developers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, strong_copyleft_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, software_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These firms integrate GPL-licensed components into their proprietary products, benefiting from the narrow interpretation that allows them to maintain proprietary layers without triggering full copyleft obligations. They actively advocate for this interpretation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_software_firms, beneficiary,
    powerful, biographical, mobile, global).

% Individual developers or small teams who create software that combines GPL and proprietary code. This reading provides them with legal clarity and flexibility, reducing the friction of mixed-license development.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, hybrid_software_developers, beneficiary,
    moderate, biographical, mobile, global).

% Organizations and individuals who believe the GPL's intent is to maximize the amount of code returned to the free software commons. They view this narrow interpretation as undermining the 'viral' nature of copyleft and reducing the pool of freely available software.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, strong_copyleft_advocates, payer,
    organized, generational, constrained, global).

% Entities like the Free Software Foundation (FSF) that publish and defend the GPL. While they advocate for strong copyleft, they must also acknowledge and navigate the legal realities and diverse interpretations that emerge in practice, making their role complex.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, open_source_foundations, agenda_setter,
    institutional, generational, constrained, global).

% Users of software that incorporates GPL components. They benefit from the broader availability of software that results from easier integration of open-source and proprietary elements, even if it means less code is fully open-source.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, software_users, beneficiary,
    moderate, immediate, mobile, global).

% Academics and legal experts who analyze copyright law and its application to software licensing. They study the implications of different GPL interpretations for intellectual property, innovation, and the open-source ecosystem.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__narrow_scope_reading, commercial_software_firms).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__narrow_scope_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework that allows commercial and open-source software components to be combined in a single product, reducing legal uncertainty and facilitating hybrid development models.
% TRANSFER_FUNCTION: Transfers the flexibility to integrate GPL components with proprietary code to commercial entities, at the 'cost' of reducing the scope of code that must be shared under copyleft to the open-source commons.
% ABSENT_VOICES: Developers and projects committed to a 'strong copyleft' interpretation, who would argue for a broader definition of 'derivative work' to ensure more code remains free, are often marginalized in industry-driven legal interpretations.
% DISAPPEARANCE_RATIONALE: If this narrow interpretation vanished, commercial firms would face significantly higher legal risks and compliance costs for using GPL components, potentially leading to reduced adoption of open-source software in proprietary products or a complete restructuring of their licensing strategies. The hybrid software ecosystem would be profoundly altered.
% FOUNDING_PROBLEM: The original GPL aimed to ensure that software built upon free software remained free, preventing proprietary enclosure. The 'derivative work' boundary was intended to define the scope of this obligation.
% FOUNDING_PROBLEM_CORROBORATION: Strong copyleft advocates and the FSF attest that the problem of proprietary enclosure remains live, and this narrow reading undermines the GPL's solution. Commercial firms and many hybrid developers argue that the problem is sufficiently addressed by the GPL's core, and this reading provides necessary flexibility for innovation, with legal scholars often providing analysis supporting both sides of the contest.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__narrow_scope_reading_tests).
:- end_tests(gpl_copyleft_scope__narrow_scope_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, reflecting the 'cost' to strong copyleft advocates who see less code returned to the commons, but it's not a pure extraction as it enables broader adoption of GPL components. Suppression (0.25) is low because this reading offers clear pathways for compliance without outright coercion, and alternatives (other licenses, different linking strategies) exist. Theater ratio is low (0.1) as the interpretation is genuinely applied, not merely performed. The 'rope' classification reflects its function as a coordination mechanism for mixed-license development, despite the asymmetric impact on different ideological camps.
 *
 * PERSPECTIVAL GAP:
 *   Commercial firms and hybrid developers experience this as a beneficial coordination mechanism, allowing them to leverage open-source components without fully 'viral' copyleft obligations. Strong copyleft advocates, however, perceive it as an erosion of the GPL's intended purpose, leading to a 'leakage' of value from the commons. The engine's classification as 'rope' from the perspective of integrators, but potentially 'tangled_rope' or 'snare' from the perspective of strong copyleft advocates, captures this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial software firms and hybrid developers are beneficiaries (d=0.0-0.2) as this reading grants them flexibility and reduces their obligations. Strong copyleft advocates are victims (d=0.8-1.0) as their goal of maximizing shared code is undermined. Open source foundations, as agenda setters, are closer to symmetric (d=0.4-0.6) as they administer the license but must navigate competing interpretations. Software users are diffuse beneficiaries (d=0.1-0.3) due to increased software availability.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a flexible coordination mechanism as pure extraction. While it does extract 'value' (in the form of proprietary code) from the copyleft commons, it simultaneously enables a broader ecosystem of mixed-license software, which is a form of coordination. The contest is over the *terms* of coordination, not its existence. If the founding problem of enabling software collaboration were dead, and this interpretation merely served to funnel value to commercial entities without any reciprocal benefit, it would lean towards a 'snare'. As it stands, it's a contested rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''narrow scope'' reading of GPL Section 2(b), or is it merely a strategic interpretation by commercial interests?',
    'Judicial precedent or widespread adoption of this interpretation by non-commercial, FSF-aligned projects.',
    'If a genuine reading, it solidifies the ''rope'' classification by demonstrating broad acceptance of its coordination function. If strategic, it leans towards ''tangled_rope'' due to unacknowledged extraction from the copyleft commons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''gpl_copyleft_scope'' kernel, specifically the ''narrow_scope_reading''. Sibling readings (''strong_copyleft_reading'', ''enforcement_vacuum_reading'') would alter the derivative work boundary and the perceived obligations of integrators.').

omega_variable(
    derivative_work_boundary_ambiguity,
    'Does ''traditional copyright doctrine'' provide a sufficiently clear and stable boundary for derivative works in the context of modern software architectures (e.g., microservices, cloud functions)?',
    'New case law specifically addressing software derivative works in contemporary architectures, or industry-wide consensus on best practices for linking and aggregation under copyright.',
    'If the boundary is unclear, the constraint''s predictability and coordination function are weakened, increasing legal risk for all parties and potentially shifting it towards a ''tangled_rope'' due to asymmetric enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_boundary_ambiguity, empirical, 'The core disagreement between readings is located in the definition and application of ''derivative work'' to software, particularly concerning linking and aggregation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 20, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, information_standard).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_copyleft_scope' kernel. This 'narrow_scope_reading' emphasizes traditional copyright doctrine, allowing more flexibility for proprietary integration, in contrast to the 'strong_copyleft_reading' which seeks to maximize code sharing, and the 'enforcement_vacuum_reading' which highlights the lack of definitive judicial resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
