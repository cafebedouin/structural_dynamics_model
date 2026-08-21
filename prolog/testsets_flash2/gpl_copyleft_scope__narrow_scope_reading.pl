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
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Copyleft Scope (Narrow Interpretation)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents a 'narrow scope' reading of GPL Section 2(b),
 *   where the definition of a 'derivative work' is strictly limited to direct
 *   modifications or close integrations, excluding mere aggregation, plugin
 *   architectures, or certain dynamic linking forms. This interpretation
 *   aligns with traditional copyright doctrine and allows commercial firms
 *   greater flexibility in combining GPL components with proprietary code. It
 *   functions as a coordination mechanism for mixed codebases, but at the
 *   cost of weakening the 'viral' copyleft effect desired by strong copyleft
 *   advocates. The claimed type is 'rope' because it facilitates
 *   coordination, but the metrics reflect a moderate level of extraction from
 *   the perspective of those desiring stronger copyleft.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.35).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.2).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Copyleft Scope (Narrow Interpretation)").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__narrow_scope_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, '0b09b9bc-cb50-434a-9d7c-009d55725323').
narrative_ontology:cs_kernel_codification('0b09b9bc-cb50-434a-9d7c-009d55725323', fixed_text).
narrative_ontology:cs_authority_grounding('0b09b9bc-cb50-434a-9d7c-009d55725323', lineage).
narrative_ontology:cs_interpretation_layer_present('0b09b9bc-cb50-434a-9d7c-009d55725323').
narrative_ontology:cs_reading_relation('0b09b9bc-cb50-434a-9d7c-009d55725323', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b09b9bc-cb50-434a-9d7c-009d55725323', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('0b09b9bc-cb50-434a-9d7c-009d55725323', foundational, derivative_work_follows_traditional_copyright).
narrative_ontology:cs_axiom_status(derivative_work_follows_traditional_copyright, holdable).
narrative_ontology:cs_axiom_grounding('0b09b9bc-cb50-434a-9d7c-009d55725323', derivative_work_follows_traditional_copyright, conventional).
narrative_ontology:cs_reference_frame('0b09b9bc-cb50-434a-9d7c-009d55725323', traditional_copyright_doctrine).
narrative_ontology:cs_drift_state('0b09b9bc-cb50-434a-9d7c-009d55725323', contemporary_software_development, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0b09b9bc-cb50-434a-9d7c-009d55725323', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_software_firms).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, open_source_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, fsf_and_copyleft_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the flexibility to integrate GPL-licensed components into proprietary software without being forced to open-source their entire codebase, as long as the integration methods (e.g., aggregation, dynamic linking) are not considered 'derivative works' under this narrow interpretation. This allows for mixed-licensing strategies.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_software_firms, beneficiary,
    institutional, biographical, mobile, global).

% Benefits from the wider adoption and integration of their GPL-licensed code into commercial products, increasing its reach and utility. While some may prefer stronger copyleft, this reading facilitates collaboration and use by a broader developer community.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, open_source_developers, beneficiary,
    moderate, biographical, constrained, global).

% Bears the cost of weakened copyleft enforcement, as their goal of ensuring all derivative works remain free is not fully realized. They actively promote a broader interpretation of 'derivative work' and may engage in legal action or advocacy to challenge this narrow reading. Their identity is tied to the strong copyleft principle.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, fsf_and_copyleft_advocates, payer,
    organized, generational, identity_locked, global).

% Interprets and applies copyright doctrine to determine the boundary of derivative works. This reading aligns with traditional copyright principles, which generally require a closer form of integration to trigger derivative work status. Their rulings shape the practical enforcement of GPL.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, copyright_lawyers_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for coordinating the use and distribution of software, allowing for the integration of GPL-licensed components into larger systems while providing clarity on the boundaries for proprietary code, thus facilitating mixed-licensing ecosystems.
% TRANSFER_FUNCTION: Transfers the flexibility to combine GPL and proprietary code to commercial firms, in exchange for wider adoption and use of GPL-licensed software. It also transfers interpretive authority to traditional copyright doctrine.
% ABSENT_VOICES: Developers who strongly advocate for a 'viral' or 'strong' copyleft effect, believing that all code linked to GPL should also be GPL, are often marginalized in legal interpretations that favor traditional copyright boundaries. They would argue for a broader definition of derivative work.
% DISAPPEARANCE_RATIONALE: If this narrow interpretation vanished, commercial firms would face significant legal uncertainty or be forced to adopt a strong copyleft stance for all integrated code, fundamentally altering their business models and the landscape of mixed-source software development. The open-source ecosystem would become more bifurcated.
% FOUNDING_PROBLEM: The GPL was created to ensure software freedom and prevent proprietary enclosure of free software, but the definition of 'derivative work' was left open to interpretation, leading to ambiguity in mixed-licensing scenarios.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and industry practitioners widely acknowledge the ongoing ambiguity and debate surrounding the 'derivative work' boundary in copyright law, especially concerning software. This problem is actively discussed in legal journals and industry forums, corroborating its live status from outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__narrow_scope_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.35) is moderate because while it allows commercial firms to retain proprietary layers, it still requires them to adhere to GPL for direct derivatives, ensuring some level of code freedom. Suppression (0.20) is low because this reading is largely permissive, defining what is NOT constrained rather than actively coercing. Theater ratio (0.10) is low as the interpretation is genuinely applied in practice, not merely for show. Accessibility collapse (0.40) is moderate, as it clarifies boundaries but doesn't eliminate the need for careful licensing. Resistance (0.15) is low from the perspective of those benefiting from this flexibility, though strong copyleft advocates do resist it.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of commercial firms, this is a clear 'rope' that enables valuable coordination. From the perspective of strong copyleft advocates, it might be seen as a 'tangled rope' or even a 'snare' that allows proprietary interests to exploit free software without fully contributing back. The engine's classification will reflect the aggregate structural data, but the subjective experience of the constraint differs significantly.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial software firms and many open-source developers are beneficiaries, gaining flexibility or wider adoption. FSF and copyleft advocates are payers, as their vision of universal code freedom is curtailed. Copyright lawyers and courts act as agenda-setters, interpreting the legal boundaries that define this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_legal_clarity,
    'Will future judicial rulings definitively clarify the ''derivative work'' boundary for software, especially concerning dynamic linking and plugin architectures?',
    'Landmark court cases or legislative action specifically addressing software copyright and derivative works in the context of open-source licenses.',
    'A definitive ruling could either solidify this narrow reading (reducing extractiveness for commercial firms) or shift towards a broader interpretation (increasing extractiveness for commercial firms, reducing it for copyleft advocates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_legal_clarity, empirical, 'Uncertainty regarding the legal interpretation of ''derivative work'' in software.').

omega_variable(
    community_norm_shift,
    'Will the prevailing norms within the open-source community shift towards either a more permissive (MIT/Apache-style) or a more restrictive (strong copyleft) stance, influencing practical enforcement?',
    'Surveys of developer licensing preferences, analysis of new project license choices, and observed enforcement actions by major open-source foundations.',
    'A shift towards permissiveness would further entrench this narrow reading, while a shift towards strong copyleft could increase resistance and pressure for broader interpretations, potentially leading to a reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_norm_shift, preference, 'Influence of community norms on license interpretation and enforcement.').

omega_variable(
    framing_underdetermination_gpl_scope,
    'Is this constraint best framed as a ''narrow scope'' reading of GPL, or as an ''enforcement vacuum'' where the actual constraint is determined by the specific context and power dynamics of the parties involved?',
    'Analysis of actual legal outcomes and industry practices: if outcomes consistently align with traditional copyright doctrine despite varying power dynamics, the ''narrow scope'' framing is stronger. If outcomes are highly variable and dependent on who has the resources to enforce, the ''enforcement vacuum'' framing is more accurate.',
    'If the ''enforcement vacuum'' framing is adopted, the constraint''s extractiveness and suppression might be re-evaluated as more context-dependent, potentially leading to a ''tangled rope'' or ''snare'' classification in specific high-power asymmetry scenarios, rather than a consistent ''rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_underdetermination_gpl_scope, conceptual, 'Ambiguity in framing the GPL''s scope: a consistent narrow interpretation versus a context-dependent enforcement vacuum.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, information_standard).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_copyleft_scope' kernel. This 'narrow_scope_reading' defines the derivative work boundary according to traditional copyright, allowing more flexibility for proprietary integration. It contrasts with the 'strong_copyleft_reading' (broader derivative work definition) and the 'enforcement_vacuum_reading' (focus on practical, context-dependent enforcement due to legal ambiguity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
