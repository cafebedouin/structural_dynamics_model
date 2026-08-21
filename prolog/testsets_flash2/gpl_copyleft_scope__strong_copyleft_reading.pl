% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__strong_copyleft_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Strong Copyleft Scope (Dynamic Linking/Combined Works)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the 'strong copyleft' reading of the GNU
 *   General Public License (GPL), specifically concerning Section 2(b) and
 *   the definition of a 'derivative work' to include combined or dynamically
 *   linked software. This reading asserts that any code coupling with
 *   GPL-licensed components triggers the copyleft obligation, requiring the
 *   entire combined work to be GPL-licensed. It functions as a snare for
 *   proprietary vendors, forcing them to choose between full source release
 *   or complete avoidance of GPL code. The Free Software Foundation (FSF) and
 *   aligned communities are the primary beneficiaries, ensuring the expansion
 *   of the free software commons.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.85).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.75).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, snare).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Strong Copyleft Scope (Dynamic Linking/Combined Works)").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, '1aa8354d-bd97-4585-9912-23d2f562c03f').
narrative_ontology:cs_kernel_codification('1aa8354d-bd97-4585-9912-23d2f562c03f', fixed_text).
narrative_ontology:cs_authority_grounding('1aa8354d-bd97-4585-9912-23d2f562c03f', lineage).
narrative_ontology:cs_interpretation_layer_present('1aa8354d-bd97-4585-9912-23d2f562c03f').
narrative_ontology:cs_reading_relation('1aa8354d-bd97-4585-9912-23d2f562c03f', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_reading_relation('1aa8354d-bd97-4585-9912-23d2f562c03f', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('1aa8354d-bd97-4585-9912-23d2f562c03f', foundational, copyleft_extends_to_all_coupling).
narrative_ontology:cs_axiom_status(copyleft_extends_to_all_coupling, holdable).
narrative_ontology:cs_axiom_grounding('1aa8354d-bd97-4585-9912-23d2f562c03f', copyleft_extends_to_all_coupling, conventional).
narrative_ontology:cs_axiom('1aa8354d-bd97-4585-9912-23d2f562c03f', secondary, software_freedom_requires_viral_licensing).
narrative_ontology:cs_axiom_status(software_freedom_requires_viral_licensing, holdable).
narrative_ontology:cs_axiom_grounding('1aa8354d-bd97-4585-9912-23d2f562c03f', software_freedom_requires_viral_licensing, deontological).
narrative_ontology:cs_reference_frame('1aa8354d-bd97-4585-9912-23d2f562c03f', original_gpl_intent_maximal_freedom).
narrative_ontology:cs_drift_state('1aa8354d-bd97-4585-9912-23d2f562c03f', contemporary_software_ecosystem, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1aa8354d-bd97-4585-9912-23d2f562c03f', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, gpl_licensed_projects).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_developers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary author and enforcer of the GPL. Interprets the license broadly to ensure maximum code freedom and prevent proprietary enclosure. Actively monitors for violations and initiates enforcement actions, particularly regarding dynamic linking and combined works. Benefits from the expansion of the free software ecosystem.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation, agenda_setter,
    institutional, generational, analytical, global).

% Projects whose code is licensed under the GPL. They benefit from the strong copyleft ensuring that contributions and derivative works remain free and open, preventing proprietary forks. Their code base grows and remains accessible to the community.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, gpl_licensed_projects, beneficiary,
    organized, generational, mobile, global).

% Individual developers who contribute to or use GPL-licensed software. They benefit from the guarantee that their contributions will not be incorporated into proprietary systems without their consent, fostering a collaborative and open environment.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_developers, beneficiary,
    moderate, biographical, mobile, global).

% Companies that develop and sell proprietary software. They are the primary targets of this strong copyleft interpretation, as it severely restricts their ability to integrate GPL-licensed components without being forced to open-source their entire product. Their options are to avoid GPL code, use alternative licenses, or face legal action.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Businesses that integrate various software components, including open-source, into larger commercial systems. This strong interpretation of GPL copyleft creates significant legal and business risks, forcing them to carefully manage dependencies or avoid GPL components altogether, incurring higher development costs.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators, payer,
    organized, biographical, constrained, global).

% Academics and legal professionals who study intellectual property law and open-source licensing. They analyze the implications of different GPL interpretations, the enforceability of copyleft, and its impact on software innovation and industry practices.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the development and distribution of free software by ensuring that all derivative works and combined code remain under the GPL, fostering a shared, open ecosystem.
% TRANSFER_FUNCTION: Transfers the right to use, modify, and distribute software (including derivative works) from proprietary control to the public domain under GPL terms, effectively 'capturing' proprietary extensions for the free software community.
% ABSENT_VOICES: Proprietary software developers who wish to use GPL components without releasing their own source code are structurally excluded from this interpretation's benefits; they would argue for a narrower definition of 'derivative work' to protect their business models.
% DISAPPEARANCE_RATIONALE: If this strong interpretation of GPL copyleft vanished, proprietary vendors would immediately integrate GPL components into closed-source products, fragmenting the free software ecosystem and undermining the core goal of ensuring software freedom. The balance of power in software development would shift dramatically towards proprietary models.
% FOUNDING_PROBLEM: The problem of proprietary software developers taking open-source code, modifying it, and then distributing the enhanced version as proprietary, thereby 'enclosing' the commons and preventing further free development.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and many free software developers attest that this problem is still live, citing ongoing attempts by proprietary entities to circumvent copyleft. Legal scholars and industry analysts outside the FSF also corroborate the persistent tension between open and closed source models, validating the founding problem's continued relevance.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__strong_copyleft_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because proprietary vendors are forced to either give up their intellectual property or incur significant costs to re-engineer solutions without GPL components. Suppression is also high (0.75) due to the credible threat of legal enforcement by the FSF and the lack of easy workarounds for integrating GPL code into proprietary systems under this interpretation. Theater ratio is low (0.1) as the enforcement actions are genuine and directly serve the license's stated goal of maximizing software freedom. The claimed type is 'snare' because the coordination story (expanding the free software commons) is achieved through a mechanism that coercively extracts proprietary rights from identifiable victims.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the FSF and free software communities, this is a 'rope' or 'scaffold' that coordinates the creation of a free software commons. From the perspective of proprietary vendors, it is a 'snare' that extracts their intellectual property. The metrics reflect the latter, while the 'claimed_type' reflects the former, highlighting the fundamental disagreement over the constraint's true nature.
 *
 * DIRECTIONALITY LOGIC:
 *   The FSF and GPL-licensed projects are clear beneficiaries (d near 0.0) as this interpretation directly serves their mission and expands their ecosystem. Proprietary software vendors and commercial integrators are the primary targets (d near 1.0) as they bear the full cost of compliance or avoidance. Free software developers are also beneficiaries, as their work is protected from proprietary enclosure. Legal scholars act as observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_precedent_ambiguity,
    'How would definitive judicial precedent on dynamic linking and combined works under GPL affect this reading''s enforceability and perceived legitimacy?',
    'A landmark court ruling explicitly affirming or rejecting the ''strong copyleft'' interpretation in a major jurisdiction.',
    'An affirmation would solidify this reading''s ''snare'' classification by increasing suppression and extractiveness. A rejection would weaken it, potentially shifting it towards a ''piton'' or ''tangled_rope'' if enforcement becomes theatrical or contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_precedent_ambiguity, empirical, 'Uncertainty regarding the legal enforceability of the strong copyleft interpretation.').

omega_variable(
    alternative_licensing_impact,
    'To what extent do permissive open-source licenses (e.g., MIT, Apache) provide a viable alternative for proprietary vendors, thereby reducing the ''snare'' effect of the GPL?',
    'Market analysis of proprietary software''s adoption rates of GPL vs. permissive licensed components, and developer surveys on licensing choices.',
    'If permissive licenses are widely adopted as a substitute, the GPL''s effective extractiveness might decrease, as proprietary vendors have a less constrained exit. If not, the snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_licensing_impact, empirical, 'Impact of permissive licenses on GPL''s coercive power.').

omega_variable(
    derivative_work_conceptual_boundary,
    'Is the concept of ''derivative work'' in copyright law inherently ambiguous when applied to software coupling (e.g., dynamic linking vs. static linking, plugins vs. libraries), leading to irreducible conceptual contestation?',
    'Philosophical and legal analysis of ''derivative work'' definitions across different jurisdictions and technological contexts. Consensus among legal scholars on a clear, universally applicable definition.',
    'If the ambiguity is irreducible, the ''strong copyleft'' reading will always be conceptually contested, regardless of empirical enforcement. This would highlight the ''conceptual'' nature of the constraint''s persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_work_conceptual_boundary, conceptual, 'Conceptual ambiguity of ''derivative work'' in software.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 5, 0.82).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 10, 0.83).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 15, 0.84).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, identity_coordination).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, open_source_business_models).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_copyleft_scope' kernel. This 'strong_copyleft_reading' asserts a broad interpretation of GPL Section 2(b) to include dynamic linking and combined works, contrasting with the 'narrow_scope_reading' and the 'enforcement_vacuum_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
