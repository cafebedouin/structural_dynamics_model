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
 *   human_readable: GPL Strong Copyleft Scope (Dynamic Linking & Coupling)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint is the 'strong copyleft' reading of the GPL's derivative
 *   work scope, asserting that dynamic linking and other forms of code
 *   coupling trigger the copyleft requirement. This interpretation is
 *   actively enforced by the Free Software Foundation (FSF) and its allies,
 *   aiming to ensure that software built upon GPL-licensed components remains
 *   free. Sibling readings include 'narrow_scope_reading' (limiting copyleft
 *   to direct derivatives) and 'enforcement_vacuum_reading' (where actual
 *   constraint depends on enforcement capacity).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.78).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.85).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Strong Copyleft Scope (Dynamic Linking & Coupling)").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, 'e626a9d2-0eb4-40ee-84b0-1e9e23c7963f').
narrative_ontology:cs_kernel_codification('e626a9d2-0eb4-40ee-84b0-1e9e23c7963f', fixed_text).
narrative_ontology:cs_authority_grounding('e626a9d2-0eb4-40ee-84b0-1e9e23c7963f', lineage).
narrative_ontology:cs_interpretation_layer_present('e626a9d2-0eb4-40ee-84b0-1e9e23c7963f').
narrative_ontology:cs_reading_relation('e626a9d2-0eb4-40ee-84b0-1e9e23c7963f', gpl_copyleft_scope__narrow_scope_reading, forecloses).
narrative_ontology:cs_reading_relation('e626a9d2-0eb4-40ee-84b0-1e9e23c7963f', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('e626a9d2-0eb4-40ee-84b0-1e9e23c7963f', foundational, software_freedom_requires_reciprocity).
narrative_ontology:cs_axiom_status(software_freedom_requires_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('e626a9d2-0eb4-40ee-84b0-1e9e23c7963f', software_freedom_requires_reciprocity, deontological).
narrative_ontology:cs_axiom('e626a9d2-0eb4-40ee-84b0-1e9e23c7963f', foundational, coupling_implies_derivative_work).
narrative_ontology:cs_axiom_status(coupling_implies_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('e626a9d2-0eb4-40ee-84b0-1e9e23c7963f', coupling_implies_derivative_work, conventional).
narrative_ontology:cs_reference_frame('e626a9d2-0eb4-40ee-84b0-1e9e23c7963f', free_software_ideology).
narrative_ontology:cs_drift_state('e626a9d2-0eb4-40ee-84b0-1e9e23c7963f', contemporary_software_ecosystem, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e626a9d2-0eb4-40ee-84b0-1e9e23c7963f', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_community).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, fsf_licensing_enforcers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively interpret and enforce the GPL, asserting that dynamic linking and other forms of code coupling create derivative works subject to the GPL's copyleft. They initiate legal action or public campaigns to ensure compliance, aiming to expand the pool of free software.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, fsf_licensing_enforcers, agenda_setter,
    institutional, generational, analytical, global).

% Benefits from the strong copyleft interpretation by ensuring that contributions to GPL-licensed projects, even when integrated into larger systems, remain free. This guarantees access to source code and fosters a collaborative development environment, preventing proprietary enclosure.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_community, beneficiary,
    organized, generational, mobile, global).

% Face significant costs and restrictions if they wish to integrate GPL-licensed components into their proprietary products. They must either release their entire product under the GPL (a major business model shift) or avoid GPL components, limiting their technical options. They actively lobby against this interpretation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors, payer,
    institutional, biographical, constrained, global).

% Companies that build systems using a mix of open-source and proprietary components. The strong copyleft interpretation forces them to carefully manage dependencies, often incurring legal review costs or foregoing efficient technical solutions to avoid triggering GPL obligations for their proprietary codebases.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators, payer,
    powerful, biographical, constrained, global).

% Advise clients on GPL compliance, often navigating the contested boundaries of derivative works. They analyze legal precedents, license texts, and community interpretations, playing a critical role in shaping the practical application of the strong copyleft reading.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, open_source_lawyers, observer,
    analytical, biographical, analytical, global).

% Are indirectly affected by the strong copyleft interpretation. They may experience higher prices for proprietary software (if vendors pass on compliance costs) or reduced functionality (if vendors avoid integrating useful GPL components). They have no direct voice in the licensing debate.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, users_of_proprietary_software, excluded,
    powerless, immediate, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__strong_copyleft_reading, free_software_community).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__strong_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that software built upon GPL-licensed components remains free and open, fostering a collaborative ecosystem and preventing proprietary enclosure of shared code, thereby coordinating the collective action problem of maintaining a commons.
% TRANSFER_FUNCTION: Transfers the right to distribute proprietary derivatives of GPL-coupled code from proprietary vendors to the free software community, or forces proprietary vendors to release their source code under GPL, effectively transferring control over derivative works.
% ABSENT_VOICES: Users of proprietary software who might benefit from easier integration of GPL components into proprietary products (e.g., lower prices, more features) but are not part of the licensing debate. Also, developers who prefer more permissive licenses and find strong copyleft restrictive.
% DISAPPEARANCE_RATIONALE: If this strong interpretation vanished, proprietary vendors would integrate GPL components more freely without releasing their source, leading to a significant shift in the open-source ecosystem towards more proprietary enclosure and less reciprocal contribution. The free software commons would erode.
% FOUNDING_PROBLEM: Preventing proprietary software from 'taking' free software code and enclosing it, thereby undermining the free software movement's goal of universal software freedom and ensuring that all users retain the four essential freedoms (run, study, modify, distribute).
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation (FSF) and many free software advocates attest that the problem of proprietary enclosure remains live and constantly evolving. Proprietary vendors and some legal scholars contest the scope of the problem, arguing that the strong copyleft interpretation stifles innovation; however, the core problem of ensuring software freedom is widely acknowledged by independent observers.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__strong_copyleft_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high (0.78) because proprietary vendors face a significant 'cost' of either releasing their source or avoiding GPL components, which is extracted as a guarantee of software freedom for the community. Suppression is very high (0.85) due to the active legal enforcement and the structural barriers it creates for proprietary integration. Theater ratio is low (0.10) because the enforcement actions are genuine and directly serve the stated goal of expanding free software, not merely performing compliance. Accessibility collapse is high (0.80) for proprietary vendors, as their alternatives for integrating GPL code are severely limited. Resistance is also high (0.70) from proprietary vendors and some legal scholars who contest this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the free software community, this is a necessary coordination mechanism to protect software freedom. From the perspective of proprietary vendors, it is an extractive snare that forces them to choose between their business model and valuable open-source components. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The FSF and the free software community are clear beneficiaries, gaining guaranteed access to source code and preventing proprietary enclosure. Proprietary software vendors and commercial integrators are the primary targets, bearing the costs of compliance or avoidance. Open source lawyers act as observers, while users of proprietary software are excluded from the debate but indirectly affected.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_boundary_ambiguity,
    'What constitutes a ''derivative work'' under copyright law, specifically concerning dynamic linking and other forms of code coupling, in a way that is universally accepted by courts?',
    'Definitive judicial precedent from a high court, or legislative clarification of copyright law regarding software coupling.',
    'If a narrow interpretation is legally upheld, the constraint''s effective extractiveness and suppression would decrease for proprietary vendors, potentially reclassifying it towards a Rope or even Piton for the FSF. If the strong interpretation is universally upheld, its Snare-like qualities would be solidified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_work_boundary_ambiguity, empirical, 'Legal ambiguity of the derivative work boundary for software coupling.').

omega_variable(
    enforcement_capacity_variability,
    'How does the actual enforcement capacity of the FSF and other strong copyleft advocates vary across jurisdictions and against different types of proprietary actors?',
    'Empirical study of GPL enforcement actions, their success rates, and the resources deployed, disaggregated by jurisdiction and target type.',
    'If enforcement capacity is highly variable or weak in practice, the ''strong copyleft'' reading might function more as an ''enforcement_vacuum_reading'' in many contexts, reducing its effective suppression and extractiveness, potentially shifting its classification towards a Piton or even a Rope in those contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_variability, empirical, 'Variability in the practical enforceability of strong copyleft.').

omega_variable(
    innovation_impact_tradeoff,
    'Does the strong copyleft interpretation genuinely foster overall software innovation by protecting the commons, or does it stifle innovation by creating barriers for proprietary integration and commercialization?',
    'Longitudinal economic studies comparing innovation metrics in ecosystems with strong copyleft vs. more permissive licensing, controlling for other factors.',
    'If strong copyleft is shown to stifle innovation, its justification as a coordination mechanism would be weakened, potentially reclassifying it as a Snare. If it demonstrably fosters innovation, its coordination function would be strengthened, supporting a Tangled Rope or even Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_impact_tradeoff, empirical, 'Trade-off between copyleft enforcement and overall software innovation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 1989, 0.05).
narrative_ontology:measurement(gpl__tr_t1999, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 1999, 0.07).
narrative_ontology:measurement(gpl__tr_t2009, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2009, 0.08).
narrative_ontology:measurement(gpl__tr_t2019, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2019, 0.09).
narrative_ontology:measurement(gpl__tr_t2024, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 1989, 0.6).
narrative_ontology:measurement(gpl__be_t1999, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 1999, 0.68).
narrative_ontology:measurement(gpl__be_t2009, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2009, 0.73).
narrative_ontology:measurement(gpl__be_t2019, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2019, 0.76).
narrative_ontology:measurement(gpl__be_t2024, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 1989, 0.7).
narrative_ontology:measurement(gpl__su_t1999, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 1999, 0.75).
narrative_ontology:measurement(gpl__su_t2009, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2009, 0.8).
narrative_ontology:measurement(gpl__su_t2019, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2019, 0.83).
narrative_ontology:measurement(gpl__su_t2024, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
