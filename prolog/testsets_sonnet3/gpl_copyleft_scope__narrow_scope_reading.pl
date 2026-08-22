% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Section 2(b) Narrow-Scope Derivative Work Reading
 *   domain: software_licensing_intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the narrow-scope reading of the GPL Section 2(b)
 *   derivative-work kernel: aggregation, plugin architectures, and many
 *   dynamic linking patterns fall outside the copyleft trigger, with the
 *   derivative-work boundary determined by traditional copyright doctrine
 *   rather than an expansive license-specific standard. This reading has
 *   become the dominant practical operating assumption in commercial software
 *   engineering, even though it has never been definitively settled by a
 *   controlling court ruling on the dynamic-linking question specifically.
 *   The sibling readings (strong_copyleft_reading,
 *   enforcement_vacuum_reading) are separate constraint stories, not
 *   alternative measurements of this one — each has its own ε, its own
 *   beneficiary/victim structure, and its own classification, linked via
 *   network.affects_constraints. This story's ε is stable at approximately
 *   0.32 under the narrow reading's own lights, evaluating the standing
 *   arrangement (commercial integration around the narrow line) as this
 *   reading assesses it.
 *
 * KEY AGENTS:
 *   - commercial_integrators: primary beneficiary (organized/mobile) — designs around the narrow line to preserve proprietary control
 *   - proprietary_plugin_vendors: beneficiary (moderate/mobile) — business model depends on plugin/derivative distinction holding
 *   - mixed_codebase_enterprises: primary beneficiary (institutional/arbitrage) — large-scale architecture decisions ride on legal certainty
 *   - downstream_code_sharing_expectant_contributors: primary target (powerless/constrained) — bears the cost of unrealized reciprocity expectations
 *   - fsf_and_copyleft_advocacy_orgs: excluded voice (organized/constrained) — advocates for the foreclosed alternative, present in discourse but weak in outcome
 *   - software_licensing_courts: analytical observer (institutional/analytical) — adjudicates sparsely, leaving the doctrine underdetermined
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.32).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.22).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Section 2(b) Narrow-Scope Derivative Work Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software_licensing_intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, '83a0c898-be63-4d66-94a5-58d1bd4c88c0').
narrative_ontology:cs_kernel_codification('83a0c898-be63-4d66-94a5-58d1bd4c88c0', fixed_text).
narrative_ontology:cs_authority_grounding('83a0c898-be63-4d66-94a5-58d1bd4c88c0', practice).
narrative_ontology:cs_interpretation_layer_present('83a0c898-be63-4d66-94a5-58d1bd4c88c0').
narrative_ontology:cs_reading_relation('83a0c898-be63-4d66-94a5-58d1bd4c88c0', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('83a0c898-be63-4d66-94a5-58d1bd4c88c0', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('83a0c898-be63-4d66-94a5-58d1bd4c88c0', foundational, derivative_work_boundary_follows_general_copyright_doctrine).
narrative_ontology:cs_axiom_status(derivative_work_boundary_follows_general_copyright_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('83a0c898-be63-4d66-94a5-58d1bd4c88c0', derivative_work_boundary_follows_general_copyright_doctrine, conventional).
narrative_ontology:cs_axiom('83a0c898-be63-4d66-94a5-58d1bd4c88c0', secondary, loose_coupling_mechanisms_do_not_create_combined_work).
narrative_ontology:cs_axiom_status(loose_coupling_mechanisms_do_not_create_combined_work, holdable).
narrative_ontology:cs_axiom_grounding('83a0c898-be63-4d66-94a5-58d1bd4c88c0', loose_coupling_mechanisms_do_not_create_combined_work, conventional).
narrative_ontology:cs_reference_frame('83a0c898-be63-4d66-94a5-58d1bd4c88c0', traditional_copyright_derivative_work_doctrine).
narrative_ontology:cs_drift_state('83a0c898-be63-4d66-94a5-58d1bd4c88c0', post_dynamic_linking_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('83a0c898-be63-4d66-94a5-58d1bd4c88c0', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_integrators).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, proprietary_plugin_vendors).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, mixed_codebase_enterprises).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, downstream_code_sharing_expectant_contributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build products that combine GPL-licensed components with proprietary layers via aggregation, plugin architecture, or dynamic linking. Under the narrow reading, they can ship proprietary code alongside or loosely coupled to GPL code without triggering copyleft on the proprietary portion. They structure their architecture specifically to fall on the non-derivative side of the line.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_integrators, beneficiary,
    organized, biographical, mobile, global).

% Sell plugins that interface with GPL host applications through defined APIs. The narrow reading lets them keep plugin source proprietary as long as the coupling is judged a plugin relationship rather than a derivative work. Their business model depends on this boundary holding.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, proprietary_plugin_vendors, beneficiary,
    moderate, biographical, mobile, global).

% Large firms that adopt GPL components internally and at the edges of larger proprietary systems. They rely on legal opinions favoring the narrow reading to justify internal architecture decisions, and can restructure code boundaries (separate processes, defined interfaces) to preserve proprietary control.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, mixed_codebase_enterprises, beneficiary,
    institutional, generational, arbitrage, global).

% Contributed code to GPL projects on the understanding that anything built on top would also be shared back to the commons. Under the narrow reading, they watch commercial derivatives built via aggregation or dynamic linking capture value from their work without any reciprocal contribution. They have no standing to compel a broader reading absent litigation, which they generally cannot afford.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, downstream_code_sharing_expectant_contributors, payer,
    powerless, generational, constrained, global).

% Argue for the strong-copyleft reading and would prefer courts adopt it, but have limited capacity to force test cases and cannot unilaterally overrule the narrow reading's practical dominance in commercial contexts. Their interpretation is not absent from discourse but is structurally weaker in litigation outcomes and industry practice.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, fsf_and_copyleft_advocacy_orgs, excluded,
    organized, civilizational, constrained, global).

% Adjudicate derivative-work boundary disputes when they reach litigation, applying traditional copyright doctrine (substantial similarity, work-for-hire analogues) rather than license-text-specific tests. Their sparse and jurisdiction-specific rulings are what leaves the boundary as contested as it is.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, software_licensing_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__narrow_scope_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__narrow_scope_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, judicially-grounded line for what counts as a derivative work under copyright, letting firms design architectures (separate processes, documented APIs, loose coupling) with predictable licensing consequences instead of guessing at an untested expansive standard.
% TRANSFER_FUNCTION: Moves the option value of code-boundary design from the GPL commons to whichever party controls the architecture — typically the commercial integrator — since drawing the line at traditional derivative-work doctrine lets proprietary code sit adjacent to, rather than fused with, GPL code without triggering share-back obligations.
% ABSENT_VOICES: FSF-aligned advocates and original contributors who released code expecting broad copyleft reach are present in public discourse but structurally absent from the actual determination, which is made by corporate legal departments structuring code to avoid litigation risk rather than by courts settling the doctrine definitively.
% DISAPPEARANCE_RATIONALE: If the narrow-scope reading were displaced by an authoritative strong-copyleft ruling, plugin ecosystems and dynamically-linked commercial integrations built on GPL components would face immediate relicensing pressure or architectural rework; firms currently relying on legal opinions favoring the narrow line would need to either open-source integrated code or excise GPL dependencies entirely.
% FOUNDING_PROBLEM: GPL Section 2(b) was drafted to prevent a specific evasion: taking GPL code, modifying or combining it, and distributing the result under a proprietary license without sharing the changes — while leaving room for genuinely separate programs to interoperate without becoming 'infected' by copyleft.
% FOUNDING_PROBLEM_CORROBORATION: Software licensing attorneys advising both open-source and proprietary clients corroborate that the narrow reading tracks conventional copyright derivative-work doctrine and has become the de facto industry-operating assumption; this corroboration comes from parties adjacent to but not identical with the beneficiary set (outside counsel, not the integrators themselves), though it is not independent of commercial interests that favor licensing certainty over expansive copyleft.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__narrow_scope_reading, 0.32, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.32) because the narrow reading does functionally redirect value — commercial integrators capture architectural flexibility that original contributors did not anticipate ceding — but this is bounded, not severe: the doctrine tracks a genuinely defensible copyright-law analogy (traditional derivative-work tests), not a naked license-text stretch. Suppression is low (0.22) because no active enforcement apparatus polices the boundary; firms self-select architectures and the constraint operates through legal-risk calculation rather than coercion. Theater ratio is low and rises only slightly (0.18 by interval end) reflecting the mild uptick in formalized 'GPL-compliance' architecture reviews as legal opinions solidify around the narrow reading — mostly genuine risk management, not performance. Accessibility collapse is moderate (0.35): firms and contributors both retain real alternative licensing/architecture choices, unlike a mountain where alternatives are foreclosed. Resistance is moderate (0.45), reflecting ongoing copyleft-advocate pushback that has real voice but limited enforcement leverage.
 *
 * PERSPECTIVAL GAP:
 *   From the commercial integrator seat, this reading is a rope: a workable, judicially-grounded coordination mechanism that lets mixed codebases exist without perpetual litigation risk. From the seat of a contributor who released code under GPL expecting broad copyleft reach, the same structural fact reads as a quiet redirection of value — coordination for some is unrealized entitlement for others. The engine computes these divergent per-seat readings from the declared power/exit/beneficiary structure; the claimed_type of 'rope' reflects the dominant, structurally defensible reading, and the moderate extractiveness score is the honest acknowledgment that this rope has a cost side.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial integrators, plugin vendors, and mixed-codebase enterprises are structural beneficiaries: the narrow reading directly expands their design space and reduces licensing risk, placing them near the beneficiary end of directionality. Downstream contributors who expected broad reciprocal sharing sit near the target end — they bear a diffuse, hard-to-litigate cost as commercial value is captured without contribution back to the commons, and their exit options are genuinely constrained (they cannot retroactively relicense contributed code or unilaterally force a stronger reading). FSF-aligned advocates are excluded rather than victimized in the strict sense — their voice exists but lacks enforcement capacity, which is why they appear as 'excluded' rather than 'payer.'
 *
 * MANDATROPHY ANALYSIS:
 *   The narrow reading is not itself an obsolete mandate — the founding problem (preventing outright evasion of copyleft via trivial repackaging while permitting genuine interoperation) remains live, and the narrow reading serves a genuine current coordination function for an active industry practice of mixed proprietary/open architectures. It is not a piton: it is actively used and actively contested, not merely inertial. Classifying it as rope rather than tangled_rope reflects that while there IS a payer class, the extraction is moderate, not severe, and the arrangement's coordination function (predictable architecture decisions) dominates its extraction function at current measured levels — this could shift toward tangled_rope if extraction metrics rise, which is exactly the kind of drift T17 would flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrow_reading_doctrinal_stability,
    'Is the narrow-scope reading''s grounding in traditional copyright derivative-work doctrine a stable legal foundation, or is it an industry-convenient interpretation that a definitive appellate ruling could overturn?',
    'A controlling appellate decision squarely addressing dynamic linking and plugin architecture under GPL Section 2(b) would resolve which reading has actual legal force; absent that, the question remains a matter of legal-opinion consensus rather than settled law.',
    'If courts eventually adopt the strong-copyleft reading, this constraint''s beneficiary set loses its structural protection and the arrangement would need to be reclassified — likely toward tangled_rope or even snare as previously-uncompensated extraction becomes visible in litigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_reading_doctrinal_stability, empirical, 'Whether the narrow reading''s legal grounding is durable or provisional pending litigation.').

omega_variable(
    kernel_framing_choice,
    'Is the correct framing of this kernel a single contested copyright-doctrine question (which this story assumes), or is it better modeled as a jurisdiction-fragmented set of doctrines that never converge to one answer?',
    'Comparative survey of how the derivative-work boundary is actually adjudicated across major jurisdictions (US, EU, and others with active GPL litigation) would show whether one doctrinal question or many coexists under the ''GPL Section 2(b) scope'' label.',
    'If jurisdictions diverge substantially, the narrow_scope_reading, strong_copyleft_reading, and enforcement_vacuum_reading framing may itself need further decomposition by jurisdiction rather than treated as three global readings of one kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the three-reading kernel decomposition holds globally or should further fragment by jurisdiction.').

omega_variable(
    contributor_expectation_measurement,
    'How many GPL contributors actually held broad-copyleft expectations at time of contribution, versus accepting the license text''s plain terms as narrowly scoped from the outset?',
    'Survey or historical analysis of contributor intent statements, mailing list discussions, and license-selection rationale at time of original contribution across major GPL projects.',
    'If most contributors always understood the narrow scope, the ''victim'' framing for downstream_code_sharing_expectant_contributors weakens substantially — the extraction would be better characterized as disappointed aspiration rather than a betrayed structural bargain, lowering the effective extractiveness score.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contributor_expectation_measurement, empirical, 'Whether contributor expectations at time of licensing matched the narrow or broad scope reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gpl__tr_t6, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(gpl__tr_t12, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(gpl__tr_t18, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement(gpl__tr_t24, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(gpl__tr_t30, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 30, 0.18).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(gpl__be_t6, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 6, 0.25).
narrative_ontology:measurement(gpl__be_t12, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement(gpl__be_t18, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 18, 0.29).
narrative_ontology:measurement(gpl__be_t24, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement(gpl__be_t30, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 30, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gpl_copyleft_scope__narrow_scope_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__narrow_scope_reading, 0.1).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gpl_copyleft_scope kernel. narrow_scope_reading and strong_copyleft_reading present structurally opposed derivative-work boundaries drawn from the same license text; enforcement_vacuum_reading models the absence of controlling precedent as itself the operative constraint. Each carries its own ε, beneficiary/victim structure, and claimed_type — narrow_scope_reading is authored here as a moderate-epsilon rope; strong_copyleft_reading would be authored with a materially different beneficiary/victim reversal (copyleft commons as beneficiary, commercial integrators as payer) and likely higher ε from its own lights; enforcement_vacuum_reading would center on the uncertainty itself as the extractive/coordinating mechanism. All three should link to each other via affects_constraints to preserve the kernel-family structure for contamination and coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
