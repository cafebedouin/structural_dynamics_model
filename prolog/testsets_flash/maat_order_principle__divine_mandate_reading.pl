% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Ma'at as Divine Mandate of Pharaoh
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint describes the 'divine mandate' reading of Ma'at in
 *   ancient Egypt, where cosmic order (Ma'at) flows directly from the divine
 *   through the Pharaoh to society. In this reading, the Pharaoh embodies
 *   Ma'at and, by definition, cannot violate it, positioning the ruler as the
 *   source and guarantor of order rather than being subject to its
 *   constraints. This interpretation justifies royal authority and extraction
 *   as a cosmic necessity, with high suppression of any alternative readings
 *   that might impose reciprocal obligations on the Pharaoh.
 *
 * KEY AGENTS:
 *   - pharaoh: Primary beneficiary/agenda_setter (institutional/arbitrage) — source of Ma'at, collects extraction
 *   - priestly_elite: Secondary beneficiary (organized/constrained) — interprets and propagates the divine mandate, benefits from its stability
 *   - scribal_bureaucracy: Payer/enforcer (organized/constrained) — administers royal decrees, bears the burden of maintaining the system, but also benefits from its order
 *   - commoners: Primary victims (powerless/trapped) — bear the extraction (labor, taxes) justified by cosmic order, no exit
 *   - analytical_historians: Observer (analytical/analytical) — analyzes textual and archaeological evidence to reconstruct the constraint's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.85).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.95).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, mountain).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Ma'at as Divine Mandate of Pharaoh").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).
domain_priors:emerges_naturally(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, '92ae3470-1a80-4549-a187-3c8fcb490165').
narrative_ontology:cs_kernel_codification('92ae3470-1a80-4549-a187-3c8fcb490165', implicit).
narrative_ontology:cs_authority_grounding('92ae3470-1a80-4549-a187-3c8fcb490165', lineage).
narrative_ontology:cs_interpretation_layer_present('92ae3470-1a80-4549-a187-3c8fcb490165').
narrative_ontology:cs_reading_relation('92ae3470-1a80-4549-a187-3c8fcb490165', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('92ae3470-1a80-4549-a187-3c8fcb490165', maat_order_principle__distributed_maintenance_reading, forecloses).
narrative_ontology:cs_axiom('92ae3470-1a80-4549-a187-3c8fcb490165', foundational, pharaoh_is_source_of_maat).
narrative_ontology:cs_axiom_status(pharaoh_is_source_of_maat, holdable).
narrative_ontology:cs_axiom_grounding('92ae3470-1a80-4549-a187-3c8fcb490165', pharaoh_is_source_of_maat, theological).
narrative_ontology:cs_axiom('92ae3470-1a80-4549-a187-3c8fcb490165', foundational, royal_action_cannot_violate_maat).
narrative_ontology:cs_axiom_status(royal_action_cannot_violate_maat, holdable).
narrative_ontology:cs_axiom_grounding('92ae3470-1a80-4549-a187-3c8fcb490165', royal_action_cannot_violate_maat, deontological).
narrative_ontology:cs_reference_frame('92ae3470-1a80-4549-a187-3c8fcb490165', pharaonic_divine_kingship).
narrative_ontology:cs_drift_state('92ae3470-1a80-4549-a187-3c8fcb490165', late_period_decline, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('92ae3470-1a80-4549-a187-3c8fcb490165', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, priestly_elite).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, commoners).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, scribal_bureaucracy).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, ExtMetricName, E),
    domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(maat_order_principle__divine_mandate_reading),
    narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the system justifies substantial resource transfer (labor, taxes) to the Pharaoh and state as necessary for cosmic order. Suppression is extremely high (0.95) as any challenge to Pharaoh's embodiment of Ma'at is a challenge to cosmic order itself, met with severe religious and political penalties. Theater ratio is moderate (0.6) as while some royal actions genuinely maintain order, a significant portion of ritual and propaganda serves to reinforce the divine mandate and justify extraction. The claimed type is 'mountain' because this reading presents Ma'at as an unchangeable, natural cosmic law, with Pharaoh as its irreducible conduit.
 *
 * PERSPECTIVAL GAP:
 *   From the Pharaoh's and priestly elite's perspective, this is a Mountain, an unchangeable cosmic truth. From the commoners' perspective, it is a Snare, an extractive system enforced by divine authority. The engine will compute this divergence based on the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh is the ultimate beneficiary (d=0.0) as the divine conduit of Ma'at, from whom all order and legitimacy flow, and who collects the system's benefits. The priestly elite are also beneficiaries (d=0.1) as interpreters and enforcers of this divine order. Commoners are clear targets (d=1.0), bearing the costs of the system with no exit. The scribal bureaucracy is a mixed case, acting as enforcers and administrators (d=0.7) but also subject to the Pharaoh's absolute authority.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is presented as a timeless cosmic principle, making mandatrophy detection difficult. However, the high theater ratio and increasing suppression over time suggest that the 'naturalness' of the constraint requires increasing active maintenance and suppression of alternative interpretations. If the founding problem (maintaining cosmic order) were demonstrably 'dead' (e.g., if society could function without this specific royal mediation), the constraint would reclassify from a claimed Mountain to a Snare, as its persistence would then rely solely on coercion and extraction, not natural necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_legitimacy,
    'Is Ma''at, as embodied by Pharaoh, a genuine natural law of cosmic order, or a constructed constraint that benefits identifiable agents (Pharaoh, priestly elite)?',
    'Archaeological and textual analysis of pre-dynastic social structures for evidence of Ma''at-like principles independent of royal authority; comparative study of other early state formations.',
    'If genuinely natural, its classification as Mountain is robust. If constructed, it reclassifies as a Snare or Tangled Rope, with Pharaoh and the priestly elite as primary beneficiaries of a legitimation narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_legitimacy, empirical, 'Ambiguity between Ma''at as natural law and as a constructed legitimation for royal power.').

omega_variable(
    pharaoh_as_source_or_subject,
    'Does Pharaoh embody Ma''at as its source, or is Pharaoh also subject to Ma''at''s principles?',
    'Analysis of royal decrees and wisdom literature for instances where Pharaoh is explicitly criticized or held accountable to Ma''at by non-royal sources.',
    'If Pharaoh is the source, the constraint is a Mountain from Pharaoh''s seat, justifying extraction. If Pharaoh is also subject, it implies a reciprocal obligation, shifting the constraint towards a Tangled Rope or Rope for Pharaoh.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pharaoh_as_source_or_subject, conceptual, 'This reading positions Pharaoh as the source of Ma''at, not subject to it, which differs from other readings where Pharaoh has reciprocal obligations.').

omega_variable(
    sibling_reading_impact_reciprocity,
    'How would the ''reciprocity_reading'' of Ma''at alter the structural relationship between Pharaoh and society?',
    'Comparative analysis of textual evidence supporting the ''reciprocity_reading'' and its implications for royal accountability.',
    'The ''reciprocity_reading'' would introduce mutual obligations, shifting the constraint from a Mountain (for Pharaoh) to a Tangled Rope, where Pharaoh''s benefits are tied to providing justice and stability to commoners.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_reciprocity, conceptual, 'Impact of the ''reciprocity_reading'' on Pharaoh''s accountability.').

omega_variable(
    sibling_reading_impact_distributed_maintenance,
    'How would the ''distributed_maintenance_reading'' of Ma''at alter the locus of responsibility for cosmic order?',
    'Analysis of community-level religious practices and local legal traditions for evidence of distributed responsibility for Ma''at.',
    'The ''distributed_maintenance_reading'' would decentralize the responsibility for Ma''at, potentially reducing the Pharaoh''s unique role as its sole embodiment and diffusing the justification for royal extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_distributed_maintenance, conceptual, 'Impact of the ''distributed_maintenance_reading'' on the locus of responsibility for Ma''at.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__divine_mandate_reading, theater_ratio, 100, 0.5).
narrative_ontology:measurement(maat_tr_t200, maat_order_principle__divine_mandate_reading, theater_ratio, 200, 0.6).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__divine_mandate_reading, base_extractiveness, 100, 0.8).
narrative_ontology:measurement(maat_be_t200, maat_order_principle__divine_mandate_reading, base_extractiveness, 200, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__divine_mandate_reading, suppression_requirement, 100, 0.88).
narrative_ontology:measurement(maat_su_t200, maat_order_principle__divine_mandate_reading, suppression_requirement, 200, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(maat_order_principle__divine_mandate_reading, 0.08).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__distributed_maintenance_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, pharaonic_taxation_system).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, temple_economy_structure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'maat_order_principle' kernel. This 'divine_mandate_reading' emphasizes Pharaoh's role as the source of Ma'at, distinct from the 'reciprocity_reading' (Pharaoh subject to Ma'at) and the 'distributed_maintenance_reading' (Ma'at maintained by all society).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
