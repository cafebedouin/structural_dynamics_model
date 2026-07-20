% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__constitutional_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__constitutional_subordination, []).

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
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Provincial Sovereignty Boundary â Constitutional Subordination Reading
 *   domain: political/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the constitutional_subordination
 *   reading of the provincial_sovereignty_boundary kernel. Under this
 *   reading, provinces are constitutionally created and subordinate entities
 *   possessing no sovereignty independent of the federal constitutional
 *   framework. Unilateral exit is constitutionally null; federal consent is
 *   required for amendment or secession. The reading vindicates federal
 *   paramountcy in equalization, climate policy, and resource governance,
 *   while truncating provincial autonomy claims. It is one of three contested
 *   readings of the same kernel, alongside compact_federalism (provinces as
 *   sovereign parties to a compact retaining residual sovereignty) and
 *   resource_sovereignty_primacy (resource ownership as grounding absolute
 *   provincial sovereignty). The authored metrics and claimed type are
 *   independent: the constraint is claimed as tangled_rope because it
 *   combines genuine coordination of a federal state with asymmetric
 *   extraction of sovereignty from provinces, but the metrics describe a
 *   heavily enforced, substantially extractive arrangement.
 *
 * KEY AGENTS:
 *   - federal_executive: agenda_setter (institutional/arbitrage) â administers constitutional supremacy and captures sovereignty transfers
 *   - resource_rich_provinces: primary target (powerful/constrained) â bear truncated sovereignty and resource rent extraction
 *   - have_not_provinces: beneficiary (moderate/constrained) â receive redistributed fiscal capacity via federal authority
 *   - separatist_movements: secondary target (moderate/trapped) â bear constitutional nullification of independence claims
 *   - supreme_court: analytical observer (institutional/analytical) â adjudicates and legitimizes the constitutional subordination framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.72).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.78).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.72).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Provincial Sovereignty Boundary â Constitutional Subordination Reading").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, '0cf4b47d-1792-45c2-a9e5-ffcdd61cb7ca').
narrative_ontology:cs_kernel_codification('0cf4b47d-1792-45c2-a9e5-ffcdd61cb7ca', formalized).
narrative_ontology:cs_authority_grounding('0cf4b47d-1792-45c2-a9e5-ffcdd61cb7ca', lineage).
narrative_ontology:cs_interpretation_layer_present('0cf4b47d-1792-45c2-a9e5-ffcdd61cb7ca').
narrative_ontology:cs_reading_relation('0cf4b47d-1792-45c2-a9e5-ffcdd61cb7ca', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('0cf4b47d-1792-45c2-a9e5-ffcdd61cb7ca', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('0cf4b47d-1792-45c2-a9e5-ffcdd61cb7ca', foundational, provincial_creaturehood_doctrine).
narrative_ontology:cs_axiom_status(provincial_creaturehood_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('0cf4b47d-1792-45c2-a9e5-ffcdd61cb7ca', provincial_creaturehood_doctrine, conventional).
narrative_ontology:cs_axiom('0cf4b47d-1792-45c2-a9e5-ffcdd61cb7ca', foundational, federal_consent_requirement_for_exit).
narrative_ontology:cs_axiom_status(federal_consent_requirement_for_exit, holdable).
narrative_ontology:cs_axiom_grounding('0cf4b47d-1792-45c2-a9e5-ffcdd61cb7ca', federal_consent_requirement_for_exit, conventional).
narrative_ontology:cs_reference_frame('0cf4b47d-1792-45c2-a9e5-ffcdd61cb7ca', constitutional_union_supremacy).
narrative_ontology:cs_drift_state('0cf4b47d-1792-45c2-a9e5-ffcdd61cb7ca', contemporary_resource_nationalism_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0cf4b47d-1792-45c2-a9e5-ffcdd61cb7ca', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_executive).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, have_not_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, federal_paramountcy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the constitutional order, controls the amendment formula, and asserts federal paramountcy over equalization, climate policy, and provincial exit. Can shift jurisdictional boundaries through spending power, reference questions, and judicial appointments.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_executive, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive equalization transfers and federal program funding; their fiscal stability depends on federal authority to redistribute resource rents from richer jurisdictions. They defend the constitutional status quo against provincial autonomy claims.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, have_not_provinces, beneficiary,
    moderate, generational, constrained, national).

% Own natural resources under section 92A but face federal climate regulation and equalization formulas that capture resource rents. Constitutional amendment and unilateral exit are blocked by federal consent requirements and judicial interpretations of constitutional supremacy.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces, payer,
    powerful, generational, constrained, national).

% Seek sovereign statehood or substantial autonomy; constitutionally characterized as illegitimate unless pursued through federal amendment. Political expression is permitted but the constitutional path to independence is barred by federal veto structures and clarity-act thresholds.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements, payer,
    moderate, generational, trapped, regional).

% Adjudicates division-of-powers disputes and the legality of secession. Its jurisprudence affirms that provinces are constitutionally subordinate and that unilateral exit is illegal, while mandating political negotiation only after a clear referendum expression.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, supreme_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__constitutional_subordination, federal_executive).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__constitutional_subordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a unified federal state by establishing a clear constitutional hierarchy, preventing unilateral provincial fragmentation, and enabling national-scale redistribution, defense, and regulatory frameworks.
% TRANSFER_FUNCTION: Moves sovereignty, fiscal capacity, and resource rents from provinces to the federal center, and redistributes wealth from resource-producing regions to recipient regions through federal equalization and climate policy authority.
% ABSENT_VOICES: Indigenous nations asserting inherent sovereignty and sovereigntist movements advocating unilateral secession are structurally marginalized; they would reject the federal-consent gate on self-determination but are treated as constitutional nullities.
% DISAPPEARANCE_RATIONALE: If provincial sovereignty were recognized as inherent and federal veto over exit vanished, the federation would likely fragment or reorganize into a confederal or multi-state arrangement; federal redistribution and national climate policy would lose their constitutional footing.
% FOUNDING_PROBLEM: Pre-Confederation fragmentation and weak central authority; the need to forge a unified state capable of national defense, internal trade, and collective action without dissolving into hostile regional fragments.
% FOUNDING_PROBLEM_CORROBORATION: Centralist constitutional historians and federal politicians attest the problem remains live. Provincial premiers and sovereigntist scholars attest the founding crisis is resolved and the arrangement now serves federal aggrandizement. Independent academic constitutionalists outside the immediate beneficiary set offer mixed corroboration.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__constitutional_subordination, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint systematically transfers sovereignty, fiscal capacity, and resource control from provinces to the federal center. Suppression is higher (0.78) because the constraint's persistence depends on active constitutional enforcement: judicial nullification of unilateral secession, clarity-act thresholds, and amendment-formula veto. Accessibility collapse is very high (0.82) because once the constitutional frame is accepted, legal alternatives to federal subordination effectively vanish. Resistance is moderate (0.58) because provincial governments and separatist movements mount ongoing political and legal challenges. Theater ratio (0.42) reflects the growing performative dimension of 'cooperative federalism' discourse that masks constitutional asymmetry. The temporal series run on a single shared grid so every metric is sampled at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The federal executive seat experiences the constraint as necessary coordination that preserves national unity and enables redistribution; the resource-rich province and separatist seats experience the same structure as enforced extraction of self-determination. The have-not province seat experiences it as beneficial coordination. The engine computes this divergence from the structural relationship declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal executive is the structural beneficiary and agenda-setter (low d, near full beneficiary). Have-not provinces are beneficiaries of redistribution (low-moderate d). Resource-rich provinces and separatist movements are the targets: they bear the sovereignty truncation and their exit is blocked (high d, near full target). The Supreme Court sits at analytical distance (d near 0.5, symmetric observation). No overrides are necessary because the structural derivation matches the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the constraint as a pure rope (which would ignore the asymmetric sovereignty extraction from provinces) or as a pure snare (which would ignore the genuine coordination function of maintaining a federal state and enabling national redistribution). The temporal measurements show extraction and theater rising over the interval, suggesting the coordination function has not atrophied into a piton, but that enforcement has intensified as resistance has grown.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_sovereignty_or_construct,
    'Is provincial constitutional subordination a necessary structural feature of any federation, or a constructed extraction by the federal center to capture resource rents and suppress exit?',
    'Comparative constitutional analysis of federations with explicit secession clauses (e.g. Ethiopia, Saint Kitts and Nevis) versus withheld-consent systems; measure whether exit-blockage correlates with resource transfer from subunits to center.',
    'If constructed, the constraint shifts toward snare classification; if structurally necessary, it remains tangled_rope or moves toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_sovereignty_or_construct, conceptual, 'Whether provincial subordination is natural law of federation or constructed extraction').

omega_variable(
    resource_ownership_sovereignty_tension,
    'Does provincial ownership of natural resources under section 92A create a de facto sovereignty that contradicts the constitutional subordination reading?',
    'Jurisprudential tracking of s.92A cases versus federal paramountcy assertions; measure whether resource ownership has translated into veto power over federal climate or environmental policy.',
    'If resource ownership functions as effective sovereignty, the constitutional subordination reading is partially overridden and effective extractiveness is lower than measured; if federal paramountcy consistently prevails, the reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_ownership_sovereignty_tension, empirical, 'Resource ownership versus constitutional supremacy tension').

omega_variable(
    negotiation_theater_vs_substance,
    'Does the Supreme Court''s ''duty to negotiate'' following a clear referendum function as a genuine coordination mechanism or as a theatrical veto preserving federal supremacy?',
    'Counterfactual analysis of post-referendum negotiations (Quebec 1995, Scotland 2014) versus constraints in systems with no negotiation duty; assess whether the duty altered the power distribution or merely ritualized federal control.',
    'If theatrical, theater_ratio is understated and the constraint leans toward snare; if substantive, it tempers extraction with genuine exit-path coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(negotiation_theater_vs_substance, empirical, 'Duty to negotiate as theater or genuine coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t0, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0, 0.16).
narrative_ontology:measurement(prov_tr_t5, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 5, 0.2).
narrative_ontology:measurement(prov_tr_t10, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 10, 0.24).
narrative_ontology:measurement(prov_tr_t15, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 15, 0.28).
narrative_ontology:measurement(prov_tr_t20, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 20, 0.32).
narrative_ontology:measurement(prov_tr_t25, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 25, 0.35).
narrative_ontology:measurement(prov_tr_t30, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 30, 0.38).
narrative_ontology:measurement(prov_tr_t35, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 35, 0.4).
narrative_ontology:measurement(prov_tr_t40, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(prov_be_t0, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(prov_be_t5, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(prov_be_t10, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(prov_be_t15, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(prov_be_t20, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(prov_be_t25, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 25, 0.63).
narrative_ontology:measurement(prov_be_t30, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(prov_be_t35, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 35, 0.7).
narrative_ontology:measurement(prov_be_t40, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t0, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(prov_su_t5, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(prov_su_t10, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(prov_su_t15, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(prov_su_t20, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(prov_su_t25, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(prov_su_t30, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(prov_su_t35, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 35, 0.76).
narrative_ontology:measurement(prov_su_t40, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% The provincial_sovereignty_boundary kernel decomposes into three structurally distinct constraints. This file (constitutional_subordination) models the federal-supremacy reading with high extraction and active enforcement. Sibling files model the compact-theory and resource-sovereignty readings. All three share a regulatory domain but have different epsilon values, beneficiary/victim structures, and classification types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
