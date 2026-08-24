% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__constitutional_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Constitutional Subordination of Provinces (Federal Veto over Exit)
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint story captures the 'constitutional subordination' reading
 *   of the provincial sovereignty boundary kernel. It asserts that provinces
 *   derive their existence and powers solely from the federal Constitution
 *   (Constitution Act 1867, 1982), possess no inherent sovereignty, and
 *   cannot exit the federation without federal consent. The reading
 *   vindicates federal supremacy in equalization and climate policy and
 *   treats separatism as a constitutional nullity. The claimed type is
 *   tangled_rope because the arrangement performs a genuine coordination
 *   function (national unity, fiscal federalism, climate coordination) while
 *   simultaneously extracting sovereign authority from provinces that would
 *   otherwise claim it — especially resource-rich and separatist-leaning
 *   provinces. The engine will compute per-seat classifications from the
 *   structural data below.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.62).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.71).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.62).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.53).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Constitutional Subordination of Provinces (Federal Veto over Exit)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, 'afed2a3e-82dd-4a55-ad73-6b9ee47111c0').
narrative_ontology:cs_kernel_codification('afed2a3e-82dd-4a55-ad73-6b9ee47111c0', formalized).
narrative_ontology:cs_authority_grounding('afed2a3e-82dd-4a55-ad73-6b9ee47111c0', lineage).
narrative_ontology:cs_interpretation_layer_present('afed2a3e-82dd-4a55-ad73-6b9ee47111c0').
narrative_ontology:cs_reading_relation('afed2a3e-82dd-4a55-ad73-6b9ee47111c0', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('afed2a3e-82dd-4a55-ad73-6b9ee47111c0', provincial_sovereignty_boundary__resource_sovereignty_primacy, forecloses).
narrative_ontology:cs_axiom('afed2a3e-82dd-4a55-ad73-6b9ee47111c0', foundational, federal_supremacy_in_constitution).
narrative_ontology:cs_axiom_status(federal_supremacy_in_constitution, holdable).
narrative_ontology:cs_axiom_grounding('afed2a3e-82dd-4a55-ad73-6b9ee47111c0', federal_supremacy_in_constitution, conventional).
narrative_ontology:cs_axiom('afed2a3e-82dd-4a55-ad73-6b9ee47111c0', foundational, provincial_subordination_to_federal_veto).
narrative_ontology:cs_axiom_status(provincial_subordination_to_federal_veto, holdable).
narrative_ontology:cs_axiom_grounding('afed2a3e-82dd-4a55-ad73-6b9ee47111c0', provincial_subordination_to_federal_veto, conventional).
narrative_ontology:cs_reference_frame('afed2a3e-82dd-4a55-ad73-6b9ee47111c0', federal_constitutional_order).
narrative_ontology:cs_drift_state('afed2a3e-82dd-4a55-ad73-6b9ee47111c0', contemporary, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('afed2a3e-82dd-4a55-ad73-6b9ee47111c0', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, centralist_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, separatist_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_provinces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, resource_provinces).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, federal_supremacy_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, constitutional_union_indivisibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the constitutional authority to veto provincial exit and to legislate in areas of equalization and climate policy. Collects the political and fiscal benefits of a unified federation. Controls the appointment of judges who interpret the constitutional boundary. Exit from this role is not applicable; the federal government is the architect and enforcer of the constraint.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__constitutional_subordination, federal_government, beneficiary).

% Provinces that benefit from federal equalization transfers and national policy frameworks. They support the federal veto because it stabilizes the fiscal union and ensures resource redistribution. Their exit options are high — they could theoretically join a different federation or become independent, but they have no incentive to do so.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, centralist_provinces, beneficiary,
    organized, biographical, mobile, regional).

% Provinces with strong sovereignty movements (e.g., Quebec historically). They bear the cost of being unable to exit unilaterally; the federal veto makes secession legally impossible without federal consent. They pay through lost autonomy and the inability to control their full resource revenues. Exit is constrained: they can hold referendums and negotiate, but the federal government sets the terms.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, separatist_provinces, payer,
    organized, biographical, constrained, regional).

% Resource-rich provinces (e.g., Alberta, Saskatchewan) that own natural resources under s.92A but face federal climate policy and equalization that redistributes resource wealth. They benefit from the federation's market access and stability but pay through federal overrides on environmental regulation and fiscal transfers. Exit is constrained by economic integration and federal legal barriers.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_provinces, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__constitutional_subordination, resource_provinces, beneficiary).

% Not parties to the Constitution Act 1867 or 1982; their inherent sovereignty is not recognized in the federal-provincial division of powers. They are affected by both federal and provincial laws but have no formal seat in the constitutional amending formula. Their exit from the Canadian state is not contemplated in this constraint; they are structurally excluded from the sovereignty boundary debate.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, indigenous_nations, excluded,
    powerless, generational, trapped, national).

% Academic and legal observers who analyze the constitutional text, jurisprudence, and political practice. They do not collect rents nor bear costs from the constraint directly. Their role is to map the competing readings (compact federalism, constitutional subordination, resource sovereignty primacy) and assess their coherence.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__constitutional_subordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a unified federal state capable of national policies (equalization, climate, trade) by preventing unilateral provincial exit and establishing federal veto over sovereignty claims.
% TRANSFER_FUNCTION: Moves sovereign authority over exit and key policy domains (equalization, climate) from provinces to the federal government, consolidating decision-making at the centre.
% ABSENT_VOICES: Indigenous nations, who are not parties to the constitutional division of powers but are affected by federal-provincial jurisdiction disputes; municipal governments, who have no constitutional status but bear implementation costs.
% DISAPPEARANCE_RATIONALE: If the federal veto over provincial exit and the subordination of provinces were removed, the federation would become a voluntary association; provinces could unilaterally secede, equalization and national climate policy would collapse, and the Canadian state would likely dissolve or radically reorganize.
% FOUNDING_PROBLEM: The need to create a unified political entity from distinct colonies with divergent interests, ensuring national coherence and preventing fragmentation, while accommodating regional diversity through a federal distribution of powers.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Fathers of Confederation records (e.g., Macdonald's speeches) and the constitutional text itself. However, the status is contested: federalists argue the problem of national unity remains live; provincial sovereigntists argue the founding problem was a compact among equals that has been betrayed.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__constitutional_subordination, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.62) reflects the transfer of exit authority and policy control from provinces to the federal centre. Suppression (0.71) is high because the legal barrier to exit is absolute (Reference re Secession of Quebec) and enforced by the Supreme Court. Theater ratio (0.32) is moderate: the federal government performs 'cooperative federalism' rituals (first ministers' conferences) but the veto power is real and used. Accessibility collapse (0.64) is significant: once the constitutional rule is understood, unilateral exit is legally foreclosed. Resistance (0.53) is substantial: Quebec referendums, Alberta Sovereignty Act, and ongoing judicial challenges show active pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the federal seat, the constraint is a coordination mechanism that makes the federation work. From the separatist province seat, it is a snare that traps them in an unwanted union. From the resource province seat, it is a tangled rope: they get market access but lose climate policy control. The engine's per-seat computation will reveal this divergence; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is the primary beneficiary (d near 0.0) — it collects the authority to veto exit and set national policy. Centralist provinces are secondary beneficiaries (d ~ 0.2) — they gain fiscal stability. Separatist provinces are primary targets (d near 1.0) — they are legally barred from exiting. Resource provinces are also targets (d ~ 0.7) — they lose policy autonomy over resources. Indigenous nations are excluded (d undefined) — they are not parties to the constraint but are structurally suppressed. Constitutional scholars are analytical observers (d = 0.5). The engine will derive these directionalities from the beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (national unity) remains contested. The constraint has not atrophied into a piton because the federal government actively uses the veto (e.g., carbon pricing upheld, equalization maintained). However, the rising theater ratio suggests performative maintenance is increasing. If the coordination function (national unity) ceases to be live for a critical mass of provinces, the constraint could drift toward snare (pure extraction) or piton (inertial maintenance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'Is this constraint a reading of the contested kernel ''provincial_sovereignty_boundary''?',
    'Compare with sibling readings compact_federalism and resource_sovereignty_primacy to identify structural differences in beneficiary/victim sets, exit options, and claimed_type.',
    'If this is a kernel reading, its classification is reading-indexed; the kernel itself has no single classification. The engine must treat each reading as a separate constraint story linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Commitment that this constraint is one reading of a contested kernel.').

omega_variable(
    federal_veto_legitimacy,
    'Is the federal veto over provincial exit a genuine coordination mechanism for national unity or a tool for central extraction?',
    'Historical analysis of federal use of veto power; counterfactual simulation of federation without veto; compare with federations that allow unilateral exit (e.g., EU Article 50).',
    'If coordination, supports tangled_rope; if extraction, supports snare. Affects claimed_type and mandatrophy assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_veto_legitimacy, empirical, 'Legitimacy of federal veto as coordination vs extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of provincial exit structural (legal barriers) or internalized (political culture)?',
    'Study provincial secession referendums and federal response; measure political culture after legal barriers removed (hypothetical).',
    'If internalized, effective suppression higher than legal measure; the constraint''s persistence may rely on internalized federalism as much as legal veto.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of provincial exit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 0, 157).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_sov_const_sub_tr_t0, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0, 0.15).
narrative_ontology:measurement(prov_sov_const_sub_tr_t30, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 30, 0.18).
narrative_ontology:measurement(prov_sov_const_sub_tr_t60, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 60, 0.22).
narrative_ontology:measurement(prov_sov_const_sub_tr_t90, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 90, 0.26).
narrative_ontology:measurement(prov_sov_const_sub_tr_t120, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 120, 0.29).
narrative_ontology:measurement(prov_sov_const_sub_tr_t150, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 150, 0.31).
narrative_ontology:measurement(prov_sov_const_sub_tr_t157, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 157, 0.32).

% Extraction over time
narrative_ontology:measurement(prov_sov_const_sub_be_t0, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(prov_sov_const_sub_be_t30, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(prov_sov_const_sub_be_t60, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(prov_sov_const_sub_be_t90, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 90, 0.56).
narrative_ontology:measurement(prov_sov_const_sub_be_t120, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 120, 0.59).
narrative_ontology:measurement(prov_sov_const_sub_be_t150, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 150, 0.61).
narrative_ontology:measurement(prov_sov_const_sub_be_t157, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 157, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(prov_sov_const_sub_su_t0, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(prov_sov_const_sub_su_t30, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(prov_sov_const_sub_su_t60, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(prov_sov_const_sub_su_t90, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 90, 0.66).
narrative_ontology:measurement(prov_sov_const_sub_su_t120, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 120, 0.69).
narrative_ontology:measurement(prov_sov_const_sub_su_t150, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 150, 0.7).
narrative_ontology:measurement(prov_sov_const_sub_su_t157, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 157, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__constitutional_subordination, 0.12).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, equalization_payments).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, federal_climate_policy_authority).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_resource_revenue_control).

% DUAL FORMULATION NOTE:
% This constraint is the constitutional_subordination reading of the provincial_sovereignty_boundary kernel. It forecloses the compact_federalism and resource_sovereignty_primacy readings. The three readings form a constraint family linked by mutual structural contradiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
