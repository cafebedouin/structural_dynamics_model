% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty: Capacity-Based Reclassification as Neo-Colonial Extraction
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The graduated sovereignty reading asserts that state authority on the
 *   international stage exists on a spectrum determined by institutional
 *   capacity and governance legitimacy. High-capacity states retain full
 *   sovereignty and intervention authority; low-capacity states are
 *   classified as partially sovereign or temporarily subordinate to external
 *   governance partners. Framed as helping weak states build institutional
 *   capacity, the reading functions as a mechanism by which high-capacity
 *   actors gain discretion to intervene, reclassify, and extract resources
 *   from low-capacity territories while preserving the appearance of
 *   legitimacy. The constraint is CLAIMED as a snare (pure extraction with
 *   suppressed alternatives) while the kernel context presents it as one
 *   reading of a contested framework about what sovereignty means. The
 *   sibling readings (absolute: unconditional state authority; conditional:
 *   responsibility-triggered intervention) coexist with this graduated
 *   reading in actual international practice, making the kernel itself the
 *   contested ground.
 *
 * KEY AGENTS:
 *   - High-capacity states: institutional agenda-setters who define and enforce capacity standards; beneficiaries of reclassification authority
 *   - Low-capacity states: structural targets of reclassification; victims bearing the costs of intervention and policy subordination
 *   - External intervention authorities: institutional beneficiaries who gain legitimized discretion to intervene and govern; secondary agenda-setters
 *   - Marginalized populations in reclassified states: direct victims of intervention, occupation, and resource redistribution
 *   - Development professionals: intermediate beneficiary class deriving careers and legitimacy from capacity-building missions
 *   - Excluded alternative sovereigns (non-state actors, indigenous governance): structurally locked out of the sovereignty classification system
 *   - Scholarly community: analytical observers documenting the framework's operation and contesting its justifications
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.62).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.71).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.62).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty: Capacity-Based Reclassification as Neo-Colonial Extraction").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, 'a750407e-eea1-4ea3-a878-0ba43cca0d6a').
narrative_ontology:cs_kernel_codification('a750407e-eea1-4ea3-a878-0ba43cca0d6a', formalized).
narrative_ontology:cs_authority_grounding('a750407e-eea1-4ea3-a878-0ba43cca0d6a', extraction).
narrative_ontology:cs_interpretation_layer_present('a750407e-eea1-4ea3-a878-0ba43cca0d6a').
narrative_ontology:cs_reading_relation('a750407e-eea1-4ea3-a878-0ba43cca0d6a', westphalian_sovereignty__absolute_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('a750407e-eea1-4ea3-a878-0ba43cca0d6a', westphalian_sovereignty__conditional_sovereignty, influences).
narrative_ontology:cs_axiom('a750407e-eea1-4ea3-a878-0ba43cca0d6a', foundational, sovereignty_capacity_determined).
narrative_ontology:cs_axiom_status(sovereignty_capacity_determined, holdable).
narrative_ontology:cs_axiom_grounding('a750407e-eea1-4ea3-a878-0ba43cca0d6a', sovereignty_capacity_determined, empirically_contingent).
narrative_ontology:cs_axiom('a750407e-eea1-4ea3-a878-0ba43cca0d6a', foundational, external_authority_to_reclassify).
narrative_ontology:cs_axiom_status(external_authority_to_reclassify, holdable).
narrative_ontology:cs_axiom_grounding('a750407e-eea1-4ea3-a878-0ba43cca0d6a', external_authority_to_reclassify, conventional).
narrative_ontology:cs_reference_frame('a750407e-eea1-4ea3-a878-0ba43cca0d6a', unconditional_westphalian_equality).
narrative_ontology:cs_drift_state('a750407e-eea1-4ea3-a878-0ba43cca0d6a', contemporary_post_2001, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a750407e-eea1-4ea3-a878-0ba43cca0d6a', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, high_capacity_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, external_intervention_authorities).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, low_capacity_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, marginalized_populations_in_reclassified_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, development_and_governance_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess institutional infrastructure, economic resources, and military capacity sufficient to be classified as 'fully sovereign' under graduated sovereignty framework. Define and enforce the standards by which state capacity is measured. Retain authority to intervene in territories classified as lacking capacity, preserving legitimacy for intervention as governance aid rather than conquest.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, high_capacity_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Lack the institutional infrastructure, financial resources, or military capacity that the graduated sovereignty framework treats as prerequisites for full sovereignty. Subject to external reclassification: their status can be downgraded if they fail institutional benchmarks, triggering intervention authority. The exit from this position (building capacity fast enough to meet moving standards) is structurally difficult and requires accepting temporary subordination.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, low_capacity_states, payer,
    powerless, generational, trapped, global).

% International institutions (UN Security Council permanent members, regional organizations, donor-backed organizations) gain legitimized discretion to classify states and authorize intervention in territories deemed to lack capacity. Frame intervention as capacity-building and governance assistance. Collect geopolitical advantage, resource access, and institutional expansion as secondary benefits.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, external_intervention_authorities, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, external_intervention_authorities, agenda_setter).

% Bear direct costs of reclassification: military intervention, occupation infrastructure, conditional aid that comes with institutional restructuring demands, displacement from resources prioritized for intervention logistics. Have little voice in how their state is classified or what 'capacity' means; their sovereignty is doubly suspended—both by their own weak state and by external reclassifiers.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, marginalized_populations_in_reclassified_states, payer,
    powerless, immediate, trapped, local).

% Emerge as an intermediate class deriving careers, funding, and institutional legitimacy from the capacity-building mission. Set and adjudicate benchmarks for state capacity. Benefit from the flow of development aid and intervention authority; their professional standing depends on the graduated sovereignty framework remaining operative.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, development_and_governance_professionals, beneficiary,
    moderate, biographical, constrained, global).

% Non-state actors (liberation movements, autonomous communities, indigenous governance structures) that claim legitimate authority but fall outside the capacity-based classification scheme. Structurally excluded because the graduated framework only recognizes state-form sovereignty; their exclusion is enforced by the same intervention apparatus that reclassifies weak states.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, excluded_alternative_sovereigns, excluded,
    moderate, biographical, trapped, global).

% Comprises international law scholars, historians, and political theorists who document and contest the graduated sovereignty framework. Produce scholarship critiquing the concept and its application; their analysis is occasionally cited in policy but lacks enforcement authority.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, observer_scholarly_community, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__graduated_sovereignty, external_intervention_authorities).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__graduated_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a graduated classification system for state authority to reduce the friction of managing vastly unequal state capacities in a nominally equal international system. Allows high-capacity states to intervene in low-capacity territories under a framework that treats intervention as helping rather than conquest.
% TRANSFER_FUNCTION: Moves geopolitical authority, resource access rights, and structural legitimacy from low-capacity states to high-capacity interveners. Channels development aid and intervention resources toward territories classified as deficient; the same territories lose policy discretion and executive authority over those resources.
% ABSENT_VOICES: Low-capacity states have minimal voice in how capacity benchmarks are set or applied; their populations affected by reclassification have no representative role; non-state sovereigns and indigenous governance structures are structurally excluded from the classification scheme itself and would contest the entire framework if present.
% DISAPPEARANCE_RATIONALE: If graduated sovereignty and its reclassification authority vanished, the international system would revert to nominal equality under Westphalian absolute sovereignty (or transition to conditional sovereignty if responsibility norms held). Intervention authority would lose its legitimating framework, geopolitical advantage tied to capacity classification would evaporate, and low-capacity states would regain policy discretion—though their material weakness would persist. Development institutions would undergo major restructuring.
% FOUNDING_PROBLEM: Post-Cold War international order required a framework for managing intervention in failed and failing states without openly reviving colonialism. Graduated sovereignty offered a language of capacity and governance legitimacy to distinguish between helping and conquering.
% FOUNDING_PROBLEM_CORROBORATION: High-capacity states and development institutions (World Bank, UNDP, UN Peacebuilding Commission) attest the founding problem remains live: state fragility, humanitarian crises, and security vacuums require intervention frameworks and capacity support. Low-capacity states (especially those repeatedly reclassified and intervened upon) and critical international law scholars attest the problem has been instrumentalized: the framework is now primarily used to justify extraction and control, with capacity benchmarks set and moved by interveners to maintain their discretion. Testimony from government officials in frequently-reclassified states and analysis from post-colonial scholars provide corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.62) because the graduated sovereignty framework enables high-capacity actors to extract geopolitical authority, resource access, and policy control from low-capacity states while claiming to help them. The measurement shows steady accumulation from 1991 (end of Cold War, when the framework emerged) through 2025, with the steepest rises in 1991-2001 (post-Cold War interventions) and 2001-2011 (Global War on Terror expansion of intervention authority). Suppression is high (0.71) because the constraint's persistence depends on actively preventing low-capacity states from opting out of reclassification or asserting unconditional sovereignty; alternatives (absolute sovereignty doctrine, non-intervention norms) are suppressed through institutional structures that marginalize low-capacity state voices. Theater is moderate-high (0.48) because a substantial portion of intervention activity is performative (capacity-building rhetoric, governance assistance framing) while the underlying extraction mechanism is the authority to classify and intervene. The graduated framework allows high-capacity states to claim they are helping while exercising authority that is functionally indistinguishable from the colonialism they claim to have rejected.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (low-capacity states, marginalized populations) experience graduated sovereignty as a mechanism of control and extraction; the agenda-setter seats (high-capacity states, intervention authorities) experience it as legitimate governance assistance. The scholarly observer seat sees the framework as contested—a reading of the Westphalian sovereignty kernel that coexists with absolute and conditional readings but has become institutionally dominant, not through logical foreclosure but through power asymmetry. The development professional seat experiences institutional pressure to perpetuate the categories even as they claim to work toward their dissolution. These perspective divergences should produce different computed constraint types: from the payer seat, a snare (extraction with suppressed alternatives); from the beneficiary seat, a rope (coordination with asymmetry); from the observer seat, a tangled_rope (both coordination and extraction, with the coordination function (helping weak states) subordinated to the extraction mechanism (authority to intervene)).
 *
 * DIRECTIONALITY LOGIC:
 *   High-capacity states and external intervention authorities sit near the beneficiary end of the directionality spectrum (d ≈ 0.1-0.2): they define standards, retain authority to intervene, and collect geopolitical advantage without themselves being subject to reclassification. Low-capacity states sit at the target end (d ≈ 0.85-0.95): they are subject to reclassification, lose policy discretion when classified as deficient, and bear the costs of intervention and institutional restructuring. Marginalized populations sit at the extreme target end (d ≈ 0.95): they have no voice in their state's classification and bear direct costs of military intervention and resource reallocation. Development professionals sit near the symmetric point (d ≈ 0.4-0.5): they benefit institutionally but must be perceived as working toward capacity improvement, creating a nominal long-term incentive structure that conflicts with their actual short-term benefits from perpetuation. The directionality divergence between agenda-setter and payer seats is the primary mechanism by which the constraint operates: the beneficiaries design and maintain the apparatus; the targets bear the costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The graduated sovereignty framework exhibits mandatrophy candidates at multiple levels. At the institutional level: the founding problem (managing post-Cold War intervention without reviving colonialism) has been substantially solved by norm-building and humanitarian intervention doctrine, yet the graduated sovereignty apparatus persists and expands. At the development level: capacity-building has produced successful state transitions (Botswana, Rwanda post-genocide, Vietnam), yet the classification standards move upward and the intervention authority grows rather than contracts—suggesting the apparatus persists not because the founding problem remains live but because the beneficiaries have interests in its continuation. At the legitimacy level: the framework's justification depends on the claim that low-capacity states genuinely benefit from external governance, yet scholarship and testimony from low-capacity state officials increasingly reject this claim, suggesting the arrangement has become decoupled from its founding rationale. The theater ratio (0.48 at interval end) indicates that a substantial fraction of intervention activity maintains the appearance of capacity-building and governance assistance while the actual function is resource extraction and geopolitical expansion. A mandatrophy-resolved verdict would declare that the founding problem is dead but the apparatus persists—triggering the reclassification to snare as the primary structural reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    graduated_reading_foreclose_absolute,
    'Does the graduated sovereignty reading logically foreclose the absolute sovereignty reading within a single international legal framework, or do they coexist as competing readings held by different state parties?',
    'Examine whether UN Charter Article 2(1) (sovereign equality) can be interpreted consistently with graduated authority, or whether the two readings require mutually exclusive interpretations of the same text.',
    'If graduation forecloses absolute sovereignty, the readings are in logical conflict and one must be overridden as legal doctrine evolves. If both persist as live readings held by different states, the kernel is durably contested and all three readings remain structural options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(graduated_reading_foreclose_absolute, conceptual, 'Whether graduated sovereignty forecloses or coexists with absolute sovereignty.').

omega_variable(
    graduated_reading_influence_conditional,
    'Does the graduated sovereignty reading create structural pressure on the conditional sovereignty reading, making conditional intervention outcomes more extractive than they would be under a conditional-only framework?',
    'Counterfactual analysis: in a system with only conditional sovereignty (intervention triggered by specific responsibility breaches) versus a graduated system (intervention triggered by capacity classification), are low-capacity states intervened upon more frequently and more deeply in the graduated framework?',
    'If graduated creates structural pressure toward more intervention, the reading influences the conditional reading by changing the institutional environment that makes conditional intervention more likely. If they operate independently, the readings do not structurally influence each other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduated_reading_influence_conditional, empirical, 'Whether graduated sovereignty influences conditional sovereignty through expanding intervention authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1945, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1945, 0.1).
narrative_ontology:measurement_basis(west_tr_t1945, projected).
narrative_ontology:measurement(west_tr_t1991, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1991, 0.25).
narrative_ontology:measurement_basis(west_tr_t1991, observed).
narrative_ontology:measurement(west_tr_t2001, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2001, 0.35).
narrative_ontology:measurement_basis(west_tr_t2001, observed).
narrative_ontology:measurement(west_tr_t2011, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2011, 0.42).
narrative_ontology:measurement_basis(west_tr_t2011, observed).
narrative_ontology:measurement(west_tr_t2020, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2020, 0.46).
narrative_ontology:measurement_basis(west_tr_t2020, observed).
narrative_ontology:measurement(west_tr_t2025, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(west_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(west_be_t1945, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement_basis(west_be_t1945, projected).
narrative_ontology:measurement(west_be_t1991, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1991, 0.38).
narrative_ontology:measurement_basis(west_be_t1991, observed).
narrative_ontology:measurement(west_be_t2001, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2001, 0.52).
narrative_ontology:measurement_basis(west_be_t2001, observed).
narrative_ontology:measurement(west_be_t2011, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2011, 0.58).
narrative_ontology:measurement_basis(west_be_t2011, observed).
narrative_ontology:measurement(west_be_t2020, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement_basis(west_be_t2020, observed).
narrative_ontology:measurement(west_be_t2025, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(west_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1945, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement_basis(west_su_t1945, projected).
narrative_ontology:measurement(west_su_t1991, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1991, 0.48).
narrative_ontology:measurement_basis(west_su_t1991, observed).
narrative_ontology:measurement(west_su_t2001, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2001, 0.61).
narrative_ontology:measurement_basis(west_su_t2001, observed).
narrative_ontology:measurement(west_su_t2011, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2011, 0.67).
narrative_ontology:measurement_basis(west_su_t2011, observed).
narrative_ontology:measurement(west_su_t2020, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement_basis(west_su_t2020, observed).
narrative_ontology:measurement(west_su_t2025, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement_basis(west_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__graduated_sovereignty, 0.18).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, structural_adjustment_conditionality).

% DUAL FORMULATION NOTE:
% The Westphalian sovereignty kernel decomposes into three structurally distinct constraint readings: absolute_sovereignty (unconditional state authority; minimal extraction), conditional_sovereignty (responsibility-triggered intervention; moderate extraction), and graduated_sovereignty (capacity-spectrum authority; high extraction). Each reading interprets the same foundational question—what constitutes legitimate sovereign authority—differently. Graduated sovereignty is presented here as one reading instantiating a snare mechanism; sibling readings instantiate different mechanisms from the same kernel. The network edges represent how each reading's adoption affects the operability of the others: graduated readings make absolute readings harder to sustain (by introducing gradations), influence conditional readings (by expanding intervention authority beyond responsibility breaches), and enable structural_adjustment_conditionality (by justifying economic governance intervention). The three-way constraint family models the kernel as genuinely contested rather than as three independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__graduated_sovereignty, powerless, 0.91).
constraint_indexing:directionality_override(westphalian_sovereignty__graduated_sovereignty, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
