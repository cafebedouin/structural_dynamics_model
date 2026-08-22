% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment Collective Right Reading
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint story models the collective-right reading of the Second
 *   Amendment — the interpretation that the Amendment protects the authority
 *   of states to maintain organized militia forces, not an individual right
 *   to possess arms outside militia service. Under this reading, the
 *   Amendment operates as a federalism provision: it limits federal power to
 *   disarm state militia institutions, but imposes no meaningful constraint
 *   on federal or state regulation of individual firearms ownership. The
 *   coordination function is intergovernmental — preserving state military
 *   capacity against federal centralization. Extraction is low because the
 *   reading does not transfer resources from individuals to the state;
 *   rather, it removes a potential constitutional barrier to regulation. The
 *   primary structural beneficiaries are state governments as institutional
 *   rights-holders and organized militia units as the operative bodies. No
 *   individual victims are declared because this reading does not extract
 *   from individuals — it simply fails to protect them against regulation.
 *   The individual-right reading is the competing claim that would create a
 *   victim class (individuals burdened by regulation) and a beneficiary class
 *   (individuals whose ownership is protected).
 *
 * KEY AGENTS:
 *   - state_governments: Primary beneficiary (institutional/powerful) — holds the protected militia authority against federal encroachment
 *   - organized_militia_units: Secondary beneficiary (organized/powerful) — the operative bodies whose arming the right secures
 *   - federal_government: Constrained actor (institutional/powerful) — subject to the limitation on disarming state militias
 *   - individual_citizens: Excluded from rights-holding under this reading (moderate/constrained) — subject to plenary regulation of firearms outside militia service
 *   - courts: Observer (analytical/analytical) — adjudicate the reading's application
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.15).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.25).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment Collective Right Reading").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional_law/political_philosophy/legal_interpretation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, 'b3a33d0f-a8ab-4ea1-8857-52dfc45b7cef').
narrative_ontology:cs_kernel_codification('b3a33d0f-a8ab-4ea1-8857-52dfc45b7cef', fixed_text).
narrative_ontology:cs_authority_grounding('b3a33d0f-a8ab-4ea1-8857-52dfc45b7cef', lineage).
narrative_ontology:cs_interpretation_layer_present('b3a33d0f-a8ab-4ea1-8857-52dfc45b7cef').
narrative_ontology:cs_reading_relation('b3a33d0f-a8ab-4ea1-8857-52dfc45b7cef', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('b3a33d0f-a8ab-4ea1-8857-52dfc45b7cef', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('b3a33d0f-a8ab-4ea1-8857-52dfc45b7cef', foundational, second_amendment_protects_state_militia_authority_only).
narrative_ontology:cs_axiom_status(second_amendment_protects_state_militia_authority_only, holdable).
narrative_ontology:cs_axiom_grounding('b3a33d0f-a8ab-4ea1-8857-52dfc45b7cef', second_amendment_protects_state_militia_authority_only, conventional).
narrative_ontology:cs_axiom('b3a33d0f-a8ab-4ea1-8857-52dfc45b7cef', foundational, individual_arms_possession_outside_militia_unprotected).
narrative_ontology:cs_axiom_status(individual_arms_possession_outside_militia_unprotected, holdable).
narrative_ontology:cs_axiom_grounding('b3a33d0f-a8ab-4ea1-8857-52dfc45b7cef', individual_arms_possession_outside_militia_unprotected, conventional).
narrative_ontology:cs_reference_frame('b3a33d0f-a8ab-4ea1-8857-52dfc45b7cef', founding_federalism_militia_compromise).
narrative_ontology:cs_drift_state('b3a33d0f-a8ab-4ea1-8857-52dfc45b7cef', post_heller_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('b3a33d0f-a8ab-4ea1-8857-52dfc45b7cef', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, organized_militia_units).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, federal_government).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, militia_centrality_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the constitutional authority to maintain organized militia forces free from federal disarmament. This authority structures their relationship to federal military policy and preserves a state-level military capacity. They can invoke this right in intergovernmental disputes and litigation against federal overreach.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, beneficiary,
    institutional, generational, arbitrage, national).

% The operative bodies (National Guard, state defense forces) whose arming and organization the right directly secures. Their existence and federal recognition are the concrete instantiation of the state's militia authority. They benefit from federal equipment, training, and legal protection against disarmament.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, organized_militia_units, beneficiary,
    organized, generational, constrained, national).

% Subject to the constitutional limitation that it may not disarm or undermine state militia institutions. This constrains federal military centralization and some regulatory approaches, but leaves plenary authority over individual firearms regulation outside the militia context. The constraint is a structural limit on one dimension of federal power.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Outside the protection of this reading — they have no constitutional claim against firearms regulation that is not connected to organized militia service. They are subject to whatever federal and state regulations the political process produces. Their exclusion is structural: the reading's premise is that the Amendment's 'people' refers to the collective body politic acting through state militia institutions, not to individuals as such.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_citizens, excluded,
    moderate, biographical, constrained, national).

% Adjudicate the scope and application of the collective-right reading when it is invoked in litigation. Their interpretive choices determine whether the reading functions as a meaningful federalism constraint or a dead letter. They sit outside the constraint's beneficiary/victim structure but determine its operational force.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, courts, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the federal-state military relationship by constitutionally entrenching state authority to maintain organized militia forces, preventing federal monopolization of military power and preserving a structural counterweight to standing armies.
% TRANSFER_FUNCTION: Allocates constitutional authority over militia organization from the federal government to the states — a transfer of power, not resources. No material transfer occurs; the constraint operates by withholding federal power to disarm state institutions.
% ABSENT_VOICES: Individual citizens who would claim a personal right to arms for self-defense, hunting, or other purposes outside militia service. They are structurally excluded by the reading's premise that the Amendment protects only state militia authority. Their voices appear in the competing individual-right reading, not in this one.
% DISAPPEARANCE_RATIONALE: If the collective-right reading vanished overnight (i.e., if courts adopted the individual-right reading as the sole authoritative interpretation), the constitutional landscape would shift dramatically: federal and state firearms regulations would face strict scrutiny, the federalism dimension of the Amendment would collapse, and state militia authority would lose its explicit constitutional anchor. The world of gun regulation and federal-state military relations would reorganize.
% FOUNDING_PROBLEM: The founding generation feared that a federal standing army combined with federal power to disarm state militias would enable tyranny and destroy the federal balance. The Second Amendment was adopted to guarantee that states could maintain their own military forces as a check on federal power.
% FOUNDING_PROBLEM_CORROBORATION: Historical consensus (Rakove, Cornell, Bogus, and others) attests that the founding-era understanding was militia-centered and federalism-structured. The individual-right reading's proponents (e.g., Levy, Malcolm, Heller majority) contest this, arguing the right was always individual. The civic-republican reading (Shalhope, Skinner, Amar) offers a third corroboration: the right protected armed citizenship as civic virtue, which overlaps with but is not identical to the collective-right frame. No single account commands unanimity; the founding problem's status is genuinely contested across the scholarly and judicial field.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because this reading primarily functions as a structural limitation on federal power rather than a transfer mechanism. The Amendment, read collectively, does not extract resources from individuals to states; it allocates constitutional authority between federal and state governments regarding military organization. Suppression is modest (0.25) — the reading suppresses individual-rights claims against regulation, but this is a negative constraint (absence of protection) rather than active coercion. Theater ratio is very low (0.1) because the reading's doctrinal coherence does not depend on performative maintenance; it was the dominant judicial interpretation for most of American history (pre-Heller). Accessibility collapse is high (0.7) because once the collective-right premise is accepted, individual-rights alternatives are logically foreclosed within that framework — the text's militia clause becomes determinative. Resistance is moderate (0.4) because the reading faced increasing political and scholarly challenge from the 1970s onward, culminating in Heller's rejection of it.
 *
 * PERSPECTIVAL GAP:
 *   From the state-government seat, the constraint is a genuine coordination mechanism (rope) preserving federalism's military dimension. From the individual-citizen seat, the constraint appears as a snare-adjacent absence — no protection against regulation they experience as burdensome. The engine computes this divergence from the structural data: beneficiaries have low directionality, excluded agents have moderate directionality but no beneficiary declaration. The individual-right reading would invert this structure entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and organized militia units are beneficiaries (d ≈ 0.2) — the constraint protects their institutional authority and resources. The federal government bears a modest constraint (d ≈ 0.4) — it loses some regulatory/disarmament power but retains broad authority over individual firearms. Individual citizens are not victims in the extraction sense — they are simply not rights-holders under this reading (d ≈ 0.5, symmetric: they neither gain nor lose from the constraint's operation; they are outside its protective scope). The reading does not extract from individuals; it withholds protection. This is why no victims are declared: the extraction metric captures active transfer, not failure to protect.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing federal disarmament of state militias) is contested: some argue it is dead (National Guard federalization, standing army) while others argue it is live (state defense forces, federal overreach concerns). The constraint persists despite the founding problem's ambiguity because it became embedded in a broader constitutional federalism architecture. No concentrated beneficiary captures the constraint's operation — states benefit defensively, not extractively — which distinguishes it from piton or snare dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does the collective-right reading of the Second Amendment relate structurally to the individual-right and civic-republican readings of the same kernel?',
    'Comparative structural analysis of each reading''s beneficiary/victim structure, directionalities, and coordination/extraction profile across the kernel family.',
    'If the collective-right reading forecloses the individual-right reading within a single legal framework, their coexistence in the corpus as separate constraints would misrepresent the logical structure of the kernel dispute. If they coexist, the kernel is a site of genuine pluralism; if one forecloses, the engine should reflect the logical exclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Structural relationship between sibling readings of the second_amendment_arms_right kernel').

omega_variable(
    state_beneficiary_scope_ambiguity,
    'Do state governments benefit from this reading as rights-holders with enforceable claims against federal power, or as regulatory authorities relieved of individual-rights constraints?',
    'Doctrinal analysis of collective-right jurisprudence (e.g., Miller, pre-Heller lower court decisions) to distinguish between state-as-claimant and state-as-regulator beneficiary structures.',
    'If states benefit as rights-holders, the constraint coordinates intergovernmental federalism; if as regulators relieved of constraints, the reading primarily suppresses individual claims. This changes the coordination/extraction balance and the stakeholder seat assignments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_beneficiary_scope_ambiguity, conceptual, 'Whether state governments are coordination beneficiaries or extraction beneficiaries under this reading').

omega_variable(
    militia_definition_contestation,
    'What constitutes ''organized militia'' for purposes of the protected right — state-organized National Guard, broader unorganized militia, or something else?',
    'Historical analysis of militia acts (1792, 1903, 1916) and judicial definitions; legislative history of the Militia Clauses.',
    'A narrow definition (National Guard only) makes the constraint''s coordination function minimal and its extractiveness near zero for most individuals; a broader definition expands the coordination constituency and changes the beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_definition_contestation, empirical, 'Scope of the ''organized militia'' concept that determines who holds the protected right').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_be_t1791, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1791, 0.05).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_be_t1876, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1876, 0.08).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_be_t1939, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1939, 0.12).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_be_t2008, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2008, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_arms_right__collective_right_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__collective_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the second_amendment_arms_right kernel. The collective-right reading (this file) structures the right as a state-held federalism provision with low extractiveness. The individual-right reading structures it as an individual liberty with high extractiveness against regulation. The civic-republican reading structures it as a civic virtue prerequisite with moderate coordination function. Their ε values differ structurally because their beneficiary/victim architectures differ — they are different constraints, not different measurements of the same constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_arms_right__collective_right_reading, institutional, 0.2).
constraint_indexing:directionality_override(second_amendment_arms_right__collective_right_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
