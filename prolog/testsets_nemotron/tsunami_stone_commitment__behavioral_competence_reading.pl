% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__behavioral_competence_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Tsunami Stone Behavioral Norm — Living Transmission Reading
 *   domain: disaster_anthropology/commitment_system/institutional_memory
 *
 * SUMMARY:
 *   This constraint story instantiates the behavioral_competence_reading of
 *   the tsunami_stone_commitment kernel: the stone inscriptions (tsunami
 *   stones) along Japan's Sanriku coast retain live behavioral force through
 *   active intergenerational transmission. Communities that maintain the
 *   practice of reading, teaching, and physically enacting the stones'
 *   warnings — 'Do not build below this point,' 'Run to high ground when the
 *   earth shakes' — exhibit measurably higher survival rates during tsunami
 *   events (validated by the 2011 Great East Japan Earthquake). The
 *   constraint is not the stone itself but the living transmission chain that
 *   keeps the stone's instruction behaviorally active. Extraction is
 *   near-zero (ε=0.03) because the arrangement costs the community almost
 *   nothing to maintain (annual rituals, oral instruction, spatial zoning)
 *   while providing massive survival benefit. Suppression is low (0.15)
 *   because compliance is voluntary and identity-aligned — the norm is
 *   experienced as ancestral wisdom, not external imposition. The constraint
 *   is claimed as a piton: a coordination mechanism that succeeded so
 *   completely it now persists largely through ritualized performance,
 *   vulnerable to transmission chain failure but not extractive.
 *
 * KEY AGENTS:
 *   - coastal_community_members: Primary beneficiaries (powerless/identity_locked) — receive survival benefit from living norm
 *   - intergenerational_transmission_chain: Agenda setter (organized/biographical) — elders, teachers, priests who maintain the practice
 *   - tsunami_stone_inscriptions: Non-agent entity — material anchor of the norm, compiled to vindicated_propositions
 *   - disaster_anthropologists: Observers (analytical/analytical) — study the transmission mechanism and its efficacy
 *   - commemorative_reading_proponents: Excluded (moderate/constrained) — argue the stones are now symbolic artifacts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.03).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.15).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Tsunami Stone Behavioral Norm — Living Transmission Reading").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_system/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, 'cf403a15-448d-45d4-9c95-ef1286c54d44').
narrative_ontology:cs_kernel_codification('cf403a15-448d-45d4-9c95-ef1286c54d44', fixed_text).
narrative_ontology:cs_authority_grounding('cf403a15-448d-45d4-9c95-ef1286c54d44', lineage).
narrative_ontology:cs_interpretation_layer_present('cf403a15-448d-45d4-9c95-ef1286c54d44').
narrative_ontology:cs_reading_relation('cf403a15-448d-45d4-9c95-ef1286c54d44', tsunami_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf403a15-448d-45d4-9c95-ef1286c54d44', tsunami_stone_commitment__catastrophe_validation_axis, influences).
narrative_ontology:cs_axiom('cf403a15-448d-45d4-9c95-ef1286c54d44', foundational, stone_transmission_saves_lives).
narrative_ontology:cs_axiom_status(stone_transmission_saves_lives, holdable).
narrative_ontology:cs_axiom_grounding('cf403a15-448d-45d4-9c95-ef1286c54d44', stone_transmission_saves_lives, empirically_contingent).
narrative_ontology:cs_axiom('cf403a15-448d-45d4-9c95-ef1286c54d44', foundational, ancestral_warning_is_living_obligation).
narrative_ontology:cs_axiom_status(ancestral_warning_is_living_obligation, holdable).
narrative_ontology:cs_axiom_grounding('cf403a15-448d-45d4-9c95-ef1286c54d44', ancestral_warning_is_living_obligation, deontological).
narrative_ontology:cs_reference_frame('cf403a15-448d-45d4-9c95-ef1286c54d44', ancestral_tsunami_warning_practice).
narrative_ontology:cs_drift_state('cf403a15-448d-45d4-9c95-ef1286c54d44', post_2011_validation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('cf403a15-448d-45d4-9c95-ef1286c54d44', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, coastal_community_members).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, intergenerational_transmission_chain).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in the inundation zone and inherit the stone's warning as ancestral instruction. The norm tells them where not to build and when to run. Compliance is not experienced as a burden but as self-protection and ancestral fidelity. Exit would mean abandoning home, ancestry, and identity — the norm is fused with what it means to be from this coast. They receive the survival benefit directly; the cost is near-zero (annual ritual participation, spatial zoning acceptance).
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, coastal_community_members, beneficiary,
    powerless, biographical, identity_locked, local).

% Elders, priests, teachers, and local historians who maintain the practice: annual stone reading ceremonies, school visits to inscriptions, oral instruction to children, spatial zoning advocacy. They administer the living transmission. If the chain breaks, the stones become mute. Their cost is the labor of maintenance; their benefit is identity, purpose, and community cohesion. Exit is constrained — they could stop teaching, but would lose their role in the community's survival architecture.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, intergenerational_transmission_chain, agenda_setter,
    organized, biographical, constrained, regional).

% The physical stones themselves — material anchors of the norm. They do not act or collect; they are the stabilized kernel that the transmission chain activates. Listed as a non-agent for narrative completeness; compiled to vindicated_propositions (intergenerational_warning_efficacy, ritualized_evacuation_practice, materialized_ancestral_knowledge).
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_inscriptions, beneficiary,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_inscriptions).

% Study the transmission mechanism, survival outcomes, and the stone-behavior link. They provide external corroboration of the founding problem's status (live) and the constraint's efficacy. Their seat is analytical — they neither collect from nor pay into the constraint.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, disaster_anthropologists, observer,
    institutional, generational, analytical, global).

% Scholars and officials who argue the stones have decayed to commemorative artifacts — that 2011 survival was due to modern warning systems, not the stones. They would object to the claim of living behavioral force but are excluded from the community's transmission practice. Their exclusion is structural: they do not participate in the intergenerational chain that makes the norm live.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, commemorative_reading_proponents, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the intergenerational tsunami survival problem: how to transmit specific, actionable evacuation knowledge across generations when tsunamis recur on 50-150 year cycles — longer than living memory but shorter than institutional memory. The stones + living transmission chain solve this by materializing the knowledge in the landscape and ritualizing its reactivation.
% TRANSFER_FUNCTION: Moves survival probability from 'unknown/low' to 'high' for community members, at the cost of maintaining the transmission chain (ritual labor, spatial zoning, educational time). No monetary transfer; the transfer is risk reduction in exchange for maintenance effort.
% ABSENT_VOICES: The commemorative_husk_reading proponents (scholars who deny living force) and any community members who might privately doubt the norm but cannot voice it without identity rupture. Also, future generations if the transmission chain breaks — they would inherit mute stones without the living practice.
% DISAPPEARANCE_RATIONALE: If the living transmission vanished overnight (stones remain but no one reads/teaches/acts on them), the next tsunami would kill far more people in these communities. The spatial zoning would erode, rebuilding would creep below the stone lines, and the evacuation reflex would attenuate. The world rearranges because the constraint is the only structure linking ancestral experience to present behavior across the tsunami recurrence interval.
% FOUNDING_PROBLEM: Tsunamis recur on cycles longer than a human lifetime but shorter than written records. Communities needed a way to transmit specific evacuation knowledge ('do not build below this line,' 'run to high ground when the shaking stops') across the generational gap.
% FOUNDING_PROBLEM_CORROBORATION: The 2011 Great East Japan Earthquake provided external corroboration: communities with active stone transmission practices had significantly higher survival rates than those without, confirmed by independent disaster researchers (e.g., Tohoku University IRIDeS, JMA historical analysis). The founding problem (tsunami survival across generational gaps) is attested as still live by the hazard itself — the tectonic recurrence has not changed.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(tsunami_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__behavioral_competence_reading, 0.03, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics describe a constraint that functions as intended with minimal overhead: ε=0.03 because the cost of annual rituals and spatial zoning is negligible relative to the survival benefit; suppression=0.15 because the norm operates through identity and voluntary practice, not coercion; theater_ratio=0.08 because the ritual component (annual reading, school visits to stones) is the transmission mechanism itself, not performative decoration. Accessibility_collapse=0.92 because once the norm is internalized, building in the inundation zone becomes unthinkable — alternatives collapse not through force but through epistemic closure. Resistance=0.05 because no organized opposition exists; the norm is experienced as self-protection. The measurement series shows ε and theater declining slightly over 120 years as the practice stabilizes, and suppression_requirement declining as the norm becomes more deeply embedded.
 *
 * PERSPECTIVAL GAP:
 *   From the community member seat (powerless, identity_locked, local scope), the constraint is pure benefit — a gift from ancestors that costs nothing and saves lives. From the transmission chain seat (organized, biographical, regional scope), the constraint is a living practice requiring active maintenance — if the chain breaks, the stones become mute. The engine should compute these as different effective extractions: near-zero or negative χ for community members (subsidy), slightly positive χ for transmitters (maintenance cost). The commemorative_husk_reading proponents would compute high theater and zero functional benefit — a different constraint entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: coastal_community_members (survival benefit) and intergenerational_transmission_chain (identity/purpose from maintaining the practice). No victims declared — the constraint extracts from no one. The directionality derivation should yield d ≈ 0.05 for community members (full beneficiary), d ≈ 0.2 for transmitters (slight cost of maintenance), d ≈ 0.5 for disaster anthropologists (analytical). The commemorative_husk_reading proponents are excluded — they are not subject to this constraint because they deny its living force.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (tsunami survival) remains live — the 2011 event confirmed it. The constraint has not outlived its function; it has stabilized into a piton because it succeeded. Mandatrophy would occur if the transmission chain broke and the stones remained as symbolic artifacts without behavioral force — that is the commemorative_husk_reading's prediction, not this reading's reality. This reading asserts the mandate is still fully resolved: the arrangement solves the problem it was built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Is this constraint a distinct reading of the tsunami_stone_commitment kernel (behavioral_competence_reading), and what would the commemorative_husk_reading change structurally?',
    'Compare victim/beneficiary structures and ε values across readings; the behavioral reading claims near-zero extraction with community-wide benefit, while the husk reading claims the constraint has decayed to symbolic performance with no functional enforcement.',
    'If the behavioral reading is structurally true, the constraint is a genuine piton — a stabilized coordination mechanism that succeeded and now persists with minimal overhead. If the husk reading is true, the constraint is a degraded piton with no living coordination function. The engine must treat these as separate constraints with different ε, beneficiaries, and classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Commitment to this reading as a distinct constraint from sibling readings of the same kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low measured suppression (0.15) purely structural — community members voluntarily comply because the norm works — or does it contain an internalized component where non-compliance is unthinkable due to identity fusion with ancestral practice?',
    'Post-exit observation: if community members who relocate inland still exhibit heightened tsunami risk awareness and evacuation readiness compared to non-coastal populations, the suppression has an internalized/identity component that persists after structural enforcement is removed.',
    'If internalized, effective suppression is higher than the structural measure suggests — the constraint travels with the agent. This would increase χ for identity_locked agents and could shift seat classifications toward snare-like dynamics even with low base ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in intergenerational norm transmission').

omega_variable(
    piton_vs_mountain_boundary,
    'Does the constraint''s near-zero extraction and high accessibility_collapse (0.92) indicate a genuine mountain (natural law of effective warning transmission) rather than a piton (degraded coordination that succeeded and now persists by inertia)?',
    'Test whether the constraint would persist if the intergenerational transmission chain were broken — if the stones alone, without living transmission, would maintain behavioral force. If yes, mountain; if no, piton dependent on living practice.',
    'Mountain classification would assert the constraint is a structural feature of effective disaster memory — irreducible and non-extractive. Piton classification asserts it is a successful human arrangement that now persists largely through ritual performance, vulnerable to transmission failure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(piton_vs_mountain_boundary, conceptual, 'Whether successful coordination that has stabilized becomes a natural law or remains a human artifact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsunami_stone_behavioral_tr_t0, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(tsunami_stone_behavioral_tr_t20, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(tsunami_stone_behavioral_tr_t40, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(tsunami_stone_behavioral_tr_t60, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement(tsunami_stone_behavioral_tr_t80, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement(tsunami_stone_behavioral_tr_t100, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 100, 0.08).
narrative_ontology:measurement(tsunami_stone_behavioral_tr_t120, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 120, 0.08).

% Extraction over time
narrative_ontology:measurement(tsunami_stone_behavioral_be_t0, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(tsunami_stone_behavioral_be_t20, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 20, 0.04).
narrative_ontology:measurement(tsunami_stone_behavioral_be_t40, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 40, 0.04).
narrative_ontology:measurement(tsunami_stone_behavioral_be_t60, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 60, 0.03).
narrative_ontology:measurement(tsunami_stone_behavioral_be_t80, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 80, 0.03).
narrative_ontology:measurement(tsunami_stone_behavioral_be_t100, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 100, 0.03).
narrative_ontology:measurement(tsunami_stone_behavioral_be_t120, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 120, 0.03).

% Suppression requirement over time
narrative_ontology:measurement(tsunami_stone_behavioral_su_t0, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(tsunami_stone_behavioral_su_t20, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(tsunami_stone_behavioral_su_t40, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 40, 0.16).
narrative_ontology:measurement(tsunami_stone_behavioral_su_t60, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 60, 0.15).
narrative_ontology:measurement(tsunami_stone_behavioral_su_t80, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 80, 0.15).
narrative_ontology:measurement(tsunami_stone_behavioral_su_t100, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 100, 0.15).
narrative_ontology:measurement(tsunami_stone_behavioral_su_t120, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 120, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__behavioral_competence_reading, 0.08).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__commemorative_husk_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% This constraint family (tsunami_stone_commitment) decomposes the kernel into three structurally distinct readings with different ε, beneficiaries, and classifications. The behavioral reading (this file) claims near-zero extraction and community-wide benefit. The commemorative reading claims high theater and decayed function. The validation axis reading treats the 2011 event as an empirical test of the kernel's claims. All three link to each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tsunami_stone_commitment__behavioral_competence_reading, powerless, 0.05).
constraint_indexing:directionality_override(tsunami_stone_commitment__behavioral_competence_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
