% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__hybrid_embedded_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Ritual Fidelity as Embodied Survival Knowledge Transmission
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   A practicing community maintains ritual forms across generations,
 *   claiming (and demonstrating through their practice) that survival
 *   competence is encoded within the ritual's symbolic and bodily dimensions.
 *   The constraint is that ritual fidelity — precise adherence to the
 *   transmitted form — is the mechanism by which embodied knowledge (threat
 *   response, resource use, collective coordination, grief processing) passes
 *   from generation to generation. This is a READING of a contested kernel
 *   about catastrophe memory transmission. The kernel is contested: one
 *   reading emphasizes form-and-function inseparability (this one); another
 *   reading emphasizes operational competence transmission as separable from
 *   ritual symbolism; a third reading emphasizes symbol preservation as
 *   intrinsic communal good. This story instantiates the
 *   hybrid_embedded_reading: ritual form and operational function are
 *   co-constitutive. Altering form degrades function. Function only exists
 *   through enacted form. No structural victim exists unless practice is
 *   discontinued — the constraint is claimed as rope (coordination through
 *   shared practice with a mountain substrate of embodied knowledge as a
 *   physical/cognitive constraint).
 *
 * KEY AGENTS:
 *   - practicing_community: Organized actor that sustains the ritual, identity_locked to participation, beneficiary of the knowledge transmission mechanism
 *   - knowledge_bearers: Moderate-power agents (elders, experienced practitioners) who accumulate embodied competence and benefit from its validation and transmission
 *   - younger_generation: Powerless agents, identity_locked, who can only access survival competence through ritual participation
 *   - outside_observers: Analytical seat; can document form but cannot carry embodied knowledge
 *   - modernizing_pressures: Institutional actors excluded from the community's ritual practice, would argue for propositional replacement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Ritual Fidelity as Embodied Survival Knowledge Transmission").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, 'f023036f-168b-4570-96c9-b76168748781').
narrative_ontology:cs_kernel_codification('f023036f-168b-4570-96c9-b76168748781', distributed).
narrative_ontology:cs_authority_grounding('f023036f-168b-4570-96c9-b76168748781', practice).
narrative_ontology:cs_interpretation_layer_present('f023036f-168b-4570-96c9-b76168748781').
narrative_ontology:cs_reading_relation('f023036f-168b-4570-96c9-b76168748781', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f023036f-168b-4570-96c9-b76168748781', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('f023036f-168b-4570-96c9-b76168748781', foundational, form_function_inseparability).
narrative_ontology:cs_axiom_status(form_function_inseparability, holdable).
narrative_ontology:cs_axiom_grounding('f023036f-168b-4570-96c9-b76168748781', form_function_inseparability, empirically_contingent).
narrative_ontology:cs_axiom('f023036f-168b-4570-96c9-b76168748781', foundational, non_propositional_knowledge_irreducibility).
narrative_ontology:cs_axiom_status(non_propositional_knowledge_irreducibility, holdable).
narrative_ontology:cs_axiom_grounding('f023036f-168b-4570-96c9-b76168748781', non_propositional_knowledge_irreducibility, empirically_contingent).
narrative_ontology:cs_reference_frame('f023036f-168b-4570-96c9-b76168748781', embodied_knowledge_as_irreducible).
narrative_ontology:cs_drift_state('f023036f-168b-4570-96c9-b76168748781', contemporary_modernization_pressure, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('f023036f-168b-4570-96c9-b76168748781', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, knowledge_bearers).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, younger_generation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts ritual forms that have been transmitted across generations. The community sustains the ritual fidelity not primarily through explicit instruction but through repetition, correction, and embodied participation. Members carry within their bodies and practiced movements the encoded competencies — threat response patterns, resource identification, seasonal coordination, grief processing. Discontinuing the ritual means losing access to this non-propositional knowledge base, which resides nowhere else. Identity as a member is constituted through ritual participation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community, beneficiary).

% Elders and experienced practitioners who have accumulated the embodied knowledge through decades of practice. They benefit from the community's continued ritual practice because it validates their expertise and provides a context for transmitting what they know. If the ritual were abandoned, their accumulated competence would become inaccessible to the next generation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, knowledge_bearers, beneficiary,
    moderate, biographical, constrained, local).

% Learns survival competence through participation in ritual practice — not through being told the rules but through repeated enactment, correction, and embodied habituation. The constraint structures their access to knowledge that is otherwise unavailable to them. Cannot learn these competencies through propositional instruction alone; the form IS the transmission mechanism.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, younger_generation, beneficiary,
    powerless, biographical, identity_locked, local).

% Researchers, archivists, documentation specialists who might study the ritual from outside. They can observe and record formal structure but cannot access the embodied knowledge that lives only in enacted practice. They see ritual fidelity as preserving a form; they do not carry its operational content.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, outside_observers, observer,
    analytical, generational, analytical, global).

% States, educational institutions, religious authorities that promote propositional knowledge systems and efficiency metrics might argue for replacing ritual practice with explicit instruction. They are excluded from the community's decision about ritual fidelity. They would argue that survival competence can be separated from ritual form and taught directly.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, modernizing_pressures, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__hybrid_embedded_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual fidelity solves the problem of transmitting non-propositional, embodied knowledge across generations — survival competence (threat recognition, resource use, grief processing, seasonal coordination) that cannot be reduced to propositions or explicit rules but only to practiced, corrected, habituated form.
% TRANSFER_FUNCTION: The community transfers time, attention, and embodied participation into the constraint's maintenance. The constraint transfers back accumulated, validated competence that members can only access through practicing the form faithfully.
% ABSENT_VOICES: Those who have abandoned the ritual or who argue for its replacement with propositional instruction are structurally excluded from the community's ritual practice. Modernizing authorities and efficiency-focused administrators would argue that ritual form is ornamental and can be stripped away; they are kept out by the constraint's own structure (identity_locked exit for members, organized community boundary).
% DISAPPEARANCE_RATIONALE: If ritual fidelity enforcement vanished and practice were abandoned, the community would lose its primary transmission mechanism for embodied survival knowledge. Members could not acquire threat-response patterns, resource-use competence, or collective grief processing through alternative means. The competence is encoded nowhere else — not in textbooks, not in explicit rules, not in institutional training. The community would face a sharp capability gap and would need to rebuild these competencies from scratch, likely through re-learning the very ritual forms they had abandoned.
% FOUNDING_PROBLEM: Survival competence depends on embodied knowledge — practical wisdom about threats, resources, and social coordination — that cannot be fully captured in propositional form. Early communities faced repeated catastrophes; the knowledge that allowed some to survive was embedded in the ritual forms that survived communities enacted. The form preserved the competence across generational forgetting.
% FOUNDING_PROBLEM_CORROBORATION: Embodied cognition research and anthropological studies of ritual competence transmission (from sources outside the practicing community) confirm that non-propositional knowledge — pattern recognition, somatic response, collective coordination — is encoded in ritual practice and does not transfer through propositional instruction alone. Survival specialists and disaster-response researchers attest that threat-response competence acquired through repeated practice (as in ritual) is retained under stress better than competence acquired through classroom instruction. The practicing community attests the founding problem remains live — abandoning the ritual would mean losing access to knowledge that continues to matter for community resilience.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).
:- end_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the constraint imposes real costs on the community: time, embodied participation, identity fusion, and constraint on alternative knowledge-transmission methods. It is not high (not snare-level) because the constraint's primary function is coordination and knowledge preservation, not extraction by one party from another. Suppression is low (0.22) because the community maintains the constraint through internal commitment and identity alignment rather than coercive enforcement from outside. Theater ratio is very low (0.18) because the constraint's performative component (the visible ritual action) IS the operational knowledge transmission — there is minimal gap between the ritual's appearance and its function. This reading claims the form and function are inseparable, so any performative dimension is structural necessity, not theatrical overlay. Accessibility collapse is high (0.72) because once the community understands that the ritual encodes embodied knowledge, alternatives to the ritual form appear illusory — you cannot replace embodied learning with propositional instruction at the same fidelity level. Resistance is low (0.31) because community members largely accept the constraint as legitimate; external pressure to abandon it meets community resistance but is not the primary dynamic. The time series is flat with minimal drift because the constraint's core operation (embodied knowledge transmission through ritual fidelity) is stable; the small rise in theater ratio and suppression around t=24 reflects external pressure and articulation efforts by the community to defend the practice.
 *
 * PERSPECTIVAL GAP:
 *   From the younger_generation's seat: the constraint appears as the ONLY pathway to survival competence — they are fully dependent on ritual practice. From knowledge_bearers' seat: the constraint validates decades of embodied learning and creates a venue for its transmission. From the practicing_community's collective seat: the constraint is self-reinforcing (fidelity maintains competence, competence validates fidelity). From modernizing_pressures seat: the constraint appears as inefficient cultural inertia that should be replaced by explicit instruction. From outside_observers: the constraint appears as form preservation that may or may not transmit function. The engine's per-seat classification should expose these divergences: younger_generation and knowledge_bearers compute as net beneficiaries (low d), practicing_community as symmetric or slightly beneficiary (the community pays costs but receives benefits), and the excluded modernizing_pressures as viewing it as an obstruction.
 *
 * DIRECTIONALITY LOGIC:
 *   The practicing_community is the structural beneficiary (collects the embodied knowledge, organizes the transmission, identity-locked to participation — d near beneficiary end, perhaps 0.2–0.3). Knowledge_bearers are also beneficiaries (their competence is validated and transmissible — d similarly low). Younger_generation are beneficiaries with high identity-lock (they can ONLY access competence through the ritual — d near beneficiary, perhaps 0.15–0.25, because they depend on it but are not extracted from in the snare sense). Modernizing_pressures are excluded, not coordinated. No party is a victim in the snare sense unless the constraint were interpreted as preventing access to alternative knowledge systems — but those alternatives are posited as inferior, not as suppressed options. The constraint is rope-like because it solves genuine coordination (knowledge transmission) and all participating seats benefit, albeit asymmetrically. Identity-lock for younger_generation and knowledge_bearers is structural (their identity as community members is constituted through ritual participation), not imposed suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy classification because its founding problem (embodied knowledge transmission across generations in the face of catastrophe-induced forgetting) remains live — the community continues to face intergenerational knowledge loss as a real problem, and ritual practice continues to solve it. If the founding problem were dead (if propositional systems had fully replaced embodied learning), the constraint would persist as theater/piton. But the founding problem's liveness is corroborated by embodied cognition research and disaster-response studies showing that propositional instruction does NOT fully replace embodied learning — the founding problem is not solved by modernization. The constraint is thus NOT degraded inertia; it is active coordination. The mandatrophy-resolved flag should remain false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_function_separability,
    'Is the survival competence genuinely inseparable from the ritual form, or can the competence be extracted and taught through propositional/explicit methods?',
    'Comparative study of communities that abandoned the ritual and attempted to preserve competence through documentation and explicit instruction versus communities that maintained ritual fidelity. Measure threat-response performance, resource-use accuracy, and collective coordination outcomes under stress.',
    'If separable: the constraint is misclassified as rope and should be reclassified as snare (ritual form is cover for competence that is extracted and taught elsewhere, leaving the community dependent on ritual performance without accessing its operational content). If genuinely inseparable: the rope classification holds; form preservation IS functional necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(form_function_separability, empirical, 'Whether ritual form and survival competence are structurally co-constitutive or separable.').

omega_variable(
    kernel_reading_contest,
    'Which of the three readings of the catastrophe_memory_transmission kernel captures the true structural relationship: is the constraint primarily about embodied knowledge transmission (hybrid_embedded), or about operational competence extraction (operational_competence), or about symbolic identity preservation (symbol_continuity)?',
    'Ethnographic and phenomenological investigation of what practitioners report they are transmitting and learning; study of what happens when the ritual is abandoned (does competence persist in propositional form, or does it disappear?); analysis of whether practitioners could survive similar catastrophes WITHOUT the ritual if they retained documented knowledge.',
    'Different readings produce different constraint types and different victim/beneficiary structures. This resolution would determine whether the engine''s per-seat classification aligns with the reading or diverges, indicating misframing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the catastrophe_memory_transmission kernel is structurally accurate.').

omega_variable(
    identity_lock_mechanism,
    'Is the identity_locked exit for younger_generation a structural feature of embodied learning (you cannot exit from learning a competence you have already internalized), or is it a cultural enforcement mechanism (the community prevents exit by binding identity to ritual)?',
    'Study individuals who have learned the embodied competence through ritual and then chosen to leave the community. Can they retain and apply the competence outside the ritual context? Do they experience identity dissolution or practical competence loss?',
    'If structural (the competence redefines the learner): identity-lock is unavoidable cost of learning; the constraint is neutral on this dimension. If cultural enforcement: identity-lock is an extraction mechanism overlaid on the knowledge transmission; the constraint includes a suppression component that was not independently motivated by the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity-lock is structural to embodied learning or an overlay of cultural enforcement.').

omega_variable(
    modernization_pressure_absorption,
    'As external pressure to abandon the ritual grows (educational modernization, institutional efficiency demands), what mechanisms allow the community to maintain fidelity? Is it internal commitment and identity alignment, or is increasing suppression required?',
    'Temporal analysis of community narratives and enforcement mechanisms as modernization pressure intensifies. Measure: costs imposed on practitioners who consider or initiate defection; rhetorical defense of the practice (does it shift from ''this is who we are'' to ''we must prevent outsiders from interfering''); resource allocation to boundary maintenance.',
    'If maintained through commitment: the suppression metric should remain low (the measurement series supports this reading). If increasingly enforced: suppression will rise over time as the community defends against external challenge, potentially reclassifying toward tangled_rope. The flat suppression series suggests commitment is holding, but continued monitoring is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernization_pressure_absorption, empirical, 'Whether ritual fidelity is maintained through internal commitment or requires increasing external enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement_basis(cata_tr_t8, observed).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement_basis(cata_tr_t16, observed).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement_basis(cata_tr_t24, observed).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement_basis(cata_tr_t32, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(cata_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement_basis(cata_be_t8, observed).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement_basis(cata_be_t16, observed).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement_basis(cata_be_t24, observed).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 32, 0.39).
narrative_ontology:measurement_basis(cata_be_t32, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(cata_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 8, 0.19).
narrative_ontology:measurement_basis(cata_su_t8, observed).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 16, 0.2).
narrative_ontology:measurement_basis(cata_su_t16, observed).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 24, 0.22).
narrative_ontology:measurement_basis(cata_su_t24, observed).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 32, 0.23).
narrative_ontology:measurement_basis(cata_su_t32, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(cata_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__hybrid_embedded_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__symbol_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_transmission kernel. The kernel contest involves three structurally distinct readings: hybrid_embedded_reading (this story) emphasizes form-function co-constitution; operational_competence_reading emphasizes competence extraction and explicit teaching; symbol_continuity_reading emphasizes symbolic preservation as intrinsic good. The three readings produce different constraint types and different beneficiary/victim structures. All three readings are linked to the same kernel via affects_constraints. The epsilon values differ substantially across readings because the constraint's structural function is contested — what one reading sees as coordination, another sees as extraction or identity preservation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_transmission__hybrid_embedded_reading, powerless, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
