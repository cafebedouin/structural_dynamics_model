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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Ritual Form-Function Co-Constitution in Catastrophe Memory
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   In communities shaped by catastrophe (genocide, famine, displacement,
 *   war), ritual encodes survival knowledge in non-propositional form —
 *   body-memory, sensory association, spatial arrangement, repetition pattern
 *   — that transmits operational competence across generations even when
 *   explicit instruction is impossible or illegible under extreme stress.
 *   This constraint models the relationship between ritual form and
 *   functional survival knowledge as inseparable: altering the forms degrades
 *   the transmission of threat-recognition, resource-coordination, and
 *   resilience capacity, but the capacity only exists through the enacted
 *   forms themselves. The constraint is CLAIMED as rope (genuine coordination
 *   function) and the authored metrics describe stable, low-intensity,
 *   non-suppressive operation. The key asymmetry: ritual transmitters see
 *   form discontinuation as catastrophic; reformers see form persistence as
 *   unnecessary tradition. The constraint sits at the boundary between
 *   mountain (embodied knowledge as physical constraint) and rope
 *   (coordination through shared practice). This is ONE READING of a
 *   contested kernel. Sibling readings include the symbol_continuity reading
 *   (mourning and identity are intrinsic goods, not functional vehicles) and
 *   operational_competence reading (only the competence matters, form is
 *   instrumentally justified). This reading holds that form and function are
 *   co-constitutive: neither exists without the other.
 *
 * KEY AGENTS:
 *   - ritual_transmitting_community: Maintains forms and gatekeeps access; locked into practice by institutional identity
 *   - younger_generation_participants: Learn through embodied participation; constrained exit (knowledge only accessible through practice)
 *   - external_reformers: Advocate modernization and rationalization; excluded from authority over forms
 *   - continuity_scholars: Observe and measure correlation between form fidelity and resilience outcomes; analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Ritual Form-Function Co-Constitution in Catastrophe Memory").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, '1bda59c1-97c8-413b-9c3f-4b5b72b5f272').
narrative_ontology:cs_kernel_codification('1bda59c1-97c8-413b-9c3f-4b5b72b5f272', distributed).
narrative_ontology:cs_authority_grounding('1bda59c1-97c8-413b-9c3f-4b5b72b5f272', practice).
narrative_ontology:cs_interpretation_layer_present('1bda59c1-97c8-413b-9c3f-4b5b72b5f272').
narrative_ontology:cs_reading_relation('1bda59c1-97c8-413b-9c3f-4b5b72b5f272', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('1bda59c1-97c8-413b-9c3f-4b5b72b5f272', catastrophe_memory_transmission__operational_competence_reading, influences).
narrative_ontology:cs_axiom('1bda59c1-97c8-413b-9c3f-4b5b72b5f272', foundational, form_function_inseparability).
narrative_ontology:cs_axiom_status(form_function_inseparability, holdable).
narrative_ontology:cs_axiom_grounding('1bda59c1-97c8-413b-9c3f-4b5b72b5f272', form_function_inseparability, empirically_contingent).
narrative_ontology:cs_axiom('1bda59c1-97c8-413b-9c3f-4b5b72b5f272', secondary, embodied_knowledge_resistance_to_stress).
narrative_ontology:cs_axiom_status(embodied_knowledge_resistance_to_stress, holdable).
narrative_ontology:cs_axiom_grounding('1bda59c1-97c8-413b-9c3f-4b5b72b5f272', embodied_knowledge_resistance_to_stress, empirically_contingent).
narrative_ontology:cs_reference_frame('1bda59c1-97c8-413b-9c3f-4b5b72b5f272', ritual_form_as_survival_knowledge_substrate).
narrative_ontology:cs_drift_state('1bda59c1-97c8-413b-9c3f-4b5b72b5f272', contemporary_literacy_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1bda59c1-97c8-413b-9c3f-4b5b72b5f272', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_transmitting_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, younger_generation_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains ritual forms across generations, performing actions whose meaning may not be consciously articulated but whose omission or alteration degrades the community's capacity to process collective trauma and coordinate response to future catastrophe. They are locked into the practice by institutional identity and the belief that discontinuation breaks the transmission chain for survival-critical knowledge.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_transmitting_community, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_transmitting_community, beneficiary).

% Receive embodied knowledge of threat recognition, resource coordination, and collective response through participation in enacted ritual. They learn through practice rather than explicit instruction. Their options are constrained by the community's gatekeeping of ritual forms; they cannot easily access the embedded knowledge through alternative channels.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, younger_generation_participants, beneficiary,
    moderate, biographical, constrained, local).

% Argue for modernization, rationalization, or translation of ritual content into explicit propositional form. They believe the same survival knowledge can be extracted and transmitted through education, documentation, or demystification. They are excluded from the authority to alter ritual forms and their evidence for functional equivalence is not recognized by transmitting practitioners.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, external_reformers, excluded,
    moderate, biographical, mobile, regional).

% Document, analyze, and attest the relationship between ritual form fidelity and community resilience outcomes. They occupy an analytical seat external to the practicing community and can measure whether altering forms produces measurable degradation in trauma processing or threat response capacity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, continuity_scholars, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__hybrid_embedded_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__hybrid_embedded_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual enacts and rehearses collective threat response, resource coordination under scarcity, and meaning-making after catastrophe. The practice solves the coordination problem of maintaining group cohesion and operative knowledge when normal institutions have failed or are absent. The knowledge is transmitted non-propositionally: through body-memory, spatial arrangement, repetition patterns, and sensory association rather than explicit instruction.
% TRANSFER_FUNCTION: The constraint moves time and cognitive attention from individuals to the collective practice; it channels learning capacity into embodied participation rather than abstract study or documentation. The transfer is reciprocal — individuals invest in ritual fidelity and receive access to the embedded survival knowledge in return. No externalization of gain; the beneficiary (ritual community) and the knowledge-carrier (participants) are the same system.
% ABSENT_VOICES: External reformers and modernization advocates are excluded from authority over ritual forms. Their argument — that the same survival knowledge can be extracted and transmitted through rationalized instruction, documentation, or decontextualized learning — is not recognized as legitimate by the community. Younger generations who privately question form necessity cannot voice that doubt without risking institutional exclusion or being read as insufficient commitment to community survival.
% DISAPPEARANCE_RATIONALE: If ritual fidelity requirements disappeared overnight and were replaced by explicit instruction in survival skills, the transmitting community attests that threat-recognition and collective response capacity would degrade or be lost. However, modernization advocates argue the knowledge would persist and transmit more efficiently through rationalized channels. The empirical question — whether the function is inseparable from the form or merely currently-instantiated through it — remains contested.
% FOUNDING_PROBLEM: Survival knowledge must persist across generations even when catastrophe is not actively lived, and must be accessible during extreme stress when normal cognitive and institutional capacity is compromised. Non-literate or post-catastrophe communities face the specific challenge that abstract propositional knowledge becomes inaccessible under terror, disorientation, or collapse of authority; embodied, ritualized knowledge accessible through body-memory and sensory association is more resistant to cognitive degradation under catastrophic stress.
% FOUNDING_PROBLEM_CORROBORATION: Communities that have endured and recovered from multiple catastrophes (genocides, famines, displacement, wars, pandemic) attest that ritual practice maintained threat-recognition and coordination capacity across generations and loss cycles. Anthropologists and historians document correlations between ritual continuity and resilience outcomes in post-catastrophe recovery. Conversely, modernization advocates and efficiency-oriented researchers argue the founding problem is overstated — literacy expansion and explicit education demonstrate that embodied ritual is no longer functionally necessary for knowledge preservation. The contest remains unresolved; no unified authority has adjudicated whether the function is contingent on the form or genuinely inseparable.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.38) because the constraint imposes a real cost on participants (time, cognitive load, continued practice commitment) but the benefit aligns closely with the cost — participants receive what they pay for (survival-knowledge transmission). Suppression is low (0.22) because there is no centralized enforcement machinery and participants are not trapped by external coercion but by identity fusion and belief in necessity. Theater is minimal (0.12) because the ritual practice is genuinely functional; performative elements exist but are proportionally small relative to the operative transmission work. Accessibility_collapse is high (0.71) because once the form-function claim is accepted, alternatives (explicit instruction, documentation, modernized transmission) appear inadequate or incoherent, and continuity within the form becomes the only legible option. Resistance is moderate (0.31) because external reformers and younger participants questioning particular forms mount real pressure, but the community's institutional coherence and identity-locking hold the constraint stable. The measurement series are flat across the interval because the constraint has not undergone lifecycle drift in this community — the forms have remained stable, extractiveness has not accumulated, and enforcement has not intensified or decayed. Stability suggests the coordination is genuine and the community has not been captured by secondary extractors.
 *
 * PERSPECTIVAL GAP:
 *   The transmitting community experiences this constraint as genuine coordination necessity — forms encode survival knowledge no other channel preserves. From their seat, discontinuation would be catastrophic loss. The reformers experience the same constraint as unnecessary tradition obscuring knowledge that can be more efficiently transmitted through rationalized means. From their seat, form fidelity is performance masquerading as function. The engine computes per-seat classification from structural data: the community's identity-locked exit and institutional power place it near the beneficiary end; younger participants' constrained exit (knowledge only accessible through practice) and moderate power place them nearer center; reformers' mobile exit and institutional exclusion place them at the excluded edge. This divergence is structural, not evaluative.
 *
 * DIRECTIONALITY LOGIC:
 *   The ritual_transmitting_community is the agenda-setter and primary beneficiary: it maintains the forms, controls access, and derives institutional identity and continuity from the practice. Directionality for this seat is low (d near 0.2), reflecting beneficiary status and institutional power despite identity-lock (identity-lock is a characteristic of seats near the beneficiary end — people hold positions not just because they are coerced but because their identity has fused with the role). Younger participants are beneficiaries in that they receive survival-knowledge transmission, but they are also constrained — they cannot exit or access the knowledge through alternative means, and they bear the opportunity cost of time devoted to ritual practice. Directionality is symmetric (d near 0.5): genuine coordination benefit balanced against constrained exit and costs. External reformers are excluded rather than coordinated — they have high directionality (d near 0.7) not because they bear extraction but because they are expelled from the system altogether. The analytical scholars have directionality approaching analytical (d near 0.0): they are observers, not parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — transmitting survival knowledge across generations when catastrophe may disrupt normal institutional channels — remains contested and arguably live in communities where catastrophic risk persists or is commemorated. There is no evidence that the founding problem has been solved and the forms persist as zombie institutions (which would indicate mandatrophy). The constraint's persistence is justified by active belief in functional necessity, not by theatrical maintenance after the function has disappeared. Mandatrophy is not a feature of this constraint at the interval measured. However, if measuring longer intervals (centuries) or in communities where catastrophic risk has materially declined, mandatrophy would become a candidate hypothesis: forms might persist through pure institutional inertia even if the functional justification has dissolved. The theater ratio is low enough that no active mandatrophy signal emerges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_function_separability,
    'Are ritual form and survival-competence function genuinely inseparable, or is the form a contingent delivery mechanism whose content can be extracted and transmitted through alternative means?',
    'Intervention studies in communities practicing ritual transmission: introduce explicit propositional instruction or documentation while removing or simplifying ritual forms; measure whether threat-recognition, resource-coordination, and resilience outcomes degrade. Compare cohorts: those receiving only modernized instruction vs. those maintaining full ritual practice vs. those receiving hybrid transmission.',
    'If inseparable: the constraint is a mountain-substrate rope — ritual fidelity is functionally necessary and discontinuation produces measurable harm. If separable: the constraint becomes a scaffold or piton — the form persists through tradition inertia but the functional justification has dissolved, and the constraint becomes pure performance. The measurement changes the claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(form_function_separability, empirical, 'Whether embodied ritual form is a necessary substrate for survival-knowledge transmission or a contingent delivery mechanism.').

omega_variable(
    identity_lock_vs_functional_commitment,
    'Is the younger generation''s participation in ritual motivated by genuine belief in the form-function link, institutional identity with the community, or social pressure and exclusion risk?',
    'Ethnographic interviews and participant observation documenting explicit justifications for form fidelity; exit analysis tracking what happens when participants leave the community or discontinue practice; measurement of whether stated reasons align with functional outcomes vs. identity-maintenance explanations.',
    'If motivated primarily by identity fusion and exclusion risk, suppression is higher than authored and the constraint approaches snare characteristics. If motivated by genuine belief in functional necessity, suppression is lower and the rope framing holds. Identity-lock can coexist with either; the distinction affects whether participants are locked by external constraint or internalized commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_functional_commitment, empirical, 'The locus of motivation for ritual fidelity: functional belief, identity fusion, or coercive social structure.').

omega_variable(
    sibling_reading_relation_ambiguity,
    'How do the three readings of the catastrophe_memory_transmission kernel relate: Do they foreclose each other within a single coherent framework, or do they coexist as different parties'' interpretations of the same practices?',
    'Comparative reading analysis: interview communities and scholars to determine whether they hold multiple framings simultaneously or view them as contradictory alternatives. Analyze whether practices justified under one reading (e.g., symbol_continuity) can equally well serve the other readings'' functions (e.g., operational_competence). Check institutional boundaries: do different communities practice the same forms under different readings, or does each community commit to one reading?',
    'If readings foreclose each other: the kernel contest is adversarial; accepting one reading requires rejecting another. This constrains the engine''s classification: different readings of the same constraint will compute to different types if the foreclosure is genuine. If readings coexist: multiple communities can practice the same forms with different institutional justifications, and classification per-reading is legitimate. The kernel structure itself — whether it is contested or plural — becomes the omega''s resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_relation_ambiguity, conceptual, 'Whether sibling readings of the catastrophe_memory_transmission kernel logically foreclose each other or can coexist as live positions.').

omega_variable(
    suppression_mechanism_locus,
    'Is the suppression of alternative transmission paths (exclusion of reformers, gatekeeping of forms, internalized belief that alternatives are illegitimate) structural enforcement by the community''s institutional power, or internalized commitment embedded in participants'' identity and belief?',
    'Post-exit tracking: do participants who leave the community retain belief that ritual form is functionally necessary, or do they abandon the belief once institutional enforcement is removed? Do excluded reformers maintain their confidence that alternatives would work, or do they eventually accept inseparability after encountering resistance? Interview data on explicit reasoning for form fidelity vs. observed enforcement mechanisms.',
    'If structural: the constraint persists through gatekeeping and enforcement machinery; removing institutional barriers (legal pressure to modernize, external funding for alternative transmission, demographic shifts that weaken institutional coherence) could dissolve the constraint. If internalized: participants carry the suppression-belief with them after exit, and fixing requires re-education or generational change; external pressure alone is insufficient. If both: requires addressing both enforcement and cognition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_locus, empirical, 'The locus of suppression: external institutional gatekeeping vs. internalized belief in form necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t12, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement_basis(cata_tr_t12, observed).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(cata_tr_t25, observed).
narrative_ontology:measurement(cata_tr_t38, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 38, 0.13).
narrative_ontology:measurement_basis(cata_tr_t38, observed).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement_basis(cata_tr_t50, observed).
narrative_ontology:measurement(cata_tr_t62, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 62, 0.11).
narrative_ontology:measurement_basis(cata_tr_t62, observed).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 75, 0.12).
narrative_ontology:measurement_basis(cata_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t12, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement_basis(cata_be_t12, observed).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(cata_be_t25, observed).
narrative_ontology:measurement(cata_be_t38, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 38, 0.39).
narrative_ontology:measurement_basis(cata_be_t38, observed).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(cata_be_t50, observed).
narrative_ontology:measurement(cata_be_t62, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 62, 0.38).
narrative_ontology:measurement_basis(cata_be_t62, observed).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 75, 0.38).
narrative_ontology:measurement_basis(cata_be_t75, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_transmission__hybrid_embedded_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__hybrid_embedded_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__operational_competence_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_transmission kernel admits three distinct readings based on different interpretations of what ritual transmits and why form fidelity matters. This story instantiates the hybrid_embedded reading: form and function are co-constitutive, ritual shape encodes survival competence that cannot be extracted into propositional form. The sibling_symbol_continuity reading treats mourning and identity as intrinsic communal goods whose transmission is valuable independent of operational survival function. The sibling_operational_competence reading treats the survival knowledge (threat-recognition, resource-coordination) as the essential good, with ritual form instrumentally justified and potentially replaceable. All three readings share a commitment to ritual transmission across catastrophe-shaped communities; they diverge on the reasons ritual form must be preserved. The engine computes per-reading classification from the structural data each reading authors; type divergence across readings indicates the kernel contest is genuinely adversarial (different readings entail different institutional arrangements and beneficiary/victim distributions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
