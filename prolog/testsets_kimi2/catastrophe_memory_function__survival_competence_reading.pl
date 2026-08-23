% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Passover Survival-Competence Transmission (D5 Reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint story instantiates the survival_competence_reading (D5)
 *   of the catastrophe_memory_function kernel, using the Passover ritual as
 *   the central case. Under this reading, the ritual is not primarily
 *   memorial obligation or boundary maintenance, but a decentralized
 *   pedagogical technology that transmits actionable survival knowledge
 *   across generations through annual embodied rehearsal. The reading treats
 *   the ritual as a coordination mechanism enabling institutional
 *   transformation and continuity in the face of catastrophe. This file
 *   isolates the D5 functional pole; sibling readings
 *   (mourning_practice_reading, hybrid_transformation_reading) instantiate
 *   other poles of the same kernel.
 *
 * KEY AGENTS:
 *   - commemorative_community: Primary beneficiary (organized/identity_locked) â receives survival competence through participation
 *   - successor_generation: Secondary beneficiary (powerless/identity_locked) â absorbs transmitted adaptive knowledge
 *   - household_practitioners: Decentralized agenda_setter (moderate/identity_locked) â enacts and adapts the ritual
 *   - institutional_guardians: Centralized agenda_setter (institutional/constrained) â preserves textual and normative boundaries
 *   - assimilated_former_members: Excluded observer (moderate/mobile) â has exited the identity system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.25).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.23).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.23).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Passover Survival-Competence Transmission (D5 Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, '82ee871b-1370-4a49-aeda-19441a41dd5b').
narrative_ontology:cs_kernel_codification('82ee871b-1370-4a49-aeda-19441a41dd5b', fixed_text).
narrative_ontology:cs_authority_grounding('82ee871b-1370-4a49-aeda-19441a41dd5b', lineage).
narrative_ontology:cs_interpretation_layer_present('82ee871b-1370-4a49-aeda-19441a41dd5b').
narrative_ontology:cs_reading_relation('82ee871b-1370-4a49-aeda-19441a41dd5b', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('82ee871b-1370-4a49-aeda-19441a41dd5b', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('82ee871b-1370-4a49-aeda-19441a41dd5b', foundational, survival_competence_primary).
narrative_ontology:cs_axiom_status(survival_competence_primary, holdable).
narrative_ontology:cs_axiom_grounding('82ee871b-1370-4a49-aeda-19441a41dd5b', survival_competence_primary, instrumental).
narrative_ontology:cs_axiom('82ee871b-1370-4a49-aeda-19441a41dd5b', foundational, embodied_rehearsal_necessary).
narrative_ontology:cs_axiom_status(embodied_rehearsal_necessary, holdable).
narrative_ontology:cs_axiom_grounding('82ee871b-1370-4a49-aeda-19441a41dd5b', embodied_rehearsal_necessary, instrumental).
narrative_ontology:cs_reference_frame('82ee871b-1370-4a49-aeda-19441a41dd5b', catastrophe_resilience_reference).
narrative_ontology:cs_drift_state('82ee871b-1370-4a49-aeda-19441a41dd5b', contemporary_secular_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('82ee871b-1370-4a49-aeda-19441a41dd5b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, commemorative_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, successor_generation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective body that annually rehearses catastrophe survival through ritual performance, embedding adaptive knowledge in shared practice. Members cannot exit without dissolving the identity structure that constitutes the group, but they receive the coordination benefit of transmitted competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, commemorative_community, beneficiary,
    organized, generational, identity_locked, global).

% Children and young members who learn survival scripts, leadership roles, and mobilization heuristics through mandatory participatory roles in the ritual. They are the downstream recipients of the competence transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, successor_generation, beneficiary,
    powerless, biographical, identity_locked, global).

% Heads of households who lead the ritual performance, adapt the script to local conditions, and bear the primary labor of preparation and teaching. They act as decentralized nodes in the transmission network.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, household_practitioners, agenda_setter,
    moderate, generational, identity_locked, global).

% Religious authorities and textual custodians who set normative boundaries on valid ritual performance, ensuring continuity with the founding narrative and preventing deviation that might erode the competence content.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, institutional_guardians, agenda_setter,
    institutional, civilizational, constrained, global).

% Individuals who have exited the ritual system through assimilation or secularization and no longer participate in competence transmission. Their absence represents a loss of decentralized network density.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, assimilated_former_members, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__survival_competence_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables intergenerational transmission of actionable survival knowledgeâescape routes, resource hoarding, rapid mobilization, adaptive leadership selectionâthrough annual embodied rehearsal when written records may be lost and social cohesion is threatened.
% TRANSFER_FUNCTION: Moves survival heuristics, catastrophe scripts, and institutional adaptation patterns from experienced generation (household practitioners, institutional guardians) to successor generation via mandatory participatory roles and sensory-motor embedding.
% ABSENT_VOICES: Secular educators and formal pedagogy advocates who would argue that survival competence is better transmitted through standardized schooling or digital archives; assimilated former members who have exited and no longer see ritual as necessary for resilience; and rival communities with non-ritual disaster-preparedness frameworks.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the decentralized network of household competence transmission would collapse. Alternative pedagogies lack the embodied, intergenerational, identity-embedded rehearsal structure; survival knowledge would fragment into individualized, non-replicable information, and the community's adaptive capacity would degrade over one to two generations.
% FOUNDING_PROBLEM: How to preserve actionable survival knowledge and social coordination capacity across catastrophic disruptions (dispersal, persecution, loss of centralized institutions) when written records may be destroyed and normal pedagogical channels are severed.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists of ritual and disaster-resilience researchers outside the beneficiary community corroborate that ritualized transmission preserves knowledge under disruption. Conversely, secular historians of the Exodus narrative attest the founding catastrophe is historical-mythic rather than recurrent, implying the survival function may be retrojected. The corroboration is therefore split across disciplinary seats, with no unanimous outside attestation.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__survival_competence_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15) is authored just above the identity_coordination Boltzmann floor (0.08), reflecting the genuine coordination cost of annual ritual participation without elevating it to extraction. Suppression (0.25) is low because decentralized household practice lacks centralized enforcement; continuity depends on identity commitment rather than coercion. Theater_ratio (0.23) shows modest performative drift over the interval but remains below the 0.5 proxy-goal threshold, indicating that the survival-pedagogy function is still operative. Accessibility_collapse (0.45) is moderate: alternative pedagogies exist (textual, digital), but none replicate the embodied, intergenerational rehearsal structure. Resistance (0.15) is low because participants are net beneficiaries. The measurement series shares a single time grid (0â100) to prevent misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The institutional_guardian seat experiences the ritual as a fixed textual inheritance requiring orthodox preservation; the household_practitioner seat experiences it as a flexible survival drill adaptable to local conditions. The successor_generation experiences it as obligatory socialization, while the assimilated_former_member sees it as a dispensable cultural artifact. The engine computes divergent classifications from these structural positions: guardians and practitioners (agenda_setters with constrained or identity-locked exit) sit nearer symmetric than the beneficiary community, while excluded mobile agents register negligible extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (commemorative_community, successor_generation) anchor directionality toward the subsidy end (low d). Identity-locked exit modulates this upward, preventing full subsidy because the community cannot exit without dissolving the identity that constitutes it. No victim group is declared because this reading does not identify a separable extracted class; costs are internal to the beneficiary structure. Household practitioners and institutional guardians are agenda_setters without beneficiary or victim labels, so their d reverts to the moderate-power fallback, producing a symmetric structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preserving actionable knowledge across catastrophic disruption â is authored as contested rather than dead, acknowledging debate over whether modernity has extinguished the survival function. The temporal measurements show gentle theater_ratio growth (0.10 to 0.23) without crossing the 0.5 Goodhart threshold, suggesting coordination is aging but not yet atrophied into piton. Were the founding problem dead and theater_ratio above 0.5, the constraint would compute as piton; the current profile guards against that misclassification by keeping the coordination function visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_content_erosion,
    'Does the ritual still encode actionable survival competence, or has the content eroded into symbolic gesture while retaining the form?',
    'Comparative ethnography of ritual content across communities under stress vs. stability: if stressed communities activate survival-relevant script variants and stable communities do not, the competence is latent and context-dependent.',
    'If eroded, extractiveness is overstated as coordination cost; the constraint drifts toward identity_coordination without survival function (higher theater, lower genuine coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_content_erosion, empirical, 'Whether survival competence content persists in ritual performance').

omega_variable(
    decentralized_authority_tension,
    'Does decentralized household practice strengthen or weaken survival-competence transmission relative to centralized institutional control?',
    'Network analysis of ritual variation: if decentralized adaptation produces locally effective variants that survive disruption, decentralization is functional; if it produces drift and norm dissolution, centralization would be the genuine coordination mechanism.',
    'If decentralization weakens transmission, the constraint is better modeled as institutional extraction (institutional_guardians extracting compliance from households) rather than pure coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralized_authority_tension, conceptual, 'Decentralized vs centralized ritual authority ambiguity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__survival_competence_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__survival_competence_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__survival_competence_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__survival_competence_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__survival_competence_reading, theater_ratio, 100, 0.23).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 20, 0.11).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 40, 0.12).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 60, 0.13).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 80, 0.14).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 100, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_function__survival_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
