% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__operational_competence_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: catastrophe_memory_transmission__operational_competence_reading
 *   human_readable: Catastrophe Memory Transmission â Operational Competence Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint instantiates the operational_competence_reading of the
 *   catastrophe_memory_transmission kernel. It treats ritual not as symbolic
 *   continuity for its own sake, but as a functional coordination mechanism
 *   that encodes and transmits survival competenceâpattern recognition,
 *   resource coordination, and threat rehearsalâacross generations. The
 *   reading is contested by symbol_continuity_reading (which locates survival
 *   value in the symbolic form itself) and hybrid_embedded_reading (which
 *   treats competence and symbol as inseparably fused). Within this reading,
 *   ritual elements are evaluated by operational yield, and the symbolic
 *   carrier is treated as instrumental rather than intrinsically necessary.
 *
 * KEY AGENTS:
 *   - Ritual practitioners: Primary beneficiaries who extract survival competence through embodied practice (moderate power, mobile exit).
 *   - Descendant communities: Intergenerational beneficiaries who receive preparedness coordination without centralized institutions (moderate power, mobile exit).
 *   - Ritual specialists: Agenda-setters who maintain and interpret the ritual corpus, deriving authority from transmission efficacy rather than extraction (organized power, constrained exit by role).
 *   - Symbolic literalists: Payers who bear practice costs without extracting operational competence, treating ritual as pure symbol (moderate power, mobile exit).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.18).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Catastrophe Memory Transmission â Operational Competence Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, '39358080-7ebb-4c19-8eeb-f6cda9f1332d').
narrative_ontology:cs_kernel_codification('39358080-7ebb-4c19-8eeb-f6cda9f1332d', fixed_text).
narrative_ontology:cs_authority_grounding('39358080-7ebb-4c19-8eeb-f6cda9f1332d', lineage).
narrative_ontology:cs_interpretation_layer_present('39358080-7ebb-4c19-8eeb-f6cda9f1332d').
narrative_ontology:cs_reading_relation('39358080-7ebb-4c19-8eeb-f6cda9f1332d', catastrophe_memory_transmission__symbol_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('39358080-7ebb-4c19-8eeb-f6cda9f1332d', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('39358080-7ebb-4c19-8eeb-f6cda9f1332d', foundational, ritual_competence_transmission).
narrative_ontology:cs_axiom_status(ritual_competence_transmission, holdable).
narrative_ontology:cs_axiom_grounding('39358080-7ebb-4c19-8eeb-f6cda9f1332d', ritual_competence_transmission, empirically_contingent).
narrative_ontology:cs_axiom('39358080-7ebb-4c19-8eeb-f6cda9f1332d', foundational, operational_yield_criterion).
narrative_ontology:cs_axiom_status(operational_yield_criterion, holdable).
narrative_ontology:cs_axiom_grounding('39358080-7ebb-4c19-8eeb-f6cda9f1332d', operational_yield_criterion, instrumental).
narrative_ontology:cs_reference_frame('39358080-7ebb-4c19-8eeb-f6cda9f1332d', operational_functionalism).
narrative_ontology:cs_drift_state('39358080-7ebb-4c19-8eeb-f6cda9f1332d', contemporary_ritual_studies, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('39358080-7ebb-4c19-8eeb-f6cda9f1332d', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, descendant_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, ritual_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, symbolic_literalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Repeatedly perform ritual actions that encode survival competenceâpattern recognition, resource coordination, and threat rehearsalâgaining operational skills through embodied practice. They can exit by abandoning the tradition or switching to explicit institutional training, though this may carry social costs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_practitioners, beneficiary,
    moderate, generational, mobile, regional).

% Receive intergenerational transmission of catastrophe preparedness without reliance on centralized archives or state infrastructure. The ritual coordinates memory across time, ensuring competence persists even when formal institutions fail.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, descendant_communities, beneficiary,
    moderate, generational, mobile, regional).

% Maintain, interpret, and adjudicate correct performance of the ritual corpus. Their authority derives from lineage and from the demonstrable efficacy of transmission. They set standards for valid practice but do not extract asymmetric rents from the coordination.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_specialists, agenda_setter,
    organized, generational, constrained, regional).

% Adhere to ritual form and symbolic content without extracting operational competence. They bear the time and resource costs of practice but receive only social belonging and identity continuity, missing the survival-coordination benefits the ritual is structurally capable of delivering.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, symbolic_literalists, payer,
    moderate, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmit survival-relevant competence across generations without centralized institutions or propositional instruction, using embodied, repetitive practice as the coordination medium.
% TRANSFER_FUNCTION: Moves pattern-recognition heuristics, resource-coordination scripts, and threat-assessment rehearsal from experienced practitioners to novices and future community members through ritual performance.
% ABSENT_VOICES: Secular emergency-management agencies and scientific disaster-preparedness programs would argue that ritual transmission is epistemically inferior to explicit, evidence-based training; they are excluded from the traditional interpretive framework that validates ritual efficacy.
% DISAPPEARANCE_RATIONALE: If the ritual constraint disappeared, communities would lose a decentralized, low-infrastructure mechanism for preserving catastrophe readiness. Explicit state or educational institutions would need to replicate the intergenerational coordination function, and social organization around preparedness would shift toward centralized archives.
% FOUNDING_PROBLEM: Catastrophe preparedness degrades across generations when encoded only in explicit propositional knowledge or centralized storage, both of which fail during social collapse. Ritual embeds competence in embodied practice that survives institutional breakdown.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists and disaster-sociologists outside the benefiting communities attest that centralized knowledge repositories are vulnerable to infrastructure failure; evolutionary theorists corroborate that embodied transmission is robust across generational gaps. Religious communities assert the problem is live, but independent researchers in risk management and cultural anthropology provide the external corroboration.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__operational_competence_reading, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__operational_competence_reading_tests).
:- end_tests(catastrophe_memory_transmission__operational_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the constraint functions as genuine coordination: practitioners invest ritual effort and receive survival competence in return, producing a roughly symmetric exchange. Suppression is low (0.18) because the constraint does not actively block alternative preparedness mechanisms; it persists by efficacy, not coercion. Theater ratio is low (0.15) because the operational reading treats performance as functional rehearsal rather than display. Accessibility collapse is moderate (0.35): once the operational code is understood, alternatives such as explicit institutional training remain available and may dominate under stable conditions. Resistance is low (0.12) because the constraint meets little organized opposition; its persistence is argued on demonstrated yield. The temporal series show slight drift upward in extractiveness as modern alternatives reduce the relative efficiency of ritual transmission, and a flat-to-slight rise in theater as symbolic elaboration occasionally displaces operational content.
 *
 * PERSPECTIVAL GAP:
 *   From the operational practitioner's seat, the constraint is ropeâa low-overhead coordination device solving a genuine intergenerational knowledge-transfer problem. From the symbolic literalist's seat, the same ritual structure appears costly and opaque, yielding social belonging but not survival competence. The engine computes divergent seat types from this structural asymmetry even though the authored claim is rope. The operational specialist and the symbolic literalist sit at the same nominal power level but experience different directionalities because their exit options and interpretive frames differ.
 *
 * DIRECTIONALITY LOGIC:
 *   Descendant communities and ritual practitioners are structural beneficiaries: they receive survival competence through participation, placing them near the beneficiary end of directionality. Ritual specialists sit toward the symmetric middleâthey administer the constraint and derive authority from it, but their authority is contingent on successful transmission rather than rent extraction. Symbolic literalists are structural payers: they bear the costs of practice without extracting the operational competence, placing them nearer the target end. Their position is produced by their own interpretive frame rather than by coercion embedded in the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by distinguishing the operational reading from a degraded piton. A piton would show high theater_ratio and no party benefiting enough to maintain the ritual; here the theater ratio is low and beneficiary communities actively depend on competence transmission. If the operational content atrophied and only symbolic form remained, the constraint would migrate toward piton or snare; the low current theater_ratio and live founding problem argue against mandatrophy in this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_content_atrophy,
    'Has the ritual''s operational survival competence genuinely persisted, or has the operational content atrophied while the symbolic shell remains?',
    'Ethnographic observation and task-performance studies measuring whether ritual practitioners outperform non-practitioners on relevant survival tasks (rapid departure, resource rationing, threat detection) under stress conditions.',
    'If operational content has atrophied, this reading misidentifies the constraint: it would compute as piton or snare rather than rope, and the symbolic_literalist seat would become the dominant structural reality rather than a minority payer position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_content_atrophy, empirical, 'Whether ritual still encodes extractable survival competence or is pure symbolic inheritance.').

omega_variable(
    symbol_substance_separability,
    'Can survival competence be extracted from ritual independently of its symbolic form, or are the two epistemically inseparable as hybrid_embedded_reading claims?',
    'Comparative analysis of communities that have modified ritual symbolism while preserving operational structure; if competence transmission survives symbolic change, separability is demonstrated.',
    'If inseparable, the operational reading''s evaluation criterion (operational yield alone) is incoherent, and the constraint should be reclassified under a hybrid reading or as a tangled_rope where coordination and identity extraction are merged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbol_substance_separability, conceptual, 'Whether operational competence is separable from symbolic carrier.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 5, 0.19).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 15, 0.2).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 20, 0.21).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 25, 0.21).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 30, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_transmission__operational_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_transmission kernel, which decomposes into three structurally distinct claims under the epsilon-invariance principle. The operational_competence_reading evaluates ritual by functional yield; symbol_continuity_reading locates value in symbolic persistence; hybrid_embedded_reading treats the two as inseparable. Each reading carries a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
