% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Catastrophe Memory Transmission via Operational Competence
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This reading of the catastrophe_memory_transmission kernel evaluates
 *   ritual elements by their operational yield: Passover as rapid-departure
 *   readiness drill, Tisha B'Av as resource-scarcity training, Sabbath as
 *   weekly systems-check. The constraint is claimed as a rope — a
 *   coordination mechanism that solves the genuine collective-action problem
 *   of transmitting survival competence without literacy or centralized
 *   command. Beneficiaries are community members and future generations who
 *   gain embodied readiness; victims are ritual formalists and symbolic
 *   literalists whose investment in symbolic fidelity is devalued when
 *   operational competence becomes the metric. The engine will compute
 *   per-seat classifications from the structural data; this reading's claim
 *   (rope) and metrics (low extraction, low suppression) are authored
 *   independently.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.35).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.2).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Catastrophe Memory Transmission via Operational Competence").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, 'c3876744-ac7b-469d-9001-7bf2b14dda0c').
narrative_ontology:cs_kernel_codification('c3876744-ac7b-469d-9001-7bf2b14dda0c', distributed).
narrative_ontology:cs_authority_grounding('c3876744-ac7b-469d-9001-7bf2b14dda0c', practice).
narrative_ontology:cs_interpretation_layer_present('c3876744-ac7b-469d-9001-7bf2b14dda0c').
narrative_ontology:cs_reading_relation('c3876744-ac7b-469d-9001-7bf2b14dda0c', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3876744-ac7b-469d-9001-7bf2b14dda0c', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('c3876744-ac7b-469d-9001-7bf2b14dda0c', foundational, operational_yield_is_primary_metric).
narrative_ontology:cs_axiom_status(operational_yield_is_primary_metric, holdable).
narrative_ontology:cs_axiom_grounding('c3876744-ac7b-469d-9001-7bf2b14dda0c', operational_yield_is_primary_metric, empirically_contingent).
narrative_ontology:cs_axiom('c3876744-ac7b-469d-9001-7bf2b14dda0c', secondary, ritual_competence_transfer_is_sufficient).
narrative_ontology:cs_axiom_status(ritual_competence_transfer_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('c3876744-ac7b-469d-9001-7bf2b14dda0c', ritual_competence_transfer_is_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('c3876744-ac7b-469d-9001-7bf2b14dda0c', operational_competence_framework).
narrative_ontology:cs_drift_state('c3876744-ac7b-469d-9001-7bf2b14dda0c', contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c3876744-ac7b-469d-9001-7bf2b14dda0c', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, ritual_formalists).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, symbolic_literalists).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, operational_competence_transmission_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in rituals that rehearse survival patterns — pattern recognition of environmental cues, resource coordination under scarcity, threat assessment drills. Gain embodied competence for catastrophe response. Can adopt, adapt, or leave the ritual repertoire with moderate friction; alternatives (manuals, training) exist but are less accessible.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, community_members, beneficiary,
    moderate, generational, mobile, local).

% Invest authority and identity in symbolic fidelity and ritual form. Bear opportunity costs when operational yield is prioritized over symbolic continuity — their expertise and status are devalued if ritual is judged only by survival drills. Exit requires abandoning identity-bound practices and communal recognition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_formalists, payer,
    moderate, biographical, constrained, local).

% Transmit and adapt ritual elements across generations; evaluate practices by operational yield (e.g., Passover rapid-departure readiness, Tisha B'Av resource-scarcity simulations). Maintain the repertoire, decide which elements are retained or modified. Their role depends on the ritual system's perceived efficacy; exit means leaving a vocation embedded in communal structure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_practitioners, agenda_setter,
    organized, biographical, constrained, local).

% Analyze ritual as a survival-competence transmission system; document pattern-recognition, resource-coordination, and threat-assessment structures across traditions. No stake in operational outcome; their exit is analytical (changing research focus).
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, scholars_of_ritual, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits survival competence across generations by rehearsing pattern recognition (environmental cues), resource coordination (distribution under scarcity), and threat assessment (rapid-departure readiness) in a low-tech, intergenerational, literacy-independent format.
% TRANSFER_FUNCTION: Moves operational competence from the ritual repertoire into practitioners' embodied readiness; the cost is ritual maintenance effort (time, cognitive load, material resources), the benefit is survival capacity when catastrophe recurs.
% ABSENT_VOICES: Communities that have lost ritual repertoires entirely — disrupted diasporas, assimilated groups, populations without intact transmission chains — would object to the claim that ritual is sufficient for survival competence. They are absent because the constraint presupposes an intact ritual system to evaluate.
% DISAPPEARANCE_RATIONALE: Without ritual rehearsal, communities lose embodied pattern-recognition and coordination drills for catastrophe scenarios. Alternative transmission (manuals, formal training, digital archives) is less robust, less accessible under collapse conditions, and fails to encode non-propositional muscle memory.
% FOUNDING_PROBLEM: Communities facing recurrent catastrophes — exile, famine, persecution, displacement — needed reliable, low-tech, intergenerational transmission of survival behaviors without dependence on literacy, centralized instruction, or stable infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of ritual as risk-management (e.g., Sosis & Alcorta on costly signaling; Rappaport on liturgical order) corroborate from outside the beneficiary set. Survivor testimonies from Holocaust, Armenian genocide, and forced displacement contexts attest to ritual's operational role in maintaining group coherence and readiness.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__operational_competence_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.35) because the ritual repertoire is maintained by practitioners for communal benefit, not for rent extraction. Suppression is low (0.2) because participation is largely voluntary and alternatives exist (though less effective). Theater ratio is low (0.15) because the rehearsal function is real and continuously validated by catastrophe recurrence. Accessibility collapse is moderate (0.4) because once the operational logic is understood, some alternative transmission methods become thinkable, but the embodied, intergenerational format retains unique advantages. Resistance is low (0.2) because the constraint meets little active opposition — its persistence depends on perceived efficacy, not coercion.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (ritual_practitioners) experiences the constraint as genuine coordination they administer; the payer seat (ritual_formalists) experiences it as extraction of their symbolic authority. The beneficiary seat (community_members) experiences net subsidy. The observer seat sees the full structure. The engine computes this divergence from the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and future generations are structural beneficiaries (d near 0.0) — they receive survival competence at low marginal cost. Ritual formalists are structural targets (d near 0.8) — their symbolic capital is extracted when operational yield becomes the evaluation metric. Ritual practitioners sit near symmetric (d ~0.5) — they bear maintenance costs but gain authority from successful transmission. Scholars are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope prevents mislabeling this transmission system as pure extraction (snare) or natural law (mountain). The founding problem (recurrent catastrophe survival) is contested — modern threats (nuclear, pandemic, climate) may require different competence — but the coordination function (intergenerational, low-tech rehearsal) remains live for communities facing displacement and collapse. The mandate has not atrophied; it has shifted scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does the operational_competence_reading structurally relate to its sibling readings (symbol_continuity_reading, hybrid_embedded_reading) within the catastrophe_memory_transmission kernel?',
    'Comparative analysis of ritual evaluation criteria across communities: if communities consistently prioritize operational yield over symbolic fidelity, this reading forecloses the others; if they maintain both criteria simultaneously, readings coexist; if operational evaluation reshapes symbolic practice without eliminating it, influence holds.',
    'If forecloses, the kernel has a single dominant reading and other framings are cover stories. If coexists_with, the kernel is genuinely multi-stable and classification depends on which reading''s community is analyzed. If influences, this reading''s operational metric creates downstream pressure on symbolic fidelity without logical exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Structural relationship between this reading and its siblings in the contested kernel.').

omega_variable(
    operational_yield_measurement,
    'Can the operational yield of ritual elements (pattern recognition, resource coordination, threat assessment) be empirically distinguished from placebo effects, social cohesion benefits, or symbolic comfort?',
    'Controlled studies comparing catastrophe outcomes for communities with intact operational rituals vs. those with only symbolic rituals vs. those with secular training; natural experiments from ritual disruption events.',
    'If operational yield is empirically inseparable from cohesion/comfort, the rope claim weakens — the constraint may be a snare extracting compliance under a competence cover story. If separable and positive, rope classification strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operational_yield_measurement, empirical, 'Whether the claimed coordination function has measurable operational reality distinct from psychosocial effects.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low suppression measured here structural (voluntary participation) or internalized (communities believe ritual is the only way, having lost alternatives)?',
    'Post-disruption observation: if communities that lose ritual repertoires develop effective secular alternatives, suppression was structural. If they remain vulnerable and unable to reorganize, suppression was internalized.',
    'If internalized, effective suppression is higher than measured — the constraint carries its own suppression mechanism via identity fusion. This would shift classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in ritual transmission systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmto_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cmto_tr_t20, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(cmto_tr_t40, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(cmto_tr_t60, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement(cmto_tr_t80, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(cmto_tr_t100, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(cmto_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cmto_be_t20, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(cmto_be_t40, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(cmto_be_t60, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 60, 0.32).
narrative_ontology:measurement(cmto_be_t80, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 80, 0.34).
narrative_ontology:measurement(cmto_be_t100, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(cmto_su_t0, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(cmto_su_t20, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 20, 0.17).
narrative_ontology:measurement(cmto_su_t40, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement(cmto_su_t60, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 60, 0.19).
narrative_ontology:measurement(cmto_su_t80, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 80, 0.2).
narrative_ontology:measurement(cmto_su_t100, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__operational_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__operational_competence_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial label 'ritual as catastrophe memory' into three structurally distinct claims with different ε values, beneficiary/victim structures, and coordination functions. The operational_competence_reading evaluates by operational yield (rope); symbol_continuity_reading evaluates by symbolic fidelity (likely mountain or rope); hybrid_embedded_reading evaluates by non-propositional knowledge embedding (likely tangled_rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
