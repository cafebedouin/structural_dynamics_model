% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Ritual as Operational Competence Transmission
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the operational_competence_reading of
 *   the catastrophe_memory_transmission kernel. Under this reading, ritual is
 *   not merely symbolic continuity or identity performance but a functional
 *   coordination mechanism that encodes survival competence through pattern
 *   recognition, resource coordination, and threat-assessment rehearsal.
 *   Passover rapid-departure readiness and Tisha B'Av resource-scarcity
 *   training are exemplary instances: the ritual form carries operational
 *   payload. The beneficiary is diffuse future survival capacity vested in
 *   the ritual community; the potential victim is the practitioner who
 *   mistakes symbol for substance and pays the ritual's costs without
 *   extracting its competence. The claim is ropeâpure coordinationâwhile
 *   the metrics independently describe modest extraction from misreading
 *   practitioners and rising theatricality as operational content decays into
 *   symbolic performance over the interval.
 *
 * KEY AGENTS:
 *   - tradition_bearers: Primary agenda_setter (organized/identity_locked) â maintains ritual patterns and enforces liturgical fidelity
 *   - ritual_community: Primary beneficiary (organized/identity_locked) â receives survival competence through rehearsal
 *   - symbol_only_practitioners: Payer (moderate/identity_locked) â bears costs of empty performance without operational yield
 *   - secular_observers: Analytical observer (analytical/analytical) â evaluates competence claims from outside the ritual system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Ritual as Operational Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, 'a5a8b86e-2700-4760-83a1-272a1aec7b5b').
narrative_ontology:cs_kernel_codification('a5a8b86e-2700-4760-83a1-272a1aec7b5b', implicit).
narrative_ontology:cs_authority_grounding('a5a8b86e-2700-4760-83a1-272a1aec7b5b', practice).
narrative_ontology:cs_reading_relation('a5a8b86e-2700-4760-83a1-272a1aec7b5b', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5a8b86e-2700-4760-83a1-272a1aec7b5b', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('a5a8b86e-2700-4760-83a1-272a1aec7b5b', foundational, ritual_elements_evaluated_by_operational_yield).
narrative_ontology:cs_axiom_status(ritual_elements_evaluated_by_operational_yield, holdable).
narrative_ontology:cs_axiom_grounding('a5a8b86e-2700-4760-83a1-272a1aec7b5b', ritual_elements_evaluated_by_operational_yield, instrumental).
narrative_ontology:cs_axiom('a5a8b86e-2700-4760-83a1-272a1aec7b5b', foundational, survival_competence_through_embodied_rehearsal).
narrative_ontology:cs_axiom_status(survival_competence_through_embodied_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('a5a8b86e-2700-4760-83a1-272a1aec7b5b', survival_competence_through_embodied_rehearsal, empirically_contingent).
narrative_ontology:cs_reference_frame('a5a8b86e-2700-4760-83a1-272a1aec7b5b', operative_survival_transmission).
narrative_ontology:cs_drift_state('a5a8b86e-2700-4760-83a1-272a1aec7b5b', contemporary_secular_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a5a8b86e-2700-4760-83a1-272a1aec7b5b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, ritual_community).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, symbol_only_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives survival competence through ritual rehearsal: rapid-departure protocols, resource-scarcity fasting, threat-pattern recognition embedded in liturgical action. Exit means severing the primary vehicle for intergenerational knowledge transfer and communal identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_community, beneficiary,
    organized, generational, identity_locked, regional).

% Performs ritual acts believing them to be purely commemorative or symbolic; expends time, caloric resources, and opportunity cost without gaining the operational competence the ritual is structurally capable of transmitting. Their misreading is reinforced by community framing that emphasizes identity continuity over functional yield.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, symbol_only_practitioners, payer,
    moderate, biographical, identity_locked, regional).

% Maintain and transmit ritual patterns across generations; custodians of choreography, calendar, and prohibitions. They may or may not retain explicit awareness of the operational logic embedded in the practice, but they enforce fidelity to traditional form as the transmission medium itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, tradition_bearers, agenda_setter,
    organized, generational, identity_locked, regional).

% Anthropologists, cognitive scientists of religion, and disaster-resilience researchers who evaluate whether ritual genuinely encodes operational competence or is post-hoc rationalized symbolism. They observe outcomes across traditions but do not participate in the ritual economy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, secular_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__operational_competence_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmit survival-relevant operational knowledge across generations when propositional instruction fails, literacy is absent, or stress levels prevent explicit pedagogy; encodes pattern recognition, resource coordination, and threat-response rehearsal in embodied, repetitive practice.
% TRANSFER_FUNCTION: Moves survival competenceârapid departure readiness, scarcity tolerance, predator or threat cuesâfrom experienced tradition-bearers to novice community members through ritualized rehearsal that simulates catastrophe conditions.
% ABSENT_VOICES: Secular modernists and post-religious communities who view ritual as purely symbolic or superstitious residue; they would argue the competence claim is post-hoc rationalization but are excluded from traditional transmission frameworks and from liturgical authority.
% DISAPPEARANCE_RATIONALE: Without ritual as a competence-transmission channel, communities would need to construct alternative infrastructure for embodied survival training; collective memory of catastrophe-response protocols would fragment, and intergenerational coordination of resource-scarcity behavior would weaken.
% FOUNDING_PROBLEM: How to preserve and transmit survival-relevant operational knowledge across generations in pre-literate or high-stress conditions where explicit instruction is unreliable and written records are absent or inaccessible.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive anthropologists and disaster-resilience researchers outside the benefiting traditions attest that embodied practice can encode operational knowledge; traditional communities attest the problem remains live, while secular modernists attest it has been superseded by literacy and formal education.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__operational_competence_reading, 0.15, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness is low (0.15) because the constraint's primary motion is coordination: it solves a genuine intergenerational knowledge-transfer problem without a concentrated extractor. Suppression is low (0.12) because alternatives such as written manuals or secular disaster training exist and are not actively blocked; the ritual's persistence depends on its functional yield and identity lock-in, not coercion. Theater ratio rises over the interval (0.10 to 0.42) because operational content tends to decay into symbolic performance as environmental pressure relaxes, but the constraint retains substantial functional content. Accessibility collapse is moderate (0.45): once the operational logic is visible, secular alternatives become apparent, though identity lock-in slows adoption. Resistance is negligible (0.08) because few parties contest a mechanism that genuinely enhances group survival.
 *
 * PERSPECTIVAL GAP:
 *   The tradition_bearers and ritual_community experience the constraint as beneficent coordination that secures intergenerational continuity. Symbol_only_practitioners experience it as costly obligation without compensatory yield. Secular observers split between those who read the competence claim as empirically warranted and those who read it as post-hoc rationalization. The engine computes these divergences from the same structural data: beneficiary status plus identity_locked exit produces low effective extraction for the community, while payer status plus identity_locked exit produces higher effective extraction for the symbol-only performers.
 *
 * DIRECTIONALITY LOGIC:
 *   Tradition_bearers and ritual_community sit near the beneficiary end (low d): they control the ritual form and receive the survival competence it transmits. Symbol_only_practitioners sit nearer the target end (high d): they expend resources in the ritual economy without receiving the operational return. Because both community and misreading practitioners share identity_locked exit, the differentiation is driven entirely by the beneficiary/payer role split and the zero-sum time-and-resource transfer within the same ritual container. Secular observers are analytical and outside the directionality derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâtransmitting survival knowledge without literacy under catastrophe conditionsâis either live or dead depending on context. In traditional or crisis-prone settings it remains live, supporting the rope classification. In literate modernity it is arguably dead, which would push the constraint toward scaffold or piton. The operational_competence_reading resists mandatrophy mislabeling by insisting on yield-based evaluation: if the ritual no longer produces measurable survival competence, the reading itself licenses downgrading or abandonment. This prevents the coordination story from becoming a permanent justification for empty performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the operational_competence_reading of the catastrophe_memory_transmission kernel; how would classification change if the hybrid_embedded_reading or symbol_continuity_reading were adopted instead?',
    'Comparative analysis of the sibling constraints in the same kernel family, examining whether operational yield remains separable from symbolic form.',
    'Under symbol_continuity_reading, extraction would reclassify toward near-zero with no victim group; under hybrid_embedded_reading, coordination and symbolism would be inseparable, potentially raising extraction if symbolic enforcement becomes asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Position within the catastrophe_memory_transmission kernel reading set').

omega_variable(
    operational_symbol_boundary,
    'Can ritual competence be cleanly separated from symbolic form, or is the operational yield always embedded in a symbolic container?',
    'Empirical testing of ritual practitioners'' actual survival competence versus matched non-practitioners; ethnographic observation of whether ritual form can be altered without breaking competence transmission.',
    'If inseparable, this reading collapses toward hybrid_embedded_reading and may require reclassification; if separable, the operational evaluation criterion is valid and the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_symbol_boundary, empirical, 'Whether ritual competence is separable from symbolic form').

omega_variable(
    universal_law_contingency,
    'Is the transmission of survival competence through embodied ritual a universal structural feature of human groups, or a contingent coordination mechanism dependent on specific technological and environmental conditions?',
    'Cross-cultural comparison of catastrophe-response rituals; assessment of whether literate and technologically equipped societies fully substitute alternative coordination mechanisms without loss of resilience.',
    'If universal and invariant across contexts, the constraint trends mountain-ward; if contingent on pre-literate or high-stress conditions, it remains rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_law_contingency, empirical, 'Whether ritual competence transmission is universal or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_mem_op_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(catastrophe_mem_op_tr_t20, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(catastrophe_mem_op_tr_t40, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(catastrophe_mem_op_tr_t60, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(catastrophe_mem_op_tr_t80, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement(catastrophe_mem_op_tr_t100, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(catastrophe_mem_op_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(catastrophe_mem_op_be_t20, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 20, 0.09).
narrative_ontology:measurement(catastrophe_mem_op_be_t40, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(catastrophe_mem_op_be_t60, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 60, 0.12).
narrative_ontology:measurement(catastrophe_mem_op_be_t80, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 80, 0.14).
narrative_ontology:measurement(catastrophe_mem_op_be_t100, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_mem_op_su_t0, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(catastrophe_mem_op_su_t20, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 20, 0.06).
narrative_ontology:measurement(catastrophe_mem_op_su_t40, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 40, 0.07).
narrative_ontology:measurement(catastrophe_mem_op_su_t60, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 60, 0.09).
narrative_ontology:measurement(catastrophe_mem_op_su_t80, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 80, 0.11).
narrative_ontology:measurement(catastrophe_mem_op_su_t100, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__operational_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_transmission kernel, decomposed per the Îµ-invariance principle because the kernel's natural-language label conflates structurally distinct claims: operational competence transmission, symbolic continuity, and hybrid embedded practice. Each reading carries its own Îµ, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
