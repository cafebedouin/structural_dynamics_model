% ============================================================================
% CONSTRAINT STORY: survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_survival_competence_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: survival_competence_reading
 *   human_readable: Ritual as Survival Competence Transmission (Catastrophe Memory Preservation)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Ritual preserves operational threat-recognition capacity across
 *   generations by encoding collective memory of catastrophic events into
 *   practiced, embodied competence that survives textual documentation and
 *   generational turnover. This constraint exemplifies the tangled_rope type:
 *   it entangles genuine coordination (the ritual does transmit survival
 *   knowledge) with asymmetric extraction (present-generation participants
 *   bear costs that fund future-generation benefits). The constraint is
 *   instantiated in religious and cultural systems that face recurring or
 *   foreseeable catastrophes — cyclical disasters (seasonal floods, monsoons,
 *   seismic zones), epidemiological threats (plague cycles, endemic disease),
 *   or civilization-level risks (warfare, famine). The
 *   survival_competence_reading treats the ritual as an explicit
 *   knowledge-transmission mechanism where the encoded competence is
 *   threat-recognition skills: pattern recognition for danger signs,
 *   practiced response sequences, psychological resilience under threat
 *   conditions. This reading contrasts with sibling readings that emphasize
 *   mourning practice (emotional processing and identity maintenance) or
 *   hybrid atrophy (ritual once functional, now mostly theatrical). This
 *   story models the survival_competence_reading only — the mechanism by
 *   which ritual trains explicit competence despite imposing high
 *   participation costs on present-generation members who will not directly
 *   benefit from the knowledge they transmit.
 *
 * KEY AGENTS:
 *   - Present-generation ritual participants: Primary victims (powerless/trapped or moderate/constrained) — bear participation costs (time, material goods, emotional labor, suppression of exit options)
 *   - Future generations facing catastrophe: Primary beneficiaries (prospective, generational) — inherit practiced threat-recognition competence without bearing transmission costs
 *   - Religious authorities / ritual specialists: Secondary beneficiary (institutional/arbitrage) — maintain specialist status and authority through ritual knowledge control
 *   - Collective threat-recognition capacity: Abstract beneficiary — the coordination mechanism itself; encoded survival knowledge that the community as a whole retains across generational turnover
 *   - Analytical observer: Sees the entanglement of genuine transmission function with genuine extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(survival_competence_reading, 0.58).
domain_priors:suppression_score(survival_competence_reading, 0.65).
domain_priors:theater_ratio(survival_competence_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(survival_competence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(survival_competence_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(survival_competence_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(survival_competence_reading, "Ritual as Survival Competence Transmission (Catastrophe Memory Preservation)").
narrative_ontology:topic_domain(survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(survival_competence_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(survival_competence_reading, future_generations).
narrative_ontology:constraint_beneficiary(survival_competence_reading, collective_threat_recognition).
narrative_ontology:constraint_victim(survival_competence_reading, present_generation_autonomy).
narrative_ontology:constraint_victim(survival_competence_reading, ritual_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COERCED PARTICIPANT (SNARE) — Individual community members required to participate in costly rituals (time, emotional labor, material goods, bodily submission) with no exit option. The ritual is presented as obligation, not choice. Extraction is maximal from this position: high participation cost, suppression via social sanction, no alternative community membership available. The participant experiences no survival benefit — that benefit accrues to future generations not yet present.
constraint_indexing:constraint_classification(survival_competence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: AMBIVALENT COMMUNITY MEMBER (TANGLED ROPE) — Community members with sufficient resources or status to question participation, but constrained by social exclusion, marriage prospects, inheritance, or reputation damage. Experience both extraction (costly ritual participation, identity loss if they exit) and coordination benefit (genuine preservation of collective threat-recognition competence that protects the community). The constraint entangles legitimate collective defense with coercive demand. Moderate exit cost and some agency distinguish this from pure snare.
constraint_indexing:constraint_classification(survival_competence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: RELIGIOUS AUTHORITY (ROPE) — Institutional actors (priests, elders, ritual specialists) who maintain and transmit the ritual structure. They experience the constraint as pure coordination: organizing collective practice, preserving esoteric knowledge, managing ritual transmission. They benefit from their specialist status and authority. The ritual is their coordinated function, not an extraction mechanism from their position. No experienced suppression — they design and enforce it.
constraint_indexing:constraint_classification(survival_competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: FUTURE GENERATION / PROSPECTIVE BENEFICIARY (ROPE) — From the temporal position of a future generation facing catastrophe, the ritual represents pure coordination: a mechanism by which present populations encoded survival knowledge into practiced competence that future generations inherit and can activate. No cost to those born into the competence; the benefit is automatic. This perspective sees the constraint as legitimate coordination, not extraction. The present generation's costs are background to the future generation's acquisition of essential knowledge.
constraint_indexing:constraint_classification(survival_competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Cross-generational view reveals the true structure: the ritual entangles genuine coordination (survival knowledge transmission) with asymmetric extraction (present-generation coercion funds future-generation benefit). The coordination function is real — ritual does encode and transmit threat-recognition competence in ways that abstract instruction does not. The extraction is also real — the constraint suppresses present-generation exit options and demands disproportionate participation from powerless agents. Both layers are essential to the mechanism's function.
constraint_indexing:constraint_classification(survival_competence_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(survival_competence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(survival_competence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(survival_competence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the asymmetry between present costs and future benefits. The constraint demands significant participation from present-generation members (time, material resources, embodied practice, suppressed exit options) who will not live to see the catastrophe the ritual prepares for. The extraction is justified by the functional requirement — the knowledge cannot be preserved in abstract form alone; it must be practiced, embodied, and transmitted through repeated performance. But the extraction is real: the present generation subsidizes the future generation's survival. Suppression (0.65): Moderate-high, reflecting both structural barriers (social sanction for non-participation, economic dependency on ritual community, geographic isolation) and identity-based binding (ritual membership IS community membership; exit requires identity reorganization). The suppression is not complete — some individuals can and do exit — but the barriers are substantial. Theater ratio (0.48): Moderate, reflecting that the ritual has genuine functional content (threat-recognition training is encoded in performance, body-memory, psychological resilience practice) but also performative elements (ornamentation, repetition that exceeds training necessity, status display by ritual authorities). The constraint is not primarily theatrical — it is not a piton — because the functional core (threat-recognition training) dominates. The measurement trajectory shows slight increase over the interval (theater_ratio 0.35→0.48, extractiveness 0.52→0.58), reflecting the hypothesis that as ritual becomes more distant from actual catastrophe experience, performative elements gradually accumulate while extraction demands remain stable.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. The coerced participant (powerless/trapped) experiences pure extraction (snare) — they bear all costs and receive no benefit within their lifetime. The ambivalent community member (moderate/constrained) experiences the true structure (tangled rope) — they recognize both the extraction and the coordination function, but cannot exit without substantial cost. The religious authority (institutional/arbitrage) experiences pure coordination (rope) — they organize the transmission, benefit from specialist status, and experience no suppression. The future generation (prospective) experiences pure coordination (rope) — they inherit the competence automatically, bearing no cost. The analytical observer (civilizational) sees the full entanglement: genuine transmission function requiring genuine extraction to fund it. The perspectival gaps reveal that 'the same ritual' is structurally different constraints depending on temporal position and exit capacity. A participant trapped in present time sees snare. A future survivor sees rope. Only the cross-temporal analyst sees the true mechanism (tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the extraction flow. Present-generation powerless participants are pure victims: d ≈ 0.95 (trapped, no exit, bearing full cost). Ambivalent community members are mixed: d ≈ 0.60 (constrained exit, partial benefit from community membership). Religious authorities are net beneficiaries: d ≈ 0.10 (institutional exit options, control over the mechanism). Future generations are prospective beneficiaries: d ≈ 0.05 (automatic benefit, zero cost). The analytical observer is symmetric: d ≈ 0.50 (seeing both extraction and coordination layers equally). The sigmoid f(d) transforms these into experienced effective extractiveness chi. The ritual's institutional enforcement (requires_active_enforcement: true) is necessary because participation cost is high enough that voluntary compliance would degrade without enforcement — the future-generation benefit is not salient to present-generation agents making participation decisions. The enforcement is not coercion of unwilling participants alone; it is maintenance of a mechanism that requires continued participation despite individual incentive to defect.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by accepting that the entanglement is genuine and functional. The ritual is NOT masquerading as coordination when it is actually extraction (that would be snare falsely claiming rope). The ritual is NOT masquerading as extraction when it is actually coordination (that would be rope falsely claiming snare). The ritual genuinely entangles both: it coordinates threat-recognition transmission AND asymmetrically extracts from present-generation participants to fund future-generation benefit. This is the functional definition of tangled_rope. The mandatrophy is resolved by recognizing that some coordination mechanisms necessarily entangle asymmetric extraction — the cross-generational transmission of survival competence is one such case. The present generation cannot both transmit knowledge AND avoid bearing the cost of transmission. The constraint's legitimacy does not depend on hiding the extraction; it depends on whether the coordination function justifies the extraction cost. The measurement trajectory (theater ratio stable, extractiveness rising) suggests the coordination function is holding — the ritual is not degrading into pure theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_imminence_ambiguity,
    'Is the catastrophe the ritual claims to prepare for imminent, distant, speculative, or genuinely unknowable?',
    'Historical analysis of catastrophe frequency in the population (climate event, disease, warfare, famine cycles); comparison of predicted vs. actual catastrophe timing; examination of whether ritual structures match the actual threat profile',
    'If catastrophe is imminent or frequent: ritual is genuine survival coordination (rope dominates). If distant or speculative: ritual may be maintaining anxiety-based compliance where the threat justification is weaker than the extraction mechanism (snare dominates). If genuinely unknowable: the ritual''s epistemic status is irreducible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_imminence_ambiguity, empirical, 'Whether the catastrophe threat justifying the ritual is imminent or speculative').

omega_variable(
    knowledge_transmission_necessity,
    'Is the ritualized form of threat-recognition competence transmission functionally necessary, or could the knowledge be transmitted via explicit instruction, apprenticeship, or written documentation with lower participation cost?',
    'Comparison of competence retention rates: ritualized vs non-ritualized transmission; examination of whether explicit instruction breaks down (students forget, don''t believe, don''t practice); analysis of embodied knowledge components that resist documentation',
    'If ritual is necessary: extraction cost is coordination tax, justified by functional requirement (tangled rope holds). If ritual is replaceable: the extraction cost is surplus to coordination function (snare gains strength). If hybrid (some components require ritual, others don''t): constraint should decompose into separate stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_transmission_necessity, empirical, 'Whether ritual form is functionally necessary for knowledge transmission').

omega_variable(
    sibling_reading_differentiation,
    'This constraint instantiates the survival_competence_reading of the catastrophe_memory_preservation kernel. Two sibling readings exist: mourning_practice_reading (emphasizes emotional/identity processing over explicit survival learning) and hybrid_atrophy_reading (ritual originally coordination, now mostly theater). What structural signals differentiate this reading from its siblings?',
    'Ethnographic analysis: Do ritual participants report explicit threat-recognition learning outcomes? Do they practice the encoded competencies outside ritual context? Do children show measurable threat-recognition skill acquisition from ritual participation? Are the ritual structures isomorphic with actual threat profiles, or decorative?',
    'If survival_competence_reading: high extractiveness justified by functional transmission necessity; tangled_rope dominates. If mourning_practice_reading dominates: extractiveness reflects emotional processing rather than explicit knowledge; theater_ratio would be lower. If hybrid_atrophy_reading dominates: theater_ratio >> 0.70 (piton or snare). The readings produce different classifications from the same ritual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_differentiation, empirical, 'Structural differentiation of survival_competence_reading from mourning_practice and hybrid_atrophy readings').

omega_variable(
    identity_lock_binding_mechanism,
    'Are present-generation participants locked into ritual participation by identity fusion (the ritual IS their community membership, religious identity, and social personhood), or by material barriers (economic dependency, geographic isolation, legal prohibition on exit)?',
    'Post-exit ethnography: Do participants who leave the ritual community lose their sense of identity, or do they restructure identity while maintaining community access? Do converts to the ritual show identity fusion comparable to lifelong members? Can participants articulate separation between ritual practice and personal identity, or is such separation unthinkable from within the ritual frame?',
    'If identity-locked: suppression persists after structural barriers fall (cognitive capture). If material barriers only: suppression drops when barriers are removed. If both: suppression has dual mechanism — some portion is internalized, some is structural. Identity-lock would justify classifying participant exit as identity_locked rather than constrained or trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_binding_mechanism, empirical, 'Whether participant binding is structural or identity-locked').

omega_variable(
    kernel_reading_instantiation,
    'This story instantiates ONE reading of the catastrophe_memory_preservation kernel. The kernel is the contested claim that ritual encodes and transmits collective memory of existential threats across generations. What specific structural commitments define the survival_competence_reading vs the sibling readings?',
    'The survival_competence_reading treats the kernel as: ritual functions as explicit threat-recognition training (competence-based mechanism), present-generation costs fund future-generation benefits (cross-generational extraction justified by transmission function), and the constraint is tangled_rope (genuine coordination entangled with genuine extraction). Alternative readings would treat the kernel differently: mourning_practice_reading emphasizes emotional processing and identity maintenance over explicit training (rope or piton depending on theater); hybrid_atrophy_reading emphasizes degradation (piton). The delta between readings is located in: (1) what constitutes ''transmission'' (competence vs. emotion vs. cultural muscle memory), (2) whether the extraction is functionally justified or surplus, and (3) the time horizon over which transmission occurs (immediate grief processing vs. generational knowledge vs. civilizational memory).',
    'If survival_competence_reading is correct: ritual is justified institutional enforcement (tangled_rope), extractiveness is moderate-high (0.58) and suppression is moderate-high (0.65). If mourning_practice_reading is correct: extractiveness would drop (explicit training is weaker) and theater_ratio would change (emotional processing has different output signature). If hybrid_atrophy_reading is correct: theater_ratio >> 0.70 and extractiveness drops (functionality atrophied). The classification framework correctly produces different outputs for the same ritual depending on which reading instantiates the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Structural definition of survival_competence_reading as one instantiation of catastrophe_memory_preservation kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(survival_competence_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(surv_tr_t0, survival_competence_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(surv_tr_t3, survival_competence_reading, theater_ratio, 3, 0.42).
narrative_ontology:measurement(surv_tr_t6, survival_competence_reading, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(surv_be_t0, survival_competence_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(surv_be_t3, survival_competence_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(surv_be_t6, survival_competence_reading, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(survival_competence_reading, attachment_coordination).
narrative_ontology:affects_constraint(survival_competence_reading, mourning_practice_reading).
narrative_ontology:affects_constraint(survival_competence_reading, hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_preservation kernel decomposes into three constraint stories with different structural properties and classifications. The survival_competence_reading treats ritual as explicit knowledge transmission (tangled_rope, extractiveness 0.58). Sibling readings instantiate the same kernel with different functional emphases and produce different classifications. The three stories are linked via network.affects_constraints as alternative readings of a single kernel, not as causal dependencies. Each reading is self-contained; they are related by interpretive alternatives, not by constraint hierarchy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
