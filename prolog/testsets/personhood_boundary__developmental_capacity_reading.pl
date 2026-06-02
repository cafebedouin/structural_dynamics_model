% ============================================================================
% CONSTRAINT STORY: personhood_boundary__developmental_capacity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__developmental_capacity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: personhood_boundary__developmental_capacity_reading
 *   human_readable: Personhood Boundary: Developmental Capacity Reading
 *   domain: moral_philosophy/developmental_ethics
 *
 * SUMMARY:
 *   The developmental capacity reading assigns moral weight gradually as
 *   neural capacity emerges, denying full personhood status at conception and
 *   early gestation but acknowledging that moral significance increases with
 *   neurological integration and capacity for consciousness and suffering.
 *   This reading is one interpretation of a deeply contested kernel: the
 *   personhood boundary itself. The kernel is what capacities ground
 *   personhood; the reading specifies that neural capacity, emerging
 *   developmentally, is the determinant. This reading coexists with
 *   birth-threshold reading (personhood begins at birth regardless of prior
 *   capacity) and the spartan-eugenic reading (personhood tracks capacity,
 *   potentially excluding the disabled and cognitively impaired). The
 *   developmental capacity reading occupies a middle ground: it avoids the
 *   arbitrariness of a single threshold (birth) while avoiding the eugenic
 *   implications of strict capacity-dependency for already-born persons.
 *   However, the reading itself instantiates a Tangled Rope constraint: it
 *   provides genuine coordination benefits (enables nuanced ethical
 *   reasoning, bridges philosophical camps, supports medical practice under
 *   uncertainty) while extracting from those without sufficient capacity and
 *   from reproductive-age persons whose autonomy is progressively restricted
 *   as gestation advances.
 *
 * KEY AGENTS:
 *   - Early Gestational Entities: Primary victim (powerless/trapped) — assigned minimal or zero moral weight during formative stages; no agency or representation
 *   - Profoundly Cognitively Disabled Persons: Secondary victim (moderate/constrained) — moral status contingent on capacity level; may be permanently below the threshold of full personhood under strict reading
 *   - Reproductive-Age Women and Pregnant Persons: Organized victim (organized/identity_locked) — face progressive legal restrictions as fetal capacity develops; identity as pregnant persons is constituted through the constraint
 *   - Developmental Capacity Theorists: Primary beneficiary (institutional/arbitrage) — control the authoritative interpretation; their disciplinary standing is enhanced by the reading's nuance and apparent scientific grounding
 *   - Medical Practice Institutions: Secondary beneficiary (institutional/arbitrage) — maintain constant institutional application of the reading; capacity metrics provide appearance of precision masking underlying indeterminacy
 *   - Liberal Democratic Legal Systems: Organized beneficiary (organized/constrained) — the reading enables compromise policy that satisfies multiple constituencies; constrained because empirical uncertainty about consciousness forces ongoing review cycles
 *   - Analytical Observer: Neutral position (analytical/analytical) — risks naturalizing a normative reading choice as biological law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__developmental_capacity_reading, 0.58).
domain_priors:suppression_score(personhood_boundary__developmental_capacity_reading, 0.72).
domain_priors:theater_ratio(personhood_boundary__developmental_capacity_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__developmental_capacity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(personhood_boundary__developmental_capacity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(personhood_boundary__developmental_capacity_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__developmental_capacity_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__developmental_capacity_reading, "Personhood Boundary: Developmental Capacity Reading").
narrative_ontology:topic_domain(personhood_boundary__developmental_capacity_reading, "moral_philosophy/developmental_ethics").

domain_priors:requires_active_enforcement(personhood_boundary__developmental_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__developmental_capacity_reading, '164aefe4-c1af-43fe-9549-2daadfb710ad').
narrative_ontology:cs_kernel_codification('164aefe4-c1af-43fe-9549-2daadfb710ad', fixed_text).
narrative_ontology:cs_authority_grounding('164aefe4-c1af-43fe-9549-2daadfb710ad', lineage).
narrative_ontology:cs_interpretation_layer_present('164aefe4-c1af-43fe-9549-2daadfb710ad').
narrative_ontology:cs_reading_relation('164aefe4-c1af-43fe-9549-2daadfb710ad', personhood_boundary__birth_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('164aefe4-c1af-43fe-9549-2daadfb710ad', personhood_boundary__spartan_eugenic_reading, influences).
narrative_ontology:cs_axiom('164aefe4-c1af-43fe-9549-2daadfb710ad', foundational, neural_capacity_morally_determinative).
narrative_ontology:cs_axiom_status(neural_capacity_morally_determinative, holdable).
narrative_ontology:cs_axiom_grounding('164aefe4-c1af-43fe-9549-2daadfb710ad', neural_capacity_morally_determinative, empirically_contingent).
narrative_ontology:cs_axiom('164aefe4-c1af-43fe-9549-2daadfb710ad', foundational, capacity_necessity_for_full_status).
narrative_ontology:cs_axiom_status(capacity_necessity_for_full_status, holdable).
narrative_ontology:cs_axiom_grounding('164aefe4-c1af-43fe-9549-2daadfb710ad', capacity_necessity_for_full_status, deontological).
narrative_ontology:cs_axiom('164aefe4-c1af-43fe-9549-2daadfb710ad', secondary, disabled_born_persons_retain_full_status).
narrative_ontology:cs_axiom_status(disabled_born_persons_retain_full_status, holdable).
narrative_ontology:cs_axiom_grounding('164aefe4-c1af-43fe-9549-2daadfb710ad', disabled_born_persons_retain_full_status, deontological).
narrative_ontology:cs_reference_frame('164aefe4-c1af-43fe-9549-2daadfb710ad', developmental_neural_integration).
narrative_ontology:cs_drift_state('164aefe4-c1af-43fe-9549-2daadfb710ad', contemporary_neuroscience_limit, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('164aefe4-c1af-43fe-9549-2daadfb710ad', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(personhood_boundary__developmental_capacity_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__developmental_capacity_reading, developmental_stage_gradualists).
narrative_ontology:constraint_beneficiary(personhood_boundary__developmental_capacity_reading, capacity_theorists).
narrative_ontology:constraint_beneficiary(personhood_boundary__developmental_capacity_reading, medical_ethicists).
narrative_ontology:constraint_victim(personhood_boundary__developmental_capacity_reading, early_gestational_entities).
narrative_ontology:constraint_victim(personhood_boundary__developmental_capacity_reading, anencephalic_infants).
narrative_ontology:constraint_victim(personhood_boundary__developmental_capacity_reading, profoundly_cognitively_disabled_beings).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY GESTATIONAL ENTITY (SNARE) — No agency, no advocacy, no exit. The developmental capacity reading assigns minimal moral weight during early gestation; whatever moral status accrues does so gradually and invisibly. The entity bears full suppressive cost of legal non-personhood during formative stages.
constraint_indexing:constraint_classification(personhood_boundary__developmental_capacity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: PROFOUNDLY COGNITIVELY DISABLED PERSONS (TANGLED ROPE) — Their moral status under this reading depends on capacity levels that may never be reached. They benefit from inclusive legal personhood but also experience extraction: the reading's logic subjects their continued moral inclusion to contingent capacity assessments. Constrained exit: they cannot demonstrate capacity if the framework requires capacities beyond their reach.
constraint_indexing:constraint_classification(personhood_boundary__developmental_capacity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEVELOPMENTAL CAPACITY THEORISTS (ROPE) — Institutional authority (bioethics committees, philosophy departments, medical regulatory bodies) benefits from the development-based framework: it provides nuanced middle-ground reasoning that satisfies multiple constituencies and enables complex policy coordination without outright prohibition or absolute protection. Arbitrage: they can switch frameworks (birth threshold, biological humanity) without losing disciplinary standing.
constraint_indexing:constraint_classification(personhood_boundary__developmental_capacity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REPRODUCTIVE-AGE WOMEN AND PREGNANT PERSONS (SNARE) — Organized agents with restricted exit options specific to pregnancy. They face the extraction mechanism of ambiguous personhood assignment: the developmental capacity reading preserves their bodily autonomy in early pregnancy but subjects them to increasing legal restrictions as capacity develops, with no clear threshold. Identity-locked: their identity as pregnant persons is constituted through the constraint; exit requires abandoning pregnancy itself or accepting legal non-recognition of that identity.
constraint_indexing:constraint_classification(personhood_boundary__developmental_capacity_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 5: MEDICAL PRACTICE AND RESEARCH INSTITUTIONS (PITON) — The institutional apparatus (hospital ethics boards, neonatal intensive care guidelines, fetal surgery protocols) maintains the developmental capacity framework through constant practical application and refinement. Theater ratio is high because the boundaries are constantly re-negotiated through case-by-case decisions rather than settled law; the constraint persists through institutional habit and the appearance of precision (capacity metrics, gestational age cutoffs) that mask underlying indeterminacy.
constraint_indexing:constraint_classification(personhood_boundary__developmental_capacity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LIBERAL DEMOCRATIC LEGAL SYSTEMS (SCAFFOLD) — The developmental capacity reading is a pragmatic compromise structure with a built-in sunset: as neuroscientific understanding of fetal and neonatal consciousness improves, the capacity thresholds can be revised upward or downward. The constraint functions as temporary scaffolding during uncertainty about when meaningful neural integration (consciousness, self-awareness, capacity for suffering) actually emerges. The sunset is enforced by ongoing empirical research and policy review cycles.
constraint_indexing:constraint_classification(personhood_boundary__developmental_capacity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the developmental capacity reading reflects an immutable biological fact: personhood-relevant capacities (neural integration, consciousness, pain awareness) genuinely do emerge gradually during development. This perspective sees the constraint as a natural law of biology, not a constructed institutional arrangement. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the 'natural biology' framing naturalizes what is actually a normative reading choice.
constraint_indexing:constraint_classification(personhood_boundary__developmental_capacity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__developmental_capacity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(personhood_boundary__developmental_capacity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(personhood_boundary__developmental_capacity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__developmental_capacity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(personhood_boundary__developmental_capacity_reading, TR),
    TR >= 0.70.

:- end_tests(personhood_boundary__developmental_capacity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading assigns graduated moral weight, creating an extraction mechanism that concentrates maximal suppression on early gestational entities (which have no advocacy or agency) while progressively increasing protections as capacity develops. The extraction is not maximal (0.8+) because the reading preserves significant autonomy for pregnant persons in early pregnancy and because it avoids the eugenic implications of strict capacity-dependency for born persons. Theater ratio (0.68): High. The developmental capacity reading claims precision ('sliding scale,' 'neural integration,' 'consciousness thresholds') but these terms mask deep indeterminacy: consciousness emergence is empirically uncertain, capacity metrics are applied inconsistently across medical and legal contexts, and the 'scale' itself is unevenly operationalized (many jurisdictions use arbitrary gestational-age cutoffs rather than capacity-based thresholds). The theater has increased over time as neuroscience has failed to produce clean, consensus thresholds while the philosophical framework continues to assert scientific rigor. Suppression (0.72): High. Multiple suppressive mechanisms: (1) early gestational entities have zero legal standing; (2) reproductive-age persons face progressive restriction as pregnancy advances, with no clear threshold for when fetal rights override maternal autonomy; (3) the capacity framework produces suppression of disabled persons' full moral status; (4) medical institutions suppress alternative readings (birth-threshold, biological-humanity readings) by treating the developmental reading as scientifically justified when it is actually normatively chosen.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates deep perspectival disagreement rooted in the observer's structural position. The early gestational entity and reproductive-age pregnant persons see an extraction mechanism (snare and snare-to-tangled-rope experience). The capacity theorists and medical institutions see coordination benefits and practical necessity (rope and piton). The disabled persons see contingency threatening their status (tangled rope with identity risk). The legal systems see a pragmatic compromise requiring constant adjustment (scaffold). The analytical observer risks naturalizing the reading as biological law (false summit mountain). The core gap: the reading's claim to be grounded in objective neural facts (objective capacity emergence) masks a normative choice about which capacities ground personhood. Different reading of the same kernel would assign different agents to beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   The developmental capacity reading creates directionality divergence based on the agent's structural position relative to capacity-based determination. Early gestational entities have d ≈ 1.0 (pure target): they bear extraction with no mitigation. Reproductive-age persons have d ≈ 0.75 (primary target with some identity-based binding): they face progressive restriction but retain agency in early pregnancy. Cognitively disabled persons have d ≈ 0.65 (moderate target with identity risk): their moral status is contingent but they retain bodily personhood. Capacity theorists have d ≈ 0.15 (beneficiary with arbitrage): they benefit from authoritative interpretation and can exit by adopting alternative frameworks without loss of standing. Medical institutions have d ≈ 0.20 (beneficiary with arbitrage): they benefit from the reading's constant application and can switch frameworks if institutional pressures demand. The analytical observer has d ≈ 0.72 (analytical target): the observer's capacity to see the normative choice is obscured by the reading's scientific framing (oracle gap).
 *
 * MANDATROPHY ANALYSIS:
 *   READING-SPECIFIC MANDATROPHY: The developmental capacity reading resolves the mandatrophy by showing that all six types are legitimate perspectival readings of the same kernel interpretation. The constraint is a Tangled Rope (coordination + extraction hybrid) at the imposed-reading level because it genuinely does coordinate medical and legal practice while asymmetrically extracting from early entities and reproductive-age persons. No single type captures all perspectives. The mandatrophy resolution hinges on recognizing that the reading ITSELF is a normative choice (one of three sibling readings of the personhood kernel), not a discovery of natural fact. The analytical observer's mountain is a false summit: the 'natural biology' framing naturalizes what is actually a reading choice. The mandatrophy is resolved by making this choice explicit and acknowledging the sibling readings as live alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consciousness_emergence_threshold,
    'At what gestational or developmental stage does morally-relevant consciousness actually emerge?',
    'Neuroscientific evidence on thalamocortical connectivity, integrated information theory measurements, responsiveness to noxious stimuli, neural circuitry maturation. Current evidence suggests minimal integrated consciousness before 24-28 weeks but significant uncertainty remains.',
    'If consciousness emerges at 12 weeks: early gestation entities acquire much higher moral status under this reading, pushing it toward snare-dominant classification. If consciousness emerges at birth or later: the reading''s victim set expands to include early-term neonates, increasing mandatrophy pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consciousness_emergence_threshold, empirical, 'Empirical threshold for morally-relevant consciousness emergence').

omega_variable(
    capacity_necessity_vs_sufficiency,
    'Is neural capacity NECESSARY for moral status, or merely SUFFICIENT? Can other grounds (genetic humanity, relational status, potential) confer moral weight independent of current capacity?',
    'Logical analysis of the reading''s foundational axiom. If capacity is necessary: entities without capacity (anencephalic infants, permanently comatose adults) are not moral patients. If sufficient but not necessary: capacity triggers special protections but other grounds may exist.',
    'If necessary: reading forecloses birth-threshold reading and modern biological-humanity reading (contradicts their core premises). If sufficient: reading coexists with biological-humanity reading (different but compatible grounds for status).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_necessity_vs_sufficiency, conceptual, 'Whether capacity is necessary or merely sufficient for moral status').

omega_variable(
    disability_and_capacity_metrics,
    'How does this reading apply to congenital disabilities that impair but do not eliminate capacity (Down syndrome, autism spectrum, deaf-blindness)? Are they permanently assigned lower moral weight, or does the reading''s logic produce diminished capacity-adjusted moral status?',
    'Analysis of disability advocacy literature and ethical frameworks that integrate capacity-based reasoning with disability justice principles. Empirical tracking of legal outcomes for disabled persons under capacity-based personhood frameworks.',
    'If diminished capacity yields diminished moral weight: reading contradicts disability equality principles and is subject to strong normative challenge. If reading produces mechanism for equal moral status despite capacity variation: reading is compatible with disability justice (coexists with it).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disability_and_capacity_metrics, conceptual, 'Application of capacity metrics to congenital disabilities').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression (0.72) experienced by reproductive-age persons structural (legal barriers to abortion access, enforced reporting of pregnancy) or partially internalized (shame, reproductive coercion through identity fusion with maternal role)?',
    'Post-legal-change tracking: do suppression effects persist after legal restrictions are removed? Survey data on experienced reproductive autonomy vs. internalized constraints.',
    'If structural: exit requires policy reform. If internalized: suppression persists even after legal barriers fall; the identity_locked classification is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether reproductive suppression is structural or internalized').

omega_variable(
    sliding_scale_operationalization,
    'How does the ''sliding scale'' of moral weight translate into operational policy and law? At what point do protections trigger? How is marginal moral weight calculated and applied in real decisions (abortion, neonatal care, research ethics)?',
    'Comparative legal analysis: which jurisdictions operationalize developmental capacity reading, and what specific thresholds do they use (viability, 12 weeks, quickening, birth)? Are the thresholds scientifically justified or pragmatically chosen?',
    'If thresholds are scientifically justified: the constraint approximates the reading''s logic and mandatrophy is manageable. If pragmatic: the constraint is a politely-dressed snare with theater (appearances of precision masking arbitrary boundaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sliding_scale_operationalization, empirical, 'Operationalization of sliding-scale moral weight in law and policy').

omega_variable(
    kernel_alternative_reading_pressure,
    'What structural conditions would make the birth-threshold reading or the spartan-eugenic reading more attractive than the developmental capacity reading?',
    'Historical analysis of when each reading gains force (e.g., birth reading strengthens during high-maternal-mortality eras when pregnancy itself is high-risk; eugenic reading strengthens during resource scarcity). Identify conditions that shift between readings.',
    'Determines the drift state of this reading over time: is it gaining institutional ground (revival_pressure), losing ground (repudiation_pressure), or stable (stable)? Feeds into reference_frame vs. drift_state analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_alternative_reading_pressure, conceptual, 'Historical conditions driving shift between personhood readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__developmental_capacity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(persbnd_theater_t0, personhood_boundary__developmental_capacity_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(persbnd_theater_t50, personhood_boundary__developmental_capacity_reading, theater_ratio, 50, 0.68).
narrative_ontology:measurement(persbnd_theater_t100, personhood_boundary__developmental_capacity_reading, theater_ratio, 100, 0.71).

% Extraction over time
narrative_ontology:measurement(persbnd_extractiveness_t0, personhood_boundary__developmental_capacity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(persbnd_extractiveness_t50, personhood_boundary__developmental_capacity_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement(persbnd_extractiveness_t100, personhood_boundary__developmental_capacity_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(persbnd_suppression_t0, personhood_boundary__developmental_capacity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(persbnd_suppression_t50, personhood_boundary__developmental_capacity_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(persbnd_suppression_t100, personhood_boundary__developmental_capacity_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__developmental_capacity_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__developmental_capacity_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__developmental_capacity_reading, personhood_boundary__spartan_eugenic_reading).
narrative_ontology:affects_constraint(personhood_boundary__developmental_capacity_reading, abortion_access_restriction).
narrative_ontology:affects_constraint(personhood_boundary__developmental_capacity_reading, fetal_surgery_ethics).
narrative_ontology:affects_constraint(personhood_boundary__developmental_capacity_reading, neonatal_intensive_care_standards).

% DUAL FORMULATION NOTE:
% The personhood boundary kernel has three structurally distinct readings (developmental_capacity, birth_threshold, spartan_eugenic) with different ε values and beneficiary/victim sets. This constraint story covers only the developmental_capacity_reading. The other readings are separate constraint stories. All three are linked via network.affects_constraints to show kinship and causal interdependence: the developmental reading influences institutional pressure toward protecting born disabled persons (affects the eugenic reading), and its empirical failures (inability to identify clean consciousness thresholds) strengthen arguments for the birth reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__developmental_capacity_reading, powerless, 1.0).
constraint_indexing:directionality_override(personhood_boundary__developmental_capacity_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
