% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoiousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoiousios_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nicene_christological_kernel__homoiousios_reading
 *   human_readable: Homoiousios Christological Reading: Christ of Similar Substance with the Father
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   The homoiousios reading instantiates one theological position within the
 *   contested Nicene Christological kernel: Christ is of SIMILAR (but
 *   ontologically distinguishable) substance with the Father, rather than
 *   IDENTICAL substance (homoousios). This reading emerges from
 *   fourth-century bishops and theologians (notably the Homoiousian caucus at
 *   Constantinople 381) who sought to preserve both Christ's divinity and a
 *   real ontological distinction to maintain monotheistic clarity against the
 *   appearance of ditheism. The reading benefits regional episcopal authority
 *   and exegetical pluralism; it extracts from imperial uniformity and
 *   institutional ecclesiastical cohesion. The claim is tangled_rope: genuine
 *   coordination function (enabling theological pluralism) yoked with
 *   asymmetric extraction (fragmentation as the price of pluralism). The
 *   metric drift shows extractiveness rising from 0.38 to 0.52 as councils
 *   multiply and the pluralism enabled by homoiousios generates institutional
 *   friction.
 *
 * KEY AGENTS:
 *   - Regional episcopal authorities: benefit from interpretive autonomy; constrained by fragmentation
 *   - Exegetical pluralism advocates: benefit from legitimated theological diversity; organized, mobile exit
 *   - Imperial religious uniformity project: targeted by fragmentation; trapped, must renegotiate compliance repeatedly
 *   - Ecclesiastical institutional cohesion: victim of homoiousios's enabling of schism; institutional, trapped
 *   - Homoousios defenders: excluded from this reading's terms; their formula is treated as one option, not mandatory
 *   - Nicene council authority: agenda-setter; interprets doctrine but loses mandate-enforcing power under homoiousios
 *   - Patristic scholarly tradition: observer; sees homoiousios as preserving theological richness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, 0.52).
domain_priors:suppression_score(nicene_christological_kernel__homoiousios_reading, 0.61).
domain_priors:theater_ratio(nicene_christological_kernel__homoiousios_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoiousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoiousios_reading, "Homoiousios Christological Reading: Christ of Similar Substance with the Father").
narrative_ontology:topic_domain(nicene_christological_kernel__homoiousios_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoiousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoiousios_reading, 'aab0ef30-403a-44d6-9fa2-e10a9381e9db').
narrative_ontology:cs_kernel_codification('aab0ef30-403a-44d6-9fa2-e10a9381e9db', formalized).
narrative_ontology:cs_authority_grounding('aab0ef30-403a-44d6-9fa2-e10a9381e9db', lineage).
narrative_ontology:cs_interpretation_layer_present('aab0ef30-403a-44d6-9fa2-e10a9381e9db').
narrative_ontology:cs_reading_relation('aab0ef30-403a-44d6-9fa2-e10a9381e9db', nicene_christological_kernel__homoousios_reading, coexists_with).
narrative_ontology:cs_axiom('aab0ef30-403a-44d6-9fa2-e10a9381e9db', foundational, ontological_distinction_preserves_monotheism).
narrative_ontology:cs_axiom_status(ontological_distinction_preserves_monotheism, holdable).
narrative_ontology:cs_axiom_grounding('aab0ef30-403a-44d6-9fa2-e10a9381e9db', ontological_distinction_preserves_monotheism, deontological).
narrative_ontology:cs_axiom('aab0ef30-403a-44d6-9fa2-e10a9381e9db', secondary, exegetical_pluralism_legitimate_under_single_formula).
narrative_ontology:cs_axiom_status(exegetical_pluralism_legitimate_under_single_formula, holdable).
narrative_ontology:cs_axiom_grounding('aab0ef30-403a-44d6-9fa2-e10a9381e9db', exegetical_pluralism_legitimate_under_single_formula, conventional).
narrative_ontology:cs_reference_frame('aab0ef30-403a-44d6-9fa2-e10a9381e9db', nicene_monotheistic_christology_with_distinction).
narrative_ontology:cs_drift_state('aab0ef30-403a-44d6-9fa2-e10a9381e9db', post_council_fragmentation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('aab0ef30-403a-44d6-9fa2-e10a9381e9db', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, regional_episcopal_authorities).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, exegetical_pluralism_advocates).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, imperial_religious_uniformity_project).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, ecclesiastical_institutional_cohesion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, regional_episcopal_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Regional bishops maintain interpretive autonomy under homoiousios: they can argue for ontological distinction without being branded heretical by homoousios uniformity. They extract legitimacy and theological voice from the formula's flexibility. They also bear the cost of perpetual theological contention and fragmented imperial religious authority, weakening their collective bargaining power against emperors.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, regional_episcopal_authorities, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoiousios_reading, regional_episcopal_authorities, payer).

% Theologians and scholar-bishops who defend interpretive diversity against single-formula uniformity. They benefit from homoiousios because it legitimates exegetical disagreement as theologically sound, preserving space for philosophical schools (Platonist, Aristotelian, Logos theology) to coexist. Their exit is relatively open: they can withdraw to regional churches or textual scholarship if suppressed.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, exegetical_pluralism_advocates, beneficiary,
    organized, generational, mobile, continental).

% The imperial project to enforce a single Christological formula empire-wide as a tool of political integration. Under homoiousios, uniformity fractures: regional churches maintain divergent readings, theological councils splinter into factions, and the emperor must re-negotiate religious compliance repeatedly. The universalizing project bears the cost of perpetual fragmentation.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, imperial_religious_uniformity_project, payer,
    institutional, generational, trapped, universal).

% The institutional church's capacity to operate as a unified hierarchical body grounded in doctrinal unity. Homoiousios enables theological splits that fragment the church vertically (regional churches adopt divergent readings) and horizontally (bishops' councils remain in contention). The constraint extracts from unified institutional capacity by legitimating pluralism.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, ecclesiastical_institutional_cohesion, payer,
    institutional, generational, trapped, universal).

% Theologians and church leaders who hold that Christ is homoousios (fully identical substance) with the Father. Under the homoiousios reading, their formula is treated as only one defensible option, not the mandatory interpretation. They would argue for full doctrinal uniformity and exclusive homoousios authority; they are structurally excluded from the conversation under this reading's terms.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, homoousios_defenders, excluded,
    institutional, generational, trapped, continental).

% The Council of Nicaea and its successors as the institutional seat that interprets and enforces Christological doctrine. Under homoiousios, the council operates as a hermeneutical authority that permits multiple readings within orthodoxy rather than as a uniformity-enforcer. The reading grants the council interpretive flexibility but strips it of mandate-enforcing power.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, nicene_council_authority, agenda_setter,
    institutional, generational, arbitrage, universal).

% The living tradition of patristic exegesis and philosophical theology spanning multiple schools and centuries. From this seat, homoiousios appears as a historically defensible reading that preserves genuine theological richness and avoids the totalizing closure of uniform doctrine.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, patristic_scholarly_tradition, observer,
    organized, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_christological_kernel__homoiousios_reading, regional_episcopal_authorities).
narrative_ontology:fixing_cost_class(nicene_christological_kernel__homoiousios_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes a Christological framework that permits regional theological schools and interpretive traditions to coexist within a single church structure. Solves the coordination problem: how can the church be 'one' when Christian communities interpret the Incarnation through different philosophical and exegetical lenses (Platonism, Logos theology, Aristotelian substance)?
% TRANSFER_FUNCTION: Transfers ecclesiastical authority from uniform imperial doctrine to regional episcopal bodies and exegetical traditions. It moves the power to define orthodoxy from a single universal formula toward a plurality of legitimate readings, each grounded in defensible scriptural and philosophical interpretation.
% ABSENT_VOICES: Homoousios defenders (who would argue for full identity and unique authority of their formula) and ordinary believers seeking doctrinal certitude rather than theological pluralism are structurally excluded from this reading's terms. The reading legitimates complexity over simplicity; the voices it excludes are those that demand formulaic clarity.
% DISAPPEARANCE_RATIONALE: If homoiousios vanished and churches reverted to either pure pluralism (no shared framework) or mandatory homoousios uniformity, the theological landscape would reorganize entirely. Regional churches would either formally schism (under uniformity) or lose all doctrinal coherence (under no framework). The fourth-century compromise that homoiousios represents would dissolve.
% FOUNDING_PROBLEM: The Arian controversy: how to affirm Christ's divinity and pre-eternity while preserving monotheistic clarity and avoiding the appearance that Christianity teaches two gods. Nicaea convened to resolve the scandal that Christian communities held irreconcilably different Christologies, destabilizing both theology and imperial religious integration.
% FOUNDING_PROBLEM_CORROBORATION: The problem is corroborated by fourth-century council records, patristic letters, and the historical record of multiple councils (Nicaea 325, Constantinople 381, Ephesus 431) each attempting to settle the same dispute. The founding problem persists because homoiousios itself neither fully resolves the Arian controversy nor achieves the doctrinal uniformity homoousios proponents sought; the tension remains live in the historical record.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoiousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoiousios_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoiousios_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoiousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoiousios_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoiousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoiousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.52 reflects moderate asymmetry: homoiousios genuinely enables theological pluralism (coordination function) but does so by legitimating disagreement that fragments imperial religious uniformity and institutional coherence. The reading extracts from the universalizing project by making fragmentation theologically defensible. Suppression rises from 0.45 to 0.61 between Nicaea and Constantinople because maintaining homoiousios against both Arian pressure and homoousios uniformity-seekers requires active enforcement at multiple councils. Theater rises to 0.41 because councils increasingly defend the formula performatively (Christological decrees reaffirmed without new theological content) rather than solving the underlying doctrinal dispute. The shared time grid aligns all metrics at each council date (325, 355, 381, 410, 431, 451), tracking the constraint's operation across four major ecclesiastical events. Accessibility collapse is moderate (0.48) because homoiousios remains contested—alternative readings are not shut off; resistance is high (0.68) because homoousios proponents and Arians both mount serious challenges to the reading.
 *
 * PERSPECTIVAL GAP:
 *   From the imperial and institutional cohesion seats, homoiousios appears as destructive fragmentation—a reading that leverages theological complexity to undermine religious uniformity. From the regional episcopal and exegetical seats, it appears as legitimate theological sophistication that preserves genuine doctrinal coherence while avoiding totalizing closure. From the council authority seat, homoiousios grants hermeneutical flexibility but denies mandate power: the council interprets but cannot enforce a single reading. The engine computes these seat-specific classifications from power/exit/directionality; the perspectival gap is fundamental to the constraint's structure, not a mere difference of opinion.
 *
 * DIRECTIONALITY LOGIC:
 *   Regional episcopal authorities are structural beneficiaries (d near 0.1–0.2): they gain interpretive voice and theological leverage from homoiousios's pluralism, though they also pay the cost of fragmentation. Imperial uniformity and institutional cohesion are structural targets (d near 0.8–0.9): the reading directly extracts from their capacity by legitimating theological divergence. The council authority is the agenda-setter (d near 0.5): it interprets and enforces doctrine but the reading strips it of mandate power, making it a mediator rather than a uniformity-enforcer. Exegetical pluralism advocates benefit most clearly (d near 0.05), with mobile exit options keeping them from identity-lock. Homoousios defenders are excluded (structurally outside this reading's conversation), not merely disagreeing—their position is treated as optional rather than mandatory, which is an extraction from their authority.
 *
 * MANDATROPHY ANALYSIS:
 *   Homoiousios avoids mandatrophy in the interval (325–451) because the founding problem—unifying Christian theology around a single Christological formula—remains genuinely live and contested. Regional bishops, exegetical schools, Arians, and homoousios proponents all mount active resistance (measured at 0.68, high). The constraint is enforced because the empire and institutional church still believe doctrinal settlement is achievable. The theater ratio's rise from 0.22 to 0.41 suggests performative maintenance beginning to creep in (councils reaffirming formulas without resolution), but theater remains below 0.5, indicating genuine functional enforcement still dominates. Mandatrophy would emerge if the councils continued defending homoiousios while all parties openly acknowledged the formula solves nothing—the measured resistance of 0.68 shows that stage has not arrived in this interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    homoiousios_vs_homoousios_foreclosure,
    'Do homoiousios and homoousios readings logically foreclose each other (neither can be held simultaneously in any coherent framework), or do they coexist as genuinely different but non-contradictory theological options?',
    'Systematic analysis of the logical structure of each reading''s core axioms: if the axioms are contradictory, foreclosure holds; if they are merely different and each internally coherent, coexistence holds. Historical evidence from councils and patristic writers showing whether parties held they were choosing between forecloses or coexisting options.',
    'If foreclosure holds, the readings form a Hegelian binary where adoption of one eliminates the other—the constraint produces a victory condition. If coexistence holds, fragmentation is structural, not eliminable by choosing the right formula. Coexistence suggests homoiousios is a Snare or Tangled Rope (extraction from pluralism), not a Rope (pure coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(homoiousios_vs_homoousios_foreclosure, conceptual, 'Whether homoiousios and homoousios are logically incompatible or structurally different but compatible readings.').

omega_variable(
    uniformity_extraction_mechanism,
    'Is the measured extractiveness of homoiousios (0.52) the cost of pluralism itself, or the cost of a partial uniformity that pretends to be pluralism?',
    'Examine historical council records and imperial decrees: if emperors enforced homoiousios as a mandatory formula despite its pluralist framing, the extraction is from false pluralism (the formula claims autonomy while demanding compliance). If regional churches genuinely operated with multiple readings under the same formula, pluralism was real and extraction comes from fragmentation''s burden on institutional operations.',
    'If false pluralism, homoiousios is a Snare (coordination framing covering coercive uniformity). If real pluralism, it is a Tangled Rope (genuine coordination yoked with fragmentation costs). This impacts the nature of the victim: institutional burden (real pluralism) vs. theological subordination (false pluralism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniformity_extraction_mechanism, empirical, 'Whether homoiousios''s pluralism was genuine or performative.').

omega_variable(
    suppression_source_ambiguity,
    'Is the rising suppression (from 0.45 to 0.61) primarily structural suppression (imperial and ecclesiastical enforcement machinery hardening to police the boundary between homoiousios pluralism and heresy), or internalized suppression (theologians self-censoring to avoid the appearance of doctrinal contention)?',
    'Compare suppression patterns across regions: if suppression is structural, high-power regions with imperial military presence should show suppression more than peripheral regions. If suppression is internalized, dispersed regions should show equal self-censoring (fear of ecumenical judgment), indicating the mechanism is doctrinal authority, not force.',
    'If structural, the measured suppression understates the effective extraction (targets can carry suppression with them even after exit from the constraint''s jurisdiction). If internalized, the suppression is fragile and dissolves when theological authority is challenged. Affects the terminal classification: structural suppression in Snares persists; internalized suppression in identity-locked Tangled Ropes can fracture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_source_ambiguity, empirical, 'Whether measured suppression is structural enforcement or internalized self-censoring.').

omega_variable(
    kernel_vs_reading_distinction,
    'Is the homoiousios reading a genuine alternative instantiation of the Nicene kernel, or a systematic misreading that violates the kernel''s core commitment to monotheistic clarity?',
    'Textual analysis of the Nicene Creed itself (325): does the text commit to homoousios specifically, or to a more general monotheistic Christology that both homoiousios and homoousios readings can claim to satisfy? If the creed itself is ambiguous, homoiousios is a defensible reading. If it clearly intends homoousios (as homoousios defenders claim), homoiousios is a reading that reinterprets the kernel to its own ends.',
    'If homoiousios is a reading of the kernel, this constraint is correctly authored under the committer frame (kernel_id + reading_id). If homoiousios is a misreading, it is not a reading at all but a competitor constraint claiming kinship falsely. Affects the network relationship: are the two constraints siblings (coexisting readings of one kernel) or competitors (two kernels each claiming Nicaea).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_vs_reading_distinction, conceptual, 'Whether homoiousios is a legitimate reading of the Nicene kernel or a systematic reinterpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoiousios_reading, 325, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_christological_kernel__homoiousios_reading, theater_ratio, 325, 0.22).
narrative_ontology:measurement_basis(nice_tr_t325, observed).
narrative_ontology:measurement(nice_tr_t355, nicene_christological_kernel__homoiousios_reading, theater_ratio, 355, 0.32).
narrative_ontology:measurement_basis(nice_tr_t355, observed).
narrative_ontology:measurement(nice_tr_t381, nicene_christological_kernel__homoiousios_reading, theater_ratio, 381, 0.42).
narrative_ontology:measurement_basis(nice_tr_t381, observed).
narrative_ontology:measurement(nice_tr_t410, nicene_christological_kernel__homoiousios_reading, theater_ratio, 410, 0.44).
narrative_ontology:measurement_basis(nice_tr_t410, observed).
narrative_ontology:measurement(nice_tr_t431, nicene_christological_kernel__homoiousios_reading, theater_ratio, 431, 0.41).
narrative_ontology:measurement_basis(nice_tr_t431, observed).
narrative_ontology:measurement(nice_tr_t451, nicene_christological_kernel__homoiousios_reading, theater_ratio, 451, 0.41).
narrative_ontology:measurement_basis(nice_tr_t451, observed).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 325, 0.38).
narrative_ontology:measurement_basis(nice_be_t325, observed).
narrative_ontology:measurement(nice_be_t355, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 355, 0.48).
narrative_ontology:measurement_basis(nice_be_t355, observed).
narrative_ontology:measurement(nice_be_t381, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 381, 0.55).
narrative_ontology:measurement_basis(nice_be_t381, observed).
narrative_ontology:measurement(nice_be_t410, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 410, 0.54).
narrative_ontology:measurement_basis(nice_be_t410, observed).
narrative_ontology:measurement(nice_be_t431, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 431, 0.52).
narrative_ontology:measurement_basis(nice_be_t431, observed).
narrative_ontology:measurement(nice_be_t451, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 451, 0.52).
narrative_ontology:measurement_basis(nice_be_t451, observed).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 325, 0.45).
narrative_ontology:measurement_basis(nice_su_t325, observed).
narrative_ontology:measurement(nice_su_t355, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 355, 0.58).
narrative_ontology:measurement_basis(nice_su_t355, observed).
narrative_ontology:measurement(nice_su_t381, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 381, 0.65).
narrative_ontology:measurement_basis(nice_su_t381, observed).
narrative_ontology:measurement(nice_su_t410, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 410, 0.63).
narrative_ontology:measurement_basis(nice_su_t410, observed).
narrative_ontology:measurement(nice_su_t431, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 431, 0.61).
narrative_ontology:measurement_basis(nice_su_t431, observed).
narrative_ontology:measurement(nice_su_t451, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 451, 0.61).
narrative_ontology:measurement_basis(nice_su_t451, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoiousios_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_christological_kernel__homoiousios_reading, 0.12).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel__homoousios_reading).

% DUAL FORMULATION NOTE:
% The Nicene Christological kernel decomposes into two structurally distinct constraint stories: homoiousios_reading (Christ of similar substance; moderate extractiveness, enables pluralism but fragments unity) and homoousios_reading (Christ of identical substance; lower extractiveness, enforces uniformity but suppresses exegetical autonomy). The two readings are not the same constraint viewed from different angles—they have different ε values, different beneficiary/victim structures, and different empirical status in fourth-century disputes. Each reading instantiates the kernel's commitment to a single orthodox Christology; they differ on what counts as orthodox. Linked via network.affects_constraints to enable contamination analysis: fragmentation under homoiousios (this reading) structurally impacts the institutional viability of homoousios uniformity (sibling reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_christological_kernel__homoiousios_reading, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
