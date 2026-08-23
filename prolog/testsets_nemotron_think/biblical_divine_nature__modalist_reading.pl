% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__modalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__modalist_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: biblical_divine_nature__modalist_reading
 *   human_readable: Modalist Reading: Father/Son/Spirit as Sequential Modes of One Divine Person
 *   domain: theological/doctrinal
 *
 * SUMMARY:
 *   The modalist reading of the biblical divine nature kernel holds that
 *   Father, Son, and Holy Spirit are not three simultaneous persons
 *   (hypostases) sharing one essence (ousia), but three sequential modes,
 *   roles, or manifestations of the one divine person. Historically
 *   associated with Sabellius (c. 215), Noetus of Smyrna, and Praxeas, it was
 *   condemned as 'Patripassianism' (the Father suffered) and 'Sabellianism'
 *   by the early church. The reading re-emerged in medieval anti-Trinitarian
 *   movements and explosively in 20th-century Oneness Pentecostalism (~30-50
 *   million adherents). It claims to be the pure biblical alternative to both
 *   Trinitarian philosophical speculation and Unitarian reduction of Christ's
 *   deity. The constraint coordinates Oneness communities around Jesus-name
 *   baptism and Spirit infilling as salvific necessities, while extracting
 *   conformity through rebaptism requirements and ecumenical exclusion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, 0.55).
domain_priors:suppression_score(biblical_divine_nature__modalist_reading, 0.45).
domain_priors:theater_ratio(biblical_divine_nature__modalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__modalist_reading, rope).
narrative_ontology:human_readable(biblical_divine_nature__modalist_reading, "Modalist Reading: Father/Son/Spirit as Sequential Modes of One Divine Person").
narrative_ontology:topic_domain(biblical_divine_nature__modalist_reading, "theological/doctrinal").

domain_priors:requires_active_enforcement(biblical_divine_nature__modalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__modalist_reading, '1dc6e7d4-2c8f-4699-a924-730c0e9fdda1').
narrative_ontology:cs_kernel_codification('1dc6e7d4-2c8f-4699-a924-730c0e9fdda1', fixed_text).
narrative_ontology:cs_authority_grounding('1dc6e7d4-2c8f-4699-a924-730c0e9fdda1', lineage).
narrative_ontology:cs_interpretation_layer_present('1dc6e7d4-2c8f-4699-a924-730c0e9fdda1').
narrative_ontology:cs_reading_relation('1dc6e7d4-2c8f-4699-a924-730c0e9fdda1', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('1dc6e7d4-2c8f-4699-a924-730c0e9fdda1', biblical_divine_nature__unitarian_reading, forecloses).
narrative_ontology:cs_axiom('1dc6e7d4-2c8f-4699-a924-730c0e9fdda1', foundational, god_is_one_person_sequential_modes).
narrative_ontology:cs_axiom_status(god_is_one_person_sequential_modes, holdable).
narrative_ontology:cs_axiom_grounding('1dc6e7d4-2c8f-4699-a924-730c0e9fdda1', god_is_one_person_sequential_modes, theological).
narrative_ontology:cs_axiom('1dc6e7d4-2c8f-4699-a924-730c0e9fdda1', foundational, jesus_name_baptism_necessary_for_salvation).
narrative_ontology:cs_axiom_status(jesus_name_baptism_necessary_for_salvation, holdable).
narrative_ontology:cs_axiom_grounding('1dc6e7d4-2c8f-4699-a924-730c0e9fdda1', jesus_name_baptism_necessary_for_salvation, instrumental).
narrative_ontology:cs_reference_frame('1dc6e7d4-2c8f-4699-a924-730c0e9fdda1', apostolic_oneness_theology).
narrative_ontology:cs_drift_state('1dc6e7d4-2c8f-4699-a924-730c0e9fdda1', contemporary_oneness_movement, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('1dc6e7d4-2c8f-4699-a924-730c0e9fdda1', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__modalist_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, oneness_adherents).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, modalist_clergy).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, trinitarian_christians_excluded).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, unitarian_christians_excluded).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, converts_required_rebaptism).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, modalist_clergy).
narrative_ontology:constraint_vindicates(biblical_divine_nature__modalist_reading, biblical_monotheism_preserved).
narrative_ontology:constraint_vindicates(biblical_divine_nature__modalist_reading, jesus_fully_divine_without_philosophical_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets doctrinal boundaries for Oneness Pentecostal denominations (UPCI, PAW, etc.): defines the modalist Christology as non-negotiable, enforces Jesus-name baptism as essential for salvation, polices clergy adherence. Their authority derives from claimed apostolic continuity and scriptural interpretation. Exit would mean abandoning the movement they lead and the identity constituted by it.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, oneness_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Experience the modalist reading as a coherent, Jesus-centered piety that avoids Trinitarian philosophical complexity. Gain communal belonging, clear soteriology (baptism in Jesus' name + Spirit infilling), and worship focus. Exit is costly: identity is fused with the Oneness community and its distinctive practices; leaving means losing the interpretive framework that makes their religious experience intelligible.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, oneness_adherents, beneficiary,
    organized, biographical, identity_locked, global).

% Hold ministerial credentials dependent on modalist orthodoxy; benefit from clear doctrinal lane and institutional support. But bear costs: limited ecumenical recognition, barriers to academic theology positions, requirement to rebaptize Trinitarian converts. Exit to Trinitarian ministry requires credential surrender and often re-education.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, modalist_clergy, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__modalist_reading, modalist_clergy, payer).

% Structurally excluded from Oneness fellowship: their baptism (Trinitarian formula) is deemed invalid, requiring rebaptism for entry; their ordination is unrecognized; their theology is anathematized as 'three gods.' They bear the cost of non-recognition but have high exit options — they remain within the global Trinitarian mainstream with full institutional standing.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, trinitarian_christians_excluded, payer,
    institutional, civilizational, arbitrage, global).

% Excluded because modalism affirms full deity of Son and Spirit (as modes), which unitarianism denies. Their baptism is also rejected. They bear exclusion costs but maintain distinct communities with mobile exit options — they can join other unitarian or liberal Christian bodies without rebaptism barriers.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, unitarian_christians_excluded, payer,
    organized, generational, mobile, global).

% Trinitarian or unitarian converts to Oneness groups must undergo rebaptism in Jesus' name, implicitly invalidating their prior Christian initiation. This is a concrete extraction: surrender of prior baptismal identity, often family/social friction. Exit is constrained — they sought this community, but the entry cost is non-negotiable.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, converts_required_rebaptism, payer,
    moderate, immediate, constrained, local).

% Analyze the modalist reading as a 2nd-3rd century Christological option (Sabellius, Noetus, Praxeas) condemned at Rome (c. 220) and Nicaea (325), persisting in medieval anti-Trinitarian movements and modern Oneness Pentecostalism. They see the full constraint family: modalist, trinitarian, unitarian readings of the biblical divine nature kernel.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, historical_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a cognitively simple, Jesus-centered Christology that preserves strict monotheism without requiring the philosophical apparatus of hypostasis/ousia distinctions — enables worship, baptism, and communal identity around 'one God manifest in three modes.'
% TRANSFER_FUNCTION: Moves doctrinal authority and sacramental validity from historic catholic/orthodox/protestant structures (Trinitarian baptism, ecumenical recognition) to Oneness bodies (Jesus-name baptism, modalist ordination). Converts transfer their prior baptismal standing; clergy transfer institutional recognition.
% ABSENT_VOICES: Early modalist proponents (Sabellius, Noetus) whose writings survive only in hostile Trinitarian sources; modern biblical scholars who see modalism in NT texts but cannot voice it in confessional settings; 'Jesus Only' believers in Trinitarian churches who remain silent.
% DISAPPEARANCE_RATIONALE: If the modalist reading vanished overnight, ~30-50 million Oneness Pentecostals would lose their distinctive Christological identity and baptismal theology; rebaptism practices would cease; the global Oneness institutional network (UPCI, PAW, etc.) would dissolve or restructure around Trinitarian or unitarian alternatives. The religious landscape rearranges.
% FOUNDING_PROBLEM: How to confess Jesus as fully divine while preserving absolute numerical monotheism without importing Greek philosophical categories (hypostasis, ousia, persona) that the biblical text does not contain.
% FOUNDING_PROBLEM_CORROBORATION: Oneness apologists (David Bernard, Daniel Segraves) attest the problem remains live — Trinitarianism still relies on post-biblical philosophy. Trinitarian patristic scholars (Khaled Anatolios, Lewis Ayres) attest the problem was solved by Nicaea's ousia/hypostasis distinction, not by modalism. Unitarian scholars attest modalism fails monotheism by divinizing the Son. No neutral arbiter; all three readings claim the founding problem.
narrative_ontology:disappearance_verdict(biblical_divine_nature__modalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__modalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__modalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_divine_nature__modalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__modalist_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__modalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__modalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.55) reflects real coordination benefits (simple Christology, clear soteriology) alongside real extraction costs (rebaptism, ecumenical isolation, clergy credential capture). Suppression (0.45) is moderate: enforcement is active within Oneness bodies (doctrinal policing, baptismal gatekeeping) but exit to Trinitarian churches is structurally open. Theater ratio (0.25) is low — the coordination function is genuine for adherents. Accessibility collapse (0.65) is moderately high: once the modalist interpretive frame is adopted, Trinitarian alternatives appear as philosophical corruption. Resistance (0.60) remains substantial: 1800 years of conciliar condemnation and ongoing polemic from both Trinitarian and Unitarian sides. The cyclical measurement pattern (peaks at 300yrs = post-Nicene suppression; 1500-1800yrs = Oneness revival) reflects the constraint's marginalization and resurgence.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter/beneficiary seats, the constraint is experienced as rope — genuine coordination of biblical truth. From the payer seats (especially converts), it operates as snare — extraction via rebaptism and exclusion. From the analytical observer seat, it computes as tangled_rope: coordination function (Jesus-centered monotheism) + asymmetric extraction (rebaptism gate, ecumenical isolation) + active enforcement. The engine captures this divergence; the claimed_type (rope) records the modalist self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Oneness leadership (agenda_setter) sits at d≈0.1 (beneficiary end): they administer the constraint and collect institutional authority. Adherents and clergy (beneficiaries) sit at d≈0.2-0.3: genuine coordination benefit, identity-locked exit. Converts_required_rebaptism (payers) sit at d≈0.7: bear concrete entry costs. Trinitarian/unitarian excluded (payers) sit at d≈0.4-0.5: excluded but high exit options (arbitrage/mobile) dampen effective extraction. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (monotheistic Christology without Greek philosophy) is contested: Oneness says it's live; Trinitarians say Nicaea solved it; Unitarians say modalism fails monotheism. The constraint persists not because the founding problem is universally acknowledged as live, but because the Oneness movement has built institutional inertia around its solution. Mandatrophy is unresolved — the arrangement's original justification is disputed, yet it coordinates a massive global community.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the modalist reading a distinct constraint with its own ε, or a variant interpretation of the same constraint as the trinitarian reading?',
    'Apply ε-invariance test: if measuring the constraint via ''baptismal validity'' yields high extraction (rebaptism required) but measuring via ''Christological coherence'' yields low extraction (unified worship), these are two constraints. Author separate stories for each measurable claim.',
    'If distinct, the modalist reading''s ε is assessed on its own structural terms (rebaptism gate, Oneness cohesion). If conflated, the ε average obscures the extraction/coordination asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings instantiate separate constraints per ε-invariance principle.').

omega_variable(
    suppression_mechanism_modalist,
    'Is the suppression experienced by trinitarian_christians_excluded structural (formal rebaptism requirement, ecumenical anathema) or internalized (Oneness adherents'' belief that Trinitarians worship a false god)?',
    'Post-exit suppression trajectory: if a Trinitarian converts to Oneness, does the suppression (view of prior baptism as invalid) persist as internalized conviction, or was it purely structural?',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression (delegitimization of prior faith) after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_modalist, empirical, 'Structural vs. internalized suppression in inter-ecclesial exclusion.').

omega_variable(
    modalist_coordination_extraction_boundary,
    'Is the rebaptism requirement structurally necessary for the modalist coordination function (Jesus-centered piety), or is it an extractive addition that could be removed without dissolving the community?',
    'Counterfactual: if an Oneness body recognized Trinitarian baptism as valid while retaining modalist Christology, would the coordination function (shared worship, soteriology, identity) survive?',
    'If necessary, the extraction is the price of coordination (tangled_rope). If contingent, the rebaptism gate is a separable snare component layered on a rope core.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modalist_coordination_extraction_boundary, conceptual, 'Separability of coordination and extraction components in the modalist constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__modalist_reading, 0, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_tr_t0, biblical_divine_nature__modalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_tr_t300, biblical_divine_nature__modalist_reading, theater_ratio, 300, 0.3).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_tr_t600, biblical_divine_nature__modalist_reading, theater_ratio, 600, 0.2).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_tr_t900, biblical_divine_nature__modalist_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_tr_t1200, biblical_divine_nature__modalist_reading, theater_ratio, 1200, 0.08).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_tr_t1500, biblical_divine_nature__modalist_reading, theater_ratio, 1500, 0.18).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_tr_t1800, biblical_divine_nature__modalist_reading, theater_ratio, 1800, 0.25).

% Extraction over time
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_be_t0, biblical_divine_nature__modalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_be_t300, biblical_divine_nature__modalist_reading, base_extractiveness, 300, 0.65).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_be_t600, biblical_divine_nature__modalist_reading, base_extractiveness, 600, 0.4).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_be_t900, biblical_divine_nature__modalist_reading, base_extractiveness, 900, 0.3).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_be_t1200, biblical_divine_nature__modalist_reading, base_extractiveness, 1200, 0.25).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_be_t1500, biblical_divine_nature__modalist_reading, base_extractiveness, 1500, 0.45).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_be_t1800, biblical_divine_nature__modalist_reading, base_extractiveness, 1800, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_su_t0, biblical_divine_nature__modalist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_su_t300, biblical_divine_nature__modalist_reading, suppression_requirement, 300, 0.75).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_su_t600, biblical_divine_nature__modalist_reading, suppression_requirement, 600, 0.5).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_su_t900, biblical_divine_nature__modalist_reading, suppression_requirement, 900, 0.3).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_su_t1200, biblical_divine_nature__modalist_reading, suppression_requirement, 1200, 0.2).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_su_t1500, biblical_divine_nature__modalist_reading, suppression_requirement, 1500, 0.35).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_su_t1800, biblical_divine_nature__modalist_reading, suppression_requirement, 1800, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__modalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__modalist_reading, 0.08).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__unitarian_reading).

% DUAL FORMULATION NOTE:
% This is the modalist_reading of the biblical_divine_nature kernel. The trinitarian_reading and unitarian_reading are sibling constraints. All three share the kernel but instantiate distinct constraints with different ε, beneficiary/victim structures, and institutional trajectories. The modalist reading forecloses both siblings logically (one person vs. three persons; Son fully divine vs. Son subordinate) while coexisting sociologically as rival global communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_divine_nature__modalist_reading, institutional, 0.1).
constraint_indexing:directionality_override(biblical_divine_nature__modalist_reading, organized, 0.25).
constraint_indexing:directionality_override(biblical_divine_nature__modalist_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
