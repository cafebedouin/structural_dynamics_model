% ============================================================================
% CONSTRAINT STORY: hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_reading
 *   human_readable: Hybrid Reading: Medieval Latin as Partially Legitimate with Textual Correction
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The hybrid reading of 'correct Latin' instantiates a specific position
 *   within a contested kernel: medieval Latin forms are partially legitimate
 *   insofar as they preserve classical originals, but they are subject to
 *   systematic correction via textual evidence. This reading occupies a
 *   middle ground between the continuity reading (medieval forms are fully
 *   legitimate as authentic tradition) and the discontinuity reading
 *   (medieval forms are mostly error and should be abandoned in favor of
 *   classical restoration). The hybrid reading's core claim is that medieval
 *   practice preserved a genuine grammatical core of classical Latin while
 *   introducing orthographic, lexical, and phonetic innovations that can be
 *   identified and corrected through manuscript comparison and textual
 *   reconstruction. The constraint operates through the authority of reform
 *   philologists to enforce corrective standards while acknowledging the
 *   legitimacy of medieval transmission. This creates a tangled
 *   coordination-extraction hybrid: the framework coordinates scholarly
 *   standards (establishing which medieval forms are acceptable and which
 *   require correction) while simultaneously extracting authority from
 *   medieval scribal communities (delegitimizing their forms as 'errors'
 *   subject to correction). The theater ratio (0.58) reflects that much of
 *   the corrective work is performative: scholars debate the 'correct' form
 *   while medieval texts continue to circulate in their original medieval
 *   state, and ecclesiastical institutions maintain medieval Latin through
 *   ritual use despite scholarly correction efforts.
 *
 * KEY AGENTS:
 *   - Medieval Scribe: Primary victim (powerless/trapped) — reproduces inherited medieval forms while facing delegitimization and correction from reform philologists; no exit from the scribal tradition
 *   - Classical Purist Authority: Secondary beneficiary (moderate/constrained) — gains authority to establish standards but constrained by need to acknowledge medieval legitimacy; experiences mixed coordination and extraction
 *   - Reform Philologist: Primary beneficiary (institutional/arbitrage) — gains institutional authority through textual scholarship and corrective power; experiences the constraint as coordination mechanism
 *   - Textual Criticism Movement: Organized agent (organized/mobile) — sees the hybrid reading as transitional framework with implicit sunset as manuscript evidence accumulates
 *   - Ecclesiastical Latin Tradition: Institutional actor (institutional/constrained) — maintains medieval forms through ritual inertia despite corrective pressure; high theater ratio
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices about correctness as immutable linguistic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_reading, 0.35).
domain_priors:suppression_score(hybrid_reading, 0.42).
domain_priors:theater_ratio(hybrid_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(hybrid_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_reading, "Hybrid Reading: Medieval Latin as Partially Legitimate with Textual Correction").
narrative_ontology:topic_domain(hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_reading, 'cbc85a77-edeb-4c0b-bd6c-a17681aced76').
narrative_ontology:cs_kernel_codification('cbc85a77-edeb-4c0b-bd6c-a17681aced76', fixed_text).
narrative_ontology:cs_authority_grounding('cbc85a77-edeb-4c0b-bd6c-a17681aced76', lineage).
narrative_ontology:cs_interpretation_layer_present('cbc85a77-edeb-4c0b-bd6c-a17681aced76').
narrative_ontology:cs_reading_relation('cbc85a77-edeb-4c0b-bd6c-a17681aced76', hybrid_reading__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('cbc85a77-edeb-4c0b-bd6c-a17681aced76', hybrid_reading__discontinuity_reading, coexists_with).
narrative_ontology:cs_axiom('cbc85a77-edeb-4c0b-bd6c-a17681aced76', foundational, partial_legitimacy_via_textual_evidence).
narrative_ontology:cs_axiom_status(partial_legitimacy_via_textual_evidence, holdable).
narrative_ontology:cs_axiom_grounding('cbc85a77-edeb-4c0b-bd6c-a17681aced76', partial_legitimacy_via_textual_evidence, empirically_contingent).
narrative_ontology:cs_axiom('cbc85a77-edeb-4c0b-bd6c-a17681aced76', foundational, gradual_reform_not_restoration).
narrative_ontology:cs_axiom_status(gradual_reform_not_restoration, holdable).
narrative_ontology:cs_axiom_grounding('cbc85a77-edeb-4c0b-bd6c-a17681aced76', gradual_reform_not_restoration, deontological).
narrative_ontology:cs_reference_frame('cbc85a77-edeb-4c0b-bd6c-a17681aced76', classical_latin_as_transmitted_through_medieval_practice).
narrative_ontology:cs_drift_state('cbc85a77-edeb-4c0b-bd6c-a17681aced76', contemporary_textual_scholarship, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('cbc85a77-edeb-4c0b-bd6c-a17681aced76', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_reading, medieval_textual_tradition).
narrative_ontology:constraint_beneficiary(hybrid_reading, reform_philologists).
narrative_ontology:constraint_victim(hybrid_reading, classical_purist_authority).
narrative_ontology:constraint_victim(hybrid_reading, scribal_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hybrid_reading, classical_purist_authority).
narrative_ontology:constraint_victim(hybrid_reading, medieval_scribe).
narrative_ontology:constraint_victim(hybrid_reading, ecclesiastical_latin_tradition).
narrative_ontology:constraint_vindicates(hybrid_reading, textual_evidence_supremacy).
narrative_ontology:constraint_vindicates(hybrid_reading, gradual_linguistic_evolution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reproduces texts in inherited medieval forms while facing correction and delegitimization from reform philologists. Cannot abandon the scribal tradition without abandoning the role itself. Bears the cost of the hybrid reading's corrective mechanism through loss of authority and prestige.
narrative_ontology:constraint_stakeholder(hybrid_reading, medieval_scribe, payer,
    powerless, biographical, trapped, regional).

% Establishes and enforces corrective standards through textual scholarship. Gains institutional authority and prestige from the power to identify and correct medieval innovations. Can shift to different frameworks (pure classical restoration or acceptance of medieval variants) but benefits from the hybrid reading's legitimacy.
narrative_ontology:constraint_stakeholder(hybrid_reading, reform_philologist, agenda_setter,
    institutional, generational, arbitrage, global).

% Maintains authority over 'correct' Latin while acknowledging that medieval transmission preserved genuine classical forms. Benefits from the corrective framework (legitimizes their authority) but constrained by need to admit medieval practice was not pure error. Experiences mixed coordination (establishing standards) and extraction (defending classical supremacy).
narrative_ontology:constraint_stakeholder(hybrid_reading, classical_purist_authority, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hybrid_reading, classical_purist_authority, payer).

% Organized scholarly movement (manuscript scholars, editors, universities) that sees the hybrid reading as transitional framework. Accumulates manuscript evidence and develops editorial standards. Has mobile exit options and can shift to pure classical restoration or accept medieval forms as legitimate variants.
narrative_ontology:constraint_stakeholder(hybrid_reading, textual_criticism_movement, agenda_setter,
    organized, generational, mobile, global).

% Maintains medieval forms through liturgical use and institutional inertia despite corrective pressure from reform philologists. Defends medieval forms as 'authentic tradition' while simultaneously being subject to scholarly correction. Bears the cost of the hybrid reading's corrective mechanism through delegitimization of ecclesiastical practice.
narrative_ontology:constraint_stakeholder(hybrid_reading, ecclesiastical_latin_tradition, payer,
    institutional, civilizational, constrained, continental).

% The corpus of manuscript variants and reconstructed classical originals that serves as the arbiter of legitimacy in the hybrid reading. Not an agent but a non-agent entity kept for narrative completeness. Feeds the authority structure but does not collect from the constraint.
narrative_ontology:constraint_stakeholder(hybrid_reading, textual_evidence, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(hybrid_reading, textual_evidence).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishing standards for which medieval Latin forms preserve classical originals and which are medieval innovations requiring correction. The constraint solves the coordination problem of how to maintain continuity with classical tradition while acknowledging medieval transmission.
% TRANSFER_FUNCTION: Authority and prestige flow from medieval scribal communities to reform philologists. Medieval forms are delegitimized as 'errors' subject to correction, while corrective authority is concentrated in the hands of textual scholars and institutional establishments.
% ABSENT_VOICES: Medieval scribal communities themselves are largely absent from the scholarly conversation about correctness. Their perspective on the legitimacy of their own forms is not represented in the framework that judges them. Ecclesiastical institutions maintain medieval forms through ritual but do not participate in the scholarly debate about correctness.
% DISAPPEARANCE_RATIONALE: If the hybrid reading's corrective framework disappeared, the world would partially rearrange: reform philologists would lose institutional authority, medieval forms would regain legitimacy, and ecclesiastical institutions would no longer face corrective pressure. However, the underlying linguistic facts (medieval forms do differ from classical originals) would remain unchanged. The constraint's disappearance would not eliminate the linguistic differences, only the institutional framework that judges them as 'errors' requiring correction.
% FOUNDING_PROBLEM: The problem of maintaining continuity with classical Latin tradition while acknowledging that medieval transmission introduced orthographic, lexical, and phonetic innovations. How can medieval forms be recognized as legitimate transmission while also being subject to correction?
% FOUNDING_PROBLEM_CORROBORATION: Textual scholars and manuscript evidence attest that medieval forms do differ from reconstructed classical originals. Ecclesiastical institutions attest that medieval forms remain functionally adequate for liturgical purposes. The founding problem is live because the tension between medieval legitimacy and classical authority remains unresolved.
narrative_ontology:disappearance_verdict(hybrid_reading, contested).
narrative_ontology:founding_problem_status(hybrid_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIEVAL SCRIBE (SNARE) — Trapped within the constraint's enforcement: must reproduce texts in inherited medieval forms while facing correction and delegitimization from reform philologists. No exit from the scribal tradition; bears full cost of the hybrid reading's correction mechanism. Cannot abandon medieval practice without abandoning the role itself.
constraint_indexing:constraint_classification(hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CLASSICAL PURIST AUTHORITY (TANGLED ROPE) — Constrained by the need to maintain authority over 'correct' Latin while acknowledging that medieval transmission preserved genuine classical forms. Benefits from the hybrid reading's framework (legitimizes their corrective authority) but also bears costs (must admit medieval practice was not pure error). Experiences mixed coordination (establishing standards) and extraction (defending classical supremacy against textual evidence).
constraint_indexing:constraint_classification(hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORM PHILOLOGIST (ROPE) — Net beneficiary of the hybrid reading. Gains institutional authority through textual scholarship and the power to correct medieval forms via manuscript evidence. Experiences the constraint as coordination: establishing standards for which medieval forms are legitimate and which require correction. Has arbitrage options (can shift to discontinuity reading or pure classical restoration) but benefits from the hybrid framework's legitimacy.
constraint_indexing:constraint_classification(hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TEXTUAL CRITICISM MOVEMENT (SCAFFOLD) — Organized agents (manuscript scholars, editors, universities) see the hybrid reading as a transitional framework: medieval forms are legitimate insofar as they preserve classical originals, but systematic textual correction will eventually establish a unified standard. The sunset is implicit: as manuscript evidence accumulates and editorial standards mature, the need for the hybrid framework diminishes. Mobile exit options (can adopt pure classical restoration or accept medieval forms as legitimate variants).
constraint_indexing:constraint_classification(hybrid_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ECCLESIASTICAL LATIN TRADITION (PITON) — Maintains medieval forms through institutional inertia despite the hybrid reading's corrective pressure. The church's liturgical use of medieval Latin persists as performative continuity: the forms are defended as 'authentic tradition' while simultaneously being subject to scholarly correction. Theater ratio high because the tradition's authority is maintained through ritual repetition rather than through genuine resistance to textual evidence.
constraint_indexing:constraint_classification(hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, linguistic change is an immutable property of language transmission: medieval forms necessarily diverge from classical originals through copying errors, phonetic shifts, and scribal innovation. The hybrid reading's corrective framework appears as a natural law of textual scholarship — the inevitable process by which later forms are compared against earlier evidence. However, this naturalizes what is actually a contingent institutional choice about which forms count as 'correct' and who has authority to enforce correction.
constraint_indexing:constraint_classification(hybrid_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The hybrid reading creates asymmetric extraction through the authority to correct medieval forms, but the extraction is constrained by the reading's own acknowledgment that medieval forms are partially legitimate. The reform philologists benefit from institutional authority and the power to establish standards, but they cannot claim total authority over 'correct' Latin — they must justify corrections through textual evidence. The extractiveness increases over the interval (0.20 → 0.38) as the corrective framework becomes institutionalized and enforcement intensifies. Suppression (0.42): Moderate. Medieval scribal communities face significant barriers to resisting correction: the authority of textual evidence, the prestige of classical scholarship, and the institutional power of universities and editorial establishments. However, suppression is not total — ecclesiastical institutions continue to use medieval forms through ritual practice, and some scribal traditions persist despite corrective pressure. Suppression increases over the interval as the corrective framework becomes more systematized and enforcement machinery matures. Theater ratio (0.58): Moderate-high. The corrective work involves substantial performative elements: scholarly debates about the 'correct' form, editorial apparatus that displays corrections without necessarily changing the underlying texts, and ritual maintenance of medieval forms in ecclesiastical contexts despite scholarly correction. The theater ratio increases over the interval as the gap widens between what scholars claim is 'correct' and what actually circulates in practice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural arrangement (medieval forms subject to textual correction) produces radically different classifications depending on the observer's position. The medieval scribe experiences pure extraction (Snare) — they are trapped within the constraint and bear its full cost. The classical purist experiences mixed coordination and extraction (Tangled Rope) — they benefit from the corrective authority but are constrained by the need to acknowledge medieval legitimacy. The reform philologist experiences coordination (Rope) — they are solving the legitimate problem of establishing standards. The textual criticism movement experiences a transitional framework (Scaffold) — they see the corrective mechanism as temporary, with an implicit sunset as evidence accumulates. The ecclesiastical tradition experiences degraded ritual (Piton) — they maintain medieval forms through institutional inertia despite corrective pressure. The analytical observer risks seeing an immutable natural law (Mountain) — linguistic change appears inevitable and correction appears natural — but the structural data reveals this as a false summit: the authority to correct is a contingent institutional choice, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position relative to the constraint. Medieval scribes are trapped victims with no exit — they experience maximum extraction (d ≈ 1.0). Classical purists are moderate agents with constrained exit — they benefit from the corrective authority but are constrained by the need to acknowledge medieval legitimacy (d ≈ 0.45). Reform philologists are institutional beneficiaries with arbitrage options — they gain authority and prestige from textual scholarship (d ≈ 0.25). The textual criticism movement is organized with mobile exit options — they can shift to different frameworks or adopt pure classical restoration (d ≈ 0.35). The ecclesiastical tradition is institutional with constrained exit — they maintain medieval forms through ritual but face corrective pressure (d ≈ 0.55). The analytical observer has analytical exit options and sees the constraint from a civilizational perspective (d ≈ 0.5, but risks naturalizing contingent choices).
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resolves the mandatrophy by establishing a framework in which medieval forms are partially legitimate but subject to correction. This prevents the constraint from collapsing into pure extraction (the discontinuity reading's risk) or pure coordination (the continuity reading's risk). The mandate is to establish standards for which medieval forms preserve classical originals and which are innovations requiring correction. The constraint's persistence depends on the continued authority of textual evidence and the institutional power of reform philologists. If textual evidence becomes insufficient to justify corrections, the mandate fails and the constraint becomes extractive (snare). If medieval forms are fully accepted as legitimate variants, the mandate becomes unnecessary and the constraint dissolves. The hybrid reading's strength is that it acknowledges both the legitimacy of medieval transmission and the authority of textual evidence, creating a framework that can accommodate both perspectives without collapsing into either extreme.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a reading of the contested kernel ''correct_latin'', or does it constitute a distinct constraint with its own ε?',
    'Comparison with sibling readings (continuity_reading, discontinuity_reading): if the three readings produce substantially different ε values when measured against the same observable (e.g., institutional enforcement intensity, beneficiary extraction rates), they are distinct constraints and should be decomposed. If ε values converge, the constraint is genuinely a single kernel with multiple readings.',
    'If distinct constraints: each reading gets its own story file with independent ε, perspectives, and measurements. If single kernel: the three readings remain linked via cs_structure.reading_relations and share a common underlying constraint structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this is a reading of a kernel or a distinct constraint').

omega_variable(
    medieval_legitimacy_threshold,
    'What proportion of medieval forms must preserve classical originals for the hybrid reading''s ''partial legitimacy'' claim to hold?',
    'Systematic textual analysis: comparison of medieval manuscripts against reconstructed classical originals; quantification of forms that are demonstrably classical vs. those that are medieval innovations. Threshold determination: at what percentage does ''partial legitimacy'' become ''mostly error''?',
    'If threshold > 70%: medieval tradition is substantially legitimate, and the hybrid reading''s corrective framework is minimal. If threshold < 30%: medieval forms are mostly innovations, and the discontinuity reading becomes more plausible. If threshold 30-70%: the hybrid reading''s mixed framework is structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_legitimacy_threshold, empirical, 'Proportion of medieval forms that preserve classical originals').

omega_variable(
    textual_evidence_authority,
    'Does textual evidence (manuscript variants, reconstructed originals) provide sufficient grounds for correcting medieval forms, or does it merely suggest alternatives without establishing authority?',
    'Epistemological analysis: examination of how textual scholars justify corrections; comparison of correction rates across different manuscript families and time periods. Does evidence accumulation produce convergence on a single ''correct'' form, or persistent disagreement?',
    'If evidence provides authority: the hybrid reading''s corrective mechanism is justified, and reform philologists legitimately enforce standards. If evidence merely suggests: the hybrid reading''s enforcement becomes more extractive (imposing one reading over others without sufficient grounds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_evidence_authority, conceptual, 'Whether textual evidence provides sufficient authority for correction').

omega_variable(
    sibling_reading_foreclosure,
    'Does the hybrid reading''s core premise (medieval forms are partially legitimate but subject to textual correction) logically foreclose either the continuity reading (medieval forms are fully legitimate) or the discontinuity reading (medieval forms are mostly error)?',
    'Logical analysis: examination of whether the hybrid reading''s axioms (partial_legitimacy_via_textual_evidence, gradual_reform_not_restoration) directly contradict the core premises of the sibling readings. Can a single framework hold both the hybrid reading and one of its siblings?',
    'If foreclosure exists: the reading_relations should be ''forecloses'' rather than ''coexists_with''. If no foreclosure: the readings coexist as live positions held by different scholarly communities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the hybrid reading logically forecloses sibling readings').

omega_variable(
    reform_sunset_mechanism,
    'What conditions would constitute the sunset of the hybrid reading''s corrective framework? At what point would the constraint dissolve?',
    'Scenario analysis: identification of conditions under which the hybrid reading would no longer be necessary (e.g., complete textual reconstruction, universal adoption of classical standards, or acceptance of medieval forms as legitimate variants). Historical trajectory: is the field moving toward any of these endpoints?',
    'If sunset is achievable: the constraint is genuinely transitional (scaffold-like). If sunset is indefinite: the constraint is permanent (tangled_rope or snare). If sunset is actively being resisted: the constraint is extractive (snare or piton).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_sunset_mechanism, preference, 'Conditions for sunset of the hybrid reading''s corrective framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_tr_t0, hybrid_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(hybrid_tr_t3, hybrid_reading, theater_ratio, 3, 0.52).
narrative_ontology:measurement(hybrid_tr_t6, hybrid_reading, theater_ratio, 6, 0.58).
narrative_ontology:measurement(hybrid_tr_t9, hybrid_reading, theater_ratio, 9, 0.62).

% Extraction over time
narrative_ontology:measurement(hybrid_be_t0, hybrid_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(hybrid_be_t3, hybrid_reading, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(hybrid_be_t6, hybrid_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(hybrid_be_t9, hybrid_reading, base_extractiveness, 9, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_su_t0, hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hybrid_su_t3, hybrid_reading, suppression_requirement, 3, 0.38).
narrative_ontology:measurement(hybrid_su_t6, hybrid_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(hybrid_su_t9, hybrid_reading, suppression_requirement, 9, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_reading, information_standard).
narrative_ontology:boltzmann_floor_override(hybrid_reading, 0.12).
narrative_ontology:affects_constraint(hybrid_reading, continuity_reading).
narrative_ontology:affects_constraint(hybrid_reading, discontinuity_reading).
narrative_ontology:affects_constraint(hybrid_reading, textual_reconstruction_authority).
narrative_ontology:affects_constraint(hybrid_reading, ecclesiastical_latin_persistence).

% DUAL FORMULATION NOTE:
% The hybrid reading is one of three readings of the contested kernel 'correct_latin'. The three readings (hybrid_reading, continuity_reading, discontinuity_reading) are structurally distinct constraints with different ε values, different beneficiary/victim structures, and different classifications. They are linked via network.affects_constraints and via cs_structure.reading_relations. The hybrid reading influences both siblings by establishing textual evidence as the arbiter of legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hybrid_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
