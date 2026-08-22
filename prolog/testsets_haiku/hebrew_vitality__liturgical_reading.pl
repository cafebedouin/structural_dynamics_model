% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__liturgical_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Hebrew Vitality via Liturgical Preservation
 *   domain: sociolinguistics/language_revitalization
 *
 * SUMMARY:
 *   This is ONE READING of the contested kernel 'hebrew_vitality'. The
 *   liturgical_reading claims that unbroken transmission and use of Hebrew in
 *   prayer and ritual CONSTITUTES the vitality of the language — language is
 *   alive when it is spoken, even if only in liturgical contexts. This
 *   reading grounds Hebrew's survival through diaspora and the centuries
 *   without native speakers in the continuity of rabbinic practice and prayer
 *   recitation. By this frame, a Jew reciting the Amidah in Hebrew is not
 *   preserving a dead language but actualizing its vitality. This reading is
 *   DISTINCT from the native_daily_reading (only native generation counts as
 *   vitality) and the hybrid_continuity_reading (liturgy was necessary but
 *   insufficient; vernacular revival required both). Each reading
 *   instantiates a different constraint with different beneficiary structures
 *   and different operative propositions about what 'vitality' means. This
 *   story author has instantiated the liturgical reading only — see
 *   kernel_context for the sibling readings and their structural
 *   relationships.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: institutional agenda-setter, hold power to define and transmit liturgical corpus; highly mobile (could abandon the reading) but deeply invested in it
 *   - liturgical_communities: organized beneficiary, experience language vitality through prayer and ritual; mobile exit but strong identity lock
 *   - native_hebrew_speakers: observer seat, use Hebrew vernacularly but are not part of the liturgical reading's authority frame
 *   - language_revitalization_theorists: excluded, would reject the equation of ritual with vitality and argue for living generation
 *   - secularist_hebrew_users: observer, develop Hebrew outside ritual but are not recognized by this reading as constituting vitality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.15).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.05).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Hebrew Vitality via Liturgical Preservation").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistics/language_revitalization").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, '3d64067f-bcd2-4816-b32e-2259a9e0188c').
narrative_ontology:cs_kernel_codification('3d64067f-bcd2-4816-b32e-2259a9e0188c', fixed_text).
narrative_ontology:cs_authority_grounding('3d64067f-bcd2-4816-b32e-2259a9e0188c', lineage).
narrative_ontology:cs_interpretation_layer_present('3d64067f-bcd2-4816-b32e-2259a9e0188c').
narrative_ontology:cs_reading_relation('3d64067f-bcd2-4816-b32e-2259a9e0188c', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d64067f-bcd2-4816-b32e-2259a9e0188c', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('3d64067f-bcd2-4816-b32e-2259a9e0188c', foundational, unbroken_transmission_constitutes_vitality).
narrative_ontology:cs_axiom_status(unbroken_transmission_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('3d64067f-bcd2-4816-b32e-2259a9e0188c', unbroken_transmission_constitutes_vitality, deontological).
narrative_ontology:cs_axiom('3d64067f-bcd2-4816-b32e-2259a9e0188c', secondary, rabbinic_authority_over_language_definition).
narrative_ontology:cs_axiom_status(rabbinic_authority_over_language_definition, holdable).
narrative_ontology:cs_axiom_grounding('3d64067f-bcd2-4816-b32e-2259a9e0188c', rabbinic_authority_over_language_definition, conventional).
narrative_ontology:cs_reference_frame('3d64067f-bcd2-4816-b32e-2259a9e0188c', unbroken_rabbinic_transmission).
narrative_ontology:cs_drift_state('3d64067f-bcd2-4816-b32e-2259a9e0188c', contemporary_israel_native_revitalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3d64067f-bcd2-4816-b32e-2259a9e0188c', '2026-06-13T09:00:00Z').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, liturgical_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and interpret the liturgical corpus (siddur, piyyutim, prayer traditions). Preserve unbroken chain of recitation across generations. Hold authority over what counts as authentic liturgical use and when it is properly performed. Benefit from the reading's equation of liturgical preservation with language vitality — their role as transmitters is thereby elevated from custodial to essential.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, agenda_setter,
    institutional, civilizational, mobile, global).

% Practice daily and festival liturgy in Hebrew; receive liturgical texts from rabbinic tradition; experience Hebrew through prayer, blessing, and communal worship. The reading asserts that this practice CONSTITUTES the vitality of the language — they are not merely preserving but actualizing language life.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, liturgical_communities, beneficiary,
    organized, generational, mobile, global).

% Use Hebrew as a primary language for secular, daily, non-liturgical purposes. The liturgical reading does not classify them as beneficiaries or victims because it does not claim daily vernacular use is necessary for vitality — ritual use alone suffices. Their existence and flourishing are orthogonal to this reading's scope.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, native_hebrew_speakers, observer,
    organized, generational, mobile, national).

% Would argue that language vitality requires living vernacular generation, not ritual recitation alone; that a language used only in fixed liturgical contexts exhibits preservation, not vitality. They are structurally excluded from the liturgical reading's authority frame — their objection is not part of the conversation the reading conducts.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, language_revitalization_theorists, excluded,
    analytical, biographical, analytical, global).

% Use and develop Hebrew in secular contexts (literature, media, science, everyday speech). Under the liturgical reading, their contribution to language vitality is not recognized — they are not participants in the constraint structure, which routes vitality through ritual alone.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, secularist_hebrew_users, observer,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:fixing_cost_class(hebrew_vitality__liturgical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% FOUNDING_PROBLEM: After the Second Temple destruction and diaspora, Hebrew ceased to be a primary vernacular for most Jewish communities, risking assimilation and loss of Jewish identity. Yet transmission of liturgical and textual Hebrew continued unbroken through rabbinic Judaism, allowing communities across centuries and languages to maintain a shared sacred vocabulary and ritual practice.
% FOUNDING_PROBLEM_CORROBORATION: Medieval and early-modern historians (Salo Baron, David Biale) and historical linguists outside the rabbinic tradition attest that the founding problem was real: Hebrew disappeared as a vernacular in most diaspora communities by the early Middle Ages. Rabbinic authorities attest that liturgical preservation was essential to maintaining Jewish continuity. BUT: historians and linguists also attest that the founding problem IS NOW DEAD — the Zionist movement and the establishment of Israel created millions of native Hebrew speakers. Modern linguists (Bernard Spolsky, Ghil'ad Zuckermann) and Israeli academics attest that Hebrew is now a fully generative, living language with native speakers, not a liturgical-only language. The founding problem the liturgical reading was built to solve no longer exists.
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__liturgical_reading_tests).
:- end_tests(hebrew_vitality__liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.15) because the liturgical reading does not establish a victim set — liturgical preservation imposes no coercive cost on any identified party. Participants (liturgical communities) are genuine beneficiaries; they receive the practice and meaning it carries. Suppression is MINIMAL (0.05) because the constraint operates through cultural transmission and voluntary participation, not through exclusion or coercion of alternatives. The constraint is not defending itself against active resistance — secular Hebrew speakers coexist peacefully; they simply are not participants in the reading's scope. Theater_ratio is also low (0.08) because the functional core (transmitting liturgical texts, enabling practice) is genuine; there is no theatrical maintenance masking atrophied function. The accessibility_collapse is HIGH (0.92) because once you accept the reading's frame (ritual = vitality), the alternative framings collapse almost completely — the reading DEFINES vitality, so alternatives are not visible within the reading. But resistance is low (0.08) because few groups within the reading's scope are resisting the equation; those who would resist (language theorists, vernacular speakers) are simply excluded from the reading's authority structure, not suppressed. The measurement series shows slow upward drift in extractiveness and theater_ratio over the 100-year interval (t=0 to t=100), reflecting a modest trend: as secular Hebrew flourished and the founding problem died, the liturgical reading's extractive component rose slightly (it must now justify why ritual vitality matters despite living vernacular vitality), and its theatrical component rose marginally (some effort devoted to insisting on the reading's relevance despite the changed landscape).
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat, the liturgical reading is straightforward coordination: maintaining the corpus and enabling transmission is the constraint, vitality IS the unbroken practice, no extraction occurs. From the language-revitalization theorist seat (excluded), the same arrangement is a constraint on WHAT COUNTS as vitality — an attempt to route vitality definition through ritual authorities rather than through speakers and linguists, a form of authority capture on the meaning of the word itself. The engine computes the rabbinic seat as beneficiary with low extraction (the reading's own frame), and the excluded theorists seat would compute as observing an authority capture (from their frame). The divergence arises from the reading's DEFINITION of vitality, not from hidden costs — this is a conceptual constraint, not a coercive one.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities: d near beneficiary end (0.1–0.2) because they set the agenda, benefit from the authority it grants them, and have mobile exit (could endorse a different reading). Liturgical communities: d near symmetric (0.4–0.5) because they genuinely participate in and benefit from the practice; the cost (maintaining daily prayer routine) is voluntary and self-selected. Native speakers and secularist users: d = 0.5 or absent from the constraint structure entirely, because the reading does not pose any extraction relationship to them — they are observers, not parties. Language theorists: structural beneficiary of the constraint (it legitimizes their exclusion by defining them as outside the vitality frame) but also excluded from authority, creating an unstable position the engine would handle via a directionality override if one were warranted — but none is, because the reading simply does not include them in its scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining Hebrew identity through diaspora when most Jews ceased to speak it daily) WAS LIVE when the constraint emerged (post-Temple destruction through pre-Zionist diaspora, ~1900–2000 years). The founding problem IS NOW DEAD: modern Israel has millions of native Hebrew speakers; Hebrew is a fully generative, living language. The constraint (the reading that ritual = vitality) persists despite the founding problem's death. This is not a snare (no victim set, no coercion) and not a piton (theater_ratio is too low, functional core is genuine). It is a ROPE whose founding coordination problem has been solved by OTHER MEANS (native revitalization in Israel) while the rope itself continues. The reading now functions as a post-hoc justification for rabbinic authority over language definition rather than as a solution to a live problem. The constraint exhibits MANDATROPHY: the mandate (preserve Hebrew through diaspora via ritual) has outlived its urgency, but the constraint (the reading that ritual = vitality) persists as a framework that legitimizes rabbinic authority. The measurement series shows a small uptick in extractiveness and theater as the constraint ages into its post-mandate phase, reflecting this shift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_definition_boundary,
    'Is ''language vitality'' a property of the language itself (structural features, generative capacity) or a property of the commitment framework (what practitioners agree counts as alive)?',
    'Linguistic analysis of generative capacity in liturgical-only contexts vs. native-speaker contexts; ethnographic observation of what practitioners in each tradition understand by ''vitality''; comparison with other minority/diaspora language situations (Aramaic, Greek in diaspora Jewish communities; Sanskrit, Latin in other traditions).',
    'If vitality is structural (generative capacity), the liturgical reading''s equation of ritual with vitality fails — a language used only in fixed liturgical contexts lacks generative capacity and therefore lacks vitality by definition. If vitality is a commitment framework property, the reading succeeds — ritual communities can declare vitality via their participation. This is the core contestation between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_definition_boundary, conceptual, 'Whether ''vitality'' is a linguistic/structural fact or a committal framework property.').

omega_variable(
    founding_problem_death_mandatrophy,
    'Does the death of the founding problem (Hebrew now has millions of native speakers) change the constraint''s classification from rope (coordination) to piton (inertial)?',
    'Examine whether rabbinic authorities and liturgical communities still maintain the reading because it solves a live coordination problem (vitality preservation through diaspora) or because the reading itself has become institutionalized and self-justifying despite the problem''s resolution. Track whether effort devoted to the reading increases or decreases as the founding problem dies; observe whether new justifications are deployed (authority, tradition, authenticity) as the original coordination rationale weakens.',
    'If the reading persists despite the founding problem''s death and increasingly for reasons orthogonal to preservation (authority legitimization, identity maintenance, institutional continuity), it reclassifies to piton and the theater_ratio should rise substantially. If the reading remains a live solution to an ongoing coordination problem (maintaining Jewish identity through ritual despite linguistic assimilation), it stays rope. Current measurement series (low theater, slow upward drift) suggest the constraint is transitioning toward mandatrophy and piton territory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_death_mandatrophy, empirical, 'Whether the constraint''s founding problem is truly dead and the constraint is becoming inertial.').

omega_variable(
    kernel_committer_underspecification,
    'Is this constraint''s identity adequately captured by the lithurgical_reading classification, or does the kernel itself (what ''Hebrew vitality'' is) require a reading-independent core that all three sibling readings must respect?',
    'Test whether native_daily_reading and hybrid_continuity_reading accept the same KERNEL (the same question — what constitutes vitality?) but propose different ANSWERS. If they do, the kernel is reading-independent and stable. If they redefine the kernel itself, there is no single kernel — only family-related but distinct constraints.',
    'If the kernel is stable and reading-independent, the three constraints form a clean constraint family (three readings of one kernel) and network.affects_constraints is the right way to link them. If the kernel is itself reading-dependent (each reading defines what ''vitality'' means, and they have no common referent), the family structure breaks and each constraint is autonomous. The current authoring assumes the former (one kernel, three readings); this omega documents the uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_committer_underspecification, conceptual, 'Whether the hebrew_vitality kernel is reading-independent or reading-defined.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__liturgical_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebr_tr_t20, hebrew_vitality__liturgical_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(hebr_tr_t40, hebrew_vitality__liturgical_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement(hebr_tr_t60, hebrew_vitality__liturgical_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement(hebr_tr_t80, hebrew_vitality__liturgical_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement(hebr_tr_t100, hebrew_vitality__liturgical_reading, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__liturgical_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(hebr_be_t20, hebrew_vitality__liturgical_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(hebr_be_t40, hebrew_vitality__liturgical_reading, base_extractiveness, 40, 0.14).
narrative_ontology:measurement(hebr_be_t60, hebrew_vitality__liturgical_reading, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(hebr_be_t80, hebrew_vitality__liturgical_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement(hebr_be_t100, hebrew_vitality__liturgical_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__liturgical_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(hebr_su_t20, hebrew_vitality__liturgical_reading, suppression_requirement, 20, 0.03).
narrative_ontology:measurement(hebr_su_t40, hebrew_vitality__liturgical_reading, suppression_requirement, 40, 0.04).
narrative_ontology:measurement(hebr_su_t60, hebrew_vitality__liturgical_reading, suppression_requirement, 60, 0.05).
narrative_ontology:measurement(hebr_su_t80, hebrew_vitality__liturgical_reading, suppression_requirement, 80, 0.05).
narrative_ontology:measurement(hebr_su_t100, hebrew_vitality__liturgical_reading, suppression_requirement, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__liturgical_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% The hebrew_vitality kernel admits three structurally distinct readings: (1) liturgical_reading (this constraint) — ritual preservation = vitality; (2) native_daily_reading — native generation = vitality, ritual = preservation only; (3) hybrid_continuity_reading — both liturgy and vernacular required, and revitalization required reconstruction. Each reading instantiates a different constraint with different epsilon values, beneficiary sets, and authority frames. The readings coexist in contemporary discourse held by different communities and traditions. They are not logically foreclosed by each other (all three can be true at different time scales or from different perspectives) but they do compete for authority over the meaning of 'vitality'. Network links capture the family structure; each constraint also specifies reading_relations in cs_structure to formally state the relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
