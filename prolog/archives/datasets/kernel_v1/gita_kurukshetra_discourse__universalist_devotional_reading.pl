% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__universalist_devotional_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Gita Universalist Devotional Reading: Path-Independent Bhakti and Dharma as Divine Surrender
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   The Gita's teaching on devotion (bhakti) and dharma (righteous duty) is a
 *   site of deep hermeneutical contest. The universalist devotional reading
 *   interprets the text as saying: (1) Bhakti, the path of loving surrender
 *   to Krishna/the divine, is accessible to all humans regardless of caste,
 *   gender, or social role (Gita 9.29-30, 12.8-11); (2) True dharma is
 *   fundamentally the surrender of one's will to divine will, not the
 *   mechanical performance of caste-prescribed social roles (Gita 18.65-66);
 *   (3) By this teaching, caste disappears as a spiritual barrier —
 *   birth-status becomes irrelevant to the possibility of salvation. This
 *   reading has been the intellectual foundation of the bhakti movement
 *   (Vaishnavism, Shaivism, Kabir panthis, devotional Sufism influenced by
 *   bhakti) for roughly 1000 years, enabling multi-caste religious
 *   communities with radically egalitarian spiritual authority. Structurally,
 *   this reading is TANGLED ROPE: it coordinates genuine devotional practice
 *   across caste boundaries while simultaneously extracting
 *   authority-legitimation from the canonical text itself. The movements that
 *   teach this interpretation gain spiritual authority through textual
 *   warrant (coordination) but also enforce interpretive discipline and
 *   create guru-disciple hierarchies (extraction). The orthodox brahminical
 *   reading, by contrast, interprets dharma as caste-duty and maintains
 *   brahminical gatekeeping of ritual authority — the universalist reading
 *   directly undermines this authority structure, extracting legitimacy from
 *   brahminical institutions. The textual warrant for the universalist
 *   reading is strong (specific Gita passages explicitly support it) but not
 *   total — other passages detail varna-specific duties, creating
 *   hermeneutical tension that any single reading must navigate. The
 *   measuring interval (0-6 units) represents the historical development from
 *   classical Vedantic tradition through the bhakti movements to modern
 *   devotional Hinduism: theater_ratio declines as the universalist reading
 *   matures from innovative reinterpretation to established tradition;
 *   extractiveness and suppression_requirement both decline as brahminical
 *   gatekeeping is overcome and bhakti teaching becomes more widely
 *   accessible.
 *
 * KEY AGENTS:
 *   - Devotee of Lower-Caste Birth: Primary victim under orthodox reading (powerless/identity_locked) — experiences caste barrier to spiritual access; liberation through universalist reading
 *   - Bhakti Movement Organizations: Primary beneficiary and enforcer (organized/constrained) — coordinate egalitarian devotional community; extract interpretive authority through textual warrant and institutional hierarchy
 *   - Brahminical Gatekeeping Authority: Secondary victim (powerful/arbitrage) — loses monopoly on textual interpretation and ritual mediation; experiences extraction of authority-legitimacy
 *   - Orthodox Vedantic Institutions: Constrained middle actor (institutional/constrained) — maintain both readings simultaneously; manage interpretive tension; enforce boundaries between acceptable universalism and heretical egalitarianism
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees sophisticated hermeneutical pluralism; evaluates whether competing readings indicate rich text or unstable kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.52).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.48).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Gita Universalist Devotional Reading: Path-Independent Bhakti and Dharma as Divine Surrender").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__universalist_devotional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, 'e201afb3-99d5-442a-b1fa-b767f55b8de7').
narrative_ontology:cs_kernel_codification('e201afb3-99d5-442a-b1fa-b767f55b8de7', fixed_text).
narrative_ontology:cs_authority_grounding('e201afb3-99d5-442a-b1fa-b767f55b8de7', lineage).
narrative_ontology:cs_interpretation_layer_present('e201afb3-99d5-442a-b1fa-b767f55b8de7').
narrative_ontology:cs_reading_relation('e201afb3-99d5-442a-b1fa-b767f55b8de7', gita_kurukshetra_discourse__orthodox_literal_reading, coexists_with).
narrative_ontology:cs_reading_relation('e201afb3-99d5-442a-b1fa-b767f55b8de7', gita_kurukshetra_discourse__gandhian_allegorical_reading, influences).
narrative_ontology:cs_axiom('e201afb3-99d5-442a-b1fa-b767f55b8de7', foundational, bhakti_universally_accessible).
narrative_ontology:cs_axiom_status(bhakti_universally_accessible, holdable).
narrative_ontology:cs_axiom_grounding('e201afb3-99d5-442a-b1fa-b767f55b8de7', bhakti_universally_accessible, deontological).
narrative_ontology:cs_axiom('e201afb3-99d5-442a-b1fa-b767f55b8de7', foundational, dharma_as_divine_surrender_not_role_duty).
narrative_ontology:cs_axiom_status(dharma_as_divine_surrender_not_role_duty, holdable).
narrative_ontology:cs_axiom_grounding('e201afb3-99d5-442a-b1fa-b767f55b8de7', dharma_as_divine_surrender_not_role_duty, deontological).
narrative_ontology:cs_reference_frame('e201afb3-99d5-442a-b1fa-b767f55b8de7', devotional_universalism_framework).
narrative_ontology:cs_drift_state('e201afb3-99d5-442a-b1fa-b767f55b8de7', contemporary_hinduism, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e201afb3-99d5-442a-b1fa-b767f55b8de7', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, devotee_universal_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_tradition_practitioners).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, non_brahmin_religious_aspirants).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_gatekeeping_authority).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, caste_based_ritual_monopoly).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED LOWER-CASTE DEVOTEE (SNARE) — Under the orthodox literal reading (where dharma = caste-based duty and ritual access is brahminical gatekeeping), a devotee of lower-caste birth faces insurmountable extraction. Their identity cannot leave the caste-based system; their devotional aspiration is structurally locked to an inferior ritual status. The universalist reading dissolves this snare by redefining dharma as surrender to divine will, not social role. From the trapped devotee's perspective, the universalist reading (this constraint) appears as rope or liberation from snare — but this perspective shows what would be snare WITHOUT this reading.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__universalist_devotional_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: BHAKTI PRACTITIONER UNDER UNIVERSALIST READING (ROPE) — For practitioners of the universalist devotional reading, the constraint is pure coordination: the text teaches that bhakti (devotion) is path-independent and accessible to all, dissolving caste barriers. The constraint solves the coordination problem of establishing a devotional community that includes all castes. Low extraction, genuine coordination benefit. The practitioner experiences this reading as enabling rather than constraining.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__universalist_devotional_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: BHAKTI MOVEMENT ORGANIZATION (TANGLED ROPE) — The historically organized bhakti movements (Vaishnavism, Shaivism, Kabir panthis, etc.) that propagate the universalist reading simultaneously coordinate devotional practice across caste boundaries AND extract authority-legitimation from the text itself. The movements enforce devotional discipline, collect resources, manage temple networks, and establish guru-lineage hierarchies — all genuine coordination functions. But they also concentrate interpretive authority, creating dependency on the movement for textual access and spiritual guidance. Active enforcement is required to maintain both the egalitarian doctrine and the hierarchical organization. Constrained by institutional pressures to remain within Hindu (not revolutionary) frameworks while radically redefining caste boundaries.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__universalist_devotional_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: BRAHMINICAL GATEKEEPING AUTHORITY (SNARE) — The classical brahminical authority system that monopolizes ritual access and interprets dharma as caste-duty experiences the universalist reading as a direct extraction mechanism: it loses the monopoly on textual legitimacy and ritual authority. From the brahminical perspective, this reading is a snare because it permanently undermines the structural basis of their power (caste-based duty, ritual gatekeeping). The universalist reading erodes their authority through textual reinterpretation, not through external force — but the extraction of authority-legitimacy is structurally severe and irreversible within the brahminical framework.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__universalist_devotional_reading, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: ORTHODOX VEDANTIC INSTITUTION (TANGLED ROPE) — Traditional Vedantic institutions (Advaita and Dvaita philosophical schools, their associated temples and lineages) that maintain both orthodox and universalist readings simultaneously occupy a constrained middle position. They preserve classical hierarchical authority structures while selectively endorsing devotional universalism. The constraint requires active enforcement: maintaining interpretive boundaries, determining which universalist claims are doctrinally acceptable, managing tension between classical caste-dharma and egalitarian bhakti. Both coordination (connecting lay devotees across boundaries) and extraction (maintaining institutional control over interpretation) are functionally present.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__universalist_devotional_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational hermeneutical perspective, the universalist devotional reading solves a genuine coordination problem: it establishes a framework within which diverse devotional paths (regional, sectarian, caste-crossing) can share a legitimacy claim grounded in a single canonical text. The reading is not a natural law (no access barrier makes devotion impossible from without bhakti universalism) but a sophisticated coordination mechanism that enables multi-caste religious community. Low theater: the reading is internally coherent and logically defensible from the text. High coordination: establishes shared legitimacy for inclusive devotional practice. The analytical observer sees rope — genuine coordination with minimal coercive overhead.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__universalist_devotional_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gita_kurukshetra_discourse__universalist_devotional_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gita_kurukshetra_discourse__universalist_devotional_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The universalist reading generates significant extraction flows: (1) From brahminical authority: the reading directly undermines caste-based gatekeeping by claiming bhakti is universally accessible, extracting legitimacy from the orthodox interpretive monopoly. (2) From lower-caste agents under the orthodox reading: the universalist reading offers liberation but through subordination to new bhakti-movement authority structures — the caste barrier is replaced (not eliminated) by guru-lineage hierarchy. (3) From the text itself: the reading concentrates interpretive authority by claiming certain passages (bhakti verses) override others (varna-dharma verses), imposing hermeneutical hierarchy. The reading is not a pure snare because genuine coordination occurs — bhakti movements do enable multi-caste community and shared devotional practice. But the coordination is inseparable from extraction, justifying tangled_rope classification. Declining trajectory (0.62→0.52) reflects that as the reading becomes established tradition rather than innovative challenge, some extraction mechanisms normalize into routine institutional operations; suppression requirements decrease as the reading's legitimacy is accepted. Suppression (0.48): Moderate. The classical brahminical order requires active suppression of universalist claims: denial of textual warrant, claims that egalitarian readings are modern corruption, enforcement of varna-duties as binding. As bhakti movements grow, they gain sufficient institutional power to reduce suppression requirements — the reading is no longer suppressed but accommodated in modified form (orthodox institutions maintain both readings). The declining trajectory (0.65→0.48) reflects this shift from active suppression to coexistence. Theater ratio (0.38): Moderate-low. The universalist reading, once established, has high internal coherence — the claim that bhakti is universal devotion flows directly from specific Gita verses and requires no performative theater to maintain. Early on (t=0, theater=0.55) the reading involves more theater: innovative reinterpretation requires constant assertion against orthodox opposition, textual selection and emphasis perform the universalism against counter-evidence of varna-passages. As the reading becomes established (t=6, theater=0.38), the theater decreases — the interpretation is taken for granted, textual passages are cited as straightforward evidence rather than bold rereadings. However, theater remains non-trivial because any reading that simultaneously maintains both 'caste-transcending bhakti' AND 'varna-duties exist' requires some performance of coherence.
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives produce five distinct classifications of the same structural phenomenon, exemplifying the diagnostic power of indexed classification. (1) A lower-caste person trapped under orthodox interpretation sees SNARE — the text appears to mandate caste barriers, and identity-lock makes exit unthinkable from within brahminical Hinduism. (2) A bhakti practitioner sees ROPE — the universalist reading solves the coordination problem of multi-caste devotional community with minimal overhead. (3) Organized bhakti movements see TANGLED ROPE — they coordinate genuine devotional practice (rope function) while extracting authority and maintaining hierarchies (extraction). (4) Brahminical authorities see SNARE — the universalist reading extracts their monopoly without offering them equivalent benefits or exit. (5) Orthodox Vedantic institutions see TANGLED ROPE from a different vector — they coordinate tradition-preservation while extracting from both orthodox and universalist devotees through interpretive gatekeeping. (6) The analytical observer sees ROPE — a sophisticated hermeneutical coordination mechanism that solves the problem of canonical textual authority in a pluralistic society. This perspectival range proves that no single classification is 'correct' for the constraint as a whole; the presheaf of perspectives across different observer positions IS the complete structural description.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from its structural position relative to extraction flows. Lower-caste devotees face maximal extraction under the orthodox reading (high d) but partial liberation through the universalist reading (moderate d because new institutional hierarchy replaces caste barrier). Bhakti practitioners experience the universalist reading as coordination (low d, they are the intended beneficiaries). Movement organizations occupy constrained moderate positions (d≈0.50) — they benefit from interpretive authority but face pressure to remain institutionally acceptable and avoid revolutionary implications. Brahminical authorities lose authority (high d, extraction flows from them). Vedantic institutions maintain balanced positions (moderate d) by accommodating both readings. The analytical observer sees the structural pattern itself (d determined by civilizational hermeneutical perspective, not individual power).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is RESOLVED by showing that the constraint is genuinely tangled: it coordinates (enables multi-caste devotional community) AND extracts (concentrates interpretive authority, replaces caste barriers with institutional hierarchies). The temptation is to classify it as pure coordination (rope) because the universalist message is egalitarian, or as pure extraction (snare) because brahminical authority is undermined. Mandatrophy resolution requires acknowledging both functions are real: the bhakti movements that teach this reading do solve genuine coordination problems (enabling shared devotional practice) while also extracting authority-legitimation and imposing new forms of hierarchy. The tangled_rope classification holds across intermediate perspectives (moderate/constrained agents at generational timescale) and captures this irreducible duality. No reclassification to pure rope or snare is warranted unless the structural data changes — either (a) it turns out the universalist reading has no actual textual warrant (extraction without coordination → snare), or (b) new institutional forms emerge that preserve the egalitarian teaching without new hierarchy-concentration (coordination without extraction → rope). The current state is genuinely tangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_warrant_determination,
    'Does the Gita text actually support universalist devotional interpretation, or does this reading impose modern egalitarian values onto a hierarchical classical text?',
    'Close textual analysis of Gita verses on bhakti (2.71, 9.29, 12.8-11, 18.65-66), cross-reference with historical commentary traditions (Shankara, Ramanuja, Madhva), assessment of philological and contextual warrant',
    'If strongly warranted: universalist reading is a legitimate hermeneutical recovery, not innovation. Extraction classification drops to rope across most perspectives. If weakly warranted: reading is modernist reinterpretation; extraction increases as authority-concentration in contemporary interpreters; reclassify as snare from multiple perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_warrant_determination, empirical, 'Whether the Gita text historically supports universalist devotional interpretation').

omega_variable(
    caste_barrier_dissolution_mechanism,
    'If dharma is redefined as ''surrender to divine will not social role,'' what becomes of the Gita''s own dharma discourse in earlier chapters (2.31-37, 3.35, 18.41-44) that explicitly details varna-specific duties?',
    'Hermeneutical reconciliation: does universalist reading dissolve or subordinate the earlier varna-dharma passages? Historical analysis of how bhakti movements handled this textual tension; assessment of whether subordination is a coherent interpretive move or a contradiction requiring suppression',
    'If coherently reconciled: universalist reading is sophisticated and stable. If requiring suppression: extraction increases — the reading depends on silencing competing textual voices. Theater ratio rises as performative moves (universal devotion AND varna-duty) replace integration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(caste_barrier_dissolution_mechanism, conceptual, 'How universalist reading reconciles with Gita''s explicit varna-dharma passages').

omega_variable(
    institutional_authority_preservation,
    'Can bhakti movements authentically teach caste-transcending devotion while maintaining hierarchical guru-lineage authority, or does institutional gatekeeping reconstitute the caste barrier in different form?',
    'Ethnographic and historical analysis: do bhakti movement lineages actually practice caste-transcendence or merely declare it? Study of guru-disciple relations, temple access hierarchies, leadership succession, initiation barriers; assessment of whether new forms of exclusion replace caste-based ones',
    'If truly egalitarian practice: extraction decreases; reading is authentically liberatory. If new gatekeeping emerges: extraction becomes institutional-hierarchy based rather than caste-based; tangled_rope classification holds but with reclassified victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_authority_preservation, empirical, 'Whether bhakti movements actualize caste-transcendence or reinstitute gatekeeping').

omega_variable(
    brahminical_foreclosure_claim,
    'Does the universalist devotional reading logically foreclose the orthodox literal reading, or can both readings coexist as alternative hermeneutical commitments within a single interpretive tradition?',
    'Logical analysis: are the core premises (dharma = caste-duty vs dharma = divine-surrender) mutually exclusive, or can they be held simultaneously (different contexts, different agents, different timeframes)? Historical evidence: have traditional institutions (Vedantic schools, temple networks) maintained both readings simultaneously?',
    'If mutually exclusive: reading_relations = ''forecloses''. If coexistent: reading_relations = ''coexists_with''. This determines the axiom structure and authority_erosion classification for competing readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahminical_foreclosure_claim, conceptual, 'Logical foreclosure relationship between universalist and orthodox readings').

omega_variable(
    kernel_codification_stability,
    'How stable is the Gita text as a kernel? Can it support radically opposed readings (universalist vs orthodox) without the kernel itself collapsing into pure ambiguity?',
    'Literary and philosophical analysis: identify which Gita passages are amenable to multiple readings vs. those that enforce a single interpretation. Assessment of whether the tension between passages is productive (rich interpretive space) or destructive (incoherent text). Historical study of how different reading communities have treated textual authority.',
    'If kernel is robust: both readings can claim warrant; multiple readings indicate sophisticated text, not degraded authority. If kernel is unstable: each reading requires suppression of competing evidence; authority becomes volatile; extractiveness increases across all perspectives as interpretive battles intensify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_stability, conceptual, 'Stability of the Gita kernel under competing devotional and orthodox readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_univ_dev_tr_t0, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(gita_univ_dev_tr_t3, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 3, 0.42).
narrative_ontology:measurement(gita_univ_dev_tr_t6, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(gita_univ_dev_be_t0, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(gita_univ_dev_be_t3, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 3, 0.57).
narrative_ontology:measurement(gita_univ_dev_be_t6, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(gita_univ_dev_su_t0, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(gita_univ_dev_su_t3, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 3, 0.56).
narrative_ontology:measurement(gita_univ_dev_su_t6, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_gatekeeping_authority).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, caste_based_duty_hierarchy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Gita kernel. The sibling readings (orthodox_literal and gandhian_allegorical) are separate constraint stories with their own ε values and perspective structures. They are linked via this network declaration and via cs_structure.reading_relations. Do not conflate them into a single story; each reading is a distinct constraint with distinct beneficiary/victim structures and classification profiles. The shared kernel enables structural comparison across readings: where one reading's extraction decreases, another's increases; where one gains legitimacy, another loses it. The network topology captures this zero-sum hermeneutical competition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__universalist_devotional_reading, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
