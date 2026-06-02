% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__isaac_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__isaac_covenant_reading, []).

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
 *   constraint_id: abrahamic_covenant__isaac_covenant_reading
 *   human_readable: Abrahamic Covenant — Isaac Reading (Genesis 17:19-21)
 *   domain: religious_studies/institutional_theology
 *
 * SUMMARY:
 *   Genesis 17:19-21 is the locus of the Isaac-covenant reading: 'But my
 *   covenant I will establish with Isaac, whom Sarah will bear to you at this
 *   season next year... And as for Ishmael, I have heard you; behold, I will
 *   bless him and make him fruitful and will multiply him exceedingly. He
 *   shall father twelve princes, and I will make him a great nation. But my
 *   covenant I will establish with Isaac' (ESV). The Isaac-exclusive reading
 *   interprets this passage as limiting the central covenant (berith/diatheke
 *   — the binding institutional framework for God's relationship with
 *   Abraham's descendants) to Isaac's line, while Ishmael receives blessing
 *   and fruitfulness but not the covenant proper. This reading structures
 *   Jewish institutional theology and rabbinic law: the covenant becomes the
 *   foundational legitimacy claim for Jewish peoplehood, written transmission
 *   (Torah), and halakha. It simultaneously excludes Ishmael's descendants
 *   from the primary covenantal relationship. The constraint exhibits all
 *   structural properties of a tangled rope: (1) genuine coordination
 *   function — the reading provides clear institutional identity for Jewish
 *   communities and theological coherence across generations; (2) asymmetric
 *   extraction — the exclusion of Ishmael's line from covenant status creates
 *   a permanent boundary that benefits Jewish institutional authority while
 *   imposing a legitimacy cost on Islamic tradition; (3) active enforcement —
 *   the reading is maintained through institutional theological education,
 *   liturgical practice, and exegetical gatekeeping. The extractiveness has
 *   increased over the historical interval: early rabbinic period (0-500 CE)
 *   treated the Isaac reading as one interpretation among several; medieval
 *   period (500-1000 CE) solidified it as institutional orthodoxy; modern
 *   period onward (1000 CE to present) maintains it through institutional
 *   authority despite scholarly recognition of alternative readings. The
 *   suppression requirement has risen alongside extractiveness as alternative
 *   readings (including Islamic covenant claims) became more organizationally
 *   coherent threats to the reading's monopoly. Theater ratio (0.42) remains
 *   moderate because the reading has genuine exegetical content (Hebrew
 *   grammar, narrative coherence with Genesis 25:19-34 and Exodus 1:1-5)
 *   alongside institutional authority maintenance.
 *
 * KEY AGENTS:
 *   - Jewish Institutional Authority: Primary beneficiary (institutional/arbitrage) — the Isaac reading grounds rabbinic legitimacy, Torah authority, and halakha. Arbitrage exit available through Noahide universalism or covenant rethinking, but institutional path-dependence constrains actual exit.
 *   - Ishmael's Descendants / Islamic Tradition: Primary victim (powerless/identity_locked) — structurally excluded from covenantal status despite Qur'anic affirmation and genealogical claim. Identity fusion with Abrahamic narrative makes exit cognitively impossible from within Islamic theological framework, even though structural mobility exists (alternative readings available).
 *   - Christian Supersessionist Institutions: Secondary beneficiary (institutional/constrained) — benefit from Isaac-exclusive reading enabling substitution claims (Christian covenant replaces Jewish covenant) but face constraint: must assert supersession rather than genealogical continuity.
 *   - Interfaith Dialogue Communities: Mixed actor (moderate/constrained) — benefit from clear textual reference enabling theological negotiation but constrained by reading's genealogical exclusivity limiting possible dialogue outcomes.
 *   - Traditional Exegesis Institutions: Maintenance actor (institutional/arbitrage) — sustain the reading through seminary education, commentary tradition, institutional authority. Piton perspective — reading persists through inertia as much as through exegetical force.
 *   - Analytical Observer: Neutral position (analytical/analytical) — sees constraint as contingent institutional reading coordinating Jewish identity while extracting legitimacy from competing claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, 0.58).
domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, 0.68).
domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__isaac_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__isaac_covenant_reading, "Abrahamic Covenant — Isaac Reading (Genesis 17:19-21)").
narrative_ontology:topic_domain(abrahamic_covenant__isaac_covenant_reading, "religious_studies/institutional_theology").

domain_priors:requires_active_enforcement(abrahamic_covenant__isaac_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__isaac_covenant_reading, '2a83a5a9-f003-4719-bd80-6cd4e6cb9082').
narrative_ontology:cs_kernel_codification('2a83a5a9-f003-4719-bd80-6cd4e6cb9082', fixed_text).
narrative_ontology:cs_authority_grounding('2a83a5a9-f003-4719-bd80-6cd4e6cb9082', lineage).
narrative_ontology:cs_interpretation_layer_present('2a83a5a9-f003-4719-bd80-6cd4e6cb9082').
narrative_ontology:cs_reading_relation('2a83a5a9-f003-4719-bd80-6cd4e6cb9082', abrahamic_covenant__ishmael_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a83a5a9-f003-4719-bd80-6cd4e6cb9082', abrahamic_covenant__supersessionist_reading, influences).
narrative_ontology:cs_axiom('2a83a5a9-f003-4719-bd80-6cd4e6cb9082', foundational, isaac_alone_receives_covenant_berith).
narrative_ontology:cs_axiom_status(isaac_alone_receives_covenant_berith, holdable).
narrative_ontology:cs_axiom_grounding('2a83a5a9-f003-4719-bd80-6cd4e6cb9082', isaac_alone_receives_covenant_berith, empirically_contingent).
narrative_ontology:cs_axiom('2a83a5a9-f003-4719-bd80-6cd4e6cb9082', secondary, covenant_transmissible_through_isaac_lineage_only).
narrative_ontology:cs_axiom_status(covenant_transmissible_through_isaac_lineage_only, holdable).
narrative_ontology:cs_axiom_grounding('2a83a5a9-f003-4719-bd80-6cd4e6cb9082', covenant_transmissible_through_isaac_lineage_only, conventional).
narrative_ontology:cs_reference_frame('2a83a5a9-f003-4719-bd80-6cd4e6cb9082', covenant_through_isaac_exclusively).
narrative_ontology:cs_drift_state('2a83a5a9-f003-4719-bd80-6cd4e6cb9082', contemporary_comparative_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2a83a5a9-f003-4719-bd80-6cd4e6cb9082', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, jewish_institutional_continuity).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, isaac_line_descendants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, ishmael_line_descendants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, islamic_covenant_claims).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, alternative_abrahamic_readings).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISHMAEL'S DESCENDANTS / ISLAMIC TRADITION (SNARE) — Identity-locked to Abrahamic heritage claim through genealogical connection and Qur'anic affirmation (Quran 2:125-129, 19:54-55) but structurally excluded by the Isaac-exclusive reading. High suppression: the reading's textual authority preempts alternative genealogical claims. Structural mobility exists (reinterpret Genesis; claim parallel covenant tradition) but identity fusion with ancestral narrative makes exit cognitively impossible from within the Islamic theological frame. Maximum extraction — bears the full cost of exclusion without agency to revise the textual basis.
constraint_indexing:constraint_classification(abrahamic_covenant__isaac_covenant_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: CHRISTIAN SUPERSESSIONIST INSTITUTIONS (TANGLED ROPE) — Benefit from the Isaac-exclusive reading (supersession claims that Christian covenant replaces Jewish covenant resting on Isaac lineage narrowing). Also face constraint: the reading's textual authority limits their ability to claim direct descent legitimacy; they must assert substitution rather than biological/genealogical continuity. Constrained exit: could adopt non-supersessionist theology but face institutional inertia and doctrinal path-dependence. Mixed coordination (shared Abrahamic narrative frame) and extraction (narrowing legitimacy to exclude Islamic claims enables Christian theological moves).
constraint_indexing:constraint_classification(abrahamic_covenant__isaac_covenant_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: JEWISH INSTITUTIONAL AUTHORITY / RABBINIC TRADITION (ROPE) — Primary beneficiary. The Isaac-exclusive reading grounds institutional Jewish theological identity and legal continuity (halakha derives from Sinai covenant to Isaac's descendants). Arbitrage exit: can selectively adopt universalist readings (Noahide Laws extend covenant to all humanity) while maintaining Isaac-specific institutional structure. The reading functions as coordination mechanism for Jewish communal identity while providing institutional authority for internal governance. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(abrahamic_covenant__isaac_covenant_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERFAITH DIALOGUE COMMUNITIES (TANGLED ROPE) — Benefit from having a clear textual reference point (Genesis 17:19-21) that enables sophisticated theological negotiation across traditions. Constrained exit: adopting the Isaac-exclusive reading enables some conversations but forecloses others (direct Ishmael legitimacy claims). The reading creates both coordination function (shared textual anchor) and extraction (limits possible theological outcomes by pre-adjudicating genealogical exclusion).
constraint_indexing:constraint_classification(abrahamic_covenant__isaac_covenant_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: TRADITIONAL TEXTUAL EXEGESIS INSTITUTIONS (PITON) — Maintain the Isaac-exclusive reading through institutional inertia despite substantial contemporary textual scholarship recognizing Genesis 17:19-21 as one reading among contested interpretive traditions. Theater ratio (0.42) reflects genuine exegetical content (grammatical analysis of Hebrew, narrative coherence with Abraham's genealogy in Genesis 25:19-34) alongside performative institutional authority maintenance. The reading persists because institutional credibility and inheritance structures (seminaries, yeshivas, theological academies) are built on it, not primarily because contemporary exegesis reaffirms its exclusivity. Exit options exist (adopt non-exclusive reading) but require institutional reorganization.
constraint_indexing:constraint_classification(abrahamic_covenant__isaac_covenant_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COMPARATIVE TEXTUAL ANALYSIS (TANGLED ROPE) — The Isaac-exclusive reading exhibits both coordination (provides clear textual anchor for Jewish institutional identity) and extraction (legitimizes exclusion of Ishmael-line claimants from the covenant narrative). From an analytical civilizational perspective, the constraint is neither immutable natural law nor pure extraction — it is a historical institutional reading that coordinates Jewish identity while extracting legitimacy from competing genealogical claims. The analytical observer sees the constraint as contingent on interpretive choice, but the choice is now embedded in centuries of institutional practice.
constraint_indexing:constraint_classification(abrahamic_covenant__isaac_covenant_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__isaac_covenant_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(abrahamic_covenant__isaac_covenant_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(abrahamic_covenant__isaac_covenant_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, TR),
    TR >= 0.70.

:- end_tests(abrahamic_covenant__isaac_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The Isaac-exclusive reading benefits Jewish institutional authority substantially (covenant foundation for Jewish law and identity) and imposes legitimacy costs on Islamic claims to Abrahamic heritage. The extraction is not maximal (0.85+) because Jewish institutions also accept some universalist covenant interpretations (Noahide Laws, ethical monotheism extending covenant principles to all humanity) that partially mitigate exclusionary force. The reading has genuine coordination function alongside extraction — it provides institutional coherence that is valued by its beneficiaries. Suppression (0.68): Moderate-high. The reading is maintained through institutional gatekeeping (seminary curricula, theological authority), textual authority claims (Genesis 17:19-21 interpreted as definitive), and the identity fusion of target populations with Abrahamic narratives that make exit cognitively costly. Suppression is not total (0.85+) because alternative readings exist in contemporary scholarship and some interfaith movements explicitly challenge the exclusivity. Theater ratio (0.42): Moderate. The reading has genuine exegetical content — Hebrew grammatical analysis of the passage, narrative coherence with surrounding Genesis genealogies, and integration with rabbinic legal theory. But the reading is also maintained through institutional authority and inheritance (yeshiva and seminary training perpetuates it) in ways that exceed what contemporary textual evidence requires. The ratio reflects this mix: meaningful exegesis alongside institutional maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The Ishmael line (Islamic perspective) experiences snare classification because they are identity-locked to the Abrahamic narrative but structurally excluded by the reading's authority. The reading appears immutable from within the Islamic theological frame because challenging it requires revising core genealogical identity — exit is structurally possible but identity-fused agents cannot see or exercise it. Jewish institutional authority experiences rope classification because they are net beneficiaries and the reading coordinates their identity and governance. Christian supersessionists experience tangled rope because they benefit from the reading (enabling replacement theology) but are also constrained by it (cannot claim genealogical continuity, must assert substitution). Interfaith communities also experience tangled rope — the reading enables some conversations (shared textual anchor) while foreclosing others (Ishmael legitimacy). Traditional exegesis institutions experience piton classification because they maintain the reading partly through genuine exegetical argument but also partly through institutional inertia. The analytical observer experiences tangled rope classification because the reading exhibits both genuine coordination function (Jewish institutional coherence) and extractive suppression (genealogical exclusion). The perspectival gap is maximal between the beneficiary (rope: coordination function) and the victim (snare: trapped exclusion).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural position. Jewish institutional authority as beneficiary with arbitrage exit derives low d (approximately 0.15), producing negative chi (institutional power scale: -0.12 per canonical fallback). Ishmael descendants as victims with identity_locked exit derive high d (approximately 0.89), producing high chi (1.28 per canonical). The gap between these directional values (0.15 to 0.89) is the core perspectival structure: one agent experiences the constraint as beneficial coordination, the other as coercive exclusion. Christian supersessionists as secondary beneficiaries with constrained exit derive moderate d (approximately 0.40), producing moderate chi (0.40 per canonical). Interfaith dialogue communities as moderate power with constrained exit derive moderate d (approximately 0.65), producing moderate chi (1.00 per canonical). The piton perspective (exegesis institutions with arbitrage exit) derives low d, making the piton classification structural rather than from high experienced extraction. This confirms the theater-ratio gate: piton derives from theater ≥ 0.70 OR from low chi + institutional persistence, not from high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the Isaac-exclusive reading is a contested kernel reading, not a settled fact. The beneficiary perspective (rope) and victim perspective (snare) represent two legitimate structural positions from the same textual base. The analytical observer's tangled-rope classification indicates that both coordination and extraction are genuine — the reading does coordinate Jewish identity AND it does exclude competing claims. The mandatrophy dissolves when the committer structure is made explicit: this is ONE reading of the Abrahamic covenant kernel, not THE reading. Alternative readings (ishmael_covenant_reading, supersessionist_reading) would emit different classifications from the same base material. The fact that all six constraint types are accessible from different perspectives (snare, tangled_rope, rope, piton) reveals that the true structure is a contested kernel with multiple live readings, not a single constraint with observer-relative classifications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_reference_scope,
    'Does Genesis 17:19-21 intend eternal exclusion of Ishmael''s line from covenant, or does it designate primary covenantal authority while leaving secondary or derived covenantal status ambiguous?',
    'Comparative analysis of all Abrahamic covenant passages (Gen 12:1-3, Gen 15:1-21, Gen 17:1-27, Gen 21:11-13, Gen 26:24); identification of whether any passages extend covenant protections to Ishmael or his descendants; analysis of Hebrew grammatical scope markers (את vs. אל constructions); cross-traditional exegetical comparison (Jewish, Christian, Islamic interpretive lineages)',
    'If primary-only: Isaac reading is structurally sound and extraction mechanism is intentional institutional boundary-setting. If secondary-status possible: reading becomes one option among live alternatives and the constraint reclassifies toward rope (coordination without exclusionary extraction). If Ishmael explicitly included: Isaac reading forecloses and Islamic reading becomes dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_reference_scope, empirical, 'Semantic scope of Genesis 17:19-21 regarding Ishmael''s covenantal status').

omega_variable(
    institutional_authority_grounding,
    'Is the Isaac-exclusive reading maintained primarily because textual evidence supports it, or primarily because institutional authority structures (rabbinic tradition, theological continuity, institutional inheritance) are built on it?',
    'Historical analysis of exegetical tradition: tracking which readings were available at each period (medieval, early modern, modern scholarship); analysis of when alternatives were rejected and on what grounds; comparison of argumentative force in contemporary biblical scholarship vs. institutional gatekeeping; survey of institutional incentive structures (what institutional actors benefit from maintaining the reading vs. adopting alternatives)',
    'If textual evidence dominant: constraint is epistemically grounded and theater ratio should decrease. If institutional authority dominant: constraint is better classified as piton (theatrical maintenance through inertia) than tangled rope, despite current theater ratio (0.42). This distinction determines whether the constraint has genuine coordination function or merely performative authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_grounding, empirical, 'Whether the reading is grounded in textual evidence or institutional authority maintenance').

omega_variable(
    islamic_covenant_independence,
    'To what extent is the Islamic covenant tradition (Qur''anic affirmation of Abraham, Ishmael, and Mecca) an independent theological development vs. a reactive response to Jewish Isaac-exclusive reading?',
    'Chronological analysis of Qur''anic compositional layers and Islamic exegetical emergence; comparison with pre-Islamic Arabian genealogical traditions; analysis of whether Islamic covenant theology could have developed identically without the Jewish Isaac-exclusive reading as a foil or constraint; examination of Islamic internal sources (hadith, tafsir) for the logic of Ishmael inclusion',
    'If independent: Islamic covenant claim has its own epistemic legitimacy and the constraint represents genuine theological competition. If reactive: the Isaac-exclusive reading''s extractiveness derives from its capacity to pre-emptively exclude rival claims, making suppression mechanism more coercive. If evidence is mixed: the reading exhibits both independent theological logic and reactive suppression, supporting the tangled-rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(islamic_covenant_independence, empirical, 'Independence and genealogy of Islamic covenant claims relative to Jewish readings').

omega_variable(
    kernel_reading_ambiguity,
    'Is Genesis 17:19-21 the kernel text for Abrahamic covenant legitimacy, or is it one interpretation of a broader covenant kernel that includes Genesis 12:1-3 and other passages?',
    'Mapping of which passages are cited as foundational in Jewish, Christian, and Islamic theological traditions; analysis of whether the Isaac-exclusive reading is necessary to the concept of ''Abrahamic covenant'' or contingent on selecting Genesis 17:19-21 as the primary reference; examination of whether non-exclusive readings could maintain institutional Jewish identity while widening covenant scope',
    'If Genesis 17:19-21 is THE kernel: the Isaac reading is constitutive and alternatives are genuine challenges to the kernel itself. If it is one reading of a broader kernel: the Isaac reading is one interpretive option, the constraint''s extractiveness derives from institutional gatekeeping rather than textual necessity, and the constraint is vulnerable to denaturalization. This determination affects whether the constraint should be modeled as one story (if kernel is singular) or as part of a kernel family with multiple reading stories (if kernel is ambiguous).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether Genesis 17:19-21 is the foundational kernel or one interpretation of a broader covenant concept').

omega_variable(
    suppression_mechanism_coercion_level,
    'What is the primary mechanism by which the Isaac-exclusive reading suppresses alternative covenant claims: textual authority closure, institutional gatekeeping, theological foreclosure of alternative framings, or internalized identity fusion that makes alternatives cognitively unavailable?',
    'Analysis of how the reading is taught and maintained in institutional settings (seminary, yeshiva, mosque, church); examination of whether scholars holding alternative views face institutional penalties; assessment of whether the reading is presented as settled fact vs. one defensible interpretation; interview or textual analysis of how agents from excluded traditions describe their relationship to the reading (do they see it as logically foreclosed or institutionally excluded?)',
    'If primarily textual closure: suppression is epistemically grounded and legitimate within exegetical frameworks. If primarily institutional gatekeeping: suppression is coercive and the constraint''s extractiveness is overstated relative to genuine theological disagreement. If identity fusion: the suppression is internalized and appears as inevitable rather than enforced (true for identity-locked agents). Accurate mechanism identification refines the suppression metric and directionality derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_coercion_level, empirical, 'Primary suppression mechanism maintaining the Isaac-exclusive reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__isaac_covenant_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isaac_cov_tr_t0, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(isaac_cov_tr_t500, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 500, 0.42).
narrative_ontology:measurement(isaac_cov_tr_t1000, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1000, 0.42).

% Extraction over time
narrative_ontology:measurement(isaac_cov_be_t0, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(isaac_cov_be_t500, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 500, 0.55).
narrative_ontology:measurement(isaac_cov_be_t1000, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1000, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(isaac_cov_su_t0, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(isaac_cov_su_t500, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 500, 0.65).
narrative_ontology:measurement(isaac_cov_su_t1000, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1000, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__isaac_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__supersessionist_reading).

% DUAL FORMULATION NOTE:
% The Abrahamic covenant is a contested kernel with three structurally distinct readings, each instantiating a different constraint with different epsilon values. The Isaac reading (this story) has epsilon=0.58 (tangled rope: coordination + extraction). The Ishmael reading would have different epsilon reflecting Islamic covenant theology's institutional benefits and victim sets. The supersessionist reading would have different epsilon reflecting Christian theological institutional structures. All three stories are linked via network.affects_constraints because they represent readings of the same kernel — changing one reading creates downstream pressure on the others through theological legitimacy competition and institutional authority redistribution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
