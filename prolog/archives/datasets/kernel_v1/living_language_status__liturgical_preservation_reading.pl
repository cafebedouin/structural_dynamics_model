% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__liturgical_preservation_reading, []).

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
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Living Language Status: Liturgical Preservation Reading
 *   domain: sociolinguistics/religious_studies/nationalism
 *
 * SUMMARY:
 *   The liturgical preservation reading of living language status claims that
 *   a language remains alive if its sacred texts (Torah, Talmud, liturgical
 *   corpus) are continuously recited, studied, and used in ritual
 *   performance, even if no native generation of speakers uses the language
 *   for daily communication. This reading instantiates one axis of a
 *   multi-century dispute about what constitutes linguistic vitality. Under
 *   this reading, Hebrew remained a 'living language' throughout the Diaspora
 *   period (500 CE–1880 CE) despite the fact that no community raised
 *   children with Hebrew as a first language — the liturgical corpus itself,
 *   transmitted through rabbinical schools and prayer services, sufficed to
 *   preserve the language as a functional system. The constraint exhibits
 *   tangled_rope structure: genuine coordination function (rabbinical
 *   authority, liturgical institutions, and observant communities coordinate
 *   around the fixed corpus) combined with asymmetric extraction (the secular
 *   speech community is delegitimized, and linguistic innovation capacity is
 *   suppressed in favor of custodial repetition). The theater ratio has
 *   increased over the measured interval (0.35 → 0.58) as living liturgical
 *   transmission has shifted toward conscious institutional preservation and
 *   textual scholarship — the shift from embedding-in-community to
 *   embedding-in-institution increases performative overhead. This constraint
 *   is ONE reading of the kernel 'living language status'; the sibling
 *   readings (native_generation and literary_continuity) contest this
 *   definition and would produce different beneficiary/victim structures and
 *   different ε values. The false summit candidate flag arises because this
 *   reading naturalizes institutional authority (rabbinical interpretation
 *   monopoly) as if it were a law of language rather than a contingent claim
 *   about what makes a language 'living.'
 *
 * KEY AGENTS:
 *   - Rabbinical Authority: Primary beneficiary (institutional/arbitrage) — their interpretive monopoly is preserved and their institutional role is central to the reading's mechanism
 *   - Liturgical Institution (synagogues, yeshivas, ritual communities): Secondary beneficiary (institutional/constrained) — their preservation function is legitimized by the reading
 *   - Secular Speech Community: Primary victim (powerless/identity_locked) — delegitimized as desecrators of the sacred corpus; their creative linguistic use is suppressed
 *   - Vernacular Innovation Capacity: Secondary victim (analytical/analytical) — new linguistic forms and colloquial developments are devalued as inauthentic
 *   - Modern Hebrew Literary Community: Mixed (organized/constrained) — benefits from the linguistic depth of the corpus but constrained by the reading's authority claim that vitality means preservation, not innovation
 *   - Diaspora Bilingual Communities: Mixed (organized/mobile) — experience the reading as both identity anchor and coordination problem (how to transmit when daily life uses another language?)
 *   - Native Speaker Advocates (competing reading): Organized challenge to the reading's authority — argue that only generational transmission constitutes 'living' language
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.32).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.48).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Living Language Status: Liturgical Preservation Reading").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, 'a4808ddb-440d-41c9-bfad-ef56e47d4a9a').
narrative_ontology:cs_kernel_codification('a4808ddb-440d-41c9-bfad-ef56e47d4a9a', formalized).
narrative_ontology:cs_authority_grounding('a4808ddb-440d-41c9-bfad-ef56e47d4a9a', extraction).
narrative_ontology:cs_interpretation_layer_present('a4808ddb-440d-41c9-bfad-ef56e47d4a9a').
narrative_ontology:cs_reading_relation('a4808ddb-440d-41c9-bfad-ef56e47d4a9a', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('a4808ddb-440d-41c9-bfad-ef56e47d4a9a', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('a4808ddb-440d-41c9-bfad-ef56e47d4a9a', foundational, liturgical_preservation_suffices_for_vitality).
narrative_ontology:cs_axiom_status(liturgical_preservation_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('a4808ddb-440d-41c9-bfad-ef56e47d4a9a', liturgical_preservation_suffices_for_vitality, conventional).
narrative_ontology:cs_axiom('a4808ddb-440d-41c9-bfad-ef56e47d4a9a', foundational, native_generation_transmission_not_required).
narrative_ontology:cs_axiom_status(native_generation_transmission_not_required, holdable).
narrative_ontology:cs_axiom_grounding('a4808ddb-440d-41c9-bfad-ef56e47d4a9a', native_generation_transmission_not_required, conventional).
narrative_ontology:cs_reference_frame('a4808ddb-440d-41c9-bfad-ef56e47d4a9a', diaspora_liturgical_monolingualism).
narrative_ontology:cs_drift_state('a4808ddb-440d-41c9-bfad-ef56e47d4a9a', modern_hebrew_revival_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a4808ddb-440d-41c9-bfad-ef56e47d4a9a', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, liturgical_institution).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, vernacular_innovation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECULAR HEBREW SPEAKER (SNARE) — Native speakers of modern colloquial Hebrew outside liturgical contexts face structural delegitimation under this reading: their speech is dismissed as inauthentic, a corruption of the sacred corpus. Exit is identity-locked — Hebrew identity itself has become fused with the liturgical standard, making secular speech feel like desecration. The speaker bears full extraction cost: their creative linguistic agency is suppressed in favor of custodial repetition of fixed texts.
constraint_indexing:constraint_classification(living_language_status__liturgical_preservation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: MODERN HEBREW LITERARY COMMUNITY (TANGLED ROPE) — Organized agents producing new Hebrew literature (novelists, poets, essayists) experience genuine coordination through the shared corpus — they draw legitimacy and linguistic depth from the liturgical tradition — but are constrained by the reading's authority claim that vitality means liturgical preservation, not innovation. They benefit from the linguistic infrastructure the reading maintains; they are extracted from by having their creative work devalued as non-vital.
constraint_indexing:constraint_classification(living_language_status__liturgical_preservation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RABBINICAL AUTHORITY (ROPE) — The institutionalized interpreters of sacred texts experience this reading as pure coordination: their role is to maintain the liturgical corpus, transmit it faithfully, and adjudicate disputes about correct recitation and interpretation. Their authority depends entirely on the corpus being fixed and their interpretive monopoly being recognized. The constraint coordinates the community's ritual practice while preserving rabbinical institutional power.
constraint_indexing:constraint_classification(living_language_status__liturgical_preservation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LITURGICAL INSTITUTIONAL ARCHIVE (PITON) — Viewed from the longue durée, the preservation function is substantially theatrical: maintaining the liturgical corpus requires elaborate institutional machinery (rabbinic schools, ritual specialists, textual commentary), but much of this infrastructure is now maintained through deliberate performative effort rather than organic community practice. The theater ratio reflects the gap between authentic liturgical transmission (which was once generative, embedding native speakers) and contemporary institutional preservation (which requires deliberate enforcement and textual scholarship to maintain the corpus against assimilation and language death).
constraint_indexing:constraint_classification(living_language_status__liturgical_preservation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From the civilizational/universal perspective, the reading appears to identify an immutable feature of language: that texts, once fixed and sacralized, can preserve a language-system indefinitely through custodial repetition, independent of whether any native speaker generation adopts it. This perspective risks naturalizing what is actually a contested institutional claim about what constitutes 'living' language. The engine will flag this as a false summit candidate due to the declared beneficiaries (rabbinical authority has clear institutional interests in the fixed corpus).
constraint_indexing:constraint_classification(living_language_status__liturgical_preservation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: DIASPORA COMMUNITY — BILINGUAL PERSPECTIVE (TANGLED ROPE) — Diaspora Jews in multilingual contexts experience this reading as a source of linguistic legitimacy (the liturgical corpus preserves group identity and provides linguistic authority) while also facing genuine coordination problems: how to transmit liturgical Hebrew when community life is conducted in another language? This perspective has higher exit mobility than the secular native speaker — the community can choose the degree to which it privileges the liturgical reading — but faces extraction through the delegitimation of their hybrid linguistic practice.
constraint_indexing:constraint_classification(living_language_status__liturgical_preservation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__liturgical_preservation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(living_language_status__liturgical_preservation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(living_language_status__liturgical_preservation_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, TR),
    TR >= 0.70.

:- end_tests(living_language_status__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Low-moderate. The liturgical preservation reading coordinates genuine community practice (liturgical recitation, textual study, ritual use) and preserves a language-system within institutional structures. The coordination function is real. However, extraction exists: the secular speech community's linguistic creativity is suppressed, and institutional authority over language definition is concentrated in rabbinical hands. The ε value reflects the balance between coordination and extraction — higher than pure rope (which would be ε ≤ 0.35) because of the suppression of alternatives, but lower than snare (ε ≥ 0.46) because the coordination function is genuine and the mechanism is not primarily coercive extraction. Suppression (0.48): Moderate-high. Secular speech is delegitimized through authority claims; speakers are discouraged from innovation; alternative definitions of 'living language' are suppressed through institutional gatekeeping. However, suppression is not total — secular Hebrew has thrived despite (and sometimes through) the reading's authority claims. Theater ratio (0.58): Moderate-high. Liturgical transmission was once embedded in lived community practice; it is now increasingly maintained through deliberate institutional effort, textual scholarship, and conscious preservation rather than organic adoption. The rise over the interval (0.35 → 0.58) reflects the historical shift from the Diaspora period (when liturgical Hebrew was the only formal register available and community practice sustained it) to the modern period (when secular Hebrew competes and institutional machinery must work harder to maintain the liturgical standard). The measurements show gradual extractiveness and suppression accumulation as the community shifted from liturgical monolingualism to Hebrew bilingualism (liturgical + secular).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is fundamental and irresolvable within the single-constraint analysis. The rabbinical authority and liturgical institution see rope (pure coordination of the sacred corpus). The secular speaker sees snare (delegitimization and suppression). The modern literary community sees tangled_rope (coordination + extraction). The diaspora bilingual community sees tangled_rope (benefits and constraints). The analytical observer at civilizational timescale risks seeing mountain (naturalizing institutional authority as if it were a law of language). These gaps do not converge — they reflect the fact that different parties hold different readings of the kernel 'living language status,' and each reading privileges a different mechanism as constitutive of vitality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Rabbinical authority: beneficiary + arbitrage exit → low d (~0.15) → negative effective extraction (they benefit from the constraint). Secular speaker: victim + identity_locked exit → high d (~0.89) → high f(d) → high experienced extraction (they bear costs and cannot cognitively exit). Literary community: mix of beneficiary and victim + constrained exit → moderate d (~0.50) → moderate experienced extraction. The engine's derivation chain computes d automatically from these structural facts. The directionality_override mechanism is not needed here — the structural data is sufficient. The key analytical move is recognizing that the secular speaker's identity_locked exit is crucial to the snare classification from their perspective: they are not merely constrained by external barriers (which would make the exit `constrained`), but by having internalized the reading's authority claim that their speech is inauthentic — the binding mechanism is cognitive rather than structural.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this constraint is ONE reading of a contested kernel. The 'mandatrophy' (coordination-vs-extraction ambiguity) persists because different parties genuinely see different structures in the same linguistic phenomenon. The rabbinical authority sees coordination (their role is to transmit the corpus faithfully). The secular speaker sees extraction (their linguistic creativity is suppressed). The modern literary community sees tangled_rope (both coordination and extraction). The resolution is not to choose one classification as 'correct' but to recognize that the classification depends on which reading of 'living language' is accepted. If the liturgical preservation reading is accepted, tangled_rope is the classification (coordination of the corpus + extraction from the secular community). If the native generation reading is accepted, the liturgical institution appears as a snare (artificial preservation mechanism suppressing natural language evolution). If the literary continuity reading is accepted, the liturgical preservation appears as a piton (performative preservation that degrades linguistic vitality). The mandatrophy is structural — it reflects the fact that the parties are not disagreeing about facts but about what constitutes 'language vitality' itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_sufficiency_empirical,
    'Does liturgical preservation without native speaker transmission actually maintain a language-system viable for new expression, or does it preserve only a fixed corpus?',
    'Diachronic linguistic analysis: measure the productivity of morphosyntactic rules, lexical innovation, and semantic drift in texts produced by liturgically-trained speakers vs. native speakers. Historical case studies: Sanskrit (ritual-only, ≥2000 years, no linguistic evolution), ecclesiastical Latin (≤1000 years, minimal drift), rabbinic Hebrew (continuous 2000+ years, measurable drift despite liturgical fixation).',
    'If liturgical preservation maintains full productivity: this reading''s coordination function is genuine (tangled_rope sustained). If productivity decays: the preserved corpus is a text artifact, not a living language, and the reading naturalizes institutional authority over empirical language vitality (false summit confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liturgical_sufficiency_empirical, empirical, 'Whether liturgical preservation maintains linguistic productivity or merely preserves a fixed corpus').

omega_variable(
    reading_committer_ambiguity,
    'Which reading of ''living language status'' is being instantiated here: the liturgical_preservation_reading, or a competing reading (native_generation or literary_continuity)? What is the kernel that these readings contest?',
    'Recognize this constraint as one reading of a contested kernel. The kernel is the implicit claim ''what constitutes a living language?'' Three readings coexist: (1) liturgical preservation suffices (this reading), (2) native speaker transmission is necessary (native_generation_reading), (3) productive literary use is sufficient (literary_continuity_reading). These readings compete for institutional authority over the definition of language vitality. The committer frame acknowledges the contest explicitly via cs_structure.reading_relations and cs_structure.axioms.',
    'Resolving this omega requires stepping outside the single-constraint analysis. The three readings form a constraint family. The ''resolution'' is recognizing that different parties (rabbinical authority, secular modernists, native speaker communities) hold different readings, and no single reading is empirically correct — each reading privileges different linguistic mechanisms. The classification as tangled_rope + false summit candidate captures the structure: this reading coordinates genuine liturgical practice while extracting legitimacy from the secular speech community.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'That this constraint is one reading of a contested kernel; the kernel defines what constitutes language vitality').

omega_variable(
    identity_lock_mechanism_interpersonal,
    'Is the identity_locked exit option for the secular speaker driven by internalized delegitimation (cognitive capture) or by external enforcement (career/social penalties for non-liturgical speech)?',
    'Ethnographic/interview-based: distinguish between speakers who avoid non-liturgical Hebrew because they have internalized the liturgical standard as authentic and feel their secular speech is ''wrong'' (identity lock) vs. speakers who use secular Hebrew fluently but face social penalties from authority figures (constrained exit). Measure via: (a) private speech behavior vs public speech behavior; (b) speaker attitudes toward their own non-liturgical Hebrew; (c) observable penalties for code-switching; (d) transmission patterns to children (internalized speakers suppress non-liturgical Hebrew even in child-directed speech; constrained speakers allow it).',
    'If primarily identity-locked: the constraint''s suppression mechanism is cognitive (internalized authority); reclassify from snare toward rope (suppression is self-imposed coordination filter). If primarily constrained: suppression is external (career, community sanctions); classification remains snare. Mixed case: both mechanisms present — suppression is partially structural, partially internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpersonal, empirical, 'Whether suppression of secular speech is internalized (identity-locked) or externally enforced (constrained exit)').

omega_variable(
    institutional_monopoly_extraction,
    'Does the rabbinical authority genuinely benefit from the liturgical preservation reading, or is the reading simply the institutional sedimentation of genuine language-preservation needs?',
    'Institutional history analysis: (1) Track rabbinical authority''s decision-making power over language definitions in historical debates (Mishnaic era, Talmudic era, medieval period, modern period). (2) Measure: does rabbinical authority gain institutional power or resources when the liturgical preservation reading is adopted vs. when native speaker vitality is the criterion? (3) Comparative: how do rabbinical authority interests differ across readings? Under native_generation_reading, rabbinical authority has no special status. Under literary_continuity_reading, literary innovators and scholars gain authority. Under liturgical_preservation_reading, textual interpreters gain monopoly. (4) Cost-benefit: what would change if rabbinical authority lost control over the language definition? Would their institutional role be preserved or collapsed?',
    'If genuine extraction: the rabbinical authority is a beneficiary with active stakes in preserving the liturgical reading; the reading is tangled_rope with false summit risk (naturalized institutional interest). If institutional sedimentation only: the reading is rope (legitimate coordination of preservation function without extractive advantage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_monopoly_extraction, empirical, 'Whether rabbinical institutional authority genuinely benefits from the liturgical preservation reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(litur_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(litur_tr_t200, living_language_status__liturgical_preservation_reading, theater_ratio, 200, 0.48).
narrative_ontology:measurement(litur_tr_t400, living_language_status__liturgical_preservation_reading, theater_ratio, 400, 0.58).

% Extraction over time
narrative_ontology:measurement(litur_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(litur_be_t200, living_language_status__liturgical_preservation_reading, base_extractiveness, 200, 0.25).
narrative_ontology:measurement(litur_be_t400, living_language_status__liturgical_preservation_reading, base_extractiveness, 400, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(litur_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(litur_su_t200, living_language_status__liturgical_preservation_reading, suppression_requirement, 200, 0.42).
narrative_ontology:measurement(litur_su_t400, living_language_status__liturgical_preservation_reading, suppression_requirement, 400, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, hebrew_language_revival_institutional_machinery).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, diaspora_linguistic_assimilation_resistance).

% DUAL FORMULATION NOTE:
% The living_language_status kernel decomposes into three structurally distinct readings: (1) liturgical_preservation_reading (this constraint, ε≈0.32, tangled_rope); (2) native_generation_reading (ε≈0.40, snare for liturgical institutions); (3) literary_continuity_reading (ε≈0.28, rope with asymmetric benefit). The three readings have different ε values, different beneficiary/victim structures, and competing authority claims. They are NOT observables of one constraint but separate constraints in the same kernel family. Each reading links to specific institutional interests and historical discourse traditions. The false summit candidate flag indicates that the liturgical_preservation_reading risks naturalizing rabbinical institutional authority as if it were a law of language vitality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
