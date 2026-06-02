% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__reconstructionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__reconstructionist_reading, []).

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
 *   constraint_id: correct_latin_kernel__reconstructionist_reading
 *   human_readable: Correct Latin as Reconstructionist Authority (Humanist Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The reconstructionist reading of 'correct Latin' constitutes a major
 *   institutional reframing that occurred primarily between the 14th and 16th
 *   centuries, during the European Renaissance and humanist movement. Under
 *   this reading, correct Latin is the classical form recoverable through
 *   rigorous textual criticism applied to ancient manuscripts — primarily
 *   Cicero, Virgil, Livy, and legal/rhetorical texts. Medieval Latin
 *   practitioners, whose language had evolved continuously from late Latin
 *   through centuries of use, are reclassified from 'legitimate Latin
 *   speakers' to 'corruptors of Latin.' This reading demands that users
 *   subordinate living practice to the reconstructed classical standard,
 *   enforced through elite educational institutions (universities,
 *   academies), print culture (standardized editions), and ecclesiastical
 *   authority. The constraint exhibits the classic Tangled Rope signature:
 *   humanist scholars benefit from the authority to certify correctness
 *   (coordination function) while medieval practitioners and the living Latin
 *   tradition bear costs (suppression of their practice as 'incorrect').
 *   Theater ratio has increased over the interval as the reconstruction
 *   ritual (textual apparatus, apparatus critici, conjectural emendation)
 *   became more elaborate and performative, even as modern linguistics
 *   revealed that medieval Latin was systematic variation, not corruption.
 *   The suppression requirement intensified as print culture and standardized
 *   curricula made the reconstructionist standard enforceable across
 *   continental space.
 *
 * KEY AGENTS:
 *   - Humanist Scholars (14th–16th centuries): Primary beneficiaries (institutional/arbitrage) — gain authority through philological expertise to certify Latin correctness; control meaning-making apparatus for classical texts
 *   - Elite Educational Institutions (universities, academies): Beneficiary (institutional/arbitrage) — enforce reconstructionist standard through curricula; gain gatekeeping power over linguistic legitimacy
 *   - Medieval Latin Practitioners (scribes, clergy, scholars): Primary victims (powerless/trapped) — their living practice is reclassified as corrupt; cannot exit without ceasing linguistic practice
 *   - Living Latin Tradition (oral transmission, liturgical use, scholarly continuity): Victim (moderate/constrained) — transmission is delegitimized at each generation; medieval practitioners become 'corruptors'; living practice suppressed in favor of textual standard
 *   - Non-Elite Scribes and Medieval Clergy: Secondary victims (moderate/constrained) — retain some functional autonomy but operate under delegitimized authority; public devaluation of their scribal practice
 *   - Print Culture and Standardization: Enforcement mechanism (institutional/arbitrage) — disseminates reconstructionist standard through editions, grammars, textbooks; makes the standard enforceable at scale
 *   - Reformation-Era Vernacular Advocates: Organized challengers (organized/mobile) — reject reconstructionist Latin hegemony; legitimize alternatives; eventually enable exit into competing language frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__reconstructionist_reading, 0.58).
domain_priors:suppression_score(correct_latin_kernel__reconstructionist_reading, 0.62).
domain_priors:theater_ratio(correct_latin_kernel__reconstructionist_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__reconstructionist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__reconstructionist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__reconstructionist_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__reconstructionist_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__reconstructionist_reading, "Correct Latin as Reconstructionist Authority (Humanist Reading)").
narrative_ontology:topic_domain(correct_latin_kernel__reconstructionist_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__reconstructionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__reconstructionist_reading, '4d5063d5-c585-4907-aed7-61e4438102d1').
narrative_ontology:cs_kernel_codification('4d5063d5-c585-4907-aed7-61e4438102d1', fixed_text).
narrative_ontology:cs_authority_grounding('4d5063d5-c585-4907-aed7-61e4438102d1', lineage).
narrative_ontology:cs_interpretation_layer_present('4d5063d5-c585-4907-aed7-61e4438102d1').
narrative_ontology:cs_reading_relation('4d5063d5-c585-4907-aed7-61e4438102d1', correct_latin_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d5063d5-c585-4907-aed7-61e4438102d1', correct_latin_kernel__reoccupation_reading, influences).
narrative_ontology:cs_axiom('4d5063d5-c585-4907-aed7-61e4438102d1', foundational, classical_form_recoverable_via_textual_criticism).
narrative_ontology:cs_axiom_status(classical_form_recoverable_via_textual_criticism, holdable).
narrative_ontology:cs_axiom_grounding('4d5063d5-c585-4907-aed7-61e4438102d1', classical_form_recoverable_via_textual_criticism, empirically_contingent).
narrative_ontology:cs_axiom('4d5063d5-c585-4907-aed7-61e4438102d1', foundational, elite_philological_authority_legitimate).
narrative_ontology:cs_axiom_status(elite_philological_authority_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('4d5063d5-c585-4907-aed7-61e4438102d1', elite_philological_authority_legitimate, conventional).
narrative_ontology:cs_axiom('4d5063d5-c585-4907-aed7-61e4438102d1', secondary, medieval_practice_constitutes_corruption).
narrative_ontology:cs_axiom_status(medieval_practice_constitutes_corruption, holdable).
narrative_ontology:cs_axiom_grounding('4d5063d5-c585-4907-aed7-61e4438102d1', medieval_practice_constitutes_corruption, empirically_contingent).
narrative_ontology:cs_reference_frame('4d5063d5-c585-4907-aed7-61e4438102d1', classical_textual_authority).
narrative_ontology:cs_drift_state('4d5063d5-c585-4907-aed7-61e4438102d1', contemporary_linguistics, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4d5063d5-c585-4907-aed7-61e4438102d1', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__reconstructionist_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__reconstructionist_reading, humanist_scholars).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__reconstructionist_reading, elite_educational_institutions).
narrative_ontology:constraint_victim(correct_latin_kernel__reconstructionist_reading, medieval_practitioners).
narrative_ontology:constraint_victim(correct_latin_kernel__reconstructionist_reading, living_latin_tradition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIEVAL LATIN SPEAKERS (SNARE) — Structurally trapped: their living practice, once the legitimate continuation of Latin itself, is reclassified as corruption by reconstructionist authority. Cannot exit without ceasing to speak; their linguistic community is delegitimized as 'incorrect' by external textual standard. Maximum extraction: their own language is declared defective.
constraint_indexing:constraint_classification(correct_latin_kernel__reconstructionist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: LIVING LATIN TRADITION (SNARE) — The continuous oral/liturgical/scholarly transmission of Latin through medieval and early modern periods is reclassified as corrupted at each generational step. Suppression through institutional authority (universities, church, print culture) enforces the reconstructionist standard over living practice. High extraction: the tradition's own legitimacy is transferred to a past textual authority.
constraint_indexing:constraint_classification(correct_latin_kernel__reconstructionist_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: NON-ELITE SCRIBES AND CLERGY (TANGLED ROPE) — Face coordination problem (need shared linguistic standard for written communication) but also extraction: their own scribal practice, formerly authoritative, is subordinated to reconstructionist textual criticism as the source of legitimate Latin. Extraction is high but not total because they retain some functional autonomy in monastic and cathedral scriptoria, even if their practice is publicly devalued.
constraint_indexing:constraint_classification(correct_latin_kernel__reconstructionist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HUMANIST SCHOLARS AND ELITE UNIVERSITIES (ROPE) — Net beneficiaries. Gain authority to certify correctness through textual criticism and classical philology. The constraint solves the coordination problem of establishing a shared standard for 'correct' Latin that transcends dialectal and temporal variation. Low suppression from their perspective: they control the authority structure and experience the constraint as enabling rather than coercive.
constraint_indexing:constraint_classification(correct_latin_kernel__reconstructionist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-ENLIGHTENMENT PHILOLOGICAL INSTITUTIONS (PITON) — The reconstructionist standard persists through institutional inertia (university curricula, textual editions, scholarly conventions) even as the original extractive function (excluding medieval practitioners from linguistic legitimacy) has degraded. Theater_ratio high: much philological labor (apparatus critici, manuscript collation) is performative restoration of a historical fiction that medieval Latin was corrupt rather than continuous. Modern linguists understand medieval Latin as systematic variation, not degradation, yet the textual-reconstruction theatrical apparatus persists.
constraint_indexing:constraint_classification(correct_latin_kernel__reconstructionist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, language change is inherent and inevitable: all living languages diverge from ancestral forms, and any attempt to 'recover' a past state is linguistically impossible from the start. Correctness itself is a category error when applied to diachronic language evolution. However, the false summit detector will flag this: the 'natural law of language change' naturalizes a specific institutional arrangement (elite authority to define correctness via textual reconstruction).
constraint_indexing:constraint_classification(correct_latin_kernel__reconstructionist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: REFORMATION-ERA VERNACULAR ADVOCATES (TANGLED ROPE) — Organized agents (reformers, translators, printers) who challenge the reconstructionist hegemony by legitimizing Latin alternatives and eventually vernacular languages. They benefit from the coordination function (establishment of clear linguistic standards) but reject the extractive mechanism (subordination of living practice to elite textual authority). Higher exit options (mobility into competing language legitimacy claims) than medieval clergy. Moderate extraction, genuine coordination function.
constraint_indexing:constraint_classification(correct_latin_kernel__reconstructionist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__reconstructionist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(correct_latin_kernel__reconstructionist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(correct_latin_kernel__reconstructionist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__reconstructionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(correct_latin_kernel__reconstructionist_reading, TR),
    TR >= 0.70.

:- end_tests(correct_latin_kernel__reconstructionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The reconstructionist reading extracts significant value from medieval practitioners and the living tradition by subordinating them to an external textual standard. However, the extraction is not total (snare-level) because the constraint also serves a genuine coordination function — establishing a shared Latin standard enables cross-regional scholarly communication. The extractiveness reflects that the coordination benefit flows primarily to elite institutions while the costs are borne by medieval practitioners. The trajectory shows steady increase over the measurement interval (0.32 → 0.58) as humanist authority consolidated and print culture made enforcement scalable. Suppression (0.62): High. Medieval practitioners face significant structural barriers to exit: ceasing to speak Latin means ceasing participation in written culture, ecclesiastical authority, and scholarly networks. The suppression intensifies over time as the reconstructionist standard becomes institutionally entrenched. However, suppression is not absolute (mountain-level) because the Reformation and vernacular movements create alternative pathways, albeit at high cost. Theater ratio (0.68): High and increasing. The textual-critical apparatus (manuscripts, apparatus critici, conjectural emendation, standardized editions) is substantially performative. Modern linguists understand medieval Latin as systematic variation, not corruption, yet the reconstruction ritual persists through institutional inertia. The increase from 0.35 to 0.68 reflects that as the linguistic justification for the constraint weakened, the theatrical labor (scholarly apparatus) intensified to maintain authority.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the core dynamic of kernel readings: the same structural phenomenon (medieval Latin diverging from classical norms) is classified radically differently depending on perspective. Medieval practitioners see their own language delegitimized (snare). Humanist scholars see a coordination mechanism that enables classical scholarship (rope). Non-elite clergy see mixed coordination and extraction (tangled rope). Modern philologists see a degraded ritual (piton). The living Latin tradition sees pure extraction and loss of legitimacy (snare). Reformation vernacular advocates see an opportunity to exit into competing legitimacy claims (tangled rope). The analytical observer risks seeing an immutable law (mountain) — that language change is inevitable and reconstruction is therefore impossible — but this naturalizes the contingent institutional choice to subordinate living practice to reconstructed authority. The perspectival gap reveals that the 'correct Latin' reading is not a discovery of how Latin actually works; it is an institutional reframing that benefits specific actors and suppresses others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position relative to this specific reconstructionist reading. Humanist scholars and elite institutions occupy the beneficiary role with arbitrage exit options (they can define competing standards but choose not to, gaining authority from monopolizing the reconstruction process); their d ≈ 0.10, producing negative or near-zero χ — they experience the constraint as enabling rather than extractive. Medieval practitioners occupy the victim role with trapped exit (ceasing Latin means ceasing participation in written culture); their d ≈ 0.95, producing maximum f(d) ≈ 1.42 — they experience maximum extraction. Non-elite clergy occupy a mixed position: victims of delegitimization but with some functional autonomy; d ≈ 0.65, moderate extraction. Vernacular advocates have mobile exit options (they can shift to legitimizing vernaculars); d ≈ 0.50, moderate extraction. The piton perspective experiences the constraint as performative because the linguistic justification has degraded while institutional maintenance persists. The analytical mountain perspective risks naturalizing the contingent institutional arrangement as a law of language change — this is diagnosed as a false summit because identifiable beneficiaries (humanist scholars, elite institutions) exist and the constraint requires active enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_standard_vs_living_practice,
    'Is ''correct Latin'' recoverable from texts, or is the reconstructionist standard an ahistorical projection onto medieval practice that never saw itself as corrupted?',
    'Historical analysis of medieval linguistic self-awareness: do medieval manuscripts show scribal awareness of ''error''? Did medieval Latin speakers perceive their practice as divergent from a classical norm? Comparison with modern sociolinguistic accounts of variation.',
    'If medieval practice was self-aware variation: reconstructionist reading is foreclosed (medieval Latin was systematic, not corrupt). If medieval scribes unconsciously perpetuated variation: reconstructionist reading remains holdable (corruption narrative holds). If medieval practice simply continued without reference to classical forms: reconstructionist reading is revealed as imposed retroactively (false summit detected).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_standard_vs_living_practice, empirical, 'Whether medieval Latin practitioners perceived divergence from classical norms as corruption or as natural practice').

omega_variable(
    elite_authority_vs_distributed_legitimacy,
    'Does humanist textual-critical authority derive from demonstrable linguistic superiority, or from elite institutional capture of the linguistic legitimacy market?',
    'Comparative analysis: Does reconstructed classical Latin actually communicate more precisely, unambiguously, or effectively than medieval variants? Can textual criticism identify ''errors'' that medieval practitioners would have recognized as errors? Do medieval texts show internal evidence of systematic ''corruption'' or of systematic variation?',
    'If classical Latin is demonstrably superior: reconstructionist extractiveness is justified as coordination cost. If medieval variants are equally systematic: reconstructionist reading''s claim to correctness is revealed as institutional authority assertion without linguistic warrant (extraction without coordination function). If medieval Latin is superior for some communicative purposes: reconstructionist reading forecloses the continuity reading (both framings cannot coexist).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_authority_vs_distributed_legitimacy, empirical, 'Whether humanist authority derives from linguistic superiority or institutional capture').

omega_variable(
    reading_genealogy_ambiguity,
    'Does the reconstructionist reading reclassify medieval practice as corruption because medieval practice WAS corrupted, or because reconstructionism requires a corruption narrative to establish its own authority?',
    'Genealogical analysis: When does the corruption narrative first appear? What institutional conditions enable it? Does the corruption claim track actual linguistic evidence, or is it layered retroactively to justify humanist institutional authority? Comparison of medieval self-description vs humanist description of the same texts.',
    'If corruption claim is genealogically posterior to humanist institution-building: reading is revealed as legitimacy-seeking discourse (high extractiveness, false summit candidate). If corruption narrative predates humanist authority: reading''s empirical basis is stronger (moderate extractiveness). If the narratives are independent: reading forecloses continuity reading (both cannot hold within a single framework).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_genealogy_ambiguity, conceptual, 'Whether corruption narrative justifies reconstructionist authority or reconstructionism creates the corruption narrative').

omega_variable(
    latin_death_vs_continuous_transformation,
    'Does Latin ''die'' and require reconstruction (reconstructionist premise), or does it continuously transform and remain living through medieval and beyond (continuity reading premise)?',
    'Historical semantic analysis: Can a single unbroken transmission chain be traced from classical through medieval to modern neo-Latin? Do medieval practitioners report speaking a dead language that requires reconstruction, or do they report speaking Latin? Is the death metaphor a reconstructionist invention?',
    'If Latin never died and continuously transformed: reconstructionist reading is foreclosed (logically incompatible with continuity reading in a single framework). If Latin died and was resurrected: reconstructionist reading is strongly held. If the death/life metaphor is institutional choice rather than historical fact: both readings coexist (different parties'' descriptions of the same phenomenon).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(latin_death_vs_continuous_transformation, conceptual, 'Whether Latin died and requires reconstruction or continuously transformed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__reconstructionist_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clatin_recon_tr_t0, correct_latin_kernel__reconstructionist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(clatin_recon_tr_t2, correct_latin_kernel__reconstructionist_reading, theater_ratio, 2, 0.48).
narrative_ontology:measurement(clatin_recon_tr_t4, correct_latin_kernel__reconstructionist_reading, theater_ratio, 4, 0.62).
narrative_ontology:measurement(clatin_recon_tr_t6, correct_latin_kernel__reconstructionist_reading, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(clatin_recon_be_t0, correct_latin_kernel__reconstructionist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clatin_recon_be_t2, correct_latin_kernel__reconstructionist_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(clatin_recon_be_t4, correct_latin_kernel__reconstructionist_reading, base_extractiveness, 4, 0.54).
narrative_ontology:measurement(clatin_recon_be_t6, correct_latin_kernel__reconstructionist_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clatin_recon_su_t0, correct_latin_kernel__reconstructionist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clatin_recon_su_t2, correct_latin_kernel__reconstructionist_reading, suppression_requirement, 2, 0.5).
narrative_ontology:measurement(clatin_recon_su_t4, correct_latin_kernel__reconstructionist_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(clatin_recon_su_t6, correct_latin_kernel__reconstructionist_reading, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__reconstructionist_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin_kernel__reconstructionist_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__reconstructionist_reading, correct_latin_kernel__reoccupation_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__reconstructionist_reading, humanist_philological_authority).
narrative_ontology:affects_constraint(correct_latin_kernel__reconstructionist_reading, medieval_latin_linguistic_legitimacy).

% DUAL FORMULATION NOTE:
% The reconstructionist reading is one constraint story within a family of three competing readings of the 'correct_latin_kernel.' Each reading has its own ε value, beneficiary/victim structure, and classification signature. The family is linked by the shared kernel: all three readings are commitments about how to determine what counts as 'correct' Latin. Each reading's constraint story models the institutional extraction/coordination dynamics that result from adopting that reading as authority. The reconstructionist reading has ε=0.58 (high extractiveness from medieval practitioners); the continuity reading would have lower ε (no corruption narrative, so less extraction); the reoccupation reading would have ε ≤ 0.30 (pure coordination, no hierarchical standard). These are not alternative measurements of one constraint; they are structurally distinct constraints flowing from different kernel readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
