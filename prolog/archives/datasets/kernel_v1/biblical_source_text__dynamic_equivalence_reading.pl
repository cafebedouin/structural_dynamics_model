% ============================================================================
% CONSTRAINT STORY: biblical_source_text__dynamic_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__dynamic_equivalence_reading, []).

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
 *   constraint_id: biblical_source_text__dynamic_equivalence_reading
 *   human_readable: Dynamic Equivalence Reading: Communicative Effectiveness as Primary Authority
 *   domain: biblical_studies/translation_theory/religious_authority
 *
 * SUMMARY:
 *   The dynamic equivalence reading of biblical source text authority
 *   represents a sustained institutional commitment to prioritizing
 *   communicative effectiveness and pastoral intelligibility over
 *   morphological source-text fidelity. This constraint emerges from a
 *   specific theological hermeneutical choice: that the meaning of Scripture
 *   is its impact on contemporary lay readers, not the historical-linguistic
 *   precision of its original morphology. The constraint is enforced through
 *   translation committees (NIV, NCV, Message Bible), denominational
 *   endorsement, and publishing gatekeeping that privileges dynamic
 *   equivalence translations in churches and seminaries. The extractiveness
 *   derives from the suppression of competing hermeneutical approaches
 *   (formal equivalence, critical-historical reconstruction) and the
 *   structural entrapment of lay readers in translator-mediated
 *   interpretation. However, the constraint also provides genuine
 *   coordination function: making biblical text accessible to non-specialist
 *   communities and enabling pastoral mission work across language and
 *   cultural barriers. This hybrid character makes it a canonical Tangled
 *   Rope — legitimate coordination function paired with asymmetric extraction
 *   of interpretive authority from lay readers and academic textual scholars.
 *
 * KEY AGENTS:
 *   - Lay Reader Communities: Primary beneficiary + victim (powerless/trapped) — gain accessible biblical text but are linguistically entrapped in translator's interpretive frame; cannot independently verify source-text meaning
 *   - Missionary Organizations: Primary institutional beneficiary (powerful/arbitrage) — benefit from reduced communicative friction; core mission alignment with dynamic equivalence priority
 *   - Academic Textual Scholars: Secondary victim (moderate/constrained) — constrained by suppression of morphological precision; benefits from institutional relevance but bears research cost of translation-critical methodology
 *   - Denominational Translation Authorities: Institutional actor (institutional/constrained) — enforce the constraint through canonical translation choices and delegitimation of alternatives; maintain theological authority through standardized interpretation
 *   - Philological Record: Structural victim (powerless/trapped) — source-text morphological nuance becomes submerged under equivalence choices; scholars must work backward from translation to reconstruct original precision
 *   - Formal Equivalence Heritage: Degraded institutional form (institutional/arbitrage) — historical King James/Geneva tradition represents alternative authority structure; now marginalized but persists through academic protocols (piton)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the dynamic equivalence reading as inevitable translation necessity rather than constructed institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, 0.38).
domain_priors:suppression_score(biblical_source_text__dynamic_equivalence_reading, 0.52).
domain_priors:theater_ratio(biblical_source_text__dynamic_equivalence_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__dynamic_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__dynamic_equivalence_reading, "Dynamic Equivalence Reading: Communicative Effectiveness as Primary Authority").
narrative_ontology:topic_domain(biblical_source_text__dynamic_equivalence_reading, "biblical_studies/translation_theory/religious_authority").

domain_priors:requires_active_enforcement(biblical_source_text__dynamic_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__dynamic_equivalence_reading, '2e25fd47-89ff-46bb-bdda-9954e9d920a7').
narrative_ontology:cs_kernel_codification('2e25fd47-89ff-46bb-bdda-9954e9d920a7', fixed_text).
narrative_ontology:cs_authority_grounding('2e25fd47-89ff-46bb-bdda-9954e9d920a7', lineage).
narrative_ontology:cs_interpretation_layer_present('2e25fd47-89ff-46bb-bdda-9954e9d920a7').
narrative_ontology:cs_reading_relation('2e25fd47-89ff-46bb-bdda-9954e9d920a7', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('2e25fd47-89ff-46bb-bdda-9954e9d920a7', biblical_source_text__critical_reconstructive_reading, coexists_with).
narrative_ontology:cs_axiom('2e25fd47-89ff-46bb-bdda-9954e9d920a7', foundational, meaning_realized_in_reader_comprehension).
narrative_ontology:cs_axiom_status(meaning_realized_in_reader_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('2e25fd47-89ff-46bb-bdda-9954e9d920a7', meaning_realized_in_reader_comprehension, conventional).
narrative_ontology:cs_axiom('2e25fd47-89ff-46bb-bdda-9954e9d920a7', foundational, pastoral_intelligibility_over_morphological_precision).
narrative_ontology:cs_axiom_status(pastoral_intelligibility_over_morphological_precision, holdable).
narrative_ontology:cs_axiom_grounding('2e25fd47-89ff-46bb-bdda-9954e9d920a7', pastoral_intelligibility_over_morphological_precision, instrumental).
narrative_ontology:cs_reference_frame('2e25fd47-89ff-46bb-bdda-9954e9d920a7', communicative_equivalence_authority).
narrative_ontology:cs_drift_state('2e25fd47-89ff-46bb-bdda-9954e9d920a7', contemporary_digital_morphology_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2e25fd47-89ff-46bb-bdda-9954e9d920a7', '').
narrative_ontology:cs_kernel_id(biblical_source_text__dynamic_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, lay_reader_communities).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, missionary_contexts).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, pastoral_preaching).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, word_study_precision).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, historical_morphological_fidelity).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, textual_scholarship).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY READER IN LINGUISTIC ENTRAPMENT (SNARE) — Cannot access the original Hebrew/Greek morphology without specialized training. Dependent on translation choices made by intermediaries. Dynamic equivalence translation reduces linguistic barriers but traps the reader in the translator's interpretive frame — the 'equivalent meaning' is the translator's reading, not the reader's independent interpretation. No exit from interpretive dependency.
constraint_indexing:constraint_classification(biblical_source_text__dynamic_equivalence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MISSIONARY ORGANIZATION (ROPE) — Primary beneficiary of dynamic equivalence approach. Reduces friction for gospel communication to non-specialist audiences. Experiences the constraint as coordination: clear, culturally resonant communication is the core mission. Low suppression experienced — the constraint aligns with institutional goals. Net beneficiary through reduced translation licensing friction and broad cultural intelligibility.
constraint_indexing:constraint_classification(biblical_source_text__dynamic_equivalence_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ACADEMIC TEXTUAL SCHOLAR (TANGLED ROPE) — Benefits from the constraint's existence (dynamic equivalence translations make biblical content accessible for pastoral work, supporting institutional relevance of seminaries). But constrained by the extraction cost: morphological precision is suppressed, word-study rigor is compromised, and historical-critical methods require constant translation-critical footnoting. Can theoretically exit to formal equivalence or critical reconstructive reading, but career and institutional pressures constrain the exit (translation committees dominated by dynamic equivalence proponents, publishing incentives favor pastoral accessibility over textual precision).
constraint_indexing:constraint_classification(biblical_source_text__dynamic_equivalence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DENOMINATIONAL TRANSLATION AUTHORITY (TANGLED ROPE) — Coordinates community religious practice and theological coherence (genuine coordination function) while enforcing interpretive standardization through canonical translation choices (extraction mechanism). Suppression: enforced use of the authorized version, discouraged consulting of alternatives, systematic delegitimation of competing readings. Constrained exit — denominational bodies could theoretically shift to formal equivalence, but this would fragment their theological authority structure and reduce lay accessibility.
constraint_indexing:constraint_classification(biblical_source_text__dynamic_equivalence_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: PHILOLOGICAL RECORD (SNARE) — The original Hebrew/Greek morphological and syntactic precision bears the cost of the dynamic equivalence constraint. Once interpretive choices are embedded in widespread translations, the source-text's semantic nuance is submerged under layers of equivalence choices. Scholars must work backward from translation decisions to reconstruct what the source text actually said. The structural victim is the intelligibility of the source text itself for future scholars — extractiveness concentrated on something that cannot organize or exit.
constraint_indexing:constraint_classification(biblical_source_text__dynamic_equivalence_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: FORMAL EQUIVALENCE HERITAGE (PITON) — King James and Geneva translation traditions represented a coordinated commitment to morphological fidelity. Dynamic equivalence reading treats formal equivalence as a failed method — 'literalism' is mocked as obscuring meaning rather than preserving it. The formal equivalence tradition persists through inertia in academic curricula and specialized scholarly use, but its primary institutional authority (mass readership, denominational authority, pastoral vernacularity) has been displaced by dynamic equivalence. Theater ratio high: formal equivalence persists in academic protocols and footnotes, but its functional authority is degraded.
constraint_indexing:constraint_classification(biblical_source_text__dynamic_equivalence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, any translation necessarily involves loss of source-text features. No translation can preserve Hebrew morphological richness in English syntax. Dynamic equivalence merely acknowledges this structural necessity explicitly rather than hiding it under the fiction of literal fidelity. This perspective sees the constraint as an inevitable consequence of the nature of translation itself — a natural law rather than a constructed institutional arrangement. However, the structural data contradicts this: the constraint is enforced through institutional authority (translation committees, denominational approval, publishing gatekeeping), not through physical or logical necessity. The false summit detector will flag this.
constraint_indexing:constraint_classification(biblical_source_text__dynamic_equivalence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__dynamic_equivalence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biblical_source_text__dynamic_equivalence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biblical_source_text__dynamic_equivalence_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(biblical_source_text__dynamic_equivalence_reading, TR),
    TR >= 0.70.

:- end_tests(biblical_source_text__dynamic_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint suppresses morphological precision and academic word-study rigor, but the suppression is not total — formal equivalence alternatives exist (though marginalized), and digital tools enable some lay reader access to source morphology. The value reflects the asymmetric extraction of interpretive authority (lay readers are trapped; scholars are constrained; missionaries benefit) without being severe or irreversible. The measurement trajectory (0.28 → 0.38 over 50 years) shows gradual intensification as dynamic equivalence became institutional norm and formal equivalence was systematically delegitimized. Suppression (0.52): Moderate-high. Institutional enforcement through translation committees, denominational approval processes, publishing gatekeeping, and systematic dismissal of morphological precision as 'wooden literalism.' Lay readers face high suppression (trapped exit); scholars face moderate suppression (constrained exit through career incentive structures). Theater ratio (0.58): Moderate-high. Dynamic equivalence translations perform accessibility and communicability through extensive apparatus (introductions, footnotes, cross-references) that present the translator's interpretive work as transparent 'meaning transfer' rather than interpretive choice. The performative content has increased over the interval (0.42 → 0.58) as translations became more elaborate and their constructed nature less visible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full range of DR classification from a single set of base properties. The lay reader sees pure extraction (Snare) — trapped in linguistic dependence. The missionary organization sees coordination (Rope) — the constraint aligns perfectly with pastoral mission. The textual scholar sees mixed coordination and extraction (Tangled Rope) — the institutional system provides some benefits (accessibility supports seminary teaching) but suppresses core professional concerns (morphological precision). The denominational authority sees institutional coordination (Tangled Rope) — enforcing theological standardization while reducing reader autonomy. The philological record sees pure extraction (Snare) — source-text morphology is the victim with no exit. The formal equivalence tradition sees degradation and inertia (Piton) — its functional authority displaced but ritual role persists in academic protocols. The analytical observer risks seeing natural necessity (Mountain) — translation always loses source features — but the structural enforcement (institutional gatekeeping, deliberate delegitimation of alternatives) reveals this as a false summit: the constraint is constructed, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by agent structural position. Lay readers experience maximum extraction (d ≈ 0.92, trapped) because they lack exit options and depend on translator-mediated interpretation. Missionary organizations experience low extraction (d ≈ 0.15, arbitrage) — beneficiaries who can shift to other translation approaches if needed but find dynamic equivalence aligns with mission. Scholars experience moderate-high extraction (d ≈ 0.68, constrained) — they could theoretically exit to formal equivalence or critical reconstructive reading, but institutional pressures (publication venues, committee membership, funding) constrain the exit. The philological record (structural victim) experiences maximum extraction (d ≈ 0.95, trapped) — the source text's morphology cannot organize or exit. The formal equivalence heritage experiences low extraction (d ≈ 0.20, arbitrage) — an institutional alternative that retains some academic authority despite loss of mass readership. Each agent's experienced chi (effective extraction) is computed from their base extraction (0.38), f(d), and scope modifier σ(S), producing the perspectival gap: lay readers in local scope experience χ ≈ 0.70; missionaries at global scope experience χ ≈ 0.00; scholars at national scope experience χ ≈ 0.44.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    semantic_equivalence_underdetermination,
    'What constitutes ''equivalent meaning'' when translating ancient source text with no direct modern equivalent concepts (e.g., Hebraic parallelism, eschatological idioms)?',
    'Comparative analysis of dynamic equivalence translation choices for the same passage across different translation committees; identification of whether equivalent-meaning produces consistent results or reveals interpretive variance masked by translation naturalism.',
    'If consistent: equivalence is determinate and the constraint''s extraction is justified as legitimate interpretation. If variant: equivalence is interpretively underdetermined and the constraint''s extraction reflects translator preference (hidden in translation). This directly affects whether the constraint is Rope (coordination) or Snare (extractive interpretive closure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(semantic_equivalence_underdetermination, conceptual, 'Whether semantic equivalence is objectively determinate or interpretively underdetermined').

omega_variable(
    original_reading_access_permanence,
    'Does dynamic equivalence reading permanently foreclose lay readers'' access to the original text''s morphological and syntactic structures, or do digital tools (interlinear Bibles, morphological databases) enable lay readers to bypass translation intermediaries?',
    'Empirical tracking of lay reader engagement with source-text tools; comparison of comprehension and interpretive independence between communities using dynamic equivalence translations alone vs. communities with access to morphological resources.',
    'If access is permanently foreclosed: Snare classification confirmed for lay readers; extraction is structural and irreversible. If digital tools enable bypass: classification shifts toward constrained rather than trapped; the constraint''s extraction is partially mitigated by technological alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_reading_access_permanence, empirical, 'Whether digital tools enable lay reader access to source-text morphology').

omega_variable(
    pastoral_mission_and_textual_precision_commensurability,
    'Can high pastoral communicative effectiveness and high morphological precision coexist, or is the constraint''s extraction (sacrificing precision for communicability) actually necessary for the pastoral mission?',
    'Comparison of pastoral impact metrics (conversion, discipleship depth, theological retention) between congregations using dynamic equivalence translations vs. formal equivalence translations; analysis of whether theological precision supports or undermines pastoral outcomes.',
    'If they coexist: the constraint is extractive — mission serves as cover story for institutional standardization. If they trade off genuinely: the constraint is necessary coordination, not extraction. Classification may shift from Snare/Tangled Rope to pure Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pastoral_mission_and_textual_precision_commensurability, empirical, 'Whether pastoral communicative effectiveness and textual precision are mutually exclusive').

omega_variable(
    kernel_reading_commensurabilitiy,
    'Is the dynamic equivalence reading a legitimate hermeneutical approach to the biblical source text kernel, or does it essentially reject the premise that the source text''s morphological structure carries semantic significance — effectively adopting a different kernel (textual meaning as independent of linguistic structure)?',
    'Comparison of the dynamic equivalence reading''s foundational axioms with formal equivalence and critical reconstructive readings. If dynamic equivalence denies that morphological structure is semantically determinate, it may be interpreting a different kernel (meaning-as-independent-of-form) rather than reading the same kernel differently.',
    'If same kernel, different reading: Coexists_with relations to sibling readings are correct. If different kernel: the constraint and its siblings are not readings of the same kernel but competing claims about what the kernel is. This would alter the cs_structure.reading_relations from coexists_with to a hierarchical foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commensurabilitiy, conceptual, 'Whether dynamic equivalence and formal equivalence are readings of the same kernel or competing kernel definitions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__dynamic_equivalence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bst_dyn_equiv_tr_t0, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bst_dyn_equiv_tr_t25, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement(bst_dyn_equiv_tr_t50, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(bst_dyn_equiv_be_t0, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bst_dyn_equiv_be_t25, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(bst_dyn_equiv_be_t50, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(bst_dyn_equiv_su_t0, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(bst_dyn_equiv_su_t25, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(bst_dyn_equiv_su_t50, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__dynamic_equivalence_reading, information_standard).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__critical_reconstructive_reading).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, theological_interpretation_authority_lock).

% DUAL FORMULATION NOTE:
% The dynamic equivalence reading is one of three structural readings of the biblical source text kernel. Each reading has a different ε value reflecting the extractive cost of its hermeneutical choice: formal equivalence (ε ≈ 0.25) suppresses communicative accessibility; critical-reconstructive (ε ≈ 0.52) suppresses canonical authority and creates scholarly/lay interpretive fragmentation; dynamic equivalence (ε ≈ 0.38) suppresses morphological precision. These are not measurements of the same constraint from different angles — they are different constraints with different victims, different extractiveness, and different institutional enforcement mechanisms. They are linked through network.affects_constraints because decisions about which reading is authoritative (institutional capital flow to dynamic equivalence in evangelical contexts, to formal equivalence in scholarly contexts) influences the extractiveness and suppression values of the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
