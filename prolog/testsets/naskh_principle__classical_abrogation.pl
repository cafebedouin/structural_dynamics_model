% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Naskh (Quranic Abrogation) — Chronological Supersession Principle
 *   domain: islamic_jurisprudence/quranic_hermeneutics
 *
 * SUMMARY:
 *   The naskh principle (classical abrogation) is one of the foundational
 *   tools in Islamic jurisprudence for resolving apparent contradictions
 *   within the Quranic text. The principle holds that when two Quranic verses
 *   address the same legal or theological topic and appear to contradict, the
 *   verse revealed later in time abrogates (nasakh) the earlier verse,
 *   removing its legal force while potentially preserving its spiritual or
 *   historical significance. This constraint embodies a deep tension between
 *   legal certainty and theological coherence. From the institutional
 *   perspective of jurisprudential schools, naskh is a coordination mechanism
 *   that resolves disputes by appeal to chronological order rather than
 *   endless hermeneutic negotiation. From the perspective of exegetes and
 *   interpretive communities seeking to preserve the Quran's unified meaning,
 *   the principle imposes a severe constraint on reconciliation strategies.
 *   The classical reading instantiates fixed legal rulings with clear
 *   hierarchies of supersession; beneficiaries include institutional
 *   jurisprudence (gain clarity) and legal certainty doctrine; victims
 *   include exegetical coherence and theological reconciliation programs
 *   (lose interpretive flexibility). The constraint is tangled rope because
 *   it provides genuine coordination (resolving ambiguity via chronological
 *   hierarchy) while enforcing extraction (denying interpreters the liberty
 *   to harmonize through context, meaning, or situation-specific
 *   application).
 *
 * KEY AGENTS:
 *   - Exegetical Tradition (Classical Schools): Primary beneficiary (institutional/arbitrage) — naskh principle provides clear rules for resolving disputes; reduces endless hermeneutic debate
 *   - The Quranic Text Itself (Scriptural Coherence): Primary victim (powerless/trapped) — subjected to internal division where certain verses are legally 'dead' even if textually present; loses integral meaning
 *   - Contemporary Contextual Exegetes: Secondary victim (organized/constrained) — wish to reconcile verses through situational analysis ('asbab al-nuzul') but face institutional pressure to apply naskh; constrained by classical authority
 *   - Legal Jurists (Muftis, Judges): Secondary beneficiary (institutional/arbitrage) — benefit from clarity in issuing legal rulings; naskh provides decision procedure
 *   - Reform Interpreters: Analytical observer (analytical/constrained) — see naskh as historically contingent methodology; advocate for contextual harmonization as superior; constrained by traditional institutional weight of classical schools
 *   - Analytical Observer (Civilizational): Risks naturalizing the principle as logical necessity when it is actually an institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.48).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.62).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.48).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Naskh (Quranic Abrogation) — Chronological Supersession Principle").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "islamic_jurisprudence/quranic_hermeneutics").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, '11f4e129-9293-43f7-af4c-e38dccf8d58f').
narrative_ontology:cs_kernel_codification('11f4e129-9293-43f7-af4c-e38dccf8d58f', formalized).
narrative_ontology:cs_authority_grounding('11f4e129-9293-43f7-af4c-e38dccf8d58f', lineage).
narrative_ontology:cs_interpretation_layer_present('11f4e129-9293-43f7-af4c-e38dccf8d58f').
narrative_ontology:cs_reading_relation('11f4e129-9293-43f7-af4c-e38dccf8d58f', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_reading_relation('11f4e129-9293-43f7-af4c-e38dccf8d58f', naskh_principle__progressive_restriction, coexists_with).
narrative_ontology:cs_axiom('11f4e129-9293-43f7-af4c-e38dccf8d58f', foundational, chronological_revelation_determines_legal_force).
narrative_ontology:cs_axiom_status(chronological_revelation_determines_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('11f4e129-9293-43f7-af4c-e38dccf8d58f', chronological_revelation_determines_legal_force, conventional).
narrative_ontology:cs_axiom('11f4e129-9293-43f7-af4c-e38dccf8d58f', foundational, abrogated_verse_loses_legal_force).
narrative_ontology:cs_axiom_status(abrogated_verse_loses_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('11f4e129-9293-43f7-af4c-e38dccf8d58f', abrogated_verse_loses_legal_force, deontological).
narrative_ontology:cs_reference_frame('11f4e129-9293-43f7-af4c-e38dccf8d58f', classical_jurisprudential_consensus).
narrative_ontology:cs_drift_state('11f4e129-9293-43f7-af4c-e38dccf8d58f', contemporary_quranic_studies, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('11f4e129-9293-43f7-af4c-e38dccf8d58f', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, legal_clarity_doctrine).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, institutional_jurisprudence).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, quranic_exegetical_coherence).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, scriptural_reconciliation_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXEGETE FACING APPARENT CONTRADICTION (SNARE) — The interpreter confronting two apparently contradictory verses on the same topic is structurally trapped. The classical naskh principle mandates: if chronological order is known, the later verse abrogates the earlier. No exit: the interpreter cannot ignore either verse, cannot harmonize them without violating the principle, and cannot reorder the revelation sequence. Maximal extraction of interpretive agency. The principle denies the exegete the cognitive liberty to resolve tensions through theological ingenuity or contextual differentiation.
constraint_indexing:constraint_classification(naskh_principle__classical_abrogation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SCHOOL OF LAW / MADHAB (TANGLED ROPE) — A jurisprudential school benefits from the naskh principle's legal clarity: disputes are resolved via chronological hierarchy rather than endless hermeneutic debate. This coordination function (stabilizing legal rulings) is genuine. But the principle also constrains the school's interpretive scope — it must abandon exegetical solutions the school would prefer if the chronological logic mandates abrogation. Constrained by the principle's compulsory force; beneficiary of its certainty-producing function. The tension between coordination and extraction is the core of tangled rope.
constraint_indexing:constraint_classification(naskh_principle__classical_abrogation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL CONSENSUS (ROPE) — The naskh principle as codified by classical schools (Shafi'i, Hanafi, Maliki consensus on the basic framework) sees the principle as primarily a coordination mechanism: it solves the problem of apparent textual contradiction without requiring ad-hoc legislative interpretation. The institutional actor benefits from the principle's existence (it reduces interpretive disputes, provides clear adjudication rules) and experiences it as enabling rather than constraining. Net beneficiary; effective extraction is minimal from this angle.
constraint_indexing:constraint_classification(naskh_principle__classical_abrogation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HISTORICAL CHRONOLOGY AS ENFORCEMENT SUBSTRATE (PITON) — The principle depends on knowing the revelation order of verses. But the revelation chronology is reconstructed from hadith, early biographical sources, and tradition — it is not intrinsically transparent from the Quran itself. As a foundation for legal certainty, the chronological record is increasingly theatrical: modern scholarship questions many traditional datings, and the principle's enforcement requires treating the reconstructed sequence as immutable. The mechanism is inert — it persists because it has been institutionalized, not because the evidential substrate (revelation order) remains robust. Piton classification derives from the theater gate: the principle appears to function authoritatively while its foundations are contested.
constraint_indexing:constraint_classification(naskh_principle__classical_abrogation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONTEMPORARY REFORM EXEGETES (SCAFFOLD) — Modern and contemporary interpreters increasingly deploy contextual/situational analysis ('asbab al-nuzul', specific contexts of revelation, thematic coherence) to reconcile apparently contradictory verses without invoking abrogation. This perspective sees the naskh principle as a temporary cognitive support structure: useful when context is opaque, but dissolving when historical and contextual information improve. The constraint has a sunset: as exegetical method matures and historical source criticism advances, the need for blunt chronological abrogation decreases. Constrained by traditional authority (classical schools still hold sway); sees the principle's sunset as inevitable.
constraint_indexing:constraint_classification(naskh_principle__classical_abrogation, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical distance, the naskh principle appears as a natural response to the logical problem of contradiction: any revealed law containing apparent contradictions must have a resolution mechanism; chronological priority is one such mechanism. The principle looks immutable — a logical necessity embedded in any legal hermeneutics. However, the structural data contradicts this: identifiable beneficiaries (institutional jurisprudence, legal clarity doctrine) and victims (exegetical flexibility, theological coherence programs) indicate the principle is a constructed institutional arrangement, not a logical law. The engine will classify this as a false summit.
constraint_indexing:constraint_classification(naskh_principle__classical_abrogation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(naskh_principle__classical_abrogation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(naskh_principle__classical_abrogation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(naskh_principle__classical_abrogation, TR),
    TR >= 0.70.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The principle extracts interpretive agency from exegetes by mandating that chronological order determines legal force. The extraction is real: an interpreter facing two verses cannot choose to harmonize them if the principle applies. However, extraction is not maximal because (a) the principle solves a genuine coordination problem (how to resolve contradiction), and (b) classical jurisprudence developed sophisticated doctrine limiting naskh's scope (not all apparent contradictions are abrogations; specific conditions must be met). The value reflects that extraction is embedded in a coordination function. Suppression (0.62): Moderate-high. Multiple suppression mechanisms: (1) the revelation chronology is reconstructed from external sources (hadith, biography) rather than from the Quranic text itself, making the abrogation determination opaque; (2) classical jurisprudence restricted naskh to specific categories, but contemporary scholars dispute these restrictions; (3) institutional authority of traditional schools creates barriers to proposing alternative methodologies. Theater ratio (0.58): Moderate. The principle's enforcement depends on the reliability of the revelation chronology, which has become increasingly questioned in modern Quranic studies. Much of the apparent 'authority' of naskh determinations is now performative — the principle is invoked as if the chronology is certain when modern scholarship shows it is partially reconstructed and contested. The theatrical content has increased as the evidential substrate has become more questionable.
 *
 * PERSPECTIVAL GAP:
 *   The classical abrogation reading generates a wide perspectival gap. Institutional jurisprudence sees the principle as enabling (rope/tangled rope) — it solves disputes and provides clarity. Exegetes seeking textual coherence see it as constraining (snare) — it imposes a decision procedure that precludes their preferred solutions. Contextual harmonization advocates see it as temporary and increasingly obsolete (scaffold) — as exegetical methods improve, the need for blunt chronological abrogation diminishes. The analytical observer risks naturalizing the principle as logical necessity (mountain) when the structural data reveals it as an institutional arrangement with identifiable beneficiaries. The gap between the institutional perspective (positive view of certainty) and the exegetical perspective (negative view of lost coherence) is the core tension.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's d-value reflects the agent's structural relationship to the extraction flow. Institutional jurisprudence (beneficiary with arbitrage exit) experiences low d (around 0.15-0.20) — they benefit from the principle's clarity and have the option to selectively apply it or reinterpret its scope. Exegetes facing contradiction (powerless, trapped) experience high d (around 0.90-0.95) — they have no exit option and cannot reframe the problem. Contextual harmonization exegetes (organized, constrained) experience moderate d (around 0.55-0.65) — they have some scholarly agency but face institutional pressure from traditional authority. The piton perspective (institutional, arbitrage) experiences low effective extraction but high theater (the chronology mechanism is increasingly questioned). The analytical observer risks d = 0.72 (analytical baseline), which produces the false-summit signature: the principle appears unchangeable when it is actually a constructed institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing between the principle's coordination function (genuine: it solves the problem of textual contradiction) and its extraction function (real: it denies interpreters liberty to reconcile through context). The classical reading provides legal certainty at the cost of theological coherence. The mandatrophy is resolved by recognizing that tangled rope is the correct classification — both functions are real. The principle would become snare if the coordination function were completely eliminated (if alternative dispute-resolution mechanisms were available). It would become rope if the extraction function were eliminated (if naskh applied only to explicitly abrogated verses, leaving implicit contradictions open to harmonization). The current tangled rope classification reflects the actual institutional state: genuine coordination (jurisprudential consensus depends on naskh), genuine extraction (interpretive liberty is constrained).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_chronology_authenticity,
    'Is the reconstructed chronological order of Quranic revelation sufficiently reliable to bear the weight of legal abrogation determinations?',
    'Comparative hadith source criticism; cross-validation of suras'' dating across canonical hadith collections (Bukhari, Muslim, Tirmidhi); assessment of post-Quranic sources vs. internal textual markers',
    'If chronology is unreliable: the principle''s authority collapses — abrogation determinations rest on uncertain foundations, strengthening the contextual harmonization reading. If chronology is sufficiently reliable: the principle retains structural legitimacy, though challenges to specific chronological assignments remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_chronology_authenticity, empirical, 'Reliability of reconstructed revelation chronology for abrogation determinations').

omega_variable(
    coherence_vs_certainty_tradeoff,
    'Does the naskh principle''s legal certainty outweigh the loss of theological coherence and scriptural unity achieved through contextual harmonization?',
    'Doctrinal assessment: compare jurisprudential predictability under classical naskh vs. under contextual harmonization across historical case law; analyze whether apparent contradictions are genuine legal conflicts or contextual/situational differences',
    'If certainty is higher under naskh: the principle''s extraction is justified as coordination cost. If coherence is higher under harmonization: the principle appears extractive (imposing certainty at the cost of scriptural integrity). If both are achievable simultaneously: the principle forecloses a superior approach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_vs_certainty_tradeoff, conceptual, 'Trade-off between legal certainty (naskh) and theological coherence (harmonization)').

omega_variable(
    abrogation_scope_indeterminacy,
    'When a verse is abrogated, does the abrogation extend only to the specific ruling, or to the entire verse including its spiritual/ethical content?',
    'Analysis of classical jurisprudential doctrine on abrogated verses; examination of how legal vs. spiritual value is attributed to abrogated suras (e.g., Surah 2:106 and the debate over its scope)',
    'If abrogation is total: abrogated verses lose all force except historical/spiritual interest — stronger enforcement mechanism, higher suppression. If abrogation is partial (ruling-specific): the constraint''s scope is narrower, allowing harmonization of non-legal content — lower suppression, more interpretive freedom.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abrogation_scope_indeterminacy, conceptual, 'Scope of abrogation: ruling-specific vs. total verse abrogation').

omega_variable(
    reading_committer_ambiguity,
    'This constraint instantiates the classical_abrogation reading of the naskh kernel. The sibling readings (contextual_harmonization, progressive_restriction) pursue different resolution strategies for apparent Quranic contradictions. Is the classical reading foreclosed by alternative readings, or do they coexist as competing legitimate methodologies held by different jurisprudential traditions?',
    'Historical-institutional analysis: trace which readings are held by which schools of jurisprudence across time; determine whether any reading logically rules out the others, or whether the readings coexist as methodological choices. Assess whether contemporary interpretive communities use multiple readings for different verse-pairs.',
    'If foreclosed: the classical reading eliminates alternatives within the same framework — high authority, but the framework is contested. If coexisting: multiple readings remain live across traditions — lower authority for any single reading, but more interpretive pluralism available. This omega instantiates the oracle gap: recognizing the reading as ONE among several requires stepping outside the classical reading''s native framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'Kernel-level ambiguity: whether classical abrogation forecloses or coexists with alternative readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naskh_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(naskh_tr_t4, naskh_principle__classical_abrogation, theater_ratio, 4, 0.48).
narrative_ontology:measurement(naskh_tr_t8, naskh_principle__classical_abrogation, theater_ratio, 8, 0.58).

% Extraction over time
narrative_ontology:measurement(naskh_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(naskh_be_t4, naskh_principle__classical_abrogation, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(naskh_be_t8, naskh_principle__classical_abrogation, base_extractiveness, 8, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(naskh_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(naskh_su_t4, naskh_principle__classical_abrogation, suppression_requirement, 4, 0.57).
narrative_ontology:measurement(naskh_su_t8, naskh_principle__classical_abrogation, suppression_requirement, 8, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__progressive_restriction).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, tajweed_recitation_standardization).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, hadith_authenticity_grading).

% DUAL FORMULATION NOTE:
% The naskh principle is part of a constraint family under the parent kernel 'naskh_principle'. The classical_abrogation reading is linked to sibling readings (contextual_harmonization and progressive_restriction) via shared kernel but different ε values and structural properties. Classical abrogation (ε≈0.48, tangled_rope) represents the highest-extraction reading because it imposes mandatory chronological hierarchy. Contextual harmonization (ε≈0.32, rope/scaffold) represents lower extraction because it preserves interpretive liberty and theological coherence. Progressive restriction (ε≈0.40, tangled_rope) occupies the middle: it retains both verses' legal status but still constrains interpretation. All three readings address the same underlying problem (Quranic apparent contradictions) but with different solution mechanisms and different distributions of interpretive agency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__classical_abrogation, institutional, 0.18).
constraint_indexing:directionality_override(naskh_principle__classical_abrogation, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
