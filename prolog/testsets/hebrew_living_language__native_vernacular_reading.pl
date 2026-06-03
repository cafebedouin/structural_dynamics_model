% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__native_vernacular_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__native_vernacular_reading, []).

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
 *   constraint_id: hebrew_living_language__native_vernacular_reading
 *   human_readable: Hebrew as Living Language: Native Vernacular Transmission Reading
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The native-vernacular reading of the Hebrew-living-language kernel
 *   asserts that Hebrew 'lives' as a language only insofar as it is
 *   transmitted intergenerationally from native speakers to children as their
 *   primary daily speech. This reading reconstructs Hebrew from a 1800-year
 *   liturgical hiatus into a modern vernacular language through deliberate
 *   state policy, corpus planning, and educational enforcement. The reading
 *   sustains a tangled-rope classification because it performs genuine
 *   coordination — unifying diverse diaspora communities through a shared
 *   language — while simultaneously imposing asymmetric extraction on
 *   diaspora language communities (Yiddish, Ladino speakers) who are
 *   displaced by the Hebrew-only policy. The constraint exhibits moderate
 *   extractiveness (0.48) reflecting the tension between the coordination
 *   function (real unification benefit) and the suppression mechanism
 *   (pressure against diaspora language maintenance). The suppression level
 *   (0.62) captures the institutional and social barriers to maintaining
 *   Hebrew alongside other Jewish languages without assimilation. The theater
 *   ratio (0.55) reflects ongoing standardization work — native-speaker
 *   fluency standards, educational testing, lexicographic maintenance — that
 *   sustains the vernacular requirement as a normative standard. This reading
 *   is one of three contested interpretations of the Hebrew-living-language
 *   kernel; the sibling readings (liturgical-preservation and
 *   hybrid-continuity) sustain Hebrew through non-vernacular mechanisms and
 *   would allow diaspora language coexistence.
 *
 * KEY AGENTS:
 *   - Hebrew Native Speaker Communities (Israeli): Primary beneficiary (institutional/arbitrage) — experience coordination through shared language; children acquire Hebrew natively without explicit learning burden.
 *   - Israeli State Authority: Primary beneficiary (institutional/arbitrage) — secures national linguistic unity and state legitimacy through language policy enforcement.
 *   - First-Generation Hebrew Learners (1880-1948): Organized agents (organized/constrained) — reconstructed Hebrew as living language; experienced high lexicographic labor burden and suppression of previous language knowledge.
 *   - Diaspora Yiddish Communities: Primary victim (powerless/trapped) — displaced by Hebrew-only policy; cultural and linguistic continuity broken without preservation alternatives.
 *   - Diaspora Ladino Communities: Primary victim (powerless/trapped) — similarly displaced; Sephardic linguistic and cultural heritage suppressed in favor of Ashkenazi-centered Hebrew revival.
 *   - Intergenerational Language Transmission Capacity: Abstract victim (powerless/trapped) — institutional mechanisms prioritize Hebrew native transmission, constraining resources for multilingual education.
 *   - Academic Historical Linguistics: Institutional observer (institutional/arbitrage) — maintains performative standards for what counts as 'living language'; conducts ongoing verification work.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_vernacular_reading, 0.48).
domain_priors:suppression_score(hebrew_living_language__native_vernacular_reading, 0.62).
domain_priors:theater_ratio(hebrew_living_language__native_vernacular_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_vernacular_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(hebrew_living_language__native_vernacular_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hebrew_living_language__native_vernacular_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_vernacular_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_vernacular_reading, "Hebrew as Living Language: Native Vernacular Transmission Reading").
narrative_ontology:topic_domain(hebrew_living_language__native_vernacular_reading, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_vernacular_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_vernacular_reading, '9f19289f-ad5f-4b7f-b87c-c7cd57a97829').
narrative_ontology:cs_kernel_codification('9f19289f-ad5f-4b7f-b87c-c7cd57a97829', formalized).
narrative_ontology:cs_authority_grounding('9f19289f-ad5f-4b7f-b87c-c7cd57a97829', extraction).
narrative_ontology:cs_interpretation_layer_present('9f19289f-ad5f-4b7f-b87c-c7cd57a97829').
narrative_ontology:cs_reading_relation('9f19289f-ad5f-4b7f-b87c-c7cd57a97829', hebrew_living_language__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('9f19289f-ad5f-4b7f-b87c-c7cd57a97829', hebrew_living_language__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('9f19289f-ad5f-4b7f-b87c-c7cd57a97829', foundational, native_transmission_necessary_condition).
narrative_ontology:cs_axiom_status(native_transmission_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('9f19289f-ad5f-4b7f-b87c-c7cd57a97829', native_transmission_necessary_condition, empirically_contingent).
narrative_ontology:cs_axiom('9f19289f-ad5f-4b7f-b87c-c7cd57a97829', foundational, id_1800_year_hiatus_reachability_break).
narrative_ontology:cs_axiom_status(id_1800_year_hiatus_reachability_break, holdable).
narrative_ontology:cs_axiom_grounding('9f19289f-ad5f-4b7f-b87c-c7cd57a97829', id_1800_year_hiatus_reachability_break, empirically_contingent).
narrative_ontology:cs_reference_frame('9f19289f-ad5f-4b7f-b87c-c7cd57a97829', intergenerational_native_vernacular_transmission).
narrative_ontology:cs_drift_state('9f19289f-ad5f-4b7f-b87c-c7cd57a97829', contemporary_multilingual_diaspora_revival, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9f19289f-ad5f-4b7f-b87c-c7cd57a97829', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_vernacular_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_vernacular_reading, hebrew_native_speaker_communities).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_vernacular_reading, israeli_state_authority).
narrative_ontology:constraint_victim(hebrew_living_language__native_vernacular_reading, diaspora_yiddish_communities).
narrative_ontology:constraint_victim(hebrew_living_language__native_vernacular_reading, diaspora_ladino_communities).
narrative_ontology:constraint_victim(hebrew_living_language__native_vernacular_reading, intergenerational_language_transmission_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIASPORA LANGUAGE COMMUNITIES (SNARE) — Yiddish and Ladino-speaking communities face structural displacement from the Hebrew-only policy. Exit is migration or assimilation with no preservation alternative. The constraint extracts linguistic identity and community continuity while offering no coordination benefit to these agents. Maximum experienced extraction from a powerless, trapped position.
constraint_indexing:constraint_classification(hebrew_living_language__native_vernacular_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIRST-GENERATION HEBREW LEARNERS (TANGLED ROPE) — Organized agents (Jewish Agency, Hebrew language societies) reconstructed Hebrew as a living vernacular. They experienced genuine coordination — unifying diverse diaspora communities through a shared language. But the native-vernacular requirement imposed extraction costs: learning a language with 1800-year gap in native transmission, constructing lexicon for modern life, normalizing only Hebrew (suppressing Yiddish/Ladino). Coordination function is real but enforcement suppresses alternatives.
constraint_indexing:constraint_classification(hebrew_living_language__native_vernacular_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ISRAELI STATE AUTHORITY (ROPE) — The state benefits from Hebrew-only policy: national unity, institutional legitimacy grounded in language, and arbitrage access (can pivot to English or other languages while enforcing Hebrew domestically). Experiences the constraint as coordination: unifying a state through shared language. From this perspective, extraction is minimal — the state is the beneficiary. But the power imbalance with diaspora communities is structural.
constraint_indexing:constraint_classification(hebrew_living_language__native_vernacular_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HEBREW NATIVE SPEAKER COMMUNITY (TANGLED ROPE) — Children raised in Hebrew-only environments benefit from intergenerational transmission that their parents had to acquire artificially. The constraint coordinates linguistic community for native speakers. But linguistic dominance imposes costs: pressure to assimilate non-Hebrew-speaking diaspora relatives, suppression of plurilingual identity, educational labor to maintain standards. Mixed coordination and extraction from a powerful, mobile position.
constraint_indexing:constraint_classification(hebrew_living_language__native_vernacular_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: DIASPORA MULTILINGUAL COMMUNITIES (TANGLED ROPE) — Communities attempting to maintain Hebrew alongside Yiddish, Ladino, or other heritage languages face educational barriers and social pressure toward Hebrew-only norms. They experience both coordination (access to Hebrew-speaking network) and extraction (pressure to abandon other languages, reduced institutional support for multilingual education). Constrained mobility — cannot maintain multiple languages without cost.
constraint_indexing:constraint_classification(hebrew_living_language__native_vernacular_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ACADEMIC HISTORICAL LINGUISTICS (PITON) — From the analytical perspective of comparative linguistics, the native-vernacular requirement operates as a largely performative gate: determining whether Hebrew 'counts' as living depends on measuring native speaker fluency, intergenerational transmission rates, and lexicon coverage — but these metrics are continuously negotiated and rescored as standards change. The academic apparatus maintains the distinction through research publication and expert judgment rather than through the measurements themselves functioning correctly. Theater ratio reflects ongoing definitional work about what counts as a 'living language'.
constraint_indexing:constraint_classification(hebrew_living_language__native_vernacular_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, this reading risks naturalizing the native-vernacular requirement as an immutable law: 'all living languages require native child speakers' is presented as linguistic fact. However, this view overlooks the constructed nature of the vernacular standard and the alternative readings (liturgical preservation, hybrid continuity) that sustain Hebrew through non-vernacular mechanisms. The engine will flag this as a false-summit candidate, revealing that the 'law' naturalizes a reading of the kernel rather than discovering an immutable fact.
constraint_indexing:constraint_classification(hebrew_living_language__native_vernacular_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__native_vernacular_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hebrew_living_language__native_vernacular_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hebrew_living_language__native_vernacular_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_living_language__native_vernacular_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hebrew_living_language__native_vernacular_reading, TR),
    TR >= 0.70.

:- end_tests(hebrew_living_language__native_vernacular_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.48): Moderate level reflects the genuine coordination function of Hebrew revival (unifying diaspora communities) offset against real extraction costs. In the 1880-1940 period (t=0-60), extractiveness was higher (0.62→0.48) because the coordination function was emergent and less established; the state authority had to impose the vernacular standard against resistance. By 1970 (t=90), extractiveness declines to 0.45 as native transmission became self-sustaining and the coordination benefit became experienced as natural (second and third generation native speakers). The trajectory reflects normalization: extraction decreases as the constraint shifts from imposed policy to internalized cultural practice. SUPPRESSION (0.62): High but declining. Initial suppression (0.75 at t=0) required active institutional enforcement: education policies mandating Hebrew, stigmatization of diaspora languages in public spaces, resource concentration on Hebrew standardization, and pressure on immigrant communities toward linguistic assimilation. Suppression declines over time (0.75→0.58) as native transmission becomes generationally self-sustaining and coercive enforcement becomes less necessary — children raised in Hebrew-only environments require no suppression mechanism to maintain Hebrew fluency. Residual suppression (0.58 at t=90) reflects ongoing barriers to diaspora language maintenance and institutional pressure toward Hebrew-only identity. THEATER RATIO (0.55): Moderate. The native-vernacular requirement sustains itself partly through genuine intergenerational transmission and partly through performative standardization. Teacher training, curriculum standardization, language testing, dictionary maintenance, and linguistic authority all constitute theater — ongoing work to certify that Hebrew meets the vernacular standard. This is distinct from the actual transmission work (parents raising children in Hebrew). Theater ratio declines over time (0.68→0.48) as vernacular transmission becomes generationally stable; less performative certification is needed when the mechanism is self-sustaining.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across power levels. The Hebrew native speaker community (institutional/arbitrage) experiences the constraint as pure coordination (rope perspective) — they inherit a stable linguistic community and experience only the coordination benefit. The Israeli state authority (institutional/arbitrage) similarly experiences rope — the language unifies the nation and enhances state legitimacy. First-generation learners (organized/constrained) experience the true tangled rope — they coordinate a diaspora into linguistic community while absorbing high learning costs and suppressing their own linguistic inheritance. Diaspora Yiddish and Ladino communities (powerless/trapped) experience pure snare — they are displaced without alternatives, their languages suppressed, their cultural continuity severed. The analytical observer at civilizational scope risks seeing this as a natural law (mountain perspective: 'all living languages require native transmission') — but this naturalizes the native-vernacular reading of the kernel as if it were the only structurally possible interpretation. The engine's false-summit detector will flag this as naturalization of a contested reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation is constrained by the structural relationships declared in beneficiaries and victims. Hebrew native speakers (institutional/arbitrage) derive low d-values (approximately 0.15) — they are beneficiaries with mobile exit options (can code-switch to English, Arabic, or other languages). Diaspora language communities (powerless/trapped) derive high d-values (approximately 0.95) — they are victims with no exit options; maintaining Yiddish or Ladino requires migration or assimilation, both costly. The Israeli state authority (institutional/arbitrage) derives low d-values; the state can pivot language policies without existential constraint. First-generation learners (organized/constrained) derive moderate d-values (approximately 0.55) — they experience extraction (high learning burden, suppression of L1 competence) but also coordination benefit and exit optionality (they could maintain Yiddish, but chose Hebrew for community reasons). The perspectival gap emerges across these different d-values: beneficiaries with low d perceive the constraint as rope (coordination with minimal extraction), while victims with high d perceive snare (pure extraction with trapped exit). The organized agents in the middle perceive tangled rope — genuine coordination mixed with real extraction costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by exposing the kernel contest beneath the apparent classification. The native-vernacular reading cannot claim to be THE classification of Hebrew's living-language status; it is ONE reading of a contested kernel. The mandatrophy question — 'Is Hebrew a living language?' — has three structurally distinct answers depending on which kernel reading is adopted: (1) NATIVE-VERNACULAR READING (this constraint): Hebrew lives only through intergenerational child-native transmission. This requires the 1880-1948 reconstruction, justifies suppression of diaspora languages, and extracts from yiddish-speaking communities. (2) LITURGICAL-PRESERVATION READING: Hebrew lives through unbroken sacred transmission and textual scholarship across 1800 years; no gap exists because the kernel was never severed. This reading coexists with diaspora language maintenance and has zero extraction. (3) HYBRID-CONTINUITY READING: Hebrew lives through both vernacular transmission AND liturgical/textual continuity; diaspora languages are complementary rather than competitive. This reading allows multilingual communities. The mandatrophy dissolves when the constraint acknowledges it is one reading competing with structurally viable alternatives. The native-vernacular reading's strength is its clear specification (intergenerational native transmission as criterion); its weakness is that it forecloses alternative readings that different communities hold as equally valid. The false-summit mountain perspective reveals the reading's naturalization strategy: presenting a normative policy choice ('Hebrew must be the sole vernacular') as a linguistic fact ('Hebrew can only live through native transmission').
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_naturalization,
    'Does the native-vernacular reading naturalize one contested reading of the kernel as if it were the only structurally possible realization?',
    'Compare this reading (native-vernacular requirement) against sibling readings (liturgical-preservation, hybrid-continuity) and examine whether the vernacular requirement could coexist with non-vernacular transmission mechanisms or whether it logically forecloses them.',
    'If the readings coexist structurally (different communities holding different readings simultaneously): classification remains tangled_rope with false-summit flag in mountain perspective. If the vernacular reading forecloses liturgical-only reading in a single framework: reformulate as stronger foundational axiom. If naturalization is confirmed: the ''law'' is a normative policy choice, not an empirical fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_naturalization, conceptual, 'Whether native-vernacular reading naturalizes one contested kernel interpretation as universal law').

omega_variable(
    reachability_break_1800_year_gap,
    'Does the 1800-year gap in native Hebrew transmission constitute a structural reachability break (D1 verdict: no intergenerational continuity) or can liturgical and textual continuity preserve the language across the gap?',
    'Historical linguistic analysis: compare Hebrew''s reachability status to other languages with documented gaps (Sanskrit, Classical Arabic, Latin); evaluate whether textual preservation + scholarly reconstruction + community intentionality can overcome generational discontinuity.',
    'If reachability is broken: native-vernacular reading is correct — Hebrew required artificial reconstruction and cannot claim unbroken continuity. If reachability is preserved through text/liturgy: hybrid-continuity and liturgical-preservation readings are structurally viable, and native-vernacular reading forecloses alternatives without sufficient justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reachability_break_1800_year_gap, empirical, 'Whether 1800-year gap constitutes reachability break or can be bridged by textual/liturgical continuity').

omega_variable(
    lexicographic_labor_extraction_rate,
    'How much of the extractiveness (0.48) is attributable to the lexicographic labor and educational infrastructure burden imposed on first-generation Hebrew learners versus structural suppression of diaspora languages?',
    'Cost-benefit analysis: educational investment in Hebrew standardization (dictionaries, grammars, pedagogy) versus opportunity cost of diaspora language preservation infrastructure; measurement of institutional resources allocated to Hebrew versus suppressed languages.',
    'If lexicographic labor dominates (>60%): extractiveness is overstated — the constraint is primarily coordination labor, not extraction. Reclassify toward rope. If suppression dominates: extractiveness is structural asymmetry, confirming tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lexicographic_labor_extraction_rate, empirical, 'Attribution of extractiveness to lexicographic labor versus diaspora language suppression').

omega_variable(
    diaspora_linguistic_identity_foreclosure,
    'Does the Hebrew-only policy logically foreclose diaspora language maintenance (strong constraint) or merely pressure toward assimilation (weak constraint with residual community options)?',
    'Historical case analysis: compare suppression mechanisms in different diaspora communities (Eastern European, North African, Mediterranean); examine whether multilingual Hebrew communities achieved institutional stability or were systematically pressured toward Hebrew-only assimilation.',
    'If strong foreclosure: victims classification is correct, suppression ≥0.62 is accurate. If weak pressure with residual options: suppression is overstated, constraint approaches rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_linguistic_identity_foreclosure, empirical, 'Whether Hebrew-only policy forecloses diaspora language maintenance').

omega_variable(
    performance_versus_competence_theater_ambiguity,
    'Does the theater_ratio (0.55) reflect performative maintenance of native-speaker standards (ongoing verification of fluency, testing, standardization) or genuine functional requirements of intergenerational transmission?',
    'Linguistic ethnography: measure proportion of Hebrew language maintenance activity devoted to standardization/certification/testing versus actual child acquisition and daily communication.',
    'If theater > 0.65: constraint approaches piton classification — native-vernacular requirement is mostly maintained through performance/testing rather than natural acquisition. If theater < 0.45: constraint is mostly functional transmission, confirming tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_versus_competence_theater_ambiguity, empirical, 'Theater ratio attributable to standardization performance versus functional transmission').

omega_variable(
    sibling_reading_coexistence_empirical_test,
    'Do the three sibling readings (native-vernacular, liturgical-preservation, hybrid-continuity) coexist as live positions within contemporary Hebrew communities or does one reading suppress the others?',
    'Ethnographic mapping: identify contemporary communities holding each reading (vernacular-only speakers, liturgical-Hebrew communities, multilingual heritage communities); measure institutional support and legitimacy for each reading.',
    'If coexistence confirmed: reading_relations should be ''coexists_with'' for both siblings. If one reading systematically suppresses others: relation type should be ''influences'' or ''forecloses'' depending on strength.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence_empirical_test, empirical, 'Whether three kernel readings coexist as live positions or compete to exclusivity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_vernacular_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_native_theater_1880, hebrew_living_language__native_vernacular_reading, theater_ratio, 0, 0.68).
narrative_ontology:measurement(hebrew_native_theater_1910, hebrew_living_language__native_vernacular_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement(hebrew_native_theater_1940, hebrew_living_language__native_vernacular_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(hebrew_native_theater_1970, hebrew_living_language__native_vernacular_reading, theater_ratio, 90, 0.48).

% Extraction over time
narrative_ontology:measurement(hebrew_native_extractiveness_1880, hebrew_living_language__native_vernacular_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(hebrew_native_extractiveness_1910, hebrew_living_language__native_vernacular_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(hebrew_native_extractiveness_1940, hebrew_living_language__native_vernacular_reading, base_extractiveness, 60, 0.48).
narrative_ontology:measurement(hebrew_native_extractiveness_1970, hebrew_living_language__native_vernacular_reading, base_extractiveness, 90, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_native_suppression_1880, hebrew_living_language__native_vernacular_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(hebrew_native_suppression_1910, hebrew_living_language__native_vernacular_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(hebrew_native_suppression_1940, hebrew_living_language__native_vernacular_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(hebrew_native_suppression_1970, hebrew_living_language__native_vernacular_reading, suppression_requirement, 90, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_vernacular_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__native_vernacular_reading, hebrew_living_language__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_vernacular_reading, hebrew_living_language__hybrid_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_vernacular_reading, yiddish_language_displacement_suppression).
narrative_ontology:affects_constraint(hebrew_living_language__native_vernacular_reading, ladino_language_displacement_suppression).
narrative_ontology:affects_constraint(hebrew_living_language__native_vernacular_reading, jewish_diaspora_assimilation_pressure).

% DUAL FORMULATION NOTE:
% The hebrew_living_language kernel decomposes into three structurally distinct constraints reflecting competing readings of what constitutes 'living language' status. Each reading has different ε values: native-vernacular (this story, ε=0.48, moderate extraction due to diaspora displacement), liturgical-preservation (ε=0.05, minimal extraction — coordinates through study without displacement), hybrid-continuity (ε=0.30, moderate coordination with residual extraction from standardization pressure). The three stories are linked as siblings in the kernel decomposition. Upstream stories documenting yiddish and ladino displacement are also affected — those constraints presuppose the native-vernacular reading as their causal mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
