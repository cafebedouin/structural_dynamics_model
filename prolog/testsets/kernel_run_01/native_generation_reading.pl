% ============================================================================
% CONSTRAINT STORY: native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_native_generation_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: native_generation_reading
 *   human_readable: Native Generation Reading: Hebrew as Living Language Through Mother-Tongue Transmission
 *   domain: sociolinguistics/religious_studies/nation_building
 *
 * SUMMARY:
 *   The native-generation reading of Hebrew-as-living-language constructs a
 *   specific epistemic boundary: Hebrew is 'real' and 'living' only when
 *   transmitted as mother tongue through childhood spontaneous acquisition,
 *   not when learned, performed, or transmitted through religious instruction
 *   and textual study. This reading became institutionally dominant during
 *   the Hebrew language revival (1880s-1920s) and the formation of the
 *   Israeli state (1948-1970s). It served a genuine nation-building function
 *   — converting a linguistically diverse immigrant population into a unified
 *   Hebrew-speaking polity. But the reading also suppresses and delegitimizes
 *   alternative Hebrews: the liturgical Hebrew of Orthodox rabbinical
 *   tradition, the learned Hebrew of Talmudic study, the communal Hebrews of
 *   diaspora Jewish languages (Yiddish, Judeo-Arabic), and the hybrid
 *   multilingual practices of immigrant communities. The constraint is
 *   tangled_rope because it combines genuine coordination (unifying
 *   linguistically diverse populations under a shared language) with
 *   asymmetric extraction (transferring linguistic authority from religious
 *   institutions to secular nationalists, from diaspora communities to
 *   Hebrew-dominant groups, from learned to 'natural' modes). The theater
 *   ratio has increased over the interval as the constraint's original
 *   functional necessity (creating a common language for diverse immigrants)
 *   has been superseded by institutional maintenance of 'native Hebrew'
 *   purity through educational policy, media standardization, and cultural
 *   authority allocation.
 *
 * KEY AGENTS:
 *   - Hebrew Secular Nationalist Movement: Primary beneficiary (institutional/arbitrage) — gains authority to define 'real' Hebrew; legitimizes its monopoly over language policy; displaces religious authorities.
 *   - Orthodox Rabbinical Authority: Primary victim (powerless/trapped) — delegitimized as guardians of 'real' Hebrew; expelled from epistemic control over language transmission; cannot exit without abandoning institutional identity.
 *   - Non-Hebrew Immigrant Communities (Yiddish, Judeo-Arabic speakers): Victim group (powerless/trapped) — mother tongues delegitimized; forced assimilation to Hebrew; cannot exit without abandoning heritage language.
 *   - Ashkenazi Hebrew Speaker Communities: Secondary beneficiary (institutional/arbitrage) — linguistic practice legitimized as 'authentic'; educational and cultural privilege; can acquire Hebrew natively in early settlement period.
 *   - Diaspora Liturgical Hebrew Practitioners: Mixed (moderate/constrained) — experience both coordination (unified language) and extraction (delegitimization of liturgical modes); cost is high but not total.
 *   - Israeli State Educational Apparatus: Institutional enforcer (institutional/constrained) — maintains native-generation requirement through policy; increasingly performative as functional need declines.
 *   - Post-Colonial Plurality Movements: Organized resistance (organized/mobile) — see constraint as temporary; expect sunset as state consolidates; pushing for multilingual coexistence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(native_generation_reading, 0.58).
domain_priors:suppression_score(native_generation_reading, 0.65).
domain_priors:theater_ratio(native_generation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(native_generation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(native_generation_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(native_generation_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(native_generation_reading, tangled_rope).
narrative_ontology:human_readable(native_generation_reading, "Native Generation Reading: Hebrew as Living Language Through Mother-Tongue Transmission").
narrative_ontology:topic_domain(native_generation_reading, "sociolinguistics/religious_studies/nation_building").

domain_priors:requires_active_enforcement(native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(native_generation_reading, formalized).
narrative_ontology:cs_authority_grounding(native_generation_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(native_generation_reading).
narrative_ontology:cs_kernel_id(native_generation_reading, hebrew_living_language).
narrative_ontology:cs_reading_relation(native_generation_reading, liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation(native_generation_reading, hybrid_coexistence_reading, influences).
narrative_ontology:cs_axiom(native_generation_reading, foundational, native_transmission_authenticity).
narrative_ontology:cs_axiom_status(native_transmission_authenticity, holdable).
narrative_ontology:cs_axiom(native_generation_reading, foundational, suppression_of_liturgical_legitimacy).
narrative_ontology:cs_axiom_status(suppression_of_liturgical_legitimacy, holdable).
narrative_ontology:cs_reference_frame(native_generation_reading, native_generation_linguistic_authenticity).
narrative_ontology:cs_drift_state(native_generation_reading, contemporary_post_functional_necessity, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(native_generation_reading, hebrew_secular_nationalism).
narrative_ontology:constraint_beneficiary(native_generation_reading, ashkenazi_hebrew_speakers).
narrative_ontology:constraint_victim(native_generation_reading, orthodox_rabbinical_authority).
narrative_ontology:constraint_victim(native_generation_reading, non_hebrew_immigrant_communities).
narrative_ontology:constraint_victim(native_generation_reading, diaspora_hebrew_liturgical_tradition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(native_generation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

constraint_indexing:constraint_classification(native_generation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

constraint_indexing:constraint_classification(native_generation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(native_generation_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

constraint_indexing:constraint_classification(native_generation_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

constraint_indexing:constraint_classification(native_generation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

constraint_indexing:constraint_classification(native_generation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(native_generation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(native_generation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(native_generation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(native_generation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(native_generation_reading, TR),
    TR >= 0.70.

:- end_tests(native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The native-generation reading enables the nationalist movement to monopolize control over legitimate Hebrew while delegitimizing competing authorities (religious, diaspora, immigrant, learned). The extraction is not maximal because the constraint serves genuine coordination functions — it did solve the urgent nation-building problem of linguistic unification. The extractiveness has accumulated over time (0.35 → 0.58 over 20-year interval) as the original functional need receded but institutional enforcement intensified. Suppression (0.65): High. The reading suppresses alternative Hebrews through several mechanisms: (1) institutional — educational policy redirects resources to native-speaker education; (2) epistemic — the definition of 'real' Hebrew excludes liturgical and learned modes; (3) cultural — media and public discourse treat native-generation Hebrew as the authentic form; (4) psychological/identity — immigrants and their children internalize the delegitimization of their heritage languages. Suppression is not total because alternative Hebrews persist in Orthodox institutions, diaspora communities, and academic study, but the institutional suppression is substantial. Theater ratio (0.25 → 0.48): Moderate increase. The original constraint had low theater — it performed a genuine function (language transmission and unification). As the functional necessity declined (Hebrew is now natively transmitted to 90%+ of Israeli children), the constraint increasingly relies on performative maintenance: ideology of native-generation purity, educational scripting of 'natural' speech, cultural prestige allocation. The theater tracks the shift from functional coordination to inertial enforcement.
 *
 * PERSPECTIVAL GAP:
 *   Massive perspectival divergence. The nationalist movement sees rope (pure coordination with minimal extraction) because they experience the constraint as legitimate language unification without suppression. The beneficiary's extraction runs toward them — they feel no extraction, only coordination benefit. The rabbinic authority sees snare (maximum extraction, no exit) because the constraint delegitimizes their institutional function and claims to define what 'real' Hebrew is. They experience suppression as total within their own sphere (linguistic authority transferred to nationalists). Immigrant communities see snare (mother-tongue suppression, cultural extraction) or tangled rope (some coordination benefit, but at high cost to heritage languages). The liturgical practitioners see tangled rope (genuine coordination value but asymmetric extraction). The analytical observer risks seeing mountain (natural law of how languages work: living = native-generation, dead = learned) but structural data reveals false summit — the reading constructs the boundary between living and dead rather than discovering it. The reading's claim that liturgical Hebrew 'died' is revealed as institutional suppression when we examine post-1948 persistence of liturgical practice in Orthodox communities. The claim that native-generation is 'spontaneous' is revealed as institutionally shaped when we examine how educational policy, media, and state language standardization define what counts as 'natural' Hebrew speech.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) derives from their structural position relative to the constraint's direction of benefit flow. The nationalist movement (beneficiary + arbitrage) experiences low or negative χ — the constraint subsidizes their authority. The rabbinic authority (victim + trapped) experiences high χ — no exit options, total institutional displacement. Immigrant communities show the perspectival gap: those who acquire Hebrew natively (Ashkenazi beneficiaries) experience rope-level χ (coordination, minimal extraction); those who maintain heritage languages (trapped victims) experience snare-level χ (delegitimization, forced assimilation). The state educational apparatus occupies an interesting position: it is the institutional enforcer, but increasingly constrained by the declining functional necessity of its own enforcement. As theater ratio rises (0.25 → 0.48), the apparatus's experienced extraction increases because it must maintain the fiction of native-Hebrew purity despite successful language transmission. The post-colonial plurality movements (organized/mobile) experience low χ because they see an exit path (sunset clause as multilingualism becomes politically manageable).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not resolved in this constraint — extractiveness exceeds 0.70 boundary conceptually (the reading's claim on authority is not mandated by functional necessity after ~1970), but measured extractiveness is 0.58. If we measured from the alternative readings' perspectives (liturgical_preservation_reading's view of native-generation extraction, or hybrid_coexistence_reading's view), extractiveness would likely exceed 0.70 due to institutional suppression and authority displacement. The mandatrophy in this family lies at the kernel level: which reading's mandate is legitimate? The native-generation reading claims mandate as necessary for nation-building (true 1948-1970, false post-1970). The liturgical reading claims mandate as necessary for Hebrew continuity (true continuously, suppressed 1948-present). The hybrid reading claims mandate as necessary for linguistic inclusivity (true post-1970, contradicted by native-generation dominance). The manifold of readings cannot all be simultaneously mandatory — the constraint family exhibits structural mandatrophy at the kernel level, resolved only by picking which reading to institutionalize (current: native-generation) or accepting plural readings (post-colonial plurality movements' goal).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_generation_definition_boundary,
    'What specific age threshold, family structure, or linguistic practice qualifies as ''native-generation transmission''? Where is the boundary between native generation and learned/liturgical use?',
    'Linguistic documentation of actual household language practices in early 20th-century settlement communities; comparison of acquisition data for children raised in Hebrew-dominant vs multilingual households; analysis of when the definition was formalized in educational policy.',
    'If boundary is at infancy/childhood (age 0-5): excludes liturgical and learned modes entirely. If boundary is at age 7-10: allows religious education and school-based learning, collapsing the reading''s distinction. The higher the age threshold, the less the reading excludes alternative Hebrews.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_generation_definition_boundary, empirical, 'Definition and boundary of ''native-generation transmission''').

omega_variable(
    reading_as_constructed_constraint,
    'Is the ''native-generation'' reading a discovered linguistic fact about how languages work, or a constructed institutional distinction that distinguishes nationalist ''living'' Hebrew from religious ''dead'' Hebrew for political purposes?',
    'Historical analysis of Hebrew language use in pre-nationalist periods (medieval, early modern, diaspora communities); examination of when the native-generation distinction entered Hebrew linguistic discourse (typically 1880s-1920s alongside nationalist revival); cross-cultural comparison of how other language revivals (Irish, Welsh, Arabic) framed ''living'' vs ''traditional'' language.',
    'If discovered: native-generation reading is a constraint based on linguistic reality (weaker FSM signal). If constructed: the reading is a false summit — a political boundary naturalized as linguistic fact, with identifiable beneficiaries (nationalists, Ashkenazi institutions) extracting authority from alternative framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_as_constructed_constraint, conceptual, 'Whether native-generation distinction is linguistic fact or nationalist construction').

omega_variable(
    liturgical_hebrew_genuine_extinction,
    'Did liturgical and learned Hebrew actually die out, or did the native-generation reading suppress and marginalize them as ''not real'' Hebrew despite their continued use and transmission?',
    'Documentation of post-1948 liturgical Hebrew usage in Orthodox communities, diaspora communities, and academic settings; analysis of whether liturgical Hebrew declined due to inherent linguistic limits or due to suppression (educational funding redirected, institutional prestige shifted, cultural authority transferred to secular nationalists).',
    'If liturgical Hebrew genuinely extinct: native-generation reading describes a real linguistic transition (weaker extraction signal). If suppressed but viable: the reading is extractive — it constructs the appearance of extinction through delegitimization, enabling nationalist monopoly over ''real'' Hebrew.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liturgical_hebrew_genuine_extinction, empirical, 'Whether liturgical Hebrew died out or was suppressed').

omega_variable(
    mother_tongue_spontaneity_myth,
    'How much of ''native-generation'' Hebrew is genuinely spontaneous childhood acquisition vs. shaped by institutional scripting (school, media, state broadcast, standardized language policy)?',
    'Sociolinguistic analysis of variation in Hebrew speech across generations and social groups; comparison of ''native'' speech patterns with institutional prescriptions; study of how educational policy and media standardization shaped what counted as ''natural'' native Hebrew.',
    'If high institutional shaping: ''native generation'' is itself a constructed form, not natural. The reading''s central claim — that ''real'' Hebrew is spontaneous rather than performed — collapses when the ''native'' speech is itself institutionally shaped. This reveals the reading''s suppression mechanism: it appears natural and organic while actually enforcing state-defined norms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mother_tongue_spontaneity_myth, empirical, 'Degree of institutional shaping in ''native'' Hebrew speech').

omega_variable(
    committer_kernel_ambiguity,
    'Which reading of the hebrew_living_language kernel is the operative one in state institutions: native_generation_reading (what counts as ''real'' living Hebrew), liturgical_preservation_reading (what counts as legitimate Hebrew continuity), or hybrid_coexistence_reading (both modes are equally valid)?',
    'Policy analysis of educational curricula, ministry of education statements, cultural institution funding, religious authority recognition, academic legitimacy structures, media representation of Hebrew variants. Track which institutions enforce which reading and under what circumstances readings coexist or conflict.',
    'If native_generation dominates: tangled_rope confirmed, high suppression. If all three readings coexist equally: hybrid reading is operative, constraint reclassifies to lower suppression/extraction. If readings conflict openly: constraint enters instability; perspectives may reclassify within biographical timescale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_ambiguity, empirical, 'Which reading of the living Hebrew kernel is institutionally dominant').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(native_generation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ngen_theater_t0, native_generation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ngen_theater_t10, native_generation_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(ngen_theater_t20, native_generation_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(ngen_extract_t0, native_generation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ngen_extract_t10, native_generation_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ngen_extract_t20, native_generation_reading, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(native_generation_reading, attachment_coordination).
narrative_ontology:affects_constraint(native_generation_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(native_generation_reading, hybrid_coexistence_reading).
narrative_ontology:affects_constraint(native_generation_reading, yiddish_language_suppression).
narrative_ontology:affects_constraint(native_generation_reading, orthodox_institutional_authority_displacement).

% DUAL FORMULATION NOTE:
% The native_generation_reading is one member of a constraint family (kernel: hebrew_living_language) with structural siblings. All three readings operate on the same base commitment (what is 'real' Hebrew) but produce different ε values and victim/beneficiary sets. The native_generation_reading has ε=0.58 (tangled rope). The liturgical_preservation_reading likely has ε=0.45-0.52 (also tangled rope, but with victims/beneficiaries reversed). The hybrid_coexistence_reading likely has ε=0.25-0.35 (rope-level coordination without asymmetric extraction). All three readings compete for institutional dominance. Do NOT merge them into one story — each is a structurally distinct constraint with its own perspectives, measurements, and classification logic. Link them via network.affects_constraints to model their institutional interference.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(native_generation_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
