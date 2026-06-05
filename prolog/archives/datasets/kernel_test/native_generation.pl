% ============================================================================
% CONSTRAINT STORY: native_generation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_native_generation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: native_generation
 *   human_readable: Hebrew Native Generation Requirement for Linguistic Life
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The native generation reading of Hebrew linguistic life encodes a
 *   specific constraint: a language is 'living' only if children acquire it
 *   as a native (first) language through intergenerational transmission in
 *   the home. Under this reading, Hebrew revitalization required not just
 *   restoration of a literary language but creation of conditions where
 *   Hebrew would be the native language of a population — a fundamentally
 *   different project from liturgical preservation or scholarly
 *   reconstruction. This constraint exhibits tangled_rope structure: it
 *   coordinates genuine needs (unified national identity, shared civic
 *   communication) while extracting from diaspora linguistic traditions
 *   (suppressing intergenerational transmission of Yiddish, Ladino,
 *   Judeo-Arabic, Judeo-Persian). The constraint's extractiveness has
 *   increased over the interval (0.22 → 0.58) as enforcement of
 *   Hebrew-native-generation has institutionalized through education systems,
 *   media, and civic identity claims. The theater ratio has risen modestly
 *   (0.25 → 0.48), indicating that enforcement has become somewhat
 *   performative — the primary coordination work (establishing a
 *   Hebrew-speaking population) was achieved by the 1980s, yet the constraint
 *   persists through ritual affirmation of 'Hebrew revival' identity
 *   mythology. The suppression value (0.68) reflects significant barriers to
 *   diaspora language transmission: education system pressure, employment
 *   language requirements, social shame and legitimacy penalties, and the
 *   internalized identity-fusion of Jewish identity with Hebrew native
 *   speaker status. This is a commitment-system constraint grounded in the
 *   kernel 'Hebrew as living language' — the state apparatus has
 *   institutionalized one reading of what constitutes linguistic life,
 *   foreclosing other readings.
 *
 * KEY AGENTS:
 *   - Hebrew State Apparatus: Primary beneficiary (institutional/arbitrage) — consolidates national identity, centralizes cultural authority, establishes Hebrew as sole language of state legitimacy
 *   - Ashkenazi Cultural Establishment: Co-beneficiary (institutional/arbitrage) — Ashkenazi Hebrew pronunciation and usage becomes the hegemonic standard; diaspora Ashkenazi identity becomes synonymous with Israeli Hebrew speaker identity
 *   - Diaspora Linguistic Traditions (Yiddish, Ladino, Judeo-Arabic speakers): Primary victim (powerless/identity_locked or moderate/constrained) — intergenerational transmission suppressed; native language competence becomes stigmatized; children forced to choose between home language and civic belonging
 *   - Immigrant Parents: Secondary victim (moderate/constrained) — face coordination benefit (Hebrew fluency enables integration) alongside extraction (suppression of home language, pressure for linguistic assimilation)
 *   - Language Minority Advocates: Organized agent (organized/constrained) — attempt to maintain minority language infrastructure but constrained by state apparatus; benefit from Hebrew coordination but bear extraction of suppressed alternatives
 *   - Post-Revival Academy: Institutional observer (institutional/arbitrage) — maintains native generation requirement through pedagogical ritual; has achieved primary function (Hebrew-speaking population exists) but continues enforcement through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(native_generation, 0.58).
domain_priors:suppression_score(native_generation, 0.68).
domain_priors:theater_ratio(native_generation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(native_generation, extractiveness, 0.58).
narrative_ontology:constraint_metric(native_generation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(native_generation, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(native_generation, tangled_rope).
narrative_ontology:human_readable(native_generation, "Hebrew Native Generation Requirement for Linguistic Life").
narrative_ontology:topic_domain(native_generation, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(native_generation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(native_generation, '7fb703a6-7fc1-4fb1-b576-8b035ffb51d8').
narrative_ontology:cs_created_at('7fb703a6-7fc1-4fb1-b576-8b035ffb51d8', '').
narrative_ontology:cs_kernel_codification('7fb703a6-7fc1-4fb1-b576-8b035ffb51d8', formalized).
narrative_ontology:cs_authority_grounding('7fb703a6-7fc1-4fb1-b576-8b035ffb51d8', extraction).
narrative_ontology:cs_interpretation_layer_present('7fb703a6-7fc1-4fb1-b576-8b035ffb51d8').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(native_generation, hebrew_state_apparatus).
narrative_ontology:constraint_beneficiary(native_generation, ashkenazi_cultural_establishment).
narrative_ontology:constraint_victim(native_generation, diaspora_linguistic_traditions).
narrative_ontology:constraint_victim(native_generation, multilingual_jewish_identity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIASPORA SPEAKER (SNARE) — A native speaker of Yiddish, Ladino, or Judeo-Arabic cannot transmit their native tongue to children without violating the educational and social pressure toward Hebrew. Structurally mobile (could choose to teach native language) but identity-locked: Jewish identity is increasingly constituted through Hebrew native speaker status post-1948. Exit from Hebrew-native-generation would require abandoning the legitimacy claim to modern Jewish belonging. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(native_generation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: IMMIGRANT PARENT (TANGLED ROPE) — Bears genuine coordination benefits (integration into Israeli labor market and social institutions requires Hebrew fluency; coordination of mixed-language households is real work). Also bears asymmetric extraction: education system and employment pressures suppress home language transmission; social mobility tied to linguistic assimilation. Constrained by economic dependency and social belonging. Moderate power with significant extraction but not maximal — some agency in language choice within household.
constraint_indexing:constraint_classification(native_generation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE LANGUAGE AUTHORITY (ROPE) — Benefits from Hebrew native generation through unified national identity and centralized cultural authority. Experiences the constraint as coordination: spreading native speaker population is solving the genuine problem of enabling shared civic communication. Net beneficiary with arbitrage options (can shift language policy, invest in Hebrew revival, adjust enforcement). The coordination function is real — Hebrew native speakers enable interstate commerce, military coordination, democratic deliberation.
constraint_indexing:constraint_classification(native_generation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LANGUAGE MINORITY ADVOCATE (TANGLED ROPE) — Organized agents (Yiddish cultural organizations, Sephardic heritage groups) benefit from the linguistic infrastructure (they can use Hebrew for inter-community coordination) but bear extraction of suppressed intergenerational transmission. Constrained by institutional power differential: minority culture groups have limited authority over education and media but still depend on them. Can organize but cannot override state apparatus. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(native_generation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POST-REVIVAL ACADEMY (PITON) — Hebrew as resurrected language has performed its primary coordinating function (enabling shared national identity, establishing Jewish state legitimacy). The constraint persists through institutional inertia and identity mythology rather than active function. Academic study of Hebrew as native language requirement has become substantially theatrical — the constraint is maintained through ritual affirmation of 'Hebrew revival' narrative rather than through necessary enforcement. Theater ratio elevated by constant pedagogical reinforcement of why Hebrew matters, even as the practical coordination is largely achieved.
constraint_indexing:constraint_classification(native_generation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalizing perspective, all languages require native speaker transmission for survival; this is a law of linguistic life itself. No language persists without intergenerational transmission by native speakers. The constraint appears as an immutable property of how language works. However, this perspective naturalizes what is actually a policy choice: many multilingual societies sustain multiple languages through different transmission mechanisms. The mountain classification is a false summit — the engine will identify the beneficiary declarations and reclassify to reveal the contingent institutional arrangement.
constraint_indexing:constraint_classification(native_generation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(native_generation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(native_generation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(native_generation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(native_generation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(native_generation, TR),
    TR >= 0.70.

:- end_tests(native_generation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high. The native generation reading embeds substantial extraction: suppression of diaspora language transmission, economic penalties for non-Hebrew education paths, social legitimacy penalties for parents choosing alternative languages, psychological internalization of Hebrew as the only 'living' form of Jewish linguistic identity. However, the extraction is not maximal (snare-level) because genuine coordination benefits exist — Hebrew native generation does enable unified national identity and shared civic communication that benefits the entire polity. The trajectory from 0.22 (1880s, when Hebrew revival was one option among many for Zionist projects) to 0.58 (1970s-present, when Hebrew-native-generation is institutionalized and enforced) reflects accumulating extraction as the constraint became institutionalized. Suppression (0.68): Moderate-high. Significant barriers to diaspora language transmission include: (a) structural: education system requires Hebrew, employment favors Hebrew monolingualism, media is Hebrew-dominant; (b) internalized: diaspora parents internalize shame about their native language, identity-lock themselves into Hebrew-only parenting, experience psychological cost of choosing minority language transmission. The suppression is both external and internalized, making exit genuinely difficult even when material barriers might be overcome. Theater ratio (0.45): Moderate. The constraint involves real coordination work but also performative elements. The actual functional need (having a Hebrew-speaking population for shared communication) is largely satisfied post-1950; subsequent enforcement is increasingly theatrical — maintained through education requirements, media dominance, and identity mythology rather than practical necessity. The theater has risen over the interval as the constraint shifted from coordinating genuine preference (early immigrant adoption) to enforcing identity compliance (contemporary Hebrew-native-speaker requirement).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival collapse and divergence. The state apparatus sees pure coordination (Rope) — solving the genuine problem of shared communication. The diaspora speaker sees maximal extraction (Snare) — trapped by both material suppression and identity-fusion. The immigrant parent sees mixed experience (Tangled Rope) — benefits from Hebrew coordination but bears extraction of home language. The language minority advocate sees organized constraint (Tangled Rope) — can articulate the problem but cannot overcome state apparatus. The post-revival academy sees its own degraded function (Piton) — institutional theater maintaining a constraint that achieved its coordination purpose 50+ years ago. The analytical observer risks naturalizing the constraint (Mountain) — treating 'languages live through native generation' as a law of linguistics rather than a policy choice. The perspectival gap reveals that the same structural phenomenon is experienced as law (from outside the constraint), as coordinated preference (from the beneficiary position), as binding extraction (from the trapped position), and as institutional inertia (from the degraded institution position).
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus and Ashkenazi cultural establishment benefit from Hebrew native generation (low d, negative/low χ). Diaspora speakers and immigrant parents bear costs (high d, high χ). The critical directionality distinction: diaspora speakers classified as 'identity_locked' rather than 'constrained' because the binding mechanism is cognitive rather than purely material. A diaspora speaker of Yiddish is structurally mobile (can choose home language transmission) but identity-locked: Jewish identity post-1948 is constituted through Hebrew native speaker status. Exit would require becoming 'not Jewish' in the modern sense — not a material barrier but an identity-level bind. This produces a perspectival gap: from the diaspora speaker's identity-locked position, the constraint appears as mountain (unchangeable because bound to identity itself); from the analytical position, the constraint is revealed as tangled_rope (a contingent institutional arrangement, not a law of linguistics). Language minority advocates are organized but constrained — they have power to advocate and organize but cannot override state apparatus authority over education, employment, media.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies how mandatrophy resolution requires indexical analysis. The base question 'Is Hebrew native generation a coordination mechanism or an extraction mechanism?' cannot be answered globally — it depends entirely on the observer's structural position. The state apparatus experiences pure coordination (Rope). The diaspora speaker experiences pure extraction (Snare). Both perspectives are genuine accounts of the same institutional structure because they report different extraction directions and different exit options. The mandatrophy resolves by showing that 'the constraint' is not a single phenomenon but a presheaf over multiple observation points, each instantiating a different classification. The false summit detection (analytical observer) reveals that naturalizing this as a law of linguistics — 'all languages require native speakers' — conceals the policy choice (which reading of 'Hebrew living language' is enforced).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_speaker_definition_boundary,
    'Where is the boundary between native speaker (acquired in childhood as first language) and fluent speaker (acquired later, used as primary language)? Does functional native fluency acquired at age 6 vs age 3 have different civic legitimacy?',
    'Longitudinal analysis of multilingual children''s development; sociolinguistic survey of who identifies as ''native speaker'' and how that affects social positioning; analysis of official definitions used in education and civil service',
    'If boundary is cognitive/linguistic: constraint may be slightly less extractive (age 6 fluency sufficient for coordination). If boundary is identity/civic: constraint is more extractive (only earliest childhood acquisition counts, excluding immigrants, minorities, returnees). This affects suppression value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_definition_boundary, empirical, 'Definitional boundary for native speaker status').

omega_variable(
    multilingual_maintenance_feasibility,
    'In contemporary Israel, is it structurally possible for a diaspora linguistic tradition (Yiddish, Ladino, Judeo-Arabic) to be maintained as a living language ALONGSIDE Hebrew native generation, or does state infrastructure force a binary choice?',
    'Comparative analysis of multilingual policy frameworks; documentation of actual language outcomes in families that attempted bilingual transmission; economic cost analysis of maintaining educational infrastructure for minority languages',
    'If multilingualism is feasible: the constraint is less purely extractive (coordination of multiple-language speakers is possible). If binary choice is structural: the constraint is more extractive (suppression includes foreclosure of alternatives). This directly affects beneficiary/victim declarations and chi calculation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(multilingual_maintenance_feasibility, empirical, 'Whether Hebrew native generation and diaspora language maintenance are compatible').

omega_variable(
    kernel_reading_contest_hebrew_living_language,
    'Is ''Hebrew living language'' constituted through NATIVE GENERATION (this reading), LITURGICAL PRESERVATION (scholarly tradition, daily prayer, historical continuity), or SCHOLARLY RECONSTRUCTION (academic corpus management)? Each reading instantiates a different constraint with different ε, different beneficiaries, different victim sets.',
    'Historical analysis of what actually maintained Hebrew through diaspora (liturgy, scholarship, or native speaker enclaves?); comparative linguistics of pre-modern Hebrew stability vs modern revival; contemporary sociolinguistic tracking of which transmission mechanism dominates.',
    'NATIVE GENERATION reading (this story): ε=0.58, tangled_rope, suppresses diaspora languages, benefits state apparatus. LITURGICAL PRESERVATION reading: ε≤0.25, likely rope or mountain (unchanging ritual), no suppression of alternatives, benefits religious community. SCHOLARLY RECONSTRUCTION reading: ε≤0.15, likely rope (coordination of academic tradition), low extraction. The kernel is contested among these readings — they have incompatible extractiveness values and incompatible victim/beneficiary structures. This omega documents that THIS story instantiates one reading only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_hebrew_living_language, conceptual, 'This constraint is one reading of the contested kernel ''Hebrew as living language''').

omega_variable(
    identity_locked_exit_cost,
    'For a diaspora parent choosing to transmit Yiddish as native language to children, what proportion of the exit cost is structural (economic penalty from non-Hebrew education) vs identity/psychological (loss of Jewish legitimacy, sense of betrayal of Zionist project, shame internalized from education system)?',
    'Qualitative interviews with diaspora families who chose minority language transmission; analysis of family narratives describing the decision; measurement of actual economic penalties vs reported psychological costs; comparison with families who experienced similar material costs in other contexts but without identity-fusion',
    'If substantially internalized: suppression value is accurate (0.68) — the agent carries the constraint even after removing structural barriers. If substantially structural: suppression value understates the exit cost if barriers were removed. This affects the identity_locked vs constrained distinction and the effective suppression after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_cost, empirical, 'Composition of exit cost for diaspora language transmission: structural vs internalized').

omega_variable(
    revival_sustainability_mechanism,
    'Modern Hebrew revitalization succeeded through what mechanism: intentional state language planning and compulsory education, or organic social preference for Hebrew among immigrants seeking integration? Did the constraint CREATE the demand or ENFORCE existing demand?',
    'Historical analysis of early Hebrew adoption rates (pre-state) vs post-state education; comparison of adoption trajectories in communities with high state enforcement vs low state enforcement; analysis of original immigrant preferences vs evolved preferences',
    'If native generation demand is organic: the constraint is less extractive (it coordinates genuine preference). If demand was created through enforcement: the constraint is more extractive (suppression is higher, theater is lower — it''s effective, not performative). This affects the tangled_rope diagnosis and whether coordination is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revival_sustainability_mechanism, empirical, 'Whether Hebrew native generation demand is organic or enforcement-created').

omega_variable(
    false_summit_natural_law_claim,
    'Is the claim ''a language lives only through native speaker generation'' (the kernel statement) a law of linguistics or a definitional stipulation that could be revised? Are liturgical traditions and scholarly corpora insufficient for a language to be considered ''living''?',
    'Linguistic philosophy analysis of what constitutes ''living language'' vs ''dead language''; case studies of languages maintained primarily through scholarship and ritual (Classical Arabic, liturgical Aramaic, scholarly Hebrew itself pre-revival) and whether they should count as ''living''; examination of whether the definition has changed over time',
    'If ''living language'' requires native generation: the mountain perspective is justified (though still potentially a false summit). If ''living language'' can include scholarly and liturgical transmission: the definition is stipulative and serves the native-generation-extraction mechanism. This is the meta-level false summit detection — the kernel statement itself may be a naturalized contingency, not a natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether ''language lives only through native generation'' is law or stipulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(native_generation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nati_tr_t0, native_generation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nati_tr_t15, native_generation, theater_ratio, 15, 0.35).
narrative_ontology:measurement(nati_tr_t30, native_generation, theater_ratio, 30, 0.45).
narrative_ontology:measurement(nati_tr_t50, native_generation, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(nati_be_t0, native_generation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(nati_be_t15, native_generation, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(nati_be_t30, native_generation, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(nati_be_t50, native_generation, base_extractiveness, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(native_generation, identity_coordination).
narrative_ontology:affects_constraint(native_generation, liturgical_hebrew_preservation).
narrative_ontology:affects_constraint(native_generation, yiddish_intergenerational_transmission).
narrative_ontology:affects_constraint(native_generation, ladino_diaspora_language_survival).

% DUAL FORMULATION NOTE:
% Hebrew living language is a contested kernel with three structurally distinct readings: (1) native_generation (this story, ε=0.58, tangled_rope), (2) liturgical_preservation (separate story, ε≤0.25, rope), (3) scholarly_reconstruction (separate story, ε≤0.15, rope). Each reading has different beneficiaries, different suppression mechanisms, and different victim sets. The stories are linked as alternatives (sibling readings of the same kernel), not as causal dependencies. A single agent (e.g., a diaspora Hebrew community) might simultaneously instantiate constraints from multiple readings, but each reading is a self-contained constraint story with its own ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(native_generation, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
