% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__literary_revival_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: hebrew_living_language__literary_revival_reading
 *   human_readable: Hebrew Living Language via Haskalah Literary Production
 *   domain: historical_linguistics/commitment_systems/language_revitalization
 *
 * SUMMARY:
 *   Between 1750 and 1900, Hebrew undergoes a literary revival within Jewish
 *   diaspora intellectual circles, primarily in Eastern and Western Europe.
 *   The Haskalah (Jewish Enlightenment) movement establishes Hebrew as a
 *   medium for essays, poetry, philosophy, and cultural criticism—reversing
 *   centuries of rabbinic monopoly on Hebrew's use. No community speaks
 *   Hebrew natively during this interval; literary competence is acquired
 *   through study of sacred texts and deliberate intellectual practice, not
 *   childhood acquisition. The reading claims that this written generative
 *   production constitutes a legitimate form of linguistic 'liveness,'
 *   sustaining Hebrew as a vehicle for contemporary thought even in the
 *   absence of daily speech. This is one of three competing readings of the
 *   contested kernel: the liturgical-continuity reading locates aliveness in
 *   unbroken prayer recitation; the native-generation reading holds that only
 *   native daily speech—eventually realized in 20th-century Palestine—counts
 *   as true liveness. The literary-revival reading occupies the middle
 *   ground: generative written competence, elite-restricted, no native
 *   speakers, but real intellectual productivity and cultural continuity.
 *
 * KEY AGENTS:
 *   - hebrew_literary_intellectuals: Writers, philosophers, and poets (Mendelssohn, Wessely, Krochmal, Smolenskin, and their peers) who establish Hebrew literary modernism and claim authority to define Hebrew's contemporary role.
 *   - jewish_diaspora_communities: Organized religious congregations and study circles that encounter Haskalah literature as a novel claim on Hebrew's authority and meaning, competing with their own liturgical practice.
 *   - orthodox_rabbinic_authority: Institutional custodians of sacred-text interpretation and religious law, defending their interpretive monopoly against secular literary innovation.
 *   - eastern_european_jewish_masses: Yiddish-speaking majority populations excluded from intellectual Haskalah circles and from authority over Hebrew's definition.
 *   - analytical_observer: The seat examining whether literary production constitutes true language-aliveness or retrospective vindication of a particular elite identity claim.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.12).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.08).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew Living Language via Haskalah Literary Production").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical_linguistics/commitment_systems/language_revitalization").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, '69045e9a-189c-463a-8e90-bed383e5acb9').
narrative_ontology:cs_kernel_codification('69045e9a-189c-463a-8e90-bed383e5acb9', distributed).
narrative_ontology:cs_authority_grounding('69045e9a-189c-463a-8e90-bed383e5acb9', distributed).
narrative_ontology:cs_reading_relation('69045e9a-189c-463a-8e90-bed383e5acb9', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('69045e9a-189c-463a-8e90-bed383e5acb9', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_axiom('69045e9a-189c-463a-8e90-bed383e5acb9', foundational, generative_written_competence_is_aliveness).
narrative_ontology:cs_axiom_status(generative_written_competence_is_aliveness, holdable).
narrative_ontology:cs_axiom_grounding('69045e9a-189c-463a-8e90-bed383e5acb9', generative_written_competence_is_aliveness, instrumental).
narrative_ontology:cs_axiom('69045e9a-189c-463a-8e90-bed383e5acb9', secondary, intellectual_elite_literacy_sustains_language_function).
narrative_ontology:cs_axiom_status(intellectual_elite_literacy_sustains_language_function, holdable).
narrative_ontology:cs_axiom_grounding('69045e9a-189c-463a-8e90-bed383e5acb9', intellectual_elite_literacy_sustains_language_function, empirically_contingent).
narrative_ontology:cs_reference_frame('69045e9a-189c-463a-8e90-bed383e5acb9', hebrew_as_sacred_liturgical_text_only).
narrative_ontology:cs_drift_state('69045e9a-189c-463a-8e90-bed383e5acb9', haskalah_intellectual_production_phase_end_1900, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('69045e9a-189c-463a-8e90-bed383e5acb9', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_literary_intellectuals).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, jewish_cultural_continuity).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__literary_revival_reading_tests).
:- end_tests(hebrew_living_language__literary_revival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12 at interval end) because literary production is not zero-sum; participation is open to anyone with education and commitment, and the arrangement generates real intellectual value rather than capturing pre-existing resources. Suppression is minimal (0.08) because the constraint operates through persuasion and cultural authority rather than coercion—dissent (from Orthodox authorities and Yiddish-speaking masses) is real and vocal, but the literary movement cannot force compliance. Theater ratio is low-moderate (0.18) because the written practices are genuinely productive—real poems, essays, philosophy—though they do carry performative elements (assertions of Hebrew-ness, gestures toward national identity). The measurement series show a slight rise in extractiveness and theater through the 19th century as literary production becomes more explicitly nationalistic and identity-laden, particularly post-1875 as the Zionist movement begins to invest Hebrew with territorial and sovereign claims. Accessibility collapse is low (0.25) because alternatives remain available—Yiddish, local vernaculars, and liturgical Hebrew all persist as legitimate modes of Jewish expression. Resistance is moderate (0.35) because Orthodox authorities and mass populations contest the reading's claim that literary production counts as linguistic continuity.
 *
 * PERSPECTIVAL GAP:
 *   The literary intellectual seats perceive the constraint as genuine coordination and cultural recovery: they are solving a real problem (keeping Hebrew intellectually alive and meaningful) and generating real value (a body of modern Hebrew literature). From the Orthodox rabbinic seat, the same structure appears as usurpation and dilution: secular intellectuals are claiming authority over Hebrew that belongs to the religious tradition, and their literary production is a distraction from proper Jewish study and piety. From the masses' position (structurally excluded), literary Hebrew is an elite performance with little bearing on actual Jewish life, which is conducted in Yiddish and oriented toward prayer, not philosophical essays. The engine's per-seat classification should diverge here because the structural asymmetry is real: the literary intellectuals are low-extraction beneficiaries (no coercion, open participation, real productivity); the Orthodox authorities are defending a threatened monopoly (high local extraction interest, identity-locked resistance); the masses are excluded observers with no power to shape the definition of Hebrew-ness. The authored claim (rope/coordination) matches the literati's seat well; from Orthodox and mass seats, the same constraint might compute as snare or piton (an elite practice maintained theatrically and used to claim authority over who speaks for Jewish identity).
 *
 * DIRECTIONALITY LOGIC:
 *   The literary intellectuals derive near-beneficiary directionality (d ~ 0.15–0.25) because they set the terms of the constraint, control its elaboration, and collect cultural prestige. Participation is mobile (anyone with education can join), power is organized (they form societies, publish periodicals), and no direct extraction happens—they are not coercing anyone to read their work. The diaspora communities sit near symmetric (d ~ 0.45–0.55) because they experience both genuine coordination benefit (Hebrew is recovered as a living medium) and a challenge to their authority (religious authorities' interpretive monopoly is weakened). They pay a diffuse cost in the form of narrative competition—their own liturgical continuity reading is displaced as the primary claim on Hebrew's liveness—but this is structural, not directly extractive. The Orthodox authorities occupy a constrained position (d ~ 0.60–0.75) because literary production threatens their monopoly on Hebrew meaning and their identity is locked to sacred-text authority. They resist but cannot prevent the literary movement. The masses are excluded (d not computed; not in the primary constraint structure). The analytical observer is analytical (d = 0.5 by convention).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy in the strict sense—the founding problem (keeping Hebrew alive as a generative medium) and the proposed solution (literary production) remain structurally aligned throughout the 150-year interval. However, there is a latent mandatrophy risk: if native daily speech eventually emerges (as it does in early 20th-century Palestine), the literary production constraint's mandate becomes historically contingent rather than foundational. The native-generation reading would argue that literary production was always a transitional solution, not a true aliveness marker. From the literary-revival reading's own perspective, the emergence of native speakers does not invalidate the claim that literary production was alive during the diaspora period—but it does shift the weight of evidence. The theater ratio's slight rise (0.08 → 0.18) suggests increasing performativity as Haskalah literature becomes more explicitly nationalist and identity-focused, particularly after 1875. This creep toward theater indicates the constraint is beginning to derive legitimacy from identity claims and national narrative rather than purely from the productivity of literary output itself. By 1900, approximately 18% of the constraint's appearance is theatrical rather than functional—a warning sign of incipient pitonization or identity capture, though not yet a full piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literary_production_as_native_competence,
    'Does generative written competence in Hebrew, acquired through study and intellectual practice, constitute a form of linguistic ''liveness'' equivalent to native daily speech competence?',
    'Linguistic and cognitive science: compare the cognitive structure and productivity of literary-educated Hebrew competence to native speaker competence in languages undergoing revival (e.g., Irish, Basque). Assess whether literary production without childhood acquisition can sustain the full range of linguistic generativity or only a constrained domain (learned elegance vs. spontaneous fluency).',
    'If literary competence is equivalent, the constraint''s claim (Hebrew stays alive through Haskalah production) is strong. If literary production is constrained to aesthetic/intellectual domains and lacks spontaneous everyday generativity, the constraint is a partial continuity—valuable for cultural and intellectual reasons, but not a full linguistic aliveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literary_production_as_native_competence, empirical, 'Whether written literary competence without native acquisition equals linguistic aliveness.').

omega_variable(
    elite_vs_communal_language_transmission,
    'Is Hebrew liveness maintained by an elite-restricted literary practice, or does authentic linguistic continuity require communal-level transmission and participation?',
    'Historical documentation of readership and participation: how many Jewish individuals actually engaged with Haskalah literature? What was the ratio of Hebrew literati to Hebrew readers? How did Haskalah literature circulate—in specialized journals, elite circles, or broader community networks? Ethnographic parallel: how does linguistic revival work in modern communities, and what scale of participation is necessary?',
    'If Haskalah literature reached only hundreds of intellectuals across a continent, the constraint may be more a marker of elite cultural identity than communal linguistic continuity—it would be a Snare disguised as Rope (elite authority claiming to speak for a linguistic fact that affects only them). If broader circulation is documented, the constraint''s Rope classification holds more strongly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_vs_communal_language_transmission, empirical, 'Whether literary production constitutes communal linguistic continuity or elite cultural performance.').

omega_variable(
    kernel_vs_reading_ambiguity,
    'Is the Hebrew-living-language kernel a natural fact (a language either is or is not alive) or a constructed framework whose definition is contestable (liveness is what we define it as)?',
    'Philosophical and linguistic theory: examine whether ''language aliveness'' has a kernel-independent definition (formal linguistic criteria: recursion, productivity, regular acquisition) or whether the criteria themselves are historically and culturally contingent. If the latter, the three readings (liturgical, literary, native-generation) are not competing empirical claims but incompatible framings of what language-ness itself means.',
    'If aliveness is kernel-defined (empirically testable), the literary-revival reading is a claim about facts, and modern linguistics can adjudicate it. If aliveness is framework-contingent, each reading partially constitutes the object it claims to describe—and the constraint''s type depends on which framework one adopts, not on independent classification. This moves the constraint toward conceptual omega rather than empirical resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_vs_reading_ambiguity, conceptual, 'Whether ''linguistic aliveness'' is a natural-language property or a constructed framework.').

omega_variable(
    nationalist_retroactive_vindication,
    'To what extent does the literary-revival reading''s claim that Haskalah production kept Hebrew alive reflect genuine continuity, and to what extent is it a retroactive identity narrative authored by Zionist and Israeli historians to justify 20th-century national language revival?',
    'Historical textual analysis of Haskalah literature itself (does it claim to be ''keeping Hebrew alive,'' or is this framing applied later?). Comparison with contemporary Orthodox commentary and Yiddish-movement critiques (what did dissenting voices say about Haskalah''s role?). Genealogy of the ''literary revival kept Hebrew alive'' narrative in scholarly literature: when was this framing codified, and by whom?',
    'If Haskalah writers did not frame their work as linguistic continuity but as cultural innovation, the reading is a retrospective construction—the constraint''s mandate was not ''keep Hebrew alive'' but ''make Hebrew modern,'' and the aliveness claim is a later gloss. This would elevate theater_ratio and suggest the constraint is closer to Piton (maintained by institutional narrative about historical importance) than pure Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nationalist_retroactive_vindication, empirical, 'Whether literary-revival reading reflects historical intention or retrospective nationalist framing.').

omega_variable(
    suppression_of_yiddish_by_hebrew_revival,
    'Does the literary-revival constraint actively suppress Yiddish as a vehicle for Jewish intellectual and cultural expression, or do the two languages coexist independently?',
    'Historical analysis of institutional resources, publishing, education, and prestige allocation: did Hebrew literary societies actively marginalize Yiddish literature, or did they simply pursue Hebrew in parallel? Do the same intellectual figures publish in both Hebrew and Yiddish, or are the communities separate? What is the temporal pattern of relative investment and support?',
    'If the constraint suppresses Yiddish (restricts resources, education, prestige), it is a Tangled Rope or Snare, not pure Rope—the coordination function (Hebrew recovery) is genuine, but it rides on coercive suppression of an alternative. If suppression is minimal and coexistence is real, the low suppression_requirement (0.08) is accurate. Moderate suppression would elevate the metric to 0.25–0.40.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_yiddish_by_hebrew_revival, empirical, 'Whether Hebrew literary revival actively suppresses Yiddish or permits peaceful coexistence.').

omega_variable(
    reading_boundary_liturgical_vs_literary,
    'Can liturgical Hebrew recitation and literary Hebrew production be coherently distinguished as separate constraints, or are they necessarily intertwined such that one reading should decompose into two?',
    'Textual and institutional analysis: do Maskilim literati draw on liturgical sources and sacred-text study, or is their literary practice deliberately secular and distanced from liturgy? Do synagogue communities engage with Haskalah literature, or is there institutional separation? Are the authority structures (who validates Hebrew-ness) the same or different across the two domains?',
    'If the domains are deeply intertwined (literati are educated in liturgy, draw on it creatively, and address the same communities), the literary-revival reading is a specialized case of the broader Hebrew-continuity kernel and should not be a separate story. If the domains are institutionally and epistemically separated, the reading is distinct and justifies its own constraint story with its own ε and stakeholders.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_liturgical_vs_literary, empirical, 'Whether literary and liturgical Hebrew constitute separable constraints or aspects of one kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1750, hebrew_living_language__literary_revival_reading, theater_ratio, 1750, 0.08).
narrative_ontology:measurement(hebr_tr_t1800, hebrew_living_language__literary_revival_reading, theater_ratio, 1800, 0.12).
narrative_ontology:measurement(hebr_tr_t1840, hebrew_living_language__literary_revival_reading, theater_ratio, 1840, 0.16).
narrative_ontology:measurement(hebr_tr_t1875, hebrew_living_language__literary_revival_reading, theater_ratio, 1875, 0.19).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_living_language__literary_revival_reading, theater_ratio, 1900, 0.18).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1750, hebrew_living_language__literary_revival_reading, base_extractiveness, 1750, 0.05).
narrative_ontology:measurement(hebr_be_t1800, hebrew_living_language__literary_revival_reading, base_extractiveness, 1800, 0.09).
narrative_ontology:measurement(hebr_be_t1840, hebrew_living_language__literary_revival_reading, base_extractiveness, 1840, 0.11).
narrative_ontology:measurement(hebr_be_t1875, hebrew_living_language__literary_revival_reading, base_extractiveness, 1875, 0.13).
narrative_ontology:measurement(hebr_be_t1900, hebrew_living_language__literary_revival_reading, base_extractiveness, 1900, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__literary_revival_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, information_standard).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__literary_revival_reading, 0.04).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% The hebrew_living_language kernel decomposes into three structurally distinct constraints, each making incompatible claims about what constitutes Hebrew 'liveness' in diaspora and modern contexts. The literary-revival reading (this story) asserts that generative written competence among educated elites sustains the language. The liturgical-continuity reading locates aliveness in unbroken prayer recitation. The native-generation reading holds that only native daily speech counts. These readings have different ε values (literary-revival is low-extraction; native-generation is zero-extraction, Mountain-adjacent, a future natural law; liturgical-continuity is coordination-heavy but non-extractive), different stakeholder structures, and different founding problems. They are linked via network.affects_constraints to show their mutual influence: the literary reading provides evidence and framing that the other two must engage; the native-generation reading supervenes as a critique of literary production's limitations; the liturgical reading frames the historical ground from which literary innovation departs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
