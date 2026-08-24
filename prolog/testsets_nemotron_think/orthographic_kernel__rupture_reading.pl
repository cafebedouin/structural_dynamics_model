% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__rupture_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: orthographic_kernel__rupture_reading
 *   human_readable: Turkish Script Reform as Deliberate Cultural Rupture (Rupture Reading)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The 1928 Turkish alphabet reform replaced the Arabic script with a
 *   modified Latin alphabet overnight, by law. The rupture reading — the
 *   dominant Kemalist historiography and its nationalist successors — frames
 *   this as a deliberate, necessary severance from the Ottoman-Islamic past
 *   to forge a modern, European-oriented Turkish nation. This constraint
 *   story instantiates that reading: the script change is a constraint that
 *   extracts cultural continuity from the entire pre-reform literate
 *   population (victims) and transfers epistemic authority and legitimacy to
 *   the post-reform state apparatus (beneficiaries). The coordination
 *   function (mass literacy, technological compatibility) is real but, in
 *   this reading, subordinate to the rupture function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, 0.92).
domain_priors:suppression_score(orthographic_kernel__rupture_reading, 0.88).
domain_priors:theater_ratio(orthographic_kernel__rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, accessibility_collapse, 0.94).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__rupture_reading, snare).
narrative_ontology:human_readable(orthographic_kernel__rupture_reading, "Turkish Script Reform as Deliberate Cultural Rupture (Rupture Reading)").
narrative_ontology:topic_domain(orthographic_kernel__rupture_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_kernel__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__rupture_reading, '023ec78b-0e3d-4c1e-85f5-bd35d74d0422').
narrative_ontology:cs_kernel_codification('023ec78b-0e3d-4c1e-85f5-bd35d74d0422', formalized).
narrative_ontology:cs_authority_grounding('023ec78b-0e3d-4c1e-85f5-bd35d74d0422', extraction).
narrative_ontology:cs_interpretation_layer_present('023ec78b-0e3d-4c1e-85f5-bd35d74d0422').
narrative_ontology:cs_reading_relation('023ec78b-0e3d-4c1e-85f5-bd35d74d0422', orthographic_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('023ec78b-0e3d-4c1e-85f5-bd35d74d0422', orthographic_kernel__modernization_reading, influences).
narrative_ontology:cs_axiom('023ec78b-0e3d-4c1e-85f5-bd35d74d0422', foundational, cultural_rupture_as_state_founding_act).
narrative_ontology:cs_axiom_status(cultural_rupture_as_state_founding_act, holdable).
narrative_ontology:cs_axiom_grounding('023ec78b-0e3d-4c1e-85f5-bd35d74d0422', cultural_rupture_as_state_founding_act, instrumental).
narrative_ontology:cs_axiom('023ec78b-0e3d-4c1e-85f5-bd35d74d0422', secondary, islamic_textual_authority_incompatible_with_modern_nation).
narrative_ontology:cs_axiom_status(islamic_textual_authority_incompatible_with_modern_nation, holdable).
narrative_ontology:cs_axiom_grounding('023ec78b-0e3d-4c1e-85f5-bd35d74d0422', islamic_textual_authority_incompatible_with_modern_nation, instrumental).
narrative_ontology:cs_reference_frame('023ec78b-0e3d-4c1e-85f5-bd35d74d0422', kemalist_national_founding_moment).
narrative_ontology:cs_drift_state('023ec78b-0e3d-4c1e-85f5-bd35d74d0422', contemporary_akp_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('023ec78b-0e3d-4c1e-85f5-bd35d74d0422', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, kemalist_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, new_republican_bureaucracy).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, state_education_system).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ottoman_literate_elites).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, islamic_scholarly_tradition).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, pre_reform_general_literate_public).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, minority_communities_arabic_script_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_generations).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, post_reform_generations).
narrative_ontology:constraint_vindicates(orthographic_kernel__rupture_reading, national_identity_requires_cultural_rupture).
narrative_ontology:constraint_vindicates(orthographic_kernel__rupture_reading, modernization_requires_script_break).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Orchestrated the 1928 script law, banned Arabic script in public life, created new education system in Latin script. Collected legitimacy as modernizers and nation-builders. The reform centralized state control over cultural reproduction and severed the religious establishment's textual authority.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, kemalist_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gained monopoly on literacy and administrative competence in the new script. The reform created a new bureaucratic class whose capital was Latin-script literacy, displacing the old scribal-elite class. Their career advancement depended entirely on the new script regime.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, new_republican_bureaucracy, beneficiary,
    organized, biographical, constrained, national).

% Became the sole authorized transmitter of literacy. The unified curriculum in Latin script gave the state unprecedented pedagogical control. Teachers and administrators benefited from expanded positions and state patronage; the system's legitimacy rests on the rupture narrative.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, state_education_system, beneficiary,
    institutional, generational, constrained, national).

% Lost their cultural capital overnight — decades of education in Ottoman Turkish (Arabic script) rendered professionally useless. Could not access new bureaucratic positions without retraining. Many were purged or marginalized. Their libraries, documents, and intellectual heritage became inaccessible to the next generation.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ottoman_literate_elites, payer,
    moderate, biographical, trapped, national).

% The ulema's textual authority rested on Arabic-script transmission of Quran, hadith, fiqh. The script ban severed the living chain of scholarly transmission. Religious education was driven underground or into state-controlled imam-hatip schools. The tradition's reproductive capacity was structurally damaged.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, islamic_scholarly_tradition, payer,
    organized, generational, identity_locked, national).

% Ordinary literate citizens (merchants, civil servants, newspaper readers) woke up unable to read the new official gazette, street signs, or their own correspondence. No transition period; no parallel texts. Literacy became illiteracy by state decree. The cost of re-literacization fell entirely on individuals.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, pre_reform_general_literate_public, payer,
    powerless, biographical, trapped, national).

% Armenian, Greek, and Jewish communities using Arabic script for Turkish-language texts (karamanlidika, judeo-spanish in Arabic script) lost their liturgical and communal written heritage. The reform did not accommodate minority scripts; their presses were effectively shut down or forced to transliterate.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, minority_communities_arabic_script_users, payer,
    powerless, generational, trapped, national).

% Gained universal literacy in a phonetic script aligned with spoken Turkish, enabling mass education and European scientific access. But inherited a severed past — cannot read grandparents' letters, Ottoman archives, or Islamic texts without specialized training. The rupture is experienced as both liberation and loss.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_reform_generations, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__rupture_reading, post_reform_generations, payer).

% Historians, linguists, and cultural critics analyzing the reform's effects. Some frame it as modernization success; others as cultural genocide. Their work contests the official rupture narrative but operates within the Latin-script academic infrastructure the reform created.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, contemporary_scholars_critics, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Created a unified, phonetic script enabling mass literacy, standardized education, and integration with European scientific print culture — solving the genuine coordination problem of a multi-script, low-literacy society.
% TRANSFER_FUNCTION: Transferred cultural capital, textual authority, and epistemic access from the Ottoman-Islamic literate classes to the new republican state apparatus and its educational bureaucracy. The pre-reform population paid the cost of re-literacization; the state collected the legitimacy of modernization.
% ABSENT_VOICES: The Ottoman scribal class, ulema, and minority script communities were physically present but politically silenced — their objection was treated as reactionary resistance. Kurds using Arabic script for Kurdish had no representation. The dead (Ottoman ancestors) could not speak; their exclusion is structural, not incidental.
% DISAPPEARANCE_RATIONALE: If the script law and its enforcement vanished overnight, Turkish society would not revert to Arabic script — the Latin script is now the living infrastructure. But the rupture narrative (that the break was necessary and total) would collapse. The state's founding legitimacy story would lose its central pillar. Cultural institutions built on the rupture (imam-hatip system, Ottoman studies as specialized field, nationalist historiography) would face existential reconsideration.
% FOUNDING_PROBLEM: The Ottoman Empire's multi-script, multi-lingual literary landscape was seen by nationalist modernizers as an obstacle to unitary nation-state formation: low mass literacy, religious establishment's textual monopoly, and technological incompatibility with European print and telegraphy.
% FOUNDING_PROBLEM_CORROBORATION: The republican founding elite (Atatürk, İnönü, Saffet Arıkan) attested the problem was live and required rupture — documented in TBMM records and contemporary speeches. Ottomanists (Halil İnalcık, Carter Findley) and Islamicate scholars (Ahmed El-Shamsy) attest the problem was manageable through gradual reform (as in Egypt, Iran, Central Asia) and that rupture was a political choice, not a technical necessity. Minority community archives (Hrant Dink Foundation, Patriarchate records) document the disproportionate cost on non-Muslim minorities, corroborating the extraction reading.
narrative_ontology:disappearance_verdict(orthographic_kernel__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_kernel__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__rupture_reading, 0.92, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.92 at origin) because the constraint severed access to the entire written heritage of a civilization — every book, document, letter, inscription, tombstone became illegible to the next generation without specialized training. The cost of this severance was borne entirely by the pre-reform population (re-literacization, lost professional capital, severed intergenerational transmission). Suppression is very high (0.88) because the constraint's persistence depended on active enforcement: banning Arabic script in printing, education, signage, and public life; purging the old literate class; criminalizing parallel script use. Theater ratio is moderate (0.42) because the modernization rhetoric (literacy, phonetic fit, European integration) was genuinely believed by many enforcers, yet the speed, totality, and lack of transition period reveal rupture as the primary aim. Accessibility collapse is near-total (0.94) — alternatives (Arabic script, Ottoman Turkish) were not merely discouraged but legally erased from public space. Resistance is moderate (0.48) — there was intellectual resistance (Yahya Kemal, Ahmed Hamdi Tanpınar) and popular confusion, but the state's coercive capacity overwhelmed it.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute sharply divergent seat types: from the state apparatus seat, the constraint computes as rope/scaffold (genuine coordination with sunset — the literacy mission); from the Ottoman elite and Islamic tradition seats, it computes as snare (pure extraction with coercive suppression); from the post-reform generations seat, it computes as tangled_rope (coordination benefit + cultural loss). The rupture reading's claimed_type (snare) reflects the victim seats' structural experience. The divergence IS the measurement — the constraint's classification depends on which seat you occupy.
 *
 * DIRECTIONALITY LOGIC:
 *   The Kemalist state apparatus is the structural beneficiary (d ≈ 0.1): it authored the constraint, collects its legitimacy rents, and controls the new literacy infrastructure. The new republican bureaucracy and education system are beneficiaries (d ≈ 0.2-0.3): they gained monopoly positions in the new script regime. The Ottoman literate elites, Islamic scholarly tradition, general pre-reform public, and minority script users are targets (d ≈ 0.85-0.95): they bore the full cost of severance with constrained or trapped exit. Post-reform generations are dual-positioned: beneficiaries of mass literacy (d ≈ 0.3) but payers of cultural loss (d ≈ 0.6). Contemporary scholars are analytical observers (d ≈ 0.5). The identity_locked exit of the Islamic scholarly tradition is critical — their self-concept is constituted through the Arabic-script transmission chain; exit means dissolution of the tradition itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (low literacy, script fragmentation, religious textual monopoly) was real but the rupture solution was not necessary — gradual Latinization alongside Arabic script (as in Azerbaijan's first reform, or Malaysia's dual-script period) could have solved coordination without severance. The mandate (literacy modernization) has been substantially achieved (literacy >95%), yet the rupture narrative persists as founding myth. The constraint persists not because the coordination function requires it (Latin script is now self-sustaining) but because the state's legitimacy is anchored in the rupture. This is mandatrophy: the arrangement outlived its functional justification and survives as identity-anchoring theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_kernel_reading,
    'Is this constraint one reading of the contested orthographic_kernel, and does the rupture_reading''s high ε reflect the reading''s structural commitments rather than an observer-neutral fact?',
    'Compare the ε authored here with the ε authored in continuity_reading and modernization_reading stories. If ε differs substantially across readings of the same kernel, the variance is reading-indexed — confirming the committer frame. The kernel''s referent (the 1928 law and its enforcement) is fixed; the extraction assessment varies by reading.',
    'If confirmed, the rupture_reading''s snare classification is reading-relative. The continuity_reading would likely author even higher ε (cultural genocide frame); the modernization_reading would author lower ε (coordination benefit frame). The engine''s per-seat computation would then reveal which seats align with which reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading, conceptual, 'Commitment of this story to the rupture_reading of orthographic_kernel; ε is reading-indexed per OQ-26/OQ-258.').

omega_variable(
    sibling_reading_continuity_delta,
    'What structural elements would the continuity_reading change relative to this rupture_reading?',
    'The continuity_reading would: (1) expand victims to include the dead (Ottoman ancestors whose texts are now mute), (2) name the Islamic scholarly tradition as primary victim rather than one among many, (3) deny beneficiaries — arguing the state apparatus gained only illegitimate power, not genuine coordination benefit, (4) raise suppression to ~0.98 (cultural erasure), (5) claim_type mountain (the continuity is natural law; the rupture is violent interruption).',
    'The continuity_reading would compute as snare from every non-state seat, with even higher extraction. The divergence between rupture_reading (snare, state as beneficiary) and continuity_reading (snare, state as predator) reveals the kernel''s contested beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_continuity_delta, conceptual, 'Structural delta between rupture_reading and continuity_reading on the orthographic_kernel.').

omega_variable(
    sibling_reading_modernization_delta,
    'What structural elements would the modernization_reading change relative to this rupture_reading?',
    'The modernization_reading would: (1) reduce extractiveness to ~0.4 (coordination benefit offsets cultural cost), (2) name post_reform_generations as primary beneficiaries (mass literacy, European integration), (3) reclassify Ottoman elites as ''transitional losers'' not victims (their skills were adaptable), (4) lower suppression to ~0.6 (enforcement was transitional, not permanent), (5) claim_type rope or tangled_rope (genuine coordination with transitional costs).',
    'The modernization_reading would compute as rope from the state and post-reform generation seats, tangled_rope from the Ottoman elite seat. The divergence from rupture_reading centers on whether the coordination function is primary (modernization) or cover (rupture).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_modernization_delta, conceptual, 'Structural delta between rupture_reading and modernization_reading on the orthographic_kernel.').

omega_variable(
    identity_locked_mechanism_islamic_tradition,
    'Is the Islamic scholarly tradition''s identity_locked exit mechanism professional identity (career path dependence), relational identity (self-concept through transmission chain), ideological identity (worldview making exit unthinkable), or institutional identity (organization become its function)?',
    'Historical analysis of ulema responses 1928-1950: did they attempt retraining in Latin script (professional), form underground transmission circles (relational), issue fatwas declaring the reform illegitimate (ideological), or transform into imam-hatip system (institutional)? The mechanism determines whether identity_lock persists post-state-enforcement.',
    'If ideological/relational, the tradition carries suppression internally even after state enforcement relaxes (post-1980s). If professional/institutional, the lock may have partially released with state accommodation (imam-hatip expansion, theology faculties). This affects the tradition''s current directionality and the constraint''s residual extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_mechanism_islamic_tradition, empirical, 'Identity-lock mechanism for Islamic scholarly tradition under script rupture.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal bans, policing of printing, education monopoly) or internalized (generations believing Arabic script is ''backward'', ''Ottoman'', ''un-Turkish'')?',
    'Post-1980s relaxation: when legal bans on Arabic script eased (academic Ottoman studies permitted, calligraphy revived as art), did suppression persist in self-censorship and cultural stigma? Survey data on attitudes toward Ottoman Turkish literacy among Turkish citizens would reveal internalization.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression after legal enforcement decays. This would sustain snare classification even as formal suppression drops. If purely structural, the constraint may drift toward piton as enforcement atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in the Turkish script rupture.').

omega_variable(
    founding_problem_necessity,
    'Was the cultural rupture strictly necessary to solve the founding problem (mass literacy, technological compatibility), or was it a political choice to sever the Islamic past?',
    'Counterfactual comparison: Azerbaijan (Latin 1929, Cyrillic 1939, Latin 1991), Turkmenistan (Latin 1993), Uzbekistan (Latin 1993) — all achieved mass literacy with script changes but without the same rupture narrative. Iran retained Perso-Arabic script and achieved >85% literacy. The necessity claim is testable against these cases.',
    'If unnecessary, the rupture_reading''s foundational axiom (''cultural rupture as state founding act'') is instrumental but empirically contingent — the means (rupture) were not forced by the ends (literacy/modernization). This would reclassify the axiom as empirically_contingent and potentially overridden by evidence. If necessary, the axiom holds as instrumental necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_necessity, empirical, 'Whether the rupture was functionally necessary or politically chosen.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__rupture_reading, 1928, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orthographic_kernel__rupture_reading_tr_t1928, orthographic_kernel__rupture_reading, theater_ratio, 1928, 0.35).
narrative_ontology:measurement(orthographic_kernel__rupture_reading_tr_t1935, orthographic_kernel__rupture_reading, theater_ratio, 1935, 0.45).
narrative_ontology:measurement(orthographic_kernel__rupture_reading_tr_t1950, orthographic_kernel__rupture_reading, theater_ratio, 1950, 0.5).
narrative_ontology:measurement(orthographic_kernel__rupture_reading_tr_t1980, orthographic_kernel__rupture_reading, theater_ratio, 1980, 0.48).
narrative_ontology:measurement(orthographic_kernel__rupture_reading_tr_t2000, orthographic_kernel__rupture_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(orthographic_kernel__rupture_reading_tr_t2024, orthographic_kernel__rupture_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(orthographic_kernel__rupture_reading_be_t1928, orthographic_kernel__rupture_reading, base_extractiveness, 1928, 0.95).
narrative_ontology:measurement(orthographic_kernel__rupture_reading_be_t1935, orthographic_kernel__rupture_reading, base_extractiveness, 1935, 0.93).
narrative_ontology:measurement(orthographic_kernel__rupture_reading_be_t1950, orthographic_kernel__rupture_reading, base_extractiveness, 1950, 0.88).
narrative_ontology:measurement(orthographic_kernel__rupture_reading_be_t1980, orthographic_kernel__rupture_reading, base_extractiveness, 1980, 0.82).
narrative_ontology:measurement(orthographic_kernel__rupture_reading_be_t2000, orthographic_kernel__rupture_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(orthographic_kernel__rupture_reading_be_t2024, orthographic_kernel__rupture_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(orthographic_kernel__rupture_reading_su_t1928, orthographic_kernel__rupture_reading, suppression_requirement, 1928, 0.95).
narrative_ontology:measurement(orthographic_kernel__rupture_reading_su_t1935, orthographic_kernel__rupture_reading, suppression_requirement, 1935, 0.92).
narrative_ontology:measurement(orthographic_kernel__rupture_reading_su_t1950, orthographic_kernel__rupture_reading, suppression_requirement, 1950, 0.85).
narrative_ontology:measurement(orthographic_kernel__rupture_reading_su_t1980, orthographic_kernel__rupture_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(orthographic_kernel__rupture_reading_su_t2000, orthographic_kernel__rupture_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(orthographic_kernel__rupture_reading_su_t2024, orthographic_kernel__rupture_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__rupture_reading, 0.08).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__modernization_reading).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, turkish_language_reform_purification).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, imam_hatip_education_system).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, ottoman_archive_access_policy).

% DUAL FORMULATION NOTE:
% The orthographic_kernel decomposes into three constraint stories (rupture_reading, continuity_reading, modernization_reading) per ε-invariance: each reading instantiates a different constraint with different ε, beneficiary/victim structure, and claimed_type. The rupture_reading authors ε=0.92 (snare); continuity_reading would author ε≈0.97 (snare, state as predator); modernization_reading would author ε≈0.4 (rope/tangled_rope). They share the referent (1928 law) but differ in reading-indexed assessment. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_kernel__rupture_reading, institutional, 0.1).
constraint_indexing:directionality_override(orthographic_kernel__rupture_reading, organized, 0.25).
constraint_indexing:directionality_override(orthographic_kernel__rupture_reading, moderate, 0.6).
constraint_indexing:directionality_override(orthographic_kernel__rupture_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
