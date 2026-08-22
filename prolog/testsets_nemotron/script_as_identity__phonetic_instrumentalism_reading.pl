% ============================================================================
% CONSTRAINT STORY: script_as_identity__phonetic_instrumentalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__phonetic_instrumentalism_reading, []).

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
 *   constraint_id: script_as_identity__phonetic_instrumentalism_reading
 *   human_readable: Phonetic Instrumentalism Reading of Turkish Script Reform (Latin Script Adoption for Vowel Harmony Transparency)
 *   domain: comparative_linguistics/political_authority/state_building
 *
 * SUMMARY:
 *   The 1928 Turkish script reform replaced the Arabic-based Ottoman script
 *   with a modified Latin alphabet. The Kemalist state framed this as a
 *   purely technical decision: Turkish has 8 vowels with systematic
 *   front/back and rounding harmony; the Arabic script's 3 vowel letters
 *   (alif, waw, ya) plus optional diacritics could not represent this
 *   transparently. The Latin alphabet, with dedicated letters for each vowel
 *   (a, e, ı, i, o, ö, u, ü), achieves near-perfect phoneme-grapheme
 *   correspondence. Literacy campaigns using the new script succeeded
 *   dramatically. This reading — 'phonetic instrumentalism' — treats script
 *   as neutral technology chosen for optimization. It depoliticizes a reform
 *   that simultaneously severed textual continuity with the Ottoman-Islamic
 *   past, displaced the ulema's interpretive authority, and encoded a new
 *   national identity oriented toward Europe. The claimed type is 'rope'
 *   (genuine coordination with minimal extraction) but the metrics reveal
 *   substantial theater (0.55) and moderate extraction (0.18) from
 *   identity-locked populations — the engine will compute per-seat
 *   divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__phonetic_instrumentalism_reading, 0.18).
domain_priors:suppression_score(script_as_identity__phonetic_instrumentalism_reading, 0.22).
domain_priors:theater_ratio(script_as_identity__phonetic_instrumentalism_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__phonetic_instrumentalism_reading, rope).
narrative_ontology:human_readable(script_as_identity__phonetic_instrumentalism_reading, "Phonetic Instrumentalism Reading of Turkish Script Reform (Latin Script Adoption for Vowel Harmony Transparency)").
narrative_ontology:topic_domain(script_as_identity__phonetic_instrumentalism_reading, "comparative_linguistics/political_authority/state_building").

domain_priors:requires_active_enforcement(script_as_identity__phonetic_instrumentalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__phonetic_instrumentalism_reading, '3a0cbe73-be0d-4194-b611-234b196e0162').
narrative_ontology:cs_kernel_codification('3a0cbe73-be0d-4194-b611-234b196e0162', formalized).
narrative_ontology:cs_authority_grounding('3a0cbe73-be0d-4194-b611-234b196e0162', extraction).
narrative_ontology:cs_interpretation_layer_present('3a0cbe73-be0d-4194-b611-234b196e0162').
narrative_ontology:cs_reading_relation('3a0cbe73-be0d-4194-b611-234b196e0162', script_as_identity__kemalist_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a0cbe73-be0d-4194-b611-234b196e0162', script_as_identity__ottoman_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('3a0cbe73-be0d-4194-b611-234b196e0162', foundational, script_is_neutral_technology).
narrative_ontology:cs_axiom_status(script_is_neutral_technology, holdable).
narrative_ontology:cs_axiom_grounding('3a0cbe73-be0d-4194-b611-234b196e0162', script_is_neutral_technology, instrumental).
narrative_ontology:cs_axiom('3a0cbe73-be0d-4194-b611-234b196e0162', foundational, phonetic_transparency_justifies_script_choice).
narrative_ontology:cs_axiom_status(phonetic_transparency_justifies_script_choice, holdable).
narrative_ontology:cs_axiom_grounding('3a0cbe73-be0d-4194-b611-234b196e0162', phonetic_transparency_justifies_script_choice, empirically_contingent).
narrative_ontology:cs_reference_frame('3a0cbe73-be0d-4194-b611-234b196e0162', technical_optimization_framework).
narrative_ontology:cs_drift_state('3a0cbe73-be0d-4194-b611-234b196e0162', contemporary_identity_politics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3a0cbe73-be0d-4194-b611-234b196e0162', '').
narrative_ontology:cs_kernel_id(script_as_identity__phonetic_instrumentalism_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, kemalist_reformers).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, literacy_campaign_administrators).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, state_education_bureaucracy).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, arabic_literate_population).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, religious_scholars_ulema).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, traditional_madrasa_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, literacy_campaign_administrators).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, phonetic_transparency_principle).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, script_neutrality_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Orchestrated the 1928 script reform as a technical modernization measure. Frame the change as purely instrumental: Latin letters map 1:1 to Turkish phonemes, making literacy acquisition faster and vowel harmony visually transparent. Collect political capital from successful literacy campaigns and international recognition of modernization.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, kemalist_reformers, agenda_setter,
    institutional, generational, arbitrage, national).

% Expanded mass schooling infrastructure using the new script. The reform simplified teacher training and textbook production, reducing per-pupil literacy costs. Bureaucrats gained institutional growth, budget authority, and professional prestige from the literacy campaign's measurable success.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, state_education_bureaucracy, beneficiary,
    institutional, biographical, mobile, national).

% Ran the 'Millet Mektepleri' (Nation's Schools) that taught adults to read in weeks instead of years. Benefited from visible success metrics and state recognition. Also bore costs: intense mobilization pressure, improvised materials, and blame for regions where literacy gains lagged.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, literacy_campaign_administrators, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__phonetic_instrumentalism_reading, literacy_campaign_administrators, payer).

% Overnight lost functional literacy in the script used for all prior legal, religious, commercial, and personal documents. Could not read new official gazettes, property deeds, or their own family correspondence without relearning. Exit was identity-locked: the old script was bound to religious practice, family history, and communal belonging; adopting the new script felt like apostasy.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, arabic_literate_population, payer,
    powerless, biographical, identity_locked, national).

% Lost institutional authority over textual interpretation when the Quranic Arabic script was displaced. Their specialized training in Arabic-script Ottoman Turkish became professionally obsolete. Trapped: the new state banned madrasas and replaced sharia courts; no alternative institutional home existed for their expertise.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, religious_scholars_ulema, payer,
    organized, generational, trapped, national).

% Excluded from the new educational and legal order. Their curriculum, certification, and communal trust were anchored in Arabic script. The reform did not negotiate their transition — it abolished their legal standing. They would have objected to the framing of script as 'neutral technology' but had no platform in the new public sphere.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, traditional_madrasa_networks, excluded,
    organized, generational, trapped, national).

% Analyze the reform as a case study in script-phonology fit. Turkish vowel harmony (8 vowels, front/back and rounding harmony) is indeed unusually transparent in Latin orthography. But they note the reform's speed, compulsion, and symbolic loading exceed what phonetic optimization alone would require.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized a writing system that maps Turkish's 8-vowel harmony system onto a 29-letter Latin alphabet with near-perfect 1:1 phoneme-grapheme correspondence, enabling rapid mass literacy acquisition and interoperable print/telegraph/typing infrastructure.
% TRANSFER_FUNCTION: Transferred literacy capital from the Arabic-literate elite (religious scholars, Ottoman bureaucrats, merchant classes) to the new state education apparatus and the Latin-literate citizenry. Moved authority over textual interpretation from the ulema to secular state institutions. Moved cultural transmission from madrasa networks to state schools.
% ABSENT_VOICES: Arabic-literate women in segregated households (excluded from both old madrasa and new state schools), Kurdish and other minority communities whose oral languages used Arabic script (their literacy was erased without replacement), diaspora Ottoman communities in the Balkans and Middle East (suddenly cut off from homeland print culture).
% DISAPPEARANCE_RATIONALE: If the Latin script mandate vanished overnight, Turkey would not revert to Arabic script — the institutional, educational, and generational investments are irreversible. But the symbolic meaning of the script would become contested: the current consensus that 'script is neutral technology' would fracture, revealing the identity-encoding function the instrumentalist reading suppresses. Competing claims about Ottoman continuity, Islamic identity, and Westernization would resurface in education, publishing, and cultural policy.
% FOUNDING_PROBLEM: The Ottoman Arabic script (with Persian additions) poorly represented Turkish vowel harmony: 8 vowels mapped to 3-4 matres lectionis, requiring diacritics rarely used in practice. Literacy rates hovered at 10-15%. The state needed a writing system that could be taught quickly to conscript armies, civilian administrators, and a peasant population to support centralized governance and national market integration.
% FOUNDING_PROBLEM_CORROBORATION: Ottoman educational statistics (Salname yearbooks) corroborate low literacy and script-phonology mismatch. But the Kemalist framing that this *required* total script replacement — rather than diacritic reform of Arabic script (as debated by Ottoman linguists like Münif Pasha) — is attested only by the reformers themselves. Independent corroboration: early Republican literacy jumps (10% to 48% in a decade) confirm the coordination function; the simultaneous abolition of Arabic script in all public life (not just schools) confirms the identity-encoding function the instrumentalist reading obscures.
narrative_ontology:disappearance_verdict(script_as_identity__phonetic_instrumentalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__phonetic_instrumentalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__phonetic_instrumentalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(script_as_identity__phonetic_instrumentalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__phonetic_instrumentalism_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__phonetic_instrumentalism_reading_tests).
:- end_tests(script_as_identity__phonetic_instrumentalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.18) reflects real but bounded costs: the Arabic-literate generation lost functional literacy, but the new script delivered measurable coordination gains (literacy speed, printing interoperability). Suppression (0.22) was intense initially (Law 1353 mandated Latin script in all public spheres within months) but decayed as the new generation became native Latin-literate. Theater ratio (0.55) is high: the 'purely technical' framing performs ideological work — it naturalizes a rupture that also served identity engineering. The coordination function (vowel harmony transparency) is real; the extraction (dispossession of Arabic-literate capital) is real; the framing obscures their entanglement.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer seat, the constraint is a rope: genuine coordination problem (vowel harmony), minimal coercion after initial transition, net positive for all. From the Arabic-literate seat, it is a snare: extraction of literacy capital, active suppression of alternatives (Arabic script banned), no exit without identity rupture. From the ulema seat, it is a tangled rope: the coordination function (literacy) is real but weaponized against their authority. The engine's per-seat computation captures this divergence; the authored claim ('rope') represents the reformer's framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Kemalist reformers and state bureaucracy are structural beneficiaries (d ~ 0.15): they gained administrative efficiency, literacy metrics, and nation-building infrastructure. Arabic-literate population and ulema are identity-locked targets (d ~ 0.85): their literacy capital was confiscated without compensation, exit was blocked by script-identity fusion. Literacy campaign administrators sit near symmetric (d ~ 0.5): they gained professional recognition but bore mobilization costs. Traditional madrasa networks are trapped/excluded (d ~ 0.9): their institutional existence was abolished. Comparative linguists are analytical observers (d ~ 0.5). The engine will compute χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (script-phonology mismatch + mass illiteracy) was substantially solved by 1950 — literacy reached ~50%. But the constraint persisted with full enforcement long after the coordination problem was solved, because the identity-encoding function (Western orientation, Ottoman rupture) became the *actual* mandate. The instrumentalist reading prevents mandatrophy detection by framing the constraint as permanently necessary technical infrastructure. If the founding problem is dead but the constraint persists at full intensity, the gap is the extraction the instrumentalist reading obscures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonetic_optimality_vs_identity_encoding,
    'Is the Latin script''s vowel harmony transparency the *cause* of its adoption, or a *post-hoc justification* for a civilizational rupture whose primary purpose was identity encoding?',
    'Counterfactual analysis: if Ottoman linguists had adopted diacritic-reformed Arabic script (as Münif Pasha proposed in 1860s), would vowel harmony transparency have been achieved without identity rupture? Historical records of the 1928 Language Congress show phonetic arguments were presented alongside explicit ''civilizational'' arguments — the weighting is the ambiguity.',
    'If phonetic optimality was the *primary* driver, the constraint is a genuine rope with incidental extraction. If identity encoding was primary and phonetics a cover, the constraint is a tangled rope or snare with the instrumentalist framing as ideological cover. The ε value (0.18) assumes the former; the theater ratio (0.55) suggests the latter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonetic_optimality_vs_identity_encoding, conceptual, 'Whether the phonetic optimization claim is causally prior to or derivative of the identity rupture.').

omega_variable(
    literacy_gains_attribution,
    'How much of the literacy jump (10% to 48% in a decade) is attributable to script change vs. simultaneous mass schooling expansion, compulsory education laws, and mobilization campaigns?',
    'Econometric decomposition using regional variation in school construction timing vs. script mandate enforcement. Compare Turkish trajectory with contemporaneous literacy campaigns in Iran (Arabic script retained) and Soviet Central Asia (Cyrillic imposed).',
    'If script change accounts for <50% of literacy gains, the coordination function is overstated in the instrumentalist reading, inflating the rope claim. If script change is the dominant factor, the low ε is warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literacy_gains_attribution, empirical, 'Disentangling script effect from state capacity effect in literacy outcomes.').

omega_variable(
    kernel_reading_relations,
    'What is the structural relationship between the phonetic instrumentalism reading and its sibling readings of the script_as_identity kernel?',
    'Analyze whether the instrumentalist reading''s core premise (''script is neutral technology'') logically forecloses the rupture/continuity readings, coexists with them as alternative framings, or influences their legitimacy conditions.',
    'Determines cs_structure.reading_relations classification: forecloses (if neutrality claim makes identity-constitutive claims incoherent), coexists_with (if all three remain live positions in different frameworks), or influences (if instrumentalist framing shifts burden of proof onto identity claims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relation of this reading to sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__phonetic_instrumentalism_reading, 1928, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t1928, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1928, 0.35).
narrative_ontology:measurement(scri_tr_t1932, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1932, 0.45).
narrative_ontology:measurement(scri_tr_t1936, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1936, 0.52).
narrative_ontology:measurement(scri_tr_t1940, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1940, 0.55).
narrative_ontology:measurement(scri_tr_t1945, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1945, 0.55).
narrative_ontology:measurement(scri_tr_t1950, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1950, 0.55).

% Extraction over time
narrative_ontology:measurement(scri_be_t1928, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1928, 0.12).
narrative_ontology:measurement(scri_be_t1932, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1932, 0.15).
narrative_ontology:measurement(scri_be_t1936, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1936, 0.18).
narrative_ontology:measurement(scri_be_t1940, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1940, 0.18).
narrative_ontology:measurement(scri_be_t1945, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1945, 0.18).
narrative_ontology:measurement(scri_be_t1950, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1950, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t1928, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1928, 0.35).
narrative_ontology:measurement(scri_su_t1932, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1932, 0.28).
narrative_ontology:measurement(scri_su_t1936, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1936, 0.22).
narrative_ontology:measurement(scri_su_t1940, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1940, 0.22).
narrative_ontology:measurement(scri_su_t1945, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1945, 0.22).
narrative_ontology:measurement(scri_su_t1950, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1950, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__phonetic_instrumentalism_reading, information_standard).
narrative_ontology:boltzmann_floor_override(script_as_identity__phonetic_instrumentalism_reading, 0.02).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__ottoman_continuity_reading).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, turkish_language_reform_1928_1935).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, ottoman_legacy_in_modern_turkey).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the script_as_identity kernel. The kemalist_rupture_reading authors higher extraction (identity rupture as primary function); the ottoman_continuity_reading authors highest extraction (continuity denial as active suppression). All three share the same referent (the 1928 Law 1353 and its enforcement) but instantiate different constraints with different ε, beneficiaries, victims, and types. The instrumentalist reading minimizes ε by framing the reform as technical optimization; the rupture reading maximizes ε by framing it as civilizational engineering; the continuity reading maximizes ε by framing it as cultural dispossession.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__phonetic_instrumentalism_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
