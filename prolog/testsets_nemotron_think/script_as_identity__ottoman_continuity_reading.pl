% ============================================================================
% CONSTRAINT STORY: script_as_identity__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__ottoman_continuity_reading, []).

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
 *   constraint_id: script_as_identity__ottoman_continuity_reading
 *   human_readable: Arabic Script as Constitutive of Turkish-Islamic Identity (Ottoman Continuity Reading)
 *   domain: political/linguistic/religious
 *
 * SUMMARY:
 *   The Ottoman continuity reading treats Arabic script not as a mere writing
 *   system but as the constitutive boundary of Turkish-Islamic identity. From
 *   the late 19th century through the early Republic, this reading animated
 *   the resistance to script reform: the script was the vessel of the Quran,
 *   the key to the Ottoman archive, and the mark of membership in the Islamic
 *   civilizational sphere. The constraint operated through the madrasa
 *   system, the ulema's interpretive monopoly, state censorship of
 *   Latin-script publications, and the millet system's script-based communal
 *   boundaries. Its enforcement intensified as modernization pressures grew
 *   (1908 Constitutional Revolution, WWI, War of Independence), peaking in
 *   the early 1920s when the script became a front in the cultural war
 *   between the Ankara government and the Istanbul-based religious
 *   establishment. The 1928 Latin script reform was the constraint's
 *   structural collapse — not a gradual erosion but a sovereign decision by a
 *   new regime that had already seized the enforcement machinery. The claimed
 *   type (tangled_rope) reflects the dual structure: genuine coordination of
 *   a multi-ethnic imperial identity through shared scriptural access, and
 *   asymmetric extraction that concentrated literacy, authority, and archival
 *   access in a religious-bureaucratic elite while suppressing the phonetic
 *   needs of Turkish speakers and the scriptural autonomy of minorities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, 0.68).
domain_priors:suppression_score(script_as_identity__ottoman_continuity_reading, 0.78).
domain_priors:theater_ratio(script_as_identity__ottoman_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__ottoman_continuity_reading, "Arabic Script as Constitutive of Turkish-Islamic Identity (Ottoman Continuity Reading)").
narrative_ontology:topic_domain(script_as_identity__ottoman_continuity_reading, "political/linguistic/religious").

domain_priors:requires_active_enforcement(script_as_identity__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__ottoman_continuity_reading, 'b32a7874-f71d-48f0-bafb-16abb6a0e301').
narrative_ontology:cs_kernel_codification('b32a7874-f71d-48f0-bafb-16abb6a0e301', formalized).
narrative_ontology:cs_authority_grounding('b32a7874-f71d-48f0-bafb-16abb6a0e301', lineage).
narrative_ontology:cs_interpretation_layer_present('b32a7874-f71d-48f0-bafb-16abb6a0e301').
narrative_ontology:cs_reading_relation('b32a7874-f71d-48f0-bafb-16abb6a0e301', script_as_identity__kemalist_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('b32a7874-f71d-48f0-bafb-16abb6a0e301', script_as_identity__phonetic_instrumentalism_reading, coexists_with).
narrative_ontology:cs_axiom('b32a7874-f71d-48f0-bafb-16abb6a0e301', foundational, arabic_script_constitutes_islamic_identity).
narrative_ontology:cs_axiom_status(arabic_script_constitutes_islamic_identity, holdable).
narrative_ontology:cs_axiom_grounding('b32a7874-f71d-48f0-bafb-16abb6a0e301', arabic_script_constitutes_islamic_identity, deontological).
narrative_ontology:cs_axiom('b32a7874-f71d-48f0-bafb-16abb6a0e301', foundational, ottoman_textual_heritage_requires_arabic_script).
narrative_ontology:cs_axiom_status(ottoman_textual_heritage_requires_arabic_script, holdable).
narrative_ontology:cs_axiom_grounding('b32a7874-f71d-48f0-bafb-16abb6a0e301', ottoman_textual_heritage_requires_arabic_script, empirically_contingent).
narrative_ontology:cs_reference_frame('b32a7874-f71d-48f0-bafb-16abb6a0e301', ottoman_islamic_continuity_framework).
narrative_ontology:cs_drift_state('b32a7874-f71d-48f0-bafb-16abb6a0e301', early_republican_reform_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('b32a7874-f71d-48f0-bafb-16abb6a0e301', '').
narrative_ontology:cs_kernel_id(script_as_identity__ottoman_continuity_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, religious_authorities).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, traditional_ulama).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, ottoman_bureaucracy).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, arabic_script_literates).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, modernist_intellectuals).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, literacy_advocates).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, non_turkish_minorities).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, women_rural_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, traditional_ulama).
narrative_ontology:constraint_vindicates(script_as_identity__ottoman_continuity_reading, islamic_identity_requires_arabic_script).
narrative_ontology:constraint_vindicates(script_as_identity__ottoman_continuity_reading, ottoman_continuity_through_textual_heritage).
narrative_ontology:constraint_vindicates(script_as_identity__ottoman_continuity_reading, religious_authority_grounded_in_scriptural_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control religious education, fatwa issuance, and scriptural interpretation. Their authority derives from exclusive mastery of Arabic-script texts. They define the identity boundary and enforce script adherence through madrasa curricula and communal pressure. Exit means abandoning the epistemic foundation of their authority.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, religious_authorities, agenda_setter,
    institutional, generational, identity_locked, national).

% Their professional status, social prestige, and interpretive monopoly depend on Arabic-script literacy. They benefit from the constraint's maintenance of their expertise value. They also pay through the effort of maintaining a script system increasingly disconnected from spoken Turkish and from the marginalization that comes with script reform.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, traditional_ulama, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__ottoman_continuity_reading, traditional_ulama, payer).

% Administrative continuity requires Arabic-script literacy for accessing the vast Ottoman archival record (land registers, legal codes, correspondence). The script constraint preserves their institutional memory and administrative legitimacy. Exit means losing direct access to the documentary basis of their authority.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ottoman_bureaucracy, beneficiary,
    institutional, biographical, constrained, national).

% The educated class whose cultural capital is embedded in Arabic-script literacy. They benefit from the constraint's validation of their education and from gatekeeping access to religious, literary, and administrative texts. Their exit is constrained by the sunk cost of their literacy investment.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, arabic_script_literates, beneficiary,
    moderate, biographical, constrained, national).

% Advocate for Latin script to enable mass literacy, scientific modernization, and European integration. They bear the cost of exclusion from official discourse, censorship of their publications, and the structural barrier that Arabic script poses to their modernization project. Their exit is mobile — they can publish abroad, organize in journals, and eventually seize state power.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, modernist_intellectuals, payer,
    organized, generational, mobile, national).

% Focused on the pedagogical mismatch between Arabic script (poor vowel representation) and Turkish (rich vowel harmony). They bear the cost of low literacy rates, especially among women and rural populations. Their exit is constrained by the state's control of education and publishing.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, literacy_advocates, payer,
    moderate, biographical, constrained, national).

% Armenian, Greek, Jewish, and other communities whose own scripts (Armenian, Greek, Hebrew) coexisted with Ottoman Turkish in Arabic script. The identity constraint reinforces a Turkish-Islamic national frame that marginalizes their distinct identities and scripts. They are trapped — no exit from the millet system's script hierarchy without assimilating or emigrating.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, non_turkish_minorities, payer,
    powerless, generational, trapped, national).

% Systematically excluded from Arabic-script education (madrasas were male-dominated; rural access minimal). The script's complexity and gendered educational structure keep them illiterate. They are trapped by the intersection of script difficulty, patriarchal education norms, and geographic isolation.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, women_rural_populations, payer,
    powerless, biographical, trapped, national).

% The political-military faction that ultimately implements the 1928 script reform. During the constraint's active period they are excluded from the official identity framework — their secular modernization vision is treated as heresy. They have arbitrage-grade exit: they can wait for state power, build parallel institutions (Turkish Hearths, journals), and execute a top-down script change when they control the state.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, kemalist_reformers, excluded,
    powerful, generational, arbitrage, national).

% Sees the full structural field: the constraint coordinates a specific Islamic-Turkish identity against modernization pressures, extracts compliance through educational and religious gatekeeping, and suppresses alternative scripts and the populations they serve. The observer notes the coordination function (identity preservation across a multilingual empire) and the extraction function (literacy suppression, authority concentration).
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a unified Turkish-Islamic collective identity across the ethnically and linguistically diverse Ottoman domain by anchoring it in a shared scriptural tradition. The Arabic script provides access to the Quran, hadith, Islamic law, and the Ottoman administrative archive — creating a common epistemic framework that transcends vernacular differences.
% TRANSFER_FUNCTION: Moves interpretive authority and textual access from vernacular speakers to Arabic-script literates (primarily religious scholars and bureaucrats). Moves literacy acquisition cost onto populations whose spoken Turkish is poorly served by Arabic script's consonantary structure. Moves political legitimacy from secular/modernizing projects to the religious-traditional establishment that controls the script.
% ABSENT_VOICES: Women and rural populations (systematically excluded from the educational institutions that transmit Arabic-script literacy); non-Muslim minorities whose scripts and languages are subordinated to the Turkish-Islamic identity frame; future generations who would inherit a script system mismatched to their spoken language. These voices are absent because the constraint's enforcement machinery (madrasa system, state censorship, millet hierarchy) physically and legally excludes them from the deliberative space.
% DISAPPEARANCE_RATIONALE: If the Arabic-script-as-identity constraint vanished overnight, the 1928 Latin script reform would proceed without the decades of preceding debate and resistance. Mass literacy would accelerate (Turkish literacy jumped from ~10% to ~70% within two generations post-reform). Religious authority would lose its scriptural monopoly, shifting toward personal interpretation or state-controlled religious institutions. The Ottoman archival record would become inaccessible without specialized training, severing direct administrative continuity. The Turkish-Islamic identity boundary would re-form around language rather than script.
% FOUNDING_PROBLEM: How to preserve Islamic-Turkish civilizational continuity and religious authority in the face of European military, technological, and cultural penetration that threatened to dissolve the Ottoman order. The script constraint was the boundary marker that said: 'We are not them; our knowledge, law, and identity run through this script.'
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as dead by the historical fact of the Ottoman Empire's dissolution (1922) and the Republic's explicit secularization program. Ottoman reformists themselves (e.g., Münif Pasha, 1860s) documented the script's pedagogical failure for Turkish. The Kemalist reformers' own writings (Atatürk's 1928 speeches, the Language Commission records) corroborate that the continuity problem was historically superseded. No credible contemporary voice outside the traditionalist benefiting parties maintains that the Ottoman continuity problem remains live in its original form — though some Islamist intellectuals reframe it as a civilizational loss rather than a solved problem.
narrative_ontology:disappearance_verdict(script_as_identity__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__ottoman_continuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__ottoman_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(script_as_identity__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__ottoman_continuity_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__ottoman_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(script_as_identity__ottoman_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial: the constraint transfers literacy gains, interpretive authority, and archival control to Arabic-script literates while imposing high learning costs on Turkish speakers (vowel harmony mismatched by consonantary script) and excluding minority scripts. Suppression (0.78) is high: the constraint persists only through active enforcement — madrasa monopoly on education, censorship of Latin-script presses (e.g., the 1909-1911 suppression of Tercüman-ı Hakikat's Latin-script experiments), legal penalties for non-compliance. Theater ratio (0.42) is moderate: the identity-preservation function is real (the script DID coordinate a shared Islamic-Turkish consciousness across Anatolia, the Balkans, and Arab lands), but a growing share of enforcement energy defends the ulema's authority monopoly rather than the coordination function itself. Accessibility collapse (0.82) is very high within the frame: once you accept that Turkish-Islamic identity = Arabic script, alternatives (Latin script, minority scripts) appear as civilizational betrayal, not technical choices. Resistance (0.71) is strong and organized: modernist intellectuals, minority communities, and eventually the Kemalist movement mount sustained structural challenges.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (modernists, minorities, women) experience this as a snare: a script system that actively suppresses their literacy and self-representation. The agenda-setter seat (religious authorities) experiences it as a rope: a genuine coordination mechanism preserving Islamic continuity against dissolution. The engine computes this divergence from the structural data — the declared beneficiaries/victims, power asymmetries, and exit differentials. The claimed type (tangled_rope) acknowledges both experiences as structurally real: the constraint IS a coordination mechanism for one group AND an extraction mechanism for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious authorities and Ottoman bureaucracy are structural beneficiaries (d ≈ 0.15): they collect authority rents, control the interpretive pipeline, and their power grows with the constraint's enforcement. Traditional ulama and Arabic-script literates are secondary beneficiaries with partial identity-lock (d ≈ 0.25): they benefit from the expertise monopoly but pay maintenance costs. Modernist intellectuals and literacy advocates are targets with mobile/constrained exit (d ≈ 0.75): they bear the suppression cost but can organize counter-institutions. Non-Turkish minorities and women/rural populations are trapped targets (d ≈ 0.95): no exit, no voice, maximum extraction. Kemalist reformers are excluded with arbitrage exit (d ≈ 0.85 during constraint's active phase, flipping to beneficiary post-1928). The analytical observer sits at d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving Ottoman-Islamic continuity against European penetration) died with the Ottoman state (1922). The constraint persisted 6 years beyond its founding problem's death — maintained by the religious establishment's institutional inertia and the Ankara government's temporary tactical alliance with traditionalist forces during the War of Independence. The 1928 reform was the mandatrophy resolution: the new regime, having consolidated power, removed the constraint whose founding problem was dead. The theater ratio peak in 1923-1928 (0.48) captures this zombie phase: enforcement intensified even as the coordination function evaporated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (ottoman_continuity_reading) of the kernel script_as_identity. How does the committer structure — the fact that multiple readings coexist as live positions — affect the classification of this specific reading''s constraint?',
    'The engine computes per-reading classifications from each reading''s declared beneficiary/victim structure and metrics. The committer structure (which readings exist, their structural deltas) is recorded in omegas and cs_structure.reading_relations, not in the base classification. Resolution: compare the engine''s computed types across all three readings to see if the kernel produces a type family or divergent types.',
    'If all three readings compute to different types (e.g., this reading = tangled_rope, kemalist = scaffold, instrumentalist = rope), the kernel''s contestation is structurally real — not semantic. If they converge, the dispute may be framing over a shared structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Whether the kernel''s multiple readings instantiate structurally distinct constraints or framings of one constraint.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.78) primarily structural (state censorship, educational monopoly, legal penalties) or internalized (populations believing Arabic script IS their identity, such that script change feels like apostasy)?',
    'Post-reform trajectory: if suppression persists after state enforcement ends (e.g., continued preference for Arabic script in religious communities, resistance to Latin-script Quran), reclassify as partially internalized. Compare with minority communities who adopted Latin script readily vs. those who maintained Arabic-script liturgical use.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would amplify χ for identity-locked seats beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in an identity-constitutive constraint.').

omega_variable(
    coordination_extraction_boundary,
    'Is the coordination function (imperial identity unity across linguistic diversity) structurally separable from the extraction function (literacy suppression, authority concentration)? Could a Latin-script Ottoman identity have coordinated the same populations?',
    'Counterfactual: the 1928 reform replaced the script while retaining Turkish-Islamic identity markers (language, history, religion). If identity cohesion persisted post-script-change, the coordination function was script-independent and the extraction was avoidable. If identity fragmented, the script was load-bearing.',
    'If separable, this is a tangled_rope where extraction is layered on a genuine but script-agnostic coordination function. If inseparable, the high extraction is the price of the coordination itself — moving the constraint toward mountain-like necessity (from the reading''s frame) or snare-like inescapability (from victims'' frame).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable or constitutively entangled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__ottoman_continuity_reading, 1880, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(script_identity_ottoman_tr_t1880, script_as_identity__ottoman_continuity_reading, theater_ratio, 1880, 0.25).
narrative_ontology:measurement(script_identity_ottoman_tr_t1895, script_as_identity__ottoman_continuity_reading, theater_ratio, 1895, 0.3).
narrative_ontology:measurement(script_identity_ottoman_tr_t1908, script_as_identity__ottoman_continuity_reading, theater_ratio, 1908, 0.35).
narrative_ontology:measurement(script_identity_ottoman_tr_t1915, script_as_identity__ottoman_continuity_reading, theater_ratio, 1915, 0.4).
narrative_ontology:measurement(script_identity_ottoman_tr_t1923, script_as_identity__ottoman_continuity_reading, theater_ratio, 1923, 0.48).
narrative_ontology:measurement(script_identity_ottoman_tr_t1928, script_as_identity__ottoman_continuity_reading, theater_ratio, 1928, 0.42).
narrative_ontology:measurement(script_identity_ottoman_tr_t1930, script_as_identity__ottoman_continuity_reading, theater_ratio, 1930, 0.05).

% Extraction over time
narrative_ontology:measurement(script_identity_ottoman_be_t1880, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1880, 0.45).
narrative_ontology:measurement(script_identity_ottoman_be_t1895, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1895, 0.52).
narrative_ontology:measurement(script_identity_ottoman_be_t1908, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1908, 0.58).
narrative_ontology:measurement(script_identity_ottoman_be_t1915, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1915, 0.63).
narrative_ontology:measurement(script_identity_ottoman_be_t1923, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1923, 0.71).
narrative_ontology:measurement(script_identity_ottoman_be_t1928, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1928, 0.68).
narrative_ontology:measurement(script_identity_ottoman_be_t1930, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1930, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(script_identity_ottoman_su_t1880, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1880, 0.55).
narrative_ontology:measurement(script_identity_ottoman_su_t1895, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1895, 0.62).
narrative_ontology:measurement(script_identity_ottoman_su_t1908, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1908, 0.7).
narrative_ontology:measurement(script_identity_ottoman_su_t1915, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1915, 0.75).
narrative_ontology:measurement(script_identity_ottoman_su_t1923, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1923, 0.82).
narrative_ontology:measurement(script_identity_ottoman_su_t1928, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1928, 0.78).
narrative_ontology:measurement(script_identity_ottoman_su_t1930, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1930, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__ottoman_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__ottoman_continuity_reading, 0.08).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, script_as_identity__kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, script_as_identity__phonetic_instrumentalism_reading).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, ottoman_archive_access).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, religious_education_monopoly).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, millet_system_script_hierarchy).

% DUAL FORMULATION NOTE:
% This constraint (ottoman_continuity_reading) and its siblings (kemalist_rupture_reading, phonetic_instrumentalism_reading) form the script_as_identity constraint family. They share the kernel 'script as identity boundary' but instantiate different constraints with different ε values, beneficiary/victim structures, and types. The continuity reading has high ε (0.68) because it maintains a script mismatched to spoken Turkish; the rupture reading has lower ε (est. 0.35) because Latin script fits Turkish phonology but extracts via cultural rupture; the instrumentalist reading has lowest ε (est. 0.15) as a coordination standard with minimal extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__ottoman_continuity_reading, institutional, 0.15).
constraint_indexing:directionality_override(script_as_identity__ottoman_continuity_reading, organized, 0.75).
constraint_indexing:directionality_override(script_as_identity__ottoman_continuity_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
