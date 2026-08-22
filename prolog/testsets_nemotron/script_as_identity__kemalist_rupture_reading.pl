% ============================================================================
% CONSTRAINT STORY: script_as_identity__kemalist_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__kemalist_rupture_reading, []).

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
 *   constraint_id: script_as_identity__kemalist_rupture_reading
 *   human_readable: Latin Script Mandate as Secular Modernization (Kemalist Rupture Reading)
 *   domain: political/linguistic/cultural
 *
 * SUMMARY:
 *   The 1928 Turkish script reform (Law No. 1353) replaced the Arabic-based
 *   Ottoman script with a Latin-based alphabet, justified as a modernization
 *   measure enabling mass literacy and Western integration. The Kemalist
 *   rupture reading frames this as a deliberate civilizational break: the new
 *   script severs the population from the Ottoman-Islamic textual heritage,
 *   making the past illegible to the new citizenry while the state
 *   monopolizes the literacy apparatus. This reading claims zero transition
 *   costs (no incumbents to displace) and treats textual rupture as a
 *   feature, not a bug. Over three decades, the coordination function
 *   (universal literacy in a phonetically transparent script) accumulated
 *   extractive layers: the state controlled all printing, education, and
 *   publication; religious and minority education was suppressed; the Ottoman
 *   textual corpus was rendered inaccessible without state mediation. The
 *   constraint today operates as a tangled rope: genuine coordination
 *   (Turkey's literacy rate rose from ~10% to >90%) fused with asymmetric
 *   extraction (state monopoly over meaning-making, exclusion of alternative
 *   epistemologies).
 *
 * KEY AGENTS:
 *   - kemalist_state_elite: Primary agenda setter (institutional/arbitrage) — designed and enforced the reform, monopolized literacy apparatus
 *   - secular_modernist_intellectuals: Beneficiary (powerful/arbitrage) — gained epistemic authority as interpreters of the new national canon
 *   - state_education_bureaucracy: Beneficiary/agenda setter (organized/mobile) — expanded institutional reach and resources through literacy monopoly
 *   - ottoman_literate_classes: Victim (powerful/trapped) — ulema, scribes, madrasa graduates; professional obsolescence without exit
 *   - religious_educational_institutions: Victim (organized/trapped) — madrasas, vakıf schools; legally suppressed, assets seized
 *   - rural_anatolian_populations: Victim (powerless/constrained) — subjected to state literacy campaigns; gained literacy but lost textual autonomy
 *   - arabic_script_calligraphic_tradition: Victim (moderate/trapped) — centuries-old artistic/intellectual tradition rendered obsolete
 *   - minority_communities: Victim (powerless/trapped) — Armenian, Greek, Jewish, Kurdish communities; minority presses restricted, communal education curtailed
 *   - phonetic_instrumentalist_observers: Observer (analytical/analytical) — linguists who view script as neutral technology; excluded from policy
 *   - ottoman_continuity_advocates: Excluded (moderate/identity_locked) — intellectuals arguing for Arabic script as identity bearer; politically marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, 0.72).
domain_priors:suppression_score(script_as_identity__kemalist_rupture_reading, 0.78).
domain_priors:theater_ratio(script_as_identity__kemalist_rupture_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__kemalist_rupture_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__kemalist_rupture_reading, "Latin Script Mandate as Secular Modernization (Kemalist Rupture Reading)").
narrative_ontology:topic_domain(script_as_identity__kemalist_rupture_reading, "political/linguistic/cultural").

domain_priors:requires_active_enforcement(script_as_identity__kemalist_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__kemalist_rupture_reading, 'cf066282-64a7-4689-80ac-ef9f0ae2a21a').
narrative_ontology:cs_kernel_codification('cf066282-64a7-4689-80ac-ef9f0ae2a21a', formalized).
narrative_ontology:cs_authority_grounding('cf066282-64a7-4689-80ac-ef9f0ae2a21a', extraction).
narrative_ontology:cs_interpretation_layer_present('cf066282-64a7-4689-80ac-ef9f0ae2a21a').
narrative_ontology:cs_reading_relation('cf066282-64a7-4689-80ac-ef9f0ae2a21a', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('cf066282-64a7-4689-80ac-ef9f0ae2a21a', script_as_identity__phonetic_instrumentalism_reading, influences).
narrative_ontology:cs_axiom('cf066282-64a7-4689-80ac-ef9f0ae2a21a', foundational, secular_modernity_requires_civilizational_rupture).
narrative_ontology:cs_axiom_status(secular_modernity_requires_civilizational_rupture, holdable).
narrative_ontology:cs_axiom_grounding('cf066282-64a7-4689-80ac-ef9f0ae2a21a', secular_modernity_requires_civilizational_rupture, deontological).
narrative_ontology:cs_axiom('cf066282-64a7-4689-80ac-ef9f0ae2a21a', foundational, state_monopoly_literacy_is_legitimate_coordination).
narrative_ontology:cs_axiom_status(state_monopoly_literacy_is_legitimate_coordination, holdable).
narrative_ontology:cs_axiom_grounding('cf066282-64a7-4689-80ac-ef9f0ae2a21a', state_monopoly_literacy_is_legitimate_coordination, conventional).
narrative_ontology:cs_axiom('cf066282-64a7-4689-80ac-ef9f0ae2a21a', secondary, arabic_script_obstructs_turkish_phonology).
narrative_ontology:cs_axiom_status(arabic_script_obstructs_turkish_phonology, holdable).
narrative_ontology:cs_axiom_grounding('cf066282-64a7-4689-80ac-ef9f0ae2a21a', arabic_script_obstructs_turkish_phonology, empirically_contingent).
narrative_ontology:cs_reference_frame('cf066282-64a7-4689-80ac-ef9f0ae2a21a', kemalist_revolutionary_break).
narrative_ontology:cs_drift_state('cf066282-64a7-4689-80ac-ef9f0ae2a21a', contemporary_neo_ottoman_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cf066282-64a7-4689-80ac-ef9f0ae2a21a', '').
narrative_ontology:cs_kernel_id(script_as_identity__kemalist_rupture_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, kemalist_state_elite).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, secular_modernist_intellectuals).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, state_education_bureaucracy).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, ottoman_literate_classes).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, religious_educational_institutions).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, rural_anatolian_populations).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, arabic_script_calligraphic_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, rural_anatolian_populations).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, minority_communities).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, secular_nation_state_legitimacy).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, western_modernization_teleology).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, script_as_civilizational_break).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designed and enforced the 1928 script reform through Law No. 1353. Controls the Ministry of Education, state publishing houses, and the Village Institutes/People's Houses network. Collects institutional rents from monopoly over literacy definition, curriculum, and publishing. Exit is arbitrage-grade: they control the state apparatus and can reform or maintain the constraint at will.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, kemalist_state_elite, agenda_setter,
    institutional, generational, arbitrage, national).

% Gained epistemic authority as the authorized interpreters of the new national canon. Staffed the new universities, translation bureaus, and cultural institutions. Their careers and intellectual capital are built on the Latin script monopoly. Exit is arbitrage-grade: they hold prestigious positions in the new order and can operate internationally in Western languages.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, secular_modernist_intellectuals, beneficiary,
    powerful, biographical, arbitrage, national).

% Expanded from a minimal Ottoman bureaucracy to a massive national apparatus (Maarif Vekaleti, Village Institutes, People's Houses). Administers the literacy monopoly: curriculum, teacher certification, textbook approval, publishing licenses. Collects institutional resources and career advancement through the constraint. Exit is mobile: bureaucratic skills transfer, but the specific monopoly rents do not.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, state_education_bureaucracy, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__kemalist_rupture_reading, state_education_bureaucracy, agenda_setter).

% Ulema, madrasa graduates, scribes, calligraphers, Ottoman administrative elites. Their professional capital (Arabic-script literacy, Islamic legal training, calligraphic arts) was rendered obsolete overnight. No exit: the new system required Latin-script literacy and secular credentials they did not possess. Many were purged, imprisoned, or forced into menial work. Their textual heritage became illegible to their own descendants.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_literate_classes, payer,
    powerful, biographical, trapped, national).

% Madrasas, vakıf (charitable foundation) schools, and Quranic instruction networks. Legally suppressed by the 1924 Tevhid-i Tedrisat Law (unification of education) and subsequent bans on religious instruction. Assets seized, buildings repurposed. No legal exit: private religious education remained prohibited until the 1950s and is still heavily restricted. The constraint actively prevents their reproduction.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, religious_educational_institutions, payer,
    organized, generational, trapped, national).

% Subjected to intensive state literacy campaigns (Nation's Schools, Village Institutes). Gained functional literacy (coordination benefit) but only through state-controlled channels in the state-prescribed script. Lost access to Ottoman textual traditions, Sufi literature, and communal knowledge transmission. Exit is constrained: they can learn the state script but cannot access alternative literacy ecosystems. The benefit (literacy) and cost (epistemic dependency) are fused.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, rural_anatolian_populations, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__kemalist_rupture_reading, rural_anatolian_populations, beneficiary).

% Centuries-old artistic, intellectual, and spiritual tradition (hat, tezhip, calligraphic Quran production). Rendered professionally obsolete; masters lost patronage, apprenticeships ended. Preserved only as 'heritage' in state museums, stripped of living practice. No exit: the tradition requires a living transmission line the constraint severed. A few masters continued privately but without institutional recognition or succession.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, arabic_script_calligraphic_tradition, payer,
    moderate, generational, trapped, national).

% Armenian, Greek, Jewish, and Kurdish communities with their own scripts (Armenian, Greek, Hebrew, Arabic-for-Kurdish) and communal education systems. The 1928 law applied to all citizens; minority presses were restricted, communal schools brought under state curriculum control. The 1934 Surname Law and 1935 Resettlement Law further pressed assimilation. Exit is trapped: emigration was the only full exit (many did), but those who stayed had no communal literacy autonomy.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, minority_communities, payer,
    powerless, generational, trapped, national).

% Linguists and language planners who view script as neutral technology. Argue Latin script is phonetically superior for Turkish vowel harmony and agglutinative morphology. Excluded from the 1928 policy process; their technical arguments were co-opted post-hoc to justify a political rupture they did not design. They observe the constraint but have no structural position within it.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, phonetic_instrumentalist_observers, observer,
    analytical, civilizational, analytical, universal).

% Intellectuals, traditionalists, and later Islamist politicians who argue Arabic script is constitutive of Turkish-Islamic identity and historical continuity. Politically marginalized after 1928; their publications banned, organizations dissolved. Re-emerged periodically (1950s Democrat Party, 1970s National Outlook, 2000s AKP) but never regained policy influence on script. Exit is identity-locked: their self-concept is fused with the Ottoman textual heritage; adopting the Latin script framework feels like civilizational apostasy.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_continuity_advocates, excluded,
    moderate, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of mass literacy in a phonologically complex language: Turkish vowel harmony and agglutinative morphology are poorly served by Arabic script (which omits short vowels and obscures morpheme boundaries). A Latin-based alphabet with one letter per phoneme enables rapid, universal literacy acquisition — a real coordination benefit the Ottoman system failed to deliver at scale.
% TRANSFER_FUNCTION: Moves epistemic authority, publishing control, curriculum definition, and the power to mediate the past from Ottoman literate classes and religious institutions to the Kemalist state elite and secular modernist bureaucracy. The transfer is effected through: (1) mandatory Latin-script education, (2) state monopoly on textbook publishing, (3) closure of Arabic-script presses, (4) translation/selection of Ottoman corpus by state committees.
% ABSENT_VOICES: Ottoman continuity advocates (ulema, traditional intellectuals, later Islamist politicians) were structurally excluded from the 1928 congress and subsequent policy. Minority communities (Armenian, Greek, Jewish, Kurdish) had no representation in the script decision despite being subject to it. Phonetic instrumentalist linguists were consulted technically but excluded from the political framing. Their absence is maintained by the same enforcement machinery that sustains the script monopoly.
% DISAPPEARANCE_RATIONALE: If the Latin script mandate and its enforcement vanished overnight: (1) the state would lose its monopoly over literacy definition and curriculum; (2) Arabic-script education and religious instruction would re-emerge (demand exists); (3) minority communal presses and schools would revive; (4) the Ottoman textual corpus would become directly accessible without state mediation; (5) the secular modernist intellectual class would lose its epistemic gatekeeping role. The Turkish political-epistemic order would fundamentally rearrange.
% FOUNDING_PROBLEM: The Ottoman Empire faced catastrophic illiteracy (~10% literacy in 1927), a script (Arabic-based Ottoman) poorly suited to Turkish phonology, and a textual tradition accessible only to a narrow educated elite. The new Republic needed a literate citizenry for modern administration, military conscription, and national integration. The founding problem was: how to achieve universal literacy rapidly in a language whose script obstructed it?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (mass illiteracy, script-phonology mismatch) is corroborated by Ottoman census data, contemporary linguistic analyses (e.g., Hagopian 1907, Banguoğlu 1940s), and the literacy trajectory itself (10% → 90%+). However, the STATUS of the problem is contested: Kemalist beneficiaries attest it remains live (functional illiteracy persists, new media require script stability); Ottoman continuity advocates and minority communities attest it is dead (literacy achieved, constraint now serves extraction). No neutral arbiter exists — the dispute is structural.
narrative_ontology:disappearance_verdict(script_as_identity__kemalist_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__kemalist_rupture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__kemalist_rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(script_as_identity__kemalist_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__kemalist_rupture_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__kemalist_rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(script_as_identity__kemalist_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the state extracted monopoly control over the literacy apparatus, publishing, and the definition of legitimate knowledge — far exceeding the coordination cost of a phonetic script. Suppression (0.78) is high because alternatives (Arabic script education, minority presses, religious instruction) were actively prohibited, not merely outcompeted. Theater ratio (0.45) is moderate: the literacy campaign was real and effective, but a growing share of enforcement defended the state's epistemic monopoly rather than literacy itself. Accessibility collapse (0.82) is very high: the Ottoman textual universe became illegible to the new generation without state-mediated translation/selection. Resistance (0.58) is moderate: open resistance was crushed early (1925 Sheikh Said rebellion, 1930 Menemen incident); later resistance took subtle forms (private Quranic instruction, preservation of family archives). The trajectory shows extraction accumulating as the coordination function was achieved and the state pivoted to defending its monopoly.
 *
 * PERSPECTIVAL GAP:
 *   From the Kemalist state elite seat (agenda setter, institutional power, arbitrage exit), the constraint is a rope: they built a functioning literacy infrastructure from near-zero. From the Ottoman literate classes (victim, powerful but trapped), it is a snare: their professional capital was confiscated and their epistemic world erased. From rural populations (victim, powerless, constrained), it is a tangled rope: they gained literacy (coordination benefit) but only on state terms (extraction). From minority communities (victim, powerless, trapped), it is a snare: their communal literacy was suppressed. The engine computes these per-seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Kemalist state elite and secular modernist intellectuals are structural beneficiaries: they designed the constraint, control its enforcement, and collect epistemic and institutional rents. The Ottoman literate classes, religious institutions, rural populations, and minority communities are structural victims: they bear the costs of transition, lost their textual autonomy, and have no exit from the state's literacy monopoly. The state education bureaucracy sits dually: it administers the constraint (agenda setter) and expands through it (beneficiary). Rival readings (Ottoman continuity, phonetic instrumentalism) are excluded from the policy conversation — their exclusion is what the enforcement machinery maintains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mass illiteracy, need for modern communication infrastructure) was substantially solved by the 1960s — literacy exceeded 70%. Yet the script monopoly and state control over publishing persisted, accumulating extraction (censorship, ideological control, suppression of minority languages). The mandate outlived its coordination function. The constraint persists because the state elite that benefits from the epistemic monopoly also controls the apparatus that could reform it. This is classic mandatrophy: the coordination problem is dead, the arrangement persists as extraction, and the beneficiaries are the agenda setters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a kernel reading of script_as_identity, and which reading does it instantiate?',
    'Authoring metadata: this constraint is the kemalist_rupture_reading of kernel script_as_identity. Sibling readings: ottoman_continuity_reading, phonetic_instrumentalism_reading.',
    'Establishes the committer frame: the ε, beneficiary/victim structure, and classification belong to THIS reading only; other readings are separate constraints with their own structural profiles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system kernel reading identity and sibling map').

omega_variable(
    transition_cost_dispute,
    'Was the transition cost genuinely zero (no incumbents displaced) or were Ottoman literate elites systematically displaced?',
    'Historical analysis of employment records, educational institution closures, and professional displacement 1928-1935. Comparison with phonetic_instrumentalism_reading''s assessment of transition costs.',
    'If transition costs were non-zero, the claimed ''zero cost'' coordination function is falsified, increasing extractiveness and supporting snare reclassification. If genuinely zero, the coordination claim holds but asymmetric extraction remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_cost_dispute, empirical, 'Whether the Kemalist script reform displaced incumbent literate classes or occurred on a clean slate').

omega_variable(
    textual_rupture_as_feature,
    'Is the severance from Ottoman-Islamic textual heritage a deliberate feature (coordination benefit) or an extractive byproduct?',
    'Analysis of Kemalist congressional records, educational curricula 1928-1950, and state publishing priorities. Does the new script''s pedagogical material actively delegitimize the old corpus, or merely replace it?',
    'If rupture is deliberate delegitimization, extraction is higher and suppression is structural. If rupture is incidental to phonetic optimization, the coordination function is purer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_rupture_as_feature, conceptual, 'Whether textual rupture serves coordination or extraction').

omega_variable(
    state_monopoly_literacy,
    'Did the state monopolize the literacy apparatus as coordination infrastructure or as ideological control?',
    'Institutional history of Maarif Vekaleti (Ministry of Education), Village Institutes, and People''s Houses 1928-1950. Track whether alternative literacy channels (private schools, religious instruction, minority presses) were suppressed or permitted.',
    'Monopoly as coordination infrastructure supports rope/tangled_rope. Monopoly as ideological control with suppressed alternatives supports snare. The degree of suppression of minority scripts (Armenian, Greek, Hebrew, Kurdish) is decisive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_monopoly_literacy, empirical, 'Whether state literacy monopoly served universal access or ideological uniformity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__kemalist_rupture_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_tr_t0, script_as_identity__kemalist_rupture_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_tr_t5, script_as_identity__kemalist_rupture_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_tr_t10, script_as_identity__kemalist_rupture_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_tr_t15, script_as_identity__kemalist_rupture_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_tr_t20, script_as_identity__kemalist_rupture_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_tr_t25, script_as_identity__kemalist_rupture_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_tr_t30, script_as_identity__kemalist_rupture_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_be_t0, script_as_identity__kemalist_rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_be_t5, script_as_identity__kemalist_rupture_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_be_t10, script_as_identity__kemalist_rupture_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_be_t15, script_as_identity__kemalist_rupture_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_be_t20, script_as_identity__kemalist_rupture_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_be_t25, script_as_identity__kemalist_rupture_reading, base_extractiveness, 25, 0.71).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_be_t30, script_as_identity__kemalist_rupture_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_su_t0, script_as_identity__kemalist_rupture_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_su_t5, script_as_identity__kemalist_rupture_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_su_t10, script_as_identity__kemalist_rupture_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_su_t15, script_as_identity__kemalist_rupture_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_su_t20, script_as_identity__kemalist_rupture_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_su_t25, script_as_identity__kemalist_rupture_reading, suppression_requirement, 25, 0.77).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_su_t30, script_as_identity__kemalist_rupture_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__kemalist_rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__kemalist_rupture_reading, 0.1).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, script_as_identity__ottoman_continuity_reading).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, script_as_identity__phonetic_instrumentalism_reading).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, turkish_language_reform_purification).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, secular_education_monopoly).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, minority_press_restrictions).

% DUAL FORMULATION NOTE:
% This constraint (kemalist_rupture_reading) and its two siblings form the script_as_identity constraint family. Each reading instantiates a different constraint with different ε, different beneficiaries/victims, and different classifications. The kemalist reading claims coordination + extraction (tangled_rope). The ottoman continuity reading would likely classify as snare (extraction from secular/modernizing populations). The phonetic instrumentalism reading would likely classify as rope (genuine coordination with minimal extraction). They are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__kemalist_rupture_reading, institutional, 0.15).
constraint_indexing:directionality_override(script_as_identity__kemalist_rupture_reading, powerful, 0.85).
constraint_indexing:directionality_override(script_as_identity__kemalist_rupture_reading, powerless, 0.95).
constraint_indexing:directionality_override(script_as_identity__kemalist_rupture_reading, moderate, 0.75).
constraint_indexing:directionality_override(script_as_identity__kemalist_rupture_reading, organized, 0.8).
constraint_indexing:directionality_override(script_as_identity__kemalist_rupture_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
