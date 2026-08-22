% ============================================================================
% CONSTRAINT STORY: script_as_identity__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   domain: comparative_linguistics/political_authority/state_building
 *
 * SUMMARY:
 *   This story authors the Ottoman continuity reading of the contested script
 *   kernel: Arabic script is not a neutral orthographic vehicle for Turkish
 *   but a constitutive element of Turkish-Islamic civilizational identity,
 *   preserving unbroken access to Ottoman religious, legal, and
 *   administrative textual tradition. On this reading, replacing the script
 *   does not merely change how Turkish is written — it severs a living chain
 *   of interpretive custody linking the population to its own institutional
 *   and religious past. As instructed by the kernel-reading rules, this file
 *   evaluates ONLY this reading's own standing arrangement
 *   (late-Ottoman/early-Republican script-gated authority) as this reading's
 *   own lights assess it — high extraction and high suppression cost, because
 *   from this reading's own vantage the arrangement's real function
 *   (civilizational transmission) is bundled with a heavy, defensible cost in
 *   literacy exclusion that the reading treats as regrettable but necessary,
 *   not as evidence against the arrangement's legitimacy. The sibling
 *   readings (Kemalist rupture, phonetic instrumentalism) are NOT described
 *   here — they are separate constraint files linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - ulema_religious_authorities: primary agenda-setter and beneficiary — controls the interpretive gate the script constitutes
 *   - ottoman_bureaucratic_lineage_holders: beneficiary — holds accumulated document-access advantage
 *   - sufi_orders and traditional_madrasa_educators: beneficiaries — institutional continuity depends on script continuity
 *   - rural_turkish_speakers_excluded_from_arabic_literacy, women_excluded_from_religious_education_infrastructure, non_arabic_literate_provincial_administrators: payers — bear the literacy-gatekeeping cost the continuity claim treats as sacred inheritance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, 0.58).
domain_priors:suppression_score(script_as_identity__ottoman_continuity_reading, 0.71).
domain_priors:theater_ratio(script_as_identity__ottoman_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__ottoman_continuity_reading, "Arabic Script as Constitutive of Turkish-Islamic Identity (Ottoman Continuity Reading)").
narrative_ontology:topic_domain(script_as_identity__ottoman_continuity_reading, "comparative_linguistics/political_authority/state_building").

domain_priors:requires_active_enforcement(script_as_identity__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__ottoman_continuity_reading, 'ffc6c477-cc11-4731-9b4f-ff8cdc464a0f').
narrative_ontology:cs_kernel_codification('ffc6c477-cc11-4731-9b4f-ff8cdc464a0f', distributed).
narrative_ontology:cs_authority_grounding('ffc6c477-cc11-4731-9b4f-ff8cdc464a0f', lineage).
narrative_ontology:cs_interpretation_layer_present('ffc6c477-cc11-4731-9b4f-ff8cdc464a0f').
narrative_ontology:cs_reading_relation('ffc6c477-cc11-4731-9b4f-ff8cdc464a0f', script_as_identity__kemalist_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('ffc6c477-cc11-4731-9b4f-ff8cdc464a0f', script_as_identity__phonetic_instrumentalism_reading, coexists_with).
narrative_ontology:cs_axiom('ffc6c477-cc11-4731-9b4f-ff8cdc464a0f', foundational, script_is_constitutive_of_religious_legal_identity).
narrative_ontology:cs_axiom_status(script_is_constitutive_of_religious_legal_identity, holdable).
narrative_ontology:cs_axiom_grounding('ffc6c477-cc11-4731-9b4f-ff8cdc464a0f', script_is_constitutive_of_religious_legal_identity, deontological).
narrative_ontology:cs_axiom('ffc6c477-cc11-4731-9b4f-ff8cdc464a0f', secondary, civilizational_continuity_requires_unbroken_textual_custody).
narrative_ontology:cs_axiom_status(civilizational_continuity_requires_unbroken_textual_custody, holdable).
narrative_ontology:cs_axiom_grounding('ffc6c477-cc11-4731-9b4f-ff8cdc464a0f', civilizational_continuity_requires_unbroken_textual_custody, conventional).
narrative_ontology:cs_reference_frame('ffc6c477-cc11-4731-9b4f-ff8cdc464a0f', ottoman_islamic_scriptural_continuity).
narrative_ontology:cs_drift_state('ffc6c477-cc11-4731-9b4f-ff8cdc464a0f', post_1928_alphabet_reform, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('ffc6c477-cc11-4731-9b4f-ff8cdc464a0f', '').
narrative_ontology:cs_kernel_id(script_as_identity__ottoman_continuity_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, ulema_religious_authorities).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, ottoman_bureaucratic_lineage_holders).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, sufi_orders).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, traditional_madrasa_educators).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, rural_turkish_speakers_excluded_from_arabic_literacy).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, women_excluded_from_religious_education_infrastructure).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, non_arabic_literate_provincial_administrators).
narrative_ontology:constraint_vindicates(script_as_identity__ottoman_continuity_reading, turkish_islamic_civilizational_continuity_doctrine).
narrative_ontology:constraint_vindicates(script_as_identity__ottoman_continuity_reading, ottoman_institutional_legitimacy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Quranic education, issues fatwas, and controls religious legal interpretation, all of which depend on Arabic-script literacy as the gateway to sacred and juridical texts. Their institutional authority is inseparable from the script: a Turkish population literate only in Latin script cannot independently access the Quran, hadith, or centuries of Ottoman fiqh commentary without mediation through them. They set and defend the arrangement by insisting the script IS the tradition, not merely its container.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ulema_religious_authorities, agenda_setter,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__ottoman_continuity_reading, ulema_religious_authorities, beneficiary).

% Former Ottoman civil servants, archivists, and their descendants whose professional and social standing rests on their fluency in Ottoman Turkish written in Arabic script. They retain privileged access to centuries of land records, waqf (endowment) documents, and administrative precedent that a script-illiterate successor state cannot read without their mediation. A script change devalues their accumulated expertise overnight.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ottoman_bureaucratic_lineage_holders, beneficiary,
    powerful, generational, identity_locked, national).

% Transmit devotional and mystical texts, genealogies of spiritual authority, and initiatory literature written in Arabic script across generations of disciples. Their continuity as institutions depends on script continuity; a rupture threatens to sever living orders from their own textual patrimony within a generation.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, sufi_orders, beneficiary,
    organized, civilizational, identity_locked, regional).

% Teach reading, writing, and religious sciences using Arabic-script primers and classical curricula. Their pedagogical role and livelihood are constituted by the script; without it they have no distinct professional function relative to secular schoolteachers.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, traditional_madrasa_educators, beneficiary,
    moderate, generational, identity_locked, regional).

% Speak Turkish but never achieved functional Arabic-script literacy under the Ottoman system, which required years of dedicated study poorly suited to representing Turkish's vowel-rich phonology. Under the continuity arrangement they remain locked out of both religious textual authority and civil administration, dependent on literate intermediaries (imams, scribes) for basic transactions and legal understanding. Preserving the script preserves their exclusion as a structural feature, not an accident.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, rural_turkish_speakers_excluded_from_arabic_literacy, payer,
    powerless, biographical, trapped, regional).

% Systematically excluded or minimally admitted to madrasa-based Arabic literacy instruction under the late Ottoman social order. The continuity arrangement, by keeping textual authority gated behind Arabic-script training controlled by male religious institutions, reproduces their exclusion from independent scriptural and legal interpretation across generations.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, women_excluded_from_religious_education_infrastructure, payer,
    powerless, biographical, trapped, national).

% Administer provincial affairs but lack the deep Arabic-script training of the central bureaucratic elite, forcing continual dependence on scribes and religious-legal experts to interpret documents and rulings that govern their own jurisdictions. Their practical authority is capped by literacy gatekeeping they cannot bypass without years of specialized study.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, non_arabic_literate_provincial_administrators, payer,
    moderate, biographical, constrained, regional).

% Study script reforms across Turkish, Vietnamese, Korean, and other cases to assess whether script changes cause civilizational rupture or merely relocate institutional memory into translated and transliterated forms accessible to new specialists.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, comparative_historians_of_script_reform, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Arabic script coordinates access to a continuous body of religious, legal, and administrative texts (Quran, hadith, fiqh commentary, Ottoman land and waqf records) across generations, allowing religious and bureaucratic authorities to maintain a stable, verifiable chain of interpretive custody over sacred and legal material without each generation re-deriving meaning from scratch.
% TRANSFER_FUNCTION: Moves interpretive authority and practical literacy-gated power from the general Turkish-speaking population (especially rural, female, and provincially peripheral populations) to the ulema, Sufi orders, madrasa educators, and Ottoman-trained bureaucratic elites who alone possess functional Arabic-script fluency — the script functions as a rent-collecting gate on religious and civil legitimacy.
% ABSENT_VOICES: The rural Turkish-speaking majority and women excluded from religious education infrastructure would object that the continuity claim treats their exclusion as sacred inheritance rather than as a literacy bottleneck; they are not present in the ulema's or the Ottoman bureaucratic class's account of what the script preserves, because their absence from Arabic literacy is precisely what the arrangement depends on to sustain gatekeeper authority.
% DISAPPEARANCE_RATIONALE: If Arabic-script literacy requirements vanished as the basis of religious and civil authority, the ulema's interpretive monopoly, Sufi orders' generational transmission structure, and the Ottoman bureaucratic class's document-access advantage would all lose their exclusive rents within a generation; mass literacy would become achievable without years of specialized script training, redistributing interpretive and administrative access to the previously excluded majority — which is exactly what the 1928 Turkish alphabet reform in fact produced.
% FOUNDING_PROBLEM: In the reading's own terms, Arabic script was adopted centuries earlier to bind Ottoman Turkish administration and religious life into the wider Islamic civilizational and scholarly order, ensuring doctrinal fidelity, legal continuity with Islamic jurisprudence, and institutional memory across Ottoman successor generations.
% FOUNDING_PROBLEM_CORROBORATION: Ulema and Ottoman-lineage bureaucratic elites (the constraint's own beneficiaries) attest the founding problem — civilizational and doctrinal continuity — remains live and require the script to serve it. Independent linguistic historians and the post-1928 literacy record (mass Turkish literacy rose sharply within a decade of the Latin alphabet reform, per UNESCO and Turkish state education statistics from outside the ulema/bureaucratic seats) corroborate instead the phonetic-instrumentalist and rupture readings' claim that the 'continuity' function was severable from literacy access, and that most of what the script actually preserved for the general population was exclusion, not access to meaning.
narrative_ontology:disappearance_verdict(script_as_identity__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__ottoman_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__ottoman_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__ottoman_continuity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) and suppression (0.71) are authored moderately-high because the continuity reading's own defense of the arrangement requires active enforcement of Arabic-script primacy in religious and legal education against rising literacy-reform pressure — the suppression is the cost of holding the line, not incidental friction. Theater ratio (0.42) reflects that a genuine coordination function (doctrinal and legal continuity) persists throughout the interval, but an increasing share of the defense becomes performative reassertion of 'authenticity' as reformist pressure mounts (1876-1928), rather than substantive expansion of the script's actual transmission function. Accessibility collapse (0.68) is high because, for populations without dedicated years of Arabic-script training, alternatives to literate intermediaries essentially do not exist under this arrangement. Resistance (0.74) is high and rising across the interval, tracking the historical record of growing reformist agitation that culminated in the 1928 alphabet law.
 *
 * DIRECTIONALITY LOGIC:
 *   Ulema, Ottoman bureaucratic lineage holders, Sufi orders, and madrasa educators sit near the full-beneficiary end: their institutional standing is constituted by the script gate, and their exit is identity-locked because abandoning the script claim would dissolve the basis of their own authority, not merely cost them a convenience. Rural Turkish speakers, excluded women, and non-Arabic-literate provincial administrators sit near the full-target end: they are trapped or constrained, bear the literacy cost directly, and have no meaningful exit within the arrangement — their only route out is the arrangement's abolition, which is exactly what the sibling rupture reading endorses and this reading resists.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading resists being read as pure extraction by pointing to its genuine coordination function: doctrinal fidelity and Ottoman institutional memory are real goods that a script change does put at risk of translation loss and interpretive discontinuity. Classifying this as tangled_rope rather than snare or pure mountain honors that: there IS a coordination problem (maintaining unbroken textual custody across generations) bundled with real, asymmetric extraction (the literacy gate that concentrates interpretive power in a narrow, identity-locked elite while trapping the majority in dependency). Calling it a mountain (natural, inevitable) would be the false move this reading's own defenders make and that the FSM signature exists to catch — script choice is a human, contestable, historically singular decision, not a physical law, despite the reading's rhetoric of 'constitutive' identity. This story deliberately keeps the claimed_type (tangled_rope) independent of any attempt to make the metrics validate the beneficiaries' own self-account.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_script_as_identity,
    'This constraint is one reading (ottoman_continuity_reading) of the contested kernel script_as_identity. The sibling readings — kemalist_rupture_reading (Latin script enables secular modernization by severing the Ottoman-Islamic past) and phonetic_instrumentalism_reading (script is neutral technology; Latin is phonetically superior for Turkish) — are separate constraint files with their own epsilon and stakeholder structures, not alternative measurements of this one.',
    'No empirical resolution collapses the kernel to one reading; the three readings persist as live, differently-held commitments across different historical and contemporary constituencies (religious traditionalists, Kemalist state-builders, linguistic modernizers). Comparative historical outcome data (post-1928 literacy rates, religious institutional continuity measures, diaspora Ottoman-script literacy rates) can inform relative plausibility without resolving the normative kernel.',
    'If treated as one constraint rather than three, the epsilon value would be forced to average across radically different beneficiary/victim structures, violating epsilon-invariance. Keeping the readings separate preserves each reading''s own internally coherent ε and lets the network layer show how the readings structurally relate (this reading''s suppression requirement rises as the rupture reading''s legitimacy rises).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_script_as_identity, conceptual, 'Committer-frame declaration: this file is the ottoman_continuity_reading of the script_as_identity kernel; siblings are separate files.').

omega_variable(
    constitutive_vs_instrumental_script_function,
    'Is Arabic script genuinely constitutive of Turkish-Islamic identity (such that changing it severs identity), or is it a contingent, replaceable technology that happened to carry identity-relevant content which could in principle be carried by any script (including Latin, as the rupture and phonetic-instrumentalism readings hold)?',
    'Historical outcome tracking: did Turkish-Islamic religious and cultural identity in fact persist, transform, or fracture after 1928? Does the transliteration and republication of Ottoman texts into Latin script (which did occur, at scale, over subsequent decades) preserve access to the content the continuity reading claims only the original script could carry?',
    'If identity persisted and transliteration proved adequate, the continuity reading''s core premise (script-as-constitutive) is significantly weakened, and the arrangement''s high suppression cost would appear to have been paid for identity-preservation that could have been achieved with the phonetic-instrumentalist alternative at lower cost. If identity meaningfully fractured or key interpretive content proved untranslatable/unrecoverable, the continuity reading''s claim gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_instrumental_script_function, conceptual, 'Whether script is genuinely constitutive of identity or merely a contingent carrier — the kernel''s central contested claim.').

omega_variable(
    beneficiary_capture_vs_genuine_transmission_need,
    'To what extent does the continuity reading''s defense of Arabic-script primacy reflect a genuine transmission need for religious/legal doctrine versus the self-interested defense of the ulema''s, Sufi orders'', and Ottoman bureaucratic class''s own literacy-gated authority?',
    'Compare communities that maintained doctrinal continuity through translated/transliterated corpora against communities that insisted on original-script transmission; measure whether interpretive fidelity actually degraded in the former.',
    'If translated transmission proved adequate for doctrinal fidelity, the extraction component of this reading''s own arrangement is larger than the coordination component, sharpening the case for tangled_rope (or even snare) over any reading that would treat the arrangement as pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_genuine_transmission_need, empirical, 'Whether the script requirement serves genuine doctrinal transmission or primarily gatekeeper self-interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__ottoman_continuity_reading, 1876, 1928).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t1876, script_as_identity__ottoman_continuity_reading, theater_ratio, 1876, 0.22).
narrative_ontology:measurement(scri_tr_t1889, script_as_identity__ottoman_continuity_reading, theater_ratio, 1889, 0.27).
narrative_ontology:measurement(scri_tr_t1902, script_as_identity__ottoman_continuity_reading, theater_ratio, 1902, 0.31).
narrative_ontology:measurement(scri_tr_t1911, script_as_identity__ottoman_continuity_reading, theater_ratio, 1911, 0.36).
narrative_ontology:measurement(scri_tr_t1920, script_as_identity__ottoman_continuity_reading, theater_ratio, 1920, 0.4).
narrative_ontology:measurement(scri_tr_t1928, script_as_identity__ottoman_continuity_reading, theater_ratio, 1928, 0.42).

% Extraction over time
narrative_ontology:measurement(scri_be_t1876, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1876, 0.42).
narrative_ontology:measurement(scri_be_t1889, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1889, 0.47).
narrative_ontology:measurement(scri_be_t1902, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1902, 0.51).
narrative_ontology:measurement(scri_be_t1911, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1911, 0.55).
narrative_ontology:measurement(scri_be_t1920, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1920, 0.56).
narrative_ontology:measurement(scri_be_t1928, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1928, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t1876, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1876, 0.5).
narrative_ontology:measurement(scri_su_t1889, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1889, 0.56).
narrative_ontology:measurement(scri_su_t1902, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1902, 0.62).
narrative_ontology:measurement(scri_su_t1911, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1911, 0.66).
narrative_ontology:measurement(scri_su_t1920, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1920, 0.69).
narrative_ontology:measurement(scri_su_t1928, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1928, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__ottoman_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__ottoman_continuity_reading, 0.08).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, script_as_identity__kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, script_as_identity__phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language claim 'Arabic script and Turkish-Islamic identity' per the epsilon-invariance principle: script_as_identity__ottoman_continuity_reading (this file, tangled_rope, epsilon=0.58, high suppression cost defending an identity-lock gate), script_as_identity__kemalist_rupture_reading (secular modernization framing, expected lower extraction, different beneficiary set centered on the new Republican state and literate reformers), and script_as_identity__phonetic_instrumentalism_reading (script-as-neutral-technology framing, expected to classify closer to rope, minimal identity stakes, efficiency-centered beneficiary/victim structure). Each reading is evaluated independently by its own lights per the fixed epsilon referent rule; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
