% ============================================================================
% CONSTRAINT STORY: script_as_identity__phonetic_instrumentalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Phonetic Instrumentalism Reading of Turkish Script Reform
 *   domain: comparative_linguistics/political_authority/state_building
 *
 * SUMMARY:
 *   The phonetic instrumentalism reading frames the 1928 Turkish alphabet
 *   reform as a purely technical decision: Latin script with diacritics
 *   represents Turkish vowel harmony and consonant inventory more
 *   transparently than Arabic script. This reading claims the constraint is a
 *   Mountain — a natural law of phonetic fit. But the structural data reveals
 *   beneficiaries (Kemalist modernizers, Western-oriented elites, printing
 *   industry, educational reformers) and victims (Ottoman literati,
 *   Arabic-script readers, religious traditionalists) with active enforcement
 *   (Law No. 1353, mandatory in all public spheres). The theater ratio rises
 *   in recent decades as the phonetic justification is retroactively
 *   emphasized to depoliticize the reform's identity-engineering function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__phonetic_instrumentalism_reading, 0.18).
domain_priors:suppression_score(script_as_identity__phonetic_instrumentalism_reading, 0.22).
domain_priors:theater_ratio(script_as_identity__phonetic_instrumentalism_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__phonetic_instrumentalism_reading, mountain).
narrative_ontology:human_readable(script_as_identity__phonetic_instrumentalism_reading, "Phonetic Instrumentalism Reading of Turkish Script Reform").
narrative_ontology:topic_domain(script_as_identity__phonetic_instrumentalism_reading, "comparative_linguistics/political_authority/state_building").

domain_priors:requires_active_enforcement(script_as_identity__phonetic_instrumentalism_reading).
domain_priors:emerges_naturally(script_as_identity__phonetic_instrumentalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__phonetic_instrumentalism_reading, '0b5735dc-5a82-4faf-8a5f-e77a3a695eed').
narrative_ontology:cs_kernel_codification('0b5735dc-5a82-4faf-8a5f-e77a3a695eed', fixed_text).
narrative_ontology:cs_authority_grounding('0b5735dc-5a82-4faf-8a5f-e77a3a695eed', extraction).
narrative_ontology:cs_interpretation_layer_present('0b5735dc-5a82-4faf-8a5f-e77a3a695eed').
narrative_ontology:cs_reading_relation('0b5735dc-5a82-4faf-8a5f-e77a3a695eed', script_as_identity__kemalist_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b5735dc-5a82-4faf-8a5f-e77a3a695eed', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_axiom('0b5735dc-5a82-4faf-8a5f-e77a3a695eed', foundational, phonetic_optimality_justifies_script_choice).
narrative_ontology:cs_axiom_status(phonetic_optimality_justifies_script_choice, holdable).
narrative_ontology:cs_axiom_grounding('0b5735dc-5a82-4faf-8a5f-e77a3a695eed', phonetic_optimality_justifies_script_choice, empirically_contingent).
narrative_ontology:cs_axiom('0b5735dc-5a82-4faf-8a5f-e77a3a695eed', foundational, script_neutrality_principle).
narrative_ontology:cs_axiom_status(script_neutrality_principle, holdable).
narrative_ontology:cs_axiom_grounding('0b5735dc-5a82-4faf-8a5f-e77a3a695eed', script_neutrality_principle, deontological).
narrative_ontology:cs_reference_frame('0b5735dc-5a82-4faf-8a5f-e77a3a695eed', phonetic_transparency_standard).
narrative_ontology:cs_drift_state('0b5735dc-5a82-4faf-8a5f-e77a3a695eed', contemporary_linguistic_consensus, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('0b5735dc-5a82-4faf-8a5f-e77a3a695eed', '').
narrative_ontology:cs_kernel_id(script_as_identity__phonetic_instrumentalism_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, kemalist_modernizers).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, western_oriented_elites).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, printing_publishing_industry).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, educational_reformers).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, ottoman_literati).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, arabic_script_readers).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, religious_traditionalists).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, phonetic_transparency_optimizes_literacy).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, script_choice_is_technical_not_political).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drove the 1928 alphabet law as founding state elites. Used phonetic transparency as the public justification for a reform whose deeper purpose was severing Ottoman-Islamic cultural continuity and aligning Turkey with Europe. Controlled the enforcement machinery (education, publishing, bureaucracy).
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, kemalist_modernizers, agenda_setter,
    institutional, generational, arbitrage, national).

% Gained cultural capital and European integration advantages from Latin script. Their existing French-language education and European networks became more valuable. Could navigate both scripts during transition; exit to European cultural sphere was always open.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, western_oriented_elites, beneficiary,
    powerful, biographical, mobile, national).

% Latin script enabled cheaper, faster typesetting using European equipment and standards. The Ottoman Arabic script required complex ligatures and calligraphic expertise. Transition costs were high but long-term production costs dropped dramatically.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, printing_publishing_industry, beneficiary,
    organized, biographical, constrained, national).

% Argued Latin script reduced literacy acquisition time from years to months. Gained professional authority and state resources for mass education campaigns. Their pedagogical framework became the national standard.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, educational_reformers, beneficiary,
    organized, generational, constrained, national).

% Lost professional standing as scribes, calligraphers, and scholars of Ottoman Turkish. Their specialized knowledge (arabic script, Persian/Arabic vocabulary) became obsolete. No exit to comparable roles; many died in poverty or marginalization.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, ottoman_literati, payer,
    moderate, biographical, trapped, national).

% The mass of previously literate Ottomans (merchant classes, religious students, provincial administrators) found their literacy invalidated overnight. Relearning was possible but costly; many became functionally illiterate in the new system. No organized representation.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, arabic_script_readers, payer,
    powerless, biographical, constrained, national).

% Experienced script change as severance from Quranic tradition and Islamic scholarly heritage. Arabic script was constitutive of religious identity; Latin script felt like cultural amputation. Exit means abandoning religious self-understanding — structurally impossible for committed believers.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, religious_traditionalists, payer,
    moderate, generational, identity_locked, national).

% Contemporary and historical linguists evaluating the phonetic fit between Latin letters and Turkish vowel harmony. Most agree Latin script with diacritics represents Turkish phonology well, but note the reform's political drivers and the loss of etymological transparency for Arabic/Persian loanwords.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, linguistic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized Turkish orthography for mass literacy, modern printing, telegraphy, and later digital encoding — solving the genuine coordination problem of a fragmented script landscape (Arabic script with Ottoman conventions, Armenian script for Turkish, Greek script for Turkish, ad-hoc Latin transcriptions).
% TRANSFER_FUNCTION: Moves cultural capital, professional standing, and identity continuity from Ottoman-Islamic tradition (scribes, religious scholars, Arabic-script literate populations) to Western-oriented secular nation-builders (state elites, modern educators, European-aligned professionals, printing industry). The phonetic justification obscures this transfer.
% ABSENT_VOICES: Ottoman scribes and calligraphers (profession eliminated), Arabic-script madrasa students (educational path closed), provincial administrators in Arabic-script bureaucracy (replaced), Kurdish and other minority communities whose Arabic-script literacy was also erased — none were consulted on the 1928 law.
% DISAPPEARANCE_RATIONALE: If the 1928 alphabet law and its enforcement vanished overnight, Turkish publishing, education, legal codes, and digital infrastructure would need to reorganize around a new script standard. The 96-year investment in Latin-script human capital, typefaces, keyboards, and NLP tools creates massive path dependence.
% FOUNDING_PROBLEM: How to achieve mass literacy in Turkish rapidly, integrate with European scientific and print culture, and replace the Ottoman Arabic script which was poorly suited to Turkish vowel harmony and required years of training.
% FOUNDING_PROBLEM_CORROBORATION: Kemalist state archives and educational statistics corroborate the literacy jump (from ~10% to ~70% in two generations). Ottomanists and cultural historians corroborate the identity rupture and loss of access to Ottoman textual heritage. Linguists corroborate the phonetic fit but note the reform's political overdetermination. No single perspective commands consensus.
narrative_ontology:disappearance_verdict(script_as_identity__phonetic_instrumentalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__phonetic_instrumentalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__phonetic_instrumentalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(script_as_identity__phonetic_instrumentalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__phonetic_instrumentalism_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__phonetic_instrumentalism_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, ExtMetricName, E),
    domain_priors:suppression_score(script_as_identity__phonetic_instrumentalism_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(script_as_identity__phonetic_instrumentalism_reading),
    narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(script_as_identity__phonetic_instrumentalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.18) from this reading's perspective because it assesses the standing arrangement as technical optimization. But the measurement series shows high initial extraction (0.45 at T=0) during enforcement, declining as the constraint naturalized. Theater ratio starts high (0.65) — the phonetic cover story was most performative during active suppression — then declines as the script becomes mundane, but rises again (0.41 at T=100) as political contestation over identity re-escalates. Suppression requirement follows the enforcement trajectory: intense at T=0 (0.85), near-zero by mid-century, rising slightly today as script choice becomes a culture-war symbol.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat, the constraint is a Mountain (phonetic necessity). From payer seats, it is a Snare (enforced identity rupture). From beneficiary seats, it is a Rope (coordination gain). The engine computes this divergence. The phonetic instrumentalism reading only sees the Mountain seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Kemalist modernizers are agenda_setters with arbitrage exit (they built the system and could exit to European power structures). Western-oriented elites and printing industry are beneficiaries with mobile/constrained exit. Ottoman literati are payers, trapped — their human capital was script-specific and non-transferable. Arabic-script readers are payers, constrained — relearning was possible but costly. Religious traditionalists are payers, identity_locked — Arabic script was constitutive of religious self. Linguistic observers are analytical. The engine derives directionality from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mass literacy + European integration) is contested as live/dead. Literacy is achieved; European integration is ambiguous. The arrangement persists without its founding justification — a classic mandatrophy candidate. But the phonetic instrumentalism reading denies mandatrophy by claiming the technical justification remains valid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the phonetic instrumentalism reading a genuine technical assessment of script fit, or a political cover story that depoliticizes the 1928 reform''s identity-engineering function?',
    'Counterfactual analysis: if Arabic script had been reformed with diacritics for vowel harmony (as proposed by some Ottoman reformers), would Kemalists have accepted it? Historical evidence suggests no — the Latin choice was overdetermined by civilizational alignment.',
    'If cover story, the claimed Mountain classification is a false summit; the constraint is structurally a Tangled Rope (coordination + extraction) or Snare (extraction with coordination cover). The omega documents the irreducible ambiguity between technical and political framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the phonetic justification is the true driver or a legitimating narrative.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the script change''s suppression structural (state enforcement, legal penalties) or internalized (populations adopting the new script as ''modern'' and self-stigmatizing the old)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed, reclassify as partially internalized. Contemporary Turkish culture-war over script suggests internalized identity suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This affects the piton/tangled_rope boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the script reform.').

omega_variable(
    phonetic_optimality_claim,
    'Does Latin script with Turkish diacritics actually provide superior phonetic transparency for Turkish vowel harmony compared to a reformed Arabic script, or is this claim contested by linguistic evidence?',
    'Comparative orthographic analysis: Ottoman reform proposals (e.g., Huruf-ı Mukkeseb) added vowel diacritics to Arabic script. Modern linguists can model learnability and processing efficiency of both systems.',
    'If a reformed Arabic script achieves comparable transparency, the phonetic instrumentalism reading''s foundational axiom (phonetic optimality justifies script choice) loses empirical grounding, strengthening the case for political overdetermination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(phonetic_optimality_claim, empirical, 'Empirical status of the phonetic superiority claim for Latin over reformed Arabic script.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__phonetic_instrumentalism_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_tr_t0, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_tr_t10, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_tr_t25, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_tr_t50, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_tr_t75, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 75, 0.35).
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_tr_t100, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 100, 0.41).

% Extraction over time
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_be_t0, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_be_t10, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_be_t25, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_be_t50, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_be_t75, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 75, 0.12).
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_be_t100, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 100, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_su_t0, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_su_t10, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_su_t25, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 25, 0.3).
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_su_t50, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_su_t75, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 75, 0.1).
narrative_ontology:measurement(script_as_identity__phonetic_instrumentalism_reading_su_t100, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 100, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__phonetic_instrumentalism_reading, information_standard).
narrative_ontology:boltzmann_floor_override(script_as_identity__phonetic_instrumentalism_reading, 0.03).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__ottoman_continuity_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings form the script_as_identity constraint family. The phonetic_instrumentalism_reading claims Mountain (natural law); kemalist_rupture_reading claims Tangled Rope (coordination + extraction); ottoman_continuity_reading claims Snare (extraction). They share the referent (1928 alphabet law) but differ in ε and beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__phonetic_instrumentalism_reading, institutional, 0.15).
constraint_indexing:directionality_override(script_as_identity__phonetic_instrumentalism_reading, powerless, 0.9).
constraint_indexing:directionality_override(script_as_identity__phonetic_instrumentalism_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
