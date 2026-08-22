% ============================================================================
% CONSTRAINT STORY: script_as_identity__phonetic_instrumentalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: The 1928 Turkish Script Reform as Neutral Phonetic Optimization
 *   domain: linguistics/political_authority/state_building
 *
 * SUMMARY:
 *   This story instantiates the phonetic-instrumentalism reading of the
 *   script-as-identity kernel: the claim that the 1928 Turkish script reform
 *   was a neutral technical optimization — Latin script's superior phonetic
 *   transparency for Turkish vowel harmony — rather than an act of political
 *   rupture (kemalist_rupture_reading) or an assault on constitutive
 *   Ottoman-Islamic identity (ottoman_continuity_reading). This reading is
 *   generated as its own clean, ε-invariant constraint per Rule 1: it does
 *   not describe or average over the sibling readings, it only authors the
 *   technical-optimization claim's own structure. The metrics reflect what a
 *   phonetic-instrumentalism advocate would honestly measure: low direct
 *   extraction (it IS true that vowel harmony representation improved), but a
 *   rising theater_ratio, because the technical framing does increasing
 *   interpretive work over time to explain away the reform's evident
 *   identity-encoding effects (severed religious-education access, archive
 *   discontinuity) that the neutral-technology story cannot account for on
 *   its own terms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__phonetic_instrumentalism_reading, 0.22).
domain_priors:suppression_score(script_as_identity__phonetic_instrumentalism_reading, 0.58).
domain_priors:theater_ratio(script_as_identity__phonetic_instrumentalism_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__phonetic_instrumentalism_reading, rope).
narrative_ontology:human_readable(script_as_identity__phonetic_instrumentalism_reading, "The 1928 Turkish Script Reform as Neutral Phonetic Optimization").
narrative_ontology:topic_domain(script_as_identity__phonetic_instrumentalism_reading, "linguistics/political_authority/state_building").

domain_priors:requires_active_enforcement(script_as_identity__phonetic_instrumentalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__phonetic_instrumentalism_reading, '638fe641-c127-4360-8f69-6e0a2e11bc49').
narrative_ontology:cs_kernel_codification('638fe641-c127-4360-8f69-6e0a2e11bc49', distributed).
narrative_ontology:cs_authority_grounding('638fe641-c127-4360-8f69-6e0a2e11bc49', expertise).
narrative_ontology:cs_interpretation_layer_present('638fe641-c127-4360-8f69-6e0a2e11bc49').
narrative_ontology:cs_reading_relation('638fe641-c127-4360-8f69-6e0a2e11bc49', script_as_identity__kemalist_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('638fe641-c127-4360-8f69-6e0a2e11bc49', script_as_identity__ottoman_continuity_reading, influences).
narrative_ontology:cs_axiom('638fe641-c127-4360-8f69-6e0a2e11bc49', foundational, script_choice_is_technically_determined_by_phonology).
narrative_ontology:cs_axiom_status(script_choice_is_technically_determined_by_phonology, holdable).
narrative_ontology:cs_axiom_grounding('638fe641-c127-4360-8f69-6e0a2e11bc49', script_choice_is_technically_determined_by_phonology, empirically_contingent).
narrative_ontology:cs_axiom('638fe641-c127-4360-8f69-6e0a2e11bc49', secondary, identity_and_political_effects_are_incidental_not_constitutive).
narrative_ontology:cs_axiom_status(identity_and_political_effects_are_incidental_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('638fe641-c127-4360-8f69-6e0a2e11bc49', identity_and_political_effects_are_incidental_not_constitutive, conventional).
narrative_ontology:cs_reference_frame('638fe641-c127-4360-8f69-6e0a2e11bc49', phonetic_transparency_optimization_standard).
narrative_ontology:cs_drift_state('638fe641-c127-4360-8f69-6e0a2e11bc49', post_reform_consolidation_1950s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('638fe641-c127-4360-8f69-6e0a2e11bc49', '').
narrative_ontology:cs_kernel_id(script_as_identity__phonetic_instrumentalism_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, linguistic_modernization_technocrats).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, mass_literacy_campaign_administrators).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, print_and_publishing_industry).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, arabic_script_literate_older_generation).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, religious_education_establishment).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, ottoman_archive_dependent_scholars).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, script_technical_neutrality_doctrine).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, phonetic_fit_determines_orthographic_choice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the new alphabet, run the Language Commission, and justify the reform in purely technical terms: Arabic script's consonantal bias and lack of dedicated vowel letters poorly represent Turkish's eight-vowel harmony system, while the Latin adaptation gives each vowel phoneme a distinct grapheme. They administer the transition and frame every objection as a lapse into anti-scientific sentiment rather than a live political dispute.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, linguistic_modernization_technocrats, agenda_setter,
    institutional, generational, analytical, national).

% Run the Millet Mektepleri (Nation's Schools) that teach the new script to adults. A more transparent grapheme-to-phoneme mapping genuinely shortens time-to-literacy, which the administrators can report as a measurable success independent of any argument about Ottoman continuity or secular rupture. Their institutional position and funding grow with each literacy-rate improvement attributed to the switch.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, mass_literacy_campaign_administrators, beneficiary,
    institutional, generational, arbitrage, national).

% Retool presses and typefaces for Latin characters, capturing a captive market for new textbooks, newspapers, and reference works as the entire literate public must re-equip. They have every incentive to endorse the technical-superiority framing since it forecloses debate about reversing course.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, print_and_publishing_industry, beneficiary,
    organized, biographical, mobile, national).

% Spent decades acquiring literacy in Arabic script and now find themselves functionally illiterate in the new national alphabet overnight, with limited time or institutional support to relearn. The technical-neutrality framing offers them no standing to object — their loss is recoded as an acceptable transition cost of an optimization, not a harm requiring redress.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, arabic_script_literate_older_generation, payer,
    powerless, biographical, trapped, national).

% Taught Quranic recitation and religious jurisprudence through Arabic-script literacy for centuries. The phonetic-transparency argument never engages their objection at all — it treats the question as settled by vowel-harmony statistics, leaving no technical rebuttal available even though their actual complaint is about severed access to a script bound to the sacred text and legal tradition.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, religious_education_establishment, payer,
    moderate, generational, constrained, national).

% Depend on Arabic-script literacy to read centuries of Ottoman administrative, literary, and legal documents. As Latin-script literacy becomes the norm, direct access to this archive narrows to a shrinking specialist class, and general readers are cut off from primary sources regardless of any phonetic argument's technical merits.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, ottoman_archive_dependent_scholars, payer,
    moderate, civilizational, constrained, national).

% Study the reform as a case of script choice; note that the phonetic-transparency argument, while not false, is radically underdetermined as an explanation — many scripts could be adapted with diacritics to represent Turkish vowels (as some proposals for a modified Arabic script demonstrated), so technical fit alone does not necessitate Latinization. They observe the argument doing political work while presenting as apolitical.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, comparative_script_linguists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A script with transparent, near one-to-one grapheme-to-phoneme correspondence for Turkish's vowel inventory does genuinely lower the cost of teaching mass literacy compared to a script requiring readers to infer vowels from context, consonant clusters, or diacritics inconsistently applied. This is a real, measurable coordination gain for a state trying to rapidly raise literacy rates.
% TRANSFER_FUNCTION: Moves literacy capital from the Arabic-script-literate population (older generation, religious scholars, archive specialists) to a new Latin-script-literate population, while transferring cultural and economic advantage to print industries, textbook publishers, and the technocratic administrators who designed and enforce the transition — under a description (technical optimization) that does not acknowledge the transfer as a transfer at all.
% ABSENT_VOICES: Phonologists proposing modified-Arabic-script alternatives (with added vowel diacritics) that could have captured much of the phonetic-transparency gain without full discontinuity are absent from the technical framing entirely — their counter-proposals are not engaged, only the binary of old-script-versus-Latin is presented as though it were the only technical menu.
% DISAPPEARANCE_RATIONALE: If the phonetic-instrumentalism framing disappeared and the reform were argued for solely as identity rupture or continuity (the sibling readings), the same script change might still have occurred by political fiat — but its legitimacy claim would look entirely different: an avowed act of cultural severance rather than a value-neutral technical upgrade. The technocratic administrative apparatus built around 'scientific' script design would lose its cover story, though the underlying literacy infrastructure would likely persist under a different justification.
% FOUNDING_PROBLEM: Turkish written in the Ottoman Arabic-script adaptation had a documented mismatch with Turkish phonology: the script under-represented vowels central to vowel harmony, contributing to slow, inconsistent literacy acquisition and inhibiting mass education goals.
% FOUNDING_PROBLEM_CORROBORATION: Independent comparative linguists (outside both the Kemalist state apparatus and religious establishment) corroborate that the original phonetic mismatch was real and that literacy rates measurably improved post-reform; however, the same scholars attest that the phonetic problem could have been substantially addressed by modifying the existing Arabo-Persian script with additional vowel diacritics, meaning the technical problem did not require the specific solution (full Latin adoption) that was chosen. The technical problem's 'deadness' is corroborated; the necessity of the particular fix administered is not.
narrative_ontology:disappearance_verdict(script_as_identity__phonetic_instrumentalism_reading, contested).
narrative_ontology:founding_problem_status(script_as_identity__phonetic_instrumentalism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__phonetic_instrumentalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__phonetic_instrumentalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__phonetic_instrumentalism_reading, 0.22, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.22, rising only slightly) because the phonetic-transparency claim about Turkish vowel harmony is empirically defensible in isolation — Arabic script genuinely under-specifies vowels relative to Latin adaptation for this phonology, and literacy gains were real. But suppression starts high (0.75) and eases only somewhat (0.58) reflecting that adoption was mandated and enforced (script bans, generational literacy cliffs) regardless of the argument's technical merits, and the 'neutral technology' framing never had to answer for the enforcement mechanism it rode on. theater_ratio rises steadily (0.4 to 0.62) because the instrumentalist framing was increasingly deployed retrospectively to depoliticize a decision whose actual effects (severing archive access, delegitimizing religious literacy) were identity-political regardless of the phonetic argument's validity.
 *
 * DIRECTIONALITY LOGIC:
 *   Modernization technocrats and the beneficiary institutions occupy the low-d end: they administer and profit from the technical-neutrality framing without bearing its costs. Older Arabic-literate generations, religious educators, and archive-dependent scholars sit at the high-d end: trapped or constrained, they bear the literacy discontinuity and lose interpretive access to their own textual tradition, and the instrumentalist framing gives them no vocabulary to contest this as a loss rather than an optimization.
 *
 * MANDATROPHY ANALYSIS:
 *   The phonetic-transparency claim's founding problem (poor vowel representation in the inherited script) is genuinely dead — the technical mismatch was real and is resolved. But founding_problem_status=dead paired with disappearance_verdict=contested signals a mismatch worth flagging: the instrumentalist justification persists in cultural memory as *the* explanation for the reform even though comparative linguists corroborate that alternative technical fixes (modified Arabic diacritics) existed and were not adopted, meaning the specific choice of full Latin adoption cannot be explained by the technical problem alone. This is exactly the kind of divergence the framework exists to surface: a true premise (Latin is phonetically superior) doing political work (obscuring identity rupture) that the premise alone does not license.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_necessity_vs_political_choice,
    'Was full Latin adoption technically necessitated by Turkish vowel harmony, or was it one of several technically viable options (including modified Arabic script with added vowel diacritics) chosen for reasons the phonetic-instrumentalism reading does not account for?',
    'Comparative analysis of the modified-Arabic-script proposals actually circulated in the 1920s language debates, assessing whether they would have achieved comparable phonetic transparency; historical record of which proposals were considered and rejected and on what stated grounds.',
    'If modified-Arabic alternatives were technically adequate and rejected on non-technical grounds, the phonetic-instrumentalism reading''s central premise (that Latin was the superior/necessary technical solution) is substantially weakened, and the reform is better explained by the sibling readings'' rupture/continuity logic than by this reading''s neutral-technology claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_necessity_vs_political_choice, empirical, 'Whether Latin script was technically necessary or merely one option among technically adequate alternatives.').

omega_variable(
    neutrality_of_script_technology,
    'Can any script choice be evaluated as ''neutral technology'' independent of the identity, religious, and archival relationships that script systems accumulate over centuries of use?',
    'This is not resolvable by further data — it depends on whether one takes script systems to be pure information-encoding tools (supporting this reading) or irreducibly bound up with the textual traditions and communities that used them (supporting the sibling readings). Different theoretical commitments in linguistics and semiotics answer this differently.',
    'If script is never fully separable from the identity functions it has accumulated, then this reading''s low ε is an artifact of bracketing exactly the dimension (identity-encoding) that the sibling readings measure as extractive — the low ε would not be wrong on its own terms but would be systematically incomplete as an account of the reform''s total effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_of_script_technology, conceptual, 'Whether script technology can be meaningfully separated from the identity functions it carries — the conceptual fault line between this reading and its siblings.').

omega_variable(
    reform_beneficiary_capture_of_technical_frame,
    'To what extent did the technocrats and publishing interests who benefited from the reform actively construct or amplify the phonetic-transparency argument specifically because it depoliticized their material interest in the transition?',
    'Archival research into Language Commission internal deliberations and correspondence, contrasted with the Commission''s public technical justifications, to assess whether internal discussion foregrounded identity/political goals while public messaging foregrounded technical ones.',
    'Evidence of intentional depoliticization-via-technical-framing would support classifying the coupling between beneficiary interest and the instrumentalist narrative as more than incidental, strengthening the case that this reading functions partly as legitimating cover rather than purely as an accurate technical account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_beneficiary_capture_of_technical_frame, empirical, 'Whether the technical framing was strategically deployed by beneficiaries to obscure the reform''s political and identity-encoding functions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__phonetic_instrumentalism_reading, 1928, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t1928, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1928, 0.4).
narrative_ontology:measurement(scri_tr_t1932, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1932, 0.5).
narrative_ontology:measurement(scri_tr_t1938, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1938, 0.58).
narrative_ontology:measurement(scri_tr_t1945, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1945, 0.6).
narrative_ontology:measurement(scri_tr_t1952, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1952, 0.62).
narrative_ontology:measurement(scri_tr_t1960, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 1960, 0.62).

% Extraction over time
narrative_ontology:measurement(scri_be_t1928, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1928, 0.18).
narrative_ontology:measurement(scri_be_t1932, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1932, 0.2).
narrative_ontology:measurement(scri_be_t1938, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1938, 0.22).
narrative_ontology:measurement(scri_be_t1945, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1945, 0.23).
narrative_ontology:measurement(scri_be_t1952, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1952, 0.22).
narrative_ontology:measurement(scri_be_t1960, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 1960, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t1928, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1928, 0.75).
narrative_ontology:measurement(scri_su_t1932, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1932, 0.68).
narrative_ontology:measurement(scri_su_t1938, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1938, 0.62).
narrative_ontology:measurement(scri_su_t1945, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1945, 0.58).
narrative_ontology:measurement(scri_su_t1952, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1952, 0.58).
narrative_ontology:measurement(scri_su_t1960, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 1960, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__phonetic_instrumentalism_reading, information_standard).
narrative_ontology:boltzmann_floor_override(script_as_identity__phonetic_instrumentalism_reading, 0.05).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, ottoman_continuity_reading).

% DUAL FORMULATION NOTE:
% Three sibling readings of the script_as_identity kernel: phonetic_instrumentalism_reading (this story, low ε ~0.22, technical-optimization framing), kemalist_rupture_reading (identity-rupture framing, expected higher ε reflecting acknowledged political extraction from religious/traditional constituencies), and ottoman_continuity_reading (identity-loss framing centered on the victims of severed continuity, expected highest ε from the continuity-defender's perspective). Each story shares the same underlying historical event (the 1928 script reform) but authors a structurally distinct ε because each reading brackets a different aspect of the reform's function. Per the ε-invariance principle, these are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
