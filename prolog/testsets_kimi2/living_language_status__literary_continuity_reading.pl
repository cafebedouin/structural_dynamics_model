% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__literary_continuity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Literary Continuity Definition of Language Vitality
 *   domain: sociolinguistic/religious/nationalism
 *
 * SUMMARY:
 *   This constraint instantiates the literary_continuity_reading of the
 *   living_language_status kernel. Developed during the Haskalah (Jewish
 *   Enlightenment) and consolidated through modern Hebrew literature, it
 *   defines a language as living if it sustains productive literary and
 *   intellectual work, explicitly regardless of native speaker status. The
 *   definition coordinated secular Jewish intellectuals around a revival
 *   project that did not require mass vernacular adoption, granting cultural
 *   authority to literary elites while structurally excluding illiterate and
 *   non-literary speakers from the vitality accounting.
 *
 * KEY AGENTS:
 *   - maskilim_intellectuals: Primary agenda-setter (organized/continental/mobile) â defines and enforces the literary vitality standard through periodicals and canon formation
 *   - secular_hebrew_literati: Primary beneficiary (powerful/national/mobile) â collects cultural authority and legitimacy for their literary output
 *   - illiterate_speakers: Primary target (powerless/local/constrained) â bears the cost of definitional exclusion; their vernacular practices are rendered invisible
 *   - non_literary_communities: Secondary target (moderate/regional/constrained) â domestic and commercial speech devalued relative to high literary production
 *   - traditional_religious_scholars: Excluded voice (organized/continental/constrained) â holds the liturgical preservation reading but is marginalized by the literary framework
 *   - sociolinguistic_observers: Analytical observer (analytical/global/analytical) â evaluates the competing definitions from outside the cultural contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.28).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.42).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Literary Continuity Definition of Language Vitality").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistic/religious/nationalism").

domain_priors:requires_active_enforcement(living_language_status__literary_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, '25e522a8-9698-41d3-9bde-9af86d16e076').
narrative_ontology:cs_kernel_codification('25e522a8-9698-41d3-9bde-9af86d16e076', formalized).
narrative_ontology:cs_authority_grounding('25e522a8-9698-41d3-9bde-9af86d16e076', expertise).
narrative_ontology:cs_interpretation_layer_present('25e522a8-9698-41d3-9bde-9af86d16e076').
narrative_ontology:cs_reading_relation('25e522a8-9698-41d3-9bde-9af86d16e076', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('25e522a8-9698-41d3-9bde-9af86d16e076', living_language_status__native_generation_reading, influences).
narrative_ontology:cs_axiom('25e522a8-9698-41d3-9bde-9af86d16e076', foundational, literary_productivity_sufficient_for_vitality).
narrative_ontology:cs_axiom_status(literary_productivity_sufficient_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('25e522a8-9698-41d3-9bde-9af86d16e076', literary_productivity_sufficient_for_vitality, conventional).
narrative_ontology:cs_axiom('25e522a8-9698-41d3-9bde-9af86d16e076', foundational, native_speaker_status_irrelevant).
narrative_ontology:cs_axiom_status(native_speaker_status_irrelevant, holdable).
narrative_ontology:cs_axiom_grounding('25e522a8-9698-41d3-9bde-9af86d16e076', native_speaker_status_irrelevant, conventional).
narrative_ontology:cs_reference_frame('25e522a8-9698-41d3-9bde-9af86d16e076', haskalah_literary_vitality).
narrative_ontology:cs_drift_state('25e522a8-9698-41d3-9bde-9af86d16e076', contemporary_sociolinguistics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('25e522a8-9698-41d3-9bde-9af86d16e076', '').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim_intellectuals).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, secular_hebrew_literati).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, illiterate_speakers).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, non_literary_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Established periodicals, literary journals, and canon criteria defining Hebrew vitality through new written work. Their authority depends on maintaining the standard that literary productivity alone suffices for linguistic life.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, maskilim_intellectuals, agenda_setter,
    organized, generational, mobile, continental).

% Produce modern novels, poetry, and essays in Hebrew. Their work is treated as living proof of the language's vitality, granting them cultural prestige and institutional recognition.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, secular_hebrew_literati, beneficiary,
    powerful, biographical, mobile, national).

% Speak Hebrew or related vernaculars in daily life but cannot participate in the literary culture. Under this definition, their speech does not count toward the language's vitality, rendering them invisible in cultural accounting.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, illiterate_speakers, payer,
    powerless, immediate, constrained, local).

% Engage in commerce, domestic life, and oral tradition using Hebrew or Jewish vernaculars. Their language practices are devalued because the vitality standard only recognizes high literary and intellectual production.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, non_literary_communities, payer,
    moderate, biographical, constrained, regional).

% Maintain that Hebrew lives through sacred study, liturgy, and ritual transmission. Their alternative definition of vitality is marginalized by the secular literary framework.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, traditional_religious_scholars, excluded,
    organized, generational, constrained, continental).

% Study competing definitions of language vitality from outside the cultural contest, tracking how different standards allocate authority and recognition.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, sociolinguistic_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__literary_continuity_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates secular Jewish intellectuals around a cultural revival project by establishing that Hebrew remains a living language suitable for modern literature and intellectual work, even in the absence of a mass native-speaking population.
% TRANSFER_FUNCTION: Moves cultural authority and national legitimacy from traditional religious gatekeepers and non-literary populations to secular literary intellectuals and their written productions.
% ABSENT_VOICES: Traditional religious scholars who define vitality through liturgical continuity, and illiterate native speakers whose oral vernacular is invisible under the literary standard. Both are structurally excluded from the discourse that establishes what counts as a living language.
% DISAPPEARANCE_RATIONALE: If this definition vanished, the Haskalah cultural project would lose its primary legitimating framework; modern Hebrew literature's claim to prove national vitality would weaken; non-literary speakers might reclaim standing in language vitality debates; and the intellectual class would need alternative grounds for cultural authority.
% FOUNDING_PROBLEM: Hebrew had become a language of sacred study and liturgy but not daily life or modern expression; the Jewish Enlightenment needed to demonstrate that Hebrew could sustain a modern national culture without waiting for a mass vernacular revival.
% FOUNDING_PROBLEM_CORROBORATION: Zionist literary historians and Haskalah scholars attest the problem was solved by modern literature. Traditional scholars and linguistic anthropologists attest the problem was reframed by redefining vitality to exclude the actual speech community; corroboration from outside the beneficiary set comes from anthropologists studying language shift and revitalization, who note that vitality definitions determine resource allocation for endangered languages.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__literary_continuity_reading, 0.28, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__literary_continuity_reading_tests).
:- end_tests(living_language_status__literary_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the constraint operates primarily through symbolic prestige and definitional exclusion rather than material transfer. Suppression is moderate (0.42): the definition suppresses alternative vitality criteria (oral, liturgical) through institutional gatekeeping in education, publishing, and canon formation, not through physical coercion. Theater ratio (0.30) reflects the growing performative maintenance of the literary canon as proof of national vitality even after native speaker communities rendered the original justification obsolete. Accessibility collapse (0.55): once the literary framework is accepted, non-literary language practices become nearly illegible as evidence of vitality. Resistance (0.40) comes from traditional religious communities and later from sociolinguists who privilege native transmission.
 *
 * PERSPECTIVAL GAP:
 *   From the maskilim and literati seats, the constraint is genuine coordination: it solved the problem of reviving a language without native speakers by creating a shared literary project. From the illiterate and non-literary seats, the same structure is asymmetric exclusion: their speech is deemed irrelevant to whether the language lives. The engine computes this divergence from the structural data â the agenda-setter and beneficiaries have mobile exit and institutional power, while the payers are constrained and powerless.
 *
 * DIRECTIONALITY LOGIC:
 *   Maskilim intellectuals and secular literati are declared beneficiaries with mobile or constrained-but-powerful exit; their structural relationship to the constraint is subsidizing (low d). Illiterate speakers and non-literary communities are declared victims with constrained exit and low power; their structural relationship is extractive (high d). Traditional religious scholars are excluded rather than targeted, so their directionality reverts to the organized-power canonical fallback. The effective extraction (chi) is thus amplified for the illiterate and damped for the intellectual class.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling this constraint as either pure rope (which would ignore the exclusion of non-literary speakers) or pure snare (which would miss the genuine coordination function it served for nineteenth-century Jewish cultural revival). The founding problem â reviving Hebrew without native speakers â was real, but the solution asymmetrically concentrated authority in literary elites. The low epsilon honestly records that the extraction is symbolic and definitional rather than material, while the victim declarations capture the structural cost borne by excluded speakers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_sibling_structural_delta,
    'How would the beneficiary and victim structure change if the native_generation_reading or liturgical_preservation_reading were adopted instead of this literary continuity reading?',
    'Comparative analysis of the three constraint stories in the living_language_status kernel family.',
    'Would shift beneficiaries to families or ritual specialists, and recast literary elites as either irrelevant or secondary, fundamentally altering the directionality map.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_sibling_structural_delta, conceptual, 'Structural difference between kernel readings').

omega_variable(
    exclusion_as_extraction,
    'Does defining language vitality exclusively through literary productivity extract from non-literary speakers, or merely exclude them from a category without cost?',
    'Empirical study of resource allocation, educational funding, and status attribution under the literary definition versus alternative vitality definitions.',
    'If the exclusion carries material or status costs, current extractiveness may be too low and the victim count accurate; if purely definitional, victims list may overstate structural harm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_as_extraction, conceptual, 'Whether definitional exclusion constitutes extraction').

omega_variable(
    enforcement_mechanism_nature,
    'Does the dominance of the literary vitality definition require active institutional enforcement, or does it emerge naturally from the prestige of written culture?',
    'Historical analysis of canon formation, publishing gatekeeping, and educational curriculum design versus bottom-up prestige accumulation.',
    'If natural emergence, reclassification toward rope or mountain; if active enforcement through institutions, confirms tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_nature, empirical, 'Whether enforcement is institutional or emergent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__literary_continuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(livi_tr_t30, living_language_status__literary_continuity_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(livi_tr_t60, living_language_status__literary_continuity_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(livi_tr_t90, living_language_status__literary_continuity_reading, theater_ratio, 90, 0.26).
narrative_ontology:measurement(livi_tr_t120, living_language_status__literary_continuity_reading, theater_ratio, 120, 0.29).
narrative_ontology:measurement(livi_tr_t150, living_language_status__literary_continuity_reading, theater_ratio, 150, 0.3).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__literary_continuity_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(livi_be_t30, living_language_status__literary_continuity_reading, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(livi_be_t60, living_language_status__literary_continuity_reading, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(livi_be_t90, living_language_status__literary_continuity_reading, base_extractiveness, 90, 0.27).
narrative_ontology:measurement(livi_be_t120, living_language_status__literary_continuity_reading, base_extractiveness, 120, 0.28).
narrative_ontology:measurement(livi_be_t150, living_language_status__literary_continuity_reading, base_extractiveness, 150, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__literary_continuity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(livi_su_t30, living_language_status__literary_continuity_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(livi_su_t60, living_language_status__literary_continuity_reading, suppression_requirement, 60, 0.35).
narrative_ontology:measurement(livi_su_t90, living_language_status__literary_continuity_reading, suppression_requirement, 90, 0.4).
narrative_ontology:measurement(livi_su_t120, living_language_status__literary_continuity_reading, suppression_requirement, 120, 0.42).
narrative_ontology:measurement(livi_su_t150, living_language_status__literary_continuity_reading, suppression_requirement, 150, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__literary_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__native_generation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the living_language_status kernel, decomposed per the epsilon-invariance principle. The literary continuity reading coordinates around written intellectual production; sibling readings address liturgical and native-speaker criteria. Each reading carries a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
