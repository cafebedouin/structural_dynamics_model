% ============================================================================
% CONSTRAINT STORY: script_as_identity__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-20
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
 *   human_readable: Arabic Script as Constitutive of Turkish-Islamic Identity and Ottoman Continuity
 *   domain: cultural/political/linguistic
 *
 * SUMMARY:
 *   This constraint story models the Ottoman continuity reading of the
 *   script-as-identity kernel: the claim that Arabic script is not merely a
 *   writing system but the constitutive medium of Turkish-Islamic identity
 *   and the sole legitimate vehicle for accessing Ottoman-Islamic
 *   civilizational memory. The constraint operates from the late 13th century
 *   (emergence of Ottoman chancery practice) to 1928 (Latin script adoption
 *   by the Turkish Republic). It coordinates a trans-regional
 *   scholarly-religious establishment while extracting cognitive labor from
 *   vernacular populations and suppressing alternative literate developments.
 *   The reading frames the script's persistence as sacred continuity; the
 *   metrics describe an arrangement that became increasingly extractive and
 *   suppressive as the phonetic mismatch and modernization pressure grew.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, 0.68).
domain_priors:suppression_score(script_as_identity__ottoman_continuity_reading, 0.82).
domain_priors:theater_ratio(script_as_identity__ottoman_continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__ottoman_continuity_reading, "Arabic Script as Constitutive of Turkish-Islamic Identity and Ottoman Continuity").
narrative_ontology:topic_domain(script_as_identity__ottoman_continuity_reading, "cultural/political/linguistic").

domain_priors:requires_active_enforcement(script_as_identity__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__ottoman_continuity_reading, 'bbc940e8-965c-44cf-9274-2d52e2bb3950').
narrative_ontology:cs_kernel_codification('bbc940e8-965c-44cf-9274-2d52e2bb3950', fixed_text).
narrative_ontology:cs_authority_grounding('bbc940e8-965c-44cf-9274-2d52e2bb3950', lineage).
narrative_ontology:cs_interpretation_layer_present('bbc940e8-965c-44cf-9274-2d52e2bb3950').
narrative_ontology:cs_reading_relation('bbc940e8-965c-44cf-9274-2d52e2bb3950', script_as_identity__kemalist_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('bbc940e8-965c-44cf-9274-2d52e2bb3950', script_as_identity__phonetic_instrumentalism_reading, coexists_with).
narrative_ontology:cs_axiom('bbc940e8-965c-44cf-9274-2d52e2bb3950', foundational, arabic_script_as_divine_vehicle).
narrative_ontology:cs_axiom_status(arabic_script_as_divine_vehicle, holdable).
narrative_ontology:cs_axiom_grounding('bbc940e8-965c-44cf-9274-2d52e2bb3950', arabic_script_as_divine_vehicle, theological).
narrative_ontology:cs_axiom('bbc940e8-965c-44cf-9274-2d52e2bb3950', foundational, ottoman_caliphate_as_script_guardian).
narrative_ontology:cs_axiom_status(ottoman_caliphate_as_script_guardian, overridden).
narrative_ontology:cs_axiom_grounding('bbc940e8-965c-44cf-9274-2d52e2bb3950', ottoman_caliphate_as_script_guardian, conventional).
narrative_ontology:cs_axiom('bbc940e8-965c-44cf-9274-2d52e2bb3950', secondary, vernacular_literacy_as_religious_deficit).
narrative_ontology:cs_axiom_status(vernacular_literacy_as_religious_deficit, holdable).
narrative_ontology:cs_axiom_grounding('bbc940e8-965c-44cf-9274-2d52e2bb3950', vernacular_literacy_as_religious_deficit, deontological).
narrative_ontology:cs_reference_frame('bbc940e8-965c-44cf-9274-2d52e2bb3950', classical_ottoman_scriptural_authority).
narrative_ontology:cs_drift_state('bbc940e8-965c-44cf-9274-2d52e2bb3950', late_ottoman_modernist_challenge, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('bbc940e8-965c-44cf-9274-2d52e2bb3950', '').
narrative_ontology:cs_kernel_id(script_as_identity__ottoman_continuity_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, ottoman_religious_establishment).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, islamic_scholarly_class).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, traditional_educational_institutions).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, turkish_literacy_candidates).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, modernization_reformers).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, non_arabic_literate_muslim_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, islamic_scholarly_class).
narrative_ontology:constraint_vindicates(script_as_identity__ottoman_continuity_reading, arabic_script_as_divine_vehicle).
narrative_ontology:constraint_vindicates(script_as_identity__ottoman_continuity_reading, ottoman_islamic_continuity_doctrine).
narrative_ontology:constraint_vindicates(script_as_identity__ottoman_continuity_reading, script_identity_inseparability_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the definition of legitimate Islamic knowledge and the certification of religious authority. Arabic script mastery is the gatekeeping mechanism for entry into the scholarly class (ulema). The script is not merely a tool but the medium through which revelation is accessed and tradition is transmitted. Abandoning it would dissolve the institutional basis of their authority and the coherence of the textual tradition they curate.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ottoman_religious_establishment, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(script_as_identity__ottoman_continuity_reading, ottoman_religious_establishment, beneficiary).

% Their professional identity, social status, and interpretive monopoly are constituted through mastery of the Arabic-script textual corpus (Qur'an, hadith, fiqh, tasawwuf). They bear the high cognitive cost of maintaining this mastery (years of study, limited transferability to secular domains) but collect the rents of religious authority, educational gatekeeping, and communal leadership. Exit means abandoning the only framework in which their capital has value.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, islamic_scholarly_class, beneficiary,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(script_as_identity__ottoman_continuity_reading, islamic_scholarly_class, payer).

% Medreses and associated waqf-endowed schools reproduce the scholarly class through Arabic-script curricula. Their endowments, legal standing, and social function depend on the script's centrality. They extract resources (student labor, charitable donations, state subventions) through their role as preservers of the scriptural tradition. Transition to Latin script would devalue their curriculum, endowments, and institutional rationale.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, traditional_educational_institutions, beneficiary,
    organized, generational, constrained, regional).

% Anatolian Turkish speakers seeking literacy face a dual barrier: Arabic script is poorly suited to Turkish phonology (vowel harmony, consonant clusters) and requires years to master, while the textual corpus they unlock is overwhelmingly Arabic/Persian, not Turkish. The constraint extracts their cognitive labor and time, yielding literacy that does not directly serve their vernacular expressive needs. Exit is structurally blocked: no alternative script is legally or socially permitted for Turkish literacy.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, turkish_literacy_candidates, payer,
    moderate, biographical, trapped, national).

% Ottoman and early Republican officials, intellectuals, and military reformers who see the script as the primary obstacle to mass literacy, technological transfer, and administrative modernization. They bear the political cost of advocating change (accusations of irreligion, treason, cultural betrayal) and the practical cost of operating a state whose bureaucratic and educational infrastructure is locked to an ill-fitting script. Their exit option is regime change — the constraint cannot be reformed from within.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, modernization_reformers, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__ottoman_continuity_reading, modernization_reformers, excluded).

% Muslim populations (Kurds, Circassians, Albanians, Bosniaks, etc.) whose vernaculars are not Arabic and who are excluded from direct access to the scriptural tradition by the script barrier. They are doubly extracted: they bear the cost of learning a foreign script for religious purposes, and their own linguistic development is suppressed because the only legitimate literate culture is Arabic-script Ottoman Turkish. No exit within the imperial framework.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, non_arabic_literate_muslim_subjects, payer,
    powerless, biographical, trapped, regional).

% European scholars who study, catalog, and translate the Arabic-script Ottoman corpus. They benefit epistemically from the script's stability (a fixed target for philology) but have no stake in its enforcement. Their analyses often reinforce the script's prestige by treating it as the authentic vessel of Islamic civilization, inadvertently supporting the beneficiaries' framing.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, western_orientalists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a trans-regional Islamic scholarly community, a shared legal-ethical framework (Sharia), and a legitimating historical narrative across the Ottoman domains by providing a single, stable scriptural medium that transcends vernacular variation.
% TRANSFER_FUNCTION: Moves cognitive labor, study-years, and institutional resources from vernacular-speaking Muslim populations (especially Turkish, Kurdish, Balkan, Caucasian) to the Arabic-script scholarly establishment and its educational infrastructure. The script extracts the cost of learning a non-phonemic orthography and redirects the resulting literacy toward the Arabic/Persian textual tradition rather than vernacular development.
% ABSENT_VOICES: Vernacular Turkish poets and scribes who wrote in Arabic script but chafed at its inadequacy; Kurdish, Albanian, and Bosniak intellectuals who sought literacy in their own languages but were blocked by the script monopoly; women excluded from medrese education whose domestic literacy practices (e.g., elifba primers) were invisible to the scholarly record; early print entrepreneurs prevented from publishing in Turkish by script-based censorship.
% DISAPPEARANCE_RATIONALE: If the Arabic script constraint vanished overnight, the Ottoman religious establishment would lose its gatekeeping monopoly, the medrese system would lose its curricular rationale, Turkish vernacular literacy would explode (as it did historically after 1928), and the trans-regional Islamic scholarly network would fragment into vernacular or Latin-script national Islams. The political theology of the Ottoman state — grounded in the Caliph's guardianship of the Arabic-script revelation — would collapse.
% FOUNDING_PROBLEM: The early Ottoman state needed a legitimating idiom that could integrate diverse Anatolian and Balkan populations under a single sovereign claim. Arabic script, as the vehicle of Qur'anic revelation and the lingua franca of Islamic scholarship, provided a ready-made symbolic and institutional infrastructure for imperial legitimacy that did not require inventing a new literate culture from scratch.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — integrating a multi-ethnic, multi-lingual empire under a single Islamic sovereign idiom — was solved by the 16th century. The script persisted for three centuries after the integration was complete, maintained by the scholarly class whose institutional interests it served. Corroboration: Ottoman administrative records show the script's institutional entrenchment long after the founding integration; Republican-era literacy statistics (1927: ~10% literacy; 1935: ~20% after Latin script) demonstrate the script was the bottleneck, not the population's capacity. The scholarly class's own polemics against Latin script (1920s) explicitly defend their institutional role, not the founding integration.
narrative_ontology:disappearance_verdict(script_as_identity__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__ottoman_continuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__ottoman_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
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
 *   Extractiveness rises from 0.35 to 0.68 over the interval: early Ottoman use was pragmatic (chancery adoption of a prestigious existing script), but as the scholarly class institutionalized and the empire expanded, the script became a rent-generating monopoly. Suppression rises from 0.4 to 0.82: early tolerance for vernacular writing (e.g., Turkish in Arabic script, minority scripts) hardens into active prohibition of print, censorship of Turkish works, and criminalization of alternative scripts after 1850s. Theater ratio rises from 0.1 to 0.28: the coordination function (Islamic scholarly unity) is real but shrinks as a fraction of total enforcement activity; by the late period, most enforcement defends the script monopoly itself, not the scholarly network it once served. Accessibility collapse at 0.78 reflects the near-total closure of legal vernacular literacy alternatives. Resistance at 0.45 is moderate: reformist voices exist but are structurally excluded until the Young Turk period.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, the constraint appears as a rope: genuine coordination of Islamic scholarly civilization, minimal coercion (participants volunteer for the tradition), alternatives not suppressed (vernacular writing existed in margins). From the payer seats, it appears as a snare: the coordination story is cover for a script monopoly that extracts cognitive labor, suppresses print and vernacular development, and persists only through active enforcement. The engine computes this seat divergence from the declared roles, power, exit, and scope.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious establishment and scholarly class are structural beneficiaries (d ~0.15): they collect authority rents, control the textual tradition, and their identity is fused with the script (identity_locked exit). Turkish literacy candidates and non-Arabic Muslim subjects are full targets (d ~0.9): they bear the phonetic mismatch cost, have no legal exit, and are trapped by the script monopoly. Modernization reformers are constrained targets (d ~0.7): they have state power but cannot reform the script without breaking the legitimacy framework they depend on. Western orientalists are analytical observers (d=0.5). The engine will compute per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (imperial integration via Islamic legitimating idiom) was solved by the classical period. The arrangement persisted for centuries as a scaffold that never sunset — the scholarly class became the agenda setter and captured the coordination function for institutional self-preservation. The script's persistence is mandatrophic: the mandate (Islamic unity) atrophied into a rent-extraction mechanism for the ulema and medrese system. The founding_problem_status=dead and disappearance_verdict=world_rearranges mismatch flags this as a captured constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    script_identity_ontology,
    'Is the script constitutively identity-forming (the reading''s claim) or instrumentally identity-marking (a contingent symbol that could be replaced without identity loss)?',
    'Comparative analysis of script changes in other Islamic societies (e.g., Hausa ajami to Latin, Malay jawi to rumi, Swahili arabic to latin) — did communal identity survive script replacement?',
    'If identity survives script change, the reading''s foundational axiom (script_identity_inseparability) is empirically contingent and potentially overridden; if identity collapses, the axiom holds as deontological.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(script_identity_ontology, conceptual, 'Whether Arabic script is ontologically inseparable from Turkish-Islamic identity or contingently associated.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state censorship, print bans, medrese monopoly) or internalized (populations believe vernacular literacy is religiously illegitimate, script mastery equals piety)?',
    'Post-1928 trajectory: if vernacular literacy exploded without coercion (as it did), suppression was primarily structural; if resistance to Latin script persisted decades later in religious communities, internalized suppression was significant.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint''s extraction persists psychologically after legal enforcement ends. Affects classification of the constraint''s legacy effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the script monopoly.').

omega_variable(
    coordination_extraction_boundary,
    'Is the trans-regional Islamic scholarly coordination function genuinely dependent on Arabic script unity, or is script unity a sufficient but not necessary condition that the beneficiaries enforce to capture rents?',
    'Counterfactual: could a shared Latin-script or vernacular-script Islamic scholarly network have coordinated the same functions (fatwa circulation, legal precedent, mystical transmission)? Historical test: post-1928 Turkish religious scholarship in Latin script — did coordination collapse?',
    'If coordination is script-independent, the constraint is a snare (coordination is cover for extraction). If coordination genuinely requires script unity, it is a tangled rope (genuine coordination + asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the coordination function and extraction component are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__ottoman_continuity_reading, 1300, 1928).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t1300, script_as_identity__ottoman_continuity_reading, theater_ratio, 1300, 0.1).
narrative_ontology:measurement(scri_tr_t1453, script_as_identity__ottoman_continuity_reading, theater_ratio, 1453, 0.12).
narrative_ontology:measurement(scri_tr_t1550, script_as_identity__ottoman_continuity_reading, theater_ratio, 1550, 0.15).
narrative_ontology:measurement(scri_tr_t1650, script_as_identity__ottoman_continuity_reading, theater_ratio, 1650, 0.18).
narrative_ontology:measurement(scri_tr_t1750, script_as_identity__ottoman_continuity_reading, theater_ratio, 1750, 0.22).
narrative_ontology:measurement(scri_tr_t1839, script_as_identity__ottoman_continuity_reading, theater_ratio, 1839, 0.25).
narrative_ontology:measurement(scri_tr_t1908, script_as_identity__ottoman_continuity_reading, theater_ratio, 1908, 0.27).
narrative_ontology:measurement(scri_tr_t1928, script_as_identity__ottoman_continuity_reading, theater_ratio, 1928, 0.28).

% Extraction over time
narrative_ontology:measurement(scri_be_t1300, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1300, 0.35).
narrative_ontology:measurement(scri_be_t1453, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1453, 0.45).
narrative_ontology:measurement(scri_be_t1550, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1550, 0.52).
narrative_ontology:measurement(scri_be_t1650, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1650, 0.58).
narrative_ontology:measurement(scri_be_t1750, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1750, 0.62).
narrative_ontology:measurement(scri_be_t1839, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1839, 0.65).
narrative_ontology:measurement(scri_be_t1908, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1908, 0.67).
narrative_ontology:measurement(scri_be_t1928, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1928, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t1300, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1300, 0.4).
narrative_ontology:measurement(scri_su_t1453, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1453, 0.5).
narrative_ontology:measurement(scri_su_t1550, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1550, 0.6).
narrative_ontology:measurement(scri_su_t1650, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1650, 0.68).
narrative_ontology:measurement(scri_su_t1750, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1750, 0.72).
narrative_ontology:measurement(scri_su_t1839, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1839, 0.78).
narrative_ontology:measurement(scri_su_t1908, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1908, 0.8).
narrative_ontology:measurement(scri_su_t1928, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1928, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__ottoman_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__ottoman_continuity_reading, 0.08).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, script_as_identity__kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, script_as_identity__phonetic_instrumentalism_reading).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, ottoman_medrese_system).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, turkish_national_literacy_policy).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the script_as_identity kernel. The ottoman_continuity_reading claims identity-constitutive status for Arabic script (high extraction, high suppression). The kemalist_rupture_reading claims Latin script as identity-severance for secular modernity (different beneficiary/victim structure). The phonetic_instrumentalism_reading claims script neutrality (low extraction, low suppression). They are linked by kernel membership and affect each other's legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__ottoman_continuity_reading, organized, 0.2).
constraint_indexing:directionality_override(script_as_identity__ottoman_continuity_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
