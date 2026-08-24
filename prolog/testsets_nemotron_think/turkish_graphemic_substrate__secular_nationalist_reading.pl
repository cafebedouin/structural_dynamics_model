% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__secular_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__secular_nationalist_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__secular_nationalist_reading
 *   human_readable: Turkish Latin Script Reform (Secular Nationalist Reading)
 *   domain: political_linguistics/state_formation/cultural_engineering
 *
 * SUMMARY:
 *   The 1928 Turkish alphabet reform replaced the Arabic script with a
 *   modified Latin alphabet by state decree, framed as a necessary rupture
 *   aligning Turkish identity with European modernity. The secular
 *   nationalist reading presents this as a coordination achievement: solving
 *   Ottoman multi-script chaos, enabling mass literacy, and integrating
 *   Turkey into European knowledge networks. The authored metrics describe a
 *   constraint that simultaneously coordinated (real literacy gains,
 *   bureaucratic unification) and extracted (generational rupture, epistemic
 *   dispossession of Ottoman-educated classes, religious authority transfer
 *   to the state). The claim/metric gap is deliberate: the reading claims
 *   rope/scaffold while the structural data indicates tangled_rope — the
 *   engine measures that divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, 0.75).
domain_priors:suppression_score(turkish_graphemic_substrate__secular_nationalist_reading, 0.85).
domain_priors:theater_ratio(turkish_graphemic_substrate__secular_nationalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__secular_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__secular_nationalist_reading, "Turkish Latin Script Reform (Secular Nationalist Reading)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__secular_nationalist_reading, "political_linguistics/state_formation/cultural_engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__secular_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__secular_nationalist_reading, 'cb77b72a-cca5-4be2-9a70-bf3f25b0d6a7').
narrative_ontology:cs_kernel_codification('cb77b72a-cca5-4be2-9a70-bf3f25b0d6a7', formalized).
narrative_ontology:cs_authority_grounding('cb77b72a-cca5-4be2-9a70-bf3f25b0d6a7', extraction).
narrative_ontology:cs_interpretation_layer_present('cb77b72a-cca5-4be2-9a70-bf3f25b0d6a7').
narrative_ontology:cs_reading_relation('cb77b72a-cca5-4be2-9a70-bf3f25b0d6a7', turkish_graphemic_substrate__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('cb77b72a-cca5-4be2-9a70-bf3f25b0d6a7', turkish_graphemic_substrate__gradual_transition_reading, influences).
narrative_ontology:cs_axiom('cb77b72a-cca5-4be2-9a70-bf3f25b0d6a7', foundational, latin_script_necessary_for_european_modernity).
narrative_ontology:cs_axiom_status(latin_script_necessary_for_european_modernity, holdable).
narrative_ontology:cs_axiom_grounding('cb77b72a-cca5-4be2-9a70-bf3f25b0d6a7', latin_script_necessary_for_european_modernity, instrumental).
narrative_ontology:cs_axiom('cb77b72a-cca5-4be2-9a70-bf3f25b0d6a7', foundational, turkish_identity_distinct_from_ottoman_islamic).
narrative_ontology:cs_axiom_status(turkish_identity_distinct_from_ottoman_islamic, holdable).
narrative_ontology:cs_axiom_grounding('cb77b72a-cca5-4be2-9a70-bf3f25b0d6a7', turkish_identity_distinct_from_ottoman_islamic, deontological).
narrative_ontology:cs_reference_frame('cb77b72a-cca5-4be2-9a70-bf3f25b0d6a7', kemalist_rupture_modernity).
narrative_ontology:cs_drift_state('cb77b72a-cca5-4be2-9a70-bf3f25b0d6a7', contemporary_neo_ottoman_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cb77b72a-cca5-4be2-9a70-bf3f25b0d6a7', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__secular_nationalist_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, secular_nationalist_elites).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, european_oriented_intellectuals).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, later_generations).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_educated_generation).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, religious_establishment).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, traditional_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, minority_communities).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, minority_communities).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, turkish_identity_distinct_from_ottoman_islamic).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, latin_script_aligns_with_european_modernity).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, generational_rupture_necessary_for_modernization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the 1928 alphabet law and 1934 prohibition of Arabic script; controls education, publishing, and legal systems. Uses script reform to consolidate territorial homogeneity and epistemic monopoly over the national archive. Collects administrative efficiency gains and symbolic sovereignty.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, turkish_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Military officers, CHP cadres, and Western-educated intellectuals who championed the reform. Gained cultural capital as 'modernizers,' control over the new educational apparatus, and legitimacy from European recognition. Their class position was secured by the rupture with the Ottoman past.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, secular_nationalist_elites, beneficiary,
    powerful, biographical, mobile, national).

% Writers, scientists, and journalists who gained direct access to European literature and scientific discourse through Latin script. Their professional networks and publication venues expanded; they became the primary translators and mediators of European modernity into Turkish.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, european_oriented_intellectuals, beneficiary,
    organized, biographical, mobile, national).

% Citizens educated entirely in Latin script from the 1930s onward. Inherit seamless literacy in the state script, access to global knowledge economies, and a national identity aligned with Europe. Bear no personal cost of the transition but inherit the epistemic closure it created.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, later_generations, beneficiary,
    organized, generational, arbitrage, national).

% Adults literate in Arabic script (Ottoman Turkish) who lost functional literacy overnight. Could not read new laws, newspapers, or their own correspondence without relearning. Many were civil servants, judges, teachers, and merchants whose professional authority evaporated. No exit option — the state mandated the change universally.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_educated_generation, payer,
    moderate, immediate, trapped, national).

% Ulema, medrese teachers, and Sufi orders whose textual authority rested on Arabic-script Quranic exegesis, hadith collections, and Islamic jurisprudence. The reform severed intergenerational transmission of religious knowledge and transferred interpretive authority to the state-controlled Diyanet. Exit means abandoning their vocation and epistemic community.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, religious_establishment, payer,
    organized, biographical, identity_locked, national).

% Historians, philologists, and literati trained in Ottoman archives. Lost direct access to the primary sources of six centuries of Ottoman history. Forced to depend on state-sanctioned translations and Latin-script editions controlled by the Turkish Historical Society and Turkish Language Association (TDK).
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, traditional_scholars, payer,
    moderate, biographical, constrained, national).

% Armenian, Greek, and Jewish communities who had used Arabic script for Turkish-language publications alongside their own scripts. The unitary Latin mandate erased their distinct Turkish-language press but also gave some minority publishers a shared script for intercommunal publishing. Dual position: lost script autonomy, gained integration into national print market.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, minority_communities, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__secular_nationalist_reading, minority_communities, beneficiary).

% European diplomats, linguists, and orientalists who documented the reform as a 'modernization miracle' or 'cultural amputation.' Their accounts shaped the international legitimacy of the Turkish Republic but had no stake in the domestic extraction.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized literacy and bureaucratic communication across the national territory, enabling mass education, unified legal codes, and integration with European scientific and commercial networks.
% TRANSFER_FUNCTION: Moves cultural authority and epistemic access from Ottoman-educated elites and religious establishment to the secular state bureaucracy and European-oriented intellectual class; moves literacy burden onto the Ottoman-educated generation who must relearn or lose access.
% ABSENT_VOICES: Kurdish and other minority language communities whose script choices were overridden by the unitary Latin mandate; Ottoman-era women scholars and Sufi networks whose textual traditions were rendered illegible; diaspora communities outside Turkey who maintained Arabic-script Turkish publications into the 1950s.
% DISAPPEARANCE_RATIONALE: If the Latin script mandate vanished overnight, the Turkish education system, legal code, publishing industry, and digital infrastructure would all require fundamental restructuring; the generational literacy break would reverse but the state's epistemic monopoly over the national archive would dissolve.
% FOUNDING_PROBLEM: The Ottoman Empire's multi-script, multi-lingual complexity hindered centralized administration, mass literacy, and technological integration with Europe; the Arabic script was seen as ill-suited for Turkish phonology and as a barrier to European scientific engagement.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by contemporary European observers (e.g., Bernard Lewis, Geoffrey Lewis) and Ottoman reformists who documented Arabic script's inadequacy for Turkish vowel harmony; however, counter-evidence from Ottoman literacy rates and the vibrant Arabic-script press suggests the problem was amplified by nationalist ideology. Corroboration from outside the beneficiary set includes Armenian and Greek minority press debates of the 1910s-20s documenting script inadequacy for their own languages.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__secular_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__secular_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__secular_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75 peak) because the reform transferred epistemic authority and cultural capital from the Ottoman-educated generation to the state bureaucracy without compensation; suppression is very high (0.85 initial) because the constraint's persistence depended on legal prohibition of Arabic script, mandatory Latin-script education, and penal sanctions for non-compliance. Theater ratio rises over time (0.25→0.55) as the coordination function (literacy) stabilizes but performative celebration of the reform (Script Week, nationalist historiography) expands to mask the extraction. Accessibility collapse is high (0.8) because Arabic-script literacy became functionally illegal and Ottoman archives were made inaccessible to new generations. Resistance was moderate (0.55) — significant but overwhelmed by state capacity.
 *
 * PERSPECTIVAL GAP:
 *   From the state/elite seat the constraint computes as rope (coordination achieved, beneficiaries net positive). From the Ottoman-educated and religious seats it computes as snare (extraction enforced, alternatives suppressed). The engine computes this divergence from the structural data; the authored claimed_type (tangled_rope) reflects the hybrid reality visible from the analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish state and secular nationalist elites are structural beneficiaries (d near 0.0): they collect administrative control, cultural legitimacy, and European alignment. The Ottoman-educated generation and religious establishment are structural targets (d near 1.0): they bear the full cost of re-literacy, professional obsolescence, and epistemic dispossession with trapped/identity_locked exit. Later generations are beneficiaries with arbitrage-grade exit (d negative). Minority communities sit near symmetric (d ~0.5) — dual payer/beneficiary position. International observers are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Ottoman administrative-linguistic complexity blocking modernization) was real but contested in severity. The reform solved it but the mandate persisted long after the coordination function stabilized — no sunset clause, no script pluralism. The theater_ratio rise indicates performative maintenance of a rupture that has become identity-constitutive. Mandatrophy is unresolved: the constraint's mandate (Latin script as modernity's substrate) has outlived its coordination necessity but persists as identity boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_nationalist_reading_of_turkish_graphemic_substrate_kernel,
    'Is the secular nationalist reading''s core premise (Turkish identity requires rupture with Ottoman-Islamic past; Latin script is structurally necessary for European modernity) a genuine coordination necessity or an extractive identity claim?',
    'Comparative analysis of script reforms in other contexts (Azerbaijan 1920s/1990s, Kazakhstan 2010s, Central Asian post-Soviet transitions) testing whether Latin script adoption correlates with modernization outcomes independent of state enforcement intensity.',
    'If Latin script proves functionally unnecessary for Turkish phonology or modernization (as gradual_transition_reading argues), the reform''s extraction is gratuitous and the constraint is snare-like; if functionally necessary, the extraction is the price of coordination and the constraint is tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_nationalist_reading_of_turkish_graphemic_substrate_kernel, conceptual, 'Whether the secular nationalist reading''s foundational axioms are coordination necessities or extractive identity claims.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal prohibition, educational monopoly) or partially internalized (shame about Arabic-script literacy, self-censorship of Ottoman heritage)?',
    'Post-1950s relaxation of Arabic-script publishing bans: if Ottoman-script literacy and engagement with Ottoman sources remain low despite legal permission, internalized suppression is significant.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — the target carries the suppression after legal enforcement relaxes. This would increase effective extraction for identity_locked agents (religious establishment, traditional scholars).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the script reform.').

omega_variable(
    coordination_extraction_boundary,
    'Was the Arabic script genuinely inadequate for Turkish phonology and mass literacy, or was it adaptable (as demonstrated by Ottoman printing history and minority-language Arabic-script publications)?',
    'Linguistic analysis of Ottoman Turkish orthography''s fit to Turkish vowel harmony compared to the 1928 Latin alphabet; historical literacy rate data from late Ottoman period vs. early Republican period controlling for schooling expansion.',
    'If Arabic script was adaptable, the coordination function is weaker and extraction dominates (snare/tangled_rope); if genuinely inadequate, coordination is stronger (rope/tangled_rope with higher coordination share).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the reform''s coordination function was structurally necessary or ideologically inflated.').

omega_variable(
    minority_disproportionate_extraction,
    'Did the unitary Latin mandate extract disproportionately from non-Turkish minorities by erasing their distinct Turkish-language literary spheres?',
    'Archive research on Armenian, Greek, and Judeo-Spanish (Ladino) Turkish-language press 1908-1935: circulation, closure dates, and community petitions regarding script policy.',
    'If minorities bore higher extraction without compensatory benefit, the constraint''s victim set expands and its snare character intensifies; if they gained net integration benefits, the beneficiary set expands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_disproportionate_extraction, empirical, 'Whether minority communities were net victims or net beneficiaries of the unitary script mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__secular_nationalist_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t0, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(turk_tr_t10, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(turk_tr_t20, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(turk_tr_t30, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(turk_tr_t50, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(turk_tr_t70, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 70, 0.52).
narrative_ontology:measurement(turk_tr_t90, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 90, 0.55).

% Extraction over time
narrative_ontology:measurement(turk_be_t0, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(turk_be_t10, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(turk_be_t20, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(turk_be_t30, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 30, 0.73).
narrative_ontology:measurement(turk_be_t50, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(turk_be_t70, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 70, 0.62).
narrative_ontology:measurement(turk_be_t90, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 90, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t0, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(turk_su_t10, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 10, 0.88).
narrative_ontology:measurement(turk_su_t20, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(turk_su_t30, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(turk_su_t50, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(turk_su_t70, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 70, 0.65).
narrative_ontology:measurement(turk_su_t90, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 90, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__secular_nationalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(turkish_graphemic_substrate__secular_nationalist_reading, 0.08).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_education_system).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_legal_codification).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, minority_language_policies).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_historical_society_archive_access).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the turkish_graphemic_substrate kernel. The ottoman_continuity_reading and gradual_transition_reading are sibling constraints. This reading forecloses the ottoman_continuity_reading (mutually exclusive identity claims) and influences the gradual_transition_reading (rapid reform made gradualism politically non-viable).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(turkish_graphemic_substrate__secular_nationalist_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
