% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__native_daily_reading, []).

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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Hebrew Vitality: Native Daily Reading
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint represents the reading of Hebrew vitality that asserts
 *   only native, daily, vernacular use constitutes a 'living' language,
 *   relegating ritual recitation to mere 'preservation.' This perspective was
 *   central to the Zionist project of language revitalization, which actively
 *   suppressed alternative understandings of Hebrew's status. The constraint
 *   is classified as a Tangled Rope because it genuinely coordinated the
 *   creation of a modern vernacular (a collective good) but did so through
 *   significant extraction from and suppression of the existing liturgical
 *   tradition and diaspora linguistic practices.
 *
 * KEY AGENTS:
 *   - zionist_state_building_project: Primary agenda_setter (institutional/mobile) — actively enforced the vernacularization project.
 *   - secular_hebrew_speakers: Primary beneficiary (organized/mobile) — gained a full vernacular for daily life.
 *   - liturgical_tradition: Primary payer (moderate/identity_locked) — bore the cost of desacralization and re-framing.
 *   - diaspora_jewish_communities: Secondary payer (organized/constrained) — experienced pressure to conform to the vernacular ideal.
 *   - sociolinguists: Analytical observer (analytical/analytical) — study the process without direct extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.65).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.7).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Hebrew Vitality: Native Daily Reading").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, '78de26f2-df73-491d-a48f-a94c6c1b2116').
narrative_ontology:cs_kernel_codification('78de26f2-df73-491d-a48f-a94c6c1b2116', formalized).
narrative_ontology:cs_authority_grounding('78de26f2-df73-491d-a48f-a94c6c1b2116', lineage).
narrative_ontology:cs_interpretation_layer_present('78de26f2-df73-491d-a48f-a94c6c1b2116').
narrative_ontology:cs_reading_relation('78de26f2-df73-491d-a48f-a94c6c1b2116', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('78de26f2-df73-491d-a48f-a94c6c1b2116', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('78de26f2-df73-491d-a48f-a94c6c1b2116', foundational, native_generation_is_life).
narrative_ontology:cs_axiom_status(native_generation_is_life, holdable).
narrative_ontology:cs_axiom_grounding('78de26f2-df73-491d-a48f-a94c6c1b2116', native_generation_is_life, conventional).
narrative_ontology:cs_axiom('78de26f2-df73-491d-a48f-a94c6c1b2116', foundational, ritual_recitation_is_preservation_not_vitality).
narrative_ontology:cs_axiom_status(ritual_recitation_is_preservation_not_vitality, holdable).
narrative_ontology:cs_axiom_grounding('78de26f2-df73-491d-a48f-a94c6c1b2116', ritual_recitation_is_preservation_not_vitality, conventional).
narrative_ontology:cs_reference_frame('78de26f2-df73-491d-a48f-a94c6c1b2116', secular_vernacular_hebrew).
narrative_ontology:cs_drift_state('78de26f2-df73-491d-a48f-a94c6c1b2116', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('78de26f2-df73-491d-a48f-a94c6c1b2116', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, secular_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_tradition).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, diaspora_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promoted Hebrew as a spoken vernacular, establishing educational institutions and cultural norms that prioritized daily, secular use over liturgical functions. Benefited from the creation of a distinct national identity tied to a 'living' language.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_building_project, agenda_setter,
    institutional, generational, mobile, national).

% Are the direct beneficiaries of a language that functions as a full vernacular, enabling daily life, culture, and education outside of religious contexts. Their identity is often fused with the modern Hebrew project.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, secular_hebrew_speakers, beneficiary,
    organized, biographical, mobile, national).

% Bears the cost of desacralization and re-framing of Hebrew from a holy language to a secular one. Its claim to be the primary site of Hebrew vitality is challenged and often dismissed by this reading. Its persistence is due to deep historical and religious identity.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, liturgical_tradition, payer,
    moderate, civilizational, identity_locked, global).

% Experience a tension between the liturgical Hebrew of their heritage and the modern, secular Hebrew of Israel. They are often pressured to adopt the 'native daily reading' perspective, which can devalue their own forms of Hebrew engagement.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, diaspora_jewish_communities, payer,
    organized, generational, constrained, global).

% Analyze the processes of language death and revitalization, often using the Hebrew case as a prime example. They observe the structural shifts and power dynamics without being directly subject to the constraint's extraction.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, sociolinguists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the efforts of a national project to establish a shared, living language for daily communication, education, and cultural production, distinct from its historical liturgical use.
% TRANSFER_FUNCTION: Transfers linguistic authority and cultural capital from traditional religious institutions and diaspora communities to secular national institutions and native speakers, along with resources for lexical expansion and pedagogical development.
% ABSENT_VOICES: Traditionalists who view Hebrew's vitality as intrinsically linked to its sacred function and unbroken liturgical chain are often marginalized or dismissed by this reading, their perspective deemed 'preservation' rather than 'life'.
% DISAPPEARANCE_RATIONALE: If the constraint that only native daily reading constitutes vitality vanished, the cultural and political landscape of modern Israel would be fundamentally altered. The legitimacy of the secular Hebrew project would be challenged, and alternative forms of Hebrew engagement (e.g., liturgical) would gain renewed prominence, leading to a significant re-evaluation of linguistic identity.
% FOUNDING_PROBLEM: The problem of a Jewish national identity lacking a common, spoken vernacular, relying instead on a sacred language primarily used in ritual contexts, which was perceived as insufficient for modern nation-building.
% FOUNDING_PROBLEM_CORROBORATION: The Zionist state-building project and secular Hebrew speakers attest that the problem of national identity and a living language remains live, citing ongoing cultural production and educational needs. Sociolinguists corroborate the historical problem of language shift and the perceived need for vernacularization in nation-building, though they may dispute the 'deadness' of liturgical forms.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__native_daily_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__native_daily_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_vitality__native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because the vernacularization project required significant institutional investment and lexical expansion, which came at the cost of devaluing and marginalizing existing forms of Hebrew use. Suppression is high (0.70) due to active institutional policies promoting secular Hebrew and discouraging other forms, including educational reforms and cultural campaigns. Theater ratio is low (0.10) as the project was genuinely functional in creating a spoken language, with little performative maintenance. The time series shows rising extractiveness and suppression as the vernacularization project gained momentum and institutionalized its dominance, then a slight leveling off as the language became established.
 *
 * PERSPECTIVAL GAP:
 *   The Zionist state-building project and secular Hebrew speakers would experience this as a Rope, a necessary coordination mechanism for national identity. The liturgical tradition and diaspora communities would experience it as a Snare, as their forms of Hebrew engagement were actively suppressed and devalued. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Zionist state-building project and secular Hebrew speakers are clear beneficiaries, as the constraint directly enabled their goals of a modern, secular national language. The liturgical tradition and diaspora communities are victims, as their historical and religious connection to Hebrew was actively undermined and their linguistic practices were de-legitimized. The 'identity_locked' exit for the liturgical tradition reflects its deep historical and religious grounding, making exit from its commitment to Hebrew's sacred status virtually impossible.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the vernacularization project as pure coordination (Rope) by highlighting the significant extraction from and suppression of existing linguistic forms. It also avoids mislabeling it as a pure Snare by acknowledging the genuine coordination function of creating a living language for a new nation. The 'contested' status of the founding problem reflects the ongoing debate about whether the original problem of a 'dead' language was accurately framed or if it served to justify a particular national project.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_definition_ambiguity,
    'Is ''vitality'' inherently defined by native, daily, vernacular use, or can it encompass other forms of continuous engagement, such as liturgical use?',
    'Conceptual analysis of linguistic vitality metrics across diverse language communities, and a re-evaluation of the historical and cultural functions of ''non-vernacular'' languages.',
    'If vitality is broadened, the extractiveness and suppression of this constraint would be re-evaluated downwards, potentially shifting its classification towards a Rope or even a Mountain (if liturgical continuity is seen as natural). If the narrow definition holds, the current classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vitality_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''language vitality'' itself.').

omega_variable(
    desacralization_necessity,
    'Was the desacralization of Hebrew a necessary precondition for its vernacularization, or could a ''living'' language have emerged while retaining its sacred status?',
    'Comparative historical linguistics of other languages with sacred and secular registers, or counterfactual historical analysis of the Hebrew revival process.',
    'If desacralization was not strictly necessary, the extraction from the liturgical tradition could be seen as an avoidable cost, increasing the perceived extractiveness of the constraint. If it was necessary, the extraction might be viewed as an unavoidable cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desacralization_necessity, empirical, 'Whether desacralization was a necessary cost of vernacularization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 1880, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_vitality__native_daily_reading, theater_ratio, 1880, 0.05).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_vitality__native_daily_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_vitality__native_daily_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_vitality__native_daily_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_vitality__native_daily_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_vitality__native_daily_reading, base_extractiveness, 1880, 0.4).
narrative_ontology:measurement(hebr_be_t1920, hebrew_vitality__native_daily_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(hebr_be_t1960, hebrew_vitality__native_daily_reading, base_extractiveness, 1960, 0.65).
narrative_ontology:measurement(hebr_be_t2000, hebrew_vitality__native_daily_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(hebr_be_t2020, hebrew_vitality__native_daily_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_vitality__native_daily_reading, suppression_requirement, 1880, 0.3).
narrative_ontology:measurement(hebr_su_t1920, hebrew_vitality__native_daily_reading, suppression_requirement, 1920, 0.5).
narrative_ontology:measurement(hebr_su_t1960, hebrew_vitality__native_daily_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(hebr_su_t2000, hebrew_vitality__native_daily_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(hebr_su_t2020, hebrew_vitality__native_daily_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
