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
 *   This constraint story analyzes the 'native daily reading' of Hebrew
 *   vitality, which posits that only its use as a spoken, vernacular language
 *   for daily life constitutes true 'life,' while ritual recitation is merely
 *   'preservation.' This reading was central to the Zionist project's efforts
 *   to establish Hebrew as a modern national language. It is a reading of the
 *   'hebrew_vitality' kernel, distinguishing itself from readings that
 *   emphasize liturgical continuity or a hybrid approach.
 *
 * KEY AGENTS:
 *   - zionist_state_building_project: Primary beneficiary/agenda_setter (institutional/mobile)
 *   - hebrew_language_academics: Beneficiary (organized/constrained)
 *   - liturgical_tradition_adherents: Primary payer (moderate/identity_locked)
 *   - diaspora_jewish_communities: Payer (organized/constrained)
 *   - linguistic_revitalization_theorists: Analytical observer (analytical/analytical)
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
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, '3ef30869-8199-4a4f-a4b1-444004f8c806').
narrative_ontology:cs_kernel_codification('3ef30869-8199-4a4f-a4b1-444004f8c806', formalized).
narrative_ontology:cs_authority_grounding('3ef30869-8199-4a4f-a4b1-444004f8c806', extraction).
narrative_ontology:cs_interpretation_layer_present('3ef30869-8199-4a4f-a4b1-444004f8c806').
narrative_ontology:cs_reading_relation('3ef30869-8199-4a4f-a4b1-444004f8c806', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('3ef30869-8199-4a4f-a4b1-444004f8c806', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('3ef30869-8199-4a4f-a4b1-444004f8c806', foundational, vernacular_use_is_life).
narrative_ontology:cs_axiom_status(vernacular_use_is_life, holdable).
narrative_ontology:cs_axiom_grounding('3ef30869-8199-4a4f-a4b1-444004f8c806', vernacular_use_is_life, conventional).
narrative_ontology:cs_axiom('3ef30869-8199-4a4f-a4b1-444004f8c806', foundational, ritual_use_is_preservation_not_life).
narrative_ontology:cs_axiom_status(ritual_use_is_preservation_not_life, holdable).
narrative_ontology:cs_axiom_grounding('3ef30869-8199-4a4f-a4b1-444004f8c806', ritual_use_is_preservation_not_life, conventional).
narrative_ontology:cs_reference_frame('3ef30869-8199-4a4f-a4b1-444004f8c806', modern_national_language_paradigm).
narrative_ontology:cs_drift_state('3ef30869-8199-4a4f-a4b1-444004f8c806', contemporary_global_diaspora, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('3ef30869-8199-4a4f-a4b1-444004f8c806', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, hebrew_language_academics).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_tradition_adherents).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, diaspora_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promoted the vernacularization of Hebrew as a core component of national identity and sovereignty. Invested heavily in educational institutions and media to establish Hebrew as a spoken, daily language, thereby 'reviving' it from its liturgical status. Benefits from the symbolic and practical unity a common, living language provides.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_building_project, agenda_setter,
    institutional, generational, mobile, national).

% Their professional careers and research agendas are validated by the success of Hebrew as a modern, spoken language. They contribute to lexical expansion and grammatical standardization, reinforcing the 'native generation' criterion for vitality. Benefit from institutional funding and prestige associated with a living language.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, hebrew_language_academics, beneficiary,
    organized, biographical, constrained, global).

% Experience the desacralization and recontextualization of Hebrew as a loss. Their understanding of Hebrew's vitality is rooted in its unbroken ritual use, which this reading diminishes. They bear the cost of having their tradition's definition of 'life' for the language challenged and marginalized by the dominant narrative.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, liturgical_tradition_adherents, payer,
    moderate, generational, identity_locked, global).

% Historically maintained Hebrew through liturgical and scholarly use, often without daily vernacularization. This reading implies their historical relationship with Hebrew was merely 'preservation' rather than 'life,' diminishing their cultural continuity and contributions. They face pressure to adopt the vernacular standard to be considered 'truly' connected to a living Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, diaspora_jewish_communities, payer,
    organized, generational, constrained, global).

% Analyze the Hebrew case as a unique example of successful language revitalization, often adopting the 'native generation' criterion as a benchmark for vitality. They observe the dynamics of the constraint without being directly subject to its enforcement.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, linguistic_revitalization_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the efforts of a nascent nation-state and its educational/cultural institutions to establish a common, spoken language for daily life, fostering national cohesion and a distinct cultural identity.
% TRANSFER_FUNCTION: Transfers the definition of 'vitality' from liturgical continuity to native, daily vernacular use, thereby transferring cultural authority and resources from traditional religious institutions to secular, national ones. It also transfers linguistic innovation and expansion from scholarly/religious contexts to everyday speech.
% ABSENT_VOICES: Scholars and practitioners of other 'sleeping' or 'revitalized' languages who might argue for broader definitions of vitality that include non-native, non-daily forms of use (e.g., ceremonial languages, scholarly languages) are excluded from the dominant discourse, which prioritizes the Hebrew model.
% DISAPPEARANCE_RATIONALE: If the constraint (that only native, daily use constitutes vitality) vanished, the narrative of Hebrew's 'revival' would be fundamentally altered. The Zionist project's linguistic foundation would be re-evaluated, and the historical contributions of liturgical use would gain renewed recognition. The criteria for language vitality globally might broaden, leading to a rearrangement of revitalization strategies.
% FOUNDING_PROBLEM: The problem was the perceived lack of a common, modern, spoken language for the Jewish people in their ancestral homeland, hindering national self-determination and cultural unity in the early 20th century.
% FOUNDING_PROBLEM_CORROBORATION: The Zionist state-building project and its cultural institutions continue to attest that the problem of maintaining and expanding Hebrew as a living, modern language is live, citing ongoing needs for lexical development and cultural integration. Independent sociolinguists corroborate the historical context of the problem, though they may contest the 'dead' status of liturgical Hebrew.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) is moderate because the vernacularization project required significant institutional enforcement and cultural reorientation, which extracted resources and legitimacy from the existing liturgical tradition. Suppression (0.70) was high, as alternative definitions of vitality were actively marginalized or dismissed. The 'native daily reading' required a substantial shift in linguistic practice and perception, enforced through education, media, and national discourse. Theater ratio is low (0.10) because the project was genuinely functional in establishing a spoken language, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   The 'native daily reading' is experienced as a foundational act of national building by its beneficiaries (Zionist state-building project, Hebrew language academics), who see it as a 'rope' or 'scaffold' for national cohesion. However, for those rooted in the liturgical tradition, it functions as a 'snare' or 'tangled_rope,' extracting their historical claim to vitality and imposing a new, secular standard. The engine's classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Zionist state-building project and Hebrew language academics are clear beneficiaries, as the constraint directly serves their goals of national and academic legitimacy. Liturgical tradition adherents and diaspora Jewish communities are payers, as their historical relationship with Hebrew is devalued, and they face pressure to conform to the new standard. Their 'identity_locked' exit options reflect the deep cultural and religious ties that make abandoning Hebrew (even in its liturgical form) unthinkable, amplifying the effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'tangled_rope' prevents mislabeling this as a pure 'rope' (as its proponents might claim) by highlighting the asymmetric extraction from the liturgical tradition. It also avoids mislabeling it as a pure 'snare' by acknowledging the genuine coordination function of establishing a common national language. The 'contested' status of the founding problem further indicates that while the initial coordination problem (lack of a modern spoken language) was real, the persistence of the constraint now involves maintaining a specific definition of vitality that benefits certain parties at the expense of others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_vitality,
    'Is ''native daily generation'' the sole valid criterion for language vitality, or do other forms of active use (e.g., liturgical, scholarly) also constitute ''life''?',
    'Cross-linguistic comparative studies of language maintenance and revitalization, incorporating diverse cultural perspectives on linguistic ''life'' beyond the vernacular model.',
    'If other forms of use are recognized as vital, the extractiveness from liturgical tradition adherents would decrease, potentially reclassifying the constraint closer to a ''rope'' or ''scaffold'' by broadening its beneficiary base and reducing its suppressive effect on alternative definitions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_vitality, conceptual, 'Ambiguity in the definition of language vitality and its impact on the status of non-vernacular uses.').

omega_variable(
    desacralization_cost,
    'What is the full cultural and spiritual cost of desacralizing Hebrew by shifting its primary domain from sacred text to secular daily life?',
    'Qualitative sociological and anthropological studies within affected communities, documenting the long-term impacts on religious practice, identity, and cultural transmission.',
    'A higher documented cost would increase the perceived extractiveness from liturgical tradition adherents, reinforcing the ''snare'' aspect of the constraint and potentially shifting its classification further towards pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desacralization_cost, empirical, 'The unquantified cultural cost of shifting Hebrew''s primary domain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1900, hebrew_vitality__native_daily_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(hebr_tr_t1930, hebrew_vitality__native_daily_reading, theater_ratio, 1930, 0.08).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_vitality__native_daily_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(hebr_tr_t1990, hebrew_vitality__native_daily_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_vitality__native_daily_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1900, hebrew_vitality__native_daily_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(hebr_be_t1930, hebrew_vitality__native_daily_reading, base_extractiveness, 1930, 0.55).
narrative_ontology:measurement(hebr_be_t1960, hebrew_vitality__native_daily_reading, base_extractiveness, 1960, 0.65).
narrative_ontology:measurement(hebr_be_t1990, hebrew_vitality__native_daily_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(hebr_be_t2020, hebrew_vitality__native_daily_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1900, hebrew_vitality__native_daily_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(hebr_su_t1930, hebrew_vitality__native_daily_reading, suppression_requirement, 1930, 0.5).
narrative_ontology:measurement(hebr_su_t1960, hebrew_vitality__native_daily_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(hebr_su_t1990, hebrew_vitality__native_daily_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(hebr_su_t2020, hebrew_vitality__native_daily_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hebrew_vitality' kernel. Its definition of vitality (native daily use) directly influences and is influenced by other readings, such as the 'liturgical_reading' (vitality through ritual use) and the 'hybrid_continuity_reading' (vitality through both liturgical substrate and vernacular reconstruction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
