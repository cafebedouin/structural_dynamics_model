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
 *   human_readable: Hebrew Vitality: Native Daily Reading as Sole Criterion
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint represents the reading of Hebrew vitality that
 *   prioritizes native, daily speech over all other forms of use,
 *   particularly liturgical. It emerged during the Zionist project, which
 *   actively enforced the vernacularization of Hebrew. The constraint is
 *   claimed as a 'tangled_rope' because it genuinely coordinated the creation
 *   of a modern spoken language but did so by extracting value and legitimacy
 *   from the existing liturgical tradition. The metrics reflect the
 *   increasing institutional effort to establish this definition of vitality
 *   and suppress alternative views.
 *
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
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Hebrew Vitality: Native Daily Reading as Sole Criterion").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, 'f647573b-e1ce-47bb-ab6f-a92c716397ed').
narrative_ontology:cs_kernel_codification('f647573b-e1ce-47bb-ab6f-a92c716397ed', formalized).
narrative_ontology:cs_authority_grounding('f647573b-e1ce-47bb-ab6f-a92c716397ed', extraction).
narrative_ontology:cs_interpretation_layer_present('f647573b-e1ce-47bb-ab6f-a92c716397ed').
narrative_ontology:cs_reading_relation('f647573b-e1ce-47bb-ab6f-a92c716397ed', hebrew_vitality__liturgical_reading, influences).
narrative_ontology:cs_reading_relation('f647573b-e1ce-47bb-ab6f-a92c716397ed', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('f647573b-e1ce-47bb-ab6f-a92c716397ed', foundational, native_speech_is_life).
narrative_ontology:cs_axiom_status(native_speech_is_life, holdable).
narrative_ontology:cs_axiom_grounding('f647573b-e1ce-47bb-ab6f-a92c716397ed', native_speech_is_life, conventional).
narrative_ontology:cs_axiom('f647573b-e1ce-47bb-ab6f-a92c716397ed', secondary, liturgical_use_is_preservation_not_vitality).
narrative_ontology:cs_axiom_status(liturgical_use_is_preservation_not_vitality, holdable).
narrative_ontology:cs_axiom_grounding('f647573b-e1ce-47bb-ab6f-a92c716397ed', liturgical_use_is_preservation_not_vitality, conventional).
narrative_ontology:cs_reference_frame('f647573b-e1ce-47bb-ab6f-a92c716397ed', hebrew_as_dead_language).
narrative_ontology:cs_drift_state('f647573b-e1ce-47bb-ab6f-a92c716397ed', post_revival_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('f647573b-e1ce-47bb-ab6f-a92c716397ed', '').
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

% Actively promoted the exclusive use of Hebrew as a spoken vernacular, requiring lexical expansion and institutional enforcement to establish it as a living language. Benefited from the symbolic power of a 'revived' language for national identity.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_building_project, agenda_setter,
    institutional, generational, mobile, national).

% Are the direct beneficiaries of the successful vernacularization, experiencing Hebrew as their native, daily language. Their identity is fused with this modern, secular usage, often at the expense of traditional liturgical forms.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, secular_hebrew_speakers, beneficiary,
    organized, biographical, mobile, national).

% Bears the cost of desacralization and redefinition, as its centuries-long role as the primary mode of Hebrew use is reclassified as 'preservation, not life.' Its continuity is devalued in favor of a new, secular standard of vitality.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, liturgical_tradition, payer,
    moderate, civilizational, identity_locked, global).

% Are often marginalized by this definition of vitality, as their primary engagement with Hebrew is through prayer and study, not daily speech. This reading implies their connection to Hebrew is less 'vital' than that of native speakers in Israel.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, diaspora_jewish_communities, payer,
    powerless, generational, constrained, global).

% Analyze the historical processes and outcomes of Hebrew revitalization, evaluating the claims of vitality against empirical linguistic criteria. They observe the contest between different definitions of language life.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, sociolinguists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the establishment of a shared, modern vernacular for a new national identity, enabling daily communication, cultural production, and state administration in Hebrew.
% TRANSFER_FUNCTION: Transfers linguistic authority and symbolic capital from the liturgical tradition to the modern, secular, native-speaking community, redefining 'vitality' in the process.
% ABSENT_VOICES: Traditionalists who view liturgical use as the ultimate form of vitality, and scholars who emphasize the unbroken chain of Hebrew's use throughout history, are often excluded from the dominant discourse on 'revival' that centers native speech.
% DISAPPEARANCE_RATIONALE: If the constraint that only native generation constitutes vitality vanished, the discourse around Hebrew's status would immediately shift. Liturgical use would regain its standing as a form of vitality, and the historical continuity of Hebrew would be re-emphasized, challenging the 'revival' narrative and its associated nationalistic claims.
% FOUNDING_PROBLEM: The perceived lack of a living, spoken Hebrew vernacular among Jewish communities, hindering the formation of a modern national identity and daily communication.
% FOUNDING_PROBLEM_CORROBORATION: The Zionist project and its proponents attest that the problem of a non-vernacular Hebrew was live and required radical intervention. Sociolinguists corroborate the historical context of non-native daily use but contest the framing of liturgical use as 'dead' rather than 'preserved'.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high because the definition of vitality extracts symbolic and cultural capital from the liturgical tradition, reclassifying it as 'preservation' rather than 'life.' Suppression is also high, reflecting the active institutional efforts (e.g., 'language wars,' educational policies) to enforce vernacular use and marginalize other forms. Theater ratio is low as the project was genuinely focused on creating a living language, not merely performing it. The rising extractiveness and suppression over time reflect the increasing institutionalization and enforcement of this specific definition of vitality.
 *
 * PERSPECTIVAL GAP:
 *   The Zionist state-building project and secular Hebrew speakers experience this as a successful 'revival' and a necessary coordination function for national identity. The liturgical tradition and diaspora communities experience it as a form of extraction, where their historical connection to Hebrew is devalued and their linguistic practices are deemed less 'vital.'
 *
 * DIRECTIONALITY LOGIC:
 *   The Zionist state-building project and secular Hebrew speakers are beneficiaries, as their vision of a modern, native Hebrew was realized. The liturgical tradition and diaspora communities are victims, as their forms of Hebrew use were de-legitimized and their cultural capital diminished. Active enforcement was required to shift linguistic norms and suppress resistance from traditionalists.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the vernacularization project as pure coordination. While it solved a genuine coordination problem (creating a modern spoken language), it did so through asymmetric extraction from existing linguistic traditions. The 'mandate' to create a vernacular was achieved, but the 'trophy' of vitality was awarded exclusively to native speech, at the expense of other forms, indicating a tangled rope structure rather than a pure rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_vitality,
    'Is ''native daily generation'' the sole valid criterion for language vitality, or do other forms of continuous use (e.g., liturgical, scholarly) also constitute vitality?',
    'Cross-linguistic comparative studies of language maintenance and revival, and a re-evaluation of historical linguistic continuity in Hebrew.',
    'If other forms are recognized as vital, the extractiveness of this constraint would decrease, and its classification might shift towards a rope or even a mountain (if continuity is seen as natural). If it remains the sole criterion, the current high extractiveness and tangled_rope classification are reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_vitality, conceptual, 'Ambiguity in the definition of language vitality.').

omega_variable(
    desacralization_cost,
    'What is the full cultural and spiritual cost of desacralizing Hebrew by prioritizing secular vernacular use over its traditional liturgical function?',
    'Qualitative sociological and anthropological studies within Jewish communities, assessing the impact on religious practice, identity, and intergenerational transmission.',
    'A high cost would increase the perceived extractiveness and suppression, reinforcing the tangled_rope or even snare classification. A low cost would suggest the transition was less extractive than currently assessed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desacralization_cost, empirical, 'The unquantified cost of shifting Hebrew''s primary cultural function.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reading of the ''hebrew_vitality'' kernel, or an independent constraint that merely leverages the kernel''s authority?',
    'Analysis of the historical and ideological links between the Zionist project''s language policies and pre-existing notions of Hebrew''s status. If the link is weak, it''s an independent constraint.',
    'If an independent constraint, the cs_structure fields would be removed, and the constraint would be re-evaluated without the kernel context. If a genuine reading, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this constraint is truly a reading of the ''hebrew_vitality'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 1880, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_vitality__native_daily_reading, theater_ratio, 1880, 0.05).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_vitality__native_daily_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_vitality__native_daily_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(hebr_tr_t1950, hebrew_vitality__native_daily_reading, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_vitality__native_daily_reading, base_extractiveness, 1880, 0.4).
narrative_ontology:measurement(hebr_be_t1900, hebrew_vitality__native_daily_reading, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(hebr_be_t1920, hebrew_vitality__native_daily_reading, base_extractiveness, 1920, 0.6).
narrative_ontology:measurement(hebr_be_t1950, hebrew_vitality__native_daily_reading, base_extractiveness, 1950, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_vitality__native_daily_reading, suppression_requirement, 1880, 0.3).
narrative_ontology:measurement(hebr_su_t1900, hebrew_vitality__native_daily_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(hebr_su_t1920, hebrew_vitality__native_daily_reading, suppression_requirement, 1920, 0.65).
narrative_ontology:measurement(hebr_su_t1950, hebrew_vitality__native_daily_reading, suppression_requirement, 1950, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_vitality' kernel. This 'native_daily_reading' emphasizes vernacular use, while 'liturgical_reading' focuses on ritual continuity and 'hybrid_continuity_reading' attempts to reconcile both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
