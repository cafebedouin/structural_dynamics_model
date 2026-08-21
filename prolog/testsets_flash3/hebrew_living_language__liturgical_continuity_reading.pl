% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__liturgical_continuity_reading, []).

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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Hebrew as a Living Language: Liturgical Continuity Reading
 *   domain: historical_linguistics/commitment_systems
 *
 * SUMMARY:
 *   This constraint models the claim that Hebrew remains a living language
 *   due to its unbroken use in liturgy and textual study across the Jewish
 *   diaspora. This reading emphasizes continuity of symbolic and ritual
 *   function over daily spoken generativity. It is one reading of the
 *   'hebrew_living_language' kernel, which is contested by other readings
 *   focusing on literary production or native speech.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.05).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.1).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Hebrew as a Living Language: Liturgical Continuity Reading").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, '06559346-f5d3-4412-8db2-342643ff85df').
narrative_ontology:cs_kernel_codification('06559346-f5d3-4412-8db2-342643ff85df', fixed_text).
narrative_ontology:cs_authority_grounding('06559346-f5d3-4412-8db2-342643ff85df', lineage).
narrative_ontology:cs_interpretation_layer_present('06559346-f5d3-4412-8db2-342643ff85df').
narrative_ontology:cs_reading_relation('06559346-f5d3-4412-8db2-342643ff85df', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('06559346-f5d3-4412-8db2-342643ff85df', hebrew_living_language__literary_revival_reading, coexists_with).
narrative_ontology:cs_axiom('06559346-f5d3-4412-8db2-342643ff85df', foundational, unbroken_textual_transmission_confers_living_status).
narrative_ontology:cs_axiom_status(unbroken_textual_transmission_confers_living_status, holdable).
narrative_ontology:cs_axiom_grounding('06559346-f5d3-4412-8db2-342643ff85df', unbroken_textual_transmission_confers_living_status, conventional).
narrative_ontology:cs_reference_frame('06559346-f5d3-4412-8db2-342643ff85df', diaspora_liturgical_tradition).
narrative_ontology:cs_drift_state('06559346-f5d3-4412-8db2-342643ff85df', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('06559346-f5d3-4412-8db2-342643ff85df', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, jewish_communities_worldwide).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, hebrew_continuity_doctrine).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, diaspora_cultural_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains a continuous connection to ancient texts and religious practice through liturgical Hebrew. Participation is voluntary, and the benefits are primarily cultural and spiritual, reinforcing group identity and historical lineage.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, jewish_communities_worldwide, beneficiary,
    organized, generational, mobile, global).

% Serve as custodians and interpreters of Hebrew texts, ensuring the accuracy and transmission of liturgical and scholarly traditions. They set the standards for recitation and study, guiding the community's engagement with the language.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, rabbis_and_scholars, agenda_setter,
    institutional, generational, constrained, global).

% Primarily use modern Hebrew for daily communication and may not engage with liturgical forms. From their perspective, the 'living' aspect of Hebrew is tied to its generative use, not its ritual preservation, but they do not actively oppose liturgical continuity.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, secular_hebrew_speakers, excluded,
    moderate, biographical, mobile, national).

% Analyze the historical trajectory of Hebrew, documenting its continuous use in religious contexts and its later revitalization. They assess the claims of 'living language' based on empirical evidence of usage patterns over millennia.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continuous use and understanding of Hebrew across diverse Jewish communities globally, ensuring shared access to religious texts and a common liturgical practice over millennia.
% TRANSFER_FUNCTION: Transfers cultural and religious continuity, historical identity, and access to sacred texts across generations and geographies, from past to present communities.
% ABSENT_VOICES: Those who define 'living language' exclusively by native, generative daily speech (e.g., proponents of the native_generation_reading) would argue that liturgical use alone is insufficient, but they are not structurally excluded from the conversation about Hebrew's status, merely holding a different definition.
% DISAPPEARANCE_RATIONALE: If liturgical Hebrew vanished overnight, Jewish communities would lose a core element of their religious practice, historical identity, and intergenerational continuity. Access to sacred texts would be mediated solely through translation, fundamentally altering the cultural landscape.
% FOUNDING_PROBLEM: The challenge of maintaining a distinct cultural and religious identity, and access to sacred texts, for a dispersed people without a continuous territorial base or a universally spoken language.
% FOUNDING_PROBLEM_CORROBORATION: Jewish communities worldwide, as well as historical and sociological scholars, corroborate that the problem of cultural and religious continuity in diaspora remains live, and liturgical Hebrew is a primary mechanism for addressing it.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__liturgical_continuity_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__liturgical_continuity_reading_tests).
:- end_tests(hebrew_living_language__liturgical_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.05) because participation is voluntary and the 'cost' is primarily the effort of learning and maintaining the language for religious purposes, which is seen as a benefit by participants. Suppression is low (0.1) as there is no active coercion to maintain this practice; its persistence is driven by cultural and religious commitment. Theater ratio is low (0.05) as the liturgical function is genuine and central to the communities' identity. The metrics are stable over the long interval, reflecting the unbroken nature of this continuity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, Hebrew's 'living' status is self-evident through its continuous ritual use. Other readings, particularly the native_generation_reading, would dispute this, arguing that only daily spoken generativity constitutes a 'living' language. This divergence is captured by the omegas and the cs_structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish communities worldwide are the primary beneficiaries, gaining cultural and religious continuity. Rabbis and scholars act as agenda-setters, guiding the tradition. There are no identifiable victims, as participation is voluntary and perceived as beneficial. Secular Hebrew speakers are 'excluded' in the sense that their definition of 'living' differs, but they are not harmed by this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_living_language,
    'Is ''living language'' defined by continuous liturgical and textual use, or by native, generative daily speech?',
    'Conceptual clarification and consensus within the field of historical linguistics, or a shift in cultural emphasis within the Jewish community.',
    'If the definition shifts to require native speech, this reading''s claim of Hebrew being ''living'' would be reclassified as ''preserved'' or ''ritualistic'' rather than ''living'' in the full sense, potentially influencing its perceived vitality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_living_language, conceptual, 'Ambiguity in the definition of a ''living language'' in the context of Hebrew.').

omega_variable(
    impact_of_modern_hebrew_revival,
    'To what extent does the modern revival of spoken Hebrew (native_generation_reading) diminish or reinforce the ''living'' status derived from liturgical continuity?',
    'Sociolinguistic studies on language attitudes and usage patterns within Jewish communities, comparing engagement with liturgical vs. modern Hebrew.',
    'If modern Hebrew''s success is seen as rendering liturgical use ''archaic'' rather than ''continuous,'' the perceived vitality of this reading might decrease. Conversely, if it reinforces a broader sense of Hebrew''s living status, it could strengthen this reading''s claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_modern_hebrew_revival, empirical, 'Interaction between liturgical continuity and modern Hebrew revival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebr_tr_t500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 500, 0.05).
narrative_ontology:measurement(hebr_tr_t1000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(hebr_tr_t1500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(hebr_be_t500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(hebr_be_t1000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(hebr_be_t1500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(hebr_be_t2000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 2000, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(hebr_su_t500, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 500, 0.1).
narrative_ontology:measurement(hebr_su_t1000, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(hebr_su_t1500, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1500, 0.1).
narrative_ontology:measurement(hebr_su_t2000, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 2000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_living_language' kernel. This reading emphasizes liturgical and textual continuity, while others focus on native speech or literary production. All three are linked as they represent different facets of the same overarching claim about Hebrew's vitality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
