% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__marketplace_pidgin_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_linguistic_life__marketplace_pidgin_reading
 *   human_readable: Hebrew Linguistic Life: Marketplace Pidgin Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint represents the 'marketplace pidgin' reading of Hebrew
 *   linguistic life, asserting that Hebrew was continuously alive as an
 *   inter-communal medium for practical coordination in Jerusalem markets
 *   prior to 1880. This reading emphasizes continuous adaptation and
 *   functional use, rather than pure preservation or a 'revival' from
 *   dormancy. It challenges narratives that focus solely on liturgical use or
 *   native generational transmission as markers of linguistic vitality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.2).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.1).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Hebrew Linguistic Life: Marketplace Pidgin Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, 'a25ab531-31b1-4d0a-b43e-a097c65e5e82').
narrative_ontology:cs_kernel_codification('a25ab531-31b1-4d0a-b43e-a097c65e5e82', distributed).
narrative_ontology:cs_authority_grounding('a25ab531-31b1-4d0a-b43e-a097c65e5e82', diffuse_epistemic).
narrative_ontology:cs_reading_relation('a25ab531-31b1-4d0a-b43e-a097c65e5e82', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('a25ab531-31b1-4d0a-b43e-a097c65e5e82', hebrew_linguistic_life__native_generational_reading, influences).
narrative_ontology:cs_axiom('a25ab531-31b1-4d0a-b43e-a097c65e5e82', foundational, linguistic_life_is_functional_use).
narrative_ontology:cs_axiom_status(linguistic_life_is_functional_use, holdable).
narrative_ontology:cs_axiom_grounding('a25ab531-31b1-4d0a-b43e-a097c65e5e82', linguistic_life_is_functional_use, conventional).
narrative_ontology:cs_axiom('a25ab531-31b1-4d0a-b43e-a097c65e5e82', secondary, pidgin_forms_count_as_continuity).
narrative_ontology:cs_axiom_status(pidgin_forms_count_as_continuity, holdable).
narrative_ontology:cs_axiom_grounding('a25ab531-31b1-4d0a-b43e-a097c65e5e82', pidgin_forms_count_as_continuity, empirically_contingent).
narrative_ontology:cs_reference_frame('a25ab531-31b1-4d0a-b43e-a097c65e5e82', continuous_functional_use).
narrative_ontology:cs_drift_state('a25ab531-31b1-4d0a-b43e-a097c65e5e82', contemporary_nationalist_narratives, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a25ab531-31b1-4d0a-b43e-a097c65e5e82', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_merchants).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, diverse_traders).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, linguistic_historians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Used a modified Medieval Hebrew pidgin for daily transactions, enabling trade across diverse linguistic communities in Jerusalem. Benefited from the practical coordination this lingua franca provided.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_merchants, beneficiary,
    moderate, biographical, mobile, local).

% Traders from various backgrounds who adopted the Hebrew pidgin as a common medium for commerce, facilitating their economic activities and inter-communal exchange.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, diverse_traders, beneficiary,
    moderate, biographical, mobile, local).

% Analyze historical linguistic data to reconstruct the continuous functional use of Hebrew in practical contexts, challenging narratives of complete dormancy or pure revival.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, linguistic_historians, observer,
    analytical, generational, analytical, global).

% Advocate for a narrative of Hebrew's 'revival' from dormancy, often downplaying or ignoring evidence of continuous functional use in non-native, non-liturgical contexts. Their ideological framework struggles to accommodate this reading.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, nationalist_revivalists, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a common linguistic medium for practical coordination and commerce among diverse linguistic communities in Jerusalem, enabling trade and inter-communal interaction.
% TRANSFER_FUNCTION: Facilitated the transfer of goods, services, and information between speakers of different vernaculars, by providing a shared communication channel.
% ABSENT_VOICES: Nationalist revivalists and proponents of the 'native generational' reading would object, as this reading challenges their narrative of Hebrew's 'dormancy' and subsequent 'revival' by native speakers. They are excluded from the historical linguistic evidence that supports this reading.
% DISAPPEARANCE_RATIONALE: If this functional use of Hebrew had not existed, the inter-communal trade and practical coordination in Jerusalem markets would have been significantly more difficult, requiring reliance on multiple interpreters or a different, less established lingua franca. The historical linguistic landscape would be fundamentally different.
% FOUNDING_PROBLEM: The need for a common language for practical coordination and commerce among diverse linguistic groups in Jerusalem, where no single vernacular was dominant.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, travelogues, and linguistic analyses from independent scholars and observers (e.g., foreign visitors, non-Hebrew speaking residents) corroborate the continuous functional use of Hebrew in these contexts, distinct from purely liturgical or native-speaker use.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).
:- end_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it genuinely solves a coordination problem (inter-communal communication for trade) with minimal extraction. The 'extraction' (0.2) is merely the transaction cost of learning and using a pidgin. Suppression (0.1) is low, as its adoption was voluntary and driven by practical utility, not coercion. Theater ratio (0.05) is negligible, as its function was purely practical. Accessibility collapse (0.8) is high because, once adopted, it became the de facto standard for this specific coordination, making alternatives less accessible for those seeking to participate in the market. Resistance (0.05) is low, reflecting its utility.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the merchants and traders, the Hebrew pidgin was a practical tool, a 'rope' for coordination. From the perspective of nationalist revivalists, this continuous, adapted use might be seen as a 'degradation' or 'non-life' compared to a pure, native form, leading to a different classification if their ideological lens were adopted.
 *
 * DIRECTIONALITY LOGIC:
 *   Jerusalem merchants and diverse traders are beneficiaries, as the pidgin facilitated their commerce. Linguistic historians are observers, analyzing its historical function. Nationalist revivalists are 'excluded' in this context, as their ideological framework often resists acknowledging this form of continuous linguistic life.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_evidence_completeness,
    'How complete and unambiguous is the historical evidence for the continuous functional use of Hebrew as a marketplace pidgin?',
    'Discovery of new primary sources (e.g., merchant ledgers, personal correspondence) or re-analysis of existing linguistic corpora with a focus on non-liturgical, inter-communal usage.',
    'Stronger evidence would solidify this reading''s claim of continuous life, further challenging ''dormancy'' narratives. Weaker evidence would increase the conceptual ambiguity between this and the ''liturgical preservation'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evidence_completeness, empirical, 'Ambiguity regarding the extent and nature of continuous functional Hebrew use.').

omega_variable(
    linguistic_purity_vs_function,
    'Is the ''pidgin'' form of Hebrew sufficiently ''Hebrew'' to count as continuous linguistic life, or does its modified nature render it a distinct language?',
    'Consensus among historical linguists on the criteria for language identity and continuity across pidginization processes, or a conceptual shift in how ''language life'' is defined.',
    'If deemed ''not sufficiently Hebrew,'' this reading would be foreclosed by the ''native generational'' reading, which demands a purer form. If accepted, it reinforces a functional definition of linguistic life.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(linguistic_purity_vs_function, conceptual, 'Conceptual boundary of what constitutes ''Hebrew'' for purposes of linguistic continuity.').

omega_variable(
    ideological_resistance_to_reading,
    'To what extent is resistance to this reading driven by ideological commitments to a ''revival'' narrative rather than empirical linguistic evidence?',
    'Sociological and historical analysis of the motivations and arguments of nationalist revivalist movements, and their engagement (or lack thereof) with linguistic evidence for continuous functional use.',
    'If resistance is primarily ideological, it highlights the ''excluded'' status of nationalist revivalists and the political nature of linguistic history. If empirical, it points to genuine scholarly disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_resistance_to_reading, preference, 'The role of ideology in accepting or rejecting this reading of Hebrew''s linguistic life.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 1500, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1500, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(hebr_tr_t1600, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1600, 0.05).
narrative_ontology:measurement(hebr_tr_t1700, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(hebr_tr_t1800, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1880, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1500, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement(hebr_be_t1600, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1600, 0.18).
narrative_ontology:measurement(hebr_be_t1700, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1700, 0.2).
narrative_ontology:measurement(hebr_be_t1800, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1800, 0.2).
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1880, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1500, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1500, 0.08).
narrative_ontology:measurement(hebr_su_t1600, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1600, 0.09).
narrative_ontology:measurement(hebr_su_t1700, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1700, 0.1).
narrative_ontology:measurement(hebr_su_t1800, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1880, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_linguistic_life' kernel, focusing on continuous functional use in markets. It contrasts with the 'liturgical_preservation_reading' and 'native_generational_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
