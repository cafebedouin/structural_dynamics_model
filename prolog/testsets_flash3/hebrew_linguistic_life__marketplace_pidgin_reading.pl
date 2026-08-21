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
 *   constraint_id: hebrew_linguistic_life__marketplace_pidgin_reading
 *   human_readable: Hebrew Linguistic Life: Marketplace Pidgin Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint represents the 'marketplace pidgin' reading of Hebrew
 *   linguistic life, asserting that Hebrew was continuously alive as an
 *   inter-communal medium for practical coordination in Jerusalem markets
 *   prior to the modern revival movement (pre-1880). This reading emphasizes
 *   functional use over native speaker status or sacred function, challenging
 *   narratives of a 'dead' language. It is a Rope because it solved a genuine
 *   coordination problem with low extraction and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.15).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.05).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Hebrew Linguistic Life: Marketplace Pidgin Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, 'ff624007-91f7-410e-b080-fd84d23ea154').
narrative_ontology:cs_kernel_codification('ff624007-91f7-410e-b080-fd84d23ea154', distributed).
narrative_ontology:cs_authority_grounding('ff624007-91f7-410e-b080-fd84d23ea154', practice).
narrative_ontology:cs_reading_relation('ff624007-91f7-410e-b080-fd84d23ea154', hebrew_linguistic_life__liturgical_preservation_reading, influences).
narrative_ontology:cs_reading_relation('ff624007-91f7-410e-b080-fd84d23ea154', hebrew_linguistic_life__native_generational_reading, influences).
narrative_ontology:cs_axiom('ff624007-91f7-410e-b080-fd84d23ea154', foundational, functional_use_equals_life).
narrative_ontology:cs_axiom_status(functional_use_equals_life, holdable).
narrative_ontology:cs_axiom_grounding('ff624007-91f7-410e-b080-fd84d23ea154', functional_use_equals_life, conventional).
narrative_ontology:cs_reference_frame('ff624007-91f7-410e-b080-fd84d23ea154', continuous_functional_adaptation).
narrative_ontology:cs_drift_state('ff624007-91f7-410e-b080-fd84d23ea154', modern_revival_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ff624007-91f7-410e-b080-fd84d23ea154', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_merchants).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, diverse_ethnic_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Used a modified Medieval Hebrew pidgin as a lingua franca for trade, enabling transactions across diverse linguistic communities in Jerusalem markets. Benefited from efficient communication and reduced transaction costs.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_merchants, beneficiary,
    moderate, biographical, mobile, local).

% Members of various ethnic and religious groups in Jerusalem adopted the Hebrew pidgin for inter-communal communication, facilitating daily life and commerce. Benefited from a shared medium without needing to learn multiple languages.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, diverse_ethnic_groups, beneficiary,
    powerless, biographical, constrained, local).

% Analyze historical texts and records to reconstruct the actual usage of Hebrew in pre-modern Jerusalem, seeking evidence of its functional vitality beyond liturgical contexts. Their analysis supports or refutes claims of continuous linguistic life.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, linguistic_historians, observer,
    analytical, generational, analytical, global).

% Advocated for the revival of Hebrew as a modern native language, often downplaying or ignoring evidence of its continuous, albeit pidginized, functional use. Their narrative emphasizes a 'dead' language brought back to life, which this reading challenges.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_revivalists, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a common linguistic medium for practical coordination and commerce among diverse linguistic communities in Jerusalem, enabling trade and daily interactions.
% TRANSFER_FUNCTION: Facilitated the exchange of goods, services, and information between speakers of different native languages, reducing communication barriers.
% ABSENT_VOICES: Many Hebrew revivalists, who framed the language as 'dead' before its modern revival, would object to this reading as it complicates their narrative of a miraculous rebirth. They are excluded from this reading's focus on continuous functional use.
% DISAPPEARANCE_RATIONALE: If this functional pidgin had not existed, inter-communal trade and daily coordination in Jerusalem would have been significantly more difficult, requiring more complex multilingualism or reliance on a different, less neutral lingua franca. The social and economic fabric would have been different.
% FOUNDING_PROBLEM: The need for a neutral, inter-communal language for trade and daily interaction in a linguistically diverse Jerusalem, where no single vernacular was dominant among all groups.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, travelogues, and linguistic analyses by independent scholars (e.g., linguistic historians) corroborate the existence and functional use of this pidgin, supporting the claim that it addressed a genuine and ongoing coordination problem.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.15) because the pidgin facilitated trade with minimal overhead or coercive enforcement; it was adopted for its utility. Suppression is very low (0.05) as its use was voluntary and driven by practical need, not coercion. Theater ratio is negligible (0.02) as its function was purely practical, not performative. Accessibility collapse is high (0.85) because once understood, it became the default for inter-communal trade, making alternatives less attractive. Resistance is low (0.1) because it was a useful tool, not a burden.
 *
 * PERSPECTIVAL GAP:
 *   This reading directly challenges the 'native generational' and 'liturgical preservation' readings by offering an alternative criterion for linguistic vitality. The marketplace pidgin reading highlights a continuous, adaptive form of Hebrew that doesn't fit neatly into either the 'sacred text' or 'mother tongue' categories, creating a perspectival gap for those who adhere to the sibling readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Jerusalem merchants and diverse ethnic groups are beneficiaries, gaining efficient communication. Linguistic historians are observers, analyzing its historical function. Hebrew revivalists are excluded, as their narrative of a 'dead' language is challenged by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_evidence_completeness,
    'Is the historical evidence for widespread functional use of Hebrew pidgin in Jerusalem markets pre-1880 sufficiently robust and unambiguous?',
    'Discovery of new primary sources (e.g., merchant ledgers, personal letters, travelogues) that explicitly document inter-communal Hebrew pidgin usage, or further linguistic analysis of existing texts.',
    'Stronger evidence would solidify this reading''s claim of continuous linguistic life, influencing the ''native_generational_reading'' and ''liturgical_preservation_reading'' by demonstrating an alternative form of vitality. Weaker evidence would reduce its influence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evidence_completeness, empirical, 'Uncertainty regarding the extent and nature of historical evidence for Hebrew pidgin use.').

omega_variable(
    definition_of_linguistic_life,
    'Is ''functional inter-communal medium'' a valid and sufficient criterion for defining a language as ''alive'', or must it meet stricter criteria (e.g., native speakers, full grammatical complexity)?',
    'Conceptual debate and consensus within sociolinguistics and historical linguistics on the minimal criteria for linguistic vitality, potentially leading to a revised theoretical framework.',
    'If a broader definition of ''alive'' is accepted, this reading gains stronger conceptual grounding. If stricter criteria are universally adopted, this reading''s claim of continuous life might be reclassified as ''dormant'' or ''limited vitality''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_linguistic_life, conceptual, 'Ambiguity in the conceptual definition of ''linguistic life'' itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 1600, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1600, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1600, 0.02).
narrative_ontology:measurement(hebr_tr_t1670, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1670, 0.02).
narrative_ontology:measurement(hebr_tr_t1740, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1740, 0.02).
narrative_ontology:measurement(hebr_tr_t1810, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1810, 0.02).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1880, 0.02).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1600, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement(hebr_be_t1670, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1670, 0.14).
narrative_ontology:measurement(hebr_be_t1740, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1740, 0.15).
narrative_ontology:measurement(hebr_be_t1810, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1810, 0.16).
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1880, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1600, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1600, 0.05).
narrative_ontology:measurement(hebr_su_t1670, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1670, 0.05).
narrative_ontology:measurement(hebr_su_t1740, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1740, 0.05).
narrative_ontology:measurement(hebr_su_t1810, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1810, 0.05).
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1880, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, information_standard).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hebrew_linguistic_life' kernel. It focuses on the functional use of Hebrew as a marketplace pidgin, distinct from liturgical preservation or native generational transmission. All three readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
