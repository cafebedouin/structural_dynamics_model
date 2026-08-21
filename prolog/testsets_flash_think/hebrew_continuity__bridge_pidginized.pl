% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Hebrew as a Bridge Pidgin for Diaspora Interaction
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes Hebrew as it functions as a contact language or
 *   'bridge pidgin' for Jewish diaspora interaction, distinct from its
 *   liturgical use or its status as a native language in Israel. It is a
 *   reading of the 'hebrew_continuity' kernel, focusing on its instrumental
 *   utility for communication and cultural cohesion. This form of Hebrew is
 *   neither purely sacred nor fully generative, occupying a functional niche
 *   that is often dismissed by purists of other forms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.45).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.2).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.45).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew as a Bridge Pidgin for Diaspora Interaction").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:emerges_naturally(hebrew_continuity__bridge_pidginized).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, 'f686b5af-32ab-4689-bc79-6c8ce555334e').
narrative_ontology:cs_kernel_codification('f686b5af-32ab-4689-bc79-6c8ce555334e', distributed).
narrative_ontology:cs_authority_grounding('f686b5af-32ab-4689-bc79-6c8ce555334e', practice).
narrative_ontology:cs_interpretation_layer_present('f686b5af-32ab-4689-bc79-6c8ce555334e').
narrative_ontology:cs_reading_relation('f686b5af-32ab-4689-bc79-6c8ce555334e', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('f686b5af-32ab-4689-bc79-6c8ce555334e', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_axiom('f686b5af-32ab-4689-bc79-6c8ce555334e', foundational, hebrew_as_functional_bridge).
narrative_ontology:cs_axiom_status(hebrew_as_functional_bridge, holdable).
narrative_ontology:cs_axiom_grounding('f686b5af-32ab-4689-bc79-6c8ce555334e', hebrew_as_functional_bridge, conventional).
narrative_ontology:cs_axiom('f686b5af-32ab-4689-bc79-6c8ce555334e', secondary, linguistic_adaptation_for_survival).
narrative_ontology:cs_axiom_status(linguistic_adaptation_for_survival, holdable).
narrative_ontology:cs_axiom_grounding('f686b5af-32ab-4689-bc79-6c8ce555334e', linguistic_adaptation_for_survival, conventional).
narrative_ontology:cs_reference_frame('f686b5af-32ab-4689-bc79-6c8ce555334e', diaspora_interoperability_framework).
narrative_ontology:cs_drift_state('f686b5af-32ab-4689-bc79-6c8ce555334e', contemporary_diaspora_use, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f686b5af-32ab-4689-bc79-6c8ce555334e', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, diaspora_jews_using_pidgin).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, jewish_cultural_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, liturgical_hebrew_purists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals actively use a simplified, contact-language form of Hebrew for practical communication and cultural connection across diverse Jewish diaspora communities. They benefit from its utility as a bridge but bear the effort of learning and adapting to a non-native, often informal, linguistic system.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_jews_using_pidgin, beneficiary,
    moderate, biographical, constrained, global).

% Scholars and practitioners who uphold the sanctity and traditional forms of Hebrew, primarily for religious and textual purposes. They perceive pidginized Hebrew as a degradation of the sacred language, bearing a 'cultural cost' through its perceived impurity and deviation from established norms.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, liturgical_hebrew_purists, payer,
    organized, generational, identity_locked, global).

% Individuals for whom Modern Hebrew is a native, generative language. They often dismiss pidginized diaspora Hebrew as 'not real Hebrew' and typically do not participate in its use or development, remaining largely outside this specific linguistic dynamic.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, native_hebrew_speakers_in_israel, excluded,
    powerful, generational, mobile, national).

% Academics who study the emergence, structure, and function of pidginized Hebrew as a linguistic phenomenon. They analyze its role in identity formation and communication without direct participation or vested interest in its normative status.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, sociolinguists, observer,
    analytical, biographical, analytical, universal).

% Organizations dedicated to promoting Jewish continuity, education, and inter-community interaction. They often implicitly or explicitly support the use of Hebrew in various forms, including this bridge pidgin, as a means to achieve their cultural and communal goals.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, jewish_cultural_institutions, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To enable practical, low-barrier communication and cultural connection among diverse Jewish diaspora communities who do not share a common native language or a deep liturgical background, fostering a sense of shared identity and facilitating cultural exchange.
% TRANSFER_FUNCTION: Facilitates the transfer of cultural knowledge, social cohesion, and practical information among diaspora Jews, requiring the 'cost' of learning and adapting to a simplified, contact-language form of Hebrew.
% ABSENT_VOICES: Strict purists of both liturgical and native Hebrew forms, who would argue that this pidginized form dilutes the language's authenticity or sacredness, are often not part of the conversation about its practical utility or are actively dismissive of it.
% DISAPPEARANCE_RATIONALE: If this pidginized Hebrew vanished, diaspora communities would lose a unique and effective bridge for inter-group communication and cultural expression, forcing reliance on less specific lingua francas or deeper linguistic divides, leading to a significant reorganization of cultural interaction patterns and potentially weakening communal ties.
% FOUNDING_PROBLEM: The challenge of maintaining a shared linguistic and cultural connection among geographically dispersed Jewish communities with diverse native languages and varying levels of traditional Hebrew literacy, particularly in secular or less traditionally observant contexts.
% FOUNDING_PROBLEM_CORROBORATION: Sociolinguistic studies of language use in diaspora communities, ethnographic accounts of Jewish cultural exchange, and statements from community leaders outside of strict liturgical or native-speaker circles corroborate the ongoing need for such a bridge language. The continued emergence and adaptation of such forms of Hebrew in various communities attest to this live problem.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__bridge_pidginized, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).
:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.45) reflects the effort required to learn and use a simplified contact language, which, while functional, may lack the expressive range or prestige of native forms. Suppression (0.20) is low, as this form is not actively suppressed but also lacks formal institutional support or promotion as the 'ideal' Hebrew. Theater ratio (0.05) is negligible, as its existence is purely functional, not performative. Accessibility collapse (0.60) is moderate, as while other languages exist for general communication, this specific form of Hebrew offers a unique cultural bridge. Resistance (0.40) comes from those who view it as a degradation of the language. The claimed type is 'rope' because its primary function is genuine coordination for diaspora communities.
 *
 * PERSPECTIVAL GAP:
 *   Users of pidginized Hebrew experience it as a practical, enabling tool for connection, while purists of other Hebrew forms often perceive it as a linguistic compromise or even a degradation. The engine's classification will highlight its coordination function, while the resistance metric captures the friction from these differing perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jews who use this pidginized Hebrew are beneficiaries, gaining a unique communication channel and cultural connection. Jewish cultural institutions also benefit by achieving their goals of continuity. Liturgical purists and native speakers in Israel, while not directly 'victims' in an extractive sense, bear a 'cultural cost' or 'discomfort' from its existence, as it challenges their normative views of Hebrew. Sociolinguists are observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pidgin_legitimacy_ambiguity,
    'Is pidginized Hebrew a legitimate, evolving form of the language, or a degraded version that undermines its authenticity?',
    'Longitudinal sociolinguistic studies tracking its stability, grammaticalization, and acceptance by a critical mass of users over generations, or formal recognition by a widely accepted linguistic authority (if one were to emerge for diaspora Hebrew).',
    'If deemed legitimate, its coordination function is strengthened, and resistance from purists is reclassified as external friction rather than internal degradation. If deemed degraded, its long-term viability and cultural value are questioned, increasing its effective extraction for users who might feel they are using an ''inferior'' form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pidgin_legitimacy_ambiguity, conceptual, 'Ambiguity regarding the linguistic and cultural legitimacy of pidginized Hebrew.').

omega_variable(
    effort_vs_extraction_cost,
    'To what extent does the ''extraction'' associated with pidginized Hebrew represent inherent effort required for any contact language, versus costs imposed by its contested status and lack of formal support?',
    'Comparative studies with other successful pidgins/creoles that achieved formal recognition and support, assessing the differential in user effort and perceived value. Also, surveys of users'' self-reported effort and satisfaction.',
    'If costs are primarily inherent effort, the constraint leans more towards a pure Rope. If costs are significantly due to contested status and lack of support, it reinforces the Tangled Rope aspects, highlighting the burden on users to justify its existence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effort_vs_extraction_cost, empirical, 'Distinguishing inherent effort from imposed extraction in a contact language.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1950, hebrew_continuity__bridge_pidginized, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(hebr_tr_t1965, hebrew_continuity__bridge_pidginized, theater_ratio, 1965, 0.05).
narrative_ontology:measurement(hebr_tr_t1980, hebrew_continuity__bridge_pidginized, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(hebr_tr_t1995, hebrew_continuity__bridge_pidginized, theater_ratio, 1995, 0.05).
narrative_ontology:measurement(hebr_tr_t2010, hebrew_continuity__bridge_pidginized, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(hebr_tr_t2024, hebrew_continuity__bridge_pidginized, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1950, hebrew_continuity__bridge_pidginized, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(hebr_be_t1965, hebrew_continuity__bridge_pidginized, base_extractiveness, 1965, 0.42).
narrative_ontology:measurement(hebr_be_t1980, hebrew_continuity__bridge_pidginized, base_extractiveness, 1980, 0.43).
narrative_ontology:measurement(hebr_be_t1995, hebrew_continuity__bridge_pidginized, base_extractiveness, 1995, 0.44).
narrative_ontology:measurement(hebr_be_t2010, hebrew_continuity__bridge_pidginized, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(hebr_be_t2024, hebrew_continuity__bridge_pidginized, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1950, hebrew_continuity__bridge_pidginized, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(hebr_su_t1965, hebrew_continuity__bridge_pidginized, suppression_requirement, 1965, 0.23).
narrative_ontology:measurement(hebr_su_t1980, hebrew_continuity__bridge_pidginized, suppression_requirement, 1980, 0.22).
narrative_ontology:measurement(hebr_su_t1995, hebrew_continuity__bridge_pidginized, suppression_requirement, 1995, 0.21).
narrative_ontology:measurement(hebr_su_t2010, hebrew_continuity__bridge_pidginized, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(hebr_su_t2024, hebrew_continuity__bridge_pidginized, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
