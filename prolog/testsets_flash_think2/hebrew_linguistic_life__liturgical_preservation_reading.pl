% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__liturgical_preservation_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Hebrew Linguistic Life: Liturgical Preservation Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint represents the 'liturgical preservation' reading of the
 *   'hebrew_linguistic_life' kernel. It defines a language as alive through
 *   the continuous recitation, study, and transmission of its sacred texts,
 *   irrespective of its use in daily vernacular. From this perspective,
 *   Hebrew never 'died' and therefore did not require 'revival' by modern
 *   secular movements. The constraint describes a stable, long-standing
 *   theological and communal practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.05).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.05).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, mountain).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Hebrew Linguistic Life: Liturgical Preservation Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:emerges_naturally(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '56e2845c-e6e0-4da2-be05-4adee7a1cbef').
narrative_ontology:cs_kernel_codification('56e2845c-e6e0-4da2-be05-4adee7a1cbef', fixed_text).
narrative_ontology:cs_authority_grounding('56e2845c-e6e0-4da2-be05-4adee7a1cbef', lineage).
narrative_ontology:cs_interpretation_layer_present('56e2845c-e6e0-4da2-be05-4adee7a1cbef').
narrative_ontology:cs_reading_relation('56e2845c-e6e0-4da2-be05-4adee7a1cbef', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('56e2845c-e6e0-4da2-be05-4adee7a1cbef', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('56e2845c-e6e0-4da2-be05-4adee7a1cbef', foundational, hebrew_never_died).
narrative_ontology:cs_axiom_status(hebrew_never_died, holdable).
narrative_ontology:cs_axiom_grounding('56e2845c-e6e0-4da2-be05-4adee7a1cbef', hebrew_never_died, theological).
narrative_ontology:cs_axiom('56e2845c-e6e0-4da2-be05-4adee7a1cbef', foundational, sacred_use_is_life).
narrative_ontology:cs_axiom_status(sacred_use_is_life, holdable).
narrative_ontology:cs_axiom_grounding('56e2845c-e6e0-4da2-be05-4adee7a1cbef', sacred_use_is_life, theological).
narrative_ontology:cs_reference_frame('56e2845c-e6e0-4da2-be05-4adee7a1cbef', unbroken_sacred_chain).
narrative_ontology:cs_drift_state('56e2845c-e6e0-4da2-be05-4adee7a1cbef', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('56e2845c-e6e0-4da2-be05-4adee7a1cbef', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, religious_scholars).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The custodians and transmitters of the sacred texts and the liturgical tradition. Their professional and spiritual identity is deeply intertwined with the continuous study and recitation of Hebrew. They define and uphold the criteria for the language's aliveness within this framework.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, religious_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Communities that continuously recite, study, and transmit sacred texts in Hebrew. They derive communal identity, spiritual continuity, and cultural heritage from this practice. Adherence to this definition of linguistic life is central to their collective self-understanding.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_communities, beneficiary,
    organized, generational, identity_locked, global).

% Individuals who speak modern Hebrew for daily, secular purposes. Their experience of the language's vitality, based on vernacular use, is considered irrelevant or even a misunderstanding by this reading of linguistic life. They are outside the conversation that defines aliveness here.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_speakers, excluded,
    moderate, biographical, mobile, national).

% Historical and contemporary figures who championed the revival of Hebrew as a spoken, vernacular language. From the perspective of liturgical preservation, their project is seen as unnecessary (Hebrew never died) or even a desecration of its sacred status, rather than a resurrection.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, ben_yehuda_revivalists, excluded,
    powerful, generational, mobile, national).

% Scholars who study language vitality based on empirical criteria like native speakers, daily use, and generational transmission. They observe the different claims about Hebrew's aliveness but do not participate in the theological or communal definitions of this constraint.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, analytical_linguists, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__liturgical_preservation_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures the continuity of sacred tradition, religious practice, and communal identity across generations through the shared, unchanging medium of liturgical Hebrew.
% TRANSFER_FUNCTION: Transfers spiritual merit, cultural heritage, and a sense of unbroken historical connection from past generations to present and future ones, mediated by the sacred language.
% ABSENT_VOICES: Secular Hebrew speakers and Ben-Yehuda revivalists are excluded. They would argue that a language's true aliveness is demonstrated by its use in daily, mundane life and by its acquisition as a mother tongue, not solely by sacred recitation.
% DISAPPEARANCE_RATIONALE: If this understanding of Hebrew's aliveness vanished, the religious and cultural identity of many Jewish communities, for whom liturgical Hebrew is central, would be profoundly altered. The continuity of sacred texts and practices would be undermined, leading to a significant reorganization of religious and cultural life.
% FOUNDING_PROBLEM: The perceived threat of linguistic death or secularization to the sacred status of Hebrew and the religious tradition it embodies, particularly in diaspora contexts where vernaculars dominated.
% FOUNDING_PROBLEM_CORROBORATION: The continuous existence of religious texts, rabbinic commentaries, and the unbroken practice of liturgical communities themselves serve as internal corroboration. External linguistic analysis would likely contest this framing, but within the theological framework, it is self-attesting and continuously affirmed by adherents.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_linguistic_life__liturgical_preservation_reading),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) reflects that this constraint primarily describes an inherent state and a voluntary practice, not an active imposition or rent-seeking. The 'cost' is the discipline of study and transmission, which is internal to the practice. Suppression (0.05) is minimal, as participation is driven by religious commitment rather than coercion. Theater ratio (0.05) is low because the recitation and study are genuine acts of devotion and scholarship. Accessibility collapse (0.88) is high because, within this framework, alternative definitions of linguistic aliveness (e.g., based on vernacular use) are conceptually foreclosed. Resistance (0.02) is negligible within the adhering communities, as the definition is foundational to their identity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adherents, this constraint is a self-evident truth, a 'mountain' reflecting the inherent nature of Hebrew's sacred status. From an external, secular linguistic perspective, it might be seen as a 'conceptual' construct, but within its own framework, its truth is considered immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious scholars and liturgical communities are beneficiaries as they uphold and are sustained by this definition of Hebrew's aliveness. There are no 'victims' of this constraint's operation, as it describes a self-sustaining practice. Those who hold alternative definitions of linguistic life are 'excluded' from this framework, rather than being targeted by it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_natural_law,
    'Is this definition of Hebrew''s aliveness a genuine natural law within its theological framework, or a conceptual construct maintained by specific communities?',
    'Analysis of the internal consistency and universal applicability claimed by the theological framework versus its dependence on specific communal practices and interpretations.',
    'If primarily a conceptual construct, its ''mountain'' classification might be re-evaluated as a ''rope'' or ''tangled_rope'' for those within the community, reflecting a coordination function with inherent costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_natural_law, conceptual, 'Ambiguity between inherent truth and communal definition of linguistic aliveness.').

omega_variable(
    aliveness_definition_disagreement,
    'Is Hebrew''s aliveness fundamentally defined by liturgical continuity, or by vernacular use and generational transmission?',
    'This is a conceptual and preference-based disagreement between different readings of the ''hebrew_linguistic_life'' kernel, not resolvable by empirical data alone.',
    'The classification of Hebrew''s linguistic status (e.g., ''dead'' vs. ''never died'') depends entirely on which definition is adopted, leading to different classifications of the language''s historical trajectory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aliveness_definition_disagreement, conceptual, 'Core disagreement on the criteria for linguistic aliveness.').

omega_variable(
    ben_yehuda_project_status,
    'From this reading''s perspective, is Ben-Yehuda''s project a ''desecration'' or an ''unnecessary intervention'' rather than a ''resurrection''?',
    'Examination of theological texts and rabbinic rulings within the liturgical preservation tradition regarding the status of secularized Hebrew and the concept of linguistic ''revival''.',
    'If viewed as desecration, it reinforces the ''forecloses'' relationship with vernacular-based readings; if merely unnecessary, it suggests a less confrontational conceptual divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ben_yehuda_project_status, conceptual, 'Interpretation of modern Hebrew revival from a sacred-language perspective.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebr_tr_t25, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement(hebr_tr_t50, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(hebr_tr_t75, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 75, 0.05).
narrative_ontology:measurement(hebr_tr_t100, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(hebr_be_t25, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(hebr_be_t50, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(hebr_be_t75, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(hebr_be_t100, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(hebr_su_t25, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 25, 0.05).
narrative_ontology:measurement(hebr_su_t50, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 50, 0.05).
narrative_ontology:measurement(hebr_su_t75, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 75, 0.05).
narrative_ontology:measurement(hebr_su_t100, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
