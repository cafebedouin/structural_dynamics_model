% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto: Endogenous Prophetic Reinterpretation of Plural Marriage
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story analyzes the 1890 Manifesto as a legitimate
 *   prophetic reinterpretation of the divine mandate for plural marriage,
 *   where God revealed a temporal suspension of the practice to preserve the
 *   church's salvific mission. This reading emphasizes the internal
 *   theological consistency and the role of continuing revelation in adapting
 *   doctrine to new circumstances, while acknowledging the external pressures
 *   that precipitated the reinterpretation. It is one reading of the
 *   'plural_marriage_mandate' kernel, distinct from
 *   'exogenous_override_reading' (federal coercion) and
 *   'institutional_pragmatism_reading' (strategic adaptation).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.25).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.6).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "1890 Manifesto: Endogenous Prophetic Reinterpretation of Plural Marriage").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, 'ff152bb4-ac3b-4f4c-812e-9fc71d68cf7f').
narrative_ontology:cs_kernel_codification('ff152bb4-ac3b-4f4c-812e-9fc71d68cf7f', fixed_text).
narrative_ontology:cs_authority_grounding('ff152bb4-ac3b-4f4c-812e-9fc71d68cf7f', lineage).
narrative_ontology:cs_interpretation_layer_present('ff152bb4-ac3b-4f4c-812e-9fc71d68cf7f').
narrative_ontology:cs_reading_relation('ff152bb4-ac3b-4f4c-812e-9fc71d68cf7f', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff152bb4-ac3b-4f4c-812e-9fc71d68cf7f', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('ff152bb4-ac3b-4f4c-812e-9fc71d68cf7f', foundational, continuing_revelation_adapts_doctrine).
narrative_ontology:cs_axiom_status(continuing_revelation_adapts_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('ff152bb4-ac3b-4f4c-812e-9fc71d68cf7f', continuing_revelation_adapts_doctrine, theological).
narrative_ontology:cs_axiom('ff152bb4-ac3b-4f4c-812e-9fc71d68cf7f', secondary, church_survival_is_salvific_priority).
narrative_ontology:cs_axiom_status(church_survival_is_salvific_priority, holdable).
narrative_ontology:cs_axiom_grounding('ff152bb4-ac3b-4f4c-812e-9fc71d68cf7f', church_survival_is_salvific_priority, theological).
narrative_ontology:cs_reference_frame('ff152bb4-ac3b-4f4c-812e-9fc71d68cf7f', prophetic_revelation_as_adaptive_mechanism).
narrative_ontology:cs_drift_state('ff152bb4-ac3b-4f4c-812e-9fc71d68cf7f', post_manifesto_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ff152bb4-ac3b-4f4c-812e-9fc71d68cf7f', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, the_church_institution).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_members).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_members).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, prophetic_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the primary beneficiary, the institution gained legal and social legitimacy, allowing it to continue its salvific mission (temple work, missionary efforts) without federal interference. It actively enforces the new interpretation through ecclesiastical courts and excommunication.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, the_church_institution, agenda_setter,
    institutional, generational, constrained, global).

% These members accepted the reinterpretation, allowing them to remain in good standing with the church and participate fully in its ordinances, including temple marriage, which would have been jeopardized by continued federal opposition. They benefit from the church's continued social acceptance.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_members, beneficiary,
    organized, biographical, mobile, global).

% These members viewed plural marriage as a divine commandment that could not be suspended by human (or even prophetic) decree. They faced excommunication and social ostracization for adhering to the original practice, leading to the formation of splinter groups. Their identity was deeply tied to the original doctrine.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_members, payer,
    powerless, generational, identity_locked, local).

% The federal government, through anti-polygamy legislation and enforcement, created the external pressure that led to the Manifesto. From its perspective, the Manifesto represented compliance with secular law, not necessarily a divine reinterpretation. It ceased active persecution after the Manifesto.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, federal_government, observer,
    institutional, generational, analytical, national).

% Scholars who analyze the historical and theological context of the Manifesto, evaluating the claims of prophetic reinterpretation against historical evidence, internal theological consistency, and external pressures. They are not directly affected by the constraint but analyze its structural properties.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, historical_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the church's membership around a unified understanding of divine will regarding plural marriage, allowing the institution to navigate external legal pressures while maintaining its theological framework of continuing revelation.
% TRANSFER_FUNCTION: Transfers the burden of adapting to external legal pressure from the church institution to individual members, particularly those who felt bound by the original practice. It also transfers social legitimacy from the federal government to the church.
% ABSENT_VOICES: The voices of those who felt the original revelation was immutable and could not be suspended were effectively silenced or marginalized within the mainstream church. Their perspective, that the Manifesto was a capitulation rather than a reinterpretation, was excluded from the dominant narrative.
% DISAPPEARANCE_RATIONALE: If the 1890 Manifesto's reinterpretation vanished, the church's current structure and social standing would collapse. Plural marriage would either be reinstated (leading to renewed federal conflict) or the church would face an existential crisis of prophetic authority, fundamentally altering its identity and mission.
% FOUNDING_PROBLEM: The church faced existential threats from the U.S. federal government due to its practice of plural marriage, including disincorporation, confiscation of assets, and imprisonment of leaders, jeopardizing its ability to perform essential religious ordinances and continue its growth.
% FOUNDING_PROBLEM_CORROBORATION: The church institution attests the problem of existential threat was live and the Manifesto resolved it, allowing the church to survive. Historical records, federal legislation, and court cases from the period corroborate the severity of the external pressure. While the specific threat of federal disincorporation is no longer active, the principle of adapting to preserve the church's mission remains a live concern for the institution.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).
:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) is moderate, reflecting the cost borne by fundamentalist members who were excommunicated for not accepting the reinterpretation, but also the overall benefit to the church institution. Suppression (0.6) is significant, as the church actively enforced the new interpretation through ecclesiastical discipline. Theater ratio (0.1) is low, as the reinterpretation is presented as a genuine prophetic act, not merely a performance. The constraint is classified as a Rope because it coordinates the church's response to an existential threat, with the church institution and mainstream members as beneficiaries, despite the costs to fundamentalists.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the church institution and mainstream members, the Manifesto was a necessary and divinely guided act of adaptation, preserving the church's mission. From the perspective of fundamentalist members, it was a betrayal of divine commandment, leading to their excommunication and marginalization. The federal government viewed it as a victory for secular law. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The church institution is a primary beneficiary (d=0.0-0.1) as it secured its survival and continued mission. Mainstream members are also beneficiaries (d=0.1-0.2) as they maintained their standing within the church. Fundamentalist members are victims (d=0.8-0.9) due to excommunication and loss of community for adhering to the original practice. The federal government and historical theologians are observers (d=0.5) with analytical distance.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as pure extraction by emphasizing the genuine coordination function of preserving the church's salvific mission. While there is extraction from fundamentalist members, the primary purpose, from this reading's perspective, was to coordinate the church's survival. The 'founding_problem_status' being 'live' (in principle) further supports the idea that the mandate's function, though reinterpreted, remains relevant to the institution's ongoing existence, preventing a full mandatrophy classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_vs_pragmatic_motivation,
    'Was the 1890 Manifesto primarily motivated by genuine prophetic revelation, or by institutional pragmatism in response to overwhelming federal pressure?',
    'Analysis of internal church records, private correspondence of leaders, and comparative studies of other religious groups facing similar external pressures. The ''exogenous_override_reading'' and ''institutional_pragmatism_reading'' offer alternative resolutions.',
    'If primarily pragmatic, the constraint''s ''theater_ratio'' would be higher, and its ''extractiveness'' from fundamentalists would be less justified by a divine narrative, potentially shifting its classification towards a Tangled Rope or Snare from the perspective of those who believed in the original mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prophetic_vs_pragmatic_motivation, conceptual, 'Ambiguity of the Manifesto''s primary motivation.').

omega_variable(
    doctrine_vs_practice_suspension,
    'Did the Manifesto suspend the doctrine of plural marriage, or only its practice?',
    'Further prophetic pronouncements, official doctrinal statements, or historical analysis of subsequent church teachings on the theological status of plural marriage. This reading asserts only practice was suspended.',
    'If the doctrine itself was suspended, the ''accessibility_collapse'' for fundamentalists would be higher, as their core theological premise would be invalidated. If only practice, the doctrine remains ''holdable'' in principle, making their ''identity_locked'' exit slightly less absolute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_vs_practice_suspension, conceptual, 'Scope of the Manifesto''s suspension: doctrine or practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.05).
narrative_ontology:measurement(plur_tr_t1894, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1894, 0.07).
narrative_ontology:measurement(plur_tr_t1898, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1898, 0.08).
narrative_ontology:measurement(plur_tr_t1901, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1901, 0.09).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.1).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.2).
narrative_ontology:measurement(plur_be_t1894, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1894, 0.22).
narrative_ontology:measurement(plur_be_t1898, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1898, 0.23).
narrative_ontology:measurement(plur_be_t1901, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1901, 0.24).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.5).
narrative_ontology:measurement(plur_su_t1894, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1894, 0.55).
narrative_ontology:measurement(plur_su_t1898, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1898, 0.58).
narrative_ontology:measurement(plur_su_t1901, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1901, 0.59).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1904, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'plural_marriage_mandate' kernel. This 'endogenous_reinterpretation_reading' emphasizes prophetic authority and internal theological consistency, contrasting with the 'exogenous_override_reading' (federal coercion) and 'institutional_pragmatism_reading' (strategic adaptation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
