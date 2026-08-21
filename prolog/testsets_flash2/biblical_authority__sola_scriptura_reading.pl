% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__sola_scriptura_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Sola Scriptura: Scripture Alone as Sufficient Authority
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint represents the 'sola scriptura' reading of biblical
 *   authority, a foundational principle of the Protestant Reformation. It
 *   asserts that the Bible alone is the sufficient and self-interpreting
 *   source for Christian doctrine and practice, without the need for an
 *   external interpretive authority like a magisterium or tradition. This
 *   reading aims to reduce clerical extraction and empower individual
 *   believers, but it also leads to significant doctrinal fragmentation
 *   across communities. This is one reading of the 'biblical_authority'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.25).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.15).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura: Scripture Alone as Sufficient Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theology/religious_studies/history_of_christianity").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, '73cf5a35-5b93-4a05-aeb2-9eddff4b6847').
narrative_ontology:cs_kernel_codification('73cf5a35-5b93-4a05-aeb2-9eddff4b6847', fixed_text).
narrative_ontology:cs_authority_grounding('73cf5a35-5b93-4a05-aeb2-9eddff4b6847', diffuse_epistemic).
narrative_ontology:cs_reading_relation('73cf5a35-5b93-4a05-aeb2-9eddff4b6847', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_reading_relation('73cf5a35-5b93-4a05-aeb2-9eddff4b6847', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('73cf5a35-5b93-4a05-aeb2-9eddff4b6847', foundational, scripture_alone_is_sufficient).
narrative_ontology:cs_axiom_status(scripture_alone_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('73cf5a35-5b93-4a05-aeb2-9eddff4b6847', scripture_alone_is_sufficient, deontological).
narrative_ontology:cs_axiom('73cf5a35-5b93-4a05-aeb2-9eddff4b6847', foundational, scripture_is_self_interpreting).
narrative_ontology:cs_axiom_status(scripture_is_self_interpreting, holdable).
narrative_ontology:cs_axiom_grounding('73cf5a35-5b93-4a05-aeb2-9eddff4b6847', scripture_is_self_interpreting, conventional).
narrative_ontology:cs_reference_frame('73cf5a35-5b93-4a05-aeb2-9eddff4b6847', reformation_era_clarity).
narrative_ontology:cs_drift_state('73cf5a35-5b93-4a05-aeb2-9eddff4b6847', contemporary_pluralism, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('73cf5a35-5b93-4a05-aeb2-9eddff4b6847', '').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, individual_interpreters).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, priesthood_of_all_believers).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, individual_conscience).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Empowered to read and interpret scripture for themselves, without mandatory mediation by a clerical hierarchy. This grants significant autonomy in doctrinal and practical matters, but also places the burden of interpretation on the individual.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believers, beneficiary,
    moderate, biographical, mobile, local).

% Benefits from the principle that scripture is clear enough to be understood by ordinary readers, reducing reliance on specialized clerical training or institutional pronouncements. This fosters diverse theological perspectives.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, individual_interpreters, beneficiary,
    moderate, biographical, mobile, local).

% Suffers from the lack of a single, universally recognized interpretive authority. While individual communities may achieve internal coherence, the broader landscape of 'sola scriptura' traditions is characterized by significant doctrinal fragmentation and denominational diversity.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities).

% While not holding ultimate interpretive authority, they guide congregational understanding and maintain local doctrinal standards. Their authority is persuasive and pastoral, rather than magisterial, and is subject to congregational assent.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, clerical_hierarchies_in_sola_scriptura_traditions, agenda_setter,
    organized, generational, constrained, regional).

% Analyze the historical development, theological implications, and practical consequences of the sola scriptura principle, observing its impact on church structure, doctrine, and inter-denominational relations.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, theological_academics, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Christian doctrine and practice around a single, accessible textual source, reducing reliance on potentially corrupt or divergent oral traditions and human authorities. It provides a common reference point for diverse communities.
% TRANSFER_FUNCTION: Transfers interpretive authority from clerical hierarchies and historical traditions to the individual believer and the local community. It transfers the burden of doctrinal discernment to the reader.
% ABSENT_VOICES: Proponents of magisterial authority (e.g., Roman Catholic, Eastern Orthodox) and those who emphasize the necessity of tradition for interpretation (e.g., Anglican, some Lutheran) are structurally excluded from the core premise of sola scriptura. They would argue that scripture is not self-interpreting and requires an authoritative interpretive framework.
% DISAPPEARANCE_RATIONALE: If the principle of sola scriptura vanished overnight, many Protestant denominations would lose their foundational theological premise. Interpretive authority would likely revert to various forms of tradition or magisterial decree, fundamentally altering the structure of many Christian communities and individual believers' relationship to scripture.
% FOUNDING_PROBLEM: The problem of perceived corruption and theological error within the medieval Roman Catholic Church, particularly the perceived elevation of human tradition above biblical teaching and the restriction of scripture access to the laity.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within sola scriptura traditions continue to attest that the problem of human authority superseding divine revelation remains live, citing ongoing concerns about institutional overreach and doctrinal innovation. Critics from other traditions acknowledge the historical context but dispute the proposed solution's long-term efficacy for unity.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__sola_scriptura_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).
:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the principle itself is designed to minimize institutional rent-seeking from interpretive authority, shifting the burden to the individual. Suppression is low (0.15) as it actively resists external coercion in matters of faith. Theater ratio is very low (0.05) as the principle is generally genuinely applied, though some communities may develop de facto interpretive authorities. Accessibility collapse is high (0.7) because once the principle is accepted, alternatives like magisterial authority are largely foreclosed. Resistance is low (0.1) from within its own tradition, though it faces external resistance from other Christian traditions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of lay believers, this is a liberating principle (low extraction, high autonomy). From the perspective of those concerned with universal doctrinal coherence, it is a source of fragmentation and potential error (high cost to coherence). The engine's classification will reflect the low extraction from individuals, while the 'victim' of doctrinal coherence highlights a systemic cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay believers and individual interpreters are direct beneficiaries, gaining autonomy and direct access to the source of faith. Doctrinal coherence across communities is a 'victim' in the sense that it is a cost borne by the system, not an agent that is actively extracted from. Clerical hierarchies within sola scriptura traditions act as agenda-setters, guiding interpretation but without ultimate authority, thus their extraction is limited.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_interpreting_ambiguity,
    'Is scripture truly ''self-interpreting'' in practice, or does it inevitably require interpretive frameworks (e.g., historical context, linguistic tools, theological presuppositions) that are themselves external to the text?',
    'Empirical study of interpretive divergence among ''sola scriptura'' adherents on complex theological issues, comparing outcomes with and without explicit interpretive guidelines.',
    'If not truly self-interpreting, the ''sola scriptura'' principle might inadvertently create a ''distributed'' authority structure where interpretive frameworks (often implicit) become de facto authorities, potentially leading to unacknowledged extraction or suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_interpreting_ambiguity, conceptual, 'Ambiguity regarding the practical self-interpretability of scripture.').

omega_variable(
    doctrinal_fragmentation_cost,
    'What is the actual cost of doctrinal fragmentation (e.g., to evangelism, social witness, inter-denominational cooperation) resulting from the ''sola scriptura'' principle, and is this cost outweighed by the benefits of individual autonomy?',
    'Sociological and theological studies comparing the social and missional impact of highly fragmented vs. more unified Christian traditions.',
    'If the costs are deemed prohibitive, it might lead to a re-evaluation of the ''sola scriptura'' principle''s practical implications, potentially pushing towards greater emphasis on ecumenical dialogue or shared interpretive standards.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_fragmentation_cost, preference, 'Evaluation of the trade-off between individual autonomy and doctrinal coherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 1517, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1517, biblical_authority__sola_scriptura_reading, theater_ratio, 1517, 0.0).
narrative_ontology:measurement(bibl_tr_t1600, biblical_authority__sola_scriptura_reading, theater_ratio, 1600, 0.02).
narrative_ontology:measurement(bibl_tr_t1700, biblical_authority__sola_scriptura_reading, theater_ratio, 1700, 0.03).
narrative_ontology:measurement(bibl_tr_t1800, biblical_authority__sola_scriptura_reading, theater_ratio, 1800, 0.04).
narrative_ontology:measurement(bibl_tr_t1900, biblical_authority__sola_scriptura_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(bibl_tr_t2024, biblical_authority__sola_scriptura_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1517, biblical_authority__sola_scriptura_reading, base_extractiveness, 1517, 0.1).
narrative_ontology:measurement(bibl_be_t1600, biblical_authority__sola_scriptura_reading, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement(bibl_be_t1700, biblical_authority__sola_scriptura_reading, base_extractiveness, 1700, 0.2).
narrative_ontology:measurement(bibl_be_t1800, biblical_authority__sola_scriptura_reading, base_extractiveness, 1800, 0.22).
narrative_ontology:measurement(bibl_be_t1900, biblical_authority__sola_scriptura_reading, base_extractiveness, 1900, 0.23).
narrative_ontology:measurement(bibl_be_t2024, biblical_authority__sola_scriptura_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1517, biblical_authority__sola_scriptura_reading, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(bibl_su_t1600, biblical_authority__sola_scriptura_reading, suppression_requirement, 1600, 0.2).
narrative_ontology:measurement(bibl_su_t1700, biblical_authority__sola_scriptura_reading, suppression_requirement, 1700, 0.15).
narrative_ontology:measurement(bibl_su_t1800, biblical_authority__sola_scriptura_reading, suppression_requirement, 1800, 0.12).
narrative_ontology:measurement(bibl_su_t1900, biblical_authority__sola_scriptura_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(bibl_su_t2024, biblical_authority__sola_scriptura_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__conciliar_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'biblical_authority' kernel. Its core premise of scripture's sole sufficiency stands in structural tension with readings that emphasize tradition or conciliar authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
