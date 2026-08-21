% ============================================================================
% CONSTRAINT STORY: biblical_authority__conciliar_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__conciliar_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: biblical_authority__conciliar_reading
 *   human_readable: Biblical Authority via Ecumenical Councils and Patristic Consensus
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint describes the authority structure within certain
 *   Christian traditions (e.g., Eastern Orthodoxy) where Scripture is
 *   interpreted through the lens of ecumenical councils and the consensus of
 *   the Church Fathers. Tradition is understood as a living continuity of
 *   faith, not a static set of rules or a singular magisterial decree. This
 *   framework aims to ensure doctrinal stability and unity across
 *   autocephalous churches, but it also involves a moderate degree of
 *   clerical extraction (episcopal authority) and can suppress rapid
 *   doctrinal adaptation or individual theological innovation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__conciliar_reading, 0.45).
domain_priors:suppression_score(biblical_authority__conciliar_reading, 0.55).
domain_priors:theater_ratio(biblical_authority__conciliar_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__conciliar_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__conciliar_reading, "Biblical Authority via Ecumenical Councils and Patristic Consensus").
narrative_ontology:topic_domain(biblical_authority__conciliar_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__conciliar_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__conciliar_reading, '1c3fa5c8-5f56-4c25-9409-a8258407db5c').
narrative_ontology:cs_kernel_codification('1c3fa5c8-5f56-4c25-9409-a8258407db5c', formalized).
narrative_ontology:cs_authority_grounding('1c3fa5c8-5f56-4c25-9409-a8258407db5c', lineage).
narrative_ontology:cs_interpretation_layer_present('1c3fa5c8-5f56-4c25-9409-a8258407db5c').
narrative_ontology:cs_reading_relation('1c3fa5c8-5f56-4c25-9409-a8258407db5c', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('1c3fa5c8-5f56-4c25-9409-a8258407db5c', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_axiom('1c3fa5c8-5f56-4c25-9409-a8258407db5c', foundational, scripture_interpreted_by_church).
narrative_ontology:cs_axiom_status(scripture_interpreted_by_church, holdable).
narrative_ontology:cs_axiom_grounding('1c3fa5c8-5f56-4c25-9409-a8258407db5c', scripture_interpreted_by_church, conventional).
narrative_ontology:cs_axiom('1c3fa5c8-5f56-4c25-9409-a8258407db5c', foundational, tradition_as_living_consensus).
narrative_ontology:cs_axiom_status(tradition_as_living_consensus, holdable).
narrative_ontology:cs_axiom_grounding('1c3fa5c8-5f56-4c25-9409-a8258407db5c', tradition_as_living_consensus, conventional).
narrative_ontology:cs_reference_frame('1c3fa5c8-5f56-4c25-9409-a8258407db5c', apostolic_faith_consensus).
narrative_ontology:cs_drift_state('1c3fa5c8-5f56-4c25-9409-a8258407db5c', contemporary_ecumenical_dialogue, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1c3fa5c8-5f56-4c25-9409-a8258407db5c', '').
narrative_ontology:cs_kernel_id(biblical_authority__conciliar_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, church_hierarchs).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, theological_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, laity).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective body of bishops who, through ecumenical councils and synods, interpret Scripture and define doctrine. They benefit from the stability and authority derived from this process, maintaining the unity and order of the church. Their identity is deeply fused with this interpretive role.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, church_hierarchs, agenda_setter,
    institutional, generational, identity_locked, global).

% Theologians and thinkers who propose interpretations or doctrines that deviate from established conciliar and patristic consensus. They bear the cost of having their ideas scrutinized, potentially rejected, or even condemned, leading to professional or ecclesiastical marginalization. Their options are to conform, leave the tradition, or work within its boundaries to effect slow change.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, theological_innovators, payer,
    moderate, biographical, constrained, global).

% Receive a stable, consistent, and authoritative doctrinal framework, which provides spiritual security and a clear path for faith and practice. They also contribute financially (tithes) and through adherence to the church's teachings, which are shaped by this interpretive authority. Their identity is often deeply tied to the church's tradition.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, laity, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, laity, payer).

% Academics and theologians dedicated to studying the writings of the Church Fathers. Their work is foundational to understanding the patristic consensus, and they benefit from the institutional value placed on this tradition. They observe and analyze the interpretive process without directly setting doctrine.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, patristic_scholars, observer,
    analytical, generational, analytical, global).

% Protestant denominations and theologians who assert that Scripture alone is the sufficient and self-interpreting authority for doctrine. They are structurally excluded from the internal interpretive process of the conciliar tradition, as their foundational premise (sufficiency of Scripture alone) is incompatible with the conciliar framework's reliance on tradition.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, sola_scriptura_advocates, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__conciliar_reading, church_hierarchs).
narrative_ontology:fixing_cost_class(biblical_authority__conciliar_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, unified interpretation of Scripture and doctrine across diverse autocephalous churches, preventing fragmentation and theological relativism by grounding authority in historical consensus.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority from individual believers or local communities to the collective wisdom of ecumenical councils and the historical patristic tradition, maintaining hierarchical order and doctrinal consistency.
% ABSENT_VOICES: Advocates for 'sola scriptura' (Scripture alone) or a singular, centralized magisterial authority (papal infallibility) are structurally excluded from the internal discourse of this conciliar framework. They would argue for different loci of ultimate authority.
% DISAPPEARANCE_RATIONALE: If the authority of ecumenical councils and patristic consensus vanished overnight, the autocephalous churches would likely fragment doctrinally, leading to widespread theological disputes, a loss of shared identity, and significant schisms, as each local community or individual sought its own interpretation.
% FOUNDING_PROBLEM: The early Christian church faced numerous heresies and doctrinal disputes regarding the nature of Christ, the Trinity, and the canon of Scripture, threatening its unity, theological coherence, and ability to transmit a consistent faith.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Christianity, theologians from various traditions (including those outside Eastern Orthodoxy), and sociological studies of religious movements corroborate the historical need for doctrinal stability and the role of councils in achieving it. While specific heresies change, the underlying need for doctrinal coherence persists.
narrative_ontology:disappearance_verdict(biblical_authority__conciliar_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__conciliar_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__conciliar_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(biblical_authority__conciliar_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__conciliar_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__conciliar_reading_tests).
:- end_tests(biblical_authority__conciliar_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) stems from the concentration of interpretive authority within the episcopal college, which benefits from the stability and order it maintains. Suppression (0.55) is moderate, as theological dissent is managed through ecclesiastical structures, but not with the absolute coercive force of a centralized, infallible magisterium. The theater ratio is low (0.20) because the conciliar and patristic processes are genuinely functional for maintaining doctrinal coherence and historical continuity. The claimed type is 'tangled_rope' because it provides a genuine coordination function (doctrinal unity) but also involves an asymmetric extraction of interpretive authority and a cost to those seeking rapid theological change.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the church hierarchs, this system is a necessary 'rope' for preserving the integrity of the faith and coordinating diverse churches. From the perspective of theological innovators, it can feel like a 'snare' that stifles intellectual freedom and adaptation. The engine's computation will reveal the effective extraction from these different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'church_hierarchs' (episcopal collegiality) are the primary beneficiaries, as they wield and benefit from the interpretive authority. 'Theological_innovators' are the primary targets, as their work is subject to the constraint's interpretive filters. The 'laity' are beneficiaries of doctrinal stability but also payers through adherence and support. 'Patristic_scholars' are observers whose work is valued within the system. 'Sola_scriptura_advocates' are excluded, as their foundational premise is incompatible with this constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conciliar_vs_magisterial_extraction,
    'How does the level of clerical extraction in a conciliar system (episcopal collegiality) compare to a centralized magisterial system (papal authority)?',
    'Comparative historical and sociological analysis of resource flows, power concentration, and dissent management in different ecclesiastical structures.',
    'If conciliar systems show significantly lower extraction, it supports the ''tangled_rope'' classification over a ''snare'' for this reading. If comparable, it suggests the ''tangled_rope'' is more extractive than initially assessed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_vs_magisterial_extraction, empirical, 'Comparison of extraction levels between conciliar and magisterial authority structures.').

omega_variable(
    tradition_as_living_vs_static,
    'Is ''tradition as living continuity'' genuinely dynamic and adaptable, or does it function as a de facto static, unchallengeable authority?',
    'Analysis of historical instances of doctrinal development and adaptation within conciliar traditions, particularly in response to new scientific or social challenges.',
    'If tradition proves highly adaptable, the suppression metric might be lower than assessed. If it functions as a static barrier, suppression is higher, pushing the classification closer to a ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tradition_as_living_vs_static, empirical, 'The dynamic vs. static nature of tradition in practice.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''conciliar_reading'' of the ''biblical_authority'' kernel, or does it conflate elements of other readings?',
    'Detailed textual analysis of primary sources and theological statements from the tradition, comparing its explicit claims about authority and interpretation against the definitions of the ''sola_scriptura_reading'' and ''tradition_scripture_reading''.',
    'If conflated, the constraint would need decomposition into multiple, more precise readings, each with its own ε and structural properties. If distinct, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifying the distinctness of this kernel reading from its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__conciliar_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__conciliar_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bibl_tr_t20, biblical_authority__conciliar_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(bibl_tr_t40, biblical_authority__conciliar_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(bibl_tr_t60, biblical_authority__conciliar_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(bibl_tr_t80, biblical_authority__conciliar_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(bibl_tr_t100, biblical_authority__conciliar_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__conciliar_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bibl_be_t20, biblical_authority__conciliar_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(bibl_be_t40, biblical_authority__conciliar_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(bibl_be_t60, biblical_authority__conciliar_reading, base_extractiveness, 60, 0.43).
narrative_ontology:measurement(bibl_be_t80, biblical_authority__conciliar_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(bibl_be_t100, biblical_authority__conciliar_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__conciliar_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bibl_su_t20, biblical_authority__conciliar_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(bibl_su_t40, biblical_authority__conciliar_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement(bibl_su_t60, biblical_authority__conciliar_reading, suppression_requirement, 60, 0.53).
narrative_ontology:measurement(bibl_su_t80, biblical_authority__conciliar_reading, suppression_requirement, 80, 0.54).
narrative_ontology:measurement(bibl_su_t100, biblical_authority__conciliar_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__conciliar_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
