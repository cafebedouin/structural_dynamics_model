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
 *   constraint_id: biblical_authority__conciliar_reading
 *   human_readable: Biblical Authority: Conciliar Reading
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint describes the interpretive framework within certain
 *   Christian traditions where biblical authority is understood through the
 *   lens of ecumenical councils and the consensus of early Church Fathers
 *   (patristic consensus). Tradition is seen as a living continuity of faith,
 *   not a static set of rules or a magisterial decree. This reading
 *   emphasizes collegiality and historical grounding, aiming for doctrinal
 *   stability. The claimed type is 'tangled_rope' because it genuinely
 *   coordinates theological understanding but also involves moderate clerical
 *   extraction (episcopal authority) and suppresses rapid doctrinal
 *   adaptation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__conciliar_reading, 0.45).
domain_priors:suppression_score(biblical_authority__conciliar_reading, 0.6).
domain_priors:theater_ratio(biblical_authority__conciliar_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__conciliar_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__conciliar_reading, "Biblical Authority: Conciliar Reading").
narrative_ontology:topic_domain(biblical_authority__conciliar_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__conciliar_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__conciliar_reading, 'd3dee4ff-c262-45e9-aef1-72840640aa39').
narrative_ontology:cs_kernel_codification('d3dee4ff-c262-45e9-aef1-72840640aa39', formalized).
narrative_ontology:cs_authority_grounding('d3dee4ff-c262-45e9-aef1-72840640aa39', lineage).
narrative_ontology:cs_interpretation_layer_present('d3dee4ff-c262-45e9-aef1-72840640aa39').
narrative_ontology:cs_reading_relation('d3dee4ff-c262-45e9-aef1-72840640aa39', biblical_authority__sola_scriptura_reading, coexists_with).
narrative_ontology:cs_reading_relation('d3dee4ff-c262-45e9-aef1-72840640aa39', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_axiom('d3dee4ff-c262-45e9-aef1-72840640aa39', foundational, scripture_interpreted_by_consensus).
narrative_ontology:cs_axiom_status(scripture_interpreted_by_consensus, holdable).
narrative_ontology:cs_axiom_grounding('d3dee4ff-c262-45e9-aef1-72840640aa39', scripture_interpreted_by_consensus, conventional).
narrative_ontology:cs_axiom('d3dee4ff-c262-45e9-aef1-72840640aa39', foundational, tradition_as_living_continuity).
narrative_ontology:cs_axiom_status(tradition_as_living_continuity, holdable).
narrative_ontology:cs_axiom_grounding('d3dee4ff-c262-45e9-aef1-72840640aa39', tradition_as_living_continuity, deontological).
narrative_ontology:cs_reference_frame('d3dee4ff-c262-45e9-aef1-72840640aa39', patristic_conciliar_consensus).
narrative_ontology:cs_drift_state('d3dee4ff-c262-45e9-aef1-72840640aa39', contemporary_ecumenical_dialogue, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('d3dee4ff-c262-45e9-aef1-72840640aa39', '').
narrative_ontology:cs_kernel_id(biblical_authority__conciliar_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, episcopal_collegiality).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, theological_scholars).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, rapid_doctrinal_adaptation).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, individual_interpretations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, autocephalous_churches).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective body of bishops, acting through ecumenical councils, defines and guards doctrine. This structure benefits from the authority derived from historical consensus and continuity, maintaining its interpretive primacy.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, episcopal_collegiality, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from a rich interpretive tradition and a framework for theological discourse. Their work is validated by its alignment with patristic consensus and conciliar decrees, providing a stable intellectual environment.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, theological_scholars, beneficiary,
    organized, biographical, constrained, global).

% Represents the tendency for quick, unvetted changes in doctrine. This 'agent' is suppressed by the slow, deliberative process of conciliar and patristic interpretation, ensuring stability at the cost of agility.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, rapid_doctrinal_adaptation, payer,
    powerless, immediate, trapped, global).

% While not entirely suppressed, individual interpretations are expected to align with the broader consensus. Deviation can lead to marginalization or accusations of heresy, imposing a cost on independent theological thought.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, individual_interpretations, payer,
    moderate, biographical, constrained, local).

% Benefit from a shared doctrinal foundation while maintaining administrative independence. This structure allows for local cultural expression within a universal theological framework, but also contributes to fragmentation.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, autocephalous_churches, beneficiary,
    institutional, generational, identity_locked, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates theological understanding and doctrinal unity across diverse Christian communities by grounding interpretation in historical councils and patristic writings, preventing fragmentation into countless individual interpretations.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual believers or contemporary movements to historical consensus and episcopal collegiality, ensuring doctrinal stability and continuity.
% ABSENT_VOICES: Radical reformers or those advocating for rapid doctrinal shifts are excluded from the interpretive process, as their methodologies are deemed incompatible with the conciliar and patristic framework. They would argue for a more dynamic and context-responsive theology.
% DISAPPEARANCE_RATIONALE: If this interpretive constraint vanished, the Christian world would likely fragment into numerous, rapidly evolving doctrinal positions. The historical continuity and shared understanding that define many traditions would dissolve, leading to a chaotic rearrangement of theological authority.
% FOUNDING_PROBLEM: The early Christian church faced numerous heresies and doctrinal disputes, threatening its unity and the integrity of its core beliefs. The problem was how to establish and maintain a consistent, authoritative interpretation of Scripture.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Christianity and ecumenical dialogues attest to the ongoing challenge of maintaining doctrinal unity amidst diverse interpretations. The need for a stable interpretive framework remains a central concern for many Christian traditions, corroborated by continued theological debates and schisms.
narrative_ontology:disappearance_verdict(biblical_authority__conciliar_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__conciliar_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__conciliar_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is moderate (0.45) as the system channels interpretive authority, benefiting episcopal collegiality and theological scholars by providing a stable framework, but at the cost of individual interpretive freedom and rapid adaptation. Suppression is higher (0.60) due to the active enforcement of conciliar decrees and the pressure to conform to patristic consensus, which limits alternative theological expressions. Theater ratio is low (0.20) as the interpretive function is largely genuine, though some performative aspects exist in maintaining the 'living continuity' narrative. The historical interval spans from the First Council of Nicaea (325 CE) to the present, reflecting the long-term evolution of this interpretive tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of episcopal collegiality, this is a 'rope' or even a 'mountain' of divine guidance, ensuring the purity of faith. From the perspective of those advocating for rapid doctrinal adaptation or individual interpretations, it functions more like a 'snare' or 'tangled_rope', limiting their freedom and imposing costs for deviation. The engine's computation will reflect these divergent experiences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Episcopal collegiality and theological scholars are beneficiaries, as the system grants them authority and a stable intellectual environment. Rapid doctrinal adaptation and individual interpretations are 'victims' or payers, as their scope is constrained by the established consensus. Autocephalous churches are also beneficiaries, gaining shared doctrine while retaining administrative autonomy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_tradition_vs_stasis,
    'Is ''living continuity'' a genuine dynamic process of theological development, or does it primarily function to maintain historical stasis and resist contemporary challenges?',
    'Analysis of historical periods of significant doctrinal development vs. periods of rigid adherence to past formulations, assessing the actual flexibility of the interpretive framework.',
    'If primarily stasis, the ''theater_ratio'' might be higher, indicating more performative maintenance of a static tradition. If genuinely dynamic, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_tradition_vs_stasis, conceptual, 'Ambiguity in the dynamism of ''living continuity''.').

omega_variable(
    episcopal_authority_vs_extraction,
    'To what extent does episcopal authority, exercised through councils, genuinely serve the coordination of faith versus extracting power and control from local communities?',
    'Comparative studies of decision-making processes in different autocephalous churches, examining the balance between local autonomy and conciliar authority, and the material benefits accruing to the episcopacy.',
    'Higher extraction would push the classification closer to ''snare'' for local communities; stronger coordination would reinforce the ''tangled_rope'' aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(episcopal_authority_vs_extraction, empirical, 'Distinguishing coordination from extraction in episcopal authority.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as a ''conciliar_reading'' of the ''biblical_authority'' kernel, or does it conflate elements of other readings?',
    'Detailed textual analysis of primary sources and theological treatises to verify the distinct interpretive principles and their boundaries against sibling readings.',
    'Misidentification would lead to an inaccurate classification and incorrect mapping of structural relationships within the ''biblical_authority'' kernel family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifying the distinct identity of this kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__conciliar_reading, 325, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t325, biblical_authority__conciliar_reading, theater_ratio, 325, 0.1).
narrative_ontology:measurement(bibl_tr_t787, biblical_authority__conciliar_reading, theater_ratio, 787, 0.15).
narrative_ontology:measurement(bibl_tr_t1500, biblical_authority__conciliar_reading, theater_ratio, 1500, 0.18).
narrative_ontology:measurement(bibl_tr_t2024, biblical_authority__conciliar_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(bibl_be_t325, biblical_authority__conciliar_reading, base_extractiveness, 325, 0.3).
narrative_ontology:measurement(bibl_be_t787, biblical_authority__conciliar_reading, base_extractiveness, 787, 0.38).
narrative_ontology:measurement(bibl_be_t1500, biblical_authority__conciliar_reading, base_extractiveness, 1500, 0.42).
narrative_ontology:measurement(bibl_be_t2024, biblical_authority__conciliar_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t325, biblical_authority__conciliar_reading, suppression_requirement, 325, 0.45).
narrative_ontology:measurement(bibl_su_t787, biblical_authority__conciliar_reading, suppression_requirement, 787, 0.55).
narrative_ontology:measurement(bibl_su_t1500, biblical_authority__conciliar_reading, suppression_requirement, 1500, 0.58).
narrative_ontology:measurement(bibl_su_t2024, biblical_authority__conciliar_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__conciliar_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__tradition_scripture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'biblical_authority' kernel. Each reading represents a different structural claim about how biblical authority is constituted and interpreted, leading to different ε values and classifications. This 'conciliar_reading' emphasizes historical consensus and episcopal collegiality, distinct from 'sola_scriptura' (scripture alone) and 'tradition_scripture' (magisterial tradition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
