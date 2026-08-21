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
 *   This constraint represents the 'Sola Scriptura' reading of biblical
 *   authority, a foundational principle of the Protestant Reformation. It
 *   asserts that Scripture alone is the sufficient and self-interpreting
 *   authority for Christian doctrine and practice, rejecting the need for
 *   ecclesiastical tradition or magisterial interpretation as co-equal or
 *   superior sources. This reading empowers individual believers and local
 *   communities but leads to significant doctrinal fragmentation across
 *   communities. This is one reading of the 'biblical_authority' kernel.
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
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, 'fb0e2f61-1e70-4dfd-9196-d28f33df333a').
narrative_ontology:cs_kernel_codification('fb0e2f61-1e70-4dfd-9196-d28f33df333a', fixed_text).
narrative_ontology:cs_authority_grounding('fb0e2f61-1e70-4dfd-9196-d28f33df333a', diffuse_epistemic).
narrative_ontology:cs_reading_relation('fb0e2f61-1e70-4dfd-9196-d28f33df333a', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb0e2f61-1e70-4dfd-9196-d28f33df333a', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('fb0e2f61-1e70-4dfd-9196-d28f33df333a', foundational, scripture_is_perspicuous).
narrative_ontology:cs_axiom_status(scripture_is_perspicuous, holdable).
narrative_ontology:cs_axiom_grounding('fb0e2f61-1e70-4dfd-9196-d28f33df333a', scripture_is_perspicuous, deontological).
narrative_ontology:cs_axiom('fb0e2f61-1e70-4dfd-9196-d28f33df333a', foundational, tradition_is_subordinate).
narrative_ontology:cs_axiom_status(tradition_is_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('fb0e2f61-1e70-4dfd-9196-d28f33df333a', tradition_is_subordinate, deontological).
narrative_ontology:cs_reference_frame('fb0e2f61-1e70-4dfd-9196-d28f33df333a', reformation_era_scriptural_supremacy).
narrative_ontology:cs_drift_state('fb0e2f61-1e70-4dfd-9196-d28f33df333a', contemporary_postmodern_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fb0e2f61-1e70-4dfd-9196-d28f33df333a', '').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, individual_interpreters).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, clerical_hierarchies).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, priesthood_of_all_believers).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, perspicuity_of_scripture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Empowered to read and interpret scripture for themselves, without needing clerical mediation. This grants significant autonomy in doctrinal and ethical matters, but also places the burden of interpretation on the individual.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believers, beneficiary,
    moderate, biographical, mobile, local).

% Benefits from the principle that scripture is self-interpreting, allowing for diverse theological perspectives and the formation of new denominations or movements based on individual or group readings. This reduces the power of established ecclesiastical hierarchies.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, individual_interpreters, beneficiary,
    moderate, biographical, mobile, local).

% Suffers from the lack of a single, universally accepted interpretive authority. This leads to significant theological diversity, denominational fragmentation, and ongoing disputes over fundamental doctrines, making unified action or belief difficult.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities).

% Experiences a reduction in their traditional authority as the sole interpreters of scripture. While still holding roles in teaching and pastoral care, their interpretive monopoly is challenged, leading to less direct control over congregational doctrine and practice.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, clerical_hierarchies, payer,
    institutional, generational, constrained, global).

% Their historical role in establishing authoritative doctrine is diminished or rejected. They would argue for the necessity of collective, historically informed interpretation to maintain unity and guard against heresy, but their voice is not considered binding.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, ecumenical_councils, excluded,
    institutional, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(biblical_authority__sola_scriptura_reading, ecumenical_councils).

% Analyze the historical development and theological implications of Sola Scriptura, observing its effects on church structure, doctrinal development, and inter-denominational relations. They can identify patterns of fragmentation and the emergence of new interpretive communities.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, theologians_and_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, accessible, and ultimate source of authority for individual believers and local congregations, enabling decentralized theological development and reducing reliance on distant or hierarchical ecclesiastical structures.
% TRANSFER_FUNCTION: Transfers interpretive authority from clerical hierarchies and historical traditions to the individual believer and the text itself, leading to a diffusion of doctrinal power.
% ABSENT_VOICES: Proponents of conciliar authority and magisterial tradition are structurally excluded from the interpretive process, as their claims to mediate or supplement scriptural authority are rejected. They would argue for the necessity of a unified interpretive framework to prevent fragmentation.
% DISAPPEARANCE_RATIONALE: If Sola Scriptura vanished, the theological landscape of Protestantism would fundamentally shift. Denominations founded on this principle would lose their core justification, leading to either a collapse into diverse individual interpretations without a common anchor, or a re-engagement with historical traditions and hierarchical authorities to re-establish doctrinal coherence.
% FOUNDING_PROBLEM: The problem of perceived corruption and unbiblical practices within the medieval church, where human traditions and papal authority were seen to supersede or obscure the clear teaching of scripture.
% FOUNDING_PROBLEM_CORROBORATION: Many Protestant denominations and independent churches continue to attest that the problem of human tradition overriding scriptural authority remains live, citing ongoing concerns about ecclesiastical power and doctrinal deviation. Critics, however, argue that the problem has shifted from hierarchical overreach to unchecked individual interpretation, leading to new forms of fragmentation.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low because the principle aims to reduce clerical mediation and associated costs, empowering lay believers. Suppression is also low, as the constraint's persistence relies on voluntary adherence and the perceived clarity of scripture, rather than coercive enforcement. Theater ratio is minimal, as the principle is largely functional in guiding interpretation, with little performative overhead. Accessibility collapse is high because once the principle is accepted, alternative interpretive authorities (tradition, councils) are largely collapsed. Resistance is low from within the communities that adopt it, though it faces external resistance from other Christian traditions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of lay believers, this is a liberating principle (low extraction, high autonomy). From the perspective of those concerned with church unity or historical continuity, it is a source of chaos and division (high fragmentation, loss of tradition). The engine's classification will reflect the low extraction for individuals but highlight the costs to collective coherence.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay believers and individual interpreters are clear beneficiaries, gaining autonomy and direct access to authority. Doctrinal coherence across communities is a victim, as the lack of a central interpretive authority leads to fragmentation. Clerical hierarchies, particularly those with strong claims to interpretive authority, are also victims, as their power is diminished. Ecumenical councils are excluded, as their role is largely rejected by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_interpretation_ambiguity,
    'To what extent is Scripture truly ''self-interpreting'' without any external interpretive framework (e.g., historical context, linguistic expertise, theological presuppositions)?',
    'Empirical study of interpretive divergence among ''Sola Scriptura'' adherents, particularly on complex or ambiguous passages, compared to divergence within traditions with explicit interpretive frameworks.',
    'If significant interpretive divergence persists even with ''self-interpretation,'' it suggests an implicit, unacknowledged interpretive framework is always at play, potentially reintroducing unexamined forms of authority or leading to greater fragmentation than acknowledged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_interpretation_ambiguity, empirical, 'The degree to which Scripture''s ''self-interpreting'' nature is a functional reality or a theological ideal.').

omega_variable(
    doctrinal_fragmentation_cost,
    'Is the observed doctrinal fragmentation a necessary cost of individual interpretive freedom, or an avoidable consequence of rejecting legitimate, non-extractive interpretive authorities?',
    'Comparative analysis of the social and theological costs of fragmentation (e.g., schism, inability to address collective challenges) versus the benefits of individual autonomy, across different Christian traditions.',
    'If fragmentation costs are deemed severe and avoidable, it would challenge the ''rope'' classification by highlighting a significant, unmitigated negative externality, potentially shifting it towards a ''tangled_rope'' for collective entities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_fragmentation_cost, preference, 'The normative evaluation of doctrinal fragmentation as a trade-off for individual interpretive freedom.').

omega_variable(
    clerical_authority_reconstitution,
    'Does the rejection of traditional clerical hierarchies under Sola Scriptura lead to the reconstitution of new, informal, and potentially less accountable forms of interpretive authority (e.g., charismatic leaders, popular commentators)?',
    'Sociological and historical studies of Protestant movements, identifying patterns of authority emergence and consolidation in contexts that formally reject traditional hierarchies.',
    'If new, unaccountable authorities consistently emerge, the ''low clerical extraction'' claim would be challenged, suggesting a shift in the *locus* of extraction rather than its elimination, potentially reclassifying the constraint as a ''snare'' for those under the new authorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clerical_authority_reconstitution, empirical, 'Whether Sola Scriptura truly eliminates clerical extraction or merely shifts its form.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 1517, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1517, biblical_authority__sola_scriptura_reading, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(bibl_tr_t1600, biblical_authority__sola_scriptura_reading, theater_ratio, 1600, 0.08).
narrative_ontology:measurement(bibl_tr_t1750, biblical_authority__sola_scriptura_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(bibl_tr_t1900, biblical_authority__sola_scriptura_reading, theater_ratio, 1900, 0.03).
narrative_ontology:measurement(bibl_tr_t2024, biblical_authority__sola_scriptura_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1517, biblical_authority__sola_scriptura_reading, base_extractiveness, 1517, 0.2).
narrative_ontology:measurement(bibl_be_t1600, biblical_authority__sola_scriptura_reading, base_extractiveness, 1600, 0.25).
narrative_ontology:measurement(bibl_be_t1750, biblical_authority__sola_scriptura_reading, base_extractiveness, 1750, 0.22).
narrative_ontology:measurement(bibl_be_t1900, biblical_authority__sola_scriptura_reading, base_extractiveness, 1900, 0.28).
narrative_ontology:measurement(bibl_be_t2024, biblical_authority__sola_scriptura_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1517, biblical_authority__sola_scriptura_reading, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(bibl_su_t1600, biblical_authority__sola_scriptura_reading, suppression_requirement, 1600, 0.2).
narrative_ontology:measurement(bibl_su_t1750, biblical_authority__sola_scriptura_reading, suppression_requirement, 1750, 0.15).
narrative_ontology:measurement(bibl_su_t1900, biblical_authority__sola_scriptura_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(bibl_su_t2024, biblical_authority__sola_scriptura_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, theological_education_standards).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, denominational_governance_structures).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'biblical_authority' kernel. Its ε value differs significantly from the 'tradition_scripture_reading' and 'conciliar_reading' due to its unique beneficiary/victim structure and claims about interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
