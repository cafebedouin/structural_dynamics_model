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
 *   human_readable: Sola Scriptura as Self-Interpreting Authority
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint represents the 'sola scriptura' principle, a foundational
 *   tenet of the Protestant Reformation, asserting that Scripture alone is
 *   the sufficient and self-interpreting authority for Christian doctrine and
 *   practice. It is a reading of the broader 'biblical_authority' kernel.
 *   This reading emphasizes individual access to and interpretation of the
 *   Bible, leading to reduced clerical extraction but increased doctrinal
 *   fragmentation. The metrics reflect a relatively low extractiveness and
 *   suppression, consistent with a coordination mechanism that empowers
 *   individuals, but with a rising trend in extractiveness as the costs of
 *   doctrinal fragmentation become more apparent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.25).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.15).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura as Self-Interpreting Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theology/religious_studies/history_of_christianity").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, '8420e98d-d504-4b70-8223-9b1727e54e3d').
narrative_ontology:cs_kernel_codification('8420e98d-d504-4b70-8223-9b1727e54e3d', fixed_text).
narrative_ontology:cs_authority_grounding('8420e98d-d504-4b70-8223-9b1727e54e3d', distributed).
narrative_ontology:cs_reading_relation('8420e98d-d504-4b70-8223-9b1727e54e3d', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_reading_relation('8420e98d-d504-4b70-8223-9b1727e54e3d', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('8420e98d-d504-4b70-8223-9b1727e54e3d', foundational, scripture_alone_is_sufficient).
narrative_ontology:cs_axiom_status(scripture_alone_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('8420e98d-d504-4b70-8223-9b1727e54e3d', scripture_alone_is_sufficient, deontological).
narrative_ontology:cs_axiom('8420e98d-d504-4b70-8223-9b1727e54e3d', foundational, scripture_is_perspicuous).
narrative_ontology:cs_axiom_status(scripture_is_perspicuous, holdable).
narrative_ontology:cs_axiom_grounding('8420e98d-d504-4b70-8223-9b1727e54e3d', scripture_is_perspicuous, deontological).
narrative_ontology:cs_reference_frame('8420e98d-d504-4b70-8223-9b1727e54e3d', reformation_era_individual_access).
narrative_ontology:cs_drift_state('8420e98d-d504-4b70-8223-9b1727e54e3d', contemporary_theological_pluralism, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8420e98d-d504-4b70-8223-9b1727e54e3d', '').
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

% Empowered to read and interpret scripture for themselves, without requiring clerical mediation. This grants significant autonomy in matters of faith and practice, but also places the burden of interpretation on the individual.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believers, beneficiary,
    moderate, biographical, mobile, local).

% Benefits from the principle that scripture is accessible and understandable to all, fostering a direct relationship with the text. This leads to diverse interpretations and the formation of new theological perspectives.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, individual_interpreters, beneficiary,
    moderate, biographical, mobile, local).

% While still providing guidance and teaching, their authority is derived from their ability to expound scripture, not from an inherent magisterial role. Their interpretations are subject to challenge by lay members who also claim direct access to the text.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, clerical_leadership, agenda_setter,
    organized, generational, constrained, regional).

% Suffers from the lack of a centralized, authoritative interpretive body. While individual communities may achieve internal coherence, the broader landscape of 'sola scriptura' traditions is marked by significant theological diversity and fragmentation, making inter-community doctrinal agreement difficult.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities).

% Explicitly rejected as a necessary interpretive authority. While historical insights may be valued, their pronouncements are not binding in the same way as scripture itself, leading to their exclusion from the primary interpretive framework.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, ecumenical_councils_and_patristic_consensus, excluded,
    institutional, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_non_agent(biblical_authority__sola_scriptura_reading, ecumenical_councils_and_patristic_consensus).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual believers around a common textual authority, enabling decentralized theological development and congregational autonomy.
% TRANSFER_FUNCTION: Transfers interpretive authority from a centralized clerical or traditional body to the individual believer and local community, fostering spiritual independence.
% ABSENT_VOICES: Those who advocate for the necessity of tradition or conciliar authority for interpretation are structurally excluded from this framework, as their claims are deemed secondary or unnecessary to the direct understanding of scripture.
% DISAPPEARANCE_RATIONALE: If 'sola scriptura' vanished, the theological landscape of Protestantism would fundamentally shift. Individual interpretive autonomy would be undermined, leading to a scramble for new authoritative sources, likely resulting in either a return to traditional authorities or a complete collapse of shared doctrinal frameworks.
% FOUNDING_PROBLEM: The perceived corruption and unbiblical practices of the medieval church, coupled with a desire to make religious authority directly accessible to the common person.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Reformation and contemporary theologians from various traditions corroborate the historical context and ongoing relevance of the desire for direct access to scripture and resistance to perceived external authorities. Lay believers continue to attest to the importance of individual interpretation.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The low extractiveness (0.25) reflects the principle's intent to remove intermediaries and their associated costs, empowering lay believers. Suppression (0.15) is also low, as the constraint actively resists external interpretive authorities rather than imposing them. The initial high suppression_requirement in 1517 reflects the active struggle against established church authority, which then declined as the principle became more established. Theater ratio is low (0.1), indicating that the principle's function (individual interpretation) is largely genuine, though some performative aspects may exist in defending its purity against perceived compromises. Accessibility collapse is moderate (0.4) because while it removes external barriers, the complexity of scripture itself still presents interpretive challenges. Resistance is moderate (0.3) as it faces ongoing challenges from traditions that emphasize other forms of authority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of lay believers, 'sola scriptura' is a liberating rope, freeing them from hierarchical control. From the perspective of those concerned with church unity or historical continuity, it might appear as a tangled rope or even a snare, leading to fragmentation and theological instability. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay believers and individual interpreters are clear beneficiaries, gaining direct access to religious authority. Clerical leadership, while still present, shifts from an exclusive interpretive authority to a facilitative role, reducing their extractive capacity. Doctrinal coherence across communities is a 'victim' in the sense that the principle's emphasis on individual interpretation inherently leads to diverse and sometimes conflicting theological positions, making unified doctrine difficult to maintain.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (empowering individuals and decentralizing authority) remains largely live, though its status is 'contested' due to ongoing debates about its practical consequences (e.g., denominationalism, theological relativism). The low theater ratio and relatively stable extractiveness suggest it has not significantly atrophied into a piton, but the rising extractiveness over time indicates that the costs of its operation (doctrinal fragmentation) are accumulating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_fragmentation_cost,
    'Is the observed doctrinal fragmentation an inherent, unavoidable cost of individual interpretive freedom, or a remediable side-effect of insufficient communal interpretive practices within the ''sola scriptura'' framework?',
    'Empirical study of ''sola scriptura'' communities that have successfully maintained high levels of doctrinal coherence through robust communal interpretive disciplines, compared to those that have not.',
    'If remediable, the ''extractiveness'' attributed to doctrinal incoherence could be reduced, potentially reclassifying the constraint as a purer rope. If inherent, the current extractiveness is a necessary cost of the principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_fragmentation_cost, empirical, 'Assesses whether doctrinal fragmentation is a necessary or contingent outcome of ''sola scriptura''.').

omega_variable(
    self_interpreting_ambiguity,
    'To what extent is Scripture truly ''self-interpreting'' without any external interpretive framework (e.g., historical context, linguistic tools, theological presuppositions)?',
    'Conceptual analysis of hermeneutical theory and empirical observation of interpretive disagreements even among those committed to ''sola scriptura''.',
    'If Scripture is not entirely self-interpreting, then the ''sola scriptura'' reading implicitly relies on unacknowledged interpretive traditions or tools, which could introduce hidden forms of authority or extraction, pushing extractiveness upward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_interpreting_ambiguity, conceptual, 'Examines the conceptual coherence of Scripture''s ''self-interpreting'' claim.').

omega_variable(
    clerical_authority_reconstitution,
    'Does the ''sola scriptura'' principle, by decentralizing authority, merely shift the locus of clerical authority from a formal hierarchy to informal charismatic leaders or influential scholars, thereby reconstituting extraction in a different form?',
    'Sociological studies of ''sola scriptura'' communities to identify patterns of informal authority and their impact on individual interpretive freedom and resource allocation.',
    'If informal authority structures effectively re-centralize interpretive power and extract resources (e.g., through control of publishing, media, or educational institutions), the effective extractiveness of the constraint would be higher than currently measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clerical_authority_reconstitution, empirical, 'Investigates whether informal authority replaces formal clerical authority under ''sola scriptura''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 1517, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1517, biblical_authority__sola_scriptura_reading, theater_ratio, 1517, 0.05).
narrative_ontology:measurement(bibl_tr_t1600, biblical_authority__sola_scriptura_reading, theater_ratio, 1600, 0.07).
narrative_ontology:measurement(bibl_tr_t1750, biblical_authority__sola_scriptura_reading, theater_ratio, 1750, 0.08).
narrative_ontology:measurement(bibl_tr_t1900, biblical_authority__sola_scriptura_reading, theater_ratio, 1900, 0.09).
narrative_ontology:measurement(bibl_tr_t2024, biblical_authority__sola_scriptura_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1517, biblical_authority__sola_scriptura_reading, base_extractiveness, 1517, 0.1).
narrative_ontology:measurement(bibl_be_t1600, biblical_authority__sola_scriptura_reading, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement(bibl_be_t1750, biblical_authority__sola_scriptura_reading, base_extractiveness, 1750, 0.2).
narrative_ontology:measurement(bibl_be_t1900, biblical_authority__sola_scriptura_reading, base_extractiveness, 1900, 0.23).
narrative_ontology:measurement(bibl_be_t2024, biblical_authority__sola_scriptura_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1517, biblical_authority__sola_scriptura_reading, suppression_requirement, 1517, 0.8).
narrative_ontology:measurement(bibl_su_t1600, biblical_authority__sola_scriptura_reading, suppression_requirement, 1600, 0.6).
narrative_ontology:measurement(bibl_su_t1750, biblical_authority__sola_scriptura_reading, suppression_requirement, 1750, 0.4).
narrative_ontology:measurement(bibl_su_t1900, biblical_authority__sola_scriptura_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(bibl_su_t2024, biblical_authority__sola_scriptura_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
