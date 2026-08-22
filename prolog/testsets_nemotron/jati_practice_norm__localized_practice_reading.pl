% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati Boundaries as Localized Practice Coordination Norms
 *   domain: social/religious/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the localized_practice_reading of the
 *   jati_practice_norm kernel. It describes jati boundaries as emergent,
 *   continuously renegotiated coordination norms that proliferate to 3000+
 *   categories across the subcontinent. The reading emphasizes weak central
 *   enforcement, high local plasticity, and coordination functions (marriage
 *   markets, economic reciprocity, ritual cooperation, dispute resolution)
 *   that operate without a unified doctrinal or administrative center.
 *   Extractiveness is low (0.18) because the arrangement primarily solves
 *   coordination problems rather than extracting rents; suppression is
 *   minimal (0.12) because boundary violations are managed through social
 *   friction rather than coercion; theater is modest (0.22) reflecting
 *   performative maintenance of boundary claims. The constraint is classified
 *   as rope: genuine coordination with net beneficiary participants and no
 *   active enforcement machinery.
 *
 * KEY AGENTS:
 *   - local_community_members: Primary participants (moderate/constrained) — navigate boundaries daily for marriage, economic exchange, ritual participation
 *   - caste_association_leaders: Agenda setters (organized/constrained) — convene negotiations, maintain registers, mediate disputes
 *   - marriage_brokers: Beneficiaries (moderate/mobile) — profit from information asymmetry in boundary navigation
 *   - orthodox_textual_authorities: Excluded observers (institutional/analytical) — claim scriptural authority but lack enforcement power in this reading
 *   - colonial_administrator_descendants: Excluded observers (institutional/analytical) — imposed census categories that persist as alternative boundary regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.18).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.12).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Boundaries as Localized Practice Coordination Norms").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social/religious/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, 'b84bf9a7-48ed-4c7e-887b-b15fdd986931').
narrative_ontology:cs_kernel_codification('b84bf9a7-48ed-4c7e-887b-b15fdd986931', distributed).
narrative_ontology:cs_authority_grounding('b84bf9a7-48ed-4c7e-887b-b15fdd986931', practice).
narrative_ontology:cs_interpretation_layer_present('b84bf9a7-48ed-4c7e-887b-b15fdd986931').
narrative_ontology:cs_reading_relation('b84bf9a7-48ed-4c7e-887b-b15fdd986931', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('b84bf9a7-48ed-4c7e-887b-b15fdd986931', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('b84bf9a7-48ed-4c7e-887b-b15fdd986931', foundational, practice_grounds_boundary_legitimacy).
narrative_ontology:cs_axiom_status(practice_grounds_boundary_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b84bf9a7-48ed-4c7e-887b-b15fdd986931', practice_grounds_boundary_legitimacy, conventional).
narrative_ontology:cs_axiom('b84bf9a7-48ed-4c7e-887b-b15fdd986931', foundational, renegotiation_is_legitimate_boundary_maintenance).
narrative_ontology:cs_axiom_status(renegotiation_is_legitimate_boundary_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('b84bf9a7-48ed-4c7e-887b-b15fdd986931', renegotiation_is_legitimate_boundary_maintenance, conventional).
narrative_ontology:cs_reference_frame('b84bf9a7-48ed-4c7e-887b-b15fdd986931', distributed_practice_coordination).
narrative_ontology:cs_drift_state('b84bf9a7-48ed-4c7e-887b-b15fdd986931', contemporary_constitutional_order, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b84bf9a7-48ed-4c7e-887b-b15fdd986931', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, local_community_members).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, caste_association_leaders).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, marriage_brokers).
narrative_ontology:constraint_vindicates(jati_practice_norm__localized_practice_reading, social_coordination_through_negotiated_boundaries).
narrative_ontology:constraint_vindicates(jati_practice_norm__localized_practice_reading, practice_based_authority_over_textual_prescription).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Navigate jati boundaries daily for marriage alliances, economic reciprocity networks, ritual participation, and dispute resolution. Boundaries reduce search costs and provide trusted cooperation frameworks. Exit (conversion, migration, reform) carries high social cost — loss of kinship networks, economic partners, ritual standing — but is not structurally blocked. They experience the constraint as a functional coordination system with negotiation leverage proportional to local demographic weight.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, local_community_members, beneficiary,
    moderate, biographical, constrained, local).

% Convene boundary negotiations, maintain community registers, mediate disputes, organize collective rituals, and interface with state institutions. Their authority derives from recognized representation of the local group, not from external mandate. They can shape boundary definitions but must maintain consensus legitimacy; overt capture triggers schism or rival associations. Their exit is constrained by the same social embeddings as members, plus reputational capital tied to the role.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, caste_association_leaders, agenda_setter,
    organized, generational, constrained, regional).

% Specialize in navigating boundary complexities to arrange marriages across sub-caste and regional lines. They profit from information asymmetry — knowing which boundaries are permeable, which associations accept which alliances, what documentation satisfies which gatekeepers. Their role exists because the system's granularity creates search costs they reduce. They can exit to other matching markets (matrimonial apps, professional networks) but lose the trust premium of traditional brokerage.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, marriage_brokers, beneficiary,
    moderate, biographical, mobile, regional).

% Claim scriptural authority to define varna/jati boundaries from textual sources (Dharmashastras, Puranic genealogies). In this reading, they lack enforcement power over daily practice — their pronouncements are rhetorical resources in local negotiations, not binding rules. They would object to the proliferation and fluidity of boundaries as ritual pollution, but their exclusion from the coordination loop is structural: the practice reading operates without textual adjudication.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, orthodox_textual_authorities, excluded,
    institutional, civilizational, analytical, global).

% Inherited the colonial census apparatus that froze fluid boundaries into fixed categories for governance (reservations, legal protections, welfare targeting). Their categories persist as a parallel boundary regime that the practice reading must negotiate with — e.g., a community may claim one identity for census benefits and another for marriage purposes. They are excluded from the practice reading's coordination loop but their categories structure the incentive landscape.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, colonial_administrator_descendants, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__localized_practice_reading, diffuse).
narrative_ontology:fixing_cost_class(jati_practice_norm__localized_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of trustworthy cooperation across kinship, economic, and ritual domains in ecologically and socially fragmented local contexts — reduces transaction costs for marriage, labor exchange, credit, dispute resolution, and collective action without central enforcement.
% TRANSFER_FUNCTION: Moves social information (boundary definitions, reputations, alliance histories) through distributed negotiation rather than central registry. No single seat captures the gains; coordination value is distributed across participants. Marriage brokers capture information rents; caste associations capture mediation authority; members capture cooperation benefits.
% ABSENT_VOICES: Dalit and Adivasi communities whose boundaries were historically imposed from outside and who experience the negotiation process as domination rather than coordination; women whose marriage-market position is negotiated by male kin and caste leaders; migrant laborers who carry jati identities into contexts where the coordination function does not operate but the stigma does.
% DISAPPEARANCE_RATIONALE: If localized jati coordination vanished overnight, marriage markets would lose their primary trust infrastructure, economic reciprocity networks would lose their enforcement mechanism, ritual cooperation would lose its participant coordination, and dispute resolution would lose its legitimate forums. The social field would reorganize around alternative coordination mechanisms (state law, market contracts, religious reform movements, kinship rewiring) — a massive rearrangement with high transition costs.
% FOUNDING_PROBLEM: Pre-modern South Asian societies faced coordination problems across fragmented ecologies: how to establish trustworthy marriage alliances, labor cooperation, credit networks, and ritual participation without a unified state, market, or church. Jati boundaries emerged as a distributed solution — locally negotiated, reputationally enforced, plastically adaptive.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological consensus (Dumont, Srinivas, Béteille, Fuller, Gupta) attests the coordination function. The orthodox_textual_reading proponents (traditional pandits, Hindu nationalist organizations) contest that the founding problem was ritual purity maintenance, not coordination — they corroborate a different founding problem. Colonial_census_reading proponents (administrative historians, legal scholars of reservation law) attest the founding problem was governance legibility. No single corroborator outside all three readings exists.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__localized_practice_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).
:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.18) reflects that jati boundaries in this reading function as information standards for social coordination — they reduce transaction costs in marriage, labor, and ritual exchange. The 3000+ categories are not a bug but a feature: granular boundaries match local ecological, economic, and kinship realities. Suppression is low (0.12) because no central authority enforces compliance; boundary maintenance is distributed and reputational. Theater ratio (0.22) captures the performative aspect of boundary claims (public assertions of purity, status) that exceed functional necessity. Resistance (0.55) is moderate because participants continuously negotiate boundaries — the system expects and absorbs contestation. Accessibility collapse (0.35) is partial: alternatives exist (conversion, migration, reform movements) but carry high social cost.
 *
 * PERSPECTIVAL GAP:
 *   From the local_community_member seat, the constraint appears as a high-friction but functional coordination system they cannot easily exit (identity_locked exit would be the next level down from constrained). From the caste_association_leader seat, it is an agenda they set and maintain. From the marriage_broker seat, it is an information rent they capture. The engine will compute these seat divergences from the structural data. The orthodox_textual_reading and colonial_census_reading would compute substantially higher extractiveness and suppression from their structural positions because they experience the same social field as imposed classification rather than negotiated coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: local_community_members gain coordination value (marriage markets, economic networks, ritual participation); caste_association_leaders gain authority and mediation rents; marriage_brokers gain information rents. No victims declared because this reading sees the arrangement as net-beneficial for participants — costs are coordination overhead, not extraction. The directionality derivation assigns low d to beneficiaries (subsidy), moderate d to participants bearing coordination costs. The excluded observers (orthodox authorities, census heirs) are not coordinated by this constraint — they operate parallel boundary regimes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating marriage, labor, and ritual exchange in fragmented local ecologies) remains live — the coordination function has not atrophied. The arrangement persists because it continues to solve real problems, not because of inertial maintenance. Mandatrophy is not resolved; the constraint remains functionally justified. However, the proliferation of categories may represent a coordination complexity that approaches diminishing returns — an omega question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_frame,
    'Is this constraint a genuine rope-level coordination norm, or does it mask extractive dynamics visible from other readings of the jati_practice_norm kernel?',
    'Comparative structural analysis across the three declared readings (localized_practice_reading, orthodox_textual_reading, colonial_census_reading) measuring extractiveness, suppression, and enforcement patterns from each reading''s structural position.',
    'If extractiveness is substantially higher from the orthodox_textual or colonial_census readings, this reading''s rope classification reflects a partial view that obscures how the same social field operates as extraction for differently positioned agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Commitment-system framing: this reading vs. sibling readings of the same kernel').

omega_variable(
    proliferation_as_coordination_or_fragmentation,
    'Does the empirical proliferation to 3000+ jati categories indicate functional coordination plasticity or systemic fragmentation that undermines collective action?',
    'Longitudinal analysis of collective action outcomes (political mobilization, economic cooperation, conflict resolution) across regions with different proliferation densities and renegotiation rates.',
    'If proliferation enables coordination, it supports the rope reading; if it fragments solidarity and prevents coalition-building, it may function as a latent snare mechanism despite low measured extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proliferation_as_coordination_or_fragmentation, empirical, 'Whether category proliferation serves coordination or undermines it').

omega_variable(
    renegotiation_power_asymmetry,
    'Are local renegotiations genuinely symmetric, or do dominant local actors (landholders, temple authorities, political brokers) steer outcomes while maintaining the appearance of consensus?',
    'Micro-level ethnography of boundary negotiation events tracing whose preferences prevail, what sanctions back deviations, and whether exit options are equally available to all participants.',
    'If renegotiation is systematically asymmetric, the coordination story is cover for localized extraction — the constraint would compute as tangled_rope from the subordinate participants'' seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renegotiation_power_asymmetry, empirical, 'Power symmetry within the purportedly consensual renegotiation process').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__localized_practice_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(jati_tr_t50, jati_practice_norm__localized_practice_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(jati_tr_t100, jati_practice_norm__localized_practice_reading, theater_ratio, 100, 0.21).
narrative_ontology:measurement(jati_tr_t150, jati_practice_norm__localized_practice_reading, theater_ratio, 150, 0.22).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__localized_practice_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(jati_be_t50, jati_practice_norm__localized_practice_reading, base_extractiveness, 50, 0.16).
narrative_ontology:measurement(jati_be_t100, jati_practice_norm__localized_practice_reading, base_extractiveness, 100, 0.17).
narrative_ontology:measurement(jati_be_t150, jati_practice_norm__localized_practice_reading, base_extractiveness, 150, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__localized_practice_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(jati_su_t50, jati_practice_norm__localized_practice_reading, suppression_requirement, 50, 0.11).
narrative_ontology:measurement(jati_su_t100, jati_practice_norm__localized_practice_reading, suppression_requirement, 100, 0.12).
narrative_ontology:measurement(jati_su_t150, jati_practice_norm__localized_practice_reading, suppression_requirement, 150, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, information_standard).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__localized_practice_reading, 0.02).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the single label 'jati' into three structurally distinct constraints. The localized_practice_reading (this story) has low extractiveness (0.18) and functions as rope — genuine coordination. The orthodox_textual_reading imposes a fixed doctrinal framework with high suppression and extractiveness (snare/tangled_rope). The colonial_census_reading reifies categories for administrative legibility, creating extraction through state classification (tangled_rope). They affect each other: the census reading's categories became negotiation anchors for the practice reading; the textual reading's authority claims shape the rhetorical field in which practice negotiations occur.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
