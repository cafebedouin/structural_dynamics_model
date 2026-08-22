% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: Endogenous Reinterpretation of Plural Marriage Mandate (1890 Manifesto Reading)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story models the endogenous reinterpretation reading of
 *   the 1890 Manifesto — the position that God genuinely revealed the
 *   temporal suspension of plural marriage to preserve the LDS Church's
 *   salvific mission (temple ordinances, missionary work, institutional
 *   continuity). Under this reading, the Manifesto is not capitulation but
 *   continuing revelation: the same prophetic authority that instituted
 *   plural marriage in the 1840s suspends its practice in 1890 while
 *   retaining its doctrinal status. The constraint coordinates the church
 *   around a new prophetic directive, creating beneficiaries (the
 *   institution, temple-worthy members, missionary enterprise) and victims
 *   (fundamentalist dissidents who maintain the original practice and face
 *   excommunication). The coordination function is real: the church avoids
 *   disincorporation, retains temple access, and continues global expansion.
 *   The extraction is low-moderate: fundamentalists bear costs (exclusion,
 *   property loss, social rupture) but the arrangement primarily solves a
 *   coordination problem (institutional survival under prophetic authority)
 *   rather than extracting for a beneficiary class.
 *
 * KEY AGENTS:
 *   - lds_church_institution: Primary beneficiary (institutional/analytical) — gains survival, temple continuity, missionary legitimacy
 *   - temple_worthy_members: Beneficiary (organized/biographical) — retain covenant access and community standing
 *   - missionary_enterprise: Beneficiary (organized/generational) — gains global legitimacy and legal operating space
 *   - fundamentalist_dissidents: Victim (powerless/trapped) — excommunicated for maintaining suspended practice
 *   - excommunicated_polygamous_families: Victim (powerless/identity_locked) — lose community, temple access, property
 *   - federal_authorities: Observer (institutional/analytical) — monitors compliance, accepts Manifesto as settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.25).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.35).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "Endogenous Reinterpretation of Plural Marriage Mandate (1890 Manifesto Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, '8e8fd3c1-b7ed-42c2-9937-d56d2fe1525f').
narrative_ontology:cs_kernel_codification('8e8fd3c1-b7ed-42c2-9937-d56d2fe1525f', formalized).
narrative_ontology:cs_authority_grounding('8e8fd3c1-b7ed-42c2-9937-d56d2fe1525f', lineage).
narrative_ontology:cs_interpretation_layer_present('8e8fd3c1-b7ed-42c2-9937-d56d2fe1525f').
narrative_ontology:cs_reading_relation('8e8fd3c1-b7ed-42c2-9937-d56d2fe1525f', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e8fd3c1-b7ed-42c2-9937-d56d2fe1525f', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('8e8fd3c1-b7ed-42c2-9937-d56d2fe1525f', foundational, prophetic_continuity_through_revelation).
narrative_ontology:cs_axiom_status(prophetic_continuity_through_revelation, holdable).
narrative_ontology:cs_axiom_grounding('8e8fd3c1-b7ed-42c2-9937-d56d2fe1525f', prophetic_continuity_through_revelation, theological).
narrative_ontology:cs_axiom('8e8fd3c1-b7ed-42c2-9937-d56d2fe1525f', foundational, divine_temporal_suspension_preserves_doctrine).
narrative_ontology:cs_axiom_status(divine_temporal_suspension_preserves_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('8e8fd3c1-b7ed-42c2-9937-d56d2fe1525f', divine_temporal_suspension_preserves_doctrine, theological).
narrative_ontology:cs_reference_frame('8e8fd3c1-b7ed-42c2-9937-d56d2fe1525f', prophetic_authority_unbroken_since_restoration).
narrative_ontology:cs_drift_state('8e8fd3c1-b7ed-42c2-9937-d56d2fe1525f', post_1904_second_manifesto, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('8e8fd3c1-b7ed-42c2-9937-d56d2fe1525f', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_church_institution).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, temple_worthy_members).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, missionary_enterprise).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissidents).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, excommunicated_polygamous_families).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, prophetic_authority_continuity).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, divine_temporal_suspension_doctrine).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, salvific_mission_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto through its prophet-president, suspending plural marriage practice while retaining its doctrinal status. Gained legal recognition, avoided disincorporation, preserved temple operations, and enabled global missionary expansion. Collects the coordination gains: institutional survival, property retention, theological coherence. Can adjust enforcement stringency (excommunication policy, temple recommend standards) and has exit options through doctrinal reinterpretation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_church_institution, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_church_institution, beneficiary).

% Accepted the Manifesto as binding revelation, maintaining temple recommends and full community standing. Retained access to saving ordinances, avoided federal prosecution, and gained social legitimacy. Bear incidental costs (ongoing stigma, fundamentalist family rupture). Exit is constrained: leaving means losing covenant community and temple access; staying requires accepting prophetic authority's reversal.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, temple_worthy_members, beneficiary,
    organized, biographical, constrained, global).

% Gained legal operating space and cultural legitimacy for global proselytizing after the Manifesto removed the primary barrier to state recognition. The coordination function (prophetic authority) directly enables missionary success. Exit is mobile: missionary work could theoretically continue under different institutional auspices, but the prophetic coordination is its distinctive claim.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, missionary_enterprise, beneficiary,
    organized, generational, mobile, global).

% Maintained plural marriage practice as divine requirement, rejecting the Manifesto as binding. Faced excommunication, loss of temple access, community exclusion, and property dispossession. Their identity is fused to the original covenant — exit means repudiating what they believe is an eternal obligation. The constraint extracts from them via exclusion enforced by the same prophetic authority they once followed.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissidents, payer,
    powerless, biographical, identity_locked, local).

% Entire family units excluded for maintaining plural marriage. Lost church community, temple sealing ordinances for children, and often economic support networks. Children face barred temple marriage unless they individually repudiate parents' practice. Exit is structurally trapped: geographic relocation doesn't restore covenant standing; only institutional reconciliation (accepting the Manifesto) restores access.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, excommunicated_polygamous_families, payer,
    powerless, biographical, trapped, local).

% Monitored Manifesto compliance as condition for Utah statehood and church legal recognition. Accepted the 1890 Manifesto and 1904 Second Manifesto as sufficient settlement. Neither beneficiary nor payer in the constraint's internal logic — their coercive power created the existential threat the constraint responds to, but they do not participate in its prophetic coordination mechanism.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, federal_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_church_institution).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the LDS Church around a new prophetic directive that preserves institutional survival, temple ordinances, and missionary legitimacy by suspending the practice (but not doctrine) of plural marriage. Solves the existential threat of federal disincorporation through continuing revelation rather than schism.
% TRANSFER_FUNCTION: Moves covenant standing, temple access, and community membership from fundamentalist dissidents (who lose them via excommunication) to the church institution (which retains them as coordination capital). The church gains survival and legitimacy; fundamentalists bear the cost of exclusion.
% ABSENT_VOICES: Pre-1890 plural marriage practitioners who died before the Manifesto — their covenant expectations were unilaterally modified by subsequent revelation. Women in plural marriages who had no formal voice in either the 1843 revelation instituting the practice or the 1890 Manifesto suspending it. Federal officials who saw the Manifesto as tactical rather than revelatory but lacked standing to contest the church's internal theology.
% DISAPPEARANCE_RATIONALE: If the endogenous reinterpretation constraint vanished overnight, the church would face immediate schism between fundamentalist and mainstream factions, temple recommend standards would collapse, missionary legitimacy would erode, and the legal settlement with federal authorities would be destabilized. The coordinate arrangement (prophetic authority suspending practice) is what holds the current institutional configuration together.
% FOUNDING_PROBLEM: Federal disincorporation legislation (Edmunds-Tucker Act, 1887) threatened to seize church temples, dissolve the corporate entity, and end the church's ability to perform saving ordinances — the core of its salvific mission. The founding problem was existential institutional survival under prophetic authority.
% FOUNDING_PROBLEM_CORROBORATION: Church leadership attests the problem remains live (ongoing religious liberty threats, temple centrality). Fundamentalist dissidents attest the problem was falsely framed — the salvific mission required plural marriage's continuation, not its suspension. Legal historians (outside both parties) corroborate the disincorporation threat was real and immediate in 1890; sociologists of religion note the church's subsequent growth validates the survival strategy. No consensus across the beneficiary/victim divide.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.25) reflects moderate asymmetric costs: fundamentalists lose community and covenant standing, but the church does not financially or politically exploit them — the cost is exclusion from the coordination mechanism itself. Suppression (0.35) reflects active enforcement (excommunication, temple recommend denial) but not total closure: fundamentalists can rejoin by accepting the Manifesto. Theater ratio (0.15) is low because the prophetic narrative is functionally operative — it genuinely coordinates behavior, not merely performs compliance. Accessibility collapse (0.45) is moderate: alternatives (fundamentalist schism, secular exit) exist but carry high identity costs. Resistance (0.40) reflects fundamentalist schism formation and ongoing contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the church institution's seat (agenda_setter, institutional, arbitrage exit), this is pure coordination: prophetic authority solves an existential threat. From fundamentalist seats (payer, powerless, identity_locked), it is extraction: their covenant commitment is declared obsolete by the same authority that demanded it. From temple-worthy members (beneficiary, organized, constrained), it is coordination with incidental cost (social stigma reduction). The engine computes per-seat classification from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: lds_church_institution (collects survival, legitimacy, temple continuity), temple_worthy_members (retain covenant access), missionary_enterprise (gains global operating space). Victims declared: fundamentalist_dissidents (bear exclusion costs), excommunicated_polygamous_families (bear rupture costs). Directionality derives from this structure: beneficiaries sit at low d (constraint subsidizes them), victims at high d (constraint extracts via exclusion). Federal authorities are observers — they neither benefit nor pay in the constraint's internal logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal disincorporation threat to temple ordinances and church existence) was live in 1890. By 1940, the problem is contested: the church has achieved legal recognition and global presence, but fundamentalists argue the salvific mission required plural marriage's continuation. The mandate has not atrophied into piton — the prophetic suspension narrative remains theologically operative and coordinates current practice. No mandatrophy resolution declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_legitimacy_ambiguity,
    'Does this reading genuinely represent a divine reinterpretation or does it function as a legitimating narrative for institutional survival?',
    'Comparative analysis of internal deliberation records, contemporary revelatory claims, and the pattern of subsequent doctrinal development. If the suspension narrative coherently integrates into the tradition''s ongoing revelatory logic without ad hoc character, it supports genuine reinterpretation; if it appears only at the moment of existential threat and lacks internal theological precursors, it supports legitimating narrative.',
    'If legitimating narrative, the constraint shifts toward institutional_pragmatism_reading''s structural profile (higher extraction from fundamentalists, coordination function secondary to survival). If genuine reinterpretation, the rope classification holds with low extractiveness grounded in coordination around prophetic authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_legitimacy_ambiguity, conceptual, 'Whether the 1890 Manifesto''s revelatory framing is epistemically genuine or functionally instrumental').

omega_variable(
    kernel_committer_structure,
    'How does this reading''s structural relationship to the plural_marriage_mandate kernel differ from its sibling readings?',
    'Structural comparison of the three readings'' beneficiary/victim sets, claimed_type assignments, and cs_structure axioms. The endogenous reading claims rope with church-as-beneficiary; exogenous_override claims snare with federal-coercion-as-extractor; institutional_pragmatism claims tangled_rope with survival-as-extraction.',
    'The kernel''s constraint family structure determines whether readings are genuinely competing frameworks (coexists_with) or whether one forecloses another. This reading coexists_with both siblings — each represents a live interpretive position in contemporary discourse — but influences the exogenous reading by providing the legitimating narrative that the exogenous reading must contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Commitment-system structural positioning of this reading within the plural_marriage_mandate kernel family').

omega_variable(
    fundamentalist_victimhood_boundary,
    'Are fundamentalist dissidents genuine victims of extraction or voluntary exiters from a covenant community they chose to leave?',
    'Analysis of excommunication records, property disposition, and whether the church provided exit pathways (temple recommends, missionary service, community standing) that fundamentalists refused. If the church maintained full membership standing for those accepting the Manifesto while fundamentalists faced exclusion only upon public rejection, victimhood is contested.',
    'If fundamentalists are voluntary exiters, the victim set shrinks and the constraint''s extractiveness decreases (rope purity increases). If they are structural victims denied exit without covenant violation, extractiveness rises toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamentalist_victimhood_boundary, empirical, 'Whether fundamentalist exclusion constitutes extraction or covenant enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 1890, 1940).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_tr_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.05).
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_tr_t1900, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_tr_t1910, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.12).
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_tr_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_tr_t1930, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1930, 0.15).
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_tr_t1940, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1940, 0.15).

% Extraction over time
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_be_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.15).
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_be_t1900, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_be_t1910, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.22).
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_be_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.25).
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_be_t1930, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1930, 0.25).
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_be_t1940, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1940, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_su_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.4).
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_su_t1900, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_su_t1910, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.35).
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_su_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.35).
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_su_t1930, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1930, 0.35).
narrative_ontology:measurement(plural_marriage_mandate__endogenous_reinterpretation_reading_su_t1940, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1940, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.08).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% Plural marriage mandate kernel family: three readings decompose the 1890 Manifesto's structural ambiguity. This reading (endogenous_reinterpretation) claims rope with church-as-beneficiary via prophetic continuity. exogenous_override claims snare with federal-coercion-as-extractor. institutional_pragmatism claims tangled_rope with survival-as-extraction. All three share the same referent (the 1890 Manifesto and its enforcement) but instantiate different constraints with different ε, beneficiary/victim structures, and cs_structure axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
