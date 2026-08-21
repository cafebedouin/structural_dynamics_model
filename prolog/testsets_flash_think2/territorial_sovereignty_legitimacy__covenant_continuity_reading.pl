% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__covenant_continuity_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Territorial Sovereignty Legitimacy (Covenant & Continuity Reading)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'covenant_continuity_reading' of
 *   territorial sovereignty legitimacy. From this perspective, the legitimacy
 *   of Jewish sovereignty over the land derives from an ancient divine
 *   promise, continuous Jewish presence, and is confirmed (though not
 *   created) by modern international recognition. The claim is presented as a
 *   foundational truth, hence the 'mountain' classification. However, its
 *   application is highly extractive for those with competing claims, leading
 *   to high measured extractiveness and suppression. The claim/metric gap is
 *   deliberate: the constraint is CLAIMED as a mountain by its proponents,
 *   while its operational metrics reflect substantial extraction and active
 *   enforcement against counter-claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.85).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.78).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, mountain).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Territorial Sovereignty Legitimacy (Covenant & Continuity Reading)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).
domain_priors:emerges_naturally(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, '262e4331-f91d-4d4d-a3c5-0d1b86af804e').
narrative_ontology:cs_kernel_codification('262e4331-f91d-4d4d-a3c5-0d1b86af804e', formalized).
narrative_ontology:cs_authority_grounding('262e4331-f91d-4d4d-a3c5-0d1b86af804e', lineage).
narrative_ontology:cs_interpretation_layer_present('262e4331-f91d-4d4d-a3c5-0d1b86af804e').
narrative_ontology:cs_reading_relation('262e4331-f91d-4d4d-a3c5-0d1b86af804e', territorial_sovereignty_legitimacy__self_determination_reading, forecloses).
narrative_ontology:cs_reading_relation('262e4331-f91d-4d4d-a3c5-0d1b86af804e', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('262e4331-f91d-4d4d-a3c5-0d1b86af804e', foundational, divine_promise_of_land).
narrative_ontology:cs_axiom_status(divine_promise_of_land, holdable).
narrative_ontology:cs_axiom_grounding('262e4331-f91d-4d4d-a3c5-0d1b86af804e', divine_promise_of_land, theological).
narrative_ontology:cs_axiom('262e4331-f91d-4d4d-a3c5-0d1b86af804e', foundational, unbroken_historical_connection).
narrative_ontology:cs_axiom_status(unbroken_historical_connection, holdable).
narrative_ontology:cs_axiom_grounding('262e4331-f91d-4d4d-a3c5-0d1b86af804e', unbroken_historical_connection, conventional).
narrative_ontology:cs_reference_frame('262e4331-f91d-4d4d-a3c5-0d1b86af804e', biblical_covenant_and_historical_presence).
narrative_ontology:cs_drift_state('262e4331-f91d-4d4d-a3c5-0d1b86af804e', contemporary_international_law_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('262e4331-f91d-4d4d-a3c5-0d1b86af804e', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_people).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, state_of_israel).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_people).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, arab_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the primary beneficiaries, they derive a sense of historical and religious continuity, collective identity, and the right to self-determination in the land. Their connection is deeply rooted in religious texts and continuous historical presence, making exit from this claim an identity-level challenge.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_people, beneficiary,
    powerful, generational, identity_locked, global).

% The institutional embodiment of the covenant and continuity claim. It actively administers and enforces policies based on this legitimacy, including territorial control and settlement expansion. While powerful, its actions are constrained by international law and diplomatic pressure.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Bear the primary costs of this claim's enforcement, experiencing displacement, loss of land, and denial of self-determination rights. Their historical narrative and presence are largely unacknowledged by this reading, leaving them with severely constrained options and a deep sense of injustice.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_people, payer,
    powerless, generational, trapped, local).

% Experience political and diplomatic costs from the ongoing conflict fueled by competing legitimacy claims. While some have normalized relations, others actively contest the covenant/continuity reading, particularly regarding its implications for Palestinian rights and regional stability.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, arab_states, payer,
    organized, biographical, constrained, regional).

% Observes and attempts to mediate the conflict, often recognizing the State of Israel's existence (post-1948) but frequently contesting the legitimacy of its territorial claims based on covenant/continuity, especially regarding settlements. Their role is to apply international law and diplomatic pressure.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_community, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__covenant_continuity_reading, state_of_israel).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__covenant_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: For its adherents, it coordinates a shared historical narrative, religious identity, and collective claim to the land, providing a foundational basis for national self-determination and statehood.
% TRANSFER_FUNCTION: Transfers legitimacy, territorial control, and resources to the Jewish people and the State of Israel, based on historical and religious claims, from those with competing claims to the same territory.
% ABSENT_VOICES: Indigenous populations whose historical presence predates or is not recognized by the covenant narrative, and those whose claims to self-determination are based solely on modern demographic majorities and continuous residence, are structurally excluded from the foundational discourse of this reading.
% DISAPPEARANCE_RATIONALE: If the legitimacy derived from ancient covenant and continuous presence vanished, the foundational narrative for the State of Israel's existence and its territorial claims would collapse. This would necessitate a complete re-evaluation of sovereignty, identity, and land rights, fundamentally reorganizing the political and social landscape of the region.
% FOUNDING_PROBLEM: To establish and secure a sovereign homeland for the Jewish people, rooted in historical and religious claims, after centuries of diaspora, persecution, and the Holocaust.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as live by the Jewish people and the State of Israel, citing ongoing security threats and the need for a secure homeland. While international bodies recognize the State of Israel's right to exist, the specific grounding of its legitimacy in ancient covenant and continuous presence, particularly as it pertains to territorial claims beyond 1948 borders, is contested by the Palestinian people, Arab states, and significant portions of the international community.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(territorial_sovereignty_legitimacy__covenant_continuity_reading),
    narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the assertion and enforcement of this legitimacy, particularly in contested territories, directly leads to the displacement and denial of rights for the Palestinian people. Suppression is also high (0.78) as the claim requires active military, legal, and political enforcement to maintain against significant resistance and competing narratives. Theater ratio is low (0.15) because the claim is deeply held and genuinely believed by its adherents, not merely performed. Accessibility collapse is high (0.90) for those outside the covenant/continuity narrative, as alternative paths to legitimacy for the land are largely foreclosed by this reading. Resistance is high (0.88) due to the ongoing, active opposition from the Palestinian people and their allies.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries (Jewish people, State of Israel) experience this as a foundational, non-extractive right, a 'mountain' of historical and divine truth. The payers (Palestinian people, Arab states) experience it as a highly extractive and suppressive 'snare' that denies their own claims and rights. The engine's computation will highlight this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people and the State of Israel are the primary beneficiaries, as the constraint directly underpins their collective identity, national project, and territorial control (low d). The Palestinian people and Arab states are the primary targets, bearing the costs of displacement, loss of land, and political marginalization (high d). The international community acts as an observer, attempting to balance competing claims and apply international law, often finding itself in a position of partial opposition to the full extent of the covenant/continuity claim.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (securing a homeland based on historical/religious claims) is considered 'live' by its proponents. The high extractiveness and suppression, despite the 'mountain' claim, indicate a 'false summit' where a foundational claim is used to justify ongoing extraction, rather than a function that has atrophied. The classification prevents mislabeling this as a simple coordination problem or a degraded institution, instead highlighting the active, contested nature of its legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_legitimacy,
    'Is the legitimacy of sovereignty derived from ancient covenant and continuous presence a natural, immutable truth, or a constructed narrative maintained through active enforcement and international recognition?',
    'Analysis of the historical evolution of sovereignty concepts, the role of power in establishing and maintaining claims, and the degree to which the ''divine promise'' is universally accepted versus culturally specific.',
    'If primarily constructed, the ''mountain'' claim is a cover for a highly extractive ''snare'' or ''tangled_rope'', and its persistence depends entirely on the power dynamics and enforcement mechanisms, not inherent truth. If genuinely natural, the extraction is a consequence of resistance to an immutable fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_legitimacy, conceptual, 'Ambiguity regarding the inherent naturalness of the sovereignty claim versus its social and political construction.').

omega_variable(
    demographic_absence_impact,
    'Does the legitimacy claim, as derived from continuous presence, survive periods of significant demographic absence or minority status, or does it require a continuous demographic majority to remain valid?',
    'Comparative historical analysis of other indigenous or diasporic claims to land, and legal interpretations of ''continuous presence'' in international law.',
    'If demographic majority is required, the claim''s historical validity might be weakened for certain periods, potentially altering the perceived ''naturalness'' and increasing the perceived extraction from other populations present during those times. If not, the claim''s resilience is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_absence_impact, empirical, 'The role of demographic continuity in the ''continuous presence'' aspect of the legitimacy claim.').

omega_variable(
    partition_as_compromise_vs_creation,
    'Is the UN Partition Plan and 1948 establishment a compromise of a pre-existing, inherent right, or the creation of a new, internationally recognized right to statehood?',
    'Legal analysis of the status of the Balfour Declaration and League of Nations Mandate, and the legal implications of UN resolutions regarding state formation and self-determination.',
    'If a compromise, the covenant/continuity reading maintains its foundational strength, viewing subsequent territorial limitations as infringements. If a creation, the legitimacy is more contingent on international law and less on inherent historical/divine right, potentially reducing the perceived ''naturalness'' and increasing the ''constructed'' aspect of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_as_compromise_vs_creation, conceptual, 'Interpretation of modern international acts as either confirming or creating sovereignty legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1993, 0.14).
narrative_ontology:measurement(terr_tr_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(terr_tr_t2015, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1967, 0.75).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1993, 0.8).
narrative_ontology:measurement(terr_be_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2005, 0.82).
narrative_ontology:measurement(terr_be_t2015, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2015, 0.84).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1993, 0.75).
narrative_ontology:measurement(terr_su_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2005, 0.76).
narrative_ontology:measurement(terr_su_t2015, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
