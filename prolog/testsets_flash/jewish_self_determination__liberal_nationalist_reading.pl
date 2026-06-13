% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__liberal_nationalist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_self_determination__liberal_nationalist_reading
 *   human_readable: Jewish Self-Determination (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'liberal nationalist' reading of
 *   Jewish self-determination, which posits that Jewish people constitute a
 *   nation with an equal claim to self-determination as other peoples. It
 *   frames the issue as a coordination problem among competing national
 *   claims, ideally resolved through territorial partition and mutual
 *   recognition. The constraint is claimed as a Rope, reflecting its intended
 *   function as a coordination mechanism for national rights. However, its
 *   implementation has led to moderate extractiveness and suppression,
 *   particularly for Palestinian populations, as the ideal of equitable
 *   partition remains largely unrealized.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.3).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.4).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Jewish Self-Determination (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, 'df870051-a068-4c12-8079-3b705f5b23b5').
narrative_ontology:cs_kernel_codification('df870051-a068-4c12-8079-3b705f5b23b5', formalized).
narrative_ontology:cs_authority_grounding('df870051-a068-4c12-8079-3b705f5b23b5', lineage).
narrative_ontology:cs_interpretation_layer_present('df870051-a068-4c12-8079-3b705f5b23b5').
narrative_ontology:cs_reading_relation('df870051-a068-4c12-8079-3b705f5b23b5', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('df870051-a068-4c12-8079-3b705f5b23b5', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('df870051-a068-4c12-8079-3b705f5b23b5', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('df870051-a068-4c12-8079-3b705f5b23b5', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('df870051-a068-4c12-8079-3b705f5b23b5', foundational, universal_national_self_determination).
narrative_ontology:cs_axiom_status(universal_national_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('df870051-a068-4c12-8079-3b705f5b23b5', universal_national_self_determination, deontological).
narrative_ontology:cs_axiom('df870051-a068-4c12-8079-3b705f5b23b5', foundational, jewish_people_constitute_a_nation).
narrative_ontology:cs_axiom_status(jewish_people_constitute_a_nation, holdable).
narrative_ontology:cs_axiom_grounding('df870051-a068-4c12-8079-3b705f5b23b5', jewish_people_constitute_a_nation, conventional).
narrative_ontology:cs_reference_frame('df870051-a068-4c12-8079-3b705f5b23b5', post_wwii_universal_rights_framework).
narrative_ontology:cs_drift_state('df870051-a068-4c12-8079-3b705f5b23b5', contemporary_postcolonial_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('df870051-a068-4c12-8079-3b705f5b23b5', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_refuge).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_sovereignty_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, international_law_frameworks).
narrative_ontology:constraint_victim(jewish_self_determination__liberal_nationalist_reading, palestinian_people_seeking_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks a secure homeland and self-determination as a response to historical persecution and antisemitism, viewing a sovereign state as essential for collective survival and flourishing. Their claim is rooted in the universal right to national self-determination.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_refuge, beneficiary,
    organized, generational, constrained, global).

% Experience the implementation of Jewish self-determination as a constraint on their own national aspirations and territorial rights. While this reading theoretically allows for co-existence through partition, in practice, it often leads to displacement and denial of their self-determination.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_people_seeking_self_determination, payer,
    powerless, generational, trapped, regional).

% Support the principle of national self-determination for all peoples, including Jewish people, often advocating for a two-state solution or other forms of territorial partition to resolve competing claims. They administer international law and diplomatic efforts to coordinate these claims.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, liberal_democratic_states, agenda_setter,
    institutional, generational, mobile, global).

% Are vindicated by the application of universal principles of national self-determination to the Jewish people, reinforcing the idea that all nations have a right to sovereignty. The framework itself benefits from consistent application of its principles.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, international_law_frameworks, beneficiary,
    institutional, civilizational, analytical, universal).

% Reject the premise that Jewish collective survival requires a sovereign state, advocating instead for diaspora flourishing and universal human rights. They are often excluded from mainstream discussions of Jewish self-determination that center on statehood.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, anti_zionist_jewish_diaspora, excluded,
    moderate, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the universal principle of national self-determination with the specific historical and political context of the Jewish people, aiming to resolve competing national claims through equitable territorial arrangements.
% TRANSFER_FUNCTION: Transfers the right to territorial sovereignty and national recognition to the Jewish people, ideally without denying the same to other peoples, but in practice often leading to a transfer of land and resources from Palestinian populations.
% ABSENT_VOICES: The settler_colonial_reading and diasporist_reading perspectives are often marginalized in liberal nationalist discourse, as they challenge the foundational premises of a Jewish nation-state or its legitimacy. Their objections would center on the inherent extractiveness and exclusionary nature of state-building in a contested territory.
% DISAPPEARANCE_RATIONALE: If the liberal nationalist reading of Jewish self-determination vanished, the international legal and diplomatic frameworks would lose a key justification for the existence of a Jewish state, leading to a fundamental re-evaluation of territorial claims and national rights in the region. The political landscape would be profoundly altered.
% FOUNDING_PROBLEM: The historical statelessness and persecution of Jewish people, culminating in the Holocaust, which demonstrated the urgent need for a secure national home and self-determination.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by historical records, international resolutions (e.g., UN Partition Plan), and ongoing antisemitism, corroborated by numerous historians, international bodies, and human rights organizations outside the direct beneficiaries. While the specific solution (a nation-state) is contested, the historical problem of Jewish vulnerability is widely acknowledged.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_self_determination__liberal_nationalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__liberal_nationalist_reading_tests).
:- end_tests(jewish_self_determination__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.3) is moderate, reflecting the costs borne by those whose self-determination claims conflict with the establishment of a Jewish state, particularly the Palestinian people. Suppression (0.4) is also moderate, as the constraint requires active enforcement to manage competing claims and maintain territorial arrangements. The theater ratio (0.1) is low, as the core claim of national self-determination is genuinely held and acted upon, though its application is highly contested. The metrics reflect the gap between the ideal of a 'Rope' (coordination) and the realities of its implementation in a contested geopolitical context.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Jewish diaspora seeking refuge, this constraint is a vital Rope, providing a solution to historical persecution. From the perspective of Palestinian people, it operates as a Snare, extracting their land and rights. Liberal democratic states view it as a complex but necessary coordination problem. The engine's per-seat classification will reflect these divergences based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora seeking refuge and sovereignty are the primary beneficiaries, as the constraint directly addresses their historical vulnerability and aspiration for self-determination. Palestinian people seeking self-determination are the primary payers, as the implementation of this reading has historically involved displacement and denial of their own national rights. Liberal democratic states act as agenda-setters, promoting and enforcing the principle of national self-determination. International law frameworks are beneficiaries, as the consistent application of the principle reinforces their legitimacy. Anti-Zionist Jewish diaspora are excluded, as their alternative vision of Jewish collective life is outside the framework of national statehood.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (securing Jewish self-determination) is still live, but its implementation has generated significant contestation regarding its impact on other populations. The classification as a Rope (claimed) versus potentially a Snare (from the payer's seat) highlights the tension between the stated coordination function and the observed extractive outcomes. Mandatrophy is not resolved, but the ongoing contestation prevents it from becoming a Piton, as the constraint is actively defended and resisted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    territorial_partition_feasibility,
    'Is equitable territorial partition, which would allow for the self-determination of both Jewish and Palestinian peoples, genuinely feasible under current geopolitical conditions?',
    'Empirical assessment of diplomatic efforts, demographic trends, and political will for a two-state solution or other equitable land-sharing arrangements.',
    'If feasible, the ''rope'' classification is strengthened, as the coordination problem has a viable solution. If infeasible, the constraint leans towards ''tangled_rope'' or ''snare'', as the coordination function fails to resolve competing claims without extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_partition_feasibility, empirical, 'The practical viability of the liberal nationalist solution to competing national claims.').

omega_variable(
    universal_vs_particular_rights,
    'Does the assertion of a universal right to self-determination for Jewish people inherently conflict with the particular historical and indigenous claims of Palestinian people in the same territory?',
    'Conceptual analysis of rights frameworks and historical narratives, potentially informed by legal scholarship on indigenous rights and postcolonial theory.',
    'If an inherent conflict exists, the ''rope'' classification is fundamentally challenged, as the constraint cannot coordinate without extraction. If no inherent conflict, the ''rope'' classification is more robust, with extraction attributed to implementation failures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_vs_particular_rights, conceptual, 'Conceptual tension between universal national rights and particular indigenous claims.').

omega_variable(
    reading_legitimacy_source,
    'Is the liberal nationalist reading''s legitimacy primarily derived from universal human rights principles or from a selective application of those principles to a specific historical context?',
    'Analysis of the discourse and legal arguments used by proponents of this reading, comparing them to the application of self-determination principles in other postcolonial contexts.',
    'If selectively applied, the reading''s authority grounding shifts towards ''extraction'' or ''practice'' rather than ''lineage'' or ''expertise'', potentially reclassifying the constraint as more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_legitimacy_source, conceptual, 'Source of legitimacy for the liberal nationalist reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1993, 0.08).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1948, 0.2).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1993, 0.25).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(jewi_su_t1993, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1993, 0.4).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the 'jewish_self_determination' kernel. Each reading represents a distinct structural claim with its own beneficiaries, victims, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
