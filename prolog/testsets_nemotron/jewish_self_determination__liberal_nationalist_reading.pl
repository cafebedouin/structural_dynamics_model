% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: jewish_self_determination__liberal_nationalist_reading
 *   human_readable: Jewish Self-Determination (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   The liberal nationalist reading asserts that Jewish people constitute a
 *   nation like any other, entitled to the same right of self-determination
 *   that international law grants to all peoples. This reading grounds the
 *   legitimacy of Zionism and the State of Israel in the universal principle
 *   of national self-determination (Woodrow Wilson, UN Charter, ICCPR Article
 *   1) rather than in religious covenant, indigenous return, or colonial
 *   settlement. It frames the Israel-Palestine conflict as a clash of two
 *   legitimate national claims on the same territory, resolvable through
 *   partition and mutual recognition — the 'two states for two peoples'
 *   formula. The reading was dominant in Western liberal discourse from 1947
 *   through the Oslo period but has faced mounting challenge from the
 *   settler_colonial_reading (which denies the symmetry of claims) and the
 *   indigenous_return_reading (which asserts Jewish priority). The constraint
 *   operates as a coordination mechanism: it translates a universal principle
 *   into a specific territorial settlement, requiring both parties to
 *   recognize the other's equal claim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.28).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.35).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Jewish Self-Determination (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, 'd4b9fdf4-d0c1-4207-9a89-318c2e44d78e').
narrative_ontology:cs_kernel_codification('d4b9fdf4-d0c1-4207-9a89-318c2e44d78e', formalized).
narrative_ontology:cs_authority_grounding('d4b9fdf4-d0c1-4207-9a89-318c2e44d78e', lineage).
narrative_ontology:cs_interpretation_layer_present('d4b9fdf4-d0c1-4207-9a89-318c2e44d78e').
narrative_ontology:cs_reading_relation('d4b9fdf4-d0c1-4207-9a89-318c2e44d78e', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4b9fdf4-d0c1-4207-9a89-318c2e44d78e', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4b9fdf4-d0c1-4207-9a89-318c2e44d78e', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4b9fdf4-d0c1-4207-9a89-318c2e44d78e', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_axiom('d4b9fdf4-d0c1-4207-9a89-318c2e44d78e', foundational, jewish_nation_equal_standing).
narrative_ontology:cs_axiom_status(jewish_nation_equal_standing, holdable).
narrative_ontology:cs_axiom_grounding('d4b9fdf4-d0c1-4207-9a89-318c2e44d78e', jewish_nation_equal_standing, conventional).
narrative_ontology:cs_axiom('d4b9fdf4-d0c1-4207-9a89-318c2e44d78e', foundational, partition_as_just_resolution).
narrative_ontology:cs_axiom_status(partition_as_just_resolution, holdable).
narrative_ontology:cs_axiom_grounding('d4b9fdf4-d0c1-4207-9a89-318c2e44d78e', partition_as_just_resolution, conventional).
narrative_ontology:cs_reference_frame('d4b9fdf4-d0c1-4207-9a89-318c2e44d78e', wilsonian_self_determination_postwar_order).
narrative_ontology:cs_drift_state('d4b9fdf4-d0c1-4207-9a89-318c2e44d78e', post_oslo_failure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d4b9fdf4-d0c1-4207-9a89-318c2e44d78e', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, israeli_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, zionist_institutions).
narrative_ontology:constraint_victim(jewish_self_determination__liberal_nationalist_reading, israeli_citizens).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, national_self_determination_principle).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, liberal_partition_framework).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, mutual_recognition_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Global Jewish communities seeking refuge from historical persecution and a sovereign framework for collective security. The constraint provides the normative basis for a Jewish state as a guarantor of physical safety and cultural continuity. Exit from this framework means abandoning the sovereign security guarantee; constrained by historical trauma and the empirical failure of minority-rights protections in multiple societies.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora, beneficiary,
    organized, generational, constrained, global).

% Citizens of the realized sovereign state who benefit from self-determination but bear the costs of maintaining it — military service, economic burden of defense, demographic tensions, and the moral weight of governing another people under occupation. Their exit is constrained by citizenship, family, and the absence of alternative sovereign frameworks for Jewish collective life.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, israeli_citizens, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, israeli_citizens, payer).

% The people whose competing national claim occupies the same territory. Under this reading's logic, their self-determination is to be realized through partition and mutual recognition, but the reading's historical operation has often treated their claim as secondary or negotiable while the Jewish claim is treated as primary and non-negotiable. They are structurally excluded from the constraint's beneficiary set despite being the primary counterparty to the territorial coordination problem.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_nationals, excluded,
    powerless, generational, trapped, national).

% The post-1945 institutional framework (UN, international law, human rights regime) that enshrines self-determination as a universal principle and provided the legal basis for partition (UNGA 181). It administers the constraint by legitimating the Jewish claim within a universalist framework while simultaneously demanding Palestinian rights. Its authority derives from the liberal nationalist premise that all nations have equal claim.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, liberal_international_order, agenda_setter,
    institutional, generational, analytical, global).

% The pre-state and post-state organizational apparatus (Jewish Agency, WZO, Israeli government) that translates the normative claim into political reality. They benefit from the constraint's legitimation but also actively shape its interpretation. Their exit options are high — they could pivot to other framings (religious, security, indigenous) — making them arbitrage-grade actors relative to this specific reading.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, zionist_institutions, agenda_setter,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, zionist_institutions, beneficiary).

% Scholars and activists who analyze the constraint through the settler_colonial_reading. They do not participate in the constraint's operation but contest its legitimacy from outside. Their analytical seat sees the rope's coordination function as masking an extraction dynamic; they are not beneficiaries, payers, or excluded in the operational sense — they are the external falsification pressure.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, postcolonial_critics, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the problem of statelessness for a historically persecuted people by legitimating a sovereign state within a universal framework of national self-determination, while proposing partition as the mechanism for accommodating a competing national claim on the same territory.
% TRANSFER_FUNCTION: Transfers political legitimacy, international recognition, and territorial sovereignty from the imperial/colonial order (British Mandate) to a Jewish national movement, conditional on accepting partition and mutual recognition. The transfer is not extractive in principle — it claims to create two sovereign equals — but the historical sequence transferred effective control to one side while the other side's transfer remained incomplete.
% ABSENT_VOICES: Palestinian nationals are the primary absent voice — they were not party to the UNGA 181 partition vote, their leadership rejected the framework, and subsequent negotiations have rarely treated their claim as equal in practice. Refugee populations from 1948 and 1967 are structurally excluded from the constraint's benefit calculus. Mizrahi Jewish communities whose displacement from Arab countries paralleled Palestinian displacement are often marginalized within the Jewish beneficiary category.
% DISAPPEARANCE_RATIONALE: If the liberal nationalist reading vanished overnight, the legal and moral basis for a Jewish state grounded in universal self-determination would collapse. The Israeli state would lose its primary legitimation in international law; the partition framework would lose its normative anchor; Palestinian claims would shift from 'competing national claim' to 'sole legitimate claim' in the dominant discourse. The world would rearrange — but toward what is contested (one state, two states, binational state, ongoing conflict).
% FOUNDING_PROBLEM: The historical condition of Jewish statelessness and vulnerability to persecution, pogroms, and genocide across two millennia of diaspora existence, culminating in the Holocaust, which demonstrated that minority rights protections within other nations' states were insufficient for collective survival.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's historical reality (antisemitism, statelessness, Holocaust) is corroborated by universal historical consensus outside the beneficiary set. What is contested is whether the founding problem *persists* in a way that requires this specific constraint: Israeli historians (Benny Morris, Tom Segev) and Palestinian scholars (Rashid Khalidi, Edward Said) agree on the historical facts but disagree on whether the founding problem justifies the ongoing arrangement. Human rights organizations (Amnesty, HRW) attest the founding problem is real but argue the constraint's current operation exceeds its justification.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jewish_self_determination__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__liberal_nationalist_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low-to-moderate (0.28) because the reading's *principle* is non-extractive — equal claim, partition, mutual recognition — but its *historical operation* has extracted territorial control and political autonomy from Palestinians while the Jewish claim was realized. The gap between principle and practice drives the measurement. Suppression (0.35) reflects that the constraint requires active enforcement (military occupation, settlement expansion, legal asymmetry) to maintain the territorial reality that the partition principle envisioned as temporary. Theater ratio (0.22) captures the growing gap between the reading's declared commitment to two-state symmetry and the on-ground reality of asymmetric control. Accessibility collapse (0.38) is moderate: alternatives (binational state, confederation, one-state democracy) exist conceptually but are politically collapsed by the constraint's institutionalization. Resistance (0.52) is high: the constraint faces sustained resistance from the excluded Palestinian national movement, from the settler_colonial_reading in global discourse, and from the religious_covenant_reading within Israeli politics which rejects partition as illegitimate.
 *
 * PERSPECTIVAL GAP:
 *   From the Jewish diaspora seat: the constraint is a genuine rope — a coordination solution to statelessness that benefits all parties through partition. From the Palestinian national seat: the same constraint operates as a snare — the coordination story is cover for a realized transfer of their land and sovereignty. From the Israeli citizen seat: it is a tangled rope — real coordination benefit (sovereignty, security) mixed with real extraction cost (occupation, moral injury, demographic crisis). From the liberal international order seat: it is a scaffold — a transitional framework meant to produce two states, now stalled. The engine computes these divergences from the structural data; the reading's claim of 'rope' only holds from the beneficiary seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora and Israeli citizens are beneficiaries (d ~ 0.2) — the constraint provides the normative architecture for their collective security and sovereignty. Palestinian nationals are structurally excluded from the beneficiary set despite being the necessary counterparty; they bear the costs of the constraint's asymmetric realization (d ~ 0.8) but the reading's logic does not name them as victims — it names them as equal claimants whose claim has not yet been realized. This is the reading's central tension: it *claims* no victims (victims array empty) but its historical operation produces them. Zionist institutions are agenda_setters with arbitrage exit — they can switch to indigenous_return or religious_covenant framings if this reading loses traction. The liberal international order is the meta-agenda_setter that enshrines the universal principle this reading invokes. Postcolonial critics are observers who see the extraction the reading denies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish statelessness and vulnerability) is historically real and was the constraint's genuine justification. The constraint *resolved* that problem for the Jewish people (state established, recognized, defensible). Mandatrophy occurs because the constraint persists *beyond* its founding resolution — the Jewish state exists and is secure, but the constraint continues to operate as if the founding problem requires ongoing territorial expansion, settlement, and denial of Palestinian sovereignty. The reading's logic (partition, mutual recognition) has been hollowed out by its own institutionalization; what remains is the performance of 'peace process' while the territorial facts change. The mandate has atrophied into a piton-like inertia, but the reading itself still claims the rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_feasibility,
    'Is territorial partition between two sovereign states still physically and demographically feasible on the land between the river and the sea?',
    'Demographic and geographic analysis of settlement blocs, Palestinian urban contiguity, Jerusalem''s status, and resource allocation (water, aquifers) under proposed partition maps.',
    'If partition is no longer feasible, the reading''s coordination function collapses — the rope becomes a snare (extraction without coordination) or a piton (inertial performance). The reading''s claimed_type would be falsified by structural reality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partition_feasibility, empirical, 'Whether the reading''s proposed coordination mechanism (two-state partition) remains physically possible.').

omega_variable(
    mutual_recognition_asymmetry,
    'Does the reading''s historical operation require Palestinian recognition of Israel''s legitimacy as a precondition, while treating Israeli recognition of Palestinian statehood as a negotiable outcome?',
    'Comparative analysis of negotiation frameworks (Camp David, Oslo, Annapolis, Trump plan): which side''s recognition is treated as prior condition vs. final status issue.',
    'If recognition is structurally asymmetric, the reading''s ''equal claim'' principle is a performative cover for a hierarchy — the constraint would reclassify from rope to tangled_rope (coordination + asymmetric extraction) or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mutual_recognition_asymmetry, empirical, 'Whether the reading''s operational logic treats the two national claims symmetrically or asymmetrically.').

omega_variable(
    liberal_universalism_vs_particularist_exception,
    'Does the reading''s invocation of universal self-determination contain a particularist exception for the Jewish claim (e.g., ''Jews are a nation *uniquely* entitled to a state because of historical persecution'') that undermines the universal principle it invokes?',
    'Discourse analysis of key texts (Herzl, Ben-Gurion, Weizmann, contemporary liberal Zionist theorists) for exceptionalist arguments that distinguish the Jewish claim from other national claims.',
    'If the reading relies on exceptionalism, its coordination function is compromised — it cannot genuinely coordinate competing claims if one claim is structurally privileged. The constraint would be a false rope masking a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liberal_universalism_vs_particularist_exception, conceptual, 'Whether the reading''s universalist framing is structurally consistent or contains a particularist exception.').

omega_variable(
    committer_structure_reading_relations,
    'What are the structural relationships between this reading and its sibling readings of the jewish_self_determination kernel?',
    'Structural analysis of each reading''s core premises: which premises logically contradict, which coexist, which create downstream pressure.',
    'Determines the kernel''s constraint family topology and whether this reading forecloses, coexists with, or influences its siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_reading_relations, conceptual, 'Commitment-system framing: structural relations to sibling readings indigenous_return_reading, religious_covenant_reading, diasporist_reading, settler_colonial_reading.').

omega_variable(
    founding_problem_persistence,
    'Does the founding problem (Jewish vulnerability to persecution) persist in a form that *requires* this specific constraint (a Jewish state in historic Palestine), or has it been resolved such that the constraint''s continuation is mandatrophic?',
    'Empirical assessment of contemporary antisemitism levels, Jewish security in diaspora, and whether a Jewish state *as currently configured* (with occupation, settlement, demographic crisis) enhances or diminishes Jewish collective security.',
    'If the founding problem is resolved but the constraint persists and expands, mandatrophy is confirmed. If the founding problem persists *and* requires this constraint, the reading''s justification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, preference, 'Whether the constraint''s founding justification remains live or has atrophied into inertial persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 1897, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsd_lnr_tr_t1897, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1897, 0.1).
narrative_ontology:measurement(jsd_lnr_tr_t1917, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(jsd_lnr_tr_t1947, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1947, 0.2).
narrative_ontology:measurement(jsd_lnr_tr_t1967, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1967, 0.35).
narrative_ontology:measurement(jsd_lnr_tr_t1993, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(jsd_lnr_tr_t2000, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(jsd_lnr_tr_t2024, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(jsd_lnr_be_t1897, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1897, 0.15).
narrative_ontology:measurement(jsd_lnr_be_t1917, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1917, 0.2).
narrative_ontology:measurement(jsd_lnr_be_t1947, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1947, 0.25).
narrative_ontology:measurement(jsd_lnr_be_t1967, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(jsd_lnr_be_t1993, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1993, 0.3).
narrative_ontology:measurement(jsd_lnr_be_t2000, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(jsd_lnr_be_t2024, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(jsd_lnr_su_t1897, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1897, 0.1).
narrative_ontology:measurement(jsd_lnr_su_t1917, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1917, 0.2).
narrative_ontology:measurement(jsd_lnr_su_t1947, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1947, 0.4).
narrative_ontology:measurement(jsd_lnr_su_t1967, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1967, 0.6).
narrative_ontology:measurement(jsd_lnr_su_t1993, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1993, 0.45).
narrative_ontology:measurement(jsd_lnr_su_t2000, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(jsd_lnr_su_t2024, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__liberal_nationalist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, palestinian_self_determination__nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, oslo_accords_framework).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, international_law_self_determination).

% DUAL FORMULATION NOTE:
% This reading and the settler_colonial_reading are dual formulations of the same territorial-historical reality: one frames it as symmetrical national claims (rope), the other as asymmetrical colonial displacement (snare). The indigenous_return_reading and religious_covenant_reading are dual formulations from the Jewish side: one secularizes the indigenous claim, the other sacralizes it. The diasporist_reading is a negation of the kernel's premise rather than an alternative reading. All five stories form the jewish_self_determination constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__liberal_nationalist_reading, powerless, 0.85).
constraint_indexing:directionality_override(jewish_self_determination__liberal_nationalist_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
