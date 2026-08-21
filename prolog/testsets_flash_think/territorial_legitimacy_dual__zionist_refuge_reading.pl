% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__zionist_refuge_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Israel's Territorial Legitimacy (Zionist Refuge Reading)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'Zionist Refuge' reading of
 *   Israel's territorial legitimacy. From this perspective, Israel's
 *   existence is fundamentally justified by the historical persecution of
 *   Jewish people, a divine promise (for some), and the international
 *   recognition provided by the UN partition plan. The 1948 establishment is
 *   seen as an uncontested act of self-determination, while subsequent
 *   territorial control, particularly post-1967, is justified by security
 *   concerns. Palestinian displacement is framed as a consequence of Arab
 *   rejection of partition. The constraint operates as a Tangled Rope,
 *   providing a homeland for one group while actively enforcing control that
 *   extracts from and suppresses another.
 *
 * KEY AGENTS:
 *   - israeli_citizens: Primary beneficiary/payer (organized/identity_locked) — benefits from state, bears security costs.
 *   - zionist_movement: Agenda setter (institutional/analytical) — defines and enforces the state's foundational principles.
 *   - palestinian_refugees: Primary target/excluded (powerless/trapped) — bears costs of displacement, denied return.
 *   - palestinian_citizens_of_israel: Target/payer (moderate/constrained) — bears costs of discrimination, limited exit.
 *   - un_security_council: Observer/historical agenda setter (institutional/analytical) — provides historical legitimacy, limited current enforcement.
 *   - arab_states: Payer/excluded (institutional/constrained) — bears costs of conflict, historical rejection frames Palestinian displacement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.65).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.8).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Israel's Territorial Legitimacy (Zionist Refuge Reading)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, 'b71afaac-fc19-4326-bd98-b9fd213359bb').
narrative_ontology:cs_kernel_codification('b71afaac-fc19-4326-bd98-b9fd213359bb', fixed_text).
narrative_ontology:cs_authority_grounding('b71afaac-fc19-4326-bd98-b9fd213359bb', lineage).
narrative_ontology:cs_interpretation_layer_present('b71afaac-fc19-4326-bd98-b9fd213359bb').
narrative_ontology:cs_reading_relation('b71afaac-fc19-4326-bd98-b9fd213359bb', territorial_legitimacy_dual__palestinian_autochthony_reading, forecloses).
narrative_ontology:cs_reading_relation('b71afaac-fc19-4326-bd98-b9fd213359bb', territorial_legitimacy_dual__two_state_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('b71afaac-fc19-4326-bd98-b9fd213359bb', foundational, jewish_self_determination_right).
narrative_ontology:cs_axiom_status(jewish_self_determination_right, holdable).
narrative_ontology:cs_axiom_grounding('b71afaac-fc19-4326-bd98-b9fd213359bb', jewish_self_determination_right, deontological).
narrative_ontology:cs_axiom('b71afaac-fc19-4326-bd98-b9fd213359bb', foundational, un_partition_legitimacy).
narrative_ontology:cs_axiom_status(un_partition_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b71afaac-fc19-4326-bd98-b9fd213359bb', un_partition_legitimacy, conventional).
narrative_ontology:cs_axiom('b71afaac-fc19-4326-bd98-b9fd213359bb', secondary, security_imperative_for_survival).
narrative_ontology:cs_axiom_status(security_imperative_for_survival, holdable).
narrative_ontology:cs_axiom_grounding('b71afaac-fc19-4326-bd98-b9fd213359bb', security_imperative_for_survival, instrumental).
narrative_ontology:cs_reference_frame('b71afaac-fc19-4326-bd98-b9fd213359bb', post_1948_sovereignty).
narrative_ontology:cs_drift_state('b71afaac-fc19-4326-bd98-b9fd213359bb', contemporary_security_challenges, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b71afaac-fc19-4326-bd98-b9fd213359bb', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_citizens).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, zionist_movement).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_citizens_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, israeli_citizens).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, arab_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a secure homeland and self-determination, but bear the costs of ongoing conflict and military service. For many, their identity is deeply intertwined with the existence and security of the state, making exit unthinkable.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_citizens, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, israeli_citizens, payer).

% Advocates for and actively shapes policies that ensure the security and Jewish character of the state, drawing on historical and religious narratives. They are the primary architects and enforcers of the constraint's underlying principles.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, zionist_movement, agenda_setter,
    institutional, civilizational, analytical, global).

% Bear the costs of displacement and denied right of return, living in refugee camps or diaspora. Their claims to land and property are largely unrecognized by this reading, and their voices are systematically excluded from the decision-making processes that affect their future.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees, excluded).

% Hold Israeli citizenship but often face systemic discrimination and limitations on their rights and access to resources compared to Jewish citizens. They are integrated into the state but experience its foundational principles as extractive.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).

% Historically sanctioned the partition plan that led to Israel's creation. Continues to monitor the conflict and pass resolutions, but its enforcement power is limited by geopolitical dynamics. From this reading, its 1947 resolution is a key legitimizing factor.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, un_security_council, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, un_security_council, agenda_setter).

% Historically rejected the UN partition plan and engaged in conflicts with Israel, leading to further displacement and territorial changes. They bear the political and social costs of the ongoing conflict, and their historical rejection is used by this reading to frame Palestinian displacement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, arab_states, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, arab_states, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__zionist_refuge_reading, israeli_citizens).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__zionist_refuge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a secure and recognized homeland for the Jewish people, ensuring their self-determination and refuge from historical persecution.
% TRANSFER_FUNCTION: Transfers sovereignty, land, and resources to the State of Israel, primarily from the indigenous Palestinian population, justified by historical claims, divine promise, and international recognition (UN partition).
% ABSENT_VOICES: Palestinian voices advocating for the right of return, a single secular state, or full equality within a binational framework are largely absent from the dominant discourse that defines this constraint's legitimacy. They are structurally excluded from the political processes that would challenge the foundational premises of this reading.
% DISAPPEARANCE_RATIONALE: If the foundational claims of Israel's legitimacy (historical persecution, divine promise, UN partition) vanished, the state's very existence and its territorial control would be fundamentally undermined, leading to a complete reorganization of political and demographic realities in the region.
% FOUNDING_PROBLEM: The historical persecution and statelessness of the Jewish people, culminating in the Holocaust, necessitated a secure national home where Jewish self-determination could be realized.
% FOUNDING_PROBLEM_CORROBORATION: The problem of Jewish statelessness and the need for a secure homeland is corroborated by historical records of antisemitism, the Holocaust, and the ongoing need for a refuge. This is attested by Jewish historical organizations, international human rights bodies (regarding historical persecution), and the UN's original partition resolution, from outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) reflects the ongoing transfer of land and resources, and the denial of rights to Palestinians, which is a direct consequence of maintaining the state's territorial control as defined by this reading. Suppression (0.80) is high due to the active military and administrative enforcement required to maintain borders, control populations in contested territories, and manage dissent. The theater ratio (0.20) is relatively low because the security concerns are genuinely felt and acted upon, even if their application is contested by other readings. Accessibility collapse (0.85) is high because this reading largely forecloses alternatives like a single, secular state, viewing them as existential threats. Resistance (0.75) is high due to the ongoing conflict and opposition from Palestinian and Arab actors.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Israeli citizens and the Zionist movement, the constraint is a necessary and legitimate structure for self-preservation and self-determination. From the perspective of Palestinian refugees and citizens, it is an actively enforced, extractive structure that denies their rights and claims. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli citizens and the Zionist movement are beneficiaries, as the constraint directly provides them with a homeland and self-determination (low d). Palestinian refugees and citizens are targets, bearing the costs of displacement, denied rights, and discrimination (high d). The UN Security Council is an observer, having historically contributed to the constraint's legitimacy. Arab states are payers, bearing the costs of conflict and having their historical rejection used to justify Palestinian displacement.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'Rope' (ignoring extraction) or a pure 'Snare' (ignoring the genuine coordination function of providing refuge). The 'Tangled Rope' classification acknowledges both the coordination for one group and the asymmetric extraction and active enforcement against another, which is crucial for understanding its persistence and contestation. The 'founding_problem_status' being 'live' from this reading's perspective, despite contestation, highlights the ongoing perceived necessity of the constraint for its beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_promise_empirical_status,
    'To what extent does the ''divine promise'' claim contribute to the constraint''s persistence, and is it empirically verifiable or purely theological?',
    'Sociological studies of belief systems and their impact on political action; theological analysis of scriptural interpretation. Empirical data cannot ''resolve'' a theological claim, but can measure its social force.',
    'If purely theological, its influence is conceptual/preference-based, not empirical. If it drives empirically observable actions, its impact on extractiveness and suppression is real, regardless of its truth status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_promise_empirical_status, conceptual, 'The role and nature of the ''divine promise'' in legitimizing territorial claims.').

omega_variable(
    un_partition_interpretation_ambiguity,
    'Is the UN partition resolution (1947) interpreted as a definitive and immutable grant of legitimacy, or as a historical starting point subject to evolving international law and human rights principles?',
    'Legal analysis of international law precedents, historical review of UN resolutions, and diplomatic negotiations. The resolution itself is a fixed text, but its interpretive force is dynamic.',
    'If immutable, it strongly supports the ''uncontested 1948 legitimacy'' claim, dampening perceived extraction. If evolving, it opens avenues for challenging current territorial arrangements and could increase perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(un_partition_interpretation_ambiguity, conceptual, 'The interpretive force and immutability of the UN partition resolution.').

omega_variable(
    displacement_causality_attribution,
    'Is Palestinian displacement primarily a consequence of Arab rejection of partition, or a direct result of Zionist military actions and policies?',
    'Historical research, archival analysis, and testimony from multiple perspectives. This is an empirical question with significant conceptual and political implications.',
    'If primarily due to Arab rejection, it reinforces the ''Zionist Refuge'' reading''s justification for territorial control and dampens perceived extraction. If primarily due to Zionist actions, it significantly increases the perceived extractiveness and suppression of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_causality_attribution, empirical, 'Attribution of causality for Palestinian displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1987, 0.2).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1987, 0.65).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1987, 0.8).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'territorial_legitimacy_dual' kernel. It focuses on Israel's legitimacy grounded in historical persecution, divine promise, and UN partition acceptance. Sibling readings (palestinian_autochthony_reading, two_state_coexistence_reading) offer alternative framings of legitimacy and territorial claims, leading to different structural classifications and metric profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
