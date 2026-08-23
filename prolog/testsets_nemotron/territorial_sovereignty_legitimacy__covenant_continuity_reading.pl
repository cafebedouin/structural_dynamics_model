% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Covenant-Continuity Legitimacy Reading of Territorial Sovereignty
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the covenant_continuity_reading of the
 *   territorial_sovereignty_legitimacy kernel. The reading grounds Jewish
 *   sovereignty in an ancient divine covenant (biblical promise to
 *   Abraham/Jacob), continuous Jewish presence (including periods of
 *   demographic minority), and a chain of modern international recognition
 *   (Balfour 1917, UN Partition 1947, Israeli establishment 1948). Under this
 *   reading, the 1947 partition is a compromise of a pre-existing right, not
 *   the creation of a new right; post-1967 settlements are framed as return
 *   to covenant territory rather than colonization. The constraint operates
 *   as a tangled_rope: it coordinates Jewish collective self-determination
 *   and provides a stable legal framework recognized internationally
 *   (coordination function), while simultaneously extracting territorial
 *   control, resource access, and political rights from the Palestinian Arab
 *   population through active enforcement (asymmetric extraction). The engine
 *   computes per-seat classifications from the structural data below.
 *
 * KEY AGENTS:
 *   - israeli_state_institutions: Primary agenda_setter (institutional/arbitrage) — sets and enforces the sovereignty framework, collects territorial control and international legitimacy
 *   - jewish_settler_movement: Beneficiary/agenda_setter hybrid (organized/identity_locked) — drives territorial expansion framed as covenant fulfillment, receives land and state support
 *   - diaspora_jewish_institutions: Beneficiary (organized/mobile) — receives symbolic and political connection to sovereign center, mobilizes resources
 *   - palestinian_arab_population: Primary payer (powerless/trapped) — bears displacement, military rule, resource extraction, political exclusion
 *   - palestinian_refugees: Payer (powerless/trapped) — bears permanent exclusion from return, statelessness, camp confinement
 *   - palestinian_citizens_of_israel: Payer/beneficiary hybrid (moderate/constrained) — bears structural discrimination and land expropriation while holding citizenship rights
 *   - international_legal_order: Observer (institutional/analytical) — provides recognition chain that the reading cites as validation
 *   - palestinian_leadership_factions: Excluded (organized/constrained) — would contest the covenant framing but are structurally excluded from the sovereignty framework's internal logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.72).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Covenant-Continuity Legitimacy Reading of Territorial Sovereignty").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'b12e6d76-a80d-43f1-a262-511c6c597bee').
narrative_ontology:cs_kernel_codification('b12e6d76-a80d-43f1-a262-511c6c597bee', fixed_text).
narrative_ontology:cs_authority_grounding('b12e6d76-a80d-43f1-a262-511c6c597bee', lineage).
narrative_ontology:cs_interpretation_layer_present('b12e6d76-a80d-43f1-a262-511c6c597bee').
narrative_ontology:cs_reading_relation('b12e6d76-a80d-43f1-a262-511c6c597bee', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_reading_relation('b12e6d76-a80d-43f1-a262-511c6c597bee', territorial_sovereignty_legitimacy__existential_matrix_reading, influences).
narrative_ontology:cs_axiom('b12e6d76-a80d-43f1-a262-511c6c597bee', foundational, divine_covenant_grants_perpetual_title).
narrative_ontology:cs_axiom_status(divine_covenant_grants_perpetual_title, holdable).
narrative_ontology:cs_axiom_grounding('b12e6d76-a80d-43f1-a262-511c6c597bee', divine_covenant_grants_perpetual_title, theological).
narrative_ontology:cs_axiom('b12e6d76-a80d-43f1-a262-511c6c597bee', foundational, jewish_presence_continuous_despite_exile).
narrative_ontology:cs_axiom_status(jewish_presence_continuous_despite_exile, holdable).
narrative_ontology:cs_axiom_grounding('b12e6d76-a80d-43f1-a262-511c6c597bee', jewish_presence_continuous_despite_exile, empirically_contingent).
narrative_ontology:cs_axiom('b12e6d76-a80d-43f1-a262-511c6c597bee', secondary, international_recognition_chain_validates_preexisting_right).
narrative_ontology:cs_axiom_status(international_recognition_chain_validates_preexisting_right, holdable).
narrative_ontology:cs_axiom_grounding('b12e6d76-a80d-43f1-a262-511c6c597bee', international_recognition_chain_validates_preexisting_right, conventional).
narrative_ontology:cs_reference_frame('b12e6d76-a80d-43f1-a262-511c6c597bee', biblical_covenant_framework).
narrative_ontology:cs_drift_state('b12e6d76-a80d-43f1-a262-511c6c597bee', post_1967_occupation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b12e6d76-a80d-43f1-a262-511c6c597bee', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_settler_movement).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, diaspora_jewish_institutions).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_citizens_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, divine_promise_covenant).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, continuous_jewish_presence_narrative).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_legal_recognition_chain).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the sovereignty framework through legislation, military administration, and diplomatic corps. Collects territorial control, international recognition, tax base, and security architecture. Exit would mean dissolving the state — structurally impossible from within the framework.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Drives territorial expansion into West Bank and East Jerusalem framed as covenant fulfillment. Receives state subsidies, military protection, legal infrastructure, and ideological validation. Cannot exit the covenant identity without dissolving the movement's raison d'être; identity is fused with the territorial claim.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_settler_movement, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_settler_movement, agenda_setter).

% Mobilizes political, financial, and diplomatic resources for the sovereign center. Receives symbolic homeland, insurance against antisemitism, and focal point for collective identity. Can disengage (and some do) without personal catastrophe — exit is mobile, not identity-locked.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, diaspora_jewish_institutions, beneficiary,
    organized, generational, mobile, global).

% Subject to military rule (West Bank/Gaza), discriminatory law (East Jerusalem), or structural exclusion (Israel proper). Bears land expropriation, movement restrictions, resource diversion (water, land), political disenfranchisement. No exit: cannot leave the territory en masse, cannot access full rights within it, cannot dismantle the framework that subordinates them.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arab_population, payer,
    powerless, biographical, trapped, national).

% Permanently excluded from return by the same sovereignty framework that defines Jewish return as right. Confined to camps or host states with limited rights. The covenant reading makes their return structurally impossible — it would negate the Jewish demographic majority that the covenant claim requires. Exit from refugee status requires framework change they cannot effect.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Hold Israeli citizenship with voting rights and formal equality, but bear structural discrimination: land allocation, budget disparities, nation-state law symbolic exclusion, family reunification bans. Benefit from civil rights protections and welfare state access (secondary_role: beneficiary). Exit is constrained: emigration possible but costly; political integration blocked by the framework's Jewish-majority logic.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_citizens_of_israel, beneficiary).

% Provides the recognition chain (Balfour, Mandate, UN Partition, UN membership) that the reading cites as validation. Simultaneously produces competing legal frameworks (occupation law, human rights law, ICJ opinions) that constrain the reading's expansion. Neither collects nor pays; observes and adjudicates.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_legal_order, observer,
    institutional, generational, analytical, universal).

% Would contest the covenant framing and assert competing legitimacy claims (self-determination, return, statehood). Structurally excluded from the sovereignty framework's internal logic — the framework defines them as terrorist/irrelevant rather than negotiating partners. Their exclusion is what the enforcement machinery partially exists to maintain.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_leadership_factions, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__covenant_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, internationally recognized sovereign framework for Jewish collective self-determination, resolving the historical condition of statelessness and persecution. Coordinates immigration, defense, economy, and diplomacy under a single legitimate authority.
% TRANSFER_FUNCTION: Moves territorial control (land, water, borders), political rights (sovereignty, franchise, self-governance), and resources (state revenue, international aid, diplomatic capital) from the Palestinian Arab population to the Jewish-Israeli collective, justified by the covenant-continuity narrative.
% ABSENT_VOICES: The Palestinian refugee population (dispersed, stateless) and the pre-1948 Palestinian urban/rural leadership (destroyed/exiled) are absent. Their voices would assert continuous presence, property rights, and self-determination — but the covenant reading's temporal scope (biblical) and demographic logic (Jewish majority as structural requirement) structurally exclude them from the legitimacy calculus.
% DISAPPEARANCE_RATIONALE: If the covenant-continuity legitimacy claim vanished overnight, the Israeli state would lose its foundational justification for territorial control beyond the 1947 partition lines, the settlement enterprise would lose its ideological engine, the diaspora mobilization structure would lose its focal point, and the Palestinian population would face a transformed political landscape — though not necessarily immediate justice, as power facts on the ground would persist.
% FOUNDING_PROBLEM: Jewish statelessness, persecution, and vulnerability in diaspora — the condition of being a people without a sovereign territory capable of self-defense and self-determination.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Jewish statelessness/persecution) is historically corroborated by non-beneficiary sources (European history, Holocaust documentation, refugee studies). However, the STATUS of that problem (whether it persists such that the covenant-continuity arrangement remains necessary) is contested: Israeli state institutions and diaspora organizations attest it remains live (rising antisemitism, Iran threat); Palestinian and critical Israeli historians attest the founding problem was substantially solved in 1948 and the arrangement now serves expansion, not survival.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint transfers territory, resources, and political rights from Palestinian to Jewish control on a sustained basis. Suppression (0.72) is high because the constraint's persistence depends on military enforcement, legal restrictions on movement and residency, and active prevention of Palestinian return and self-determination. Theater ratio (0.28) is moderate: the coordination function (Jewish self-determination, international legal framework) is real but a growing share of enforcement activity serves territorial expansion beyond 1948/1967 lines. Accessibility collapse (0.65) reflects that the covenant framework makes alternatives (binational state, full Palestinian sovereignty) structurally illegible within its own logic. Resistance (0.78) is high: Palestinian resistance has been continuous, multi-form, and met with escalating suppression. The measurement series (1917-2000) shows rising extraction and suppression as the constraint moved from diplomatic claim to established state with expanding territorial control.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter/beneficiary seats (Israeli institutions, settler movement, diaspora institutions), the constraint appears as genuine coordination: a people returning to its ancestral homeland, achieving recognized sovereignty, building a functioning state. From the payer seats (Palestinian population, refugees, citizens), the same structure appears as enforced extraction: displacement, military rule, land expropriation, political exclusion justified by a theological-historical claim they do not share. The engine computes this divergence from the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state institutions are the primary beneficiary (collect sovereignty, territory, international recognition — d near 0.0). Jewish settler movement is beneficiary with identity_locked exit (d ~ 0.15 — cannot exit the covenant identity without self-dissolution). Diaspora institutions are beneficiaries with mobile exit (d ~ 0.2). Palestinian Arab population are primary payers with trapped exit (d ~ 0.95 — no exit from military rule/exclusion). Palestinian refugees are payers with trapped exit (d ~ 1.0 — permanent exclusion). Palestinian citizens of Israel are payers with constrained exit (d ~ 0.7 — citizenship provides limited protection but structural discrimination persists). International legal order is observer (d = 0.5). Palestinian leadership factions are excluded — their contestation is structurally prevented from entering the constraint's internal logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (Jewish statelessness and persecution) was substantially addressed by 1948 establishment. The covenant continuity reading prevents mandatrophy resolution by extending the founding problem infinitely backward (biblical covenant) and forward (messianic fulfillment), making the arrangement permanently necessary rather than temporarily justified. The theater ratio rising from 0.1 to 0.28 tracks this: coordination function (refuge/sovereignty) achieved, but enforcement expands to serve settlement enterprise. The constraint is not a piton — it has concentrated beneficiaries (state, settlers, diaspora institutions) who actively maintain and expand it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the territorial_sovereignty_legitimacy kernel, distinct from self_determination_reading and existential_matrix_reading?',
    'Cross-reading structural comparison: if epsilon, beneficiary/victim sets, or cs_structure axioms differ irreducibly, readings are distinct constraints per epsilon-invariance principle.',
    'Confirms this reading instantiates a separate constraint with its own classification; prevents conflation of legitimacy claims into a single story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment that this is a kernel reading, not a flat constraint').

omega_variable(
    covenant_historical_verifiability,
    'Can the divine covenant claim be verified or falsified by empirical means, or does it operate as a theological axiom immune to evidentiary challenge?',
    'Epistemic audit of the claim''s role in the constraint''s enforcement: if covenant functions as non-falsifiable justification for territorial control, extraction persists regardless of historical evidence.',
    'If non-falsifiable, the constraint''s coordination function is theological, not historical — pushing toward snare/tangled_rope with theological grounding_type in axioms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_historical_verifiability, conceptual, 'Epistemic status of the foundational covenant claim').

omega_variable(
    demographic_absence_survival,
    'Does the legitimacy claim genuinely survive 19 centuries of demographic minority status, or does it depend on modern demographic engineering (immigration, settlement, displacement) to become operative?',
    'Counterfactual tracing: if the 1948 establishment required demographic transformation that the covenant claim then retroactively legitimates, the constraint''s coordination story is post-hoc.',
    'If legitimacy depends on demographic engineering, the ''survival through absence'' narrative is cover for extraction — strengthens snare classification for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_absence_survival, empirical, 'Whether demographic continuity is cause or effect of the legitimacy claim').

omega_variable(
    partition_compromise_vs_creation,
    'Is the 1947 UN Partition Plan properly understood as a compromise of a pre-existing right, or as the constitutive legal act that created the sovereign right?',
    'Legal genealogy: trace whether the Israeli Declaration of Independence cites the UN resolution as recognition of a pre-existing right or as the source of the right.',
    'If constitutive, the covenant claim is decorative; if recognitive, the covenant does independent normative work. Affects whether the constraint is tangled_rope (genuine coordination + extraction) or snare (coordination is cover).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partition_compromise_vs_creation, conceptual, 'Ontological status of the 1947 partition relative to the covenant claim').

omega_variable(
    settlement_return_vs_colonization,
    'Are post-1967 settlements structurally ''return'' to covenant territory or ''colonization'' of occupied territory under international law?',
    'Compare the constraint''s internal framing (return) with the external legal framing (occupation/colonization) and the lived experience of the displaced Palestinian population.',
    'If colonization, the constraint''s extraction extends beyond 1948 lines and the ''return'' narrative is extraction cover; if return, the coordination function extends to the whole mandate territory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settlement_return_vs_colonization, empirical, 'Structural characterization of settlement enterprise under this reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 1917, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsl_cc_tr_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(tsl_cc_tr_t1947, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1947, 0.15).
narrative_ontology:measurement(tsl_cc_tr_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(tsl_cc_tr_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(tsl_cc_tr_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1993, 0.27).
narrative_ontology:measurement(tsl_cc_tr_t2000, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2000, 0.28).

% Extraction over time
narrative_ontology:measurement(tsl_cc_be_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1917, 0.35).
narrative_ontology:measurement(tsl_cc_be_t1947, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1947, 0.45).
narrative_ontology:measurement(tsl_cc_be_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(tsl_cc_be_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1967, 0.62).
narrative_ontology:measurement(tsl_cc_be_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1993, 0.65).
narrative_ontology:measurement(tsl_cc_be_t2000, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2000, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tsl_cc_su_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1917, 0.3).
narrative_ontology:measurement(tsl_cc_su_t1947, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1947, 0.5).
narrative_ontology:measurement(tsl_cc_su_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(tsl_cc_su_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(tsl_cc_su_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1993, 0.7).
narrative_ontology:measurement(tsl_cc_su_t2000, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2000, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.12).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, jerusalem_status_constraint).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugee_return_constraint).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, west_bank_settlement_enterprise).

% DUAL FORMULATION NOTE:
% This reading decomposes the territorial_sovereignty_legitimacy kernel with self_determination_reading and existential_matrix_reading. The covenant_continuity_reading claims pre-1948 legitimacy (covenant + continuous presence), the self_determination_reading claims post-1917 legitimacy (Arab demographic majority + self-determination principle), the existential_matrix_reading claims legitimacy from existential necessity (zero-sum survival). Each reading has distinct epsilon (0.68 vs ~0.45 vs ~0.75 estimated), distinct victim sets (Palestinians broadly vs Jewish minority vs both populations), and distinct coordination functions. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, institutional, 0.05).
constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, organized, 0.15).
constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, powerless, 0.95).
constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
