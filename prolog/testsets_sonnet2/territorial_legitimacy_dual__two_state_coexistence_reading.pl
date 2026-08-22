% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__two_state_coexistence_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__two_state_coexistence_reading
 *   human_readable: Two-State Coexistence Compromise Framework (1967 Boundaries, Mutual Recognition)
 *   domain: political/territorial sovereignty
 *
 * SUMMARY:
 *   This story instantiates the two-state coexistence reading of the
 *   territorial legitimacy dual kernel: the compromise framework that accepts
 *   1948 legitimacy for both national movements, treats 1967 boundaries as
 *   the reference partition line, redirects Palestinian right of return to
 *   the territory of the future Palestinian state rather than to original
 *   1948 homes, and substitutes bilateral security cooperation for zero-sum
 *   territorial competition. The framework functions as genuine coordination
 *   — it converts an intractable legitimacy contest into a bounded
 *   negotiation both sides' institutional actors can operate within — while
 *   simultaneously requiring active enforcement (donor conditionality,
 *   security cooperation mandates, diplomatic isolation of rejectionists) and
 *   generating identifiable victims among those whose claims the compromise
 *   narrows or defers. Rising theater_ratio and suppression_requirement over
 *   the 1993-2023 interval track the framework's drift from active
 *   negotiation toward indefinite process-maintenance: summits, statements,
 *   and donor conferences continue while the underlying partition has not
 *   been implemented.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.58).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.62).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence Compromise Framework (1967 Boundaries, Mutual Recognition)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, 'ed056db0-2361-4c93-9bbc-a4baaef97d71').
narrative_ontology:cs_kernel_codification('ed056db0-2361-4c93-9bbc-a4baaef97d71', distributed).
narrative_ontology:cs_authority_grounding('ed056db0-2361-4c93-9bbc-a4baaef97d71', distributed).
narrative_ontology:cs_reading_relation('ed056db0-2361-4c93-9bbc-a4baaef97d71', territorial_legitimacy_dual__zionist_refuge_reading, influences).
narrative_ontology:cs_reading_relation('ed056db0-2361-4c93-9bbc-a4baaef97d71', territorial_legitimacy_dual__palestinian_autochthony_reading, influences).
narrative_ontology:cs_axiom('ed056db0-2361-4c93-9bbc-a4baaef97d71', foundational, dual_legitimacy_1948_both_peoples).
narrative_ontology:cs_axiom_status(dual_legitimacy_1948_both_peoples, holdable).
narrative_ontology:cs_axiom_grounding('ed056db0-2361-4c93-9bbc-a4baaef97d71', dual_legitimacy_1948_both_peoples, conventional).
narrative_ontology:cs_axiom('ed056db0-2361-4c93-9bbc-a4baaef97d71', foundational, return_right_bounded_to_partition_territory).
narrative_ontology:cs_axiom_status(return_right_bounded_to_partition_territory, holdable).
narrative_ontology:cs_axiom_grounding('ed056db0-2361-4c93-9bbc-a4baaef97d71', return_right_bounded_to_partition_territory, instrumental).
narrative_ontology:cs_reference_frame('ed056db0-2361-4c93-9bbc-a4baaef97d71', oslo_framework_mutual_recognition).
narrative_ontology:cs_drift_state('ed056db0-2361-4c93-9bbc-a4baaef97d71', post_2023_process_stagnation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ed056db0-2361-4c93-9bbc-a4baaef97d71', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, international_diplomatic_architecture).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, moderate_israeli_security_establishment).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_governing_elite).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, west_bank_settler_communities).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees_outside_mandate_territory).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, gaza_residents_under_blockade).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_governing_elite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and re-sets the negotiating framework (Oslo, Roadmap, Annapolis, Abraham Accords adjuncts) around 1967 lines as the reference partition boundary. Convenes talks, funds the Palestinian Authority's administrative capacity, and issues resolutions treating the framework as the only internationally sanctioned path. Bears none of the territorial or security costs directly; its currency is legitimacy and process continuity.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_diplomatic_architecture, agenda_setter,
    institutional, generational, analytical, global).

% Gains predictable security coordination with a Palestinian Authority that has an institutional stake in suppressing armed resistance within its own territory, and gains international legitimacy for retaining a defensible core within any partition. Can walk away from negotiations without losing sovereign standing; the framework costs it negotiating leverage, not survival.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, moderate_israeli_security_establishment, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, moderate_israeli_security_establishment, agenda_setter).

% Derives its governing legitimacy and donor funding from being the internationally recognized negotiating partner for the 1967-lines state. Collects real institutional power and international standing from the framework, but is also structurally required to police its own population's resistance and absorb the political cost when statehood does not materialize.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_governing_elite, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_governing_elite, payer).

% Built communities, in many cases for generations, on land the framework designates as the future Palestinian state's territory. Under this reading their continued residence is treated as a negotiable variable subject to land swaps or evacuation, not as an established fact — their exit options are relocation within Israel or contesting the framework itself through settlement expansion.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, west_bank_settler_communities, payer,
    organized, biographical, trapped, regional).

% Live in refugee camps and diaspora communities in Lebanon, Jordan, Syria, and beyond, holding claims to homes inside pre-1967 Israel. This reading resolves their right of return by redirecting it exclusively to the new Palestinian state's territory rather than to original family land — a real transfer of what 'return' means, borne entirely by people with no seat in the negotiations that decide it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees_outside_mandate_territory, payer,
    powerless, generational, trapped, regional).

% Live under a blockade regime whose lifting is treated as contingent on security arrangements the framework's security-cooperation clause requires but has never delivered. They bear the compliance costs of a security architecture designed around a state that, from their position, has not yet come into being.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, gaza_residents_under_blockade, payer,
    powerless, immediate, trapped, regional).

% Groups holding maximalist claims on either side — those who reject any partition and those who reject any Jewish sovereign presence — are structurally excluded from a framework whose premise is mutual recognition. They would object that the compromise concedes what their founding narrative holds as non-negotiable, but the framework's legitimacy depends on their continued exclusion from the negotiating table.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, rejectionist_factions_both_sides, excluded,
    organized, generational, trapped, regional).

% Neighboring states and multilateral security bodies that would coordinate with a stabilized two-state security architecture. They monitor and lend conditional support but bear no direct cost if the framework fails to materialize.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, regional_security_partners, observer,
    institutional, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared reference point — the 1967 line — that both national movements can use to convert an otherwise zero-sum territorial and legitimacy contest into a bounded negotiation with a defined solution space, enabling security cooperation, aid flows, and diplomatic recognition to proceed without either side conceding its founding narrative outright.
% TRANSFER_FUNCTION: Moves negotiating leverage and international legitimacy toward institutional actors on both sides who can operate within the compromise (the PA elite, the moderate Israeli security establishment, the mediating powers) and moves the costs of the compromise onto those whose claims the compromise redefines or defers: settlers whose land becomes negotiable, refugees whose return is redirected, and Gazans whose blockade persists pending a security architecture not yet realized.
% ABSENT_VOICES: Rejectionist factions on both sides, and diaspora refugees with no representation in bilateral talks, would object that the framework trades away non-negotiable claims (full sovereignty over the whole land on one side; unconditional right of return to original homes on the other) for a diplomatic architecture that primarily serves the negotiating elites.
% DISAPPEARANCE_RATIONALE: If the two-state coexistence framework were abandoned overnight, the PA's international legitimacy and funding structure would collapse, security coordination arrangements would lapse, settler expansion would lose even nominal constraint, and refugee claims would revert to their maximal, contested form rather than the framework's narrowed 'return to the Palestinian state' formulation — the entire diplomatic and administrative architecture built around the 1967-line reference point would need to be reconstructed or replaced.
% FOUNDING_PROBLEM: After 1967, and especially following the 1993 Oslo process, the problem was to find a negotiable partition line and mutual-recognition formula that could end active war and occupation-administration costs without requiring either national movement to renounce its founding legitimacy claim outright.
% FOUNDING_PROBLEM_CORROBORATION: International mediators and the PA's own institutional leadership attest the framework remains the live, necessary path to resolution. Independent observers — UN human rights rapporteurs, Israeli human rights organizations (B'Tselem), and Palestinian civil society groups outside the PA's patronage structure — attest that three decades without a concluded final-status agreement, continued settlement expansion, and the unresolved Gaza blockade indicate the founding problem has shifted from 'negotiate an end state' to 'manage an indefinite interim,' with the framework's institutional beneficiaries having the least incentive to declare it dead.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rose from 0.32 at Oslo's signing to 0.58 by 2023 as the framework shifted from a live negotiating track to a diplomatic architecture whose main product is its own continuation — the PA's funding and legitimacy depend on the framework persisting, independent of whether it produces the state it promises. Theater_ratio's climb from 0.15 to 0.42 reflects an increasing share of framework-related activity (summits, roadmaps, conferences) that reaffirms the 1967-line reference point without advancing implementation. Suppression sits moderately high (0.62) because the framework requires active security cooperation to suppress armed resistance to it on the Palestinian side and active diplomatic pressure to constrain settlement expansion on the Israeli side — both only partially effective, which is itself part of why resistance (0.75) remains high from multiple directions.
 *
 * PERSPECTIVAL GAP:
 *   From the international diplomatic architecture's seat, this reads as durable coordination: a shared reference point that has kept two national movements talking, however imperfectly, for three decades. From the seat of a Gaza resident or a diaspora refugee, the same structure reads as extraction with a coordination veneer: real costs (blockade, foreclosed return to ancestral homes) sustained by a process that primarily reproduces the standing of those who administer it. The engine computing divergent per-seat types from these structural declarations is the intended signal, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The PA governing elite and the moderate Israeli security establishment are declared beneficiaries because the framework is the source of their institutional legitimacy and operational latitude, even though each also bears real costs (the PA absorbs the political cost of policing its own population; Israel absorbs international pressure to freeze settlement activity). Settlers, non-mandate-territory refugees, and Gaza residents are declared victims because the framework's specific compromises — negotiable land swaps, redirected return, and conditional blockade relief — redefine or defer their claims without their direct participation in setting the terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ending active war and occupation costs through a negotiated interim — was live in 1993. Whether it remains live in 2023 is genuinely contested: the framework's institutional beneficiaries (PA elite, international mediators) attest it as the only viable path, while independent human-rights observers attest the underlying problem has mutated into indefinite interim management. This mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges is exactly the mandatrophy signature the classification is built to surface: dismantling the framework would rearrange real institutional arrangements even if the arrangements no longer serve the founding problem they were built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compromise_or_containment,
    'Is the two-state coexistence framework a genuine transitional coordination mechanism en route to sovereign statehood, or has it become a containment structure that manages the conflict indefinitely while serving the institutional interests of its administering parties?',
    'Track whether final-status negotiations produce an implemented, bordered, sovereign Palestinian state within a defined horizon versus indefinite extension of interim arrangements (donor conferences, security-coordination renewals) without territorial implementation.',
    'If genuinely transitional, this reading is closer to scaffold (coordination toward a declared end-state); if containment has become the actual function, the tangled_rope classification is conservative and the structure trends toward snare for the victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compromise_or_containment, conceptual, 'Whether the framework is transitional scaffolding toward statehood or a durable containment/management structure.').

omega_variable(
    return_redirection_legitimacy,
    'Does redirecting the Palestinian right of return to the future Palestinian state''s territory (rather than to pre-1948 homes) constitute a legitimate negotiated resolution of a contested claim, or an extraction of that claim''s substance under the label of compromise?',
    'Compare international legal scholarship on refugee return rights (UN GA Res. 194 and subsequent interpretation) against the negotiated formula''s actual content, and survey affected refugee communities'' own assessment of whether the redirected formula satisfies their claim.',
    'If the redirection is read as legitimate resolution, the framework''s treatment of refugees is coordination with a real settlement; if read as extraction, refugees are correctly classified as victims of a transfer dressed as compromise, strengthening the tangled_rope reading over a rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(return_redirection_legitimacy, preference, 'Whether narrowing the right of return to the new state''s territory is genuine resolution or extraction of the underlying claim.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does accepting dual 1948 legitimacy under this reading logically foreclose the maximalist versions of the zionist_refuge_reading (whole-land claim) and the palestinian_autochthony_reading (unconditional return to original homes), or can all three readings coexist as live positions held by different factions within each national movement?',
    'Examine whether any single political coalition or legal framework has successfully held both the compromise formula and a maximalist sibling claim simultaneously without internal contradiction, versus whether adopting the compromise has historically required factions to explicitly renounce the maximalist claim.',
    'If genuine coexistence is possible within factions (many voters hold both a maximalist sentiment and support negotiation), the coexists_with relation is correct; if adopting the compromise structurally requires abandoning the maximalist premise, a forecloses relation would be more accurate for at least the return-to-original-homes clause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether this reading''s premises genuinely coexist with sibling readings'' maximalist claims or partially foreclose them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 1993, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1993, 0.15).
narrative_ontology:measurement(terr_tr_t1998, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1998, 0.22).
narrative_ontology:measurement(terr_tr_t2003, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2003, 0.3).
narrative_ontology:measurement(terr_tr_t2008, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement(terr_tr_t2013, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2013, 0.38).
narrative_ontology:measurement(terr_tr_t2018, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2018, 0.4).
narrative_ontology:measurement(terr_tr_t2023, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2023, 0.42).

% Extraction over time
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1993, 0.32).
narrative_ontology:measurement(terr_be_t1998, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1998, 0.4).
narrative_ontology:measurement(terr_be_t2003, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2003, 0.48).
narrative_ontology:measurement(terr_be_t2008, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2008, 0.52).
narrative_ontology:measurement(terr_be_t2013, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2013, 0.55).
narrative_ontology:measurement(terr_be_t2018, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2018, 0.57).
narrative_ontology:measurement(terr_be_t2023, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2023, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1993, 0.45).
narrative_ontology:measurement(terr_su_t1998, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1998, 0.5).
narrative_ontology:measurement(terr_su_t2003, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2003, 0.58).
narrative_ontology:measurement(terr_su_t2008, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2008, 0.55).
narrative_ontology:measurement(terr_su_t2013, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2013, 0.6).
narrative_ontology:measurement(terr_su_t2018, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2018, 0.61).
narrative_ontology:measurement(terr_su_t2023, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2023, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_autochthony_reading).

% DUAL FORMULATION NOTE:
% This story is the third member of the territorial_legitimacy_dual kernel family, alongside zionist_refuge_reading and palestinian_autochthony_reading. Each reading authors its own ε: the refuge reading and autochthony reading each ground legitimacy in a single people's exclusive historical claim (higher suppression toward the other's claim, lower internal coordination cost), while this compromise reading carries a genuine coordination function (enabling any negotiation to occur at all) alongside real, identifiable victims among those whose claims are narrowed by the compromise. The three stories are not the same constraint measured three ways — they are structurally distinct claims about what grounds legitimate sovereignty, linked here because each is cited as evidence or counter-evidence in disputes over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
