% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Territorial Legitimacy via International Legal Partition (UN 181 / 1948 Borders)
 *   domain: political/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the partition reading of the
 *   territorial_legitimacy kernel: the claim that both Israeli and
 *   Palestinian statehood derive legitimate title from UN General Assembly
 *   Resolution 181 (1947) and the 1949 Armistice Lines (Green Line),
 *   rendering settlements beyond 1967 lines illegitimate and a two-state
 *   solution structurally possible. The reading's ε is assessed against the
 *   standing arrangement — the partition framework as it has operated from
 *   1948 to present — by this reading's own lights. The constraint functions
 *   as a tangled_rope: it coordinates mutual recognition and territorial
 *   delimitation (genuine coordination function) while simultaneously
 *   extracting from Palestinian refugees (denied return/compensation) and
 *   from Israeli settlement enterprise (denied legitimacy/territory). Active
 *   enforcement is required through UN mechanisms, diplomatic pressure, and
 *   the Oslo architecture.
 *
 * KEY AGENTS:
 *   - palestinian_national_movement: Primary beneficiary (statehood recognition within 1967 lines) — coordinates on international legitimacy; also victim (refugee return unresolved, territorial fragmentation)
 *   - israeli_state_1948_borders: Primary beneficiary (international legitimacy for 1948 territory) — also victim (security costs, demographic pressure from refugee return claim)
 *   - palestinian_refugees_1948: Victim — excluded from return/compensation under partition framework; structural exclusion from the coordination
 *   - israeli_settlement_enterprise: Victim — extraction via delegitimization and eventual dismantling requirement; also agenda_setter (de facto territorial control)
 *   - international_legal_order: Agenda_setter/beneficiary — administers the framework, gains legitimacy from its operation
 *   - arab_states_accepting_partition: Beneficiary — conflict resolution framework; also payer (absorption costs, political risk)
 *   - jewish_communities_beyond_green_line: Victim — extraction via mandated withdrawal; identity_locked exit
 *   - palestinian_territorial_contiguity: Victim — extraction via fragmentation (settlements, Area C, Gaza separation); constrained exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.35).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.58).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Territorial Legitimacy via International Legal Partition (UN 181 / 1948 Borders)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, 'd304f456-9411-4ded-b017-2be5259e6874').
narrative_ontology:cs_kernel_codification('d304f456-9411-4ded-b017-2be5259e6874', formalized).
narrative_ontology:cs_authority_grounding('d304f456-9411-4ded-b017-2be5259e6874', lineage).
narrative_ontology:cs_interpretation_layer_present('d304f456-9411-4ded-b017-2be5259e6874').
narrative_ontology:cs_reading_relation('d304f456-9411-4ded-b017-2be5259e6874', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d304f456-9411-4ded-b017-2be5259e6874', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('d304f456-9411-4ded-b017-2be5259e6874', foundational, partition_as_legitimate_title).
narrative_ontology:cs_axiom_status(partition_as_legitimate_title, holdable).
narrative_ontology:cs_axiom_grounding('d304f456-9411-4ded-b017-2be5259e6874', partition_as_legitimate_title, conventional).
narrative_ontology:cs_axiom('d304f456-9411-4ded-b017-2be5259e6874', foundational, green_line_as_international_border).
narrative_ontology:cs_axiom_status(green_line_as_international_border, holdable).
narrative_ontology:cs_axiom_grounding('d304f456-9411-4ded-b017-2be5259e6874', green_line_as_international_border, conventional).
narrative_ontology:cs_axiom('d304f456-9411-4ded-b017-2be5259e6874', secondary, two_state_solution_as_structural_outcome).
narrative_ontology:cs_axiom_status(two_state_solution_as_structural_outcome, holdable).
narrative_ontology:cs_axiom_grounding('d304f456-9411-4ded-b017-2be5259e6874', two_state_solution_as_structural_outcome, instrumental).
narrative_ontology:cs_reference_frame('d304f456-9411-4ded-b017-2be5259e6874', un_resolution_181_partition_plan).
narrative_ontology:cs_drift_state('d304f456-9411-4ded-b017-2be5259e6874', post_oslo_failure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d304f456-9411-4ded-b017-2be5259e6874', '2026-08-27T14:30:00Z').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, palestinian_national_movement).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, israeli_state_1948_borders).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, international_legal_order).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, arab_states_accepting_partition).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, israeli_settlement_enterprise).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, jewish_communities_beyond_green_line).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_territorial_contiguity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_national_movement).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, israeli_state_1948_borders).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, arab_states_accepting_partition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains international recognition for Palestinian statehood within 1967 lines through the partition framework; pays through acceptance of 1948 territorial loss, refugee return deferral, and Jerusalem compromise. Exit is constrained: abandoning partition means losing the only internationally recognized path to statehood, but staying in it means accepting permanent territorial and demographic concessions.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_national_movement, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, palestinian_national_movement, payer).

% Gains international legitimacy for sovereignty within 1948/1949 lines through the partition framework; pays through withdrawal pressure from 1967 territories, refugee return claims, and Jerusalem sharing. Exit is constrained: the partition framework is the only source of legal title for 1948 territory; rejecting it isolates Israel internationally but retaining it requires territorial concessions.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_state_1948_borders, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, israeli_state_1948_borders, payer).

% Structurally excluded from the partition framework's resolution: UNGA 194's 'right of return' or compensation remains unimplemented; the two-state solution absorbs their claim into a 'just agreed solution' that has never materialized. No exit: they cannot return, cannot integrate in host states (Lebanon, Syria), and cannot access the statehood the framework promises. The constraint extracts their displacement as the price of partition.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_refugees_1948, payer,
    powerless, biographical, trapped, regional).

% Extracts territorial control and demographic facts on the ground in 1967 territories; the partition framework mandates their withdrawal/delegitimization as the price of Palestinian statehood. Identity-locked: settlers' self-concept, religious commitment, and communal infrastructure are fused to the land; exit means abandoning a core identity. As agenda_setter, they actively reshape facts to foreclose the framework's territorial logic.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_settlement_enterprise, payer,
    powerful, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, israeli_settlement_enterprise, agenda_setter).

% Administers the partition framework through UNGA, UNSC, ICJ, and the Quartet. Gains institutional legitimacy from managing the conflict's legal architecture. Not a payer: the framework's costs are borne by the parties, not the order. Analytical exit: can reinterpret or abandon the framework without existential cost.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_legal_order, agenda_setter,
    institutional, civilizational, analytical, universal).

% Gain regional conflict resolution framework and normalization pathway (Arab Peace Initiative); pay political costs of recognizing Israel, absorbing Palestinian refugees, and managing domestic rejectionist pressures. Mobile exit: can pivot to bilateral normalization (Abraham Accords) or maintain rhetorical commitment without enforcement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, arab_states_accepting_partition, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, arab_states_accepting_partition, payer).

% Communities established in East Jerusalem, West Bank settlement blocs, and Golan Heights under Israeli administration. The partition framework treats their presence as illegal and mandates withdrawal or land swaps. Identity-locked: religious, historical, and communal identity fused to specific geography; exit is experienced as existential abandonment. Not agenda_setters — they are subjects of the framework's territorial logic.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, jewish_communities_beyond_green_line, payer,
    moderate, biographical, identity_locked, local).

% The partition framework's viability depends on contiguous Palestinian territory (West Bank-Gaza connection, viable state); settlement blocs, Area C fragmentation, and Gaza separation extract this contiguity. Not an agent — a structural condition that the constraint's operation degrades. Trapped: no exit from the geography; the framework's coordination function requires contiguity that its enforcement fails to secure.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_territorial_contiguity, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(territorial_legitimacy__partition_reading, palestinian_territorial_contiguity).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__partition_reading, international_legal_order).
narrative_ontology:fixing_cost_class(territorial_legitimacy__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves competing nationalist claims to the same territory by allocating sovereignty along internationally recognized lines (UN 181, 1949 Armistice), enabling mutual recognition, diplomatic relations, and a legal basis for ending the conflict.
% TRANSFER_FUNCTION: Moves territorial sovereignty from contested/undetermined status to recognized states: Israeli sovereignty west of Green Line, Palestinian sovereignty in West Bank/Gaza. Moves refugee return claims from individual right to 'just agreed solution.' Moves Jerusalem from corpus separatum to divided/final-status. Moves settlement enterprise from de facto control to negotiated status.
% ABSENT_VOICES: Palestinian refugees (excluded from return/compensation mechanisms), Jewish communities in 1967 territories (excluded from legitimacy), Hamas/rejectionist factions (excluded from diplomatic process), Mizrahi Jewish refugees from Arab lands (excluded from symmetry claims), Bedouin communities in Negev/Naqab (excluded from both state frameworks).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).
:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) reflects asymmetric costs: Palestinian refugees bear unresolved displacement (high extraction from their seat); Israeli settlements bear delegitimization and withdrawal pressure (extraction from their seat); both states gain legitimacy (coordination benefit). Suppression (0.58) reflects active enforcement needed: UN mechanisms, diplomatic Quartet, Oslo architecture, ICJ opinions — the framework does not self-sustain. Theater ratio (0.22) is moderate: Oslo created performative process layers, but core coordination (mutual recognition, territorial basis) remains functional. Accessibility collapse (0.62) is significant: once the partition framework is accepted, alternatives (one-state, confederation, Jordanian/Egyptian reversion) collapse structurally. Resistance (0.48) is substantial from both settlement enterprise and rejectionist Palestinian factions.
 *
 * PERSPECTIVAL GAP:
 *   From the Palestinian national movement seat: the constraint is coordination (statehood, borders, international law) with extractive residue (refugees, Jerusalem, settlements). From the Israeli state seat: the constraint is coordination (legitimacy, security, recognition) with extractive residue (refugee return claim, 1967 territorial withdrawal). From the refugee seat: the constraint is snare (partition legitimizes displacement). From the settlement seat: the constraint is snare (partition delegitimizes their existence). The engine computes these per-seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian national movement and Israeli state (1948 borders) are primary beneficiaries: both gain international legitimacy, recognized borders, and a conflict-ending framework. Palestinian refugees and Israeli settlement enterprise are primary victims: refugees are structurally excluded from return; settlements are structurally mandated for dismantling. The international legal order is agenda_setter (administers the framework). Arab states are secondary beneficiaries (regional stability) and payers (political costs). Jewish communities beyond Green Line and Palestinian territorial contiguity are victims of the framework's territorial logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The partition framework's founding problem (resolving competing national claims in Mandate Palestine via international law) remains live and contested — neither dead nor fully resolved. The constraint has not atrophied into piton: active diplomatic investment (Quartet, Arab Peace Initiative, ICJ) sustains it. It is not a false summit mountain: it explicitly declares itself as constructed international law (emerges_naturally: false) with identifiable beneficiaries and victims. The mandatrophy risk is that the framework persists as theater while facts on the ground (settlements, fragmentation) make its coordination function inoperable — tracked by rising theater_ratio from 1967 onward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is this constraint one reading of a contested kernel (territorial_legitimacy), distinct from security_necessity_reading and indigenous_continuity_reading?',
    'Structural decomposition: if the constraint''s ε, beneficiaries, victims, and classification shift when the legitimating premise changes (partition vs. security vs. indigenous continuity), then these are distinct constraints linked by network.affects_constraints, not one constraint with observer variance.',
    'If confirmed as a kernel reading, this constraint must carry reading_relations and axioms in cs_structure, and its ε is assessed against the standing arrangement (partition framework) by this reading''s own lights — not by a blended or averaged standard.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether territorial legitimacy via partition is a distinct constraint reading within a kernel family.').

omega_variable(
    partition_enforceability_post_1967,
    'Does the partition framework retain operative force for territories occupied in 1967, or has it been superseded by subsequent resolutions and facts on the ground?',
    'Legal-historical analysis of UNSC Resolutions 242, 338, and subsequent practice; ICJ advisory opinions; state practice regarding Green Line vs. 1948 borders.',
    'If partition legitimacy extends only to 1948 lines, the constraint''s coordination function is narrower and its extraction on 1967-occupying actors is higher; if it extends to 1967 lines with land-swap provisions, the coordination function is broader and the constraint is more rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_enforceability_post_1967, empirical, 'Whether the partition reading''s territorial scope is fixed at 1948 lines or extends to 1967 lines with modifications.').

omega_variable(
    refugee_return_as_extraction_or_coordination,
    'Is the partition framework''s stance on Palestinian refugee return (UNGA 194) a coordination mechanism (addressing displacement) or an extraction vector (demographic threat to Israeli state viability)?',
    'Demographic modeling of return scenarios; legal analysis of ''right of return'' vs. ''just resolution'' language; negotiation history from Lausanne 1949 through Taba 2001.',
    'If return is a coordination claim, the constraint is more rope-like; if it functions as demographic extraction against the Israeli state, the constraint is more tangled_rope or snare from the Israeli seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(refugee_return_as_extraction_or_coordination, conceptual, 'Whether refugee return under partition legitimacy is coordination or extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(territorial_legitimacy__partition_reading_tr_t1948, territorial_legitimacy__partition_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_tr_t1967, territorial_legitimacy__partition_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_tr_t1993, territorial_legitimacy__partition_reading, theater_ratio, 1993, 0.15).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_tr_t2000, territorial_legitimacy__partition_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_tr_t2023, territorial_legitimacy__partition_reading, theater_ratio, 2023, 0.22).

% Extraction over time
narrative_ontology:measurement(territorial_legitimacy__partition_reading_be_t1948, territorial_legitimacy__partition_reading, base_extractiveness, 1948, 0.18).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_be_t1967, territorial_legitimacy__partition_reading, base_extractiveness, 1967, 0.42).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_be_t1993, territorial_legitimacy__partition_reading, base_extractiveness, 1993, 0.31).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_be_t2000, territorial_legitimacy__partition_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_be_t2023, territorial_legitimacy__partition_reading, base_extractiveness, 2023, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(territorial_legitimacy__partition_reading_su_t1948, territorial_legitimacy__partition_reading, suppression_requirement, 1948, 0.35).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_su_t1967, territorial_legitimacy__partition_reading, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_su_t1993, territorial_legitimacy__partition_reading, suppression_requirement, 1993, 0.45).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_su_t2000, territorial_legitimacy__partition_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(territorial_legitimacy__partition_reading_su_t2023, territorial_legitimacy__partition_reading, suppression_requirement, 2023, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__partition_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, oslo_accords_architecture).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, arab_peace_initiative).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, icj_wall_advisory_opinion).

% DUAL FORMULATION NOTE:
% This constraint is one member of the territorial_legitimacy kernel family. The partition_reading (this constraint) coordinates mutual statehood via international law; the security_necessity_reading coordinates Israeli legitimacy via territorial control; the indigenous_continuity_reading coordinates Palestinian legitimacy via historical continuity. Their ε values differ substantially: partition_reading ε≈0.35 (tangled_rope), security_necessity_reading ε≈0.65 (snare from Palestinian seat), indigenous_continuity_reading ε≈0.55 (tangled_rope from Israeli seat). They are linked via affects_constraints because each reading's legitimacy claims structurally pressure the others' operating space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__partition_reading, institutional, 0.15).
constraint_indexing:directionality_override(territorial_legitimacy__partition_reading, organized, 0.35).
constraint_indexing:directionality_override(territorial_legitimacy__partition_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
