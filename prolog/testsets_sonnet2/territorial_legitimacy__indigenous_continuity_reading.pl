% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Territorial Legitimacy via Indigenous Continuity (1948 as Nakba) — Standing Arrangement Read as Settler-Colonial Dispossession
 *   domain: political/legal/territorial
 *
 * SUMMARY:
 *   This constraint instantiates the indigenous-continuity reading of the
 *   territorial legitimacy kernel: it treats 1948 as Nakba
 *   (catastrophe/dispossession) rather than as the lawful founding moment of
 *   a partitioned state, and grounds Palestinian claims to sovereignty over
 *   the whole of historic Palestine in continuous habitation predating the
 *   settler-colonial project, with the right of return for 1948 refugees as
 *   structurally central rather than a negotiable humanitarian add-on. This
 *   is one of three sibling readings of a single contested kernel; the
 *   partition reading and the security-necessity reading are separate
 *   constraint stories with their own ε and structural data. Per the
 *   ε-invariance principle, this file does not average across readings or
 *   hedge between them — it authors the standing arrangement (the current
 *   territorial and political order) exactly as THIS reading's own lights
 *   assess it: as a highly extractive, actively suppressed settler-colonial
 *   outcome.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.86).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.88).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy via Indigenous Continuity (1948 as Nakba) — Standing Arrangement Read as Settler-Colonial Dispossession").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political/legal/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, '1ade30ff-aa43-4789-aefb-c4c8ec07a709').
narrative_ontology:cs_kernel_codification('1ade30ff-aa43-4789-aefb-c4c8ec07a709', distributed).
narrative_ontology:cs_authority_grounding('1ade30ff-aa43-4789-aefb-c4c8ec07a709', distributed).
narrative_ontology:cs_reading_relation('1ade30ff-aa43-4789-aefb-c4c8ec07a709', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('1ade30ff-aa43-4789-aefb-c4c8ec07a709', territorial_legitimacy__security_necessity_reading, influences).
narrative_ontology:cs_axiom('1ade30ff-aa43-4789-aefb-c4c8ec07a709', foundational, continuous_indigenous_habitation_grounds_sovereignty).
narrative_ontology:cs_axiom_status(continuous_indigenous_habitation_grounds_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('1ade30ff-aa43-4789-aefb-c4c8ec07a709', continuous_indigenous_habitation_grounds_sovereignty, deontological).
narrative_ontology:cs_axiom('1ade30ff-aa43-4789-aefb-c4c8ec07a709', foundational, id_1948_state_formation_constitutes_settler_colonial_dispossession).
narrative_ontology:cs_axiom_status(id_1948_state_formation_constitutes_settler_colonial_dispossession, holdable).
narrative_ontology:cs_axiom_grounding('1ade30ff-aa43-4789-aefb-c4c8ec07a709', id_1948_state_formation_constitutes_settler_colonial_dispossession, empirically_contingent).
narrative_ontology:cs_axiom('1ade30ff-aa43-4789-aefb-c4c8ec07a709', secondary, right_of_return_is_structurally_non_negotiable).
narrative_ontology:cs_axiom_status(right_of_return_is_structurally_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('1ade30ff-aa43-4789-aefb-c4c8ec07a709', right_of_return_is_structurally_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('1ade30ff-aa43-4789-aefb-c4c8ec07a709', pre_1948_indigenous_habitation_pattern).
narrative_ontology:cs_drift_state('1ade30ff-aa43-4789-aefb-c4c8ec07a709', contemporary_post_oslo_collapse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1ade30ff-aa43-4789-aefb-c4c8ec07a709', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, israeli_state_and_settler_population).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, international_powers_underwriting_partition_outcome).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, displaced_1948_refugees_and_descendants).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinians_under_ongoing_occupation).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, internally_present_palestinian_minority_in_israel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds sovereign control over the territory of historic Palestine established in 1948 and expanded in subsequent wars, administers citizenship and land law, and controls the military and legal apparatus that enforces the boundary between citizen and non-citizen Palestinian populations. From this reading's perspective, the state's founding is read as the successful conclusion of a settler-colonial project riding on the expulsion of the indigenous population, not the vindication of a return to an ancestral homeland.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state_and_settler_population, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, israeli_state_and_settler_population, agenda_setter).

% The UN, and the great powers that engineered and later recognized the 1947 partition and the resulting 1948 state, treat the outcome as settled international law. From the indigenous-continuity reading, this recognition converted an act of dispossession into a juridical fact, and these institutions continue to benefit from the stability of that settlement (diplomatic, economic, strategic) without bearing its costs.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_powers_underwriting_partition_outcome, beneficiary,
    institutional, generational, analytical, global).

% Palestinians expelled or fled in 1948 and their multi-generational descendants, dispersed across refugee camps in neighboring states and the diaspora, hold UNRWA-registered refugee status but are barred by Israeli law from returning to or reclaiming property in the territory. Their claim to the land is treated in this reading as continuous and unextinguished by time or by the intervening state's existence; their situation is the central fact the reading organizes around.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, displaced_1948_refugees_and_descendants, payer,
    powerless, civilizational, trapped, regional).

% Residents of the West Bank and Gaza live under military occupation, settlement expansion, movement restriction, and administrative law regimes that this reading interprets as the ongoing operational phase of the 1948 dispossession rather than a separate post-1967 problem. Exit from the territory or from the legal regime is not available to most; those with resources may emigrate but at the cost of severing the continuity claim to the land.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinians_under_ongoing_occupation, payer,
    powerless, biographical, trapped, regional).

% Palestinian citizens of Israel remained within the 1948 borders and hold formal citizenship, but this reading treats their position as structurally subordinate — land allocation, planning law, and demographic policy are read as instruments that continue the dispossession by administrative rather than military means. They can vote and litigate but cannot alter the constitutional character of the state as a Jewish-majority polity through those channels.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, internally_present_palestinian_minority_in_israel, payer,
    moderate, generational, constrained, national).

% Political factions and civil society organizations that assert the indigenous-continuity claim as their founding narrative are excluded from any negotiating framework premised on the partition reading's legitimacy — international mediation structures (Oslo-derivative and successor frameworks) require accepting Israel's existence within some borders as a precondition to participate, which forecloses the continuity reading's core demand from the negotiating table itself.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_national_movement, excluded,
    organized, civilizational, constrained, regional).

% UN special rapporteurs, ICJ advisory proceedings, and human rights organizations document population transfer, settlement activity, and refugee status without power to enforce a resolution; their findings are cited by advocates of the continuity reading as corroboration but are not binding on the state actors who control territory.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_law_and_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__indigenous_continuity_reading, israeli_state_and_settler_population).
narrative_ontology:fixing_cost_class(territorial_legitimacy__indigenous_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no genuine coordination problem this reading's arrangement solves for the population it describes as dispossessed — the reading identifies the 1948 outcome as an extractive settlement, not a solution to a shared problem among the parties it names as victims. The coordination function this reading DOES identify is the coordination among the beneficiary states and the settler population to consolidate and defend the territorial outcome.
% TRANSFER_FUNCTION: Moves land, property, and political sovereignty from the indigenous Palestinian population present in 1948 to the incoming Jewish settler population and the state apparatus built to hold that transfer, and continues to move land and movement rights from Palestinians under occupation and within Israel to the state and settler population through ongoing administrative and military mechanisms.
% ABSENT_VOICES: The 1948 and post-1967 refugee population is not a party to any negotiating or legal framework that could alter their status; their claim is asserted by advocacy organizations and some state patrons but they hold no direct seat. The internally present Palestinian minority votes but cannot address the constitutional character of the state through that vote.
% DISAPPEARANCE_RATIONALE: If the legitimacy claim organizing the current territorial arrangement were rejected and replaced by unrestricted return and single-state sovereignty premised on indigenous continuity, the demographic, political, and property structure of the entire territory would be fundamentally reorganized — this is precisely why the reading is contested rather than settled.
% FOUNDING_PROBLEM: The founding problem this reading identifies is anti-colonial: ending a settler-colonial project that displaced an indigenous population under cover of international legal partition, and restoring sovereignty and property to the population that inhabited the land continuously prior to 1948.
% FOUNDING_PROBLEM_CORROBORATION: UN human rights bodies, historians of the 1948 war (the 'New Historians' scholarship using declassified Israeli military archives), and UNRWA's own registration records corroborate the historical facts of mass displacement in 1948 from outside the Palestinian national movement itself. Whether those facts sustain the specific normative conclusion (settler-colonial illegitimacy, right of return as legally central) is disputed by legal scholars and states who accept the same displacement facts but read them through the partition or security-necessity framework instead.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.86, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.86) because, by this reading's own premises, the transfer of land, property, and sovereignty from an indigenous population to an incoming settler population and its state is the central and ongoing fact the reading is about — not a byproduct of otherwise-legitimate state-building. Suppression is authored very high (0.88) because the reading holds that continued Israeli state power, military occupation infrastructure, and the international diplomatic architecture recognizing 1948 as settled all function to prevent the return and restoration of sovereignty this reading demands. Resistance is high (0.9) reflecting sustained Palestinian national mobilization, refugee advocacy, and international solidarity movements contesting the arrangement since its inception. Accessibility collapse is moderate (0.5), not near-mountain levels, because this reading holds the alternative (return, restored sovereignty) remains conceptually and legally live, not naturalized away — the whole point of the reading is that the current order has NOT successfully closed off the alternative in the moral or legal sense, even though it has closed it off physically and administratively for most rights-holders.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state and settler population are coded as full beneficiaries under this reading's lens — sovereignty, land, and security accrue to them through the same structure the reading identifies as extractive. International powers that underwrote and continue to recognize the 1948/partition outcome are coded as secondary beneficiaries: they bear none of the displacement cost and gain regional stability and alliance value. The 1948 refugees and their descendants are the paradigm victims — trapped, civilizational time horizon, because the claim is inherited across generations without resolution. Palestinians under occupation and the internal Palestinian minority in Israel are victims of the ongoing operational phase, differentiated by exit options (trapped vs. constrained) reflecting their different legal statuses (non-citizen occupied population vs. citizen minority).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the founding problem (ending settler-colonial dispossession, restoring indigenous sovereignty) as unambiguously still live — hence founding_problem_status is 'contested' at the story level because outside corroboration (historical scholarship, UN documentation) supports the underlying displacement facts while disputing whether they license this reading's specific normative conclusion. The classification must not be read as adjudicating between readings — the engine computing a high-extraction type for this reading is the expected and intended result of authoring the reading's own perspective faithfully, not evidence that other readings are wrong.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_colonial_characterization_contested,
    'Is the 1948 state formation properly characterized as settler-colonial dispossession of a continuously-present indigenous population, or as the lawful exercise of self-determination by a returning national group with its own indigenous claim to the same territory?',
    'This is not resolvable by additional historical data alone — the underlying displacement facts (documented in Israeli military archives and UN records) are largely agreed upon across readings; the dispute is over the normative framework (settler-colonialism vs. competing national self-determination) used to characterize those facts. Resolution would require agreement on contested categories in international law and historiography that the parties themselves dispute.',
    'If the settler-colonial characterization is accepted as the governing frame, this reading''s classification (snare, near-total extraction) is the accurate structural description. If a competing national self-determination frame is accepted instead, the same historical facts support the partition or security-necessity readings'' much lower extraction assessments — this is exactly the kernel-contest structure the story format is designed to hold without collapsing into one number.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settler_colonial_characterization_contested, conceptual, 'Whether the founding characterization (settler-colonial vs. competing self-determination) is itself resolvable or is an irreducible framing dispute between kernel readings.').

omega_variable(
    right_of_return_legal_status_ambiguity,
    'Does the right of return asserted for 1948 refugees and their multi-generational descendants constitute a live, presently-enforceable legal claim under international law (UN General Assembly Resolution 194 and successor instruments), or has it been effectively superseded by seven decades of non-implementation and intervening state practice?',
    'Would require either a binding international judicial determination (e.g., an ICJ ruling squarely addressing multi-generational refugee return claims) or a negotiated settlement between the parties themselves establishing the claim''s disposition — neither currently exists.',
    'If the right of return is a live legal claim, the extraction and suppression metrics authored here (treating current non-return as ongoing enforced extraction) are structurally accurate. If the claim is treated as historically important but legally lapsed through non-implementation, the same facts would support characterizing the current arrangement as tragic history rather than live ongoing extraction — this would not change ε for THIS reading (which holds the claim live by its own premises) but bears on how contested that authored ε is across readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_legal_status_ambiguity, conceptual, 'Whether the 1948 refugee right of return is a presently live legal claim or a historically significant but non-enforceable claim — central to this reading''s structural centrality of return.').

omega_variable(
    coalition_power_of_dispersed_refugee_population,
    'Given that displaced 1948 refugees and descendants are coded as powerless and trapped individually, does their aggregate diasporic and organizational mobilization (through refugee advocacy networks, UNRWA registration as a collective legal category, and diplomatic support from Arab and Muslim-majority states) constitute a form of coalition power this individual-level coding misses?',
    'Track diplomatic outcomes attributable to organized refugee/diaspora advocacy (UN resolutions, ICJ referrals, state-level recognition campaigns) versus outcomes attributable to individual-level negotiation, which is structurally unavailable to this population.',
    'If coalition power is substantial, some victim-seat stakeholders should be understood as having more effective agency than the individual powerless/trapped coding suggests, which would matter for assessing resistance capacity (already authored high at 0.9) without necessarily changing the extraction assessment itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_power_of_dispersed_refugee_population, empirical, 'Whether dispersed refugee populations achieve coalition-level power despite individually powerless structural positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1993, 0.55).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1948, 0.78).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1967, 0.83).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1993, 0.8).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2000, 0.84).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2010, 0.85).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2024, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1993, 0.7).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__security_necessity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the territorial_legitimacy kernel, each authored as a structurally distinct, ε-invariant constraint per the decomposition principle. indigenous_continuity_reading authors a high-extraction (0.86), snare-classified reading treating the 1948 state formation as settler-colonial dispossession with right of return as structurally central. partition_reading and security_necessity_reading are separate files with their own ε values, beneficiary/victim structures, and classifications, reflecting their own normative premises. The three files do not average or reconcile — they are linked via affects_constraints because each reading's persistence and rhetorical strength structurally influences the political viability and negotiating position available to the others (e.g., international recognition sustaining the partition reading directly constrains the political space available to the indigenous-continuity reading's demands, and vice versa).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
