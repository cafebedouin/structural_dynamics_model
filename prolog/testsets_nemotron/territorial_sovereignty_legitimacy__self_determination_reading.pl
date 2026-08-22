% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Self-Determination Reading of Territorial Sovereignty Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story captures the self-determination reading of
 *   territorial sovereignty legitimacy in Israel/Palestine. The reading
 *   asserts that sovereignty in the territory derives from the modern
 *   principle of self-determination applied to the Arab population that held
 *   demographic majority and continuous residence during the 19th-20th
 *   centuries. It frames the 1947 UN Partition Plan and 1948 establishment of
 *   Israel as unjust external impositions, the Israeli state as a colonial
 *   project, and the right of return as restoration of the status quo ante.
 *   The reading operates as a tangled rope: it coordinates international
 *   diplomatic and legal action around a universalizable norm (coordination
 *   function) while extracting legitimacy from the Zionist project and
 *   imposing costs on Israeli Jews and settlers (asymmetric extraction).
 *   Active enforcement is required through UN bodies, ICJ, ICC, BDS
 *   campaigns, and Arab League diplomacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.68).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.75).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Self-Determination Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, 'b58510b5-312f-4278-8188-371317b60df0').
narrative_ontology:cs_kernel_codification('b58510b5-312f-4278-8188-371317b60df0', distributed).
narrative_ontology:cs_authority_grounding('b58510b5-312f-4278-8188-371317b60df0', distributed).
narrative_ontology:cs_reading_relation('b58510b5-312f-4278-8188-371317b60df0', territorial_sovereignty_legitimacy__covenant_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('b58510b5-312f-4278-8188-371317b60df0', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('b58510b5-312f-4278-8188-371317b60df0', foundational, self_determination_primary_sovereignty_source).
narrative_ontology:cs_axiom_status(self_determination_primary_sovereignty_source, holdable).
narrative_ontology:cs_axiom_grounding('b58510b5-312f-4278-8188-371317b60df0', self_determination_primary_sovereignty_source, conventional).
narrative_ontology:cs_axiom('b58510b5-312f-4278-8188-371317b60df0', foundational, demographic_continuity_grounds_territorial_claim).
narrative_ontology:cs_axiom_status(demographic_continuity_grounds_territorial_claim, holdable).
narrative_ontology:cs_axiom_grounding('b58510b5-312f-4278-8188-371317b60df0', demographic_continuity_grounds_territorial_claim, empirically_contingent).
narrative_ontology:cs_axiom('b58510b5-312f-4278-8188-371317b60df0', secondary, partition_1947_as_external_imposition).
narrative_ontology:cs_axiom_status(partition_1947_as_external_imposition, holdable).
narrative_ontology:cs_axiom_grounding('b58510b5-312f-4278-8188-371317b60df0', partition_1947_as_external_imposition, empirically_contingent).
narrative_ontology:cs_axiom('b58510b5-312f-4278-8188-371317b60df0', secondary, zionist_project_as_colonial_enterprise).
narrative_ontology:cs_axiom_status(zionist_project_as_colonial_enterprise, holdable).
narrative_ontology:cs_axiom_grounding('b58510b5-312f-4278-8188-371317b60df0', zionist_project_as_colonial_enterprise, conventional).
narrative_ontology:cs_axiom('b58510b5-312f-4278-8188-371317b60df0', foundational, right_of_return_as_status_quo_ante_restoration).
narrative_ontology:cs_axiom_status(right_of_return_as_status_quo_ante_restoration, holdable).
narrative_ontology:cs_axiom_grounding('b58510b5-312f-4278-8188-371317b60df0', right_of_return_as_status_quo_ante_restoration, deontological).
narrative_ontology:cs_reference_frame('b58510b5-312f-4278-8188-371317b60df0', mandate_era_arab_majority_self_determination).
narrative_ontology:cs_drift_state('b58510b5-312f-4278-8188-371317b60df0', post_oslo_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b58510b5-312f-4278-8188-371317b60df0', '2026-08-24T14:30:00Z').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_movement).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, arab_states_supporting_palestinian_cause).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, international_law_advocates_self_determination).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, israeli_jewish_population).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, jewish_settlers_west_bank).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, zionist_institutional_project).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advances the self-determination reading as the organizing principle for Palestinian statehood claims. Sets the diplomatic agenda at UN and Arab League forums. The reading is fused to Palestinian national identity — abandoning it would dissolve the movement's raison d'etre. Gains legitimacy capital and international recognition from the reading's traction.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_movement, agenda_setter,
    organized, generational, identity_locked, regional).

% Use the self-determination reading to legitimize their support for Palestinian claims while avoiding costly direct confrontation. The reading provides a normative framework that aligns with their domestic political needs (street credibility, anti-colonial posture) without requiring military commitment. Exit would mean diplomatic isolation in the Arab world.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, arab_states_supporting_palestinian_cause, beneficiary,
    institutional, generational, constrained, regional).

% Legal scholars, NGOs, and UN officials who invoke self-determination as the governing norm. Gain professional standing and institutional relevance from the reading's centrality in international discourse. Can shift to other frameworks (human rights law, humanitarian law) if the self-determination reading loses traction — their exit is professional, not existential.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_law_advocates_self_determination, beneficiary,
    organized, biographical, mobile, global).

% Bears the costs of the self-determination reading's delegitimization of their state's founding and ongoing sovereignty. The reading frames their presence as colonial imposition, threatening the moral legitimacy of their polity. Exit is identity-locked: the reading contests the very foundation of their collective existence in the territory. They cannot 'leave' the constraint without conceding their own illegitimacy.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_jewish_population, payer,
    institutional, generational, identity_locked, national).

% Directly targeted by the reading's colonial framing and right-of-return logic. Face potential displacement, legal liability, and loss of property if the reading's maximal demands are implemented. Have state backing (IDF, legal system) but remain vulnerable to international pressure and diplomatic isolation. Exit means abandoning ideological commitment to 'Greater Israel' — constrained but not identity-locked for all.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, jewish_settlers_west_bank, payer,
    powerful, biographical, constrained, local).

% The organized institutional expression of Jewish sovereignty (government, Jewish Agency, WZO, settlement enterprise). The self-determination reading extracts legitimacy from the project's foundational narrative, reframing its achievements as colonial crimes. Simultaneously sets the counter-agenda (security, historical connection, legal title). Identity-locked: the project's coherence depends on rejecting the reading's core premises.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, zionist_institutional_project, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, zionist_institutional_project, agenda_setter).

% The reading's primary moral beneficiaries (right of return as status quo ante restoration) but structurally excluded from decision-making. No vote in PLO/PA diplomacy, no leverage over Arab state sponsors, no mechanism to enforce return. Their situation is the reading's strongest moral claim and its most visible failure of realization.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees_diaspora, excluded,
    powerless, generational, trapped, global).

% States (US, EU, UK, etc.) that formally endorse two-state solution and self-determination in principle but functionally maintain the status quo. Their analytical seat produces endless process (peace plans, parameters, Quartet reports) that validates the reading's vocabulary while preventing its implementation. Exit is analytical — they can shift frameworks without existential cost.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, western_liberal_democracies, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universalizable normative framework (self-determination) that replaces zero-sum historical/religious claims with a procedural standard applicable to all peoples. Coordinates international diplomatic recognition, UN resolutions, and NGO advocacy around a single legal vocabulary.
% TRANSFER_FUNCTION: Transfers moral legitimacy, diplomatic recognition, and territorial sovereignty claims FROM the Zionist project (grounded in historical connection, legal title, security) TO the Palestinian national movement (grounded in demographic continuity, self-determination, anti-colonial right). The transfer operates through international law institutions, UN bodies, and global public opinion.
% ABSENT_VOICES: Palestinian refugees in diaspora camps (Lebanon, Jordan, Syria) — the reading's core moral constituency but excluded from diplomatic agency. Mizrahi Jews (Jews from Arab/Muslim lands) — their expulsion/exodus parallels Palestinian displacement but is erased by the reading's binary colonial/indigenous framing. Israeli peace camp — supports Palestinian self-determination but rejects the reading's delegitimization of Israel's 1948 founding, leaving them without a structural home.
% DISAPPEARANCE_RATIONALE: If the self-determination reading vanished overnight, the diplomatic architecture built since 1967 (UNSC 242, Oslo, Arab Peace Initiative, ICJ opinions) would collapse. The Palestinian national movement would lose its primary international legal vocabulary. The conflict would revert to competing historical/religious claims (covenant vs. presence) or existential zero-sum framing — a fundamentally different structural landscape.
% FOUNDING_PROBLEM: The collapse of Ottoman imperial order and the imposition of European colonial mandates (Sykes-Picot, Balfour, British Mandate) created a legitimacy vacuum in Palestine. The self-determination reading emerged as the anti-colonial answer: the Arab majority's right to determine their political future free from external imposition, countering the Zionist project's claim to the same territory.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: League of Nations Mandate system records (British Mandate explicitly violated Article 22 self-determination mandate); UNSCOP 1947 minority report (India, Iran, Yugoslavia) rejecting partition as violation of self-determination; ICJ 2004 Wall Opinion affirming Palestinian right to self-determination; General Assembly resolutions 181, 194, 242, 3379 (revoked). Contested by: Israeli historical narrative (self-determination exercised via 1948 establishment, UN admission); US diplomatic practice (self-determination balanced against Israel's security/legitimacy); Covenant continuity reading proponents.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the reading's structural operation: it extracts moral legitimacy, diplomatic space, and territorial claim from the Zionist project while demanding material concessions (right of return, 1967 lines, Jerusalem). Suppression (0.75) is high because the reading's persistence depends on actively suppressing the covenant continuity reading's legitimacy in international forums (delegitimization campaigns, apartheid accusations, lawfare) and suppressing exit alternatives for Palestinian refugees (no integration in host states, right of return as non-negotiable). Theater ratio (0.42) is moderate: the diplomatic process (Oslo, Annapolis, Kerry parameters) performs negotiation while the reading's maximal demands (full return, full sovereignty) remain structurally non-negotiable. Accessibility collapse (0.65) and resistance (0.58) reflect the reading's entrenched position in international law and the active contestation it faces.
 *
 * PERSPECTIVAL GAP:
 *   From the Palestinian national movement's seat, the reading is a rope — a genuine coordination mechanism that solves the legitimacy problem of statelessness. From the Israeli Jewish population's seat, it is a snare — a delegitimization machine that offers no stable equilibrium short of their polity's dissolution. From the Western democracies' seat, it is a scaffold — a transitional framework meant to produce a two-state solution that never materializes. The engine computes these divergences from the structural data; the claimed type (tangled_rope) reflects the reading's dual coordination/extraction structure as authored.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian national movement is the primary agenda setter and structural beneficiary — the reading constitutes their political identity and international standing (d ~ 0.15). Arab states and international law advocates benefit diplomatically and professionally without existential commitment (d ~ 0.25-0.35). Israeli Jewish population and Zionist institutional project are identity-locked targets — the reading contests their foundational legitimacy, making exit equivalent to self-negation (d ~ 0.9). Settlers are constrained payers with state backing but high vulnerability (d ~ 0.75). Refugees are trapped excluded — moral beneficiaries with zero agency (d ~ 0.95). Western democracies are analytical observers with arbitrage-grade exit (d ~ 0.05).
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem (anti-colonial self-determination against British/Zionist imposition) remains contested — not dead (colonial structures persist in occupation/settlement) but not purely live (1948 facts on ground, Jewish self-determination now also operative). This contested status prevents clean mandatrophy resolution: the reading cannot be retired as obsolete (occupation continues) nor fully vindicated (no sovereign state realized). The tension sustains the constraint's extractive energy — it remains a live claim on a future that the structural balance of power blocks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the self-determination reading a distinct constraint with stable ε, or does its classification depend on which observable (diplomatic recognition, legal opinions, public opinion, facts on ground) is measured?',
    'Apply ε-invariance test: if measuring via UNGA resolutions yields low ε but measuring via refugee return implementation yields high ε, the label ''self-determination reading'' covers multiple constraints. Decompose into separate stories linked by network.affects_constraints.',
    'If ε varies by observable, the current story conflates structurally distinct claims. Decomposition would produce separate constraint stories for ''self-determination as diplomatic vocabulary'' (rope-like) and ''self-determination as return implementation'' (snare-like), linked as a constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the self-determination reading satisfies ε-invariance or requires decomposition per DP-001.').

omega_variable(
    coordination_extraction_separability,
    'Can the reading''s coordination function (universalizable self-determination norm) be separated from its extraction function (delegitimization of Zionist project, right of return as non-negotiable)?',
    'Counterfactual: if a two-state agreement on 1967 lines with limited return to Palestine (not Israel) were implemented, would the reading''s proponents accept it as fulfillment? If yes, coordination and extraction are separable; if no, extraction is structural to the reading.',
    'If inseparable, the reading is a snare disguised as a rope/tangled_rope — the coordination vocabulary is cover for maximalist extraction. If separable, the current tangled_rope classification holds with lower extraction potential.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the reading''s coordination and extraction components are structurally separable or fused.').

omega_variable(
    refugee_agency_paradox,
    'The reading''s primary moral beneficiaries (refugees) are structurally excluded from agency. Does this exclusion constitute internalized suppression (refugees accept leadership that cannot deliver) or structural suppression (host states, PLO, UNRWA block integration)?',
    'Post-exit trajectory analysis: if refugees in host states with civil rights (e.g., Jordanian citizens of Palestinian origin) still demand return, suppression is internalized/identity-locked. If refugees in states denying rights (Lebanon) would integrate given the option, suppression is structural.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target population carries the suppression internally. This would increase effective extraction for the refugee seat and support snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_agency_paradox, empirical, 'Structural vs. internalized suppression mechanism for the excluded refugee population.').

omega_variable(
    temporal_scope_legitimacy,
    'Why does the reading restrict legitimacy to the ''modern period (19th-20th centuries)''? Is this a principled epistemic boundary (modern international law begins ~19th century) or a strategic boundary that excludes pre-19th century Jewish presence and post-20th century Israeli demographic reality?',
    'Compare with other self-determination claims (Kosovo, South Sudan, East Timor) — do they apply similar temporal cutoffs? If the cutoff is unique to this case, it is strategic; if general, it is principled.',
    'If strategic, the reading''s claimed universalizability (coordination function) is undermined — it is a particularist claim wearing universalist clothing. This increases extraction and supports snare classification. If principled, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_scope_legitimacy, conceptual, 'Whether the modern-period temporal boundary is a principled epistemic limit or a strategic exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsl_sdr_tr_t1917, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(tsl_sdr_tr_t1947, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1947, 0.25).
narrative_ontology:measurement(tsl_sdr_tr_t1967, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1967, 0.35).
narrative_ontology:measurement(tsl_sdr_tr_t1993, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1993, 0.38).
narrative_ontology:measurement(tsl_sdr_tr_t2000, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(tsl_sdr_tr_t2024, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(tsl_sdr_be_t1917, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1917, 0.35).
narrative_ontology:measurement(tsl_sdr_be_t1947, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1947, 0.45).
narrative_ontology:measurement(tsl_sdr_be_t1967, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(tsl_sdr_be_t1993, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1993, 0.58).
narrative_ontology:measurement(tsl_sdr_be_t2000, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(tsl_sdr_be_t2024, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tsl_sdr_su_t1917, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1917, 0.4).
narrative_ontology:measurement(tsl_sdr_su_t1947, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1947, 0.55).
narrative_ontology:measurement(tsl_sdr_su_t1967, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(tsl_sdr_su_t1993, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1993, 0.68).
narrative_ontology:measurement(tsl_sdr_su_t2000, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(tsl_sdr_su_t2024, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__self_determination_reading, 0.08).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_right_of_return_implementation).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, israeli_settlement_enterprise_legitimacy).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, two_state_solution_diplomatic_framework).

% DUAL FORMULATION NOTE:
% This constraint is one member of the territorial_sovereignty_legitimacy constraint family. The kernel 'territorial_sovereignty_legitimacy' decomposes into three readings with distinct ε values: covenant_continuity_reading (lower extraction, higher coordination — Rope/Mountain from Israeli seat), existential_matrix_reading (high extraction, zero coordination — Snare from all seats), self_determination_reading (this story: substantial coordination AND extraction — Tangled Rope). The ε values differ because each reading's referent arrangement and beneficiary/victim structure differ structurally, not merely perspectivally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__self_determination_reading, institutional, 0.9).
constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__self_determination_reading, organized, 0.15).
constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__self_determination_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
