% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__settler_colonial_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Zionist Settlement and Palestinian Dispossession (Settler-Colonial Reading)
 *   domain: political/nationalism/postcolonial
 *
 * SUMMARY:
 *   This constraint story instantiates the settler-colonial reading of the
 *   Jewish sovereignty in Palestine kernel. It treats Zionist immigration and
 *   state-building as a European-style settler-colonial project: a
 *   demographic-engineering enterprise that displaces an indigenous
 *   population to establish a replacement society, backed initially by
 *   British imperial power and subsequently by U.S. hegemony. The reading
 *   insists that refugee status of Jewish immigrants does not alter the
 *   structural function of their settlement — displacement is displacement
 *   regardless of the settlers' intent or victimhood. The kernel is
 *   contested; sibling readings (liberal nationalist, religious Zionist,
 *   cultural Zionist, post-Zionist) disagree on the structural
 *   characterization. This story authors ONE reading only — the
 *   settler-colonial frame — with its own ε, beneficiaries, victims, and
 *   metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.85).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.9).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Zionist Settlement and Palestinian Dispossession (Settler-Colonial Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political/nationalism/postcolonial").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, '7828a5fe-e00b-45e7-867b-aa5ff8ce0a86').
narrative_ontology:cs_kernel_codification('7828a5fe-e00b-45e7-867b-aa5ff8ce0a86', formalized).
narrative_ontology:cs_authority_grounding('7828a5fe-e00b-45e7-867b-aa5ff8ce0a86', extraction).
narrative_ontology:cs_interpretation_layer_present('7828a5fe-e00b-45e7-867b-aa5ff8ce0a86').
narrative_ontology:cs_reading_relation('7828a5fe-e00b-45e7-867b-aa5ff8ce0a86', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7828a5fe-e00b-45e7-867b-aa5ff8ce0a86', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7828a5fe-e00b-45e7-867b-aa5ff8ce0a86', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('7828a5fe-e00b-45e7-867b-aa5ff8ce0a86', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('7828a5fe-e00b-45e7-867b-aa5ff8ce0a86', foundational, settler_colonial_structure_invariance).
narrative_ontology:cs_axiom_status(settler_colonial_structure_invariance, holdable).
narrative_ontology:cs_axiom_grounding('7828a5fe-e00b-45e7-867b-aa5ff8ce0a86', settler_colonial_structure_invariance, empirically_contingent).
narrative_ontology:cs_axiom('7828a5fe-e00b-45e7-867b-aa5ff8ce0a86', foundational, displacement_as_structural_function).
narrative_ontology:cs_axiom_status(displacement_as_structural_function, holdable).
narrative_ontology:cs_axiom_grounding('7828a5fe-e00b-45e7-867b-aa5ff8ce0a86', displacement_as_structural_function, empirically_contingent).
narrative_ontology:cs_reference_frame('7828a5fe-e00b-45e7-867b-aa5ff8ce0a86', mandate_colonial_settlement).
narrative_ontology:cs_drift_state('7828a5fe-e00b-45e7-867b-aa5ff8ce0a86', contemporary_occupation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7828a5fe-e00b-45e7-867b-aa5ff8ce0a86', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, british_imperial_interests).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, us_imperial_interests).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, zionist_institutions).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_population).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__settler_colonial_reading, settler_colonial_structure_invariance).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__settler_colonial_reading, displacement_as_structural_function).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to ongoing territorial dispossession, military occupation, and legal exclusion. The constraint structures their statelessness and limits exit to flight or submission. Resistance is criminalized; international advocacy is constrained by donor conditionality.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_population, payer,
    moderate, generational, trapped, national).

% Denied return by the same legal architecture that enables Jewish immigration. Their exclusion is the mirror image of the Law of Return. Stateless across generations; exit from refugeehood requires the very political settlement the constraint prevents.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Receive citizenship, land, and state subsidies through the Law of Return and settlement infrastructure. Many arrive as refugees from persecution, yet their settlement structurally enacts displacement. Exit from the beneficiary position requires rejecting the national project that defines their belonging.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants, payer).

% The Balfour Declaration and Mandate policy positioned Zionist settlement as a strategic asset for British control of the Suez corridor and regional influence. The constraint's early enforcement was British military and administrative power.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, british_imperial_interests, beneficiary,
    institutional, civilizational, arbitrage, global).

% Post-1967, U.S. diplomatic, military, and financial support sustains the constraint as a regional anchor. The displacement regime is tolerated and funded because it serves U.S. power projection; Palestinian dispossession is the externalized cost.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, us_imperial_interests, beneficiary,
    institutional, civilizational, arbitrage, global).

% Jewish Agency, WZO, and later Israeli state institutions administer immigration, land allocation, and settlement policy. They set the agenda of territorial expansion and demographic engineering. They benefit materially and politically but also bear the cost of maintaining the enforcement apparatus.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, zionist_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Jewish critics who reject the settler-colonial structure are marginalized within Jewish communal institutions and often accused of bad faith. Their exclusion is internal to the constituency the constraint claims to represent.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, anti_zionist_jewish_voices, excluded,
    moderate, biographical, identity_locked, global).

% UN bodies, ICJ, ICC, and human rights NGOs document the constraint's operation as occupation, apartheid, and population transfer. Their assessments carry legal weight but no enforcement power; the constraint persists despite their verdicts.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, international_legal_observers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates Jewish demographic concentration and territorial control in Palestine through a unified legal-administrative-military apparatus. It solves the collective-action problem of establishing and maintaining a Jewish-majority state on land with a non-Jewish majority by centralizing immigration, land expropriation, and security under a single sovereign authority.
% TRANSFER_FUNCTION: Moves land, water, residency rights, and political sovereignty from the Palestinian population to Jewish immigrants and the Israeli state. The transfer is zero-sum: every dunam settled, every residency granted, every vote counted reduces the Palestinian share. The colonial metropole (Britain, then U.S.) receives strategic leverage and regional stability on its terms.
% ABSENT_VOICES: Palestinian refugees in diaspora (denied representation in the polity that governs their fate), anti-Zionist Jewish voices (excluded from communal representation), and the pre-1948 Palestinian political leadership (eliminated by the Nakba). They would object to the displacement regime but are structurally absent from the decision-making arena.
% DISAPPEARANCE_RATIONALE: If the settlement-displacement constraint vanished overnight, the legal architecture of the Law of Return, the Absentees' Property Law, the military occupation, and the settlement enterprise would collapse. Palestinian return would become legally possible; the demographic engineering would reverse; the regional strategic calculus for the U.S. and former colonial powers would fundamentally shift. The world rearranges because the constraint IS the arrangement.
% FOUNDING_PROBLEM: The founding problem, per this reading, was not Jewish statelessness per se but the European imperial need for a loyal settler colony in the strategic Levant. The Balfour Declaration solved Britain's problem of securing the Suez approaches and dividing the Arab world; Zionist institutions accepted the partnership to achieve statehood. The Jewish refugee crisis was real but was instrumentalized by a structure whose logic was colonial from inception.
% FOUNDING_PROBLEM_CORROBORATION: British Cabinet minutes (1917), Colonial Office correspondence, and the Peel Commission report attest to the imperial-strategic calculus. Palestinian and anti-colonial Jewish witnesses (e.g., Judah Magnes, Edward Said, Ilan Pappé) corroborate that the displacement function was structural, not incidental. No corroborating source outside the Zionist beneficiary institutions supports the claim that the founding problem remains live.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint's operation is zero-sum territorial replacement: every unit of land, water, and political right transferred to Jewish immigrants is extracted from Palestinians. Suppression is very high (0.9) because the displacement regime requires continuous military enforcement, legal exclusion (denial of return), and political repression to persist. Theater is low-moderate (0.25) — the security and 'right to exist' framing performs some legitimating function, but the enforcement apparatus is overwhelmingly functional for displacement, not performative. The measurement grid uses a shared time axis (1917-2024) with seven points capturing Balfour, Arab Revolt, Nakba, 1967 occupation, Oslo, Second Intifada, and present.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat types from the structural data. The Palestinian payer seats should compute as snare (high χ, trapped). The Jewish immigrant seat should compute as tangled_rope (dual beneficiary/payer, constrained exit). Imperial beneficiary seats compute as rope (coordination for them, extraction externalized). The agenda-setter seat computes as the enforcement nexus. The observer seat computes as analytical (d=0.5 by definition). The claimed type (tangled_rope) reflects the constraint's hybrid character: genuine coordination of Jewish demographic concentration AND asymmetric extraction from Palestinians.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit structure. Palestinian population and refugees are structural payers (d → 1.0): trapped, identity-locked to the land, no exit from the constraint's logic. Jewish immigrants are dual-positioned: beneficiaries of citizenship and land (d → 0.0 for those benefits) but payers of the moral and political cost of enacting displacement (d → 0.5+ for that dimension). British and U.S. imperial interests are pure beneficiaries (d → 0.0) — they extract strategic value with minimal cost. Zionist institutions are agenda-setters with arbitrage exit (they could change the policy but choose not to). Anti-Zionist Jewish voices are identity-locked excluded — their Jewish identity makes exit from the constituency impossible, but their dissent is structurally silenced.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (imperial strategic need for a settler colony) is dead — British empire gone, Cold War over, U.S. hegemony shifting. Yet the constraint persists and intensifies. This is not mandatrophy in the simple sense (a once-useful coordination now obsolete) because the constraint was never primarily coordination for its victims. The extraction function remains live for current beneficiaries (U.S. regional strategy, Israeli state interests). The mandate has not atrophied; it has been successfully repurposed. The declaration of founding_problem_status=dead with disappearance_verdict=world_rearranges flags a zombie constraint: the original justification is gone but the arrangement rearranges the world if removed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_structure,
    'Does the refugee status and persecution history of Jewish immigrants alter the structural classification of their settlement as displacement, or is the settler-colonial function invariant to intent?',
    'Comparative analysis of other settler colonies where settlers were also refugees (e.g., Pied-Noirs in Algeria, Germans in Eastern Europe post-WWII). If structural function is invariant, intent is analytically irrelevant to classification.',
    'If intent is irrelevant, the constraint remains tangled_rope/snare regardless of Jewish immigrant victimhood. If intent modulates structure, the constraint may decompose into a constraint family with distinct sub-constraints for refugee-driven vs. ideological settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_structure, conceptual, 'Whether structural classification respects agent intent or only outcome.').

omega_variable(
    indigeneity_contestation,
    'Is the Palestinian population correctly characterized as ''indigenous'' in the settler-colonial analytic sense, given Jewish historical presence and competing indigeneity claims?',
    'Engage the genealogical criteria of indigeneity in postcolonial theory (pre-colonial continuity, distinct culture, self-identification, marginalization by settler state) against the historical record of both populations.',
    'If Palestinian indigeneity is contested, the settler-colonial framing loses its primary structural anchor. If affirmed, the classification holds. This is the core empirical-conceptual hinge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigeneity_contestation, conceptual, 'Whether the settler-colonial analytic applies to the Palestinian-Zionist case.').

omega_variable(
    imperial_beneficiary_shift,
    'Did the beneficiary shift cleanly from British to U.S. imperial interests, or did the constraint''s beneficiary structure fragment into multiple competing imperial/regional patrons?',
    'Trace diplomatic, military, and financial flows 1948-2024. Test whether a single metropole successor exists or whether the constraint now serves a coalition of patrons (U.S., EU, Gulf states, Russia) with divergent interests.',
    'A fragmented beneficiary structure would suggest the constraint is evolving toward piton (multiple weak beneficiaries, no single maintainer) or a multi-metropole tangled_rope. A clean shift supports the single-metropole colonial continuity thesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_beneficiary_shift, empirical, 'Whether the colonial metropole relationship is continuous or fragmented.').

omega_variable(
    kernel_reading_relations,
    'What is the structural relationship between this settler-colonial reading and its sibling readings of the jewish_sovereignty_palestine kernel?',
    'Map the logical and institutional relations: does this reading foreclose the liberal nationalist reading (cannot both be true in one framework), coexist with it (different parties hold each), or influence it (create pressure without foreclosure)? Same for religious, cultural, and post-Zionist readings.',
    'Determines cs_structure.reading_relations. Foreclosure implies one reading''s victory eliminates others; coexistence implies stable pluralism; influence implies dynamic interaction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relations among kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1917, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(jewi_tr_t1936, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1936, 0.15).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1967, 0.22).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1993, 0.24).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1917, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1917, 0.45).
narrative_ontology:measurement(jewi_be_t1936, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1936, 0.55).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1993, 0.82).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2000, 0.84).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1917, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1917, 0.6).
narrative_ontology:measurement(jewi_su_t1936, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1936, 0.7).
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1948, 0.85).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1967, 0.88).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1993, 0.89).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__settler_colonial_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__settler_colonial_reading, 0.15).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the jewish_sovereignty_palestine kernel. The ε values differ substantially: this reading assesses the standing displacement arrangement at ε=0.85; the liberal nationalist reading would assess the same referent (the standing arrangement) at a lower ε by disputing the extraction characterization; the religious Zionist reading would assess ε near zero by treating the arrangement as divine fulfillment. Each reading instantiates a different constraint with its own stakeholders and metrics. They are linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__settler_colonial_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
