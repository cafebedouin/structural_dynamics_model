% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: Sovereignty-Primary Border Authority
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereignty_primary reading of the
 *   contested kernel 'border_normative_status.' It asserts that territorial
 *   boundaries are legitimate instruments of collective self-determination
 *   and that states possess foundational authority to exclude non-members.
 *   The constraint operates as a tangled_rope: it performs a genuine
 *   coordination function (stabilizing the people-territory-authority triad)
 *   while simultaneously extracting life-chances from excluded migrants,
 *   stateless persons, and denied asylum seekers through actively enforced
 *   suppression. The coordination function is real — without bounded
 *   political communities, democratic accountability and collective resource
 *   management lack a subject. But the extraction is structural and growing:
 *   the constraint now operates far beyond its founding justification,
 *   deploying militarized enforcement, extraterritorial deterrence, and
 *   citizenship-stripping against populations who never consented to the
 *   arrangement. The claim/metric independence is maintained: the reading
 *   claims tangled_rope (coordination + extraction), and the metrics describe
 *   high extraction (0.72), very high suppression (0.85), moderate theater
 *   (0.25), high accessibility collapse (0.78), and significant resistance
 *   (0.68).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.72).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.85).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.72).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Sovereignty-Primary Border Authority").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, 'f07d6a85-57ba-44a6-b995-cc0552efcca1').
narrative_ontology:cs_kernel_codification('f07d6a85-57ba-44a6-b995-cc0552efcca1', formalized).
narrative_ontology:cs_authority_grounding('f07d6a85-57ba-44a6-b995-cc0552efcca1', lineage).
narrative_ontology:cs_interpretation_layer_present('f07d6a85-57ba-44a6-b995-cc0552efcca1').
narrative_ontology:cs_reading_relation('f07d6a85-57ba-44a6-b995-cc0552efcca1', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_reading_relation('f07d6a85-57ba-44a6-b995-cc0552efcca1', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('f07d6a85-57ba-44a6-b995-cc0552efcca1', foundational, collective_self_determination_requires_exclusion).
narrative_ontology:cs_axiom_status(collective_self_determination_requires_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('f07d6a85-57ba-44a6-b995-cc0552efcca1', collective_self_determination_requires_exclusion, conventional).
narrative_ontology:cs_axiom('f07d6a85-57ba-44a6-b995-cc0552efcca1', foundational, territorial_sovereignty_entails_membership_control).
narrative_ontology:cs_axiom_status(territorial_sovereignty_entails_membership_control, holdable).
narrative_ontology:cs_axiom_grounding('f07d6a85-57ba-44a6-b995-cc0552efcca1', territorial_sovereignty_entails_membership_control, conventional).
narrative_ontology:cs_reference_frame('f07d6a85-57ba-44a6-b995-cc0552efcca1', westphalian_state_system).
narrative_ontology:cs_drift_state('f07d6a85-57ba-44a6-b995-cc0552efcca1', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f07d6a85-57ba-44a6-b995-cc0552efcca1', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, citizen_polities).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, state_apparatus).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, territorial_elites).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, stateless_persons).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, asylum_seekers_denied_entry).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, collective_self_determination_doctrine).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, state_sovereignty_primacy).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, territorial_integrity_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collective political communities that constitute states. They receive the primary benefit of the constraint: the ability to determine membership, preserve cultural-political continuity, and control collective resources. Their exit from the constraint would mean dissolving the political community itself.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, citizen_polities, beneficiary,
    organized, generational, constrained, national).

% The institutional machinery that administers border control, immigration enforcement, and citizenship adjudication. It sets the rules of exclusion, operates detention and deportation systems, and extracts administrative resources and political legitimacy from the constraint. It can reform the constraint but rarely chooses to narrow its own authority.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__sovereignty_primary, state_apparatus, beneficiary).

% Economic and political elites whose capital, status, and security depend on stable territorial control. They benefit from the constraint's protection of property regimes and labor market segmentation. Their exit options are high — they hold multiple citizenships, capital mobility, and diplomatic access.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, territorial_elites, beneficiary,
    powerful, biographical, mobile, global).

% Persons seeking entry who are denied by sovereign authority. They bear the full cost of exclusion: thwarted life plans, family separation, exposure to danger in origin or transit countries, and the psychological weight of being designated 'outside the community.' Their exit from the constraint is structurally blocked — they cannot opt out of being excluded.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Persons with no recognized nationality, for whom the sovereignty-primary border regime produces a permanent condition of rightlessness. The constraint does not merely exclude them — it produces their condition. No state claims them; all states exclude them. Exit is conceptually unavailable.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, stateless_persons, payer,
    powerless, biographical, trapped, universal).

% Persons fleeing persecution who are denied access to territory and thus to asylum procedures. The constraint treats their claims as discretionary exceptions to sovereign exclusion rather than as overriding obligations. They bear lethal costs; their exit from the constraint is blocked by the very persecution they flee.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, asylum_seekers_denied_entry, payer,
    powerless, immediate, trapped, global).

% Citizens displaced by state policies justified through border enforcement (internal displacement, denaturalization, extraterritorial detention). Their interests are treated as externalities of the sovereign border regime. They would object but are rhetorically absorbed into the 'national community' whose self-determination the constraint serves.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, displaced_citizens, excluded,
    moderate, biographical, constrained, national).

% Scholars, courts, and treaty bodies that interpret the constraint from outside its enforcement. They document the tension between sovereignty claims and human rights obligations but lack enforcement power. Their analytical seat is the only one from which the constraint's full victim structure is visible.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, international_legal_observers, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__sovereignty_primary, state_apparatus).
narrative_ontology:fixing_cost_class(border_normative_status__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable framework for collective self-governance by defining the bounded political community within which democratic deliberation, resource distribution, and legal accountability can operate. Solves the problem of 'who decides' by anchoring political authority in a territorial people.
% TRANSFER_FUNCTION: Moves the power to determine membership, control territory, and allocate collective goods from excluded non-members to the citizen polity and its state apparatus. The transfer is exclusion itself: the right to say 'no entry' is a transfer of life-chances from outsiders to insiders.
% ABSENT_VOICES: Would-be migrants who never reach the border because visas are denied, routes are blocked, or deterrence works — they are absent before the constraint even applies. Future generations who will inherit the demographic and ecological consequences of closed borders. Non-human living systems bisected by political boundaries.
% DISAPPEARANCE_RATIONALE: If sovereign exclusion authority vanished overnight, the global migration regime would reorganize around freedom of movement as a default. States would lose their primary tool for demographic engineering and labor market protection. The citizen polity's claim to exclusive self-determination would collapse into a contested negotiation with all affected persons. The world would rearrange radically.
% FOUNDING_PROBLEM: The post-Westphalian and post-colonial need to stabilize political authority in a world of contested territories and mobile populations. The constraint was built to answer: who has the right to rule this territory and this people? The sovereign border answered by making the fit between people and territory the foundation of legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: State practice and diplomatic history corroborate the founding problem (UN Charter, decolonization records). But human rights treaty bodies, migration scholars, and affected communities contest whether the founding problem still justifies the current scope of exclusion — arguing that the problem of 'stabilizing authority' has been solved for core states while the exclusion apparatus has expanded beyond its founding justification.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the constraint transfers the most fundamental life-chances — where to live, whether to be safe, whether to be recognized as a rights-holder — from powerless excluded persons to organized citizen polities and institutional state apparatuses. Suppression is very high because the constraint's persistence depends on active, violent enforcement: walls, patrols, detention camps, visa regimes, carrier sanctions, and extraterritorial pushbacks. Theater is moderate-low because the enforcement machinery is genuinely functional at exclusion, not merely performative — though the rhetoric of 'border security' increasingly masks economic protectionism and demographic engineering. Accessibility collapse is high because once the sovereign border claim is understood as legitimate, alternatives (open borders, free movement, non-territorial citizenship) become conceptually and politically inaccessible within the dominant framework. Resistance is significant because the constraint generates its own opposition: migrant rights movements, sanctuary cities, legal challenges, and the daily resistance of unauthorized presence.
 *
 * PERSPECTIVAL GAP:
 *   The citizen polity and state apparatus seats experience the constraint as coordination — the machinery that makes their collective agency possible. The excluded migrant and stateless person seats experience it as snare — the machinery that makes their rightlessness structural. The territorial elite seat experiences it as rope — a coordination service they pay for indirectly but can also bypass. The engine computes this divergence from the declared power/exit/spatial_scope data; the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizen polities are structural beneficiaries (d near 0.0): the constraint subsidizes their collective self-determination by offloading the costs of global inequality onto excluded others. The state apparatus is a dual-positioned beneficiary/agenda-setter (d ~ 0.15): it administers the constraint and extracts resources/legitimacy from it, but also bears some enforcement costs. Territorial elites are mobile beneficiaries (d ~ 0.2): they benefit from the constraint but can exit its costs. Excluded migrants, stateless persons, and denied asylum seekers are full targets (d near 1.0): they bear the extraction with trapped exit. Displaced citizens are excluded (no seat in the coordination): their displacement is treated as an externality of the very regime that claims to serve them. International legal observers are analytical (d = 0.5): they see the full structure but hold no structural position within it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stabilizing political authority in a world of contested territories) was real in 1945 and remains real for fragile states. But for the core states that dominate the global migration regime, the founding problem is substantially solved — their authority is stable, their territories recognized, their peoples constituted. The constraint persists and intensifies (extraction rising from 0.55 to 0.72 over the interval) not because the coordination problem remains acute, but because the extraction apparatus has acquired institutional inertia and political profitability. This is mandatrophy: the mandate (stabilize authority) has outlived its function for the powerful actors, but the constraint remains because it now serves extraction. The theater_ratio rise from 0.18 to 0.25 tracks this: a growing share of enforcement activity defends the extraction, not the coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_justification_vs_current_scope,
    'Does the current scope of sovereign exclusion authority (militarized borders, extraterritorial deterrence, citizenship stripping) still serve the founding coordination problem (stabilizing political authority), or has it become a self-justifying extraction apparatus?',
    'Comparative analysis of border enforcement intensity vs. measures of state authority stability across regime types. If enforcement intensity correlates with extraction indicators (remittance capture, labor market segmentation, demographic engineering) rather than authority stability indicators, the mandate has atrophied.',
    'If the mandate has atrophied for core states, the constraint reclassifies toward snare for those actors — the coordination story becomes cover for extraction. The tangled_rope classification would hold only for fragile states where the founding problem remains live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_justification_vs_current_scope, empirical, 'Whether the constraint''s current operation still tracks its founding coordination justification.').

omega_variable(
    coordination_extraction_separability,
    'Is the coordination function (bounded political community enabling democratic self-governance) structurally separable from the extraction function (sovereign exclusion of non-members from life-chances)?',
    'Counterfactual analysis: could a political community maintain democratic accountability, collective resource management, and legal accountability WITHOUT the power to exclude non-members from territory? Historical cases of relatively open borders with functioning democracies (e.g., 19th century US, Schengen area pre-2015) provide partial evidence.',
    'If separable, the extraction is not the price of coordination but a separable choice — the constraint is a tangled_rope where the rope and the snare are distinct mechanisms. If inseparable, the extraction is constitutive of the coordination — the constraint''s tangled_rope nature is structural, not contingent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components can be institutionally disentangled.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (sovereignty_primary) of the contested kernel ''border_normative_status.'' What would change structurally if the freedom_primary or qualified_sovereignty reading were instantiated instead?',
    'Author the sibling constraint stories and compare their beneficiary/victim structures, extractiveness metrics, and stakeholder seats. The structural delta is the answer.',
    'Documents the kernel contest as irreducible structural ambiguity. The engine cannot resolve which reading is ''correct'' — they instantiate different constraints with different ε values. This omega ensures the committer structure is not lost in the single-reading generation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel identity: this constraint is a reading, not the kernel itself.').

omega_variable(
    citizen_polity_coherence,
    'Is the ''citizen polity'' a coherent collective agent that benefits from the constraint, or is it a fracturing coalition whose members experience the constraint differently (e.g., native-born vs. naturalized, racialized vs. non-racialized, propertied vs. precarious)?',
    'Disaggregate the citizen_polity stakeholder by race, class, migration history, and political incorporation. Measure whether the constraint''s benefits and costs distribute evenly within the putative beneficiary group.',
    'If the citizen polity is not a coherent beneficiary, the constraint''s coordination function is internally fractured — the ''self'' in self-determination is contested. This would shift the constraint toward snare for subordinated citizen groups who are policed by the same border regime that claims to serve them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_polity_coherence, empirical, 'Whether the primary beneficiary group is structurally coherent or internally stratified by the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bnsp_tr_t1945, border_normative_status__sovereignty_primary, theater_ratio, 1945, 0.18).
narrative_ontology:measurement(bnsp_tr_t1960, border_normative_status__sovereignty_primary, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(bnsp_tr_t1975, border_normative_status__sovereignty_primary, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(bnsp_tr_t1990, border_normative_status__sovereignty_primary, theater_ratio, 1990, 0.23).
narrative_ontology:measurement(bnsp_tr_t2005, border_normative_status__sovereignty_primary, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(bnsp_tr_t2025, border_normative_status__sovereignty_primary, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(bnsp_be_t1945, border_normative_status__sovereignty_primary, base_extractiveness, 1945, 0.55).
narrative_ontology:measurement(bnsp_be_t1960, border_normative_status__sovereignty_primary, base_extractiveness, 1960, 0.58).
narrative_ontology:measurement(bnsp_be_t1975, border_normative_status__sovereignty_primary, base_extractiveness, 1975, 0.62).
narrative_ontology:measurement(bnsp_be_t1990, border_normative_status__sovereignty_primary, base_extractiveness, 1990, 0.66).
narrative_ontology:measurement(bnsp_be_t2005, border_normative_status__sovereignty_primary, base_extractiveness, 2005, 0.69).
narrative_ontology:measurement(bnsp_be_t2025, border_normative_status__sovereignty_primary, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(bnsp_su_t1945, border_normative_status__sovereignty_primary, suppression_requirement, 1945, 0.75).
narrative_ontology:measurement(bnsp_su_t1960, border_normative_status__sovereignty_primary, suppression_requirement, 1960, 0.78).
narrative_ontology:measurement(bnsp_su_t1975, border_normative_status__sovereignty_primary, suppression_requirement, 1975, 0.8).
narrative_ontology:measurement(bnsp_su_t1990, border_normative_status__sovereignty_primary, suppression_requirement, 1990, 0.82).
narrative_ontology:measurement(bnsp_su_t2005, border_normative_status__sovereignty_primary, suppression_requirement, 2005, 0.84).
narrative_ontology:measurement(bnsp_su_t2025, border_normative_status__sovereignty_primary, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(border_normative_status__sovereignty_primary, 0.08).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__qualified_sovereignty).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, global_migration_regime).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, citizenship_law).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, refugee_protection_framework).

% DUAL FORMULATION NOTE:
% This constraint is one member of the border_normative_status constraint family. The three readings (sovereignty_primary, freedom_primary, qualified_sovereignty) instantiate different constraints with different ε values, beneficiary/victim structures, and stakeholder seats. They are linked here and in the sibling stories' network.affects_constraints arrays. The ε-invariance principle requires separate stories because the coordination/extraction boundary shifts fundamentally across readings: sovereignty_primary treats exclusion as coordination; freedom_primary treats exclusion as extraction; qualified_sovereignty treats exclusion as conditionally legitimate coordination with extractive limits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_normative_status__sovereignty_primary, organized, 0.1).
constraint_indexing:directionality_override(border_normative_status__sovereignty_primary, institutional, 0.15).
constraint_indexing:directionality_override(border_normative_status__sovereignty_primary, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
