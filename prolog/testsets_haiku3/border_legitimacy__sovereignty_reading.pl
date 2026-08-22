% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_reading, []).

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
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: Border Authority via Territorial Sovereignty (Sovereignty Reading)
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty reading of the border
 *   legitimacy kernel: the state derives legitimate authority to exclude from
 *   territorial sovereignty enshrined in Westphalian doctrine and the UN
 *   Charter. In this reading, borders are not restrictions to be justified
 *   but expressions of self-determination. Citizens benefit from bounded
 *   membership; excluded migrants bear the cost of exclusion under a
 *   structure the reading treats as legitimate. The sovereignty reading
 *   coexists with freedom-of-movement and humanitarian-obligation readings
 *   held by different parties in parallel institutions. This story models the
 *   constraint AS THIS READING SEES IT — high extractiveness relative to
 *   excluded migrants, high suppression to maintain the barrier, justified
 *   enforcement. Sibling readings would author different beneficiary/victim
 *   structures and directionality profiles from their own epistemic seats.
 *
 * KEY AGENTS:
 *   - state_apparatus: Sets and enforces border policy; institutional power; treats exclusion as sovereign right
 *   - citizen_collective: Bounded political community; benefits from membership protection and resource control
 *   - excluded_migrants: Powerless; bear costs of exclusion; trapped by border enforcement machinery
 *   - asylum_seekers: Immediate temporal horizon; face deportation or limbo; trapped by legal and physical barriers
 *   - human_rights_advocates: Structurally excluded from the sovereignty reading's authority structure; would contest the legitimacy frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.68).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.71).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Border Authority via Territorial Sovereignty (Sovereignty Reading)").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, '36735ba8-3334-4e69-8248-ea4b46c5a45f').
narrative_ontology:cs_kernel_codification('36735ba8-3334-4e69-8248-ea4b46c5a45f', fixed_text).
narrative_ontology:cs_authority_grounding('36735ba8-3334-4e69-8248-ea4b46c5a45f', lineage).
narrative_ontology:cs_interpretation_layer_present('36735ba8-3334-4e69-8248-ea4b46c5a45f').
narrative_ontology:cs_reading_relation('36735ba8-3334-4e69-8248-ea4b46c5a45f', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_reading_relation('36735ba8-3334-4e69-8248-ea4b46c5a45f', border_legitimacy__humanitarian_obligation_reading, coexists_with).
narrative_ontology:cs_axiom('36735ba8-3334-4e69-8248-ea4b46c5a45f', foundational, territorial_sovereignty_constitutive).
narrative_ontology:cs_axiom_status(territorial_sovereignty_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('36735ba8-3334-4e69-8248-ea4b46c5a45f', territorial_sovereignty_constitutive, conventional).
narrative_ontology:cs_axiom('36735ba8-3334-4e69-8248-ea4b46c5a45f', foundational, state_exclusive_authority_over_borders).
narrative_ontology:cs_axiom_status(state_exclusive_authority_over_borders, holdable).
narrative_ontology:cs_axiom_grounding('36735ba8-3334-4e69-8248-ea4b46c5a45f', state_exclusive_authority_over_borders, deontological).
narrative_ontology:cs_axiom('36735ba8-3334-4e69-8248-ea4b46c5a45f', secondary, citizenship_as_bounded_membership).
narrative_ontology:cs_axiom_status(citizenship_as_bounded_membership, holdable).
narrative_ontology:cs_axiom_grounding('36735ba8-3334-4e69-8248-ea4b46c5a45f', citizenship_as_bounded_membership, instrumental).
narrative_ontology:cs_reference_frame('36735ba8-3334-4e69-8248-ea4b46c5a45f', westphalian_sovereignty_doctrine).
narrative_ontology:cs_drift_state('36735ba8-3334-4e69-8248-ea4b46c5a45f', contemporary_postcolonial_migration_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('36735ba8-3334-4e69-8248-ea4b46c5a45f', '2026-06-12T14:33:22Z').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, state_sovereignty_framework).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizen_collective).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces border policy, issues visas, controls who crosses. In the sovereignty reading, this power is understood as legitimate territorial authority. The state maintains enforcement infrastructure (border agencies, surveillance, deportation systems) and justifies exclusion as necessary for sovereign function.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Citizens benefit from protection of membership status, preferential access to jobs, social benefits, and political voice. The sovereignty reading frames their interests as legitimate collective self-determination. They maintain the political support (voting, tax compliance) that enables the state to enforce borders.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizen_collective, beneficiary,
    organized, generational, mobile, national).

% Systematically denied access to territory, legal status, and economic opportunity. They bear the cost of exclusion without voice in policy-setting. Their exit from the constraint is impossible — borders are planetary; there is no 'outside' the system to retreat to.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Face immediate threats (persecution, violence, climate disaster) and seek refuge. The sovereignty reading evaluates their claims as discretionary — the state may or may not admit them. Rejected claims result in deportation to danger, detention, or indefinite waiting in border zones. Stakes are existential; exit options are zero.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, asylum_seekers, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__sovereignty_reading, asylum_seekers, excluded).

% Experience similar migration pressure and enforce similar borders. They operate within the same sovereignty doctrine but their interests diverge when migrants rejected by one state seek entry to another. They observe the constraint from a structurally symmetric position.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, neighboring_states, observer,
    institutional, generational, constrained, regional).

% The UN Charter and Westphalian doctrine ground sovereignty and territorial integrity as organizing principles. The 1951 Refugee Convention carves out narrow exceptions (non-refoulement) but affirms state discretion as primary. This non-agent entity vindicates the sovereignty reading's legitimacy frame.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_legal_order, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(border_legitimacy__sovereignty_reading, international_legal_order).

% Would argue that freedom of movement and human dignity override sovereign discretion. They are structurally excluded from the sovereignty reading's authority structure — their claims are heard in parallel forums (human rights courts, NGO campaigns) but do not shape this reading's decision-making. Their exclusion is the boundary-marking mechanism the constraint relies on.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, human_rights_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__sovereignty_reading, state_apparatus).
narrative_ontology:fixing_cost_class(border_legitimacy__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables stable political community by allowing citizens to form bounded collectives with control over membership, resource distribution, and cultural reproduction. Solves the collective-action problem of maintaining a state without the chaos of unlimited entry claiming equal membership rights.
% TRANSFER_FUNCTION: Moves the benefit of territorial access and membership rights from excluded non-citizens to citizens. Non-citizens are required to bear the cost of exclusion (separation, opportunity denial, deportation risk) to sustain citizens' privileged access. In the sovereignty reading, this transfer is justified as necessary for legitimate state function.
% ABSENT_VOICES: Excluded migrants and asylum seekers have no seat at the table where border policy is set. Human rights advocates and cosmopolitan philosophers are structurally excluded — their objections are heard in parallel institutions but do not bind the sovereign authority. The excluded parties would frame border control as rights violation rather than legitimate sovereignty exercise, but their voice is not present in the authority structure that this reading affirms.
% DISAPPEARANCE_RATIONALE: If border authority disappeared overnight (borders became open), the political structure of nation-states would transform irreversibly. Citizens would lose enforceable membership rights, resource claims, and cultural boundaries. The redistributive state would face unlimited claims on social benefits. International law would require renegotiation from its foundations. The disappearance would be civilization-altering, not merely administrative adjustment.
% FOUNDING_PROBLEM: After decolonization and the dissolution of empire, the international order needed a principle to organize political authority into discrete territorial units with enforceable boundaries. Territorial sovereignty and the right to exclude provided that organizing principle.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus and most international law scholars attest the founding problem is live — sovereignty remains the operational principle of international relations. Critical voices (humanitarian advocates, postcolonial theorists, cosmopolitan philosophers) attest the founding problem is either solved (borders no longer necessary for survival) or was always illegitimate (based on colonial imposition, not universal principle). Legislative debates in liberal democracies increasingly surface this contest; the corroboration is genuinely split.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint transfers access rights, economic opportunity, and legal protection from non-citizens to citizens in a systematic, ongoing pattern. The transfer is not incidental — it is the constraint's central function. Suppression is similarly high (0.71) because the state must maintain active enforcement machinery (border patrol, visa denial, deportation) to sustain the exclusion against the pressure of excluded populations. Theater is moderate (0.42) because sovereignty discourse legitimates the enforcement as justice, not coercion, but the machinery itself is increasingly visible (detention centers, biometric databases, surveillance) — performative justification is required to maintain the reading. Accessibility of alternatives collapses substantially (0.72) once the constraint is understood: excluded migrants cannot simply 'go around' territorial borders; the barrier is planetary in scope. The measurement series shows monotonic increase in extractiveness, suppression, and theater from 1945–2025, tracking the hardening of border enforcement and the intensification of migration pressure. The leveled coercion grid shows stakes_inflation is highest at the individual level (immigrants' cost of exclusion rises as borders harden and global inequality deepens), while suppression rises evenly across levels (the institutional machinery scales up). Resistance grows at class level (migrant advocacy, labor organizing, humanitarian campaigns) faster than at organizational or structural levels.
 *
 * PERSPECTIVAL GAP:
 *   From the state's position (agenda_setter seat), the constraint appears as legitimate self-determination grounded in sovereignty — the state does not experience itself as extracting from migrants, but as exercising authority over its own territory. From the excluded migrant's position (payer seat), the identical structure appears as coercive exclusion from basic life opportunities — no rational consent, maximum suppression. From the citizen's position (beneficiary seat), the constraint is protection of in-group interests formulated as justice. The engine should compute sharply different type classifications across these seats: the state's seat may compute as rope (coordination + minimal extraction), the migrant's seat as snare (pure extraction, no coordination), the citizen's seat as mountain-adjacent (natural, unchangeable). These divergences should be visible in per-seat type outputs; the divergence IS the measurement we are taking.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus: d near 0.0 (full beneficiary) — controls the rules, sets enforcement level, collects legitimacy. Citizen collective: d near 0.2–0.3 (strong beneficiary) — benefits from protected access and resource control, but also bears costs of maintaining enforcement apparatus and potential international friction. Excluded migrants: d near 0.95–1.0 (full target) — bear the entire cost of exclusion, have no voice in policy-setting, are trapped by legal and physical barriers. Asylum seekers: d near 1.0 (full target + acute temporal pressure) — immediate danger, maximal suppression. Human rights advocates: d near 0.8 (strong target in this reading) — they are excluded from the decision-making authority and their claims are structurally delegitimized by the reading. The coercion grid amplifies individual-level stakes (d rises) while the suppression machinery scales at all levels; this creates asymmetric directionality intensification.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty reading avoids mis-labeling by declaring both real coordination (bounded political community, stable membership, collective resource distribution) AND asymmetric extraction (systematic exclusion of non-citizens from territorial access and economic opportunity). The tangled rope classification requires all three elements: beneficiaries (citizens, state), victims (excluded migrants), and active enforcement (border patrol, visa controls, deportation). Mandatrophy is NOT present here — the founding problem (organizing post-colonial international order into sovereign territories) remains live as a contested claim, not a dead function. The constraint continues to serve the coordination function of sustaining nation-states, even though the reading is increasingly contested. The measurement series shows the constraint is gaining power, not withering.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_doctrine_legitimacy,
    'Is Westphalian territorial sovereignty a self-evident principle grounded in natural law or a constructed doctrine that benefits states and citizens at the expense of excluded populations?',
    'Genealogical analysis of how sovereignty doctrine emerged from European power consolidation; comparison with pre-Westphalian and non-Western approaches to territorial authority; examination of whether sovereignty requires exclusion or could be compatible with freedom of movement.',
    'If constructed, the doctrine loses its claim to naturalness and ε should be classified higher (approaching snare). If natural, ε classification as tangled rope with legitimate enforcement holds. The answer determines whether the constraint is foundational political architecture or institutional extractive cover story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_doctrine_legitimacy, conceptual, 'Whether territorial sovereignty is natural or constructed — foundational to reading legitimacy.').

omega_variable(
    alternatives_to_sovereign_exclusion,
    'Could the coordination functions that sovereignty serves (stable political community, resource redistribution, cultural reproduction) be achieved without categorical exclusion of non-citizens?',
    'Thought experiments with open borders in specific institutional contexts; analysis of historical periods with more porous movement (pre-national empires, early modernity); natural experiments from regional open-border agreements (Schengen) showing what coordination functions survive without sovereignty-based exclusion.',
    'If coordination functions are achievable without exclusion, extractiveness is higher than claimed (the coordination is cover story; pure extraction remains). If exclusion is structurally necessary for coordination, the extraction is genuinely coupled to coordination and tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternatives_to_sovereign_exclusion, empirical, 'Whether border exclusion is necessary for the coordination functions sovereignty claims to serve.').

omega_variable(
    reading_incommensurability_in_authority_structure,
    'Can the sovereignty reading and the freedom-of-movement reading coexist within a single institutional authority structure, or do they foreclose each other?',
    'Institutional design analysis: test whether legal or constitutional frameworks can hold both principles simultaneously (e.g., can a state recognize a sovereign right to exclude AND a human right to move?); examine jurisdictions that have attempted both (EU, national constitutions with contradictory provisions).',
    'If incommensurable (foreclose each other), the reading_relations should be ''forecloses'' not ''coexists_with'' — one reading structurally displaces the other in any unified decision-making authority. If commensurable or held by different seats, ''coexists_with'' holds. This affects how the engine models constraint family interactions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_incommensurability_in_authority_structure, conceptual, 'Whether sibling readings are logically incompatible or can coexist in institutional practice.').

omega_variable(
    extraction_intensity_and_suppression_causality,
    'Does the increasing suppression requirement (measured over 1945–2025) cause the increasing extractiveness, or are they independent trends?',
    'Counterfactual analysis: if enforcement were relaxed (visa controls eased, deportations declined), would extractiveness fall, or would citizens find alternative mechanisms to maintain exclusion? Historical comparison across low-enforcement and high-enforcement periods.',
    'If suppression causes extractiveness, the constraint is fragile and dependent on enforcement intensity — removal of enforcement would decompose it. If independent, extractiveness would persist even with enforcement relaxation, suggesting the constraint has normative weight beyond coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_intensity_and_suppression_causality, empirical, 'Whether increasing suppression enables or is independent of increasing extraction.').

omega_variable(
    internalized_vs_structural_suppression,
    'How much of the measured suppression on excluded migrants is structural (external barriers: visa laws, deportation, border walls) versus internalized (migrants'' learned helplessness, belief in the legitimacy of exclusion)?',
    'Post-exit trajectory analysis: if migrants who successfully cross borders and establish legal standing show suppression persistence (believing they ''shouldn''t'' have rights), suppression is partially internalized. If suppression drops upon exit, it was purely structural. Comparison of first-generation and second-generation migrants'' subjective experience of constraint.',
    'If substantially internalized, the constraint persists through normative capture in addition to enforcement machinery. If purely structural, removing enforcement would decompose the constraint. Internalization affects interpretation of theater_ratio (whether performative justification has colonized migrant consciousness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Decomposition of suppression into structural and internalized components.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1945, border_legitimacy__sovereignty_reading, theater_ratio, 1945, 0.22).
narrative_ontology:measurement(bord_tr_t1965, border_legitimacy__sovereignty_reading, theater_ratio, 1965, 0.26).
narrative_ontology:measurement(bord_tr_t1985, border_legitimacy__sovereignty_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(bord_tr_t2005, border_legitimacy__sovereignty_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(bord_tr_t2015, border_legitimacy__sovereignty_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(bord_tr_t2025, border_legitimacy__sovereignty_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t1945, border_legitimacy__sovereignty_reading, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement(bord_be_t1965, border_legitimacy__sovereignty_reading, base_extractiveness, 1965, 0.52).
narrative_ontology:measurement(bord_be_t1985, border_legitimacy__sovereignty_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(bord_be_t2005, border_legitimacy__sovereignty_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(bord_be_t2015, border_legitimacy__sovereignty_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(bord_be_t2025, border_legitimacy__sovereignty_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1945, border_legitimacy__sovereignty_reading, suppression_requirement, 1945, 0.38).
narrative_ontology:measurement(bord_su_t1965, border_legitimacy__sovereignty_reading, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement(bord_su_t1985, border_legitimacy__sovereignty_reading, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement(bord_su_t2005, border_legitimacy__sovereignty_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(bord_su_t2015, border_legitimacy__sovereignty_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(bord_su_t2025, border_legitimacy__sovereignty_reading, suppression_requirement, 2025, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1945, tn=2025
narrative_ontology:measurement(bord_grid_01, border_legitimacy__sovereignty_reading, accessibility_collapse(class), 1945, 0.48).
narrative_ontology:measurement(bord_grid_02, border_legitimacy__sovereignty_reading, accessibility_collapse(class), 2025, 0.72).
narrative_ontology:measurement(bord_grid_03, border_legitimacy__sovereignty_reading, accessibility_collapse(individual), 1945, 0.55).
narrative_ontology:measurement(bord_grid_04, border_legitimacy__sovereignty_reading, accessibility_collapse(individual), 2025, 0.7).
narrative_ontology:measurement(bord_grid_05, border_legitimacy__sovereignty_reading, accessibility_collapse(organizational), 1945, 0.58).
narrative_ontology:measurement(bord_grid_06, border_legitimacy__sovereignty_reading, accessibility_collapse(organizational), 2025, 0.75).
narrative_ontology:measurement(bord_grid_07, border_legitimacy__sovereignty_reading, accessibility_collapse(structural), 1945, 0.62).
narrative_ontology:measurement(bord_grid_08, border_legitimacy__sovereignty_reading, accessibility_collapse(structural), 2025, 0.78).
narrative_ontology:measurement(bord_grid_09, border_legitimacy__sovereignty_reading, resistance(class), 1945, 0.28).
narrative_ontology:measurement(bord_grid_10, border_legitimacy__sovereignty_reading, resistance(class), 2025, 0.62).
narrative_ontology:measurement(bord_grid_11, border_legitimacy__sovereignty_reading, resistance(individual), 1945, 0.25).
narrative_ontology:measurement(bord_grid_12, border_legitimacy__sovereignty_reading, resistance(individual), 2025, 0.58).
narrative_ontology:measurement(bord_grid_13, border_legitimacy__sovereignty_reading, resistance(organizational), 1945, 0.32).
narrative_ontology:measurement(bord_grid_14, border_legitimacy__sovereignty_reading, resistance(organizational), 2025, 0.52).
narrative_ontology:measurement(bord_grid_15, border_legitimacy__sovereignty_reading, resistance(structural), 1945, 0.35).
narrative_ontology:measurement(bord_grid_16, border_legitimacy__sovereignty_reading, resistance(structural), 2025, 0.48).
narrative_ontology:measurement(bord_grid_17, border_legitimacy__sovereignty_reading, stakes_inflation(class), 1945, 0.62).
narrative_ontology:measurement(bord_grid_18, border_legitimacy__sovereignty_reading, stakes_inflation(class), 2025, 0.78).
narrative_ontology:measurement(bord_grid_19, border_legitimacy__sovereignty_reading, stakes_inflation(individual), 1945, 0.7).
narrative_ontology:measurement(bord_grid_20, border_legitimacy__sovereignty_reading, stakes_inflation(individual), 2025, 0.82).
narrative_ontology:measurement(bord_grid_21, border_legitimacy__sovereignty_reading, stakes_inflation(organizational), 1945, 0.48).
narrative_ontology:measurement(bord_grid_22, border_legitimacy__sovereignty_reading, stakes_inflation(organizational), 2025, 0.64).
narrative_ontology:measurement(bord_grid_23, border_legitimacy__sovereignty_reading, stakes_inflation(structural), 1945, 0.52).
narrative_ontology:measurement(bord_grid_24, border_legitimacy__sovereignty_reading, stakes_inflation(structural), 2025, 0.68).
narrative_ontology:measurement(bord_grid_25, border_legitimacy__sovereignty_reading, suppression(class), 1945, 0.42).
narrative_ontology:measurement(bord_grid_26, border_legitimacy__sovereignty_reading, suppression(class), 2025, 0.72).
narrative_ontology:measurement(bord_grid_27, border_legitimacy__sovereignty_reading, suppression(individual), 1945, 0.4).
narrative_ontology:measurement(bord_grid_28, border_legitimacy__sovereignty_reading, suppression(individual), 2025, 0.75).
narrative_ontology:measurement(bord_grid_29, border_legitimacy__sovereignty_reading, suppression(organizational), 1945, 0.35).
narrative_ontology:measurement(bord_grid_30, border_legitimacy__sovereignty_reading, suppression(organizational), 2025, 0.65).
narrative_ontology:measurement(bord_grid_31, border_legitimacy__sovereignty_reading, suppression(structural), 1945, 0.38).
narrative_ontology:measurement(bord_grid_32, border_legitimacy__sovereignty_reading, suppression(structural), 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_legitimacy__sovereignty_reading, 0.18).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__humanitarian_obligation_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, nation_state_legitimacy).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, citizen_membership_rights).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the border_legitimacy kernel. The freedom_of_movement_reading and humanitarian_obligation_reading are structurally distinct constraints with different ε values, beneficiary/victim sets, and type classifications, even though all three describe aspects of the same border control machinery. The kernel contest is whether territorial sovereignty (this reading), human rights (freedom_of_movement), or humanitarian duty (humanitarian_obligation) legitimates border control. Each reading instantiates a different constraint because ε is reading-indexed. All three must be compiled to model the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_legitimacy__sovereignty_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
