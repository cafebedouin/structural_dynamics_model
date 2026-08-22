% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Two-State Coexistence Reading of Dual Territorial Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This story authors ONE reading of a contested kernel: the
 *   territorial_legitimacy_dual kernel, instantiated here as the
 *   two_state_coexistence_reading. This reading grants 1948 legitimacy to
 *   both national movements, treats the 1967 boundaries as the basis for
 *   partition, limits Palestinian right of return to the territory of a
 *   prospective Palestinian state (not to pre-1948 homes inside Israel), and
 *   substitutes negotiated security cooperation for zero-sum territorial
 *   competition. Two sibling readings exist as SEPARATE constraint stories,
 *   not as alternatives folded into this one: zionist_refuge_reading
 *   (Israel's legitimacy grounded in historical persecution and UN partition
 *   acceptance, without conceding equivalent Palestinian territorial
 *   legitimacy) and palestinian_autochthony_reading (Palestinian legitimacy
 *   grounded in continuous habitation and unrestricted right of return,
 *   without conceding 1948 Israeli legitimacy). This story's ε (0.58) is
 *   authored for the two-state coexistence reading's own standing arrangement
 *   — the diplomatic framework as actually practiced, including its
 *   decades-long non-implementation — not for either sibling's endorsed
 *   alternative and not averaged across readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.58).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.62).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence Reading of Dual Territorial Legitimacy").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, 'ebca66dc-f1ee-4084-9a78-9e2380dc9e86').
narrative_ontology:cs_kernel_codification('ebca66dc-f1ee-4084-9a78-9e2380dc9e86', distributed).
narrative_ontology:cs_authority_grounding('ebca66dc-f1ee-4084-9a78-9e2380dc9e86', distributed).
narrative_ontology:cs_reading_relation('ebca66dc-f1ee-4084-9a78-9e2380dc9e86', territorial_legitimacy_dual__zionist_refuge_reading, influences).
narrative_ontology:cs_reading_relation('ebca66dc-f1ee-4084-9a78-9e2380dc9e86', territorial_legitimacy_dual__palestinian_autochthony_reading, influences).
narrative_ontology:cs_axiom('ebca66dc-f1ee-4084-9a78-9e2380dc9e86', foundational, mutual_1948_legitimacy_recognition).
narrative_ontology:cs_axiom_status(mutual_1948_legitimacy_recognition, holdable).
narrative_ontology:cs_axiom_grounding('ebca66dc-f1ee-4084-9a78-9e2380dc9e86', mutual_1948_legitimacy_recognition, conventional).
narrative_ontology:cs_axiom('ebca66dc-f1ee-4084-9a78-9e2380dc9e86', foundational, return_right_bounded_by_partition_state).
narrative_ontology:cs_axiom_status(return_right_bounded_by_partition_state, holdable).
narrative_ontology:cs_axiom_grounding('ebca66dc-f1ee-4084-9a78-9e2380dc9e86', return_right_bounded_by_partition_state, instrumental).
narrative_ontology:cs_reference_frame('ebca66dc-f1ee-4084-9a78-9e2380dc9e86', oslo_era_negotiated_partition_baseline).
narrative_ontology:cs_drift_state('ebca66dc-f1ee-4084-9a78-9e2380dc9e86', post_2020_normalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ebca66dc-f1ee-4084-9a78-9e2380dc9e86', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, international_mediating_powers).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, regional_normalization_partners).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees_outside_1967_lines).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_in_partition_territory).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, gaza_residents_under_blockade).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, east_jerusalem_palestinian_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains sovereign continuity and international recognition west of the 1967 line while the framework offers it normalization with regional and global powers in exchange for territorial concession east of it. Can negotiate the pace and terms of any withdrawal, and can stall implementation indefinitely without bearing the framework's enforcement costs directly.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_apparatus, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_apparatus, agenda_setter).

% Gains international recognition as the legitimate representative of a prospective state bounded by 1967 lines, and administrative authority over fragments of that territory. Depends on continued negotiation status for its own legitimacy, which ties its interests to the framework's persistence even when implementation stalls.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership, agenda_setter).

% Sponsor and enforce the framework through diplomatic recognition, aid conditionality, and Security Council resolutions. Bear none of the territorial costs and derive stability, alliance management, and diplomatic capital from being seen as framework guarantors, regardless of whether the two-state outcome ever materializes.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_mediating_powers, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, international_mediating_powers, agenda_setter).

% Gulf and Arab states use nominal endorsement of the two-state framework as the acceptable public cover for normalizing relations with Israel, gaining security and economic benefits from normalization while paying no cost tied to actual Palestinian statehood.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, regional_normalization_partners, beneficiary,
    institutional, generational, arbitrage, regional).

% Descendants of those displaced in 1948 and living in Jordan, Lebanon, Syria, and diaspora communities. The framework confines any right of return to the prospective Palestinian state within 1967 boundaries, foreclosing return to pre-1948 homes and villages inside what became Israel. They have no seat in negotiations that resolve their claims by definition rather than by consent.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees_outside_1967_lines, payer,
    powerless, generational, trapped, regional).

% Communities established in territory the framework designates as the future Palestinian state. Face relocation, loss of state backing, or contested status under any implemented partition. Their political organization gives them capacity to resist implementation, which is a primary reason the framework has never been executed as designed.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_in_partition_territory, payer,
    organized, biographical, constrained, regional).

% Live under a blockade and periodic military operations while nominally part of the territory the two-state framework designates as the prospective Palestinian state. The framework's diplomatic architecture has not translated into sovereignty, freedom of movement, or economic normalcy on the ground; they bear the costs of the framework's non-implementation most acutely.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, gaza_residents_under_blockade, payer,
    powerless, immediate, trapped, local).

% Reside in territory both readings claim as a capital. Hold permanent-resident status rather than citizenship in the annexing state, face home demolitions and residency revocation, and their status remains formally unresolved under every version of the framework that has been negotiated.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, east_jerusalem_palestinian_residents, payer,
    powerless, biographical, trapped, local).

% Fund refugee services, monitor implementation, and produce reports assessing the framework's status. Take testimony from all sides and can document gaps between framework rhetoric and ground conditions, but have no enforcement power to compel implementation.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, un_agencies_and_donor_states, observer,
    institutional, generational, analytical, global).

% Palestinians and Israelis who reject partition itself — favoring either a single binational state or, on the Israeli right, permanent unilateral control — are treated as outside the diplomatically legitimate conversation, even though their numbers and political influence are substantial on both sides.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, one_state_advocates_on_both_sides, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__two_state_coexistence_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__two_state_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared diplomatic vocabulary and negotiating baseline that lets two populations with incompatible maximalist claims (all of the land, exclusively) each retain recognized legitimacy for part of the territory, avoiding perpetual zero-sum war by trading total victory for partial, mutually recognized sovereignty.
% TRANSFER_FUNCTION: Moves diplomatic recognition and negotiating leverage to the two established leaderships (Israeli state, Palestinian Authority) and to mediating powers who broker the framework, while moving the costs of unresolved implementation — statelessness, blockade, contested residency, foreclosed return — onto refugees, Gaza residents, East Jerusalem residents, and settlers whose fates are decided by the framework's terms without their direct consent.
% ABSENT_VOICES: Refugees outside the 1967-line territories have no return remedy under this reading and no seat at the negotiating table; one-state advocates on both sides are excluded from the diplomatically sanctioned conversation entirely; Gaza and East Jerusalem residents experience the framework's costs without its promised benefits and are represented, if at all, only through leaderships whose interests partially diverge from theirs.
% DISAPPEARANCE_RATIONALE: Israeli and Palestinian negotiating elites, and the international diplomatic architecture built around them, would lose their primary shared reference point and organizing framework — decades of UN resolutions, peace processes, and bilateral recognition regimes are built on this compromise language. Refugees outside 1967 lines and one-state advocates on both sides would argue the world is largely unchanged for them, since the framework has not delivered return, statehood, or resolution regardless of its formal persistence. Whether disappearance rearranges the world thus depends entirely on which seat is asked.
% FOUNDING_PROBLEM: After 1948 and again after 1967, two national movements each claimed exclusive legitimate sovereignty over overlapping territory, producing repeated war; the two-state framework was built to convert this zero-sum contest into a negotiable partition both sides' mainstream leaderships could accept as a basis for ending armed conflict.
% FOUNDING_PROBLEM_CORROBORATION: International mediating powers and both establishment leaderships attest the framework remains the live, necessary basis for resolution. Independent researchers, UN agency reporting, and refugee advocacy organizations outside the benefiting leaderships attest that continuous settlement expansion, blockade conditions, and the absence of implementation for over three decades indicate the founding problem has been formally preserved as negotiating language while the underlying territorial contest continues unresolved on the ground — a status the framework's own beneficiaries have strong institutional incentive not to acknowledge.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, contested).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate-high and rising over the measured interval (0.32 to 0.58) because the framework has functioned increasingly as a stable diplomatic equilibrium that benefits established leaderships and mediating powers while implementation for those bearing its costs (refugees, Gaza residents, East Jerusalem residents) has not advanced proportionally — the coordination function (ending total war) is real, but an increasing share of the framework's operation is rent extraction by parties whose institutional standing depends on the negotiation continuing rather than concluding. Theater ratio rises correspondingly (0.2 to 0.4) as diplomatic activity (summits, resolutions, normalization agreements) increasingly substitutes for implementation. Suppression is authored as structural, not merely rhetorical: the framework requires active diplomatic and military enforcement (blockade maintenance, settlement continuation, refugee status non-resolution) to hold its current shape rather than either collapsing toward full partition or full annexation.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state institutions and Palestinian Authority leadership are coded as beneficiaries with institutional/organized power and arbitrage/constrained exit respectively — both derive recognition and negotiating standing from the framework's persistence, even as PA leadership's exit options are more constrained than the Israeli state's, since Palestinian statehood recognition is itself contingent on the ongoing process. International mediators and regional normalization partners are pure structural beneficiaries: they gain diplomatic capital and normalization benefits with essentially no cost exposure, hence the highest d toward the beneficiary end. Refugees outside 1967 lines, Gaza residents, East Jerusalem residents, and settlers are coded as payers with the framework's terms determining their fates without their direct consent — the two-state framework structurally forecloses return to pre-1948 territory for the first group and threatens dispossession or unresolved status for the others, which is why their exit options are trapped or constrained despite differing power levels.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification protects against two mislabeling errors in either direction. Reading this purely as coordination (rope) would erase the fact that the framework's non-implementation has itself become load-bearing for several beneficiary seats — negotiation-without-resolution is not incidental drift but a structurally stable equilibrium some parties are incentivized to preserve. Reading this purely as extraction (snare) would erase the genuine coordination function the framework performs: it remains the only shared vocabulary under which the two established leaderships and the international community can jointly claim to be working toward ending the conflict, and it has coincided with periods of reduced large-scale warfare relative to pre-Oslo escalation cycles. The tangled_rope frame holds both: real coordination function, real and asymmetric extraction, both riding the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the two-state coexistence reading a genuine third position, or is it structurally a suppressed synthesis that neither the zionist_refuge_reading nor the palestinian_autochthony_reading actually endorses in practice — i.e., is it a diplomatic fiction maintained by external mediators rather than a position either party''s base holds?',
    'Survey and platform-analysis data on what share of Israeli and Palestinian political constituencies actually endorse the specific terms of this reading (1948 mutual legitimacy + 1967 partition baseline + limited return) versus treating it as a negotiating position adopted only for international audiences.',
    'If neither underlying constituency genuinely holds this reading and it exists primarily as a mediator-sustained diplomatic artifact, the coordination function claimed here is weaker than authored and the classification should shift toward snare (the coordination story is cover for mediator and elite extraction) rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this reading is a genuinely held position or a mediator-sustained diplomatic fiction.').

omega_variable(
    implementation_versus_framework_distinction,
    'Should the persistent non-implementation of the two-state framework (decades without a finalized partition) be treated as evidence against the framework''s own claimed structure, or as a separate enforcement/political failure layered on top of a structurally sound compromise?',
    'Comparative analysis of other partition frameworks that WERE implemented within a comparable timeframe, to establish whether multi-decade non-implementation is characteristic of genuine transitional frameworks or is itself diagnostic of extraction dominance.',
    'If multi-decade non-implementation is characteristic of frameworks whose extraction has come to dominate their coordination function, this supports the rising extractiveness trend authored in the measurements and the tangled_rope (not scaffold) classification; if it is typical of genuinely difficult but eventually-successful transitions, the classification might better be read as a stalled scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implementation_versus_framework_distinction, empirical, 'Whether decades of non-implementation indicates structural extraction dominance or ordinary transitional difficulty.').

omega_variable(
    right_of_return_limitation_legitimacy,
    'Is confining the right of return to the prospective Palestinian state (rather than to pre-1948 homes) a legitimate compromise term of a workable peace framework, or is it a structural extraction from refugees who never consented to this limitation being treated as binding?',
    'This is fundamentally a normative/preference question about individual property and residency rights versus collective political settlement, not resolvable by empirical data alone — it depends on which theory of displaced persons'' rights one adopts.',
    'Under a strong individual-rights reading, the return limitation constitutes ongoing extraction from refugees regardless of any collective political benefit, supporting a higher-extraction, victim-centered reading. Under a state-centered political-settlement reading, the limitation is a legitimate cost of ending an intractable conflict, more consistent with rope-leaning coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_limitation_legitimacy, preference, 'Whether limiting return to the future Palestinian state is legitimate compromise or extraction from non-consenting refugees.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(terr_tr_t10, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(terr_tr_t20, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(terr_tr_t30, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(terr_tr_t40, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(terr_tr_t56, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 56, 0.4).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(terr_be_t10, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(terr_be_t20, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(terr_be_t30, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(terr_be_t40, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(terr_be_t56, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 56, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(terr_su_t10, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(terr_su_t20, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(terr_su_t30, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(terr_su_t40, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(terr_su_t56, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 56, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__two_state_coexistence_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_autochthony_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language label 'the Israeli-Palestinian legitimacy question' into structurally distinct kernel readings sharing the territorial_legitimacy_dual kernel_id. zionist_refuge_reading authors Israeli legitimacy grounded in persecution/UN partition without symmetric Palestinian territorial concession; palestinian_autochthony_reading authors Palestinian legitimacy grounded in continuous habitation and unrestricted return without conceding 1948 Israeli legitimacy; this story (two_state_coexistence_reading) authors the mutual-recognition compromise with 1967 boundaries and limited return. Each carries its own ε, its own beneficiary/victim structure, and its own classification — none is an average or synthesis of the others at the level of ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__two_state_coexistence_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
