% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Self-Determination Reading of Territorial Sovereignty Legitimacy (Modern Arab Demographic Majority)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested
 *   territorial-sovereignty-legitimacy kernel: that legitimacy derives from
 *   the modern (19th-20th century) principle of self-determination as applied
 *   to the population holding demographic majority and continuous residence
 *   in the territory during that period. Under this reading, the 1947 UN
 *   Partition Plan and subsequent Israeli statehood are read as an externally
 *   imposed disruption of a rightful self-determination claim, the
 *   post-1948/1967 Israeli state is framed as a colonial-settler project
 *   overriding indigenous sovereignty, and the Palestinian right of return is
 *   framed as restoration of a legitimate status quo ante rather than a novel
 *   claim. This is generated as a single, ε-invariant constraint under Rule
 *   1: it does not adjudicate between itself and the covenant-continuity or
 *   existential-matrix readings, and does not average or hedge across them.
 *   Those are separate constraints, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - palestinian_arab_residents_and_refugees: primary bearers of the claim's unresolved status (powerless/trapped) — the reading speaks for them but cannot alone deliver restitution
 *   - palestinian_national_movement_leadership: primary institutional beneficiary (organized/constrained) — derives diplomatic leverage and legitimacy from articulating this reading
 *   - external_recognizing_states_favoring_arab_self_determination: secondary institutional beneficiary (institutional/arbitrage) — gains diplomatic capital at low direct cost
 *   - israeli_jewish_population: excluded party whose competing claim this reading's frame does not admit
 *   - third_party_mediating_states: analytical observer seat adjudicating between competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.62).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.71).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Self-Determination Reading of Territorial Sovereignty Legitimacy (Modern Arab Demographic Majority)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, '222bf999-6277-40c8-8ff6-bc530b2d0474').
narrative_ontology:cs_kernel_codification('222bf999-6277-40c8-8ff6-bc530b2d0474', distributed).
narrative_ontology:cs_authority_grounding('222bf999-6277-40c8-8ff6-bc530b2d0474', distributed).
narrative_ontology:cs_reading_relation('222bf999-6277-40c8-8ff6-bc530b2d0474', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('222bf999-6277-40c8-8ff6-bc530b2d0474', territorial_sovereignty_legitimacy__existential_matrix_reading, influences).
narrative_ontology:cs_axiom('222bf999-6277-40c8-8ff6-bc530b2d0474', foundational, modern_period_temporal_boundary_for_legitimacy).
narrative_ontology:cs_axiom_status(modern_period_temporal_boundary_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('222bf999-6277-40c8-8ff6-bc530b2d0474', modern_period_temporal_boundary_for_legitimacy, conventional).
narrative_ontology:cs_axiom('222bf999-6277-40c8-8ff6-bc530b2d0474', foundational, continuous_demographic_majority_grounds_sovereignty_claim).
narrative_ontology:cs_axiom_status(continuous_demographic_majority_grounds_sovereignty_claim, holdable).
narrative_ontology:cs_axiom_grounding('222bf999-6277-40c8-8ff6-bc530b2d0474', continuous_demographic_majority_grounds_sovereignty_claim, empirically_contingent).
narrative_ontology:cs_axiom('222bf999-6277-40c8-8ff6-bc530b2d0474', secondary, partition_as_externally_imposed_injustice).
narrative_ontology:cs_axiom_status(partition_as_externally_imposed_injustice, holdable).
narrative_ontology:cs_axiom_grounding('222bf999-6277-40c8-8ff6-bc530b2d0474', partition_as_externally_imposed_injustice, conventional).
narrative_ontology:cs_reference_frame('222bf999-6277-40c8-8ff6-bc530b2d0474', late_ottoman_mandate_demographic_baseline).
narrative_ontology:cs_drift_state('222bf999-6277-40c8-8ff6-bc530b2d0474', post_oslo_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('222bf999-6277-40c8-8ff6-bc530b2d0474', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, external_recognizing_states_favoring_arab_self_determination).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_movement_leadership).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_arab_residents_and_refugees).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, internally_displaced_1948_1967_populations).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, self_determination_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, demographic_majority_as_sovereignty_ground).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, uti_possidetis_temporal_cutoff_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Population whose claim to sovereignty rests on continuous demographic majority and residence through the late Ottoman and Mandate periods. Bears the practical cost of the reading's non-realization: displacement since 1947-48, statelessness for refugee descendants, and continued contestation of return. The reading names their legitimacy but cannot on its own deliver restitution; they carry the gap between doctrine and outcome.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_arab_residents_and_refugees, payer,
    powerless, generational, trapped, regional).

% Populations displaced within the historic territory across 1948 and 1967, whose right of return this reading treats as restoration of a rightful status quo ante. Their situation is used as evidentiary and moral ballast for the reading's legitimacy claim while remaining materially unresolved decade after decade.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, internally_displaced_1948_1967_populations, payer,
    powerless, generational, trapped, regional).

% Political and diplomatic leadership (PLO, PA, and allied factions) that articulates and deploys the self-determination reading in UN forums, international law venues, and negotiations. Derives negotiating leverage, international standing, and domestic legitimacy from the reading's persuasive force, independent of whether it converts into sovereign territory.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_movement_leadership, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_movement_leadership, agenda_setter).

% States and blocs (many in the Global South, parts of the EU, UN majority coalitions) that recognize Palestinian statehood on self-determination grounds. Gain diplomatic capital, anti-colonial solidarity credentials, and voting-bloc cohesion from endorsing the reading, at low direct cost to themselves.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, external_recognizing_states_favoring_arab_self_determination, beneficiary,
    institutional, generational, arbitrage, global).

% Population whose own sovereignty claim (grounded in covenant-continuity and existential-matrix readings) is treated by this reading's temporal cutoff and colonial framing as illegitimate or derivative. Not a party this reading is built to persuade; its counter-claims are structurally outside this reading's frame rather than adjudicated within it.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_jewish_population, excluded,
    organized, generational, constrained, regional).

% States and international bodies (US, EU, UN mediators) that must adjudicate between competing sovereignty readings in negotiations, aid conditionality, and recognition policy, without being bound to any single reading's premises.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, third_party_mediating_states, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a juridical and moral framework by which a demographically continuous population's territorial claim can be asserted and organized internationally, coordinating diplomatic recognition, legal argumentation, and refugee advocacy around a single coherent doctrine (self-determination plus demographic continuity).
% TRANSFER_FUNCTION: Moves diplomatic legitimacy, international recognition, and moral standing toward the Palestinian national movement and its external backers; moves practical territorial and political concessions away from the Israeli state's claim to the same land, and moves the burden of unresolved refugee status onto Palestinian Arab residents and refugees whose lived situation is invoked but not thereby resolved.
% ABSENT_VOICES: Israeli Jewish population and diaspora communities whose covenant-continuity and existential-matrix claims are structurally excluded from this reading's temporal and demographic frame; Mizrahi and other Jewish refugee populations displaced from Arab states in the same period, whose parallel displacement this reading does not address.
% DISAPPEARANCE_RATIONALE: If the self-determination reading vanished as an organizing legitimacy claim, Palestinian diplomatic strategy would lose its primary juridical anchor in UN forums and international law venues, external recognition campaigns would need a different basis, and negotiating positions built on demographic-majority-at-a-fixed-date arguments (right of return, 1948 borders framing) would require reconstruction on other grounds (e.g., purely humanitarian or negotiated-settlement arguments).
% FOUNDING_PROBLEM: The problem of an indigenous, demographically dominant population's sovereignty claim being displaced by external partition (1947 UN Partition Plan) and subsequent war (1948, 1967) without its consent, framed as an unresolved decolonization question analogous to other 20th-century self-determination movements.
% FOUNDING_PROBLEM_CORROBORATION: UN General Assembly resolutions and a substantial body of international law scholarship (including non-Palestinian, non-Israeli academic sources) attest the self-determination framing remains a live juridical claim under international law. Israeli governments, most Israeli legal scholars, and some Western governments dispute the framing's application to this territory, arguing competing claims (covenant-continuity, security-existential) are equally or more valid; neutral historians note the demographic-majority premise itself is contested for parts of the territory and periods.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.62 for this reading: the doctrine mobilizes real diplomatic and legal resources on behalf of a population but, as authored, has not converted into territorial sovereignty for that population across the measured interval, while the same doctrine has been used to justify suppression of intermediate settlement options (e.g., federated or partition compromises framed as illegitimate impositions) and to sustain refugee status as a persistent political asset rather than resolve it operationally. Suppression is high (0.71) because the reading is defended by active diplomatic, legal, and at times armed contestation against competing sovereignty claims, not by voluntary consensus. Theater ratio (0.30) reflects that a meaningful share of invocation is genuine legal argument (UN resolutions, ICJ proceedings) alongside a growing share of purely rhetorical deployment in diplomatic forums with no operational follow-through. Accessibility collapse is moderate (0.45): alternative legitimacy framings (partition, negotiated two-state, one-state binational) remain actively contested rather than foreclosed. Resistance is high (0.80) because the reading is met with sustained, organized counter-argument from the Israeli state and allied readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab residents and refugees are declared as payers/victims: the reading is asserted on their behalf, but the gap between the doctrine's assertion and its material non-realization means they bear the ongoing cost of statelessness and displacement while the doctrine's institutional articulators (national leadership, sympathetic states) capture the diplomatic value of asserting it. Palestinian national leadership and external recognizing states are declared beneficiaries because they derive standing, legitimacy, and coalition value from the reading's persuasive force independent of territorial outcome — this is the structural asymmetry a tangled_rope claim requires: genuine coordination (organizing a demographic-majority population's claim) coexisting with asymmetric extraction (the doctrine's political value accrues disproportionately to its institutional articulators relative to the population it purports to represent).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (an indigenous demographic-majority population denied self-determination by external partition) remains substantively live by most non-partisan international law readings, which is why this is authored as contested rather than dead — this is not a case of an arrangement persisting after its function expired. The tangled_rope classification instead captures that the doctrine's persistence serves a dual function: it is simultaneously a genuine, non-obsolete coordination claim AND a structure whose political capital is partly captured by institutional actors at some remove from the population bearing the doctrine's unresolved costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporal_cutoff_arbitrariness,
    'Is bounding the relevant historical window to the 19th-20th century modern period a principled application of self-determination doctrine, or an arbitrary cutoff chosen because it favors this reading''s demographic premise over the covenant-continuity reading''s longer historical claim?',
    'Comparative analysis of how self-determination doctrine has been applied temporally in other decolonization contexts (e.g., settler-colonial disputes with multi-century population histories) to assess whether a modern-period cutoff is a general principle or reading-specific selection.',
    'If the cutoff is shown to be a general, consistently-applied principle of self-determination doctrine, this reading''s foundational premise strengthens considerably. If shown to be selected specifically to exclude the covenant-continuity reading''s ancient-presence claim, the reading''s legitimacy account is weakened and its axiom looks more like advocacy framing than doctrine application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_cutoff_arbitrariness, conceptual, 'Whether the modern-period temporal scope is principled doctrine or reading-specific selection.').

omega_variable(
    demographic_majority_evidentiary_contest,
    'Was there in fact a stable, continuous Arab demographic majority throughout the relevant modern period across the entire contested territory, or did demographic composition vary significantly by sub-region and era in ways that complicate the majority premise?',
    'Independent historical-demographic scholarship (Ottoman census records, British Mandate census data, and subsequent historiographic analysis) assessing population composition by sub-region across the 19th-20th centuries.',
    'If demographic composition varied substantially by sub-region, the reading''s uniform claim of continuous majority across the whole territory would need geographic qualification, which would change which sub-territories the self-determination claim most strongly supports.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_majority_evidentiary_contest, empirical, 'Contested historical demography underlying the majority premise.').

omega_variable(
    leadership_capture_vs_genuine_representation,
    'To what extent does the political leverage the self-determination doctrine generates for Palestinian national leadership and sympathetic external states track and serve the material interests of Palestinian Arab residents and refugees themselves, versus operating as an institutional or geopolitical asset partly decoupled from those interests?',
    'Longitudinal tracking of refugee material outcomes (living conditions, statelessness resolution rates, return/compensation settlements) against diplomatic and financial resources mobilized in the doctrine''s name.',
    'If material outcomes for the payer population have not improved proportionally to the political capital generated, the tangled_rope classification (genuine coordination function co-existing with asymmetric extraction) is strengthened; if outcomes track closely, the reading is closer to a pure rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(leadership_capture_vs_genuine_representation, empirical, 'Whether doctrine-generated political capital converts to material benefit for the population it represents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1917, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(terr_tr_t1947, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1947, 0.15).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1993, 0.28).
narrative_ontology:measurement(terr_tr_t2000, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(terr_be_t1917, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1917, 0.35).
narrative_ontology:measurement(terr_be_t1947, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1947, 0.5).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1967, 0.58).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1993, 0.55).
narrative_ontology:measurement(terr_be_t2000, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1917, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1917, 0.4).
narrative_ontology:measurement(terr_su_t1947, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1947, 0.6).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1967, 0.72).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1993, 0.68).
narrative_ontology:measurement(terr_su_t2000, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2000, 0.73).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the territorial_sovereignty_legitimacy kernel, each authored as a separate, ε-invariant constraint per the ε-invariance principle. The self_determination_reading (this file) authors moderate-high extractiveness (0.62) reflecting the reading's institutional-capture dynamic under contest; covenant_continuity_reading and existential_matrix_reading author their own independent ε values from their own structural premises. No reading's ε is derived from or averaged with the others; each stands as a distinct constraint linked here for contamination-propagation and network analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
