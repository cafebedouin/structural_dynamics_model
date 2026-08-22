% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Hybrid Scaffolding of Imposed Practice Legitimacy
 *   domain: political/historical/cultural
 *
 * SUMMARY:
 *   This constraint models the hybrid scaffolding reading of imposed practice
 *   legitimacy: top-down mandates for calendar and dress reform succeed only
 *   when reinforced by ideological messaging (state schools, propaganda,
 *   elite modeling) that generates quasi-endogenous pull. Pure decree
 *   (calendar reform without scaffolding) failed; pure endogenous climb
 *   (dress reform without state mandate) was too slow; the scaffolded
 *   combination achieved partial displacement where urban elites adopted
 *   Western markers as identity capital while rural populations bore
 *   enforcement costs without access to transition infrastructure. The
 *   constraint is structurally a tangled rope — genuine coordination function
 *   (state needs unified time/dress for administration) combined with
 *   asymmetric extraction (rural populations pay the transition cost, urban
 *   elites capture the status gains).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.62).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.48).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.54).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Hybrid Scaffolding of Imposed Practice Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political/historical/cultural").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '0050d8de-f4d8-445b-8c75-5836ef261dff').
narrative_ontology:cs_kernel_codification('0050d8de-f4d8-445b-8c75-5836ef261dff', implicit).
narrative_ontology:cs_authority_grounding('0050d8de-f4d8-445b-8c75-5836ef261dff', extraction).
narrative_ontology:cs_interpretation_layer_present('0050d8de-f4d8-445b-8c75-5836ef261dff').
narrative_ontology:cs_reading_relation('0050d8de-f4d8-445b-8c75-5836ef261dff', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('0050d8de-f4d8-445b-8c75-5836ef261dff', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_axiom('0050d8de-f4d8-445b-8c75-5836ef261dff', foundational, scaffolding_necessary_for_displacement).
narrative_ontology:cs_axiom_status(scaffolding_necessary_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('0050d8de-f4d8-445b-8c75-5836ef261dff', scaffolding_necessary_for_displacement, empirically_contingent).
narrative_ontology:cs_axiom('0050d8de-f4d8-445b-8c75-5836ef261dff', foundational, partial_displacement_is_stable).
narrative_ontology:cs_axiom_status(partial_displacement_is_stable, holdable).
narrative_ontology:cs_axiom_grounding('0050d8de-f4d8-445b-8c75-5836ef261dff', partial_displacement_is_stable, empirically_contingent).
narrative_ontology:cs_reference_frame('0050d8de-f4d8-445b-8c75-5836ef261dff', traditional_practice_legitimacy).
narrative_ontology:cs_drift_state('0050d8de-f4d8-445b-8c75-5836ef261dff', post_imposition_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0050d8de-f4d8-445b-8c75-5836ef261dff', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites_adopting_western_identity).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations_excluded_from_scaffolding).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, imposition_requires_ideological_reinforcement).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, partial_displacement_is_stable_outcome).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues top-down mandates for calendar and dress reform; controls legal enforcement machinery and state media for ideological messaging. Collects legitimacy gains from successful imposition; bears cost of enforcement and messaging infrastructure.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_imposition_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Adopt Western dress and calendar as identity markers signaling modernity and alignment with state power. Gain social capital, professional advancement, and political access through visible compliance. Can exit by reverting to traditional markers if political winds shift.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites_adopting_western_identity, beneficiary,
    powerful, biographical, mobile, regional).

% Face legal penalties for non-compliance with dress and calendar mandates but lack access to state schools, media, and elite networks that provide the ideological scaffolding. Bear the full coercive weight without the identity benefits or transition pathways. Exit means geographic displacement or cultural erasure.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations_excluded_from_scaffolding, payer,
    powerless, generational, trapped, local).

% Lose institutional authority over time-reckoning and bodily practice as state calendar and dress codes displace religious alternatives. Their objection is structurally excluded from the reform process; resistance is channeled into underground preservation or marginal accommodation.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditional_religious_authorities, excluded,
    organized, generational, constrained, national).

% Observes the constraint from outside the imposition; evaluates the structural relationship between decree, scaffolding, and partial displacement without bearing costs or collecting benefits.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, historical_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the state's problem of displacing entrenched traditional practices (calendar, dress) that anchor rival sources of legitimacy; provides a transitional mechanism where pure decree failed and pure internalization was too slow.
% TRANSFER_FUNCTION: Moves compliance costs and identity renegotiation burdens from the state onto rural populations (who bear enforcement penalties without scaffolding) while transferring status gains and political access to urban elites who perform the new practices.
% ABSENT_VOICES: Rural populations and traditional religious authorities would object to the asymmetry of scaffolding access and the coercive displacement of their practices; they are excluded from the reform conversation by design — the scaffolding infrastructure (schools, media, elite networks) does not reach them.
% DISAPPEARANCE_RATIONALE: If the hybrid scaffolding constraint vanished, the state would revert to pure decree (which historically failed) or pure climb (which is too slow for state-building timelines); traditional practices would reassert in rural areas while urban elites would retain adopted markers as identity capital — the partial displacement equilibrium would collapse into either re-traditionalization or accelerated Westernization depending on which force fills the vacuum.
% FOUNDING_PROBLEM: State-building projects in late-imperial and early-republican contexts needed to displace traditional calendars and dress codes that anchored rival legitimacies (religious, tribal, imperial) but found that pure legal mandate produced evasion and pure cultural diffusion was too slow to serve political timelines.
% FOUNDING_PROBLEM_CORROBORATION: State archives and reformist intellectuals attest the founding problem was real and pressing; rural oral histories and religious scholars attest the problem was manufactured by the state to justify power consolidation — no neutral party corroborates either framing.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the asymmetric burden: rural populations face penalties for non-compliance without the schools/media/elite networks that make adoption a status gain. Suppression (0.48) is moderate — the constraint relies more on ideological scaffolding than raw coercion, but enforcement machinery exists and is deployed selectively. Theater ratio (0.31) captures the performative dimension: elite modeling and state media create a spectacle of voluntary adoption that masks the coercive floor. Accessibility collapse (0.54) is partial — alternatives persist in rural zones and underground practice. Resistance (0.57) is significant but channeled: rural non-compliance, religious authority preservation, and hybrid practices that satisfy neither pole.
 *
 * PERSPECTIVAL GAP:
 *   From the state seat, the constraint appears as necessary coordination (unified calendar/dress for modern administration). From the urban elite seat, it appears as voluntary identity adoption (modernity signaling). From the rural payer seat, it appears as coercive extraction (penalties without pathways). The engine computes these divergences from the structural data; the claimed_type (tangled_rope) captures the authoring seat's structural reading.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus is the agenda setter with arbitrage exit — it designs the constraint and can modify or abandon it. Urban elites are beneficiaries with mobile exit — they gain status and access, can revert if advantageous. Rural populations are payers with trapped exit — they bear penalties without scaffolding, cannot easily leave the constraint's reach. Traditional authorities are excluded with constrained exit — they resist but within narrowing space. The observer sees the full structure without positional stakes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (displacing rival legitimacies for state-building) is contested as live vs. manufactured. The constraint persists past its declared sunset (no formal sunset clause) because the partial displacement equilibrium serves both the state (administrative unity) and urban elites (identity capital) — neither bears enough cost to dismantle it, while rural payers lack power to force change. This is mandatrophy: the original mandate has outlived its function but the constraint remains through coalition inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffolding_necessity_ambiguity,
    'Is the ideological scaffolding (state schools, media, elite modeling) structurally necessary for the mandate''s partial success, or would decree alone have achieved similar displacement over a longer horizon?',
    'Counterfactual comparison with cases where decree was attempted without scaffolding (calendar) vs. scaffolding without decree (dress in some regions) — but historical record conflates the two; requires isolating the scaffolding variable.',
    'If scaffolding is necessary, the constraint is genuinely tangled_rope (coordination + extraction). If decree alone would have worked, the scaffolding is theater masking a snare. If scaffolding alone would have worked, the mandate is a scaffold that became a piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffolding_necessity_ambiguity, conceptual, 'Whether the coordination function requires the scaffolding layer or the scaffling is extractive cover.').

omega_variable(
    partial_displacement_stability,
    'Is the partial displacement equilibrium (urban Westernization + rural traditional persistence) a stable attractor or a transient state toward full displacement?',
    'Longitudinal tracking of practice adoption rates in rural areas over generations; if rural adoption accelerates without additional scaffolding, the equilibrium is transient. If rural persistence holds despite urban saturation, it is stable.',
    'If transient, the constraint is a scaffold with an implicit sunset (full displacement). If stable, it is a tangled_rope with a permanent asymmetric structure. The claimed_type (tangled_rope) assumes stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partial_displacement_stability, empirical, 'Whether the hybrid outcome is an endpoint or a waypoint.').

omega_variable(
    kernel_reading_relations,
    'How does this hybrid scaffolding reading structurally relate to the exogenous_override_reading and endogenous_climb_reading of the same kernel?',
    'Analyze whether any single party could hold this reading and a sibling reading simultaneously without contradiction (coexists_with), or whether this reading''s core premise (scaffolding is necessary) logically rules out the sibling''s core premise (forecloses), or whether this reading creates downstream pressure on siblings (influences).',
    'Determines the reading_relations in cs_structure and whether the kernel has genuine foreclosure pairs or only coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relationship between this reading and its kernel siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 1900, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1900, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(legi_tr_t1912, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1912, 0.22).
narrative_ontology:measurement(legi_tr_t1920, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1920, 0.26).
narrative_ontology:measurement(legi_tr_t1930, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1930, 0.29).
narrative_ontology:measurement(legi_tr_t1940, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1940, 0.3).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1950, 0.31).

% Extraction over time
narrative_ontology:measurement(legi_be_t1900, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1900, 0.35).
narrative_ontology:measurement(legi_be_t1912, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1912, 0.42).
narrative_ontology:measurement(legi_be_t1920, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1920, 0.51).
narrative_ontology:measurement(legi_be_t1930, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1930, 0.58).
narrative_ontology:measurement(legi_be_t1940, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1940, 0.6).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1950, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1900, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(legi_su_t1912, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1912, 0.58).
narrative_ontology:measurement(legi_su_t1920, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1920, 0.52).
narrative_ontology:measurement(legi_su_t1930, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1930, 0.49).
narrative_ontology:measurement(legi_su_t1940, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1940, 0.48).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1950, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the legitimacy_of_imposed_practice constraint family (kernel). The three readings decompose the natural-language concept 'imposed practice legitimacy' into structurally distinct claims with different ε values: exogenous_override (low ε, high suppression, Mountain-claim), endogenous_climb (low ε, low suppression, Rope-claim), hybrid_scaffolding (moderate ε, moderate suppression, Tangled Rope-claim). The hybrid reading claims the coordination function requires scaffolding; the others claim it does not (decree suffices / internalization suffices).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
