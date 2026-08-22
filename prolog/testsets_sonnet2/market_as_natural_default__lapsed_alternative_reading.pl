% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market Allocation as Default Institution — Lapsed Alternative (Amnesia) Reading
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This story is one of three readings of the 'market_as_natural_default'
 *   kernel: the shared observation that markets are widely presented as the
 *   natural, default institution for resource allocation, obscuring that
 *   other allocation systems (commons, guilds, kinship networks, cooperative
 *   federations) coexisted with or preceded markets in most historical
 *   contexts. This reading holds that the naturalization is a D3 artifact of
 *   lapsed institutional memory — the alternatives were never actively
 *   suppressed by an identifiable beneficiary class; the archival,
 *   pedagogical, and narrative infrastructure that would keep them visible
 *   simply eroded through ordinary attrition, and could be substantially
 *   recovered through historical research. This reading is deliberately
 *   narrow: it does NOT claim that no one currently benefits from market
 *   naturalization (that is the beneficiary_maintained_reading, a separate
 *   constraint), nor that lapsed memory created conditions later exploited by
 *   capture (that is the hybrid_amnesia_reading, also separate). Each reading
 *   has its own ε and its own stakeholder structure; they are linked via
 *   network.affects_constraints, not merged here.
 *
 * KEY AGENTS:
 *   - contemporary_economics_curricula: incidental beneficiary of low reconstruction cost (institutional/constrained) — reproduces the default without actively defending it
 *   - historical_alternative_institutions: non-agent placeholder for lapsed record — excluded by attrition, not suppression
 *   - economic_historians: analytical observers who demonstrate the alternatives remain recoverable
 *   - general_public: diffuse incidental beneficiary/payer — gains a stable default, pays a narrowed imagination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.1).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.08).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market Allocation as Default Institution — Lapsed Alternative (Amnesia) Reading").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology_studies/economic_history").

domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '4dc23d94-c38e-43e1-a7c0-06e2cd28a24b').
narrative_ontology:cs_kernel_codification('4dc23d94-c38e-43e1-a7c0-06e2cd28a24b', distributed).
narrative_ontology:cs_authority_grounding('4dc23d94-c38e-43e1-a7c0-06e2cd28a24b', diffuse_epistemic).
narrative_ontology:cs_reading_relation('4dc23d94-c38e-43e1-a7c0-06e2cd28a24b', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('4dc23d94-c38e-43e1-a7c0-06e2cd28a24b', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_axiom('4dc23d94-c38e-43e1-a7c0-06e2cd28a24b', foundational, naturalization_is_memory_artifact_not_agency_artifact).
narrative_ontology:cs_axiom_status(naturalization_is_memory_artifact_not_agency_artifact, holdable).
narrative_ontology:cs_axiom_grounding('4dc23d94-c38e-43e1-a7c0-06e2cd28a24b', naturalization_is_memory_artifact_not_agency_artifact, empirically_contingent).
narrative_ontology:cs_axiom('4dc23d94-c38e-43e1-a7c0-06e2cd28a24b', secondary, alternatives_remain_recoverable_via_historical_research).
narrative_ontology:cs_axiom_status(alternatives_remain_recoverable_via_historical_research, holdable).
narrative_ontology:cs_axiom_grounding('4dc23d94-c38e-43e1-a7c0-06e2cd28a24b', alternatives_remain_recoverable_via_historical_research, empirically_contingent).
narrative_ontology:cs_created_at('4dc23d94-c38e-43e1-a7c0-06e2cd28a24b', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, contemporary_economics_curricula).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, general_public).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, general_public).
narrative_ontology:constraint_vindicates(market_as_natural_default__lapsed_alternative_reading, market_coordination_efficiency_heuristic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teaches market allocation as the default starting point for analysis because it is the framework that survived, was codified into textbooks, and is cheapest to reproduce pedagogically. It does not administer the market and collects no rent from the arrangement's naturalized status, but its continued reproduction of the market-as-default frame is easier than reconstructing lapsed alternatives (mutual aid ledgers, guild allocation, communal tenure systems) from scattered archival record. Incidental beneficiary of low reconstruction cost, not an architect of closure.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, contemporary_economics_curricula, beneficiary,
    institutional, generational, constrained, national).

% Non-agent placeholder for the actual historical arrangements (commons management, guild price-setting, kinship-based distribution, cooperative federations) that once coexisted with or preceded market allocation in a given region. These are not agents who can object; they are simply absent from present discourse because the record documenting how they worked was not preserved, digitized, or taught, not because any party suppressed them.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, historical_alternative_institutions, excluded,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_non_agent(market_as_natural_default__lapsed_alternative_reading, historical_alternative_institutions).

% Researchers who can and do recover records of pre-market or non-market allocation systems from archives, oral history, and comparative anthropology. Their work demonstrates the alternatives are recoverable, not permanently foreclosed — the amnesia is a lapsed-memory condition, not a maintained wall. Their exit from the naturalized frame is already exercised professionally; the constraint does not bind them structurally.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% Encounters markets as the unmarked, default way goods and labor get allocated, absent any sense that this was one option among several live alternatives at various historical junctures. Benefits from a stable, legible, low-cognitive-load default; pays a small opportunity cost in the form of a narrowed imagination about what allocation could look like — a cost that is diffuse, not the product of anyone's rent extraction, and correctable via education.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, general_public, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__lapsed_alternative_reading, general_public, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_as_natural_default__lapsed_alternative_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, low-cost, historically inherited default for resource allocation and price discovery, sparing every new generation from re-deriving an allocation mechanism from scratch.
% TRANSFER_FUNCTION: Moves cognitive and pedagogical effort away from reconstructing or teaching alternative allocation systems and toward reproducing the inherited market default; no monetary or resource transfer between identifiable beneficiary and victim classes is present in this reading.
% ABSENT_VOICES: The historical alternative institutions themselves have no voice because their institutional memory has lapsed — guild ledgers, commons-management records, and cooperative allocation schemes are simply under-preserved and under-taught, not actively silenced by any living party.
% DISAPPEARANCE_RATIONALE: If market-as-default naturalization vanished overnight (i.e., if the historical alternatives were suddenly common knowledge and taught alongside markets), curricula would need updating and public imagination would widen, but no beneficiary class loses a revenue stream or administrative function — the change would be pedagogical and cultural, not structural, which is why some would call it a rearrangement and others a non-event.
% FOUNDING_PROBLEM: No single founding problem exists in this reading — market dominance in cultural memory arose from the ordinary attrition of institutional record-keeping for alternatives that lost out or were absorbed, not from a deliberate act designed to solve a coordination problem by closing alternatives.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and comparative anthropologists, who are outside the group that benefits from reduced pedagogical cost, corroborate that the amnesia is a genealogical accident of archival survival and curricular path-dependence rather than a maintained closure; no beneficiary-class source is needed to confirm this because no beneficiary class actively maintains the amnesia.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, contested).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.1, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, ExtMetricName, E),
    domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.10) and flat across the interval because this reading asserts there is no active extraction machinery — no party collects rents from keeping alternatives forgotten. Suppression is very low (0.08) because nothing structural prevents recovery; the barrier is informational/archival, not coercive. Accessibility collapse is authored moderately high (0.72) because from inside the naturalized frame the alternatives genuinely do feel unavailable — that is the D3 phenomenon this reading names — even though the underlying collapse is soft and reversible via research, unlike a mountain's irreducible collapse. Resistance is low (0.15): almost no one actively fights to keep the alternatives forgotten, because there is no beneficiary interest at stake in this reading's account.
 *
 * DIRECTIONALITY LOGIC:
 *   No victim class is declared in this reading, consistent with the expected structural delta (no identifiable beneficiary class extracting from a target class). The economics curricula and general public sit near the beneficiary end of directionality because they gain a low-cost default; economic historians sit at the analytical pole since their relationship to the constraint is investigative, not extractive. This flat, low-directionality profile is the structural signature that distinguishes this reading from its siblings, where directionality sharpens around an identifiable capturing class.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/disappearance_verdict pairing (dead/contested) is intentionally soft rather than a clean capture signal: because no beneficiary class actively maintains the amnesia, there is no zombie-mandate to detect — the arrangement's persistence is better modeled as institutional inertia in curricular reproduction than as a maintained extraction structure outliving its function. This is precisely what should NOT be conflated with a tangled_rope or snare classification; this reading's whole point is that the coordination/extraction split characteristic of those types is largely absent here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amnesia_versus_maintenance,
    'Is the observed naturalization of market allocation genuinely inert lapsed memory, or does it show signs of active post-hoc defense by an identifiable beneficiary class (which would instead support the beneficiary_maintained_reading)?',
    'Track whether attempts to reintroduce historical alternative-allocation curricula into economics education meet organized institutional resistance (funding withdrawal, curriculum committee pushback tied to identifiable interests) versus simple inertia (nobody objects, it just doesn''t happen because no one prioritizes it).',
    'If organized resistance from an identifiable beneficiary class is found, this constraint should be re-classified as an instance of the beneficiary_maintained_reading or hybrid_amnesia_reading instead — the low-ε lapsed-alternative account would no longer be the accurate reading for that context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amnesia_versus_maintenance, empirical, 'Whether the naturalization is inert amnesia or actively defended by a beneficiary class.').

omega_variable(
    recoverability_of_alternatives,
    'How completely have records of historical alternative allocation institutions actually been lost, versus merely under-emphasized in dominant curricula while remaining accessible to specialists?',
    'Archival survey of the depth and completeness of surviving records (guild ledgers, commons management records, cooperative federation charters) across multiple regions and periods.',
    'If records are substantially complete and merely under-taught, accessibility_collapse should be revised downward (closer to a rope-like recoverable condition); if records are genuinely fragmentary and largely irrecoverable, accessibility_collapse should be revised upward, moving this reading structurally closer to a mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recoverability_of_alternatives, empirical, 'Degree to which lapsed alternatives are actually recoverable versus genuinely lost.').

omega_variable(
    mountain_claim_versus_constructed_default,
    'Is treating ''market as natural default'' as claimed_type mountain defensible given that beneficiaries (economics curricula) are declared, or does the FSM signature correctly flag this as a constructed-but-naturalized arrangement masquerading as natural law?',
    'Compare this constraint''s computed engine classification (after FSM evaluation) against the authored mountain claim; if the engine reclassifies toward tangled_rope despite the low authored ε, that divergence itself is the data point about whether even an inert-amnesia naturalization still shows the false-summit signature structurally.',
    'Confirms or challenges whether the ''no active beneficiary interest'' framing is sufficient to sustain a genuine mountain claim, or whether ANY declared beneficiary — however incidental — is enough to trigger reclassification regardless of the low extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_claim_versus_constructed_default, conceptual, 'Whether the mountain claim survives FSM scrutiny given a declared incidental beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mark_tr_t8, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(mark_tr_t16, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement(mark_tr_t24, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement(mark_tr_t32, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 32, 0.12).
narrative_ontology:measurement(mark_tr_t40, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 0, 0.09).
narrative_ontology:measurement(mark_be_t8, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 8, 0.09).
narrative_ontology:measurement(mark_be_t16, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 16, 0.1).
narrative_ontology:measurement(mark_be_t24, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 24, 0.1).
narrative_ontology:measurement(mark_be_t32, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 32, 0.1).
narrative_ontology:measurement(mark_be_t40, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 40, 0.1).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(market_as_natural_default__lapsed_alternative_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, information_standard).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__lapsed_alternative_reading, 0.03).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This constraint is the low-ε member of a three-story kernel family sharing the 'market_as_natural_default' commitment. beneficiary_maintained_reading authors high ε with an identifiable capturing class actively defending the naturalization; hybrid_amnesia_reading authors a two-stage genealogy where this story's lapsed-memory condition is Stage 1 and subsequent beneficiary capture is Stage 2, producing intermediate ε. This story supplies the 'influences' edge into hybrid_amnesia_reading because the initial lapse this reading describes is the structural precondition the hybrid reading's capture stage depends on; the relation to beneficiary_maintained_reading is coexists_with because both readings can be simultaneously true of different historical episodes or regions without one logically foreclosing the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
