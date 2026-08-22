% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__m4_m5_collapse_reading, []).

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
 *   constraint_id: electronic_money_emergence__m4_m5_collapse_reading
 *   human_readable: M4/M5 Statistical Aggregate Distinction as Retroactive Category Creation
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This story instantiates one reading of the electronic-money-emergence
 *   kernel: the claim that the M4/M5 statistical distinction — a reporting
 *   boundary drawn by central bank statisticians to separate broad monetary
 *   aggregates from narrower ones — did not track a real historical emergence
 *   of electronic money at all, but rather retroactively manufactured the
 *   category by drawing a line in aggregated data and then treating the
 *   line's appearance as if it dated a discovery. On this reading, there was
 *   no emergence event in the relevant sense; there was a classification
 *   decision that stabilized into a piton — a measurement artifact that
 *   persists because it is administratively convenient and theoretically
 *   load-bearing for downstream monetary aggregate research, not because it
 *   corresponds to any underlying monetary physics or discrete technological
 *   threshold. The sibling readings (became_thinkable_reading,
 *   first_held_reading) are separate constraints in this family: they locate
 *   a real emergence event at a conceptual or first-instance moment. This
 *   story does not adjudicate between them; it asserts that the M4/M5-based
 *   dating in particular is not evidence of emergence at all, independent of
 *   whether emergence occurred elsewhere by another measure.
 *
 * KEY AGENTS:
 *   - central_bank_statistics_offices: agenda_setter/beneficiary (institutional/arbitrage) — sets and benefits from the classification boundary
 *   - monetary_aggregate_theorists: beneficiary (organized/mobile) — builds theoretical and career capital on the aggregate as a real object
 *   - monetary_historians: payer (moderate/constrained) — bears the cost of a distorted periodization
 *   - policy_analysts_relying_on_aggregates: payer (moderate/constrained) — inherits classification artifacts as policy inputs
 *   - actual_currency_users_and_institutions: excluded (powerless/analytical) — generated the underlying practice with no voice in its classification
 *   - monetary_theory_historians_of_science: observer (analytical/analytical) — sees the full classificatory structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.42).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.31).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, piton).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "M4/M5 Statistical Aggregate Distinction as Retroactive Category Creation").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic_history/monetary_theory/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, '7a07903c-9e79-4578-abe3-68519663e708').
narrative_ontology:cs_kernel_codification('7a07903c-9e79-4578-abe3-68519663e708', distributed).
narrative_ontology:cs_authority_grounding('7a07903c-9e79-4578-abe3-68519663e708', extraction).
narrative_ontology:cs_interpretation_layer_present('7a07903c-9e79-4578-abe3-68519663e708').
narrative_ontology:cs_reading_relation('7a07903c-9e79-4578-abe3-68519663e708', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a07903c-9e79-4578-abe3-68519663e708', electronic_money_emergence__first_held_reading, coexists_with).
narrative_ontology:cs_axiom('7a07903c-9e79-4578-abe3-68519663e708', foundational, classification_boundaries_are_not_discovery_events).
narrative_ontology:cs_axiom_status(classification_boundaries_are_not_discovery_events, holdable).
narrative_ontology:cs_axiom_grounding('7a07903c-9e79-4578-abe3-68519663e708', classification_boundaries_are_not_discovery_events, conventional).
narrative_ontology:cs_axiom('7a07903c-9e79-4578-abe3-68519663e708', foundational, aggregate_statistics_retroactively_construct_historical_categories).
narrative_ontology:cs_axiom_status(aggregate_statistics_retroactively_construct_historical_categories, holdable).
narrative_ontology:cs_axiom_grounding('7a07903c-9e79-4578-abe3-68519663e708', aggregate_statistics_retroactively_construct_historical_categories, empirically_contingent).
narrative_ontology:cs_reference_frame('7a07903c-9e79-4578-abe3-68519663e708', statistical_convention_as_neutral_measurement).
narrative_ontology:cs_drift_state('7a07903c-9e79-4578-abe3-68519663e708', contemporary_monetary_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7a07903c-9e79-4578-abe3-68519663e708', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistics_offices).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, monetary_aggregate_theorists).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, monetary_historians).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, policy_analysts_relying_on_aggregates).
narrative_ontology:constraint_vindicates(electronic_money_emergence__m4_m5_collapse_reading, monetary_aggregate_classificatory_realism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and publish the M4/M5 aggregate definitions, deciding what instruments count as 'electronic money' for reporting purposes. Benefit from a stable, defensible classification scheme that makes their historical data series look continuous and their statistical authority look uncontested. Can revise the definitions administratively without external veto, and face little cost from the classification being treated as a discovery rather than a convention.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistics_offices, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistics_offices, beneficiary).

% Build careers and models on the aggregates as if they tracked a real underlying phenomenon (the 'emergence' of electronic money). The retroactive category gives their models a clean historical origin story and validates the aggregate as an object of study rather than an artifact of reporting convention. Free to shift theoretical frameworks if the convention is challenged, at modest reputational cost.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_aggregate_theorists, beneficiary,
    organized, biographical, mobile, national).

% Attempting to write accurate histories of dematerialized currency must either accept the M4/M5 boundary as if it marked a real event, or spend scarce scholarly effort disentangling the statistical convention from the underlying practice history. Bear the cost of a distorted periodization that the aggregate literature treats as settled fact; cannot simply opt out because the aggregates are the dominant citable data source.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_historians, payer,
    moderate, generational, constrained, national).

% Use M4/M5 series as inputs to policy analysis and forecasting, inheriting whatever discontinuities or artifacts the classification boundary introduces without being positioned to audit the boundary's construction. Their exit is constrained because alternative data series are sparser or non-standardized, making the flawed convention the path of least resistance.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, policy_analysts_relying_on_aggregates, payer,
    moderate, biographical, constrained, national).

% The households, firms, and banks whose transactions actually generated dematerialized payment instruments had no role in the classificatory decision that later declared their practices to constitute an 'emergence event.' Their lived adoption of electronic instruments preceded and is indifferent to the statistical boundary drawn around it.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, actual_currency_users_and_institutions, excluded,
    powerless, immediate, analytical, national).

% Study how statistical categories retroactively construct historical narratives, positioned to identify when a measurement convention has been mistaken for a discovered natural kind. Have no stake in the aggregate's continued authority.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_theory_historians_of_science, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The M4/M5 boundary genuinely coordinates a reporting convention: it lets central banks, researchers, and policy bodies use a common, comparable definition of 'broad money' components across time and jurisdictions for aggregation and comparison purposes.
% TRANSFER_FUNCTION: Moves interpretive authority from the messy, gradual, decentralized history of dematerialized payment practice to the statistics office's classificatory apparatus — converting a diffuse socio-technical process into a discrete, citable 'emergence' that the aggregate literature and its downstream policy analysis then treat as a fact about the world rather than a fact about reporting.
% ABSENT_VOICES: The banks, clearing houses, and individual users whose actual practices generated dematerialized instruments over decades were never consulted about where the statistical line should fall; historians of the underlying technology and practice are marginal to the aggregate-data-driven mainstream monetary literature that inherited the classification as settled.
% DISAPPEARANCE_RATIONALE: If the M4/M5 boundary were abolished, central bank reporting would need a replacement convention (some line must be drawn for aggregation to function), so the coordination function would not simply vanish. But the specific 'emergence of electronic money' narrative built on top of it — the idea that a real historical event is dated by the statistical break — would dissolve, and monetary historiography would have to re-periodize using non-statistical evidence. Practitioners committed to the aggregate literature would say little changes; historians of practice would say the entire origin story evaporates.
% FOUNDING_PROBLEM: Central banks needed a stable, internationally comparable way to aggregate increasingly heterogeneous liquid liabilities (deposits, near-money instruments, emerging electronic balances) for monetary policy targeting and cross-country comparison.
% FOUNDING_PROBLEM_CORROBORATION: Central bank methodology documents and IMF monetary statistics manuals attest the aggregation problem is real and ongoing (a live reporting need). However, historians of payment technology and science-studies scholars of classification (outside the central banking and aggregate-theory beneficiary set) attest that the specific claim of a dated 'emergence of electronic money' is an artifact of when the reporting convention was adopted, not evidence the underlying technological or social shift occurred at that moment — no source outside the beneficiary set corroborates the emergence-dating claim itself.
narrative_ontology:disappearance_verdict(electronic_money_emergence__m4_m5_collapse_reading, contested).
narrative_ontology:founding_problem_status(electronic_money_emergence__m4_m5_collapse_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__m4_m5_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__m4_m5_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__m4_m5_collapse_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).
:- end_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) — this is not primarily a rent-extraction mechanism; the harm is epistemic distortion rather than direct resource transfer. It rose gradually over the interval as more downstream policy and academic work came to depend on the aggregate boundary, compounding the artifact's authority. Theater ratio is high and rising (0.35→0.68): an increasing share of the activity around the M4/M5 boundary is performative — citing the classification as if it settled a historical question — rather than functional reporting work. Suppression is moderate (0.31): there is no active coercion preventing historians from contesting the periodization, but accessibility_collapse is fairly high (0.58) because once the aggregate literature achieves institutional dominance, alternative non-statistical periodizations become progressively harder to publish, cite, or teach against. Resistance is low-moderate (0.35): a stable core of historians of technology and science-studies scholars contest the framing, but the aggregate convention faces little organized resistance from within mainstream monetary economics.
 *
 * PERSPECTIVAL GAP:
 *   From the statistics office's seat, the M4/M5 boundary is a working convention necessary for the coordination function of comparable reporting — not a claim about history at all, so no distortion is felt. From the monetary historian's seat, the same boundary functions as an unearned historical claim smuggled in under statistical authority, producing real costs to accurate historiography. The engine should register this as seat divergence: agenda_setter and beneficiary seats see coordination; payer seats see an artifact imposed as fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Central bank statistics offices and aggregate theorists are declared beneficiaries because they derive continuing institutional and theoretical value from the classification being treated as a real event rather than a convention — this yields low d (beneficiary end). Monetary historians and policy analysts are declared victims/payers because they must either accept a distorted periodization or spend resources correcting it, and their exit options are constrained by the aggregate literature's dominance as the standard data source — this yields high d (target end). Actual currency users are excluded rather than beneficiary or victim in the aggregate-classification sense: the classification does not act on them directly, but its narrative erases their actual historical agency.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a genuine need to aggregate heterogeneous liquid liabilities for monetary policy) remains live — central banks still need workable aggregation conventions. What has drifted is the SEPARATE claim, layered on top of the reporting function, that the boundary marks a historical emergence event. Classifying this specific reading as piton rather than snare or rope captures that: no one is coercively extracting value through the boundary (ruling out snare), but the boundary also is not a clean, low-cost coordination solution anymore (ruling out rope) — it has calcified into an inertial classificatory habit that persists because dismantling the emergence narrative would be more work than any single actor benefits enough to undertake, while the disciplines that could correct it bear diffuse, hard-to-organize costs. This is the piton signature: administered by an agenda_setter who could revise it, but for whom the correction cost exceeds any single benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_committer_structure_m4_m5_reading,
    'This constraint is one reading (m4_m5_collapse_reading) of the electronic_money_emergence kernel. The sibling readings — became_thinkable_reading (emergence dated to conceptual possibility) and first_held_reading (emergence dated to first institutional instance) — locate a real emergence event elsewhere. Does rejecting the M4/M5 dating as artifactual imply anything about whether those sibling readings correctly identify a real emergence event by their own criteria?',
    'Independent historical reconstruction of dematerialized payment practice using non-statistical sources (bank internal records, technology adoption studies, legal instrument records) to test whether a discrete emergence threshold exists under either sibling framing, decoupled entirely from aggregate reporting dates.',
    'If a genuine threshold exists under the became_thinkable or first_held criteria, this reading''s claim is narrow (only the M4/M5 dating is artifactual, not the underlying possibility of dating emergence). If no threshold exists under any framing, this reading''s skepticism generalizes and the entire kernel may be about a gradual process with no true emergence point, undermining all three readings'' shared premise that a datable emergence occurred.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_structure_m4_m5_reading, conceptual, 'Whether rejecting the statistical dating implies rejecting the datable-emergence premise shared by all kernel readings, or only rejecting this one operationalization.').

omega_variable(
    convention_vs_discovery_ambiguity,
    'Is the M4/M5 boundary a defensible operational convention that happens to get misread as historical fact by downstream users, or is it authored by statistics offices with the (perhaps unconscious) intent of claiming discovery-grade authority for what is actually a definitional choice?',
    'Archival review of the original methodological documents and internal deliberation records from the central banks/statistical bodies that first drew the M4/M5 boundary, to assess whether the emergence framing was present at authorship or added later by secondary literature.',
    'If the emergence framing originated with the statistics offices themselves, the beneficiary declaration is stronger and the piton reading is closer to a soft snare (deliberate authority inflation). If the framing was added later by secondary theorists and popularizers, the statistics offices are closer to unwitting hosts of a piton that others built on top of their convention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(convention_vs_discovery_ambiguity, empirical, 'Whether the retroactive emergence claim originates in the classification''s authorship or in its later theoretical reception.').

omega_variable(
    theater_ratio_trajectory_interpretation,
    'Does the rising theater_ratio (0.35 to 0.68) reflect increasing performative citation of the M4/M5 boundary as historical fact, or does it reflect a genuine increase in the boundary''s functional load as more instruments needed classifying over time?',
    'Content analysis of citations to the M4/M5 boundary across decades, coding each citation as functional (used for aggregation/reporting purposes) versus narrative (used to support a claim about when electronic money ''emerged'').',
    'A predominantly narrative citation pattern would confirm the piton reading; a predominantly functional pattern would suggest the theater_ratio increase is better explained by genuine growth in classificatory complexity, which would weaken (though not eliminate) the artifact claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_trajectory_interpretation, empirical, 'Whether the rising theater ratio reflects narrative misuse or genuine functional complexity growth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1970, 0.35).
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1980, 0.42).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1990, 0.51).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2000, 0.6).
narrative_ontology:measurement(elec_tr_t2010, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2010, 0.65).
narrative_ontology:measurement(elec_tr_t2020, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2020, 0.68).

% Extraction over time
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1990, 0.36).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2000, 0.39).
narrative_ontology:measurement(elec_be_t2010, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement(elec_be_t2020, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2020, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(electronic_money_emergence__m4_m5_collapse_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__m4_m5_collapse_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__m4_m5_collapse_reading, 0.03).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__first_held_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints instantiating readings of the electronic_money_emergence kernel. became_thinkable_reading and first_held_reading each assert a genuine, datable emergence event (located at conceptual possibility versus first institutional instance respectively); this reading (m4_m5_collapse_reading) instead denies that the specific statistical dating convention (M4/M5) corresponds to any real event, characterizing it as a retroactively-constructed classificatory piton. All three share the kernel_id electronic_money_emergence but are structurally distinct constraints with independent ε values, independent beneficiary/victim structures, and independent classifications, linked here per the ε-invariance decomposition principle rather than treated as one constraint with an observer-dependent reading parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
