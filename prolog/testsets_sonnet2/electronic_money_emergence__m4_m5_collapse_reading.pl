% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: M4/M5 Statistical Aggregate Boundary as Retroactive Category-Creator
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This story instantiates the 'M4/M5 collapse' reading of a contested
 *   kernel about when electronic money emerged. On this reading there was no
 *   genuine emergence event at all — the statistical convention used to
 *   separate broad-money aggregates (M4-type vs M5-type instruments, or their
 *   national equivalents) is what RETROACTIVELY creates the appearance of a
 *   category called 'electronic money' coming into being. The boundary was
 *   drawn for accounting continuity, then read backward as though it recorded
 *   a discovered natural kind. What began as a measurement convention has
 *   calcified into a piton: it still performs a real (if narrow) coordination
 *   function — comparable time series for monetary policy — but the
 *   theatrical overlay, the treatment of the boundary as marking a historical
 *   threshold, has grown over time as more secondary literature and policy
 *   commentary cite the aggregate transition as if it were an event.
 *
 * KEY AGENTS:
 *   - central_bank_statistics_departments: agenda_setter/beneficiary (institutional/arbitrage) — maintains and benefits from the classificatory convention
 *   - monetary_aggregate_forecasting_industry: beneficiary (organized/mobile) — has built commercial infrastructure on the boundary's continuity
 *   - monetary_historians: payer (moderate/constrained) — must work around a data source that pre-encodes a false periodization
 *   - policy_analysts_relying_on_aggregates: payer (moderate/constrained) — draw policy conclusions from what is partly a reporting artifact
 *   - technology_and_institutional_actors_of_the_period: excluded (powerless/trapped) — the historical actors classified after the fact with no voice
 *   - economic_historiography_reviewers: observer (analytical/analytical) — can identify but rarely displace the convention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.38).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.31).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, piton).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "M4/M5 Statistical Aggregate Boundary as Retroactive Category-Creator").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic_history/monetary_theory/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, '4c1ff5a9-3a60-43cf-9664-b7a5197df339').
narrative_ontology:cs_kernel_codification('4c1ff5a9-3a60-43cf-9664-b7a5197df339', distributed).
narrative_ontology:cs_authority_grounding('4c1ff5a9-3a60-43cf-9664-b7a5197df339', extraction).
narrative_ontology:cs_interpretation_layer_present('4c1ff5a9-3a60-43cf-9664-b7a5197df339').
narrative_ontology:cs_reading_relation('4c1ff5a9-3a60-43cf-9664-b7a5197df339', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c1ff5a9-3a60-43cf-9664-b7a5197df339', electronic_money_emergence__first_held_reading, coexists_with).
narrative_ontology:cs_axiom('4c1ff5a9-3a60-43cf-9664-b7a5197df339', foundational, emergence_is_measurement_artifact_not_event).
narrative_ontology:cs_axiom_status(emergence_is_measurement_artifact_not_event, holdable).
narrative_ontology:cs_axiom_grounding('4c1ff5a9-3a60-43cf-9664-b7a5197df339', emergence_is_measurement_artifact_not_event, conventional).
narrative_ontology:cs_axiom('4c1ff5a9-3a60-43cf-9664-b7a5197df339', secondary, statistical_boundaries_lack_ontological_authority_over_history).
narrative_ontology:cs_axiom_status(statistical_boundaries_lack_ontological_authority_over_history, holdable).
narrative_ontology:cs_axiom_grounding('4c1ff5a9-3a60-43cf-9664-b7a5197df339', statistical_boundaries_lack_ontological_authority_over_history, conventional).
narrative_ontology:cs_reference_frame('4c1ff5a9-3a60-43cf-9664-b7a5197df339', monetary_aggregate_reporting_as_neutral_bookkeeping).
narrative_ontology:cs_drift_state('4c1ff5a9-3a60-43cf-9664-b7a5197df339', contemporary_retrospective_historiography, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4c1ff5a9-3a60-43cf-9664-b7a5197df339', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistics_departments).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, monetary_aggregate_forecasting_industry).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, monetary_historians).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, policy_analysts_relying_on_aggregates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the M4/M5 (or equivalent broad-money) classification boundary as a going statistical convention, deciding which instruments count as 'electronic money' for aggregate reporting. Continues publishing the series and defending the boundary's continuity even as the underlying instruments it partitions have become nearly indistinguishable from one another in function. Benefits from having a stable, citable line to draw regardless of whether it tracks anything monetary economists would call a real phase transition.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistics_departments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistics_departments, beneficiary).

% Builds models, forecasts, and commentary keyed to the published aggregate boundary. Has professional and commercial reasons to treat the M4/M5 split as a meaningful economic threshold rather than an artifact of reporting convention, since abandoning the frame would strand existing analytical infrastructure and client-facing products.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_aggregate_forecasting_industry, beneficiary,
    organized, biographical, mobile, national).

% Attempt to write accurate histories of when and how electronic money 'emerged' but find the primary data source — the statistical series itself — was constructed around a classification boundary that did not exist when the underlying instruments first appeared. Their scholarship is constrained by having to either accept the artifact's implicit timeline or do costly archival reconstruction to route around it; most cannot fully route around it because pre-boundary transaction-level data was never retained in a comparable form.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_historians, payer,
    moderate, civilizational, constrained, global).

% Use the published aggregate series as if it tracked a real underlying monetary transition, drawing policy conclusions (about velocity, transmission mechanisms, financial stability) that are actually artifacts of when the classification boundary was drawn and redrawn. They have no practical exit from using the official series since it is the only continuously published data of its kind.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, policy_analysts_relying_on_aggregates, payer,
    moderate, biographical, constrained, national).

% The banks, clearinghouses, and early electronic-payment operators whose actual practices are retroactively sorted into 'electronic' or 'non-electronic' by a boundary drawn after the fact. They have no voice in how their historical instruments get classified and cannot contest a categorization applied decades later to justify a narrative of emergence they did not experience as an event.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, technology_and_institutional_actors_of_the_period, excluded,
    powerless, generational, trapped, national).

% Assess competing narratives of digital money's emergence, including this reading, and can in principle show that the statistical boundary is a measurement convention rather than a discovered natural kind — but their findings circulate mainly within specialist literature and rarely displace the operational convention that statistics agencies continue to use.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, economic_historiography_reviewers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The M4/M5 boundary lets central banks and analysts report a consistent, comparable time series of 'broad money' components across decades, which genuinely solves a real coordination problem: without SOME fixed accounting convention, monetary aggregates could not be compared over time at all.
% TRANSFER_FUNCTION: The arrangement transfers narrative authority — the power to say WHEN electronic money 'began' — from the messy, contested historical record of actual technology and institutional adoption to the statistics department's classification schedule. It moves analytical convenience toward those who maintain the series and moves interpretive cost onto historians and analysts who must treat an accounting line as though it were a discovered event.
% ABSENT_VOICES: The historical actors — early clearinghouses, the first electronic-transfer operators, the institutions whose ledgers were later sorted by the boundary — have no voice in how their practices are retroactively classified. Their absence means the 'emergence' narrative is authored entirely by the classifiers who came after the fact, not by anyone who lived through an actual transition.
% DISAPPEARANCE_RATIONALE: If the M4/M5 distinction were abolished tomorrow, the actual monetary instruments, payment technologies, and institutional practices it categorizes would continue exactly as before — nothing about how money moves or is held would change. Only the labeling convention and the statistical series built on it would vanish; forecasters would need a new aggregate boundary, and historians would lose (or be freed from) a spurious periodization, but no underlying monetary arrangement depends on this specific line existing.
% FOUNDING_PROBLEM: Central banks needed a stable, replicable accounting convention to track the growing share of bank deposits and instruments considered 'money' as electronic transfer and computerized banking became common, so that monetary policy could be based on consistent aggregate figures rather than ad hoc counts.
% FOUNDING_PROBLEM_CORROBORATION: Central bank methodological notes attest the ongoing need for a stable aggregate boundary for policy purposes (a claim from an interested party). Independent monetary historians and economic methodologists outside the statistics-producing institutions corroborate that SOME boundary convention remains functionally necessary for time-series comparability, while explicitly disputing that this particular boundary tracks any real monetary discontinuity — the coordination need is corroborated from outside; the emergence narrative built on top of it is not.
narrative_ontology:disappearance_verdict(electronic_money_emergence__m4_m5_collapse_reading, world_unchanged).
narrative_ontology:founding_problem_status(electronic_money_emergence__m4_m5_collapse_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__m4_m5_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__m4_m5_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__m4_m5_collapse_reading, 0.38, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored moderate-low (0.38 at interval end) because this is not primarily a rent-extraction structure — no one is charged a toll by the boundary — but there IS a real cost transferred: historians and policy analysts inherit interpretive costs (having to treat an artifact as an event, or do costly work to correct for it) that flow toward the maintainers of the convention, who get a stable citable narrative at low cost to themselves. Theater ratio is authored high and rising (0.35 to 0.71) because the coordination function (comparable aggregate reporting) is real but shrinking in relative importance while the narrative overlay — treating the boundary crossing as a historical 'emergence' worth dating — has grown substantially in secondary and policy literature, which is the classic piton signature: real function persists in a thin, unglamorous form while performative narrative content expands around it. Suppression is authored low-moderate (0.31): no one is coerced into accepting the framing, but the absence of a comparably continuous alternative data source functions as a soft lock-in. Accessibility collapse (0.58) reflects that once the aggregate series exists and is the primary continuously-published dataset, constructing an alternative periodization becomes costly enough that most researchers just don't.
 *
 * PERSPECTIVAL GAP:
 *   From the statistics department's seat, the boundary is simply a necessary and stable accounting convention — nothing has been claimed to 'emerge,' only reported. From the historian's seat, the same boundary functions as a silent narrative machine: because it is the only continuously available series, its internal transition point gets read as an external historical fact, an inference the statistics department never explicitly makes but does nothing to correct. The engine should compute these seats differently: the agenda-setter's structural position looks close to a rope (real coordination, low coercion) while the payer seats experience a piton-like drag (bearing the cost of a stabilized artifact whose original function has partially decoupled from what it is now used to claim).
 *
 * DIRECTIONALITY LOGIC:
 *   Central bank statistics departments and the forecasting industry built on their output sit near the beneficiary end: they get a stable, low-maintenance-cost narrative infrastructure. Monetary historians and policy analysts sit toward the target end: they bear the interpretive and analytical cost of a data source that pre-encodes a contestable periodization as though it were neutral bookkeeping. The excluded historical actors are not targets of extraction in a modern economic sense, but they are targets of a different kind of cost — the erasure of their actual, ungeneralizable practices into a binary retroactive classification they never consented to and could not have anticipated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing SOME stable accounting convention for monetary aggregates — remains live and is corroborated outside the beneficiary set, so this is not a pure mandatrophy case: the underlying coordination function has not fully died. What has drifted is the SECONDARY use of the convention as a historical-emergence marker, which the founding problem never justified and which the founding institutions did not set out to create. Classifying this as piton rather than snare or rope prevents two mislabeling errors: it does not treat the accounting convention itself as pure extraction (there is a real, still-live coordination function), and it does not credit the 'emergence' narrative built on top of it as though it were a discovered fact the statistics simply reveal. The piton frame isolates the correct object of critique: not the aggregate boundary's existence, but its retroactive narrativization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artifact_or_genuine_threshold,
    'Does the M4/M5 (or equivalent) boundary track any real discontinuity in monetary instruments'' functional properties, or is it purely a bookkeeping convenience that happens to correlate with unrelated technological change?',
    'Fine-grained archival reconstruction of instrument-level transaction data spanning the boundary''s introduction, compared against independent measures of electronic settlement adoption (e.g., clearinghouse automation records) to test whether the statistical boundary correlates with an actual step-change in settlement mechanics or merely with a reporting-convention change.',
    'If a genuine correlated discontinuity is found, this reading weakens substantially and the constraint may need reclassification toward a genuine (if narrow) rope; if no correlation is found, the piton/artifact reading is strengthened and the theatrical overlay identified in this story is confirmed as free-floating narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artifact_or_genuine_threshold, empirical, 'Whether the statistical boundary corresponds to any real underlying monetary discontinuity.').

omega_variable(
    committer_framing_location,
    'This story treats the kernel''s contest as located in WHEN (and whether) an emergence event occurred; a sibling framing could instead locate the contest in WHETHER ''electronic money'' is a natural kind at all, independent of dating. Under this reading, the disagreement is about dating and artifact-status; under the alternative framing, it would be about ontological category validity, which could produce a different classification (e.g., all three readings collapsing into variants of a single contested-ontology constraint rather than three separate emergence-dating claims).',
    'Compare how each reading''s proponents actually argue: if the debate in the literature is fought over DATES (this reading, became_thinkable, first_held), the dating framing is correct; if instead the debate is fought over whether ''electronic money'' names anything coherent, the ontological framing would be more accurate and the three stories would need restructuring.',
    'Under the current dating framing, the three readings are genuinely distinct constraints with different beneficiary/victim structures, as authored. Under the ontological framing, they might instead be facets of one deeper contest, changing how network edges and cross-reading omegas should be drawn.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_location, conceptual, 'Alternative framing of where the kernel''s contest is actually located (dating vs. ontological validity).').

omega_variable(
    beneficiary_intent_ambiguity,
    'Do central bank statistics departments actively benefit from (and thus have incentive to preserve) the retroactive emergence narrative, or do they merely fail to correct a narrative that outside commentators construct unprompted from their published series?',
    'Review internal methodological communications and public statements from statistics departments to see whether they endorse, are neutral toward, or have attempted to disclaim the ''emergence'' interpretation of their aggregate boundary.',
    'If departments actively promote the emergence narrative (e.g., in accessible publications, anniversaries, retrospectives), the beneficiary classification is well-grounded and closer to tangled-rope territory. If they are merely passive and have made disclaiming efforts, the piton classification is more clearly correct — this is inertia rather than cultivated benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_intent_ambiguity, empirical, 'Whether the beneficiary institutions actively cultivate or merely passively tolerate the retroactive-emergence narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t0, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(elec_tr_t8, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 8, 0.44).
narrative_ontology:measurement(elec_tr_t16, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 16, 0.53).
narrative_ontology:measurement(elec_tr_t24, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 24, 0.61).
narrative_ontology:measurement(elec_tr_t32, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 32, 0.67).
narrative_ontology:measurement(elec_tr_t40, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 40, 0.71).

% Extraction over time
narrative_ontology:measurement(elec_be_t0, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(elec_be_t8, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(elec_be_t16, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 16, 0.29).
narrative_ontology:measurement(elec_be_t24, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 24, 0.32).
narrative_ontology:measurement(elec_be_t32, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 32, 0.35).
narrative_ontology:measurement(elec_be_t40, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 40, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(electronic_money_emergence__m4_m5_collapse_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__m4_m5_collapse_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__m4_m5_collapse_reading, 0.05).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__first_held_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the electronic_money_emergence kernel. became_thinkable_reading locates emergence in conceptual/technical possibility prior to measurement; first_held_reading locates it in the first actual institutional holding of dematerialized currency; this reading (m4_m5_collapse_reading) denies a genuine emergence event exists at all, treating the appearance of one as an artifact of a later statistical classification boundary. Each reading carries its own epsilon and stakeholder structure per the ε-invariance principle; they are linked here as a constraint family rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
