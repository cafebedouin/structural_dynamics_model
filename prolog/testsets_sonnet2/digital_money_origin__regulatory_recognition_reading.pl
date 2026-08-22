% ============================================================================
% CONSTRAINT STORY: digital_money_origin__regulatory_recognition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__regulatory_recognition_reading, []).

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
 *   constraint_id: digital_money_origin__regulatory_recognition_reading
 *   human_readable: Regulatory Recognition as the Origin of Digital Money
 *   domain: monetary_history/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the regulatory_recognition_reading of the
 *   digital_money_origin kernel: digital money is treated as having emerged
 *   only when monetary authorities formally incorporated it into statistical
 *   aggregates (M1/M2/M3-style measures) and regulatory perimeters (e-money
 *   directives, payment services licensing, stablecoin reserve regimes). This
 *   reading dates the origin latest among the three sibling readings — later
 *   than the moment the concept became technically conceivable, and later
 *   than the moment individuals first held and used non-physical instruments
 *   as stores of value. The constraint set here is dominated by legal and
 *   regulatory barriers rather than technical ones: incumbent,
 *   already-licensed financial institutions are structurally positioned to
 *   benefit from recognition regimes built in their image, while innovators
 *   who built working digital money before any authority counted it face
 *   reclassification, compliance cost, or existential enforcement risk. This
 *   is a single, ε-invariant reading — the sibling readings (technical
 *   conceivability, first practical holding) are separate constraint stories
 *   with their own ε and stakeholder structures, linked via the network
 *   field, not folded into this one.
 *
 * KEY AGENTS:
 *   - central_bank_statistical_authorities: agenda_setter (institutional/analytical) — sets the recognition criteria that define the origin date under this reading
 *   - incumbent_deposit_taking_banks: beneficiary (powerful/arbitrage) — pre-fitted to recognition regimes, gains legitimacy and market position
 *   - unregulated_payment_innovators: payer (moderate/constrained) — built the instruments now retroactively excluded from 'origin' status and exposed to enforcement
 *   - early_stablecoin_and_e_money_issuers: payer (moderate/constrained) — circulated real digital money before recognition, treated as pre-history
 *   - financial_historians: observer (analytical) — documents the gap between actual first use and formal recognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.61).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.58).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Regulatory Recognition as the Origin of Digital Money").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "monetary_history/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, '933b0f4d-6d91-4393-bfbd-45d8dd8b587a').
narrative_ontology:cs_kernel_codification('933b0f4d-6d91-4393-bfbd-45d8dd8b587a', distributed).
narrative_ontology:cs_authority_grounding('933b0f4d-6d91-4393-bfbd-45d8dd8b587a', extraction).
narrative_ontology:cs_interpretation_layer_present('933b0f4d-6d91-4393-bfbd-45d8dd8b587a').
narrative_ontology:cs_reading_relation('933b0f4d-6d91-4393-bfbd-45d8dd8b587a', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('933b0f4d-6d91-4393-bfbd-45d8dd8b587a', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_axiom('933b0f4d-6d91-4393-bfbd-45d8dd8b587a', foundational, existence_requires_formal_state_ratification).
narrative_ontology:cs_axiom_status(existence_requires_formal_state_ratification, holdable).
narrative_ontology:cs_axiom_grounding('933b0f4d-6d91-4393-bfbd-45d8dd8b587a', existence_requires_formal_state_ratification, conventional).
narrative_ontology:cs_axiom('933b0f4d-6d91-4393-bfbd-45d8dd8b587a', secondary, aggregate_measurement_constitutes_the_measured_phenomenon).
narrative_ontology:cs_axiom_status(aggregate_measurement_constitutes_the_measured_phenomenon, holdable).
narrative_ontology:cs_axiom_grounding('933b0f4d-6d91-4393-bfbd-45d8dd8b587a', aggregate_measurement_constitutes_the_measured_phenomenon, instrumental).
narrative_ontology:cs_reference_frame('933b0f4d-6d91-4393-bfbd-45d8dd8b587a', pre_digital_fiat_monetary_perimeter).
narrative_ontology:cs_drift_state('933b0f4d-6d91-4393-bfbd-45d8dd8b587a', post_stablecoin_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('933b0f4d-6d91-4393-bfbd-45d8dd8b587a', '').
narrative_ontology:cs_kernel_id(digital_money_origin__regulatory_recognition_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, incumbent_deposit_taking_banks).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, central_bank_statistical_authorities).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unregulated_payment_innovators).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, early_stablecoin_and_e_money_issuers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, monetary_policy_committees).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, monetary_aggregate_measurement_authority).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, regulatory_perimeter_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which instruments count toward M1/M2/M3 and similar aggregates, and which entities fall inside prudential and payments regulation. This decision is the act the reading identifies as the moment digital money 'emerged' — not the technology's invention or first use, but its formal absorption into the statistical and legal apparatus. Controls the criteria and can redraw the perimeter at will.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, central_bank_statistical_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, central_bank_statistical_authorities, beneficiary).

% Already hold banking licenses and are pre-fitted to whatever regulatory category digital money is assigned to; new recognition regimes are typically built around bank-compatible custody, settlement, and reporting structures. Benefits from the recognition event because it retroactively legitimizes deposit-linked digital instruments while raising the bar for anyone outside the licensed perimeter.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_deposit_taking_banks, beneficiary,
    powerful, generational, arbitrage, national).

% Built working digital payment and value-transfer systems before any authority counted them as money. Once regulatory recognition occurs, their instruments are reclassified as either compliant (requiring costly licensing, capital, and reporting they may not survive) or as unlicensed money transmission subject to enforcement. Their prior technical achievement is retroactively erased from the origin story in favor of the recognition date.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_payment_innovators, payer,
    moderate, biographical, constrained, national).

% Issued digital value instruments that circulated and were used as money by real holders well before any monetary authority added them to an aggregate or regulatory schedule. Under this reading their activity does not count as the origin of digital money at all — it is pre-history — until an authority ratifies it. This shapes which entities get grandfathered and which get treated as having invented nothing until permitted to exist.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, early_stablecoin_and_e_money_issuers, payer,
    moderate, biographical, constrained, continental).

% Depends on clean, authoritative monetary aggregates to conduct policy. Recognition brings digital instruments inside the measured money supply, restoring their ability to claim complete visibility over the transmission mechanism. Benefits from a bright-line origin date because it makes the entire pre-recognition period analytically simple to ignore.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_policy_committees, beneficiary,
    institutional, civilizational, analytical, national).

% Documents the gap between when digital money instruments were actually created and used, and when authorities began counting them. Notes that this reading systematically dates 'emergence' to the latest possible point among the three candidate readings, and that this dating choice is not neutral — it privileges the institutions that did the counting.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, financial_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Formal incorporation into statistical aggregates and regulatory frameworks solves a genuine coordination problem: policymakers, banks, and the public need a shared, authoritative account of what counts as money in order to conduct monetary policy, assess systemic risk, and apply consumer protections consistently.
% TRANSFER_FUNCTION: Moves legitimacy, market access, and reduced compliance friction toward entities already structured to fit the recognized category (chiefly licensed banks), while moving cost and existential risk onto entities whose digital money predates the category and must now retrofit themselves into it or exit.
% ABSENT_VOICES: The unregulated innovators and early issuers who actually built the instruments being recognized are not parties to the recognition decision — the monetary authority decides unilaterally what counts, when, and under what conditions, without the builders present at the table where the origin date is set.
% DISAPPEARANCE_RATIONALE: If regulatory recognition vanished, incumbent banks assert monetary stability and consumer protection would erode; unregulated innovators and historians would argue the underlying money already existed and worked — only the state's bookkeeping and licensing apparatus would vanish, not the money itself. The two camps disagree about whether the recognition event is constitutive of digital money's existence or merely descriptive of it.
% FOUNDING_PROBLEM: Monetary authorities needed accurate, comprehensive aggregates and clear regulatory perimeters to conduct policy and prevent unsupervised money-like instruments from creating systemic or consumer risk outside their view.
% FOUNDING_PROBLEM_CORROBORATION: Central bank research departments and academic monetary economists outside the incumbent banking sector corroborate that aggregate visibility and prudential perimeter-setting remain live functional needs; however, the same outside economists frequently note that the recognition date is a political and administrative artifact, not evidence that the underlying money did not exist earlier — this partial corroboration is itself contested.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, contested).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__regulatory_recognition_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__regulatory_recognition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__regulatory_recognition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects that recognition regimes function as gatekeeping: value flows toward already-licensed incumbents who can absorb compliance cost, while it is extracted (via forced compliance spend, market exclusion, or enforcement) from earlier movers who lack banking-style infrastructure. Suppression (0.58) is substantial but not maximal because innovators retain the option of seeking licensure or relocating to friendlier jurisdictions — the exit is constrained, not fully trapped. Theater ratio (0.32) is moderate: statistical aggregation is a real functional need (policy visibility), but a portion of the recognition apparatus increasingly serves to perform regulatory thoroughness and jurisdictional primacy rather than to capture genuinely new information. accessibility_collapse (0.5) and resistance (0.55) reflect that alternatives to the recognition-gated framing persist — unlicensed and offshore digital money markets continue to operate — so collapse is partial and resistance from excluded innovators is real and ongoing.
 *
 * PERSPECTIVAL GAP:
 *   From the statistical-authority seat, recognition is simply the natural completion of a measurement problem — digital money 'became real' to policy once it could be counted. From the seat of an issuer who had already been running a functioning digital money system for years before recognition, the same event looks like a jurisdictional land-grab: an external body declaring the start date of something that was, from the inside, already well underway. The engine computes these as structurally different positions from the same beneficiary/victim/power declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Central bank statistical authorities and monetary policy committees sit at the low-d, beneficiary end: recognition centralizes their epistemic authority over 'what counts as money' and vindicates their aggregate-measurement mandate. Incumbent banks derive high benefit with low structural cost because recognition regimes are typically drafted around bank-compatible custody and settlement, giving them arbitrage-grade exit relative to the category itself. Unregulated innovators and early issuers sit at the high-d, target end: their prior activity is definitionally excluded from 'origin' under this reading, and post-recognition their operations face reclassification risk, compliance burden, or enforcement — constrained exit because relocating or seeking licensure carries real cost and uncertain outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (authorities needing comprehensive, trustworthy monetary aggregates to conduct policy) remains live — this is not a pure mandatrophy case. But the LATEST-POSSIBLE dating convention this reading embeds functions as a mandatrophy risk vector: by defining 'emergence' as the moment of formal recognition rather than the moment of functional existence, the reading structurally re-centers the recognizing authority as the origin-point of a phenomenon it did not create, and licenses treating pre-recognition activity as illegitimate rather than as unrecognized. Classifying this as tangled_rope (not snare) preserves the genuine coordination function of monetary aggregation while flagging the asymmetric extraction that rides on it — collapsing it to pure snare would erase the real policy-visibility need; collapsing it to pure rope would erase the retroactive delegitimization of prior innovators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    origin_date_as_political_artifact,
    'Is the regulatory recognition date a genuine marker of digital money''s emergence, or an administrative artifact that retroactively erases functioning prior instruments from the historical record?',
    'Comparative historical analysis: track usage volume, transactional velocity, and store-of-value function of digital instruments in the years before formal recognition versus after, across multiple jurisdictions with staggered recognition dates.',
    'If pre-recognition usage was already extensive and functionally indistinguishable from post-recognition usage, the recognition-reading''s origin date is exposed as a jurisdictional/administrative convenience rather than a substantive emergence event — strengthening the case that this reading primarily serves incumbent legitimation rather than describing when digital money actually began.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(origin_date_as_political_artifact, conceptual, 'Whether the recognition date measures genuine emergence or administrative convenience.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Does adopting the regulatory_recognition_reading as authoritative for policy or legal purposes foreclose the first_held_reading and became_thinkable_reading as live alternatives within the same institutional framework, or do all three persist as coexisting accounts held by different communities (technologists, users, regulators)?',
    'Examine whether legal/regulatory texts that adopt the recognition date explicitly deny or are silent on prior functional existence; silence permits coexistence, explicit denial signals foreclosure.',
    'If regulatory texts explicitly assert that no digital money existed before recognition, this would push the reading toward forecloses on first_held_reading rather than coexists_with; current evidence suggests coexistence across different discourse communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, conceptual, 'Whether the recognition reading logically forecloses or merely coexists with the sibling readings.').

omega_variable(
    incumbent_benefit_intentionality,
    'Is the incumbent-favoring structure of recognition regimes an intentional design choice by regulators (possibly captured or influenced by incumbent lobbying) or an unintended consequence of building regulatory categories around the only pre-existing, well-understood institutional form (banks)?',
    'Legislative history and rulemaking record analysis: examine comment periods, lobbying disclosures, and drafting committee composition for recognition frameworks (e.g., e-money directives, stablecoin reserve rules).',
    'Evidence of active incumbent lobbying shaping category design would strengthen the tangled_rope classification and suggest the extraction is deliberate; evidence of path-dependent regulatory convenience would suggest the extraction is a structural byproduct rather than a captured outcome, though still real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incumbent_benefit_intentionality, empirical, 'Whether incumbent-favoring recognition design is captured or path-dependent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__regulatory_recognition_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(digi_tr_t8, digital_money_origin__regulatory_recognition_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(digi_tr_t16, digital_money_origin__regulatory_recognition_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(digi_tr_t24, digital_money_origin__regulatory_recognition_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(digi_tr_t32, digital_money_origin__regulatory_recognition_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(digi_tr_t40, digital_money_origin__regulatory_recognition_reading, theater_ratio, 40, 0.32).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(digi_be_t8, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(digi_be_t16, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(digi_be_t24, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(digi_be_t32, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(digi_be_t40, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(digi_su_t8, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(digi_su_t16, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(digi_su_t24, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(digi_su_t32, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(digi_su_t40, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__first_held_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the digital_money_origin kernel. became_thinkable_reading dates origin to technical/institutional conceivability (earliest, likely lowest ε, diffuse or absent beneficiary structure). first_held_reading dates origin to the first practical individual holding of non-physical monetary instruments (middle date, beneficiaries plausibly early adopters/issuers). This story, regulatory_recognition_reading, dates origin latest, to formal incorporation into statistical aggregates and regulatory frameworks, and carries the highest ε and clearest incumbent-benefit/innovator-cost structure of the three. Each reading is authored as a separate, ε-invariant constraint per the decomposition principle; they are linked here rather than merged because the underlying premise of each reading — what counts as 'emergence' — differs in a way that changes beneficiaries, victims, and extraction levels, not merely the observable used to measure a single shared constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
