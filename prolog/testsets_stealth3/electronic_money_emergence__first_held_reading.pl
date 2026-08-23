% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__first_held_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: First Institutional Bearing Threshold for Electronic Money
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This story instantiates the first_held_reading of the
 *   electronic_money_emergence kernel: digital money came into existence at a
 *   discrete, legally recognizable event — the first time an institutional
 *   bearer held dematerialized currency in a form distinguishable from
 *   physical notes. The standing arrangement under contest is the
 *   institutional dating convention built on that threshold: the practice of
 *   fixing the category boundary of electronic money at institutional
 *   recognition events in official statistics, licensing regimes, and legal
 *   precedent. Per the epsilon-referent rule, extractiveness is authored for
 *   THAT arrangement as this reading sees it: a mostly truth-tracking
 *   convention with a genuine coordination function that nonetheless
 *   concentrates category-defining authority in the institutions positioned
 *   to witness the threshold, and overrules rival periodizations in every
 *   venue that matters for canon formation. Claim/metric independence holds:
 *   claimed_type is tangled_rope (genuine coordination plus asymmetric
 *   authority extraction through the same structure); the metrics are
 *   authored descriptively and the engine computes per-seat types from the
 *   structural data. KEY AGENTS (by structural relationship): -
 *   central_bank_statistics_authorities: Agenda setter
 *   (institutional/constrained) — administers the money-proper boundary and
 *   collects the convention's category-defining authority -
 *   first_institutional_bearer_banks: Primary beneficiary (powerful/mobile) —
 *   holds historical primacy and regulatory precedent anchored to the
 *   threshold event - financial_conduct_regulators: Beneficiary
 *   (institutional/constrained) — receives a clean attachment trigger for
 *   e-money supervision - early_eft_network_operators: Primary target
 *   (organized/trapped) — pre-threshold systems classified as infrastructure,
 *   not money - gradualist_monetary_historians: Target
 *   (moderate/identity_locked) — continuous-evolution periodization overruled
 *   by the discrete threshold - nonbank_digital_wallet_pioneers: Excluded
 *   voice (moderate/constrained) — built dematerialized value outside the
 *   banking perimeter, absent from certification - macroeconomic_data_users:
 *   Incidental beneficiary (organized/mobile) — consume the stabilized
 *   boundary's output - monetary_theory_analysts: Analytical observer — sees
 *   the full structure across readings
 *
 * KEY AGENTS:
 *   - central_bank_statistics_authorities: agenda setter, institutional power, constrained exit — runs the boundary, collects the authority
 *   - first_institutional_bearer_banks: primary beneficiary, powerful, mobile — primacy and precedent from the dated event
 *   - financial_conduct_regulators: beneficiary, institutional, constrained — clean supervision trigger
 *   - early_eft_network_operators: primary target, organized, trapped — written out of the category they helped create
 *   - gradualist_monetary_historians: target, moderate, identity_locked — method overruled by the threshold
 *   - nonbank_digital_wallet_pioneers: excluded, moderate, constrained — no seat at certification
 *   - macroeconomic_data_users: incidental beneficiary, organized, mobile — consume the stabilized series
 *   - monetary_theory_analysts: analytical observer — comparative seat across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.42).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.46).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, tangled_rope).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "First Institutional Bearing Threshold for Electronic Money").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:requires_active_enforcement(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, 'f6a8f8c1-c46d-468e-b18d-560f9497350d').
narrative_ontology:cs_kernel_codification('f6a8f8c1-c46d-468e-b18d-560f9497350d', formalized).
narrative_ontology:cs_authority_grounding('f6a8f8c1-c46d-468e-b18d-560f9497350d', lineage).
narrative_ontology:cs_interpretation_layer_present('f6a8f8c1-c46d-468e-b18d-560f9497350d').
narrative_ontology:cs_reading_relation('f6a8f8c1-c46d-468e-b18d-560f9497350d', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6a8f8c1-c46d-468e-b18d-560f9497350d', electronic_money_emergence__m4_m5_collapse_reading, forecloses).
narrative_ontology:cs_axiom('f6a8f8c1-c46d-468e-b18d-560f9497350d', foundational, institutional_bearing_constitutes_money_proper).
narrative_ontology:cs_axiom_status(institutional_bearing_constitutes_money_proper, holdable).
narrative_ontology:cs_axiom_grounding('f6a8f8c1-c46d-468e-b18d-560f9497350d', institutional_bearing_constitutes_money_proper, conventional).
narrative_ontology:cs_axiom('f6a8f8c1-c46d-468e-b18d-560f9497350d', foundational, threshold_event_precedes_measurement).
narrative_ontology:cs_axiom_status(threshold_event_precedes_measurement, holdable).
narrative_ontology:cs_axiom_grounding('f6a8f8c1-c46d-468e-b18d-560f9497350d', threshold_event_precedes_measurement, empirically_contingent).
narrative_ontology:cs_reference_frame('f6a8f8c1-c46d-468e-b18d-560f9497350d', institutional_recognition_datum).
narrative_ontology:cs_drift_state('f6a8f8c1-c46d-468e-b18d-560f9497350d', contemporary_tokenized_instrument_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f6a8f8c1-c46d-468e-b18d-560f9497350d', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_bank_statistics_authorities).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, first_institutional_bearer_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, financial_conduct_regulators).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, macroeconomic_data_users).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, early_eft_network_operators).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, gradualist_monetary_historians).
narrative_ontology:constraint_vindicates(electronic_money_emergence__first_held_reading, institutional_recognition_constitutes_money_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the monetary aggregates and decides where the boundary between bookkeeping balances and money proper sits. Certifies when an instrument class crosses into the aggregates, publishes the dating used in official series, and defends the boundary each time a new instrument appears. The category-defining authority attaches to this office; the cost is that every blurred instrument generates reclassification work and challenge exposure. Leaving the framework would mean surrendering the office's grip on the aggregates its mandate requires it to publish.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_bank_statistics_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Held the first dematerialized balances in legally recognized bearer-distinguishable form. The dated event anchors their claim to historical primacy, supplies precedent their counsel cites in boundary disputes, and confers the regulatory standing of recognized money-holders. They can reorganize holdings freely; the primacy claim travels with the dated event regardless of what they do afterward.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, first_institutional_bearer_banks, beneficiary,
    powerful, biographical, mobile, global).

% License and supervise electronic-money issuance. The discrete threshold tells them exactly when supervision attaches: before it, balances are deposits or prepaid instruments; after it, licensable e-money. They collect jurisdictional clarity without administering the boundary themselves. Their statutes presuppose the boundary, so departing from it would require re-legislation.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, financial_conduct_regulators, beneficiary,
    institutional, generational, constrained, national).

% Built and ran large-scale electronic funds-transfer networks carrying dematerialized value decades before the recognized threshold. Under the dating convention their balances count as payment infrastructure rather than money proper, which places them before the beginning of the story they helped create. The classification attaches to their historical record; nothing they do now can move them across the line.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, early_eft_network_operators, payer,
    organized, biographical, trapped, continental).

% Periodize money's dematerialization as continuous technical evolution with no discrete origin. Official statistics, supervisory statutes, and textbooks cite the threshold date, overruling their datings in every venue that matters for canon formation. Their professional method is the gradualism the threshold rejects; abandoning it would dissolve the research program their careers are built on.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, gradualist_monetary_historians, payer,
    moderate, biographical, identity_locked, global).

% Consume the monetary aggregates for research and policy analysis. A stable money-proper boundary is what makes the series usable; they benefit from the convention's output without administering it. They can switch to alternative measures at analytic cost, so their position is comfortable but not captive.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, macroeconomic_data_users, beneficiary,
    organized, generational, mobile, global).

% Developed stored-value and wallet systems holding dematerialized purchasing power outside the banking perimeter, before and across the threshold. They had no seat in the statistical committees or hearings where the boundary was fixed; their instruments were classified as prepaid claims rather than money. They would contest both the date and the dichotomy the date rests on.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, nonbank_digital_wallet_pioneers, excluded,
    moderate, biographical, constrained, global).

% Study competing periodizations of dematerialized money across the economics, history, and sociology of money. Positioned to compare what each dating convention privileges and obscures; collects nothing and pays nothing under the arrangement.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, monetary_theory_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__first_held_reading, central_bank_statistics_authorities).
narrative_ontology:fixing_cost_class(electronic_money_emergence__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes one publicly verifiable date-event at which dematerialized balances count as money proper rather than bookkeeping entries, so that monetary statistics, licensing regimes, reserve and insurance treatment, and historiographic citation can share a single boundary instead of renegotiating it per dispute.
% TRANSFER_FUNCTION: Moves category-defining authority over electronic money from dispersed technical communities to the institutions positioned to witness and certify the threshold event, and moves historical primacy and regulatory precedent to the first institutional bearers; the costs fall on pre-threshold system builders and gradualist historians whose accounts the boundary overrules.
% ABSENT_VOICES: Pre-threshold electronic payment builders (early EFT network engineers, stored-value wallet developers) and gradualist monetary historians were not seated when the threshold was certified; they would contest the discreteness of the event and the note-versus-dematerialized dichotomy it rests on. They sit outside the statistical committees and legislative hearings where the boundary is maintained.
% DISAPPEARANCE_RATIONALE: If the threshold convention vanished overnight, monetary aggregates would lose their money-proper boundary mid-series, e-money licensing would lack its attachment trigger, and courts would have no precedent anchor. Statistical offices, supervisors, and legal systems would have to reconstruct a boundary immediately, and the reconstruction fight would itself rearrange the affected institutions.
% FOUNDING_PROBLEM: Statistical and legal authorities needed to determine when dematerialized balances become money rather than internal bookkeeping entries — for aggregate measurement, reserve and deposit-insurance treatment, and the attachment of electronic-money supervision.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: independent macroeconomists consuming monetary-aggregate series attest the measurement problem is live, since their published work depends on a defensible money-proper boundary, and international standards bodies drafting e-money supervision guidance attest the attachment-trigger problem is live. Neither group collects the convention's authority rents.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__first_held_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__first_held_reading_tests).
:- end_tests(electronic_money_emergence__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end): the convention tracks a real transition, but converting the witnessing of that transition into durable category-defining authority lets the same structure that coordinates measurement also concentrate interpretive power and double as a licensing moat. Suppression is moderate (0.46): rival periodizations survive fully in academic discourse but are excluded from official statistics, supervisory triggers, and textbook canon; persistence requires actively maintaining the boundary against each new instrument class, so the suppression figure reflects enforced exclusion from official venues rather than elimination of alternatives. Theater is low-moderate (0.25): the threshold performs real statistical and legal work; a growing share of activity is commemorative and definitional maintenance. Accessibility_collapse 0.40: workable alternatives remain in scholarship. Resistance 0.52: sustained revisionist pressure from historians and constructivist sociologists of money. All three metric series run on one shared time grid {0,6,12,18,24,30}; end-state values equal the scalar base_properties. The rising trajectories model accumulation: as the boundary matured from a statistical convenience into load-bearing legal and supervisory infrastructure, the rents attached to holding it grew.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the trapped payer seats should compute opposite classifications from identical structural facts. From inside the statistical authority, the threshold is simply where the phenomenon became administrable — the seat experiences near-coordination. From the early EFT operators' position, the same boundary is enforced exclusion: it places their systems before the beginning of the story they helped build, and nothing they can do moves them across the line. The gradualist historians experience a third variant: professional-method-level displacement, where the constraint operates on identity rather than on assets. The engine derives this divergence from power and exit asymmetry; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: the statistics authorities receive category-defining authority, the first bearers receive primacy and precedent, the conduct regulators receive a clean attachment trigger, and data users receive usable series — the convention subsidizes all four. Victims derive high directionality: EFT operators bear the historical-recording cost with trapped exit (the classification attaches to their past record), and gradualist historians bear the canonical-cost with identity-locked exit (their method is what the threshold rejects). Directionality override: central_bank_statistics_authorities would derive near-full-beneficiary (~0.10) from their beneficiary declaration alone, but they also absorb the recurring cost of defending the boundary — every blurred instrument class generates reclassification cycles and challenge exposure — so the override places them nearer symmetric at 0.28. Suppression is authored as a raw structural property and is deliberately left unscaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a defensible money-proper boundary for aggregate measurement and supervision attachment — remains live as new dematerialized instruments keep appearing, so the mandate has not outlived its function and mandatrophy_resolved is not declared. The tangled_rope classification guards both failure directions: reading the convention as pure rope would miss the authority concentration and licensing-moat effects that the payer seats demonstrably experience; reading it as snare would erase the genuine measurement function that independent data users corroborate from outside the benefiting parties. The mismatch consumer should find status=live paired with verdict=world_rearranges — no zombie flag expected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_first_held,
    'Is the discrete-institutional-event framing the right instantiation of the electronic_money_emergence kernel, or do the sibling readings (thinkable-prior, statistical-artifact) better capture the structure of the emergence question?',
    'Cross-story comparison of the three reading files: whichever reading''s beneficiary/victim structure best predicts where periodization disputes actually concentrate carries the kernel.',
    'Adopting became_thinkable_reading shifts beneficiaries toward pre-institutional technical communities and moves the event earlier than any legal-recognition datum; adopting m4_m5_collapse_reading dissolves the discrete event into measurement politics and removes the first-bearer beneficiary seat entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment_first_held, conceptual, 'Committer structure: this constraint is one reading of the electronic_money_emergence kernel; sibling readings change the beneficiary/victim sets and epsilon.').

omega_variable(
    bearer_distinguishability_criterion,
    'What makes dematerialized currency ''distinguishable from physical notes'' — legal form (constituted by statute and recognition) or functional substitutability (indistinguishable in ordinary use)?',
    'Examine whether the instruments at the recognized threshold were legally distinct bearer forms or merely functionally equivalent balances, and trace how courts subsequently treated the distinction.',
    'A legal-form criterion keeps the event discrete and epsilon near the authored value; a functional criterion blurs the threshold, weakens the discrete-event premise, and raises measured extraction as boundary maintenance expands to ever-more-substitutable instruments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bearer_distinguishability_criterion, conceptual, 'Whether the threshold''s distinguishing criterion is constitutive-legal or functional.').

omega_variable(
    first_bearer_identifiability,
    'Which institution and which event actually qualify as the first institutional bearer holding — candidate events differ across jurisdictions and settlement systems?',
    'Archival verification of the earliest legally recognized dematerialized bearer holding, dated jurisdiction by jurisdiction.',
    'A single identifiable event supports the discrete-threshold premise; multiple near-simultaneous regional candidates weaken discreteness and pull the reading toward the thinkable sibling''s gradualism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_bearer_identifiability, empirical, 'Empirical identifiability of the singular founding event the reading requires.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eem_first_held_tr_t0, electronic_money_emergence__first_held_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(eem_first_held_tr_t6, electronic_money_emergence__first_held_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(eem_first_held_tr_t12, electronic_money_emergence__first_held_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(eem_first_held_tr_t18, electronic_money_emergence__first_held_reading, theater_ratio, 18, 0.2).
narrative_ontology:measurement(eem_first_held_tr_t24, electronic_money_emergence__first_held_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(eem_first_held_tr_t30, electronic_money_emergence__first_held_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(eem_first_held_be_t0, electronic_money_emergence__first_held_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(eem_first_held_be_t6, electronic_money_emergence__first_held_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(eem_first_held_be_t12, electronic_money_emergence__first_held_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(eem_first_held_be_t18, electronic_money_emergence__first_held_reading, base_extractiveness, 18, 0.39).
narrative_ontology:measurement(eem_first_held_be_t24, electronic_money_emergence__first_held_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(eem_first_held_be_t30, electronic_money_emergence__first_held_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(eem_first_held_su_t0, electronic_money_emergence__first_held_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(eem_first_held_su_t6, electronic_money_emergence__first_held_reading, suppression_requirement, 6, 0.37).
narrative_ontology:measurement(eem_first_held_su_t12, electronic_money_emergence__first_held_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(eem_first_held_su_t18, electronic_money_emergence__first_held_reading, suppression_requirement, 18, 0.43).
narrative_ontology:measurement(eem_first_held_su_t24, electronic_money_emergence__first_held_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(eem_first_held_su_t30, electronic_money_emergence__first_held_reading, suppression_requirement, 30, 0.46).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, information_standard).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'when did digital money emerge' decomposes into three epsilon-invariant readings of one kernel, per DP-001. This file is the first_held_reading (discrete institutional event; epsilon ~0.42; institutional beneficiaries). became_thinkable_reading carries a different epsilon and a different beneficiary set (pre-institutional technical communities); m4_m5_collapse_reading treats the category as a measurement artifact with no pre-statistical event at all. Each member links the others via affects_constraints. Direction of influence: the institutional-fixation reading shapes the operating environment of the other two — once the official date is fixed, the thinkable account is recast as precursor history and the artifact account as metatheoretical dissent — without resolving the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electronic_money_emergence__first_held_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
