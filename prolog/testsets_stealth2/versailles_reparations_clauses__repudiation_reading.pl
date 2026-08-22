% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__repudiation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__repudiation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Versailles Reparations Clauses — Repudiation Reading (Duress-Nullity Doctrine)
 *   domain: international relations/legal history/political economy
 *
 * SUMMARY:
 *   A defeated Germany signs the Versailles reparations provisions (Articles
 *   231 and 232ff) in June 1919; the treaty text becomes a contested kernel
 *   read three ways. This story instantiates the repudiation reading alone:
 *   the instrument was imposed under duress and is therefore void, leaving
 *   Germany no binding payment obligation beyond token gestures. Generated as
 *   a clean, epsilon-invariant constraint, the reading operates — from the
 *   nationalist opposition of 1919 through strategic default, the Ruhr
 *   crisis, the Dawes and Young interludes, the 1931 moratorium, the 1932
 *   Lausanne termination, and open repudiation under the 1933 government — as
 *   a rule that extinguishes the entire creditor claim structure while
 *   converting the released capacity into German fiscal and military power.
 *   The epsilon referent is the standing creditor-debtor settlement as this
 *   reading governs it; by the reading's own lights the nullified claims
 *   lacked legitimate origin, but epsilon measures the structural transfer of
 *   claim-value, which is what the metric records. Claim and metrics are
 *   authored independently: the reading presents itself as simple legal
 *   rectitude (a restorative, rope-like correction), while the authored
 *   metrics describe a substantially extractive, actively enforced
 *   arrangement — the engine measures that divergence. KEY AGENTS (by
 *   structural relationship): - german_state_treasury: Agenda-setting debtor
 *   administration ([institutional]/[arbitrage]) — sets the payment posture
 *   and collects the released fiscal capacity - german_rearmament_programs:
 *   Primary structural beneficiary ([organized]/[identity_locked]) — funded
 *   by the released capacity - german_domestic_taxpayers: Diffuse beneficiary
 *   ([moderate]/[constrained]) — spared transfer taxation -
 *   allied_creditor_governments: Primary target ([powerful]/[trapped]) —
 *   claims rendered uncollectible - war_damaged_civilian_claimants: Secondary
 *   target ([powerless]/[trapped]) — compensation stream terminated -
 *   allied_bondholders: Financial target ([organized]/[constrained]) —
 *   recovery values collapse - minor_allied_claimant_states: Excluded
 *   claimants ([powerless]/[trapped]) — no seat at the terminal negotiations
 *   - independent_transfer_analysts: Analytical observer
 *   ([analytical]/[analytical]) — documents the structure
 *
 * KEY AGENTS:
 *   - german_state_treasury: Agenda-setting debtor administration (institutional/arbitrage) — administers the non-payment posture and collects the released fiscal capacity
 *   - german_rearmament_programs: Primary structural beneficiary (organized/identity_locked) — expansion tracks each relaxation of creditor claims
 *   - german_domestic_taxpayers: Diffuse beneficiary (moderate/constrained) — spared the taxation a full transfer program would require
 *   - allied_creditor_governments: Primary target (powerful/trapped) — treaty claims rendered uncollectible paper
 *   - war_damaged_civilian_claimants: Secondary target (powerless/trapped) — pension and compensation stream terminated with the regime
 *   - allied_bondholders: Financial target (organized/constrained) — recovery values collapse across successive default cascades
 *   - minor_allied_claimant_states: Excluded claimants (powerless/trapped) — treaty entitlements reallocated and extinguished without separate representation
 *   - independent_transfer_analysts: Analytical observer (analytical/analytical) — documents the transfer arithmetic and negotiation record from outside the contending governments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.84).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.7).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Versailles Reparations Clauses — Repudiation Reading (Duress-Nullity Doctrine)").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international relations/legal history/political economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, 'a0e9894c-6261-4cbc-a2b9-d85c6ce67493').
narrative_ontology:cs_kernel_codification('a0e9894c-6261-4cbc-a2b9-d85c6ce67493', fixed_text).
narrative_ontology:cs_authority_grounding('a0e9894c-6261-4cbc-a2b9-d85c6ce67493', lineage).
narrative_ontology:cs_interpretation_layer_present('a0e9894c-6261-4cbc-a2b9-d85c6ce67493').
narrative_ontology:cs_reading_relation('a0e9894c-6261-4cbc-a2b9-d85c6ce67493', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('a0e9894c-6261-4cbc-a2b9-d85c6ce67493', versailles_reparations_clauses__limited_responsibility_reading, coexists_with).
narrative_ontology:cs_axiom('a0e9894c-6261-4cbc-a2b9-d85c6ce67493', foundational, duress_vitiates_treaty_obligation).
narrative_ontology:cs_axiom_status(duress_vitiates_treaty_obligation, holdable).
narrative_ontology:cs_axiom_grounding('a0e9894c-6261-4cbc-a2b9-d85c6ce67493', duress_vitiates_treaty_obligation, deontological).
narrative_ontology:cs_axiom('a0e9894c-6261-4cbc-a2b9-d85c6ce67493', secondary, article_231_war_guilt_fiction).
narrative_ontology:cs_axiom_status(article_231_war_guilt_fiction, holdable).
narrative_ontology:cs_axiom_grounding('a0e9894c-6261-4cbc-a2b9-d85c6ce67493', article_231_war_guilt_fiction, empirically_contingent).
narrative_ontology:cs_reference_frame('a0e9894c-6261-4cbc-a2b9-d85c6ce67493', void_ab_initio_coerced_instrument).
narrative_ontology:cs_drift_state('a0e9894c-6261-4cbc-a2b9-d85c6ce67493', post_lausanne_rearmament_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a0e9894c-6261-4cbc-a2b9-d85c6ce67493', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_state_treasury).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_rearmament_programs).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_domestic_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, allied_creditor_governments).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, war_damaged_civilian_claimants).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, allied_bondholders).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, duress_vitiates_consent_doctrine).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, transfer_problem_impossibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Reich's payment posture: decides which obligations to acknowledge, which to defer, and which token gestures to offer. Converts each relaxation of creditor claims into budgetary headroom, increasingly directed to armament lines after 1933. Faces episodic creditor pressure abroad and nationalist pressure at home to concede nothing; its leadership changes several times across the interval while the non-payment posture deepens. Its freedom to reframe the obligation question through new doctrine and financial maneuvering is greater than any other seat's.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_state_treasury, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive the fiscal space released by suspended and extinguished payment schedules. Expansion milestones track each step of claim relaxation — the moratorium year, the Lausanne termination, the open repudiation. The programs' planning assumes the settlement never reverses; a restored full payment schedule would consume their funding base. Over the interval the programs become fused with the regime's self-conception, such that abandoning the settlement would mean dissolving what they have become.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_rearmament_programs, beneficiary,
    organized, generational, identity_locked, national).

% Are spared the taxation a full transfer program would have required — contemporary estimates ran to a large share of national income. The relief is diffuse and passive; they neither set the posture nor organize around it, and household consumption recovers as the external drain ends.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_domestic_taxpayers, beneficiary,
    moderate, biographical, constrained, national).

% Hold treaty claims measured in tens of billions of gold marks, allocated among themselves by inter-allied debt agreements. Enforcement attempts — the occupation of the Ruhr foremost — cost more than they recovered and fractured the coalition. Successive conferences scale the claims down; the 1932 conference ends the regime with a nominal final settlement conditioned on a loan that is never issued. By the mid-1930s the claims are uncollectible paper with no market, no enforcement path, and no buyer.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_creditor_governments, payer,
    powerful, biographical, trapped, continental).

% Pensioners, widows, and residents of devastated regions in northern France and Flanders whose compensation was to be funded from reparation receipts. Every downward revision of the schedule shrinks their expected recovery; the regime's termination ends it. They have no separate representation at any negotiating table and no means of pressing claims independently.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, war_damaged_civilian_claimants, payer,
    powerless, biographical, trapped, regional).

% Hold reparation-linked instruments and the war debts of allied governments, organized through protective committees in London and New York. Each default cascade — Russian, German, then inter-governmental — cuts recovery values; committees negotiate and publish but cannot compel payment. Some paper trades at deep discounts, the only exit available.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_bondholders, payer,
    organized, biographical, constrained, global).

% Smaller allies — Serbia/Yugoslavia, Romania, Greece, Portugal — hold treaty entitlements to reparation shares. They command no enforcement capacity of their own, are not separately represented at the decisive conferences, and watch their shares evaporate in each inter-allied reallocation without a distinct voice.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, minor_allied_claimant_states, excluded,
    powerless, generational, trapped, regional).

% Economists and legal scholars outside the contending governments — contemporaries documenting the transfer problem's arithmetic and later historians reconstructing the negotiation record. They publish analyses both camps quote selectively; they hold no enforcement role and no stake in the outcome.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, independent_transfer_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__repudiation_reading, german_state_treasury).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__repudiation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Terminates an economically unpayable cross-border obligation schedule: the Versailles payment demands exceeded what German exports and finances could transfer without collapsing the German economy and destabilizing the creditor economies' import markets. The repudiation rule settles, once and for all, how the regime ends — releasing both sides from a decade of enforcement crises, moratoria, and renegotiation cycles.
% TRANSFER_FUNCTION: Moves capitalized claim-value — the reparation debt and the inter-allied debts pegged to it — from Allied creditor governments, their bondholders, and war-damaged claimants to the German state, realized as untaxed fiscal capacity and, after 1933, as armament expenditure. Compensation to the losing claimants: none.
% ABSENT_VOICES: Minor allied claimant states and individual war-damaged claimants had no seat at the terminal negotiations; their shares were reallocated and extinguished by the great powers bilaterally. German democratic fulfillment politicians who accepted bounded obligations were marginalized at home before the endgame. The 1932 settlement's nominal condition — an international loan — was never issued, and no body represented the claimants' interest in that failure.
% DISAPPEARANCE_RATIONALE: If the repudiation rule vanished overnight — if the obligations were suddenly treated as binding again — German fiscal-military planning of the 1930s collapses (the budgets that funded rearmament assumed the claims were dead), creditor governments face uncollectible claims they had politically written off, and the diplomatic architecture built on the 1932 termination unwinds. The arrangement's disappearance forces a wholesale renegotiation under far worse conditions.
% FOUNDING_PROBLEM: The Versailles schedule was unenforceable as written: the transfer burden exceeded German capacity, creditor coalitions fractured over enforcement costs, and the German polity rejected the war-guilt foundation. The repudiation doctrine was built to resolve this — to end the obligation regime entirely on German terms rather than through indefinite scaling-down.
% FOUNDING_PROBLEM_CORROBORATION: Independent economic-historical scholarship attests the schedule's unenforceability and the regime's terminal date — the transfer-problem literature beginning with Keynes and the standard historiography treating the 1932 Lausanne conference as the regime's end; no successor government, court, or claimant institution has pursued the extinguished claims since. Corroboration comes from outside the benefiting parties; the German state's own advocacy is not the source.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__repudiation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__repudiation_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__repudiation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__repudiation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.84 at interval end) because the operative rule cancels the whole capitalized claim structure with zero compensation, concentrating the loss on the least represented holders. Suppression (0.70) is a raw, unscaled structural property: by the interval's end the claims' revival is foreclosed less by argument than by armed deterrence and the absence of any institutional path back. Theater (0.30) reflects the token-gesture layer — fulfillment ceremonies, final-payment rhetoric — real but subordinate to the nullification function. Accessibility_collapse (0.58): after Lausanne, revived schedules, arbitration, and partial-payment alternatives collapse, though deferred legal residues lingered in later debt agreements. Resistance (0.72): the Ruhr occupation, coalition diplomacy, and two decades of contention show the rule never rested on assent. The three metric series share one eight-point grid (t=0..20); the suppression_requirement series traces an enforcement ratchet to the Ruhr peak followed by transformation into deterrent dominance — enforcement decaying in the creditor direction while hardening in the debtor's. The mid-series extractiveness dip (t=6) marks the Dawes restoration, when creditor enforcement briefly succeeded.
 *
 * PERSPECTIVAL GAP:
 *   The German seats and the creditor seats should compute opposite types from identical structure. From the treasury and rearmament seats the arrangement is a liberation: burdens vanish, capacity accrues, and the doctrine reads as legal rectitude restored. From the creditor government, bondholder, and claimant seats the same rule operates as uncompensated confiscation of acquired rights with no exit. Coalition prospects for the powerless seats are poor: war-damaged claimants and minor allied states are dispersed, impoverished, and institutionally unrepresented, and the organized bondholders' market lever presses as much on their own governments as on the debtor. The engine computes this per-seat divergence from the power and exit data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the treasury (agenda-setting collector with arbitrage-grade freedom to reframe obligations) sits nearest the beneficiary pole; rearmament programs, identity-fused with the settlement that funds them, sit nearly as low; taxpayers receive a diffuse passive subsidy. Victim declarations drive high directionality: civilian claimants and minor allied states — powerless and trapped — sit nearest the target pole; creditor governments are high despite great-power standing because their claims have no market and no enforcement path; bondholders are high with slight moderation from discounted-sale exit. No directionality overrides are needed: the beneficiary/victim and exit declarations reproduce the structural relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enforcing an unpayable schedule — is dead, which invites filing the whole structure as resolved or inertial. That would mislabel it twice. It is not a piton: nothing about the arrangement was maintained by inertia or performance; it was carried to completion by escalating enforcement, from default politics through occupation-resistance to armed deterrence, and its consequences remain load-bearing in every subsequent account of the period. Nor is it a rope: the termination's benefits accrued entirely to one side while identifiable losers — compensated by nothing — absorbed the write-off. The tangled_rope classification holds both truths in one structure: a genuine coordination service (ending a regime every serious analyst agreed was unworkable) delivered through a mechanism that simultaneously extracted the entire claim-value from the weakest holders. The mandatrophy discipline prevents the doctrine's self-presentation — mere legal rectitude, nothing taken that was rightfully held — from laundering the asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta,
    'This constraint is the repudiation_reading of the versailles_reparations_clauses kernel; how would the structural data change under the sibling readings?',
    'Instantiate the sibling stories (punitive_liability_reading, limited_responsibility_reading) and compare victim sets, epsilon, and enforcement structure; the disagreement lives in instrument-validity and obligation-extent.',
    'Under the punitive reading the victim set relocates to German taxpayers with near-total burden; under the limited reading transfers become capacity-bounded and both seats bear moderate burdens; this reading''s tangled_rope classification does not transfer to siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Committer structure: one reading of the Versailles reparations kernel; sibling structural deltas.').

omega_variable(
    duress_factual_trigger,
    'Do the historical facts support the duress premise — continued blockade, starvation conditions, and threatened resumption of hostilities at the June 1919 signature?',
    'Archival and diplomatic record of the signature circumstances; historiographic consensus assessment of the coercion conditions.',
    'If the duress facts fail, the foundational axiom loses its factual trigger and the reading collapses toward the limited_responsibility_reading''s bounded-obligation structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_factual_trigger, empirical, 'Empirical basis of the duress-vitiates-consent premise.').

omega_variable(
    principle_selectivity,
    'Is duress-vitiates-consent wielded as a general principle or selectively, invoked where the invoker benefits?',
    'Comparative application across the parallel settlements imposed on Germany''s allies (Saint-Germain, Trianon, Neuilly) and across interwar sovereign-debt practice.',
    'Selective application shifts the doctrine''s legitimacy claim toward cover-story status and pushes the computed type toward the snare end; consistent application supports the coordination-function half of the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(principle_selectivity, conceptual, 'Generality versus selectivity of the duress principle across interwar settlements.').

omega_variable(
    counterfactual_enforcement_viability,
    'Could the creditor coalition ever have sustained enforcement of the full schedule, or was termination overdetermined by the transfer arithmetic?',
    'Counterfactual diplomatic analysis: Anglo-American debt-linkage scenarios, sustained coalition cohesion, and the export-absorption capacity of recipient markets.',
    'If enforcement was never viable, the nullification approaches inevitable adjustment (rope-weighted); if it was viable but abandoned under pressure, the extraction component dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_enforcement_viability, empirical, 'Whether claim nullification was contingent power-play or overdetermined adjustment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t0, versailles_reparations_clauses__repudiation_reading, theater_ratio, 0, 0.6).
narrative_ontology:measurement_basis(vers_tr_t0, observed).
narrative_ontology:measurement(vers_tr_t3, versailles_reparations_clauses__repudiation_reading, theater_ratio, 3, 0.52).
narrative_ontology:measurement_basis(vers_tr_t3, observed).
narrative_ontology:measurement(vers_tr_t6, versailles_reparations_clauses__repudiation_reading, theater_ratio, 6, 0.48).
narrative_ontology:measurement_basis(vers_tr_t6, observed).
narrative_ontology:measurement(vers_tr_t9, versailles_reparations_clauses__repudiation_reading, theater_ratio, 9, 0.44).
narrative_ontology:measurement_basis(vers_tr_t9, observed).
narrative_ontology:measurement(vers_tr_t12, versailles_reparations_clauses__repudiation_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement_basis(vers_tr_t12, observed).
narrative_ontology:measurement(vers_tr_t15, versailles_reparations_clauses__repudiation_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement_basis(vers_tr_t15, observed).
narrative_ontology:measurement(vers_tr_t18, versailles_reparations_clauses__repudiation_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement_basis(vers_tr_t18, observed).
narrative_ontology:measurement(vers_tr_t20, versailles_reparations_clauses__repudiation_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(vers_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(vers_be_t0, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(vers_be_t0, observed).
narrative_ontology:measurement(vers_be_t3, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement_basis(vers_be_t3, observed).
narrative_ontology:measurement(vers_be_t6, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement_basis(vers_be_t6, observed).
narrative_ontology:measurement(vers_be_t9, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 9, 0.38).
narrative_ontology:measurement_basis(vers_be_t9, observed).
narrative_ontology:measurement(vers_be_t12, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(vers_be_t12, observed).
narrative_ontology:measurement(vers_be_t15, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement_basis(vers_be_t15, observed).
narrative_ontology:measurement(vers_be_t18, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 18, 0.82).
narrative_ontology:measurement_basis(vers_be_t18, observed).
narrative_ontology:measurement(vers_be_t20, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 20, 0.84).
narrative_ontology:measurement_basis(vers_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t0, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(vers_su_t0, observed).
narrative_ontology:measurement(vers_su_t3, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 3, 0.45).
narrative_ontology:measurement_basis(vers_su_t3, observed).
narrative_ontology:measurement(vers_su_t6, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 6, 0.72).
narrative_ontology:measurement_basis(vers_su_t6, observed).
narrative_ontology:measurement(vers_su_t9, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 9, 0.55).
narrative_ontology:measurement_basis(vers_su_t9, observed).
narrative_ontology:measurement(vers_su_t12, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(vers_su_t12, observed).
narrative_ontology:measurement(vers_su_t15, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(vers_su_t15, observed).
narrative_ontology:measurement(vers_su_t18, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 18, 0.66).
narrative_ontology:measurement_basis(vers_su_t18, observed).
narrative_ontology:measurement(vers_su_t20, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(vers_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Versailles reparations' conflates three structurally distinct obligation regimes instantiated from one fixed text. The punitive reading (upstream — the treaty's own self-understanding) and the limited reading (operationalized in the Dawes/Young machinery) are linked to this downstream repudiation story, which fed on the others' enforcement failures. Each member carries its own epsilon, victim set, and claimed type; edges run punitive -> limited -> repudiation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
