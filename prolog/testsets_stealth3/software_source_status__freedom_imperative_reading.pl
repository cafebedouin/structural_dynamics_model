% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__freedom_imperative_reading, []).

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
 *   constraint_id: software_source_status__freedom_imperative_reading
 *   human_readable: Proprietary Software Licensing Regime (Freedom-Imperative Reading)
 *   domain: economic/political/technological
 *
 * SUMMARY:
 *   A single persisting commitment — what the existence of software source
 *   code obligates between maker and runner — is read several ways; this file
 *   instantiates the freedom-imperative reading, under which source access,
 *   modification, and redistribution are inalienable user rights and
 *   proprietary distribution is a categorical injustice rather than a
 *   regrettable trade-off. The standing arrangement this story evaluates is
 *   the proprietary licensing regime itself: binary-only distribution under
 *   copyright-enforced end-user licenses, with modification and sharing
 *   legally foreclosed. Assessed through this reading's own lights, that
 *   arrangement is heavily extractive: nearly every act of computing now
 *   occurs atop code the runner may not inspect, repair, or adapt, and the
 *   arrangement persists through layered enforcement — contractual,
 *   statutory, and increasingly architectural. Claimed type and metrics are
 *   authored independently: the regime retains a real
 *   funding-and-distribution coordination function even under this reading's
 *   indictment, and the metrics deliberately sit near the snare boundary —
 *   the engine measures the gap; this story does not reconcile it. KEY AGENTS
 *   (by structural relationship): - proprietary_software_vendors: primary
 *   agenda setter and principal collector (institutional/arbitrage) — drafts
 *   the licenses, withholds the source, runs the enforcement machinery -
 *   closed_ecosystem_gatekeepers: secondary collector
 *   (institutional/arbitrage) — channel tolls premised on opaque-binary
 *   distribution - proprietary_software_end_users: primary diffuse target
 *   population (moderate/constrained) - institutional_procurement_buyers:
 *   leveraged target seat (powerful/mobile) — could demand source terms and
 *   historically renew without them - interoperability_developers: specialist
 *   target class (moderate/trapped) — legally exposed by the enforcement line
 *   itself - future_users_of_archived_software: absent voice
 *   (powerless/trapped) — inheritors of a record dying with its activation
 *   servers - free_software_movement: organized counter-party
 *   (organized/constrained) — maintains the freedoms the arrangement
 *   withholds
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: agenda setter and principal collector (institutional/arbitrage) — drafts license terms, withholds source, administers enforcement
 *   - closed_ecosystem_gatekeepers: secondary collector (institutional/arbitrage) — channel commissions dependent on closed distribution
 *   - proprietary_software_end_users: diffuse target population (moderate/constrained) — accepts un-negotiable terms, absorbs telemetry and forced upgrade cycles
 *   - institutional_procurement_buyers: leveraged target seat (powerful/mobile) — volume leverage and migration capability dampen their effective burden relative to trapped users
 *   - interoperability_developers: specialist target class (moderate/trapped) — work legally foreclosed mid-project
 *   - future_users_of_archived_software: absent voice (powerless/trapped) — no seat, no representation, irreversible losses
 *   - free_software_movement: organized counter-party (organized/constrained) — copyleft counter-constraints, litigation defense, advocacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.87).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.82).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Proprietary Software Licensing Regime (Freedom-Imperative Reading)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "economic/political/technological").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, '0fc78849-bea5-4afa-8554-db95676469b2').
narrative_ontology:cs_kernel_codification('0fc78849-bea5-4afa-8554-db95676469b2', fixed_text).
narrative_ontology:cs_authority_grounding('0fc78849-bea5-4afa-8554-db95676469b2', lineage).
narrative_ontology:cs_interpretation_layer_present('0fc78849-bea5-4afa-8554-db95676469b2').
narrative_ontology:cs_reading_relation('0fc78849-bea5-4afa-8554-db95676469b2', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('0fc78849-bea5-4afa-8554-db95676469b2', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('0fc78849-bea5-4afa-8554-db95676469b2', software_source_status__utilitarian_hybrid_reading, forecloses).
narrative_ontology:cs_axiom('0fc78849-bea5-4afa-8554-db95676469b2', foundational, software_freedom_categorically_required).
narrative_ontology:cs_axiom_status(software_freedom_categorically_required, holdable).
narrative_ontology:cs_axiom_grounding('0fc78849-bea5-4afa-8554-db95676469b2', software_freedom_categorically_required, deontological).
narrative_ontology:cs_axiom('0fc78849-bea5-4afa-8554-db95676469b2', foundational, freedom_loss_admits_no_efficiency_compensation).
narrative_ontology:cs_axiom_status(freedom_loss_admits_no_efficiency_compensation, holdable).
narrative_ontology:cs_axiom_grounding('0fc78849-bea5-4afa-8554-db95676469b2', freedom_loss_admits_no_efficiency_compensation, deontological).
narrative_ontology:cs_reference_frame('0fc78849-bea5-4afa-8554-db95676469b2', four_freedoms_inalienable_baseline).
narrative_ontology:cs_drift_state('0fc78849-bea5-4afa-8554-db95676469b2', contemporary_service_substitution_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0fc78849-bea5-4afa-8554-db95676469b2', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, closed_ecosystem_gatekeepers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, proprietary_software_end_users).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, institutional_procurement_buyers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, interoperability_developers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, future_users_of_archived_software).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish software as binaries under end-user licenses they draft: source code is withheld, modification and redistribution are contractually and statutorily foreclosed, and compliance is policed through audits, activation systems, and litigation. Revenue arrives as license fees, subscriptions, and per-seat charges. Because the firm's durable assets are the code and the customer relationships rather than exclusivity itself, its realistic exits are model pivots — open-core releases, dual licensing, conversion to hosted services — any of which preserves the firm while changing what it withholds.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate distribution channels — app stores, console platforms, enterprise suite bundles — whose toll position presupposes software arriving as opaque binaries. Commission on in-channel transactions flows to them; they drafted none of the underlying licenses but defend the source-status quo wherever opening it would shrink their cut, and they retain credible pivots to alternative channel strategies.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, closed_ecosystem_gatekeepers, beneficiary,
    institutional, generational, arbitrage, global).

% Run software they cannot inspect, repair, adapt, or legally share; accept terms written entirely by the vendor; and absorb telemetry, forced-upgrade cycles, and the shutdown of products they paid to own outright. Moving to free alternatives carries compatibility and retraining costs that land hardest on non-specialists, so switching happens rarely and at purchase time, against a pre-set menu.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_end_users, payer,
    moderate, biographical, constrained, global).

% Deploy proprietary systems for records, infrastructure, and public services at scale. They hold real leverage — volume commitments, escrow clauses, procurement statutes — and occasionally win concessions such as government-use rights, yet renewal on vendor-posted terms remains the default because migration projects run for years and fail conspicuously, making incumbency the safe career choice for the officials involved.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, institutional_procurement_buyers, payer,
    powerful, biographical, mobile, global).

% Build connectors, compatible implementations, and preservation tools around proprietary formats and protocols. Interface documentation is guarded, decompilation attracts penalties, and takedown notices arrive mid-project; the work depends on studying artifacts the surrounding legal environment declares off-limits, and no comparable legal channel exists for doing it openly.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, interoperability_developers, payer,
    moderate, biographical, trapped, global).

% Will inherit today's scientific and cultural record, much of which executes on proprietary stacks with lapsed licenses, dead activation servers, and undocumented formats. They cannot buy, negotiate, or object now; whatever is not preserved in runnable form is unreachable to them, and no seat in any current licensing conversation represents them.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, future_users_of_archived_software, excluded,
    powerless, civilizational, trapped, global).

% Maintains the counter-tradition: copyleft licenses granting the freedoms the dominant licenses withhold, along with distributions, archives, and long-running advocacy and litigation-defense efforts. Its influence travels through adoption and argument rather than administration; it holds no seat where proprietary terms are drafted and spends durable resources contesting them.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_movement, observer,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_source_status__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Funds and distributes large-scale professional software: license and subscription revenue pays development teams; centralized release, update, and support pipelines deliver maintained binaries to populations who cannot compile or maintain code themselves. Whatever else it does, the arrangement solves, at scale, the problem of producing and sustaining professional-grade software for non-programmers.
% TRANSFER_FUNCTION: Moves money (license fees, subscriptions, per-seat charges, channel commissions) from software users and institutions to vendors and gatekeepers; and moves control over computing — source access, repair, modification, redistribution rights — from everyone who runs the software to the vendor holding the copyright. The second transfer is the one this reading treats as constitutive of the arrangement's character.
% ABSENT_VOICES: Ordinary users never sat across the table: terms are drafted by vendor counsel and accepted post hoc via clickwrap. Future users of archived software are absent entirely — no one represents dead-server products in any negotiation. Interoperability developers typically learn the boundaries via cease-and-desist letters. Advocacy organizations speak for some affected users but were never seated where the terms were written; the unanimity of the arrangement's acceptance was manufactured in rooms those voices could not enter.
% DISAPPEARANCE_RATIONALE: Overnight removal of proprietary licensing forces wholesale rearrangement: funded development contracts collapse until subscription, service, or public-funding models scale up; enterprises lose support lines for critical systems; distribution shifts onto free-software channels that lack ready capacity in some domains (desktop polish, specialized verticals). The rearrangement is exactly why this reading's proponents expect disruption alongside the justice, and why opponents predict breakdown — both sides agree the world would not stay still.
% FOUNDING_PROBLEM: How to fund professional software production and protect development investment when copies are costlessly replicable — answered by asserting exclusive control over copying, modification, and distribution via copyright applied to binaries.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the durable existence of large funded free-software projects (operating-system kernels, web servers, databases) contradicts the necessity premise that exclusive binary rights are indispensable to funding; academic studies of open-source sustainability and published accounts of government free-stack migrations corroborate that funding does not require withholding source. No attesting party outside the vendor and gatekeeper seats asserts that binary-only exclusivity is indispensable; the vendor seats themselves attest it is.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__freedom_imperative_reading, 0.87, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (epsilon 0.87 at interval end) prices the standing arrangement as this reading assesses it: the transfer of computing control reaches essentially every proprietary deployment, and the service-delivery turn has extended the reach from purchased copies to rented runtime access. Suppression (0.82) is structural and unscaled in authorship: copyright statute, anti-circumvention law, contractual audit rights, and hosted architectures that dissolve the artifact to be modified. Theater (0.33) captures consent rituals, security-through-obscurity claims, and residual ownership language layered over rental reality — real production activity dominates, so theater stays minority-share. Accessibility collapse is moderate (0.42): free alternatives persist and function, but workflow compatibility keeps whole sectors effectively captive. Resistance is high (0.72) and organized — notably, copyleft licensing is itself a counter-constraint engineered to turn the arrangement's own copyright machinery against it. All three temporal series share one grid (1983, 1991, 1998, 2007, 2015, 2026). The suppression_requirement series is authored because this interval specifically tracks an enforcement ratchet — pre-DMCA litigation posture, through DMCA-era anti-circumvention and DRM normalization, to audit-standard enterprise licensing and architectural enforcement — not merely shifting extraction; a flat scalar would erase the ratchet this story exists to trace.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor seat the regime is coordination it built and defends — payroll, release trains, support desks; from the user seat the same structure is enforced opacity with a bill attached, and the categorical reading refuses to net the two. Same-power lateral divergence: two enterprises of identical size and standing meet different effective constraints — one negotiates source escrow because it can credibly threaten migration; the other renews on posted terms because a visible failed migration would end careers, so identical balance sheets buy different exit. Inter-institutionally, gatekeepers and vendors both profit yet diverge on remedies: mandated source disclosure helps vendors' competitors while shrinking gatekeepers' toll base less immediately. The engine computes per-seat classifications from power, exit, and role declarations; the authored claim adjudicates none of this.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendor and gatekeeper seats sit at the beneficiary end: the arrangement subsidizes them with fee flows and with control transferred from users. Trapped seats — end users, interoperability developers, future archived users — sit at the full-target end: money, autonomy, and access to their own inheritance flow away from them, and exit is blocked by compatibility lock-in, legal exposure, and nonexistence respectively. Procurement buyers occupy an intermediate band: they pay the same transfers but hold mobility and volume leverage, damping their effective burden relative to trapped seats at equal nominal payment. The derivation chain covers every seat from the beneficiary/victim declarations plus exit options; no directionality overrides are authored. Suppression is authored raw; only extractiveness rides directionality and spatial scope in the engine's arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — funding professional software when duplication is free — has not died; it is contested, which distinguishes this story from a zombie mandate, and mandatrophy_resolved is therefore left undeclared. Exclusive licensing still demonstrably funds software, so declaring the mandate outlived would be false. The classification work runs in the opposite direction: holding claimed_type=tangled_rope keeps the genuine funding coordination visible and prevents flattening the vendor seat into pure predation (which would mispredict how vendors respond to remedies — they pivot models rather than collapse), while the reading's categorical axioms are routed through epsilon, the omega set, and the cs_structure block rather than by inflating epsilon past descriptive honesty. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: no dead-mandate flag fires, correctly — the dispute concerns the legitimacy of the solution, not the obsolescence of the problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of kernel software_source_status (freedom_imperative_reading). Do the freedom-imperative premises — inalienable source rights, categorically illegitimate licensing restrictions — correctly characterize the standing arrangement, or do the pragmatic-development, property-rights, or utilitarian-hybrid readings govern instead?',
    'Not data-decidable: resolution arrives through institutional adoption — default licenses, statutes, procurement doctrine, court rulings — shifting which reading governs practice, or through a reading collapsing on internal inconsistency. Sibling files carry the competing characterizations.',
    'Under the property-rights reading the victim set empties and the arrangement reads as legitimate exercise of creator entitlement; under the hybrid reading victims become contextual; under this reading the entire proprietary user population sits in the victim set and licensing restrictions are illegitimate as such.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the software-source kernel governs the classification').

omega_variable(
    categorical_vs_tradeable_freedom_weighting,
    'Is the freedom-loss component a categorically non-compensable violation of user autonomy (this reading) or a tradeable cost weighable against the funding coordination benefit (the hybrid and pragmatic readings)?',
    'Values adjudication — political, legal, philosophical; no dataset settles a categorical versus tradeable normative weighting. Signals to watch: whether remedy proposals consistently pair source mandates with funding guarantees (tradeable framing) or reject compensation framings outright (categorical framing).',
    'A tradeable framing nets effective extraction against coordination value and softens verdicts toward coordination-with-overhead; the categorical framing leaves the freedom-transfer dispositive regardless of coordination value, strengthening snare-side per-seat verdicts among trapped seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_vs_tradeable_freedom_weighting, preference, 'Whether freedom loss admits compensation by efficiency gains').

omega_variable(
    saas_referent_split_pressure,
    'Has the standing arrangement split into two structurally distinct constraints — the licensed-binary regime and the hosted-service access regime — such that this story''s epsilon no longer refers to one stable object?',
    'Track whether source-withholding and modification-denial remain the operative harm mechanisms as delivery shifts toward services that distribute no artifact at all; if the harm mechanism changes character, decompose per the epsilon-invariance principle and link the successor story through network.affects_constraints.',
    'Decomposition would re-date this story''s terminal measurements and spawn a service-access constraint whose victim set, enforcement profile, and epsilon differ from binary licensing; failing to decompose would date transitions against a mutated referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saas_referent_split_pressure, empirical, 'Whether service-based delivery breaks this story''s epsilon invariance and forces decomposition').

omega_variable(
    funding_without_withholding_separability,
    'Is the funding-and-support coordination function separable from source-withholding — could vendors finance materially equivalent development while publishing source?',
    'Natural experiments: sustained-revenue open-core and dual-licensing firms, publicly procured free-software deployments at scale, support-and-services revenue analyses, and longitudinal comparison of development output per revenue unit across closed and open funding structures.',
    'Demonstrated separability strengthens the snare-side reading — source-withholding becomes extraction riding on a funding function that does not require it; demonstrated inseparability attributes part of measured epsilon to the irreducible price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_without_withholding_separability, empirical, 'Whether the funding coordination function structurally requires the withholding mechanism').

omega_variable(
    closure_norm_internalization,
    'Is the measured suppression structural (statute, contract, architecture) or partially internalized (users treating closedness as the natural condition of software, unable to imagine inspection, repair, or sharing)?',
    'Post-transition trajectory analysis: track user populations after migration to free software (municipal, educational, enterprise cohorts) — if perceived incapacity to modify and verify persists after access is granted, the suppression is partly internalized rather than purely imposed.',
    'Internalized closure raises effective suppression above the structural measure and predicts the arrangement''s behavioral persistence even where enforcement lapses; purely structural suppression predicts rapid behavioral release once barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(closure_norm_internalization, empirical, 'Structural versus internalized suppression in the software-closure norm').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 1983, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(swsrc_fir_tr_t1983, software_source_status__freedom_imperative_reading, theater_ratio, 1983, 0.14).
narrative_ontology:measurement(swsrc_fir_tr_t1991, software_source_status__freedom_imperative_reading, theater_ratio, 1991, 0.17).
narrative_ontology:measurement(swsrc_fir_tr_t1998, software_source_status__freedom_imperative_reading, theater_ratio, 1998, 0.21).
narrative_ontology:measurement(swsrc_fir_tr_t2007, software_source_status__freedom_imperative_reading, theater_ratio, 2007, 0.26).
narrative_ontology:measurement(swsrc_fir_tr_t2015, software_source_status__freedom_imperative_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(swsrc_fir_tr_t2026, software_source_status__freedom_imperative_reading, theater_ratio, 2026, 0.33).

% Extraction over time
narrative_ontology:measurement(swsrc_fir_be_t1983, software_source_status__freedom_imperative_reading, base_extractiveness, 1983, 0.6).
narrative_ontology:measurement(swsrc_fir_be_t1991, software_source_status__freedom_imperative_reading, base_extractiveness, 1991, 0.64).
narrative_ontology:measurement(swsrc_fir_be_t1998, software_source_status__freedom_imperative_reading, base_extractiveness, 1998, 0.7).
narrative_ontology:measurement(swsrc_fir_be_t2007, software_source_status__freedom_imperative_reading, base_extractiveness, 2007, 0.76).
narrative_ontology:measurement(swsrc_fir_be_t2015, software_source_status__freedom_imperative_reading, base_extractiveness, 2015, 0.82).
narrative_ontology:measurement(swsrc_fir_be_t2026, software_source_status__freedom_imperative_reading, base_extractiveness, 2026, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(swsrc_fir_su_t1983, software_source_status__freedom_imperative_reading, suppression_requirement, 1983, 0.45).
narrative_ontology:measurement(swsrc_fir_su_t1991, software_source_status__freedom_imperative_reading, suppression_requirement, 1991, 0.49).
narrative_ontology:measurement(swsrc_fir_su_t1998, software_source_status__freedom_imperative_reading, suppression_requirement, 1998, 0.58).
narrative_ontology:measurement(swsrc_fir_su_t2007, software_source_status__freedom_imperative_reading, suppression_requirement, 2007, 0.67).
narrative_ontology:measurement(swsrc_fir_su_t2015, software_source_status__freedom_imperative_reading, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement(swsrc_fir_su_t2026, software_source_status__freedom_imperative_reading, suppression_requirement, 2026, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial debate over whether software should be open decomposes, per the epsilon-invariance principle, into four structurally distinct readings of the software_source_status kernel, each with its own epsilon, victim set, and coordination characterization. This file is the freedom-imperative member; the property-rights member carries an empty victim set, the pragmatic member treats freedom as instrumental, and the hybrid member contextualizes victims. The upstream members with higher empirical consensus (pragmatic methodology evidence) are frequently cited as support for downstream normative claims, which is why the family links run in both directions rather than as a strict chain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
