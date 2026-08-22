% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
 *   human_readable: WTO Dispute Settlement Binding Interpretive Authority over TRIPS
 *   domain: international_trade_law/public_health/intellectual_property
 *
 * SUMMARY:
 *   Since 1995 the WTO dispute settlement system has held binding
 *   interpretive authority over the TRIPS Agreement: panel and appellate
 *   reports, adopted by reverse consensus and backed by authorization to
 *   suspend trade concessions, fix what the treaty's obligations mean for all
 *   members. This story instantiates the
 *   dispute_settlement_interpretive_authority reading of the
 *   trips_agreement_interpretive_kernel — the meta-level claim that TRIPS
 *   meaning is set by adjudication rather than by member self-declaration.
 *   Per the epsilon-invariance principle, the kernel label decomposes into
 *   three structurally distinct constraints: this one (who fixes meaning),
 *   and two substantive siblings (what the text mandates: strong exclusivity
 *   versus public-health flexibility). Each gets its own epsilon,
 *   beneficiaries, and victims; they are linked through
 *   network.affects_constraints. The epsilon referent here is the standing
 *   arrangement itself — binding adjudicative interpretation enforced through
 *   retaliation — assessed as it has actually operated, including its
 *   post-2019 degradation, not the restored system any party advocates. The
 *   arrangement carries a genuine coordination function (a consensual venue
 *   replacing unilateral retaliation, shelter for weaker members) AND
 *   asymmetric extraction (precedent stock that narrowed developing members'
 *   policy space, retaliation capacity available mainly to large economies),
 *   which is why the claimed type is tangled_rope. The temporal series
 *   records three phases: construction (1995-2008, enforcement machinery
 *   built up, extraction rising with precedent accumulation), consolidation
 *   (2008-2017, peak extraction), and decay (2019-present, appellate
 *   paralysis, rising theater, falling enforcement capacity).
 *
 * KEY AGENTS:
 *   - wto_dispute_settlement_body: agenda-setter (institutional/constrained) — adopts reports by reverse consensus, authorizes retaliation, acts only through member-initiated process
 *   - developed_ip_exporting_states: primary beneficiary with dual cost-bearing (powerful/arbitrage) — collected precedent rents, occasionally absorbs adverse rulings, maintains bilateral fallback channels
 *   - originator_pharmaceutical_industry: concentrated beneficiary (organized/mobile) — gains market exclusivity from narrow flexibility readings
 *   - developing_country_members: primary payer bloc (organized/constrained) — lost policy space to precedent, retains coalition capacity proven at Doha, depends on the venue for shelter
 *   - generic_pharmaceutical_manufacturers: payer (moderate/constrained) — operating space shrinks with strict readings
 *   - medicine_access_patient_populations: ultimate payer, no voice (powerless/trapped) — bears price and access consequences
 *   - wto_secretariat_legal_division: institutional beneficiary (institutional/identity_locked) — careers and function fused with system vitality
 *   - international_trade_law_profession: professional beneficiary (organized/mobile) — practice market scales with caseload and doctrinal complexity
 *   - health_access_civil_society: excluded voice (organized/constrained) — no standing, influence confined to adjacent political venues
 *   - trade_policy_analysts: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.48).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.38).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.48).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO Dispute Settlement Binding Interpretive Authority over TRIPS").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "international_trade_law/public_health/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '023adfdc-4e9a-4c3d-95c2-6faabeea788b').
narrative_ontology:cs_kernel_codification('023adfdc-4e9a-4c3d-95c2-6faabeea788b', fixed_text).
narrative_ontology:cs_authority_grounding('023adfdc-4e9a-4c3d-95c2-6faabeea788b', lineage).
narrative_ontology:cs_interpretation_layer_present('023adfdc-4e9a-4c3d-95c2-6faabeea788b').
narrative_ontology:cs_reading_relation('023adfdc-4e9a-4c3d-95c2-6faabeea788b', trips_agreement_interpretive_kernel__strong_exclusivity_reading, influences).
narrative_ontology:cs_reading_relation('023adfdc-4e9a-4c3d-95c2-6faabeea788b', trips_agreement_interpretive_kernel__public_health_flexibility_reading, influences).
narrative_ontology:cs_axiom('023adfdc-4e9a-4c3d-95c2-6faabeea788b', foundational, treaty_meaning_is_adjudicated_not_self_declared).
narrative_ontology:cs_axiom_status(treaty_meaning_is_adjudicated_not_self_declared, holdable).
narrative_ontology:cs_axiom_grounding('023adfdc-4e9a-4c3d-95c2-6faabeea788b', treaty_meaning_is_adjudicated_not_self_declared, conventional).
narrative_ontology:cs_axiom('023adfdc-4e9a-4c3d-95c2-6faabeea788b', secondary, precedent_locks_substantive_readings_across_members).
narrative_ontology:cs_axiom_status(precedent_locks_substantive_readings_across_members, holdable).
narrative_ontology:cs_axiom_grounding('023adfdc-4e9a-4c3d-95c2-6faabeea788b', precedent_locks_substantive_readings_across_members, empirically_contingent).
narrative_ontology:cs_reference_frame('023adfdc-4e9a-4c3d-95c2-6faabeea788b', consensus_based_multilateral_adjudication).
narrative_ontology:cs_drift_state('023adfdc-4e9a-4c3d-95c2-6faabeea788b', post_appellate_body_paralysis_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('023adfdc-4e9a-4c3d-95c2-6faabeea788b', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_ip_exporting_states).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, originator_pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_secretariat_legal_division).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, international_trade_law_profession).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_members).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_pharmaceutical_manufacturers).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, medicine_access_patient_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_ip_exporting_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The member-governed organ that adopts panel and appellate reports by reverse consensus and authorizes suspension of trade concessions against non-complying members. It acts only when members initiate disputes, cannot amend findings, and its continuing authority rests entirely on members' willingness to keep bringing cases and accepting outcomes; if members stopped using it, it would have nothing left to administer.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, constrained, global).

% Large economies whose exports concentrate in IP-intensive goods, media, and services. Early jurisprudence extended protection beyond the negotiated minimum text, and their industries gained measurable market exclusivity from that precedent. The same states occasionally lose cases, absorb adverse findings, fund the institution's budget, and accept limits on unilateral trade action; several maintain parallel bilateral channels — preferential trade agreements and domestic enforcement statutes — that they activate when the multilateral route disappoints.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_ip_exporting_states, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_ip_exporting_states, payer).

% Research-based drug companies holding large patent portfolios. They press for strict enforcement of patent terms and against broad compulsory-licensing interpretations; adopted rulings that narrow flexibility space translate directly into longer effective market exclusivity across member markets. They can relocate research spending and portfolio decisions toward jurisdictions offering stronger protection.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, originator_pharmaceutical_industry, beneficiary,
    organized, biographical, mobile, global).

% Members that negotiated the agreement expecting policy space for industrial development and public health. Adopted jurisprudence has narrowed usable readings of compulsory-licensing and transition provisions in several episodes, raising the domestic cost of invoking lawful flexibilities. As a bloc they forced the 2001 ministerial declaration reaffirming flexibility rights; individually they lack retaliation capacity against major markets and depend on the rules-based venue to check larger powers. Leaving the system would forfeit market-access guarantees everywhere at once.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_members, payer,
    organized, generational, constrained, global).

% Producers of off-patent and licensed medicines operating in the space left by patent terms and exceptions. Stricter interpretive readings shrink that space through litigation exposure, border measures, and regulatory caution among importing governments. Their commercial geography concentrates in markets where enforcement is strictest, limiting relocation options.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_pharmaceutical_manufacturers, payer,
    moderate, biographical, constrained, global).

% People in low- and middle-income countries dependent on affordable medicines. Prices and availability shift with how broadly or narrowly licensing flexibilities are read; they have no voice in proceedings and no alternative supplier when prices rise. Their stake reaches them only through government decisions and advocacy intermediaries.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, medicine_access_patient_populations, payer,
    powerless, immediate, trapped, global).

% Career staff who service panels, maintain the jurisprudence database, and train panelists. Institutional purpose and staff careers are bound up with the multilateral adjudication system's vitality; a permanent shift to ad hoc bilateral settlement would leave the division administering a shrinking caseload.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_secretariat_legal_division, beneficiary,
    institutional, generational, identity_locked, global).

% Panelists, counsel, arbitrators, and academic specialists whose practice and publication markets scale with dispute volume and doctrinal complexity. Demand for their services grew with the system's caseload; a collapse into power-based settlement would shrink the interpretive work they live from, though bilateral arbitration offers partial replacement.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, international_trade_law_profession, beneficiary,
    organized, biographical, mobile, global).

% Advocacy organizations campaigning for medicine affordability. They hold no standing before panels; requests to submit amicus materials are granted rarely and at panels' discretion. Their influence operates in ministerial conferences and national politics — venues adjacent to, not inside, the place where treaty meanings are fixed.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, health_access_civil_society, excluded,
    organized, generational, constrained, global).

% Academic and think-tank specialists tracking jurisprudence, compliance patterns, and institutional health. They publish assessments of rulings and reform proposals, take no material position in disputes, and observe the system from outside its decision processes.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trade_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_ip_exporting_states).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single consensual venue where more than 160 members resolve conflicting readings of TRIPS obligations through codified procedure instead of unilateral retaliation; adopted reports create a common reference meaning for treaty terms across jurisdictions, and the retaliation authorization gives the common meaning consequence.
% TRANSFER_FUNCTION: Moves interpretive control over TRIPS meaning from individual member capitals to adjudicative bodies; moves enforcement leverage toward members able to credibly suspend concessions; and, through the accumulated precedent stock, has moved regulatory policy space from developing-country capitals toward IP-exporting commercial interests.
% ABSENT_VOICES: Patient populations, generic producers, and health-access advocacy organizations have no standing before panels; amicus acceptance is discretionary and rare. Their objections enter only through political channels such as the 2001 ministerial — venues outside the adjudicative room where meanings are actually fixed. The unanimity of the adjudicative record partly reflects that these seats were never in it.
% DISAPPEARANCE_RATIONALE: If binding interpretive authority vanished overnight, TRIPS meaning would fragment: members would fall back on unilateral determination of obligations and bilateral leverage, large-market regulators would become de facto interpreters for everyone selling into their markets, pending disputes would hang unresolved, and the pharmaceutical regime would reorganize around preferential-agreement chapters and domestic enforcement statutes. Dozens of members that currently obtain shelter from the venue would lose it simultaneously.
% FOUNDING_PROBLEM: Before 1995, trade frictions over intellectual property were settled by unilateral threat — the largest market applying its domestic law extraterritorially against smaller trading partners. The agreement's negotiators built binding adjudication so that IP commitments had enforcement behind them and weaker members had institutional shelter from unilateral pressure.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: developing-country members — the group with the least to gain from exporter-friendly precedent — consistently defend the dispute system's existence in DSU review negotiations and refused abolition proposals even while blocking appointments; the pre-1995 record of unilateral measures harming third-party exporters is documented in contemporaneous government protests and trade literature; and the continued filing of disputes by members on all sides attests the underlying problem has not gone away.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.48 at interval end) is moderate-to-substantial: the precedent stock measurably narrowed usable compulsory-licensing and transition readings in identifiable episodes, transferring regulatory space toward IP-exporting interests, but the same venue delivered real shelter value to the paying members, capping net extraction below snare levels. Suppression (0.38) is the current enforcement capacity, deliberately traced by the suppression_requirement series: it rose through 2013 as reverse-consensus adoption and retaliation authorization matured, then fell after the 2019 appellate paralysis removed the binding apex — the story's narrative specifically tracks enforcement-capacity change, which is why the series is authored. Theater ratio (0.55) is the clearest current signal: members still file disputes, cite appellate jurisprudence as controlling, and appeal into a void, maintaining the performance of binding adjudication atop a partially hollowed enforcement core. Accessibility collapse (0.50): alternatives exist (bilateral settlement, preferential-agreement chapters, unilateral statutes, the interim appeal arrangement) but are strictly inferior for small members, so understanding the arrangement collapses alternatives only partially. Resistance (0.62) is high and concrete: the appointment blockade is open resistance to binding authority itself, members have refused reform consensus for years, and the 2001 ministerial fight was successful resistance from the payer bloc. All three series run on one shared seven-point grid (1995, 2001, 2007, 2013, 2019, 2022, 2025) so no metric row borrows another's end-state values; every point is historically observed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat the arrangement is a legitimate, member-built institution performing exactly its designed function; from the developing-member payer seat the same structure reads as a venue that took real grievances in and returned narrowed policy space, while still being preferable to unilateral great-power settlement; from the IP-exporter beneficiary seat it is a vindication machine that converted negotiated text into enforceable market exclusivity; from the excluded advocacy seat it is an illegitimate closed room where the people who bear the health consequences have no standing. The engine computes these divergent classifications from the structural data — power, exit, and declared position — and the divergence between the agenda-setter's computed type and the trapped payer's computed type is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the four collecting seats: the pharmaceutical industry (organized, mobile) and the law profession (organized, mobile) sit well toward the beneficiary end; the secretariat (institutional, identity_locked) collects status and function from the arrangement's persistence. Victim declarations drive high directionality for the three paying seats: developing members (organized but constrained — leaving forfeits all market access at once), generic producers (moderate, constrained), and patient populations (powerless, trapped — the nearest-to-full-target seat, bearing costs with zero voice and zero exit). One override is authored: the developed IP-exporting seat would derive near the full-beneficiary end from its beneficiary declaration plus arbitrage-grade exit, but its actual position includes recurring adverse rulings, budget contributions, and accepted limits on unilateral action, so its directionality is corrected upward to 0.25 — mostly beneficiary, materially cost-bearing. The override is safe to key on the powerful atom because no other stakeholder holds that power level. Suppression is authored as a raw structural property and is not scaled; only extraction is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — replacing unilateral power-based settlement of trade-meaning disputes with consensual adjudication — is still live, and the parties' behavior proves it: members keep filing disputes, the payer bloc defends the system's existence in reviews, and no member proposes abolishing it even while blocking parts of it. Because founding_problem_status is live and disappearance_verdict is world_rearranges, the mismatch consumer finds no capture/zombie flag — correctly, since the arrangement has not outlived its mandate. But the temporal series carries the classic pre-degradation signature: theater climbing past 0.5 while enforcement capacity falls. If appellate restoration fails permanently, the arrangement drifts toward inertial maintenance — adjudication performed for its legitimating display rather than its binding effect — and a future re-authoring should expect the computed type to move. The tangled_rope claim prevents mislabeling in both directions: reading the arrangement as pure coordination ignores the documented asymmetric precedent capture and the retaliation-capacity asymmetry that lets only large economies credibly enforce; reading it as pure extraction ignores the shelter function that kept dozens of weak members inside the system voluntarily and the coalition victory at Doha that shows the paying bloc retains real corrective power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position_omega,
    'This constraint is the dispute_settlement_interpretive_authority reading of the trips_agreement_interpretive_kernel; how would instantiating a sibling reading instead change the structural facts recorded here?',
    'Comparative read across the three instantiated stories of the kernel: diff the beneficiary/victim sets, epsilon values, and stakeholder exit profiles between this story and trips_agreement_interpretive_kernel__strong_exclusivity_reading and trips_agreement_interpretive_kernel__public_health_flexibility_reading.',
    'Under the public_health_flexibility_reading the victim set centers on patient populations and generic producers and epsilon is assessed on flexibility-chilling effects; under the strong_exclusivity_reading the same arrangements are reframed as the price of innovation incentives and the victim set shrinks toward unauthorized copiers. The disagreement is located in who counts as bearing costs, not in whether adjudication binds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position_omega, conceptual, 'Committer structure: this story is one of three readings of the TRIPS interpretive kernel; sibling instantiation would shift the victim set and epsilon referent.').

omega_variable(
    precedent_direction_ambiguity,
    'Does accumulated adopted jurisprudence lock in exclusivity-favoring readings of TRIPS net-net, or flexibility-preserving readings?',
    'Systematic coding of all adopted panel and appellate reports touching TRIPS provisions, scored for directional effect on compulsory licensing, parallel importation, transition periods, and enforcement obligations.',
    'A net-exclusivity direction establishes the interpretive authority as a transmission belt for IP-exporter interests (raising effective extraction on developing members); a net-flexibility direction supports the neutral-arbiter framing and lowers measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_direction_ambiguity, empirical, 'Directional bias of the precedent stock produced by binding interpretation.').

omega_variable(
    appellate_restoration_trajectory,
    'Will binding multilateral adjudication be restored (appointment consensus), permanently replaced by the multi-party interim appeal arrangement and bilateral channels, or continue decaying?',
    'Track ministerial conference outcomes, appointment negotiation status, interim-appeal-arrangement membership growth, and the share of disputes ending in appealed-into-the-void reports.',
    'Restoration reverses the theater-ratio climb and consolidates the tangled-rope profile; permanent substitution converts the arrangement toward inertial maintenance with performative adjudication; continued decay dates a type transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_restoration_trajectory, empirical, 'Whether the enforcement-decay trajectory visible after 2019 stabilizes, reverses, or completes.').

omega_variable(
    retaliation_incidence_distribution,
    'Who actually bears the costs when retaliation is authorized — the non-complying government, its exporting firms, third-country traders caught in suspended concession flows, or end consumers?',
    'Econometric study of authorized retaliation episodes (e.g., the Antigua-US gaming dispute, Brazil-US cotton case) tracing price and volume effects across affected markets.',
    'If third parties and consumers bear most retaliation costs, the victim set broadens beyond the currently named seats and the enforcement mechanism''s own extraction profile rises independent of the interpretive function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_incidence_distribution, empirical, 'Incidence of retaliation costs across the named and unnamed affected populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 1995, 0.15).
narrative_ontology:measurement_basis(trip_tr_t1995, observed).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2001, 0.18).
narrative_ontology:measurement_basis(trip_tr_t2001, observed).
narrative_ontology:measurement(trip_tr_t2007, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2007, 0.22).
narrative_ontology:measurement_basis(trip_tr_t2007, observed).
narrative_ontology:measurement(trip_tr_t2013, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2013, 0.28).
narrative_ontology:measurement_basis(trip_tr_t2013, observed).
narrative_ontology:measurement(trip_tr_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2019, 0.38).
narrative_ontology:measurement_basis(trip_tr_t2019, observed).
narrative_ontology:measurement(trip_tr_t2022, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2022, 0.48).
narrative_ontology:measurement_basis(trip_tr_t2022, observed).
narrative_ontology:measurement(trip_tr_t2025, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2025, 0.55).
narrative_ontology:measurement_basis(trip_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement_basis(trip_be_t1995, observed).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2001, 0.46).
narrative_ontology:measurement_basis(trip_be_t2001, observed).
narrative_ontology:measurement(trip_be_t2007, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2007, 0.54).
narrative_ontology:measurement_basis(trip_be_t2007, observed).
narrative_ontology:measurement(trip_be_t2013, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2013, 0.58).
narrative_ontology:measurement_basis(trip_be_t2013, observed).
narrative_ontology:measurement(trip_be_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2019, 0.56).
narrative_ontology:measurement_basis(trip_be_t2019, observed).
narrative_ontology:measurement(trip_be_t2022, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2022, 0.52).
narrative_ontology:measurement_basis(trip_be_t2022, observed).
narrative_ontology:measurement(trip_be_t2025, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2025, 0.48).
narrative_ontology:measurement_basis(trip_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement_basis(trip_su_t1995, observed).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2001, 0.45).
narrative_ontology:measurement_basis(trip_su_t2001, observed).
narrative_ontology:measurement(trip_su_t2007, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2007, 0.52).
narrative_ontology:measurement_basis(trip_su_t2007, observed).
narrative_ontology:measurement(trip_su_t2013, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2013, 0.55).
narrative_ontology:measurement_basis(trip_su_t2013, observed).
narrative_ontology:measurement(trip_su_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2019, 0.5).
narrative_ontology:measurement_basis(trip_su_t2019, observed).
narrative_ontology:measurement(trip_su_t2022, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2022, 0.42).
narrative_ontology:measurement_basis(trip_su_t2022, observed).
narrative_ontology:measurement(trip_su_t2025, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement_basis(trip_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'TRIPS interpretive authority' conflates a meta-level constraint (who fixes the text's meaning: this story) with two substantive constraints (what the text mandates: the strong-exclusivity and public-health-flexibility siblings). The decomposition follows the epsilon-invariance principle: measuring the arrangement by its precedent output yields different epsilon than measuring it by its enforcement machinery or by its flexibility administration, so they are separate stories. This story is upstream of both siblings — adopted rulings are the transmission mechanism through which whichever substantive reading prevails becomes operative — hence the affects_constraints edges point at both, and each sibling story should carry a reciprocal edge documenting that its viability is conditioned by this reading's operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
