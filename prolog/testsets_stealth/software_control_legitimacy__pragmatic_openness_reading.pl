% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__pragmatic_openness_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Pragmatic Openness Norm: Software Control as a Methodology Choice
 *   domain: software engineering / political economy of technology / intellectual property
 *
 * SUMMARY:
 *   This file instantiates ONE reading — the pragmatic openness reading — of
 *   the contested kernel software_control_legitimacy: the claim that the
 *   open-versus-proprietary question is a development-methodology choice to
 *   be settled by outcomes, with open source preferred where peer review and
 *   collaboration demonstrably produce better software, and proprietary
 *   models remaining fully legitimate alternatives. The standing arrangement
 *   under assessment is the industry settlement built on that claim:
 *   permissive-license defaults, portfolio-and-reputation signaling that
 *   channels developers toward open participation, corporate consumption of
 *   community-maintained infrastructure, and the explicit legitimation of
 *   closed development. Assessed by this reading's own lights, the
 *   arrangement carries no victim set — both models are legitimate — and its
 *   costs (uncompensated maintenance labor, discursive displacement of the
 *   freedom framing) are borne by voluntary participants. The sibling
 *   readings (freedom-imperative, property-rights, commons) instantiate
 *   different constraints with different victim structures and epsilon
 *   values; per the epsilon-invariance principle the contest is recorded in
 *   the omega variables and sibling files, not inside this constraint. The
 *   claim/metric relationship is deliberately unreconciled: the reading is
 *   CLAIMED as rope (methodology pluralism coordinating on quality) while the
 *   authored metrics describe the arrangement's actual operation, including a
 *   small but rising free-rider cost. KEY AGENTS (by structural
 *   relationship): - open_source_developers: Net beneficiary with a real cost
 *   side (moderate/mobile) — contribute labor and reputation into shared
 *   codebases - software_end_users: Beneficiary (moderate/mobile) — receive
 *   the quality and transparency dividend where openness wins -
 *   proprietary_software_vendors: Beneficiary (powerful/arbitrage) — the norm
 *   is what stands between them and delegitimation -
 *   corporate_open_source_consumers: Deepest beneficiary seat; the
 *   arrangement's marginal gains land here (institutional/arbitrage) -
 *   open_source_foundations: Agenda setter (institutional/constrained) —
 *   steward definitions and shared infrastructure -
 *   free_software_movement_advocates: Cost-bearer (organized/identity_locked)
 *   — pay in discursive currency - software_engineering_researchers:
 *   Analytical observer (analytical/analytical) — test the reading's
 *   load-bearing empirical claim
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.2).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.12).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Pragmatic Openness Norm: Software Control as a Methodology Choice").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "software engineering / political economy of technology / intellectual property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, 'b69076f2-e2bd-49fc-a1db-90b0577fb3b6').
narrative_ontology:cs_kernel_codification('b69076f2-e2bd-49fc-a1db-90b0577fb3b6', distributed).
narrative_ontology:cs_authority_grounding('b69076f2-e2bd-49fc-a1db-90b0577fb3b6', expertise).
narrative_ontology:cs_reading_relation('b69076f2-e2bd-49fc-a1db-90b0577fb3b6', software_control_legitimacy__freedom_imperative_reading, influences).
narrative_ontology:cs_reading_relation('b69076f2-e2bd-49fc-a1db-90b0577fb3b6', software_control_legitimacy__property_rights_reading, influences).
narrative_ontology:cs_reading_relation('b69076f2-e2bd-49fc-a1db-90b0577fb3b6', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('b69076f2-e2bd-49fc-a1db-90b0577fb3b6', foundational, methodology_instrumentalism).
narrative_ontology:cs_axiom_status(methodology_instrumentalism, holdable).
narrative_ontology:cs_axiom_grounding('b69076f2-e2bd-49fc-a1db-90b0577fb3b6', methodology_instrumentalism, instrumental).
narrative_ontology:cs_axiom('b69076f2-e2bd-49fc-a1db-90b0577fb3b6', foundational, outcome_contingent_proprietary_legitimacy).
narrative_ontology:cs_axiom_status(outcome_contingent_proprietary_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b69076f2-e2bd-49fc-a1db-90b0577fb3b6', outcome_contingent_proprietary_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('b69076f2-e2bd-49fc-a1db-90b0577fb3b6', methodology_pluralist_market).
narrative_ontology:cs_drift_state('b69076f2-e2bd-49fc-a1db-90b0577fb3b6', contemporary_ai_weights_and_corporate_oss_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('b69076f2-e2bd-49fc-a1db-90b0577fb3b6', '2026-08-10T12:00:00Z').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, open_source_developers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_end_users).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, corporate_open_source_consumers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, open_source_foundations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_control_legitimacy__pragmatic_openness_reading, open_source_developers).
narrative_ontology:constraint_victim(software_control_legitimacy__pragmatic_openness_reading, free_software_movement_advocates).
narrative_ontology:constraint_vindicates(software_control_legitimacy__pragmatic_openness_reading, eyeballs_shallow_bugs_conjecture).
narrative_ontology:constraint_vindicates(software_control_legitimacy__pragmatic_openness_reading, bazaar_development_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Contribute code, review, and maintenance labor to shared repositories, often unpaid or underpaid alongside employment. What flows to them: reputation, portable skills, employment leverage, early access to tooling, and the professional standing of a public track record. What flows from them: the maintenance labor that keeps critical infrastructure running. Exit looks like moving to closed-source employment or dropping out of public contribution — both are real options, which is why participation persists; the recurring cost is burnout on projects the industry depends on but does not pay for.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_developers, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__pragmatic_openness_reading, open_source_developers, payer).

% Run software whose quality, security posture, and longevity depend on which methodology produced it. Where openness wins they get inspectable, durable, vendor-independent tools; where closed products win on fit or polish they buy those. Their lever is product choice and little more — they do not sit in the venues where methodology norms are set, and switching costs bound how mobile they actually are.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_end_users, beneficiary,
    moderate, biographical, mobile, global).

% Sell software whose source customers cannot inspect or modify. What the norm gives them: legitimacy — closed development is framed as one methodology among several rather than an ethical failing, so they compete on outcomes instead of defending their right to exist. What it costs them: open substitutes commoditize their categories, and hiring pipelines favor candidates with public portfolios. They can and do shift per product between closed, open-core, and hosted models.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_vendors, beneficiary,
    powerful, generational, arbitrage, global).

% Large platform and cloud companies that build commercial products on community-maintained open infrastructure. They contribute selectively — enough to steer projects they depend on — while the bulk of maintenance labor remains unpaid or foundation-funded. What accrues to them: the output of collaborative development as free input to revenue-generating products, plus the legitimacy the norm extends to whatever they keep closed. Their scale lets them fork, fund, or abandon upstream projects at will, an option no individual maintainer holds.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, corporate_open_source_consumers, beneficiary,
    institutional, generational, arbitrage, global).

% Steward the license definitions, trademarks, and shared infrastructure the ecosystem runs on — the Open Source Definition, and Apache- or Linux-style project foundations. They decide what counts as open, host projects no single company will maintain, and set the procedural agenda through which methodology disputes get resolved. Their room to maneuver is bounded by their missions and by the corporate members who fund them.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_foundations, agenda_setter,
    institutional, generational, constrained, global).

% Hold that software control is a question of user freedom and that proprietary programs are an ethical wrong, not a style choice. What they bear under the operative settlement: their framing has been displaced — 'open source' won the vocabulary, corporate adoption proceeded on explicitly non-moral grounds, and their core claim now enters the conversation only in weakened form. They campaign, publish, and license (copyleft) against the current, and their position is constitutive of who they are: adopting the pragmatic frame would dissolve the movement's reason for existing.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, free_software_movement_advocates, payer,
    organized, generational, identity_locked, global).

% Study whether and where open development actually outperforms closed: defect rates, security response, velocity, maintainability. They are the seat that tests the settlement's load-bearing empirical claim, with no stake in which answer wins beyond the publication itself. Their findings feed back into corporate methodology decisions and foundation policy.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_engineering_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__pragmatic_openness_reading, corporate_open_source_consumers).
narrative_ontology:fixing_cost_class(software_control_legitimacy__pragmatic_openness_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the ecosystem-scale problem of how development methodology gets chosen: it replaces per-project relitigation of first principles (freedom versus property) with a shared decision rule — pick the model by outcomes — plus the shared vocabulary ('open source') and signaling infrastructure (portfolios, foundations, license definitions) that let millions of developers and thousands of firms coordinate participation without a central authority.
% TRANSFER_FUNCTION: Moves maintenance labor and reputation from individual developers into shared codebases, and from there into commercial products built on that infrastructure; moves legitimacy from ethical principle to demonstrated outcome, which lets open and proprietary models alike claim validity; and moves the discursive center of gravity from moral argument to engineering argument.
% ABSENT_VOICES: The strongest form of the freedom-imperative objection is present but structurally muted: its holders are in the conversation, yet the norm reframes their claim (proprietary software wrongs its users) as a methodology preference, so the objection enters defanged. End users of proprietary software — the people the freedom reading says are wronged — have no seat in methodology debates at all. Maintainers who burn out exit silently rather than contesting the settlement that produced their workload.
% DISAPPEARANCE_RATIONALE: The settlement is load-bearing: corporate open-source strategy, permissive-license defaults, hiring and portfolio signaling, and the funding model for shared infrastructure all presuppose it. If it vanished overnight, the industry would relitigate methodology from first principles — the freedom and property camps would contest every license and procurement decision — and the vocabulary and institutions built on the pragmatic frame would lose their coordinating function, even though the code and the communities would persist.
% FOUNDING_PROBLEM: In the late 1990s the collaborative-development movement could not get commercial adoption: the free-software frame's moral premises (proprietary software is an ethical wrong) read as hostile to business, and firms would not build on a movement that condemned them. The frame was rebuilt as 'open source' — a methodology claim with no ethical premises — to make collaborative development adoptable at commercial scale.
% FOUNDING_PROBLEM_CORROBORATION: The adoption problem's existence and its solution are attested from outside the pragmatic frame's beneficiary set: the free software movement's contemporaneous and continuing objections (Stallman's public statements that 'open source' was coined to avoid the freedom argument) confirm both the original adoption barrier and the reframe; contemporaneous business-press coverage of the Netscape/Mozilla decision documents firms adopting on explicitly pragmatic grounds; and the empirical software engineering literature records adoption drivers as commercial rather than ethical. No party inside the settlement disputes that the founding problem was commercial adoptability — the dispute is over whether solving it that way was a betrayal or a victory.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__pragmatic_openness_reading_tests).
:- end_tests(software_control_legitimacy__pragmatic_openness_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.20 at interval end) because the norm commands no transfer: it moves decisions and labor only through persuasion, signaling, and voluntary participation, and it explicitly legitimates the alternative model. The small but real cost side — uncompensated maintenance labor flowing into commercial products, and the displacement of the freedom framing — is what keeps epsilon off zero and rising slowly across the series (0.08 to 0.20) as open source became critical infrastructure and corporate consumption scaled. Suppression is low (0.12) and carries no time series: the norm has no enforcement machinery — no gate, no sanction — only defaults, vocabulary, and a reputational gradient, so the enforcement picture is static and is captured by the scalar rather than a suppression_requirement series. Theater is low (0.20) but rising with open-washing: firms performing openness for legitimacy while keeping substance closed; the performative share grows as the norm's legitimacy value grows. Accessibility_collapse is low (0.30): the norm does not collapse alternatives — it ranks them, and proprietary practice remains fully legitimate and widely exercised. Resistance is moderate-low (0.30): the freedom-imperative camp rejects the norm as an amoral defanging of a moral claim and the property camp reads openness advocacy as hostile to commercial software, but both contest it discursively while operating inside the settlement day to day. Theater_ratio and base_extractiveness are authored on one shared seven-point grid (1998, 2003, 2008, 2013, 2018, 2023, 2026); every tracked metric carries a value at every point, and the terminal points are present-state assessments as of the generation date.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the corporate consumer seat the arrangement is a commons that underprices its own output — the best possible input pricing. From the maintainer seat the same arrangement is an unpaid obligations engine that pays in reputation. From the proprietary vendor seat it is legitimacy insurance that also commoditizes the vendor's categories. From the freedom-advocate seat it is a defeated moral claim wearing the victor's vocabulary. The engine derives these per-seat classifications from the structural data (power, exit, role, position); this story's rope claim is the reading's own seat and does not adjudicate the divergence — where the computed per-seat types diverge from the claim, that divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   No victims are declared: per this reading's lights both models are legitimate and nobody is wronged by the arrangement — the declared structure is beneficiaries only, which is what keeps the reading's epsilon low. Open_source_developers are net beneficiaries (career capital, tooling, standing) with a real cost side (uncompensated maintenance), encoded in their dual beneficiary/payer role; their mobility keeps them nearer the beneficiary end than burnout alone would place them. Software_end_users benefit where openness wins and retain product choice. Proprietary_software_vendors benefit directly — the norm is what stands between them and the freedom reading's delegitimation — though the openness it normalizes also commoditizes their categories, a mixed position the derivation approximates without an override. Corporate_open_source_consumers sit nearest the beneficiary end of anyone: arbitrage-grade exit over the commons (fork, fund, or abandon at will) and first claim on its output. Free_software_movement_advocates are the arrangement's clearest cost-bearers: identity-locked (their position is constitutive of who they are), organized, and paying in discursive currency — the derivation reads their payer role and identity lock as the highest-directionality seat in the story. Open_source_foundations administer the settlement and sit near symmetric: they set the procedural agenda but collect no transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making collaborative development commercially adoptable without its ethical premises — is dead: solved so thoroughly that the solution is now the industry's default vocabulary. But the arrangement is not a zombie and mandatrophy is not resolved, because what maintains it now is a live function (quality-driven methodology coordination, security transparency, shared infrastructure) rather than the dead mandate. The status-by-verdict pattern (founding problem dead, world rearranges on disappearance) is exactly the mismatch that flags for capture/zombie review; the cross-check against the computed path resolves it as mandate-expansion rather than capture: theater is low, no seat administers a transfer, and the arrangement would still coordinate if the founding problem had never existed. The classification discipline matters here in both directions: reading the norm as pure coordination (as its beneficiaries do) would miss the rising free-rider cost side; reading it as defanged extraction (as the freedom reading does) would fabricate a victim set this reading's structure does not contain. The metrics sit deliberately between: low but nonzero and slowly rising epsilon, with the drift question routed to the maintenance_labor_extraction_drift omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is the pragmatic_openness_reading of kernel software_control_legitimacy. How much of the classification is contingent on the reading rather than on the underlying arrangement of software development practice?',
    'No empirical resolution: the four readings are held by different parties with different grounding types (instrumental, deontological, conventional-property, governance-first). Resolution would require a party to switch grounding, not new data. Sibling constraint stories instantiate the alternatives.',
    'Under the freedom_imperative_reading the same standing arrangement gains a victim set (users of proprietary software denied control over their computing) and substantially higher epsilon; under the property_rights_reading the victim set inverts (creators facing contestation of their restriction authority); under the commons_reading the evaluative axis shifts from methodology outcomes to governance quality. Epsilon and type are reading-indexed over a fixed referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Reading-contingency of classification within the software_control_legitimacy kernel.').

omega_variable(
    methodology_quality_claim_status,
    'Does open source actually produce better software through peer review and collaboration — this reading''s load-bearing empirical claim — and in which layers of the software stack?',
    'Replicated empirical software engineering studies comparing defect rates, security response times, and velocity across open and closed comparables; natural experiments from firms that opened or closed specific products.',
    'If the quality claim fails broadly, the reading dissolves into mere preference or collapses toward the property reading; if it holds layer-specifically (infrastructure yes, application and frontier-model layers contested), the norm''s scope contracts to the layers where it is true.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodology_quality_claim_status, empirical, 'Empirical status of the openness-produces-quality premise that grounds this reading''s instrumental authority.').

omega_variable(
    maintenance_labor_extraction_drift,
    'Is the uncompensated-maintainer economy sustainable, or does corporate free-riding on maintenance labor grow until the collaborative function the norm coordinates degrades?',
    'Maintainer-burnout surveys, funding-flow analyses of who pays for critical-infrastructure maintenance, and contribution-asymmetry data between corporate consumers and the maintainer base.',
    'If free-riding intensifies, the arrangement drifts toward a hybrid with a de facto victim set (maintainers as effective targets, corporate consumers as capturers) and the rising base_extractiveness series continues; if funding rebalances toward maintainers, epsilon stabilizes low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_labor_extraction_drift, empirical, 'Whether the free-rider cost side of the norm grows or stabilizes over the coming interval.').

omega_variable(
    ai_weights_recontestation,
    'Does the open-versus-closed AI weights debate re-instantiate the pragmatic frame (outcomes decide, per artifact) or break it — when model weights rather than source code are the artifact, do peer review and collaboration even apply?',
    'Track whether the open-weights debate is argued in outcome terms (capability, safety, auditability) or reverts to freedom and property framings; watch whether pragmatic institutions (foundations, OSI-style definitional bodies) extend their authority to weights.',
    'If the frame extends, this reading''s scope expands to a new artifact class; if it breaks, the pragmatic settlement is domain-bound to source code and the kernel contest re-opens at the AI layer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_weights_recontestation, empirical, 'Whether the pragmatic frame survives the artifact-class shift from source code to model weights.').

omega_variable(
    authority_framing_underdetermination,
    'Is the kernel''s authority structure correctly framed as expertise-grounded (engineering results adjudicate the openness claim), or is the deeper truth that the kernel has no adjudicator at all — four readings with no arbiter, making the contest itself the structure?',
    'Conceptual: examine whether any institution actually adjudicates between readings or whether each reading self-adjudicates within its own grounding; check whether engineering outcomes ever settle disputes across readings or only within this one.',
    'If no cross-reading adjudicator exists, the distributed codification carries the whole commitment-system classification and the expertise framing is local to this reading only — changing how drift and foreclosure compute across the sibling set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_underdetermination, conceptual, 'Whether the kernel''s authority is expertise (this reading''s frame) or genuinely unadjudicated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 1998, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1998, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 1998, 0.05).
narrative_ontology:measurement_basis(soft_tr_t1998, observed).
narrative_ontology:measurement(soft_tr_t2003, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2003, 0.07).
narrative_ontology:measurement_basis(soft_tr_t2003, observed).
narrative_ontology:measurement(soft_tr_t2008, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement_basis(soft_tr_t2008, observed).
narrative_ontology:measurement(soft_tr_t2013, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2013, 0.13).
narrative_ontology:measurement_basis(soft_tr_t2013, observed).
narrative_ontology:measurement(soft_tr_t2018, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2018, 0.16).
narrative_ontology:measurement_basis(soft_tr_t2018, observed).
narrative_ontology:measurement(soft_tr_t2023, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2023, 0.19).
narrative_ontology:measurement_basis(soft_tr_t2023, observed).
narrative_ontology:measurement(soft_tr_t2026, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2026, 0.2).
narrative_ontology:measurement_basis(soft_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t1998, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 1998, 0.08).
narrative_ontology:measurement_basis(soft_be_t1998, observed).
narrative_ontology:measurement(soft_be_t2003, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2003, 0.1).
narrative_ontology:measurement_basis(soft_be_t2003, observed).
narrative_ontology:measurement(soft_be_t2008, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2008, 0.12).
narrative_ontology:measurement_basis(soft_be_t2008, observed).
narrative_ontology:measurement(soft_be_t2013, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2013, 0.14).
narrative_ontology:measurement_basis(soft_be_t2013, observed).
narrative_ontology:measurement(soft_be_t2018, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2018, 0.16).
narrative_ontology:measurement_basis(soft_be_t2018, observed).
narrative_ontology:measurement(soft_be_t2023, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2023, 0.19).
narrative_ontology:measurement_basis(soft_be_t2023, observed).
narrative_ontology:measurement(soft_be_t2026, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2026, 0.2).
narrative_ontology:measurement_basis(soft_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_control_legitimacy__pragmatic_openness_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, information_standard).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the open source versus proprietary debate' covers four structurally distinct constraints — one per reading of the software_control_legitimacy kernel. They differ in epsilon (this reading: low, no victim set; freedom reading: high, proprietary-software users as victims; property reading: high from the creator seat; commons reading: intermediate, governance-dependent), in victim structure, and in claimed type. Per the epsilon-invariance principle they are authored as separate stories linked by network edges rather than one story with a framing parameter. This story is the establishment member of the family: its vocabulary and settlement are the operating environment the sibling readings contest, which is why its edges to the freedom and property readings are 'influences' rather than 'coexists_with'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
