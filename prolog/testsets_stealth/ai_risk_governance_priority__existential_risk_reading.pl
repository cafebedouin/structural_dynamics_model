% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__existential_risk_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: X-Risk Primacy Mandate in AI Governance (Existential-Risk Reading)
 *   domain: technology governance/ethics/risk assessment
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   ai_risk_governance_priority: the existential_risk_reading, under which AI
 *   risk governance must prioritize preventing superintelligence scenarios
 *   that could annihilate or permanently curtail humanity's potential. The
 *   constraint under examination is the standing governance arrangement that
 *   priority produces — the funding streams, statutory mandates, evaluation
 *   regimes, and agenda-setting power organized around catastrophic-scenario
 *   prevention. Its ε referent is that standing arrangement as this reading
 *   assesses it, never the arrangement a sibling reading would install. Per
 *   the epsilon-invariance principle, the colloquial label 'AI risk
 *   governance' covers structurally distinct claims with different victim
 *   sets and different epsilon profiles; the sibling readings
 *   (near_term_harms_reading, bridge_reading) are separate constraint stories
 *   linked through the network, not folded into this one. KEY AGENTS (by
 *   structural relationship): - safety_leadership_frontier_labs:
 *   agenda-setting beneficiary (institutional/arbitrage) — defines evaluation
 *   standards and safety framing while collecting legitimacy, capital access,
 *   and regulatory moat value - xrisk_research_institutions: primary
 *   beneficiary (organized/identity_locked) — collects funding and
 *   professional purpose; exit would dissolve the identity the institutions
 *   constitute - governmental_ai_safety_institutes: agenda setter
 *   (institutional/constrained) — administers the mandate; bound by statutory
 *   charter - present_harms_affected_communities: primary payer
 *   (powerless/constrained) — bears displaced mitigation of demonstrated
 *   present harms - independent_and_opensource_developers: secondary payer
 *   (moderate/constrained) — bears compliance costs scaled against their size
 *   - future_humanity: nominal protectee, structurally excluded
 *   (powerless/trapped) — invoked constantly, able to participate never -
 *   ai_fairness_advocacy_networks: excluded voice (organized/constrained) —
 *   would reorder the agenda toward present harms -
 *   interdisciplinary_risk_scholars: analytical observer
 *   (analytical/analytical) — sees the full allocation pattern without a
 *   portfolio stake
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.65).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.6).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "X-Risk Primacy Mandate in AI Governance (Existential-Risk Reading)").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "technology governance/ethics/risk assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, 'b3d11862-35e4-4102-80da-c49434f68aef').
narrative_ontology:cs_kernel_codification('b3d11862-35e4-4102-80da-c49434f68aef', formalized).
narrative_ontology:cs_authority_grounding('b3d11862-35e4-4102-80da-c49434f68aef', expertise).
narrative_ontology:cs_interpretation_layer_present('b3d11862-35e4-4102-80da-c49434f68aef').
narrative_ontology:cs_reading_relation('b3d11862-35e4-4102-80da-c49434f68aef', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3d11862-35e4-4102-80da-c49434f68aef', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('b3d11862-35e4-4102-80da-c49434f68aef', foundational, xrisk_priority_lexically_orders_agenda).
narrative_ontology:cs_axiom_status(xrisk_priority_lexically_orders_agenda, holdable).
narrative_ontology:cs_axiom_grounding('b3d11862-35e4-4102-80da-c49434f68aef', xrisk_priority_lexically_orders_agenda, empirically_contingent).
narrative_ontology:cs_axiom('b3d11862-35e4-4102-80da-c49434f68aef', foundational, irreversible_loss_justifies_precautionary_resource_bias).
narrative_ontology:cs_axiom_status(irreversible_loss_justifies_precautionary_resource_bias, holdable).
narrative_ontology:cs_axiom_grounding('b3d11862-35e4-4102-80da-c49434f68aef', irreversible_loss_justifies_precautionary_resource_bias, deontological).
narrative_ontology:cs_reference_frame('b3d11862-35e4-4102-80da-c49434f68aef', xrisk_primacy_framework).
narrative_ontology:cs_drift_state('b3d11862-35e4-4102-80da-c49434f68aef', contemporary_partial_codification_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b3d11862-35e4-4102-80da-c49434f68aef', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, safety_leadership_frontier_labs).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, governmental_ai_safety_institutes).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, present_harms_affected_communities).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, independent_and_opensource_developers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, future_humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, future_humanity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the largest frontier training runs while publishing safety frameworks, hosting governance summits, and defining evaluation standards. Safety positioning accompanies capital raising and shapes which risks regulators treat as first-order. These firms can rebrand, relocate operations, or pivot public messaging at comparatively low cost, and their market position survives most versions of the governance debate.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, safety_leadership_frontier_labs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, safety_leadership_frontier_labs, beneficiary).

% Run alignment research programs, forecasting exercises, and policy shops funded by philanthropy, grants, and lab contracts. Staffing plans, publication pipelines, and fundraising appeals all presuppose that catastrophic loss-of-control scenarios are the governing concern. Careers and organizational missions were built around this framing; abandoning it would dissolve the professional identity these institutions were founded to pursue.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutions, beneficiary,
    organized, biographical, identity_locked, global).

% Newly chartered public bodies that convene model evaluations, set testing protocols, and advise legislators, with statutory mandates centered on catastrophic-risk scenarios. Budget lines and legal charters tie them to this portfolio; redirecting toward other harm categories would require fresh legislation and inter-agency renegotiation.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, governmental_ai_safety_institutes, agenda_setter,
    institutional, generational, constrained, national).

% Live with documented discrimination, surveillance, misinformation exposure, and job displacement from systems already deployed. Advocacy for redress competes with catastrophic-scenario programming for the same limited regulatory bandwidth, hearing slots, and funding calls. They cannot opt out of AI-mediated housing, hiring, credit, and policing systems, and their harms arrive on timescales shorter than any governance cycle built around speculative futures.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, present_harms_affected_communities, payer,
    powerless, immediate, constrained, global).

% Build and release models outside the large labs. Compliance regimes designed around frontier-scale catastrophic scenarios — compute thresholds, licensing, know-your-customer requirements — impose fixed costs that weigh heaviest on small teams. Jurisdiction shopping offers partial relief at best as rules harmonize across borders, and the leading deployment platforms increasingly sit inside the regulated perimeter regardless.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, independent_and_opensource_developers, payer,
    moderate, biographical, constrained, global).

% Do not yet exist and therefore cannot testify, consent, lobby, or verify that protection purchased in their name is delivered. Every charter, statement, and framework in this governance stream speaks on their behalf. Whether the arrangements made today actually preserve their prospects is not observable by anyone currently party to the conversation, and no mechanism exists for them to contest how their interests are represented.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, future_humanity, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, future_humanity, beneficiary).

% Civil-society organizations with standing expertise in auditing deployed systems for bias, surveillance, and labor harms. They hold consultative seats in some forums but report that agenda time, funding calls, and legislative drafts center catastrophic scenarios; their proposals tend to enter consultations late and emerge diluted, and their core constituencies have no seat in the invitation-only summit circuit.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_fairness_advocacy_networks, excluded,
    organized, immediate, constrained, global).

% Academics from decision theory, epidemiology, disaster studies, and science-and-technology studies who compare how different hazard classes are governed. They publish analyses of base rates, institutional incentives, and precautionary doctrine across domains, and draw no funding from any AI governance constituency, which lets them see the whole allocation pattern without a stake in any portfolio.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, interdisciplinary_risk_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__existential_risk_reading, safety_leadership_frontier_labs).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a race-dynamics collective-action problem: no single lab or state can unilaterally slow capability development without losing position to rivals, so a coordinated governance priority aimed at catastrophic outcomes addresses a commons problem that bilateral restraint cannot. The function is real conditional on the empirical premise that such outcomes are sufficiently probable.
% TRANSFER_FUNCTION: Moves funding, talent, regulatory attention, and agenda-setting authority from broad AI-harm mitigation and general research toward catastrophic-risk-focused institutions, evaluation regimes, and governance frameworks; moves legitimacy and capital access toward labs adopting safety-leadership positioning; moves compliance costs onto smaller developers and opportunity costs onto communities awaiting redress for present harms.
% ABSENT_VOICES: Present-harm-affected communities sit outside the invitation-only summit and framework circuit; future generations are literally absent and spoken for by proxies with their own institutional interests; open-source developers are underrepresented in processes designed around frontier-lab counterparts; accelerationist dissenters self-exclude or are framed as reckless rather than engaged. They are located in affected neighborhoods, code repositories, unfunded academic departments, and rival jurisdictions.
% DISAPPEARANCE_RATIONALE: If the priority mandate vanished overnight, dedicated funding streams, career ladders, statutory mandates, summit infrastructure, and lab safety-branding strategies would lose their organizing object within months; resources would redistribute toward nearer-term harm mitigation and unrestricted capability work, and the current institutional ecology of institutes, eval consortia, and policy shops would reorganize around whichever framing captured the vacated agenda space.
% FOUNDING_PROBLEM: The prospect that advanced AI systems could escape human control and cause irreversible catastrophe — a problem compounded by competitive dynamics in which every actor lacks sufficient incentive for unilateral caution, so that only coordinated prioritarian governance could supply the missing restraint.
% FOUNDING_PROBLEM_CORROBORATION: Partial external corroboration exists: public statements on extinction-level risk signed by tens of thousands of respondents including many senior scientists with no x-risk institutional affiliation, and planning documents from several national security and foreign-policy establishments that treat loss-of-control as a scenario warranting preparation. The reading acknowledges plainly that the densest attestations still come disproportionately from people embedded in the benefiting ecosystem — lab safety teams, grant-funded institutes — and that fully independent corroboration of the probability premise remains thin.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. I claim tangled_rope because the arrangement possesses BOTH a genuine coordination function — race-dynamics commons problems are real, and unilateral restraint is structurally unstable — AND asymmetric operation: identifiable seats collect resources and legitimacy while other identifiable seats bear displaced mitigation and compliance costs, sustained by active enforcement (grant gatekeeping, summit invitation control, statutory mandates, reputational sanction of defectors). The metrics describe the arrangement's actual operation as this reading's own evidential standards assess it: extractiveness 0.65 reflects substantial resource and attention capture that is real but bounded by the possibility that the underlying premise is true and the spending is protective; suppression 0.60 reflects mostly epistemic and reputational enforcement (recklessness stigma, funding gatekeeping, framing control) rather than state coercion — suppression is authored as a raw structural property and is NOT scaled by power or scope, only extractiveness is; theater_ratio 0.40 reflects a growing share of summitry, voluntary-commitment signing, and safety-branding that performs concern without binding anyone, alongside genuine technical alignment work; accessibility_collapse 0.35 reflects that alternatives (near-term-harms-first governance, accelerationist laissez-faire) remain visibly available and argued — the frame does not close the option space the way a natural law would; resistance 0.60 reflects active, organized pushback from fairness advocates, open-source developers, and parts of the capability community. The temporal series run on ONE shared grid (t = 0,3,6,9,12,14,16, mapping approximately 2019 to 2025) with all three metrics authored at every point; trajectories are monotonic rather than cyclical — the dynamic is accumulation (enforcement infrastructure maturing, theater growing with institutionalization, extraction deepening as crowding-out compounds), not oscillation, so no intermittent-reinforcement reading applies.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats should compute differently, and the structural data supports that divergence. From the frontier-lab seat, the arrangement is stewardship: they built the frameworks, host the summits, and experience the priority as responsible self-governance that also happens to advantage them. From the xrisk_institution seat, it is vocation fused with livelihood — identity_locked exit means the arrangement and the self are not separable. From the present-harms-communities seat, the same arrangement operates as the systematic subordination of their documented, dated injuries to scenarios nobody can date; from the open-source developer seat, it operates as fixed compliance costs sized for actors a hundredfold larger. The observer seat sees a single allocation pattern that each participant experiences as a different institution. The engine computes per-seat classifications from the power, exit, and role data; this commentary explains why the divergence is structural rather than perceptual noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: frontier labs sit nearest the beneficiary pole (d near 0.1) because they both set the rules and collect moat value, with arbitrage-grade exit damping any residual cost they bear; xrisk_research_institutions sit low (d near 0.15) but their identity_locked exit means they cannot actually leave, which keeps them structurally invested rather than mobile; governmental institutes sit low-to-mid (d near 0.25) because mandate benefit comes bundled with mission-lock costs. Victim declarations drive high directionality: present_harms_affected_communities approach the full-target pole (d near 0.85) — powerless, constrained exit, immediate-horizon losses; independent_and_opensource_developers sit high (d near 0.7) with moderate power and partial mobility. future_humanity is declared a victim per this reading's structural delta, but their directionality is genuinely indeterminate: they exert no structural force in either direction, which is precisely why they are seated as excluded rather than as an operating party. No directionality_overrides are authored: overrides key on power atoms, and this story contains two powerless seats (present-harms communities and future humanity) whose true directionalities diverge — a coarse powerless-atom override would corrupt one to fix the other, so the derivation chain is left to read each seat's beneficiary/victim and exit data directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters unusually here because both failure modes are live temptations. Reading the arrangement as pure coordination (rope) would erase the measurable displacement of present-harm mitigation and the moat value accruing to incumbents — the cover-story victory. Reading it as pure extraction (snare) would erase the genuine race-dynamics commons problem that gives the priority its coordination content — the cynicism mirror-image, which ironically serves the same incumbents by discrediting all governance. Tangled_rope holds both truths: coordination function and asymmetric extraction through the same structure, requiring active enforcement. On obsolescence: the founding problem is authored live per this reading's lights, and the R5 mismatch consumer should find no zombie signature (status=live paired with world_rearranges is the coherent pairing — the arrangement persists because its problem persists, not because its function atrophied behind theatrical maintenance). Piton is ruled out structurally: concentrated beneficiaries demonstrably profit, which the piton profile forbids. If the probability premise were later resolved downward, the correct trajectory would be toward snare (cover without substance), not piton — the enforcement machinery would persist while the coordination justification evaporated, and the temporal series here supplies the baseline against which that drift would be detected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of kernel ai_risk_governance_priority — how would instantiating the near_term_harms_reading or bridge_reading instead change the structural classification?',
    'Cross-reading comparison across the linked sibling stories: align victim sets, epsilon distributions, and computed per-seat types across all three readings of the kernel and diff the structural deltas.',
    'Under the near-term-harms reading the victim set inverts (x-risk apparatus becomes the distractor, marginalized communities the protected class) and this arrangement''s seats would compute as agenda-distorting rather than protective; under the bridge reading the suppression of present-harms voices drops substantially because neither concern-class is subordinated. Classification is reading-relative by design; this omega marks the relativity explicitly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story is one indexed reading of a three-reading kernel; sibling readings are separate constraints.').

omega_variable(
    xrisk_probability_calibration,
    'Is the empirical premise — that superintelligence-driven catastrophe within the policy-relevant horizon carries non-negligible probability — actually warranted at the strength the priority arrangement assumes?',
    'Structured forecasting tournaments with resolution criteria, calibrated expert elicitation with published error bars, and trend extrapolation subjected to adversarial red-teaming, tracked over successive capability generations.',
    'If calibrated probability is far below the assumed level, the coordination justification thins toward cover and the arrangement drifts snare-ward (enforcement persisting without substance); if it is at or above the assumed level, most measured extraction is the price of genuine protection and the tangled_rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(xrisk_probability_calibration, empirical, 'Whether the founding premise''s probability mass justifies the priority''s resource claim.').

omega_variable(
    safety_leadership_substantiveness,
    'Do labs claiming safety leadership deliver substantive safety work, or does the positioning function primarily as brand cover while capability racing continues unchanged?',
    'Audited evaluation results, incident and near-miss disclosure records, staff retention in safety teams versus capability teams, and whistleblower testimony compared against public safety commitments.',
    'If substantiveness is low, the primary beneficiary seat is collecting rents under protective cover and effective extraction concentrates further on that seat''s counterparties; if high, the seat is delivering the coordination good it charges for.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_leadership_substantiveness, empirical, 'Whether the safety-leadership beneficiary seat produces the good or performs it.').

omega_variable(
    present_harm_displacement_magnitude,
    'How much present-harm mitigation — in funding, regulatory bandwidth, and legislative attention — is actually displaced by catastrophic-scenario prioritization?',
    'Funding-flow and agenda-time analysis constructing the counterfactual allocation: compare budget shares, hearing calendars, and standards-development effort against pre-priority baselines and against jurisdictions that did not adopt the priority.',
    'Sizes the payer seats'' borne cost directly; a large displacement confirms the asymmetric-extraction half of the tangled_rope structure, a small one suggests the priority is largely additive rather than displacing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(present_harm_displacement_magnitude, empirical, 'Magnitude of crowding-out borne by present-harms constituencies.').

omega_variable(
    resistance_suppression_mechanism,
    'Is the suppression of dissent from the priority structural (funding gatekeeping, credential and invitation barriers, statutory mandates) or internalized (self-censorship under recklessness stigma, identity fusion with the safety mission)?',
    'Post-exit trajectory study: interview researchers and officials after they leave the ecosystem and measure whether their willingness to voice near-term-harms positions persists once gatekeepers are no longer relevant to them; audit funding decisions for viewpoint-correlated rejection rates.',
    'If internalized, effective suppression exceeds the structural measure — defectors carry the frame with them and the option space stays closed even where no barrier operates; if structural, removing the gatekeeping mechanisms would reopen alternatives quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_suppression_mechanism, empirical, 'Structural versus internalized composition of the measured suppression.').

omega_variable(
    future_persons_standing,
    'Can future humanity legitimately bear costs or hold beneficiary position in the structural derivation when they have no agency, no observer, and no verification channel — or is their invocation structurally rhetorical?',
    'Conceptual analysis within the framework''s agent ontology: decide whether proxy-held interests with zero enforcement capacity count as structural positions or as narrative devices, and whether the distinction changes directionality arithmetic for non-existent parties.',
    'If rhetorical, the future_humanity victim declaration should not feed directionality and the arrangement''s extraction profile rests entirely on its present-day payers; if structural, the arrangement''s ledger includes obligations no current seat can audit, strengthening the case that its accountability gap is constitutive rather than incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_persons_standing, conceptual, 'Standing of non-existent parties in the beneficiary/victim structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_r_tr_t3, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 3, 0.24).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(ai_r_tr_t9, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 9, 0.33).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(ai_r_tr_t14, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 14, 0.39).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 16, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_r_be_t3, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(ai_r_be_t9, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 9, 0.6).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(ai_r_be_t14, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 14, 0.64).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 16, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ai_r_su_t3, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 3, 0.42).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(ai_r_su_t9, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 9, 0.53).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(ai_r_su_t14, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 14, 0.59).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 16, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'AI risk governance' conflates three structurally distinct priority arrangements. This story (existential_risk_reading) authors epsilon for the catastrophic-risk-priority arrangement with victims including present-harms communities and a contested future-humanity stake; ai_risk_governance_priority__near_term_harms_reading authors epsilon for the present-harms-first arrangement with a different victim set (communities harmed by ungoverned capability racing) and different beneficiary set; ai_risk_governance_priority__bridge_reading authors epsilon for the unified-framework arrangement. The upstream/downstream structure runs from this reading outward: the existential reading's institutional weight (funded institutes, summit infrastructure, statutory mandates) shapes the operating environment — legitimacy conditions and resource availability — within which the bridge reading's unified frameworks must be built, which is why the reading_relations edge to the bridge is 'influences' rather than 'coexists_with'. Each member links to the others via network.affects_constraints; no member averages its epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
