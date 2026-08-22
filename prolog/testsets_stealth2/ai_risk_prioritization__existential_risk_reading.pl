% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__existential_risk_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: Existential-Risk-First AI Prioritization Norm
 *   domain: technological/governance/risk_assessment
 *
 * SUMMARY:
 *   Since roughly 2014, a specific prioritization norm has organized the AI
 *   safety field: the claim that misaligned artificial general intelligence
 *   poses an extinction-level threat, from which it follows that alignment
 *   research is paramount and other AI concerns are subordinate. This norm
 *   channels billions in funding, thousands of careers, conference agendas,
 *   legislative attention, and moral urgency toward catastrophic-risk work,
 *   while near-term algorithmic harms — discriminatory models, wrongful
 *   arrests, automated welfare denial, surveillance — are routinely reframed
 *   as distractions from the larger emergency. The ε referent for this story
 *   is the standing arrangement under contest: the existential-first
 *   prioritization regime as it actually operated across funding bodies,
 *   labs, academia, and policy from 2014 to 2026 (interval unit = years, t0 =
 *   2014, t12 = 2026), assessed by this reading's own lights. The claim and
 *   metrics are independent authored facts: I claim tangled_rope because I
 *   judge the arrangement to possess BOTH a genuine coordination function
 *   (concentrating scarce talent on a real, unresolved technical problem
 *   under real uncertainty) AND asymmetric extraction (agenda capture,
 *   resource diversion from measurable present harms, active marginalization
 *   of a rival framing). The metrics describe the arrangement's observed
 *   operation without being tuned to that claim.
 *
 * KEY AGENTS:
 *   - longtermist_funders: Agenda setter (institutional/arbitrage) — allocates the resources the priority ordering distributes and can redirect capital at will
 *   - xrisk_research_institutions: Primary beneficiary (institutional/constrained) — collects funding, status, and agenda control; their continuity depends on the ordering holding
 *   - frontier_ai_labs: Dual-positioned beneficiary/payer (institutional/constrained) — the framing licenses continued capability racing while exposing labs to control demands
 *   - near_term_harm_affected_communities: Primary target (powerless/trapped) — bears present, measurable harms whose redress is deprioritized
 *   - algorithmic_justice_researchers: Secondary target (organized/identity_locked) — loses agenda share; exit would break professionally constituted identity
 *   - ml_capability_researchers: Regulated party (powerful/mobile) — subject to publication limits and scrutiny the framing justifies
 *   - ai_policy_regulators: Inter-institutional observer (institutional/analytical) — adjusts statutory emphasis between catastrophe-frame and harm-frame instruments
 *   - future_humanity: Claimed protected class (non-agent) — cannot speak; enters only through proxy institutions that are also the arrangement's beneficiaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.63).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.66).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "Existential-Risk-First AI Prioritization Norm").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "technological/governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, 'abf29f8b-c568-4a8d-bf14-7a4c56eb5da4').
narrative_ontology:cs_kernel_codification('abf29f8b-c568-4a8d-bf14-7a4c56eb5da4', formalized).
narrative_ontology:cs_authority_grounding('abf29f8b-c568-4a8d-bf14-7a4c56eb5da4', expertise).
narrative_ontology:cs_interpretation_layer_present('abf29f8b-c568-4a8d-bf14-7a4c56eb5da4').
narrative_ontology:cs_reading_relation('abf29f8b-c568-4a8d-bf14-7a4c56eb5da4', ai_risk_prioritization__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('abf29f8b-c568-4a8d-bf14-7a4c56eb5da4', foundational, misaligned_agi_extinction_plausible_this_century).
narrative_ontology:cs_axiom_status(misaligned_agi_extinction_plausible_this_century, holdable).
narrative_ontology:cs_axiom_grounding('abf29f8b-c568-4a8d-bf14-7a4c56eb5da4', misaligned_agi_extinction_plausible_this_century, empirically_contingent).
narrative_ontology:cs_axiom('abf29f8b-c568-4a8d-bf14-7a4c56eb5da4', foundational, future_persons_stakes_dominate_present_harms).
narrative_ontology:cs_axiom_status(future_persons_stakes_dominate_present_harms, holdable).
narrative_ontology:cs_axiom_grounding('abf29f8b-c568-4a8d-bf14-7a4c56eb5da4', future_persons_stakes_dominate_present_harms, deontological).
narrative_ontology:cs_reference_frame('abf29f8b-c568-4a8d-bf14-7a4c56eb5da4', alignment_first_default_trajectory).
narrative_ontology:cs_drift_state('abf29f8b-c568-4a8d-bf14-7a4c56eb5da4', post_frontier_scaling_mainstreaming, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('abf29f8b-c568-4a8d-bf14-7a4c56eb5da4', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_harm_affected_communities).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, algorithmic_justice_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, ml_capability_researchers).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, longtermist_axiology).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, agi_extinction_plausibility).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, alignment_problem_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Allocate multi-billion-dollar grant portfolios guided by the conviction that influencing the long-term future carries overwhelming moral weight. They decide which research agendas receive sustained support, finance field-building infrastructure (fellowships, prizes, community institutions), and can redirect capital as convictions shift. Their exit is unusually easy: moving money to other causes costs administration, not identity.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, agenda_setter,
    institutional, generational, arbitrage, global).

% Conduct alignment, governance, and forecasting research aimed at catastrophic outcomes from advanced AI. They receive the largest dedicated share of AI-safety funding, supply much of the field's senior personnel, and publish many of the frameworks through which the priority ordering is articulated. Their staffing, reputations, and continuity are built on the ordering remaining in place, so exit would mean dissolution rather than relocation.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutions, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutions, agenda_setter).

% Build the most capable systems while maintaining safety teams whose scale the catastrophic-risk framing justifies. The framing simultaneously supplies license to continue rapid capability development (an aligned builder must get there first) and exposes the labs to demands for capability controls, publication limits, and third-party evaluation. They fund safety work, adopt the rhetoric, and resist binding constraints — collecting from the frame and paying into it at once.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, frontier_ai_labs, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, frontier_ai_labs, payer).

% Live with the failures of systems already deployed: discriminatory lending and hiring models, wrongful arrests from recognition systems, automated denial of welfare benefits, opaque content moderation. Redress for these injuries competes for attention and funding inside a discourse ordered by longer-timescale stakes. There is no exit from systems that already operate on them; individually dispersed, their leverage exists only in coalition.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_harm_affected_communities, payer,
    powerless, immediate, trapped, national).

% Document, measure, and litigate bias, displacement, and surveillance in deployed systems. Their funding lines, venue slots, and press standing have contracted relative to catastrophic-risk work, and their arguments are routinely reframed as distractions from the larger emergency. Leaving the specialty would mean abandoning accumulated expertise and a professional identity constituted around the justice mission — exit is technically possible and personally dissolving.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, algorithmic_justice_researchers, payer,
    organized, biographical, identity_locked, global).

% Advance model capabilities and meet the ordering from the regulated side: publication restrictions, evaluation regimes, compute thresholds, and moralized scrutiny justified by catastrophic risk. Their skills transfer across employers and borders, so individual exit is feasible — though the leading-edge employers available to them all operate inside the same framing, which blunts what mobility would otherwise buy.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, ml_capability_researchers, payer,
    powerful, biographical, mobile, global).

% Draft and enforce AI regulation across jurisdictions. The catastrophic-risk framing shapes which instruments they reach for — frontier-model evaluations, compute reporting thresholds, safety institutes — alongside nearer-term consumer-protection and anti-discrimination tools. They observe the field's internal contest between the two risk framings and adjust statutory emphasis accordingly.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, ai_policy_regulators, observer,
    institutional, generational, analytical, continental).

% The people who would exist if civilization navigates the coming decades. They cannot speak, vote, fund, or organize; their interests enter the present only through proxy institutions — which are, in this arrangement, the same institutions that benefit from the priority ordering. Whether their stakes are represented or appropriated by those proxies is the deepest open question the arrangement contains.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_humanity, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(ai_risk_prioritization__existential_risk_reading, future_humanity).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutions).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__existential_risk_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem under its own premise: concentrating scarce safety talent, funding, and attention on the single highest-consequence failure mode; producing shared research agendas, evaluation standards, and governance proposals instead of fragmented effort; giving dispersed funders a common metric for impact.
% TRANSFER_FUNCTION: Moves funding, talent, attention, and moral urgency from near-term algorithmic-harms remediation toward alignment and catastrophic-risk research; moves agenda-setting authority to longtermist institutions; transfers discursive legitimacy from justice frames to catastrophe frames.
% ABSENT_VOICES: Communities currently injured by deployed systems have no seat in the prioritization councils where the ordering is set; their advocates attend with reduced standing and shrinking budgets. Future generations are present only as proxies — represented by the very institutions that benefit from the ordering. Both absences are structural: the first because the ordering defines their concerns as subordinate, the second because nonexistence bars participation by definition.
% DISAPPEARANCE_RATIONALE: If the existential-first ordering vanished overnight, funding portfolios would rebalance toward measurable present harms, career incentives would pull researchers toward deployed-system accountability, conference and journal agendas would reweight, lab safety narratives would lose their licensing function, and legislative attention would shift toward consumer protection and anti-discrimination instruments — the AI governance landscape would reorganize around a near-term risk portfolio within a few funding cycles.
% FOUNDING_PROBLEM: In the late 2000s and early 2010s, machine learning capability was advancing rapidly while no serious institution worked on the problem of controlling systems more capable than their designers — the alignment problem was, by the founders' account, an unstaffed civilization-level risk.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists outside the beneficiary set, with a caveat: intergovernmental advisory bodies (UN high-level panels, national AI safety institutes, frontier-model evaluation regimes adopted by multiple governments) and mainstream ML researchers unaffiliated with longtermist funding attest that catastrophic-risk-from-advanced-AI is a real concern warranting dedicated work, and that alignment remains unsolved. What outsiders do NOT generally attest is the paramountcy ranking — the claim that this problem outranks all present harms is argued almost entirely from inside the arrangement's own beneficiary and agenda-setting seats, and that residual self-certification is itself signal.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.63 at interval end: the ordering diverts a large share of AI-safety resources, attention, and moral seriousness away from harms with identifiable present victims toward a risk whose magnitude is deeply contested, and the diversion grew as longtermist funding scaled. Suppression is 0.66 and is authored as a RAW structural property — the engine scales only extractiveness by directionality and scope. The suppression is mostly rhetorical and institutional (distraction framing, grant gatekeeping, conference programming, hiring norms) rather than legal-coercive, but it is actively maintained: the suppression_requirement series rises steeply through the 2020-2023 conflict period as the justice community pushed back, then plateaus as regulators begin integrating both frames. Theater_ratio climbs from 0.15 to 0.42: early-period activity was predominantly sincere technical work; as money scaled, safety-washing by capability labs and apocalypse-inflected fundraising added a growing performative share. Accessibility_collapse is low (0.35) because alternatives never collapsed — justice research, FATE venues, and consumer-protection regulation persisted throughout — and resistance is correspondingly substantial (0.60). All three metric series run on ONE shared seven-point grid (t=0,2,4,6,8,10,12) so the engine samples every metric at every examined time point. Coalition note: the powerless target seat is not helpless in aggregate — affected-community advocates, justice researchers, and labor organizations form coalitions that produced real regulatory counterweight (EU-style horizontal statutes), which is why resistance is high despite dispersed individual power.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter and beneficiary seats the arrangement is responsible stewardship: civilization-scale risk warrants disproportionate resource concentration, and the opportunity cost borne by near-term work is tragic but justified triage. From the trapped target seat the same structure operates as agenda foreclosure that prices observable present suffering below hypothetical future catastrophe — a trade made by people who will not bear the present harms. The dual-positioned lab seat experiences both: the framing protects its freedom to race and threatens it with binding controls. Same-power lateral divergence: algorithmic_justice_researchers and ml_capability_researchers hold comparable skill, standing, and mobility on paper, yet sit at opposite exits — the constraint binds justice researchers through identity (their professional self-concept is constituted by the justice mission, so exit is identity-breaking) and binds capability researchers only through reputation (skills transfer freely across employers who all share the frame). Identity-lock note: the binding mechanism on the justice side is professional identity fusion; if that frame broke — if funders rebalanced and venues reopened — the computed suppression on that seat would drop sharply, while the trapped communities' position would barely move.
 *
 * DIRECTIONALITY LOGIC:
 *   Longtermist funders sit nearest the beneficiary end: they author the ordering, control its resource flow, and hold arbitrage-grade exit (capital moves at administrative cost), so effective extraction from them is minimal or negative. X-risk research institutions are next: full recipients of the flow, but with constrained exit (institutional missions, staff, and reputations are built on the ordering). Frontier labs derive a middling d from their dual declaration — they collect legitimacy and defensive narrative value while paying in compliance burden and control exposure. Near-term harm affected communities sit nearest the full-target end: they bear the constraint's opportunity costs directly and cannot exit systems already operating on them. Algorithmic justice researchers are high-d targets whose identity_lock amplifies their trapped-ness beyond what credentials alone imply. Capability researchers are moderately high-d: the controls bind wherever they work, muting their mobility advantage. Regulators approximate the analytical-symmetric seat. Future humanity is deliberately authored as a NON-agent (agent: false): nonexistent persons must not feed the directionality arithmetic as if they collected or paid, even though they are the arrangement's claimed protected class — their exclusion from the computation is itself the structural fact the story turns on.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no serious technical program for aligning increasingly capable AI systems — remains live: capabilities have grown enormously over the interval and alignment remains unsolved, a fact corroborated well outside the beneficiary set. Mandatrophy is therefore NOT resolved, and the classification's job here is bidirectional error-prevention. Reading the arrangement as pure extraction (snare) erases the genuine coordination achievement: real technical work on a real unsolved problem, real uncertainty reduction, and a collective-action solution to fragmentation that would be valuable under almost any probability weighting. Reading it as pure coordination (rope) erases the asymmetric extraction: a specific institutional cluster captures the flow, a rival framing is actively suppressed rather than merely outcompeted, and the victims are identifiable people with present injuries. Tangled_rope holds both halves. The temporal series supplies the drift tripwire: if theater_ratio continues climbing while direct risk-reducing output stagnates, or if extinction-probability credence collapses without corresponding portfolio adjustment, the arrangement drifts toward the extractive pole and should be recomputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    one_reading_of_ai_risk_prioritization_kernel,
    'This constraint is one reading (existential_risk_reading) of the ai_risk_prioritization kernel; what would the near_term_harms_reading change structurally if it were the operative constraint?',
    'Compare the sibling story''s compiled structure: the victim set becomes present-day affected populations, the timescale collapses to the present, beneficiaries become justice-oriented institutions, and the suppression arrow reverses (catastrophism framed as evasion of present accountability).',
    'Adopting the sibling reading relocates the extraction finding onto catastrophist institutions, reverses the resource flow, and likely recomputes this arrangement as predominantly extractive from the justice seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_reading_of_ai_risk_prioritization_kernel, conceptual, 'Committer structure: kernel membership, reading identity, sibling structural delta, and location of the disagreement.').

omega_variable(
    extinction_probability_uncertainty,
    'What is the actual probability and severity of extinction-scale outcomes from misaligned advanced AI within the relevant planning horizon?',
    'Structured forecasting tournaments with resolvable ground truth, empirical progress on scalable oversight and dangerous-capability evaluations, and longitudinal calibration audits of expert predictions.',
    'A credibly low probability collapses the protective justification and the arrangement drifts toward rent-collection; a credibly high one strengthens the coordination reading and raises the threshold at which its suppressive side counts as justified triage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extinction_probability_uncertainty, empirical, 'The empirical premise that carries the entire priority ordering.').

omega_variable(
    capture_versus_sincere_coordination,
    'Do the benefiting institutions'' revealed allocations track their stated mission (reducing catastrophic risk) or institutional self-perpetuation?',
    'Funding-flow audits under budget shocks, turnover analysis of internally dissenting staff, and comparison of spending on direct risk-reducing output versus field-building, convening, and brand infrastructure.',
    'Evidence of capture pushes the computed classification toward pure extraction and validates the sibling reading''s strongest critique; evidence of mission-tracking supports the hybrid or coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_versus_sincere_coordination, empirical, 'Whether the arrangement''s beneficiaries serve the mission or the mission serves them.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the marginalization of near-term justice work maintained by external gates (grant criteria, editorial norms, hiring pipelines) or internalized (researchers pre-emptively discounting their own work as less serious)?',
    'Post-exit suppression trajectory: track researchers who leave the catastrophist frame; if justice-oriented output recovers to baseline after exit the suppression was substantially external, if it stays depressed it was internalized.',
    'If internalized, effective suppression exceeds the structural measure and persists after any gatekeeping reform; if purely external, suppression would fall quickly once funders rebalanced portfolios.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split underlying the measured suppression scalar.').

omega_variable(
    future_persons_moral_weighting,
    'Do the stakes of not-yet-existent future people carry the overwhelming moral weight the priority ordering assumes?',
    'Not resolvable by data alone: it turns on contested axiological commitments (person-affecting views, social discount rates, population ethics); resolution arrives only as philosophical positions shift or institutions adopt explicit, inspectable weightings.',
    'Under person-affecting or steeply discounted views the claimed victim set thins dramatically, the protective function shrinks, and the arrangement''s extractive side dominates its classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(future_persons_moral_weighting, preference, 'The axiological premise determining whether the claimed victim set is real.').

omega_variable(
    authority_framing_underdetermination,
    'Does the arrangement''s authority rest in the argumentative doctrine (forecasting and alignment arguments adjudicated on demonstrated competence) or in the funding network that transmits and rewards the doctrine?',
    'Trace deference patterns: if rank-and-file adherence tracks independent assessment of argument quality, authority is expertise-grounded; if it tracks grant dependence and career incentive, authority is extraction-grounded and the commitment-system pattern changes accordingly.',
    'Under the network-as-authority framing the interpretive layer reads as drift-denial machinery rather than genuine anomaly absorption, shifting the commitment-system classification and strengthening extraction findings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_underdetermination, conceptual, 'CS-framing under-determination: doctrine-as-authority versus funding-network-as-authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__existential_risk_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(ai_r_tr_t0, observed).
narrative_ontology:measurement(ai_r_tr_t2, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement_basis(ai_r_tr_t2, observed).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_prioritization__existential_risk_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(ai_r_tr_t4, observed).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_prioritization__existential_risk_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement_basis(ai_r_tr_t6, observed).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_prioritization__existential_risk_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(ai_r_tr_t8, observed).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_prioritization__existential_risk_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(ai_r_tr_t10, observed).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_prioritization__existential_risk_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement_basis(ai_r_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(ai_r_be_t0, observed).
narrative_ontology:measurement(ai_r_be_t2, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement_basis(ai_r_be_t2, observed).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement_basis(ai_r_be_t4, observed).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 6, 0.54).
narrative_ontology:measurement_basis(ai_r_be_t6, observed).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(ai_r_be_t8, observed).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(ai_r_be_t10, observed).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement_basis(ai_r_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(ai_r_su_t0, observed).
narrative_ontology:measurement(ai_r_su_t2, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2, 0.33).
narrative_ontology:measurement_basis(ai_r_su_t2, observed).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement_basis(ai_r_su_t4, observed).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement_basis(ai_r_su_t6, observed).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(ai_r_su_t8, observed).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(ai_r_su_t10, observed).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement_basis(ai_r_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, near_term_harms_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'AI risk' decomposes into two structurally distinct constraints sharing one kernel (ai_risk_prioritization): this file (existential_risk_reading) and near_term_harms_reading. They differ in victim set, timescale, beneficiary structure, and therefore in ε; forcing both into one story would make ε observer-dependent, violating ε-invariance. Neither is strictly upstream of the other: they compete for the same pools of funding, talent, media attention, and legislative bandwidth, so each structurally influences the other's operating environment. The linkage here records that competition edge; the sibling file should carry the reciprocal edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
