% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__integrated_reading, []).

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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: Dual-Mandate Norm Governing AI Alignment Priorities (Integrated Reading)
 *   domain: economic/political/technological
 *
 * SUMMARY:
 *   Within AI governance, the question of what alignment work must address —
 *   and in what order — is administered through a standing arrangement: the
 *   dual-mandate norm embedded in funder grant templates, risk-management
 *   frameworks, and lab policy, requiring that catastrophic-risk work and
 *   present-harm work proceed as complementary priorities rather than
 *   competitors. This file instantiates the integrated reading of that
 *   arrangement. The epsilon referent is the standing dual-mandate
 *   arrangement itself, assessed by this reading's own lights: the reading
 *   endorses the arrangement and still authors its real costs at moderate
 *   magnitude — residual harm persists on both fronts while the mandate
 *   prices partial attention as full response, compliance economies grow
 *   around the documentation burden, and single-focus agendas pay an
 *   opportunity tax. Per the epsilon-invariance principle, the colloquial
 *   debate ('what should alignment prioritize') decomposes into three
 *   structurally distinct constraints with distinct epsilon values, victim
 *   sets, and enforcement surfaces; the sibling readings are separate files
 *   linked through network.affects_constraints, and nothing in this file
 *   hedges epsilon across them.
 *
 * KEY AGENTS:
 *   - alignment_funders_and_standards_bodies: agenda setter (institutional/arbitrage) — writes the dual-mandate templates and can rewrite them next cycle
 *   - interdisciplinary_field_mediators: beneficiary (organized/identity_locked) — careers constituted by the bridge the mandate names
 *   - assurance_and_redteam_industry: beneficiary (organized/mobile) — collects the compliance economy the dual deliverables create
 *   - large_frontier_labs: dual-positioned beneficiary/payer (powerful/constrained) — moat gains offset real compliance outlays
 *   - present_marginalized_communities: primary target (powerless/trapped) — residual deployment harm, proceduralized voice
 *   - future_populations: primary target (powerless/trapped) — residual catastrophe exposure, represented only by proxies
 *   - single_focus_xrisk_specialists: target (organized/constrained) — marginal budgets diluted by the integrated gate
 *   - single_focus_fairness_specialists: target (organized/constrained) — budget lines absorbed into integrated programs
 *   - small_deployment_labs: target (moderate/constrained) — fixed dual-compliance costs weigh heaviest at small scale
 *   - global_south_affected_communities: excluded voice (powerless/trapped) — outside both canonical agendas and the consultations that set the balance
 *   - policy_regulators: analytical observer (institutional/analytical) — takes testimony from every seat, adjusts the documentation requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.47).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.38).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Dual-Mandate Norm Governing AI Alignment Priorities (Integrated Reading)").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "economic/political/technological").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, '0350a13f-1e44-4417-a2f5-da23ff830557').
narrative_ontology:cs_kernel_codification('0350a13f-1e44-4417-a2f5-da23ff830557', distributed).
narrative_ontology:cs_authority_grounding('0350a13f-1e44-4417-a2f5-da23ff830557', distributed).
narrative_ontology:cs_reading_relation('0350a13f-1e44-4417-a2f5-da23ff830557', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('0350a13f-1e44-4417-a2f5-da23ff830557', ai_alignment_priority__nearterm_harms_reading, influences).
narrative_ontology:cs_axiom('0350a13f-1e44-4417-a2f5-da23ff830557', foundational, harm_classes_complementary_not_competing).
narrative_ontology:cs_axiom_status(harm_classes_complementary_not_competing, holdable).
narrative_ontology:cs_axiom_grounding('0350a13f-1e44-4417-a2f5-da23ff830557', harm_classes_complementary_not_competing, empirically_contingent).
narrative_ontology:cs_axiom('0350a13f-1e44-4417-a2f5-da23ff830557', foundational, no_population_class_discounting).
narrative_ontology:cs_axiom_status(no_population_class_discounting, holdable).
narrative_ontology:cs_axiom_grounding('0350a13f-1e44-4417-a2f5-da23ff830557', no_population_class_discounting, deontological).
narrative_ontology:cs_reference_frame('0350a13f-1e44-4417-a2f5-da23ff830557', complementary_dual_portfolio_balance).
narrative_ontology:cs_drift_state('0350a13f-1e44-4417-a2f5-da23ff830557', post_framework_codification_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0350a13f-1e44-4417-a2f5-da23ff830557', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, interdisciplinary_field_mediators).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, assurance_and_redteam_industry).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, large_frontier_labs).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, present_marginalized_communities).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, future_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, small_deployment_labs).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, single_focus_xrisk_specialists).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, single_focus_fairness_specialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, present_marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, future_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, large_frontier_labs).
narrative_ontology:constraint_vindicates(ai_alignment_priority__integrated_reading, harm_complementarity_thesis).
narrative_ontology:constraint_vindicates(ai_alignment_priority__integrated_reading, dual_methodology_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Philanthropic funders, public research agencies, and standards bodies decide how AI-safety money and regulatory attention divide between loss-of-control research and deployment-harm mitigation. They wrote the both-priorities language into grant templates and risk frameworks, score proposals against it, and convene the proceedings where the balance is renegotiated. They can rewrite the templates next cycle at administrative cost.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, alignment_funders_and_standards_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Researchers, institute leads, and program officers whose professional standing rests on bridging the capability-risk and deployment-harm communities. Conferences, journals, and center budgets exist to host the bridge they personify. Moving into a single camp would mean rebuilding reputation and networks from a lower rung.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, interdisciplinary_field_mediators, beneficiary,
    organized, biographical, identity_locked, global).

% Audit firms, red-team boutiques, and evaluation vendors selling the documentation that both priority streams require. Every mandate naming two deliverables widens their addressable market, and their client rosters span both camps. They can rebrand services as standards shift.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, assurance_and_redteam_industry, beneficiary,
    organized, biographical, mobile, global).

% Frontier developers staff both red-team programs and impact-assessment teams and pay the resulting bills, which are small at their scale. Frameworks built around dual deliverables favor organizations that can field both benches, filtering out competitors who cannot. Regulatory and reputational exposure follows them across jurisdictions, so relocation is not a clean exit.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, large_frontier_labs, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, large_frontier_labs, payer).

% People subjected today to biased screening, surveillance, and automated denial. The dual mandate attaches audits, complaint channels, and fairness evaluations to deployment decisions, and those channels are real. Residual discriminatory harm persists nonetheless, and community demands tend to return as completed paperwork rather than changed outcomes. No one can opt out of being governed by deployed models.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, present_marginalized_communities, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, present_marginalized_communities, beneficiary).

% People who exist only if catastrophic loss-of-control outcomes are avoided. They hold no seat and act only through proxy advocates in the capability-risk camp. The dual mandate reserves a standing budget line for their interests; it also caps that line below what dedicated priority would fund, and the difference between promise and allocation lands on them alone.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_populations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, future_populations, beneficiary).

% Loss-of-control researchers whose grant applications are asked to add a present-harms component before they qualify. Marginal dollars they would spend on capability evaluations route to fairness audits instead. Dedicated x-risk organizations exist, but the largest funding pools sit behind the integrated gate.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, single_focus_xrisk_specialists, payer,
    organized, biographical, constrained, global).

% Deployment-harm researchers and auditors whose work is reframed as the near-term leg of someone else's portfolio. Budget lines they once owned now pass through integrated programs where capability-risk work takes the larger share. Standalone fairness shops survive on less money and thinner access to frontier models.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, single_focus_fairness_specialists, payer,
    organized, biographical, constrained, global).

% Small and mid-size companies shipping models must document safety evaluations and bias assessments to reach regulated markets. The fixed cost of running two compliance stacks weighs heaviest at their size; several have narrowed product lines or left regulated segments rather than staff both functions.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, small_deployment_labs, payer,
    moderate, biographical, constrained, global).

% Communities bearing data-labeling labor, linguistic exclusion, and outsourced content-moderation trauma. The loss-of-control conversation barely registers their present, and the audit conversation rarely reaches their jurisdictions. They hold no seat in the funder consultations where the balance is set.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, global_south_affected_communities, excluded,
    powerless, biographical, trapped, continental).

% Agencies implementing risk frameworks gather testimony from every other seat, commission comparisons of siloed versus combined oversight, and can tighten or loosen the dual-documentation requirements that put the both-priorities rule into practice.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, policy_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__integrated_reading, assurance_and_redteam_industry).
narrative_ontology:fixing_cost_class(ai_alignment_priority__integrated_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps both harm classes staffed, funded, and methodologically connected: prevents bifurcation into mutually neglectful camps, maintains shared evaluation infrastructure and incident-learning channels that serve both portfolios, and gives each camp a protected budget line against zero-sum capture by the other.
% TRANSFER_FUNCTION: Moves research funding, talent slots, institutional attention, and regulatory bandwidth into a balanced two-line portfolio: marginal dollars shift from pure capability-risk work toward deployment-harm mitigation relative to survival-first priority, while capability-risk investment is preserved relative to justice-only priority; dual-documentation costs move onto deploying companies, and compliance fees move to the assurance industry.
% ABSENT_VOICES: Global South communities affected by data extraction, linguistic exclusion, and moderation labor would object that both canonical agendas are drawn around US/UK/EU concerns and neither reaches their jurisdictions; they are outside the funder consultations where the balance is negotiated. Single-focus specialists who believe forced integration dilutes both agendas are present but outvoted at the gatekeeping layer. Neither group's objection currently alters the templates.
% DISAPPEARANCE_RATIONALE: If the dual mandate vanished overnight, funding would consolidate into whichever silo captured the vacated gatekeeping — capability-risk portfolios under lab-and-funder coalitions or justice portfolios under civil-society coalitions; shared evaluation infrastructure built for both audiences would fragment; labs would drop whichever compliance stream lost its enforcement hook; and the two harm classes would again be argued against each other rather than resourced together.
% FOUNDING_PROBLEM: Early AI safety and ethics discourse split into two alarm streams — loss-of-control scenarios and discriminatory deployment — each claiming the other distracted from the real problem. Funders faced recurring zero-sum budget fights, institutions were forced to pick sides, and both harm classes went under-addressed during the split.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by incident documentation spanning both harm classes (deployment-discrimination case records and documented capability incidents), by regulatory risk assessments that cite both risk classes independently of either advocacy camp, and by bibliometric accounts of the field's pre-integration fragmentation. No corroborating source attests the founding problem is dead; the 'dead' claim circulates only inside the two single-focus camps as advocacy.
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__integrated_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.47) rather than low because the mandate's costs are real and asymmetric: residual harm persists on both protected fronts, the documentation economy grows, and single-focus agendas pay opportunity taxes — but it is capped well below snare levels because the mandate demonstrably delivers protection on both fronts that neither silo would provide alone. Suppression (0.38) reflects funder and reviewer gatekeeping against single-focus proposals while siloed organizations remain legally free to exist; suppression is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by the engine, through directionality and scope. Theater_ratio (0.31) captures the growing share of balance-as-reporting: responsible-AI documents that name both priorities while resourcing neither decisively. Accessibility_collapse is low (0.35) because the sibling readings remain fully live alternatives — understanding this arrangement does not close off the survival-first or justice-first positions. Resistance (0.58) is high because both camps actively contest dilution and affected communities contest proceduralization. The measurement series run on one shared seven-point grid so every tracked metric is authored at every examined time point; the suppression_requirement series is authored deliberately because the story traces enforcement-capacity change — the interval spans the build-out from voluntary balance norms to codified dual-documentation requirements, and that hardening is the dynamic under study, not noise around a static scalar. On the receipt surface: gain_flow names assurance_and_redteam_industry because the compliance dollars the dual deliverables generate demonstrably land there; fixing_cost is 'cheap' because the agenda setters can amend or dissolve the templates at administrative cost — though adequate repair (closing the residual-harm gap on both fronts) is a separate and unfunded question, which is why removal being cheap does not make the arrangement benign.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda_setter seat the arrangement is stewardship: a portfolio discipline that keeps both harm classes funded and talking. From the payer seats it divides further: single-focus specialists experience dilution of their marginal dollar, small labs experience a fixed compliance wall, and the two moral-patient classes experience partial protection priced as full response — the highest-directionality seats in the story. From the beneficiary seats it divides again: mediators experience the mandate as their professional home, the assurance industry as a market, and large labs as a manageable bill that conveniently filters smaller competitors. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Present_marginalized_communities and future_populations are declared victims with trapped exit and no seat: they sit nearest the full-target end — the mandate extracts from them in the specific sense that residual harm persists while their claims are processed rather than fully resourced. Small labs and both single-focus specialist camps are declared victims with constrained exit: high directionality, moderated slightly by their ability to migrate to siloed organizations. Mediators and the assurance industry are declared beneficiaries with low directionality; the mediators' identity_locked exit pushes them toward durable defense of the frame. Large_frontier_labs are the correction case: the derivation reading the beneficiary declaration alone would land them near the beneficiary pole, but their actual position nets moat gains against compliance bills they genuinely pay and safety work they genuinely fund, so a directionality_override sets the powerful atom to 0.3 — mid-range, reflecting genuine dual positioning. No other overrides are used; the beneficiary/victim declarations plus exit options produce accurate relationships for every remaining seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a field bifurcating into two mutually neglectful camps while both harm classes went under-addressed — remains live, so no mandatrophy is declared. The classification guards against both available mislabels. Sold as pure coordination ('balance helps everyone'), the arrangement hides its asymmetries: the tax on single-focus agendas, the compliance wall for small labs, and the residual harm borne by the very populations the balance claims to serve — the victim declarations force those into the open. Sold as pure extraction ('both-and means neither'), the framing erases the genuine coordination good: anti-bifurcation, protected budget lines on both fronts, and shared evaluation infrastructure that neither silo would maintain. Tangled_rope holds both truths. The mandatrophy watch-item is conditional: if the complementarity thesis fails empirically (see omega complementarity_empirical_status), the mandate begins to outlive its function while its enforcement machinery persists — at that point the arrangement drifts toward inertia maintained by the identities and revenue streams attached to it, and the classification should be revisited.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of kernel ai_alignment_priority (the integrated_reading); how would the classification shift if instantiated under the sibling readings?',
    'Compile and compare all three reading files of the kernel; rerun classification with each sibling''s victim and beneficiary sets substituted.',
    'Under the existential_risk_reading, present-harm spending reads as diversion from survival work and the victim set narrows toward future populations; under the nearterm_harms_reading, capability-risk spending reads as extraction from justice and the victim set narrows toward present marginalized groups. Epsilon and type are reading-indexed over a shared referent; cross-reading averages are meaningless.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Reading-indexed classification over a shared kernel; sibling files carry the other instantiations.').

omega_variable(
    complementarity_empirical_status,
    'Is the complementarity thesis empirically true — do integrated portfolios actually reduce both harm classes, or does forced balance dilute effort on each?',
    'Longitudinal comparison of incident rates on both harm classes across siloed versus integrated organizations; natural experiments where dual mandates were dropped or tightened.',
    'If dilution dominates, the coordination half of the arrangement weakens, the extraction share rises, and the structure is pressured toward pure extraction; if complementarity holds, the coordination claim is vindicated and measured extraction reads increasingly as overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_empirical_status, empirical, 'Whether the dual mandate''s coordinating premise survives contact with outcome data.').

omega_variable(
    future_population_proxy_fidelity,
    'Future populations hold no seat and act only through proxy advocates; is the proxy faithful to the interests it claims to represent?',
    'Compare proxy-stated priorities against independent philosophical and decision-theoretic work on future-person interests; test whether proxy positions correlate with catastrophism-funding salience rather than with the underlying interest claims.',
    'If proxies systematically skew, the future_populations entry partly covers researcher-agenda rents, the mandate''s protective claim on that front weakens, and effective extraction on the arrangement rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_population_proxy_fidelity, empirical, 'Whether representation-by-proxy for future populations is faithful or self-serving.').

omega_variable(
    dual_compliance_incumbent_moat,
    'Does the dual-documentation burden function as an incumbent moat rather than a neutral safety requirement?',
    'Measure compliance cost as a share of revenue across firm sizes; track small-lab attrition and product-line narrowing after framework mandates bind in regulated markets.',
    'If moat effects dominate, large_frontier_labs'' beneficiary position hardens, suppression of entrants rises, and the arrangement skews toward extraction on new market participants while the safety rationale thins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_compliance_incumbent_moat, empirical, 'Whether dual-compliance costs concentrate competitive advantage in incumbents.').

omega_variable(
    mediator_identity_lock,
    'Is the mediator seat''s defense of the integrated frame epistemic, or fused with the bridge-role professional identity that the frame constitutes?',
    'Track mediator positions and career migrations when bridge funding contracts; examine departure interviews for whether the frame is abandoned or defended as identity.',
    'If identity-fused, mediator testimony systematically overstates coordination benefits, and their beneficiary seat carries a hidden enforcement function the structural data alone would not reveal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mediator_identity_lock, empirical, 'Epistemic versus identity-fused commitment among the field''s bridge role.').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel best framed as a resource-allocation norm, or as a legitimacy-maintenance arrangement for the mediating institutions that administer the balance?',
    'Test whether allocation outcomes track the stated balance criteria or track administrator-institution growth; ask whether identical allocations would have arisen without the integrated frame''s gatekeeping.',
    'Under the mediator-authority framing, authority_grounding shifts toward extraction, the agenda_setter seat''s directionality rises, and the distributed-authority reading adopted in cs_structure would be revised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Two coherent framings of the same arrangement yield different commitment-system classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aip_integrated_tr_t0, ai_alignment_priority__integrated_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(aip_integrated_tr_t4, ai_alignment_priority__integrated_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(aip_integrated_tr_t8, ai_alignment_priority__integrated_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(aip_integrated_tr_t12, ai_alignment_priority__integrated_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(aip_integrated_tr_t16, ai_alignment_priority__integrated_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(aip_integrated_tr_t20, ai_alignment_priority__integrated_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(aip_integrated_tr_t24, ai_alignment_priority__integrated_reading, theater_ratio, 24, 0.31).

% Extraction over time
narrative_ontology:measurement(aip_integrated_be_t0, ai_alignment_priority__integrated_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(aip_integrated_be_t4, ai_alignment_priority__integrated_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(aip_integrated_be_t8, ai_alignment_priority__integrated_reading, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(aip_integrated_be_t12, ai_alignment_priority__integrated_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(aip_integrated_be_t16, ai_alignment_priority__integrated_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(aip_integrated_be_t20, ai_alignment_priority__integrated_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(aip_integrated_be_t24, ai_alignment_priority__integrated_reading, base_extractiveness, 24, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(aip_integrated_su_t0, ai_alignment_priority__integrated_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(aip_integrated_su_t4, ai_alignment_priority__integrated_reading, suppression_requirement, 4, 0.24).
narrative_ontology:measurement(aip_integrated_su_t8, ai_alignment_priority__integrated_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(aip_integrated_su_t12, ai_alignment_priority__integrated_reading, suppression_requirement, 12, 0.32).
narrative_ontology:measurement(aip_integrated_su_t16, ai_alignment_priority__integrated_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(aip_integrated_su_t20, ai_alignment_priority__integrated_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(aip_integrated_su_t24, ai_alignment_priority__integrated_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__nearterm_harms_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'AI alignment priorities' covers three structurally distinct claims with distinct epsilon values, victim sets, and enforcement surfaces. This file (integrated_reading, moderate epsilon, dual victim set spanning present and future populations) sits between its siblings: the existential_risk_reading concentrates the victim set on future populations and treats present-harm spending as diversion; the nearterm_harms_reading concentrates it on present marginalized groups and treats capability-risk spending as displacement. The upstream/downstream structure runs from this reading toward nearterm_harms_reading (codified dual mandates channel justice-first work into integrated institutions) while coexisting with existential_risk_reading (which persists largely outside integrated institutions). Each file links the other two through network.affects_constraints; no file averages epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__integrated_reading, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
