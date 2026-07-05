% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Existential-Risk Framing of AI Risk Prioritization
 *   domain: AI Safety / Technology Governance / Risk Assessment
 *
 * SUMMARY:
 *   Since roughly the mid-2010s, a well-funded research and policy ecosystem
 *   has coalesced around the premise that advanced AI systems could become
 *   powerful enough to act catastrophically against human interests, and that
 *   this possibility warrants prioritizing alignment research, capability
 *   evaluations, and controls above other AI-related harms. This framing has
 *   attracted significant philanthropic capital, shaped hiring at frontier
 *   labs, and increasingly influences AI governance discourse. It coexists
 *   with — and often competes for the same scarce attention and funding as —
 *   a distinct research and advocacy tradition focused on documented
 *   present-tense algorithmic harms.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: agenda-setting beneficiary (institutional/arbitrage) — defines research priorities and collects funding/prestige
 *   - longtermist_funders: beneficiary (powerful/arbitrage) — directs capital toward the framing with no near-term accountability
 *   - algorithmically_harmed_communities: primary payer (powerless/trapped) — bears present-tense algorithmic harm while attention is diverted
 *   - near_term_ai_justice_researchers: payer (moderate/constrained) — competes for deprioritized funding and attention
 *   - future_humanity_speculative: invoked non-agent victim (powerless/analytical) — cannot corroborate or contest the claims made on its behalf
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.58).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.52).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "Existential-Risk Framing of AI Risk Prioritization").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "AI Safety / Technology Governance / Risk Assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, 'a9c6c544-b226-4a7c-8073-3f9d78596bb0').
narrative_ontology:cs_kernel_codification('a9c6c544-b226-4a7c-8073-3f9d78596bb0', distributed).
narrative_ontology:cs_authority_grounding('a9c6c544-b226-4a7c-8073-3f9d78596bb0', distributed).
narrative_ontology:cs_reading_relation('a9c6c544-b226-4a7c-8073-3f9d78596bb0', ai_risk_prioritization__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('a9c6c544-b226-4a7c-8073-3f9d78596bb0', foundational, expected_value_dominates_under_extinction_stakes).
narrative_ontology:cs_axiom_status(expected_value_dominates_under_extinction_stakes, holdable).
narrative_ontology:cs_axiom_grounding('a9c6c544-b226-4a7c-8073-3f9d78596bb0', expected_value_dominates_under_extinction_stakes, instrumental).
narrative_ontology:cs_axiom('a9c6c544-b226-4a7c-8073-3f9d78596bb0', secondary, future_persons_carry_moral_weight_comparable_to_present_persons).
narrative_ontology:cs_axiom_status(future_persons_carry_moral_weight_comparable_to_present_persons, holdable).
narrative_ontology:cs_axiom_grounding('a9c6c544-b226-4a7c-8073-3f9d78596bb0', future_persons_carry_moral_weight_comparable_to_present_persons, deontological).
narrative_ontology:cs_reference_frame('a9c6c544-b226-4a7c-8073-3f9d78596bb0', capability_control_precautionary_framework).
narrative_ontology:cs_drift_state('a9c6c544-b226-4a7c-8073-3f9d78596bb0', post_frontier_model_scaling_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('a9c6c544-b226-4a7c-8073-3f9d78596bb0', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, frontier_ai_labs_safety_teams).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, algorithmically_harmed_communities).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_ai_justice_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, future_humanity_speculative).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, misaligned_agi_extinction_hypothesis).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, alignment_research_tractability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set research agendas, conference programs, and funding criteria around the existential-threat framing. Publish the papers and forecasts that define what counts as a serious AI risk. Draw grants, prestige, and policy access from the premise that misalignment is the central threat; can pivot funding narratives as the field's attention shifts.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions, beneficiary).

% Philanthropic and venture capital sources that direct resources toward alignment research and capability-control advocacy. Gain reputational and ideological returns from association with civilization-scale stewardship; face no binding accountability if the predicted extinction risk fails to materialize on any observable timeline.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, beneficiary,
    powerful, civilizational, arbitrage, global).

% Employ the existential framing to justify continued scaling under a self-administered safety mandate, positioning their own labs as the responsible stewards best qualified to build the very systems they warn about. Benefit from regulatory capture that favors incumbents equipped to run 'alignment' programs over new entrants.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, frontier_ai_labs_safety_teams, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, frontier_ai_labs_safety_teams, agenda_setter).

% Experience present-tense algorithmic discrimination in hiring, lending, policing, and welfare administration. Their harms are documented and ongoing but compete for research funding, media attention, and regulatory bandwidth against the existential framing, which routinely characterizes their concerns as a lower-priority distraction from the 'real' threat.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, algorithmically_harmed_communities, payer,
    powerless, immediate, trapped, national).

% Study deployed-system harms — bias audits, labor displacement, surveillance — and compete for the same funding pools and conference slots as x-risk researchers. Report having grant applications and papers deprioritized when reviewers or funders treat existential risk as the field's central organizing question.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_ai_justice_researchers, payer,
    moderate, biographical, constrained, national).

% Invoked as the primary constituency whose survival is at stake, but has no present voice, cannot corroborate the threat model, and cannot object to how resources are allocated in its name. Its interests are entirely mediated by the institutions claiming to represent it.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_humanity_speculative, payer,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_prioritization__existential_risk_reading, future_humanity_speculative).

% Draft AI governance frameworks and must weigh competing claims about which risks warrant binding regulation. Take testimony from both existential-risk and near-term-harms camps; their allocation of regulatory attention and enforcement resources is shaped by whichever framing dominates the policy conversation at a given moment.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, ai_policy_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scarce technical talent, philanthropic capital, and policy attention around a shared hypothesis (that unaligned superintelligent systems pose catastrophic risk), enabling sustained multi-year research programs (interpretability, alignment theory, capability evaluations) that would be difficult to fund or staff without a unifying threat narrative.
% TRANSFER_FUNCTION: Moves research funding, regulatory attention, media coverage, and policy access away from near-term algorithmic harm interventions and toward long-horizon alignment research and capability-control advocacy; moves prestige and institutional legitimacy toward labs and institutes positioned as existential-risk stewards.
% ABSENT_VOICES: Communities currently harmed by deployed algorithmic systems (credit scoring, hiring algorithms, predictive policing) are rarely represented in x-risk governance forums; the 'future humanity' the framing claims to protect has, definitionally, no voice at all and cannot corroborate or dispute the threat model asserted on its behalf.
% DISAPPEARANCE_RATIONALE: X-risk institutions and funders would argue the world becomes catastrophically more exposed to unaligned AGI without dedicated alignment research. Near-term harms researchers and affected communities would argue that resources, attention, and regulatory capacity currently withheld from documented present-tense harms would become available, and that little alignment-specific technical progress would actually be lost since much safety-relevant work (robustness, interpretability) is dual-purpose. The two camps do not agree on the counterfactual.
% FOUNDING_PROBLEM: As AI capabilities advanced rapidly in the 2010s-2020s, researchers observed that future systems might become powerful enough to act in ways their designers neither intended nor could correct, motivating dedicated technical work on ensuring advanced AI systems remain controllable and aligned with human intentions.
% FOUNDING_PROBLEM_CORROBORATION: Some AI capability researchers outside the dedicated x-risk institutions (e.g., researchers focused on robustness and interpretability for engineering reasons, not existential motivation) corroborate that alignment-adjacent technical problems are real and current. However, no corroboration exists for the specific claim that extinction-level outcomes are the dominant expected harm relative to documented near-term harms — this claim is corroborated almost exclusively by the same institutions and funders that benefit from its centrality; independent AI ethics researchers and affected-community advocates dispute the relative prioritization, not the existence of alignment as a technical subfield.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects the diversion of scarce research funding, policy attention, and regulatory bandwidth away from documented, measurable near-term harms toward speculative long-horizon risk, without the diverted resources being clearly attributable to concrete safety gains achieved to date. Suppression (0.52) is moderate-to-substantial: the existential framing does not use overt coercion but does structurally marginalize near-term harms research through funding gatekeeping, conference programming, and rhetorical framing ('distraction from the real risk'). Theater ratio (0.40) captures that a meaningful share of alignment activity is genuine technical work (interpretability, evaluations) while another share functions as institutional legitimacy performance — safety branding by labs that continue to scale capabilities. Accessibility collapse (0.45) is moderate: near-term harms research persists as a viable field, it is not eliminated, only structurally disadvantaged in resource competition. Resistance (0.60) is substantial: near-term harms researchers, affected communities, and some policy actors actively contest the prioritization.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat, this is straightforward coordination: a real, serious risk requires dedicated attention and resources, and the field's growth reflects genuine problem recognition, not extraction. From the payer seats — communities experiencing algorithmic harm now, and researchers studying it — the same structure operates as an extraction of attention and funding legitimated by an unfalsifiable future harm, while their falsifiable, documented present harm competes at a structural disadvantage. The engine's per-seat computation should reflect this divergence: agenda-setter and beneficiary seats will likely compute closer to coordination-flavored, payer seats closer to extraction-flavored, given the same base structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutions and longtermist funders sit near the beneficiary end: they set the agenda, collect funding and prestige, and face minimal accountability if the predicted catastrophe is indefinitely deferred. Frontier lab safety teams occupy a dual position — beneficiaries of the framing's legitimizing cover, and partial agenda-setters who use the existential narrative to justify continued capability scaling under self-administered safety branding. Algorithmically harmed communities and near-term justice researchers sit near the target end: they bear real, present, measurable costs (discriminatory outcomes, displaced funding, deprioritized advocacy) while the resources that could address those costs are directed elsewhere. Future humanity is nominally the primary beneficiary of the entire arrangement's stated purpose, but as a non-agent entity with no capacity to corroborate or object, it functions structurally more like an unfalsifiable justification than an actual party — it cannot receive anything, it can only be invoked.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a genuine open technical question about controllability of advanced AI systems) remains at least partly live — this is not a pure zombie mandate. But the specific prioritization claim (that extinction risk should dominate resource allocation relative to documented near-term harms) is corroborated almost exclusively by the institutions and funders who benefit from that prioritization, which is the classic mandatrophy risk pattern: a claim whose only attestors are its own beneficiaries. Classifying this as tangled_rope rather than snare or mountain preserves the genuine coordination function (alignment research is not fake) while registering the asymmetric extraction (near-term harms researchers and affected communities pay through the same funding-and-attention structure) — collapsing it to either pure category would either launder the extraction as pure natural necessity or dismiss the real technical coordination problem entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_probability_estimate_contestation,
    'Is the probability of extinction-level AGI misalignment within the relevant timescale high enough to justify the described resource-allocation priority relative to documented near-term harms, or is the estimate itself contested/unfalsifiable within any actionable timeframe?',
    'No direct empirical resolution is available before the event either occurs or the timescale lapses; partial resolution could come from calibrated forecasting track records of x-risk-associated researchers, adversarial red-teaming of the threat models, or convergence/divergence of independent expert elicitation outside the x-risk-funded community.',
    'If the probability estimate is well-calibrated and substantial, the prioritization functions closer to genuine (if urgent) coordination; if the estimate is systematically inflated or unfalsifiable within actionable time, the prioritization functions closer to extraction dressed as prudence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_probability_estimate_contestation, empirical, 'Whether the extinction-risk probability justifies the resource allocation it drives.').

omega_variable(
    kernel_reading_sibling_delta,
    'This constraint is one reading (existential_risk_reading) of the ai_risk_prioritization kernel; the sibling near_term_harms_reading instantiates a structurally distinct constraint with a different victim set (presently harmed communities vs. speculative future humanity), different beneficiaries (near-term justice researchers/advocacy orgs vs. x-risk institutions/longtermist funders), and different timescale (immediate vs. 10-100 years). Where is the disagreement between readings actually located — is it a genuine empirical disagreement about probability and timescale, or an irreducible values disagreement about how to weight present certain harm against future uncertain harm?',
    'Decompose the disagreement into its empirical component (probability/timescale estimates, addressed by the omega above) and its normative component (population ethics weighting of present vs. speculative future persons, which is not resolvable by additional data). Document which component dominates in specific policy disputes.',
    'If the disagreement is primarily empirical, better forecasting and evaluation methodology could substantially narrow it. If primarily normative (a longtermist population-ethics commitment vs. a person-affecting near-term ethics commitment), no amount of additional data resolves it and the two readings will persist as genuinely coexisting positions rather than converging.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Where the existential vs. near-term readings'' disagreement is structurally located.').

omega_variable(
    future_humanity_representation_problem,
    'Can ''future humanity'' meaningfully function as a beneficiary/victim category in resource-allocation decisions when it has no present voice, cannot corroborate claims made on its behalf, and cannot be harmed or benefited in any way that is checkable before the fact?',
    'Philosophical and institutional analysis of proxy representation mechanisms for non-existent or future persons; comparison to analogous constructs (environmental trusteeship, intergenerational equity frameworks) that have developed accountability mechanisms despite representing absent parties.',
    'If no meaningful representation mechanism exists, claims made in the name of future humanity function as unfalsifiable licenses for whoever administers the claim, strengthening the case that this constraint has snare-adjacent features despite its genuine coordination core. If meaningful proxy mechanisms exist and are actually used, the beneficiary claim is more structurally grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_humanity_representation_problem, conceptual, 'Whether unrepresented future humanity can function as a legitimate structural beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__existential_risk_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_prioritization__existential_risk_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_prioritization__existential_risk_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_prioritization__existential_risk_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_prioritization__existential_risk_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_prioritization__existential_risk_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint (existential_risk_reading) and near_term_harms_reading form a two-member family decomposed from the colloquial single label 'AI risk prioritization.' They share a kernel (ai_risk_prioritization) but diverge on victim set, beneficiary set, and timescale — per the ε-invariance principle these could not be authored as a single constraint with a measurement parameter. This story influences its sibling by shaping which resource pool and policy attention the sibling's beneficiaries compete for; the relationship is bidirectional in practice but authored here as an outbound edge from this reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
