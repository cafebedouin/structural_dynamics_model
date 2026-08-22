% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Existential-Risk-Priority Reading of AI Risk Governance
 *   domain: AI governance / technology ethics / risk assessment
 *
 * SUMMARY:
 *   This story instantiates the existential-risk reading of the contested
 *   AI-risk-governance kernel: the claim that governance resources must be
 *   prioritized toward preventing superintelligence scenarios capable of
 *   annihilating or permanently curtailing humanity's potential. This is one
 *   of three readings of the same kernel (near_term_harms_reading,
 *   bridge_reading are separate constraint stories); this file authors only
 *   this reading's structure, beneficiaries, victims, and epsilon — it does
 *   not average over or hedge against the sibling readings. Under this
 *   reading, ε is authored high on speculative future-capability governance
 *   (control of frontier model access, compute governance, alignment research
 *   funding) and low on present algorithmic-bias remediation, because the
 *   reading's own internal logic treats near-term harms as lower
 *   expected-value targets for scarce governance attention.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.61).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.42).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "Existential-Risk-Priority Reading of AI Risk Governance").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "AI governance / technology ethics / risk assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, 'abc83e77-dc1f-44d4-b61d-4d8cfd84ea25').
narrative_ontology:cs_kernel_codification('abc83e77-dc1f-44d4-b61d-4d8cfd84ea25', distributed).
narrative_ontology:cs_authority_grounding('abc83e77-dc1f-44d4-b61d-4d8cfd84ea25', expertise).
narrative_ontology:cs_interpretation_layer_present('abc83e77-dc1f-44d4-b61d-4d8cfd84ea25').
narrative_ontology:cs_reading_relation('abc83e77-dc1f-44d4-b61d-4d8cfd84ea25', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('abc83e77-dc1f-44d4-b61d-4d8cfd84ea25', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('abc83e77-dc1f-44d4-b61d-4d8cfd84ea25', foundational, tail_risk_dominates_expected_value).
narrative_ontology:cs_axiom_status(tail_risk_dominates_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('abc83e77-dc1f-44d4-b61d-4d8cfd84ea25', tail_risk_dominates_expected_value, instrumental).
narrative_ontology:cs_axiom('abc83e77-dc1f-44d4-b61d-4d8cfd84ea25', foundational, irreversible_harm_warrants_present_sacrifice).
narrative_ontology:cs_axiom_status(irreversible_harm_warrants_present_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('abc83e77-dc1f-44d4-b61d-4d8cfd84ea25', irreversible_harm_warrants_present_sacrifice, deontological).
narrative_ontology:cs_reference_frame('abc83e77-dc1f-44d4-b61d-4d8cfd84ea25', precautionary_tail_risk_priority).
narrative_ontology:cs_drift_state('abc83e77-dc1f-44d4-b61d-4d8cfd84ea25', post_frontier_model_scaling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('abc83e77-dc1f-44d4-b61d-4d8cfd84ea25', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutes).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, frontier_ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_safety_credentialed_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, algorithmic_bias_affected_communities).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, gig_and_content_moderation_workers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, surveilled_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, near_term_harm_researchers_deprioritized).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, future_humanity).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, future_humanity).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__existential_risk_reading, longtermist_expected_value_calculus).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__existential_risk_reading, orthogonality_thesis_policy_relevance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets research agendas, funding priorities, and governance frameworks around superintelligence scenarios; drafts model evaluation standards and advises legislators and international bodies. Draws philanthropic and government funding tied specifically to existential-risk framing, and its institutional standing and funding pipeline depend on the framing continuing to dominate.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutes, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutes, beneficiary).

% Publicly commits to existential-risk mitigation (interpretability teams, adversarial red-teaming, alignment research) which functions simultaneously as genuine safety work and as a regulatory-capture and reputational shield that legitimizes continued frontier model deployment and forestalls near-term-harm-focused regulation of their existing products.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, frontier_ai_labs_claiming_safety_leadership, beneficiary,
    institutional, generational, arbitrage, global).

% Experience discriminatory lending, hiring, and policing algorithms deployed today. Advocacy resources, regulatory attention, and legislative bandwidth that could address these harms are redirected toward speculative superintelligence scenarios; they have no lobbying budget comparable to x-risk philanthropy and cannot exit the systems that score and sort them.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, algorithmic_bias_affected_communities, payer,
    powerless, immediate, trapped, local).

% Labor displacement and psychologically damaging moderation work generated by current AI deployment goes comparatively under-addressed in governance frameworks that treat these as secondary to civilizational risk; workers can change employers but not exit the underlying labor-market restructuring.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, gig_and_content_moderation_workers, payer,
    powerless, immediate, constrained, global).

% Subject to AI-enabled surveillance and predictive policing now in deployment; governance capacity spent on hypothetical AGI containment scenarios is capacity not spent on regulating present-day surveillance infrastructure that affects them directly and immediately.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, surveilled_populations, payer,
    powerless, immediate, trapped, national).

% Researchers and advocacy organizations working on bias, misinformation, and labor displacement compete for funding, conference space, and legislative attention against a well-resourced x-risk research ecosystem; many report grant rejections and agenda displacement traceable to the existential framing's dominance in funder and policymaker priorities.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, near_term_harm_researchers_deprioritized, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, near_term_harm_researchers_deprioritized, excluded).

% The nominal ultimate beneficiary of existential-risk mitigation — a hypothetical population whose continued existence and potential the framework claims to protect. Cannot participate in the governance process, cannot object to trade-offs made in its name, and cannot verify whether the resources spent on its behalf actually reduce risk or merely legitimize present-day institutional power.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, future_humanity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, future_humanity, payer).

% Career paths, publication venues, and prestige structures have formed specifically around alignment-as-control and adversarial testing framings; individual researchers can move between labs and institutes within this ecosystem but exiting the framing itself would mean abandoning accumulated professional capital.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_safety_credentialed_researchers, beneficiary,
    organized, biographical, mobile, global).

% Draft AI legislation and are lobbied intensively by both x-risk-framed and near-term-harm-framed advocacy coalitions; must allocate scarce legislative attention and technical staff capacity between the two framings, and their choices determine which victim set receives protective regulation first.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, legislators_and_regulators, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, legislators_and_regulators, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__existential_risk_reading, xrisk_research_institutes).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scarce global attention, philanthropic capital, and regulatory bandwidth around a single organizing question — could advanced AI systems cause irreversible civilizational harm — so that safety research, evaluation standards, and international coordination mechanisms (model registries, compute governance, red-teaming protocols) can be built before capabilities outrun oversight.
% TRANSFER_FUNCTION: Moves funding, legislative attention, media coverage, and technical talent away from present, measurable, unevenly-distributed harms (bias, labor displacement, surveillance) and toward speculative future-scenario research and governance infrastructure, benefiting institutions and researchers whose funding and prestige are tied to the existential framing.
% ABSENT_VOICES: Communities currently harmed by deployed algorithmic systems are rarely present in the rooms where existential-risk governance frameworks are drafted; near-term-harm researchers report being crowded out of funding calls and legislative hearings that have been reframed around AGI scenarios. Future humanity, the nominal beneficiary, cannot speak for itself and cannot audit whether the resources spent in its name are well spent.
% DISAPPEARANCE_RATIONALE: X-risk institutions and safety-leadership-claiming labs would say the world becomes catastrophically more exposed to uncontrolled superintelligence if this priority framework vanished — funding, evaluation standards, and international coordination mechanisms would collapse. Near-term-harm advocates would say the world changes very little for people currently harmed by deployed systems, and governance attention would simply redirect toward problems that are measurable today; some would say it improves for them. The disagreement is exactly the kernel contest this constraint is one reading of.
% FOUNDING_PROBLEM: As AI capabilities advanced rapidly with unclear scaling limits, a coalition of researchers and philanthropists argued that governance frameworks focused only on present, measurable harms would fail to anticipate a discontinuous jump to systems capable of large-scale, irreversible, potentially existential harm — and that by the time such harm became measurable, it would be too late to correct.
% FOUNDING_PROBLEM_CORROBORATION: X-risk institutions and safety-leadership labs attest the problem remains live and urgent, citing continued capability advances. Near-term-harm researchers and several independent AI ethics scholars (outside both the x-risk funding ecosystem and the labs it evaluates) attest that the speculative harm has not materialized on the timeline originally claimed, that present algorithmic harms are well-documented and ongoing, and that the framing's institutional entrenchment has outpaced any evidence resolving whether the founding problem is real, overstated, or a vehicle for deferring near-term accountability.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.61 by interval end) reflects a genuine coordination function — pooling attention and capital toward a real, if uncertain, tail risk — combined with an asymmetric transfer: the coordination is funded and staffed disproportionately by redirecting resources away from documented, currently-materializing harms toward speculative ones, and that redirection benefits a specific set of institutions and careers. Suppression (0.42) is moderate: near-term-harm advocates are not legally barred from speaking, but face a genuinely uneven playing field in funding calls, conference programming, and legislative access, which functions as soft suppression rather than coercive silencing. Accessibility collapse (0.35) is comparatively low — the near-term-harms and bridge readings remain live, visible, and actively argued in public discourse, unlike a genuine mountain. Resistance (0.55) is substantial: near-term-harm researchers and affected communities actively contest the priority allocation.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (xrisk_research_institutes), the arrangement reads as urgent, underfunded coordination against irreversible catastrophic risk. From the payer seats (algorithmic_bias_affected_communities, surveilled_populations), the same structure reads as an extraction of scarce governance attention justified by an unfalsifiable future harm, while documented present harm goes comparatively unaddressed. The engine computes these as structurally different seat classifications from the same authored data; this divergence is exactly what the kernel contest names, not an error in either reading.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk institutes, safety-leadership-claiming labs, and credentialed alignment researchers are structural beneficiaries: funding, prestige, and regulatory legitimacy flow toward them under this reading, pushing their derived directionality toward the low-d beneficiary end. Present-harm-affected communities, gig/moderation workers, and surveilled populations are structural targets: the resource redirection is a direct cost to them, pushing derived d toward the high-target end, compounded by their trapped or constrained exit options. Future humanity occupies an unusual dual seat — nominally the ultimate beneficiary, but with no mechanism to verify or object, which is why its exit_options is authored as analytical rather than any real option; this is the seat that the framing's legitimacy depends on rhetorically but that structurally cannot corroborate or contest the framing itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (discontinuous capability jump outrunning governance capacity) may remain live, dead, or contested depending on capability trajectories that have not yet resolved — hence founding_problem_status is authored contested rather than resolved either way. The tangled_rope classification (rather than snare) is deliberate: this reading does have a genuine coordination function (real uncertainty about capability trajectories, real value in evaluation standards and international coordination infrastructure) alongside the asymmetric extraction from present-harm communities — collapsing it to pure extraction would erase the genuine uncertainty the founding problem names; calling it a pure rope would erase the documented resource redirection and the deprioritized researchers' corroborated testimony.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expected_value_calculus_legitimacy,
    'Is the longtermist expected-value framing (small probability of astronomically large harm justifies large present resource allocation) a sound basis for governance priority-setting, or does it launder institutional resource capture through an unfalsifiable future harm calculation?',
    'Track record analysis of prior speculative-catastrophe governance priorities against realized outcomes; philosophical scrutiny of whether the probability estimates driving the calculus are epistemically grounded or arbitrarily chosen to justify predetermined funding conclusions.',
    'If the calculus is sound, the tangled_rope classification understates the genuine coordination value and the extraction reading is too harsh. If the calculus is a laundering mechanism, the classification may understate extraction and this reading functions closer to a snare wearing coordination language.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expected_value_calculus_legitimacy, conceptual, 'Whether longtermist expected-value reasoning is a legitimate governance basis or a rent-justifying framework.').

omega_variable(
    capability_timeline_uncertainty,
    'Does the underlying empirical claim — that transformative or superintelligent AI capability is near enough and discontinuous enough to warrant this reading''s resource allocation over present-harm remediation — hold up against the actual observed capability trajectory?',
    'Longitudinal tracking of capability benchmarks against predicted timelines made by the reading''s proponents; comparison of realized capability jumps to the discontinuity claims that ground the founding problem.',
    'If capability trajectories are gradual and containable through ordinary regulatory adaptation, the founding problem is closer to dead and the reading''s resource claim weakens substantially, strengthening the near_term_harms_reading''s competing priority claim. If a genuine discontinuity risk is validated, the founding problem is confirmed live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_timeline_uncertainty, empirical, 'Whether the empirical capability-jump premise underlying this reading is validated over time.').

omega_variable(
    reading_framing_underdetermination,
    'Is the choice to treat existential-risk-priority and near-term-harms-priority as separate, competing kernel readings (rather than as the bridge_reading''s single unified framework) itself a framing choice that advantages whichever coalition currently controls governance attention?',
    'Compare institutional outcomes (funding allocation, legislative time) under jurisdictions or periods that explicitly adopt a bridge/unified framework versus those that treat the two priorities as competing for the same scarce attention pool.',
    'If the bridge framework achieves comparable existential-risk mitigation without the near-term-harm displacement this reading''s metrics show, the competing-priorities framing (rather than either reading''s substantive content) is doing most of the extractive work — the extraction would be an artifact of false scarcity, not genuine tradeoff.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether framing the two concerns as competing (rather than unified) itself manufactures the extraction this reading exhibits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(ai_r_tr_t24, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(ai_r_be_t24, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 24, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 4, 0.31).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(ai_r_su_t24, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__existential_risk_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, bridge_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language 'AI risk governance priority' claim per the epsilon-invariance principle: existential_risk_reading (this file, high epsilon on speculative capability governance, tangled_rope), near_term_harms_reading (high epsilon on present algorithmic harms, separate victim/beneficiary structure), and bridge_reading (unified framework claim, likely lower net extraction if the coordination-without-displacement premise holds). All three share the same kernel_id (ai_risk_governance_priority) but instantiate structurally distinct constraints with different epsilon values, beneficiary sets, and victim sets — they are linked via affects_constraints rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
