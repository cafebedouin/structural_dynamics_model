% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: Existential-Risk-Priority Reading of AI Risk Governance
 *   domain: AI governance / technology ethics / risk assessment
 *
 * SUMMARY:
 *   This story instantiates the existential-risk reading of the contested 'AI
 *   risk governance priority' kernel: the claim that governance resources,
 *   regulatory attention, and institutional legitimacy should be organized
 *   primarily around preventing catastrophic or civilization-ending outcomes
 *   from advanced AI systems, on the grounds that these outcomes are
 *   irreversible and dwarf all other harms in expected magnitude. Under this
 *   reading, alignment-as-control research, adversarial red-teaming of
 *   frontier models, and international coordination frameworks aimed at
 *   AGI-scenario prevention become the central objects of governance. The
 *   structural effect is that a small set of well-resourced labs and research
 *   institutes — the ones capable of doing frontier capabilities work and
 *   therefore best positioned to also do frontier safety work — become the
 *   primary interlocutors for what 'AI safety' means, while populations
 *   currently experiencing algorithmic harm (biased lending and hiring
 *   systems, exploitative content-moderation labor, surveillance deployment
 *   in the Global South) become a lower governance priority whose costs are
 *   borne now while the benefits of x-risk prevention (if real) are
 *   indefinitely deferred and diffuse across all of future humanity.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: primary beneficiary (institutional/arbitrage) — gain funding, legitimacy, and a privileged seat in governance design
 *   - frontier_ai_labs_claiming_safety_leadership: primary beneficiary (institutional/arbitrage) — use x-risk framing to justify self-regulation and slow externally-imposed near-term compliance costs
 *   - present_day_algorithmically_harmed_populations: primary victim (powerless/trapped) — bear ongoing algorithmic harm while governance attention and funding are diverted upstream
 *   - global_south_ai_deployment_subjects: secondary victim (powerless/trapped) — subject to deployed systems with comparatively less governance scrutiny because attention is concentrated on frontier-lab AGI scenarios
 *   - displaced_content_moderation_and_gig_workers: secondary victim (moderate/constrained) — labor conditions receive less regulatory urgency than speculative capability races
 *   - national_regulators: agenda-setter (institutional/constrained) — must allocate finite legislative and enforcement bandwidth between the two governance tracks
 *   - future_humanity: nominal beneficiary/analytical construct (analytical/analytical) — the party whose interests the existential-risk framing claims to represent, with no present voice in the allocation debate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.62).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.48).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "Existential-Risk-Priority Reading of AI Risk Governance").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "AI governance / technology ethics / risk assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, 'fc813cd4-bc96-44ee-9e05-9638a08704bc').
narrative_ontology:cs_kernel_codification('fc813cd4-bc96-44ee-9e05-9638a08704bc', distributed).
narrative_ontology:cs_authority_grounding('fc813cd4-bc96-44ee-9e05-9638a08704bc', expertise).
narrative_ontology:cs_interpretation_layer_present('fc813cd4-bc96-44ee-9e05-9638a08704bc').
narrative_ontology:cs_reading_relation('fc813cd4-bc96-44ee-9e05-9638a08704bc', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc813cd4-bc96-44ee-9e05-9638a08704bc', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('fc813cd4-bc96-44ee-9e05-9638a08704bc', foundational, irreversibility_dominates_expected_value_calculus).
narrative_ontology:cs_axiom_status(irreversibility_dominates_expected_value_calculus, holdable).
narrative_ontology:cs_axiom_grounding('fc813cd4-bc96-44ee-9e05-9638a08704bc', irreversibility_dominates_expected_value_calculus, instrumental).
narrative_ontology:cs_axiom('fc813cd4-bc96-44ee-9e05-9638a08704bc', foundational, future_persons_carry_equal_moral_weight_to_present_persons).
narrative_ontology:cs_axiom_status(future_persons_carry_equal_moral_weight_to_present_persons, holdable).
narrative_ontology:cs_axiom_grounding('fc813cd4-bc96-44ee-9e05-9638a08704bc', future_persons_carry_equal_moral_weight_to_present_persons, deontological).
narrative_ontology:cs_reference_frame('fc813cd4-bc96-44ee-9e05-9638a08704bc', precautionary_catastrophic_risk_priority).
narrative_ontology:cs_drift_state('fc813cd4-bc96-44ee-9e05-9638a08704bc', post_frontier_model_scaling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fc813cd4-bc96-44ee-9e05-9638a08704bc', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, frontier_ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_safety_credentialed_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, present_day_algorithmically_harmed_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, global_south_ai_deployment_subjects).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, displaced_content_moderation_and_gig_workers).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__existential_risk_reading, long_termist_expected_value_reasoning).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__existential_risk_reading, capabilities_precede_alignment_urgency_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research institutes and think tanks focused on catastrophic-risk scenarios receive funding, media prominence, and privileged access to governance-drafting processes as a direct consequence of the existential-risk framing's dominance. They set much of the technical vocabulary ('alignment,' 'capability overhang,' 'AGI timelines') that governance bodies adopt, and can reposition their research agenda freely as the framing evolves.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, agenda_setter).

% Labs building the most capable systems also fund and publish the most visible safety research, allowing them to frame themselves as responsible stewards of existential risk while facing comparatively lighter binding regulation on present-day deployed-system harms (bias, labor practices, environmental cost). Their scale and resources let them move between jurisdictions and reframe compliance narratives at will.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, frontier_ai_labs_claiming_safety_leadership, beneficiary,
    institutional, generational, arbitrage, global).

% People denied credit, flagged by biased hiring or policing algorithms, or subject to automated content moderation decisions today. They cannot opt out of the systems governing them and depend on regulatory and advocacy attention that is structurally deprioritized relative to speculative future scenarios.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, present_day_algorithmically_harmed_populations, payer,
    powerless, immediate, trapped, national).

% Populations in lower-income countries subject to AI systems (surveillance, welfare-eligibility scoring, agricultural or financial automation) deployed with comparatively less governance scrutiny, in part because international AI governance forums are dominated by existential-risk agendas set by institutions headquartered in wealthy countries.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, global_south_ai_deployment_subjects, payer,
    powerless, immediate, trapped, regional).

% Workers whose labor conditions (traumatic content exposure, algorithmic wage-setting, opaque deactivation) are a direct present cost of AI deployment. They have some collective-action capacity (unionization efforts, worker organizing) but compete for regulatory attention against a governance agenda focused on frontier capability risk.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, displaced_content_moderation_and_gig_workers, payer,
    moderate, biographical, constrained, global).

% Legislators and regulatory bodies must allocate finite staff time, expertise, and political capital between drafting frontier-model safety requirements and enforcing present-harm remedies (algorithmic accountability, labor protection, data protection). Their choices are shaped by which framing dominates expert testimony and international coordination forums.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, national_regulators, agenda_setter,
    institutional, generational, constrained, national).

% The nominal ultimate beneficiary of existential-risk prevention — people who do not yet exist and cannot advocate, litigate, or organize for their own interests. Included for completeness as the constraint's stated rationale, not as an acting agent; the gap between this abstract beneficiary and the concrete institutional beneficiaries who administer the framing on its behalf is central to the tangled_rope reading.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, future_humanity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_governance_priority__existential_risk_reading, future_humanity).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__existential_risk_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely coordinates scarce technical, regulatory, and philanthropic attention around a class of catastrophic, potentially irreversible AI failure modes that no single actor could address alone and that plausibly requires cross-institutional and cross-national cooperation to mitigate.
% TRANSFER_FUNCTION: Moves governance bandwidth, research funding, and regulatory priority away from remediating demonstrated present-day algorithmic harms and toward speculative-capability safety research, with the transfer flowing from powerless, trapped present populations to institutionally powerful labs and research institutes positioned to define and study the speculative risk.
% ABSENT_VOICES: Present-day algorithmically harmed populations, Global South civil society groups, and displaced platform-labor workers would object that resource allocation ignores measurable ongoing harm in favor of institutions' preferred long-horizon framing; they are rarely seated at frontier AI-safety summits or in the drafting rooms for major AGI-governance frameworks, participating (if at all) through separate, lower-profile advocacy and labor-rights channels.
% DISAPPEARANCE_RATIONALE: X-risk institutions and frontier labs would argue the world becomes measurably more dangerous overnight (arrangements they consider load-bearing for catastrophic-risk prevention would vanish). Near-term-harm advocates and labor organizers would argue governance attention and funding would simply become available for present remedies, and the world affecting them directly would improve or be unchanged in risk terms. The disagreement over which counterfactual is correct is itself part of what the kernel contest is about.
% FOUNDING_PROBLEM: The founding problem was the recognition, beginning roughly in the 2010s AI-safety research community, that sufficiently capable AI systems pursuing misspecified or misaligned objectives could produce outcomes ranging from severe to civilizationally irreversible, and that by the time such systems existed it might be too late to correct course — motivating research and governance investment in advance of the risk materializing.
% FOUNDING_PROBLEM_CORROBORATION: Some independent AI safety researchers outside the major labs and x-risk institutes (including academic computer scientists and some AI ethics scholars not funded by frontier labs) attest the underlying capability trajectory is real and the concern remains live. Other independent voices — including fairness and accountability researchers, labor economists, and civil-society technology policy groups with no stake in x-risk funding — attest that whatever the abstract validity of the concern, its institutional prioritization has become disproportionate to its evidentiary basis relative to demonstrated present harms, and serves the reputational and funding interests of the institutions promoting it. No corroboration from a source with no stake in either framing was identified; the corroboration on both sides comes from parties with some institutional or professional interest in the outcome, which is itself worth noting.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate-high 0.62 by interval end: the reading does not extract wealth directly, but it extracts governance bandwidth, regulatory attention, and funding priority from populations experiencing demonstrated present harm, redirecting these toward speculative-scenario research disproportionately conducted by and benefiting a narrow set of well-capitalized labs and institutes. Suppression is moderate (0.48) — the mechanism is not coercive prohibition but institutional agenda-capture: near-term-harm advocates are not silenced, but their claims are persistently reframed as 'smaller stakes' in comparative-priority debates that the existential framing structurally wins by invoking irreversibility and scale. Theater ratio rises to 0.44 over the interval, reflecting a documented pattern where AGI-scenario governance commitments (voluntary safety pledges, adversarial testing showcases, international 'AI safety summits') increasingly substitute for binding present-harm remedies. Accessibility collapse is moderate (0.4): near-term-harms advocates and Global South stakeholders retain real institutional channels (litigation, direct regulation, worker organizing) that have not been foreclosed by the x-risk framing, distinguishing this from a Mountain-grade collapse. Resistance is moderate-high (0.55): fairness researchers, labor organizers, and Global South civil society groups actively contest the resource allocation, and this contestation is precisely what the bridge_reading and near_term_harms_reading constraints capture as separate, competing constraints in this family.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of an x-risk research institution, this arrangement reads as urgent, well-justified coordination against the single highest-stakes risk humanity faces — a rope. From the seat of a Global South community subject to an opaque, biased deployed system today, the same governance architecture reads as an extraction structure that indefinitely defers accountability for present, measurable harm in favor of speculative future scenarios that happen to center the expertise and legitimacy of already-powerful labs. The engine computes both seats from the same structural data; the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutions and frontier labs claiming safety leadership sit near the full-beneficiary end of directionality: the existential framing directly increases their funding, regulatory deference, and epistemic authority over what 'responsible AI development' means, while their arbitrage-grade exit options (they can reposition messaging, relocate operations, or pivot research agendas at will) further damp any extraction they might otherwise bear. Present-day algorithmically harmed populations and Global South deployment subjects sit near the full-target end: they are powerless, largely trapped (cannot opt out of algorithmic systems governing credit, employment, or state surveillance), and bear the opportunity cost of governance attention diverted upstream — a cost that is diffuse, hard to litigate, and rarely attributed causally to the x-risk framing itself, which is exactly what makes it durable. National regulators occupy an intermediate agenda-setter position: institutionally powerful but genuinely resource-constrained, they face real tradeoffs in staff-hours and legislative calendar space between the two tracks, which is the concrete mechanism underlying the tangled_rope's 'same structure, different payers' requirement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem this reading was built to address — the possibility that sufficiently capable AI systems could produce irreversible, civilization-scale harm — has not disappeared; if anything, capability trajectories over the interval have kept it live by most technical assessments. This blocks a clean mandatrophy verdict: unlike a pure zombie mandate, the existential-risk concern is not manifestly dead. What the tangled_rope classification captures instead is that the mandate, while live, has become the exclusive resourcing lens through which a set of institutions with a direct stake in that framing administer AI governance, at the structural expense of populations with demonstrated, present, non-speculative harm. The classification therefore does not say 'x-risk concern is fake' — it says the current arrangement bundles a genuine coordination problem (preventing catastrophic AI outcomes) with an asymmetric extraction structure (present victims subsidize deferred, uncertain future benefit, administered by parties who profit from the framing) that requires active enforcement (institutional gatekeeping of what counts as a legitimate AI safety concern) to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speculative_vs_demonstrated_harm_epistemics,
    'Is the probability mass assigned to civilization-ending superintelligence scenarios epistemically load-bearing, or is it an unfalsifiable prior that happens to justify redirecting governance resources toward the institutions best positioned to study it?',
    'Track record analysis of forecasting accuracy for capability-timeline predictions made by x-risk institutions over the interval; compare calibration against near-term-harm predictions made by algorithmic-fairness researchers over the same interval.',
    'If x-risk forecasts are systematically uncalibrated (persistent overconfidence or unfalsifiable horizon-pushing), the extraction reading strengthens — resources are being allocated on a speculative claim that primarily benefits the institutions making it. If forecasts prove calibrated, the coordination reading strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(speculative_vs_demonstrated_harm_epistemics, empirical, 'Whether existential-risk probability estimates are genuine signal or self-serving unfalsifiable priors.').

omega_variable(
    resource_diversion_causal_link,
    'Does resourcing the existential-risk framework causally displace funding, regulatory attention, and talent from present-harm mitigation, or is the relationship additive (a rising tide of AI-safety funding overall)?',
    'Longitudinal tracking of philanthropic and government AI-safety budgets, disaggregated by target (speculative capabilities vs. deployed-system harms), against control periods before the existential-risk framing achieved institutional prominence.',
    'A strong displacement finding supports the tangled_rope/victim reading (present-harm populations pay through opportunity cost); a weak or null finding weakens the victim declaration and pushes the constraint toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_diversion_causal_link, empirical, 'Whether x-risk prioritization structurally competes with near-term harm resourcing.').

omega_variable(
    kernel_framing_underdetermination,
    'Is ''AI risk governance'' genuinely one contested kernel with three readings, or are existential risk and near-term harm actually non-competing claims that only APPEAR to compete because they are forced to share a single finite governance/funding envelope?',
    'Examine whether jurisdictions or institutions that fund both tracks at scale (rather than trading off) produce measurably different outcomes than jurisdictions that treat the tracks as substitutes.',
    'If the tracks are genuinely non-competing under adequate resourcing, this reading''s tangled_rope classification (which depends on a victim group paying through the same structure) weakens toward rope; if resourcing is inherently zero-sum in practice, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s readings are truly mutually exclusive commitments or artifacts of scarce shared institutional bandwidth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(ai_r_tr_t24, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(ai_r_be_t24, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(ai_r_su_t24, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'AI risk governance priority,' per the ε-invariance principle: measuring this claim by (a) speculative-capabilities extraction yields a high ε (this story); measuring the same natural-language label by (b) present-deployed-harm extraction yields a structurally distinct, much lower-ε-on-capabilities / higher-ε-on-present-harm claim (near_term_harms_reading); and (c) a reading that denies the tradeoff exists at all (bridge_reading) has its own distinct ε profile. All three are linked via affects_constraints rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
