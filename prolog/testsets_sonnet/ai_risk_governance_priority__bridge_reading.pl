% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__bridge_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: AI Risk Governance: Bridge/Unified-Framework Reading
 *   domain: AI governance/technology ethics/risk assessment
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested 'AI risk governance
 *   priority' kernel: the bridge/unified-framework reading, which holds that
 *   present harms and existential risks are structurally entangled and must
 *   be addressed through unified institutional frameworks rather than treated
 *   as competing priorities. This is a distinct constraint from the
 *   existential_risk_reading (which prioritizes superintelligence-scenario
 *   prevention) and the near_term_harms_reading (which prioritizes present
 *   documented harms to marginalized populations) — each of those is a
 *   separate story with its own epsilon, beneficiary/victim structure, and
 *   classification. The bridge reading has a real coordination function
 *   (avoiding zero-sum competition between camps for governance attention)
 *   but also generates a concentrated beneficiary class of cross-field
 *   brokering institutions and individuals whose funding and influence depend
 *   on maintaining the appearance of successful integration, while both
 *   victim populations (present marginalized groups; future humanity as
 *   represented by dedicated technical safety work) bear diffuse costs from
 *   resource redirection.
 *
 * KEY AGENTS:
 *   - bridging_institutions: agenda_setter (institutional/arbitrage) — administers unification criteria, captures convening power
 *   - cross_field_broker_researchers: beneficiary (organized/mobile) — small population capturing outsized funding/visibility
 *   - present_marginalized_populations: payer (powerless/trapped) — bear present algorithmic harms, immediate horizon
 *   - future_humanity_proxies: payer (powerless/trapped) — represented only by proxy, civilizational horizon
 *   - narrow_specialist_researchers_sidelined_by_integration_mandates: payer (moderate/constrained) — bear integration compliance costs
 *   - existential_risk_specialists: excluded (moderate/constrained) — would argue for undiluted focus
 *   - near_term_harms_advocates: excluded (moderate/constrained) — would argue for undiluted focus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.47).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.38).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "AI Risk Governance: Bridge/Unified-Framework Reading").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "AI governance/technology ethics/risk assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, '3c306c78-5362-45b0-8e34-e5c7b42c8549').
narrative_ontology:cs_kernel_codification('3c306c78-5362-45b0-8e34-e5c7b42c8549', distributed).
narrative_ontology:cs_authority_grounding('3c306c78-5362-45b0-8e34-e5c7b42c8549', distributed).
narrative_ontology:cs_reading_relation('3c306c78-5362-45b0-8e34-e5c7b42c8549', ai_risk_governance_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('3c306c78-5362-45b0-8e34-e5c7b42c8549', ai_risk_governance_priority__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('3c306c78-5362-45b0-8e34-e5c7b42c8549', foundational, harms_are_non_mutually_exclusive_and_structurally_entangled).
narrative_ontology:cs_axiom_status(harms_are_non_mutually_exclusive_and_structurally_entangled, holdable).
narrative_ontology:cs_axiom_grounding('3c306c78-5362-45b0-8e34-e5c7b42c8549', harms_are_non_mutually_exclusive_and_structurally_entangled, empirically_contingent).
narrative_ontology:cs_axiom('3c306c78-5362-45b0-8e34-e5c7b42c8549', secondary, unified_institutional_frameworks_are_necessary_for_adequate_governance).
narrative_ontology:cs_axiom_status(unified_institutional_frameworks_are_necessary_for_adequate_governance, holdable).
narrative_ontology:cs_axiom_grounding('3c306c78-5362-45b0-8e34-e5c7b42c8549', unified_institutional_frameworks_are_necessary_for_adequate_governance, instrumental).
narrative_ontology:cs_reference_frame('3c306c78-5362-45b0-8e34-e5c7b42c8549', pre_bifurcation_integrated_ai_ethics_discourse).
narrative_ontology:cs_drift_state('3c306c78-5362-45b0-8e34-e5c7b42c8549', post_2023_camp_polarization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3c306c78-5362-45b0-8e34-e5c7b42c8549', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, cross_field_broker_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, funders_of_integrated_safety_ethics_programs).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_humanity_proxies).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, narrow_specialist_researchers_sidelined_by_integration_mandates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, ai_safety_ethics_funders).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__bridge_reading, harms_are_structurally_entangled_not_mutually_exclusive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms of what counts as legitimate 'unified' AI risk research — grant calls, conference tracks, cross-disciplinary fellowships. Administers the framework that requires present-harm and existential-risk work to be presented as integrated. Captures funding, convening power, and citation centrality disproportionate to its numeric size (the small set of cross-field papers/institutions accounting for most cross-field links). Can pivot framing language as funder priorities shift, giving it durable exit options unavailable to specialists downstream.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, bridging_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, bridging_institutions, beneficiary).

% A small number of individuals who publish and speak across both the near-term-harms and existential-risk communities. They accrue outsized visibility, funding access, and agenda-setting influence because the bridge framework needs legible translators. Their careers are structurally advantaged by the unification mandate even though most researchers in either home field never cross over.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, cross_field_broker_researchers, beneficiary,
    organized, biographical, mobile, global).

% Bear documented, currently-occurring harms from deployed AI systems (algorithmic bias in lending/hiring/policing, surveillance, labor displacement). Under the bridge framework, resources and urgency attention that could address these harms directly are partially redirected toward long-horizon integration work whose payoff for their immediate situation is diffuse and delayed. They have no practical exit from the systems harming them and no seat in the framework-setting process.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, present_marginalized_populations, payer,
    powerless, immediate, trapped, national).

% Cannot advocate for themselves; represented only through proxy institutions and researchers who claim to speak for long-term existential stakes. Under the bridge reading, existential-risk-specific technical work (e.g., narrow alignment research) is diluted by mandates to demonstrate near-term relevance, potentially under-resourcing the highest-stakes low-probability scenarios that a dedicated existential framework would prioritize.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_humanity_proxies, payer,
    powerless, civilizational, trapped, universal).

% Researchers doing deep, non-bridging work in either camp (pure bias auditing, or pure technical alignment theory) find grant panels and journals increasingly require them to gesture at the other camp's concerns to be fundable or publishable. This raises their transaction costs and can crowd out depth in favor of legible cross-references, without proportionate benefit to their actual research questions.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, narrow_specialist_researchers_sidelined_by_integration_mandates, payer,
    moderate, biographical, constrained, national).

% Philanthropic and government funders who have adopted the unified-framework language in grant criteria, partly because it hedges against accusations of neglecting either constituency. They fund integrated programs that depend on the small broker population continuing to exist and perform the integration role; if brokers exit or burn out, the funding structure has no distributed fallback.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, ai_safety_ethics_funders, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, ai_safety_ethics_funders, payer).

% Draw on bridge-framework outputs to draft AI regulation that claims to address both dimensions. They evaluate whether the unified framing produces workable rules or produces vague omnibus language that under-serves both the immediate victims and the long-horizon risks it claims to cover.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, policy_makers_and_regulators, observer,
    institutional, generational, analytical, national).

% Would argue the bridge framework dilutes focus and resources away from the most severe tail risks by forcing constant near-term legibility. Their objection is registered in academic debate but rarely shapes the funding criteria set by bridging institutions and funders.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, existential_risk_specialists, excluded,
    moderate, civilizational, constrained, global).

% Would argue the bridge framework legitimizes continued underinvestment in urgent present-day remediation by allowing institutions to claim coverage of 'both' concerns while structurally under-resourcing either. Present in some policy conversations but structurally outnumbered by well-funded bridge-framework advocates in grant panels.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, near_term_harms_advocates, excluded,
    moderate, immediate, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__bridge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves a real coordination problem: without some bridging mechanism, near-term-harms researchers and existential-risk researchers compete for the same finite pool of AI governance attention, funding, and policy bandwidth as if their concerns were zero-sum, when many technical and institutional interventions (robustness, auditability, accountable deployment pipelines) plausibly serve both timescales at once.
% TRANSFER_FUNCTION: Moves funding, convening authority, and policy legitimacy toward a small set of cross-field brokering institutions and individuals, and away from both (a) narrowly-focused near-term harm remediation that could otherwise capture that funding directly and (b) narrowly-focused existential-risk technical research that could otherwise capture it directly.
% ABSENT_VOICES: Present marginalized populations bearing algorithmic harms today are not in the room when 'unified framework' criteria are set by funders and bridging institutions; neither are the representatives who would advocate purely for long-horizon existential technical work. Both excluded camps object in academic literature but neither controls the funding criteria.
% DISAPPEARANCE_RATIONALE: Bridging institutions and funders argue that without the unified framework, resources would fragment and duplicate, and cross-cutting interventions (like governance of frontier model deployment) would go unaddressed by either silo. Excluded specialist camps on both sides argue the world would in fact rearrange for the better in their own domain — near-term advocates believe remediation funding would flow faster without integration overhead; existential specialists believe technical safety research would regain focus. The verdict genuinely depends on which counterfactual funding allocation one credits.
% FOUNDING_PROBLEM: AI governance discourse in the mid-2020s had bifurcated into two largely non-communicating camps — one focused on documented present-day algorithmic harms to marginalized groups, one focused on speculative catastrophic/existential risk from advanced AI — each accusing the other of misallocating scarce attention and resources, producing fragmented, sometimes contradictory policy recommendations to legislators.
% FOUNDING_PROBLEM_CORROBORATION: Independent policy analysts and legislative staffers outside both research camps corroborate that fragmentation was a real problem circa 2023-2025 (cited in multiple parliamentary and congressional testimony transcripts as a reason lawmakers received conflicting guidance). However, those same outside observers are split on whether the bridge framework actually resolved the fragmentation or merely relocated authority to a new brokering class without closing the underlying resource-allocation dispute — no fully outside-the-benefiting-parties source affirms the founding problem is now solved.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__bridge_reading, 0.47, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.47 at interval end) reflecting genuine coordination value diluted by resource capture toward a narrow broker class rather than distributed benefit to either camp's core constituencies. Suppression is moderate-low (0.38) because the framework operates mainly through funding-criteria gatekeeping and citation/legitimacy pressure rather than hard coercion — specialist researchers face raised transaction costs, not prohibition. Theater ratio rises to 0.42 over the interval, reflecting a growing tendency for grant applications and policy documents to perform cross-field integration language without correspondingly deep cross-field methodological work — a Goodhart-style substitution where 'mentions both camps' substitutes for genuinely unified analysis. All three tracked metrics share one time grid (T=0 through T=24, seven points) per the alignment rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Bridging institutions and cross-field brokers sit near the beneficiary end: they set the integration criteria and capture disproportionate funding/visibility from performing the bridge role, with mobile-to-arbitrage exit options letting them redefine the framework as funder priorities shift. Both victim populations sit near the target end but for different structural reasons: present marginalized populations are trapped by immediate exposure to deployed systems and have no voice in framework-setting; future humanity proxies are trapped by the impossibility of self-representation and depend entirely on advocates whose incentives are partially captured by the bridge framework's own legitimacy needs. Narrow specialists occupy an intermediate position — moderate power, constrained but not trapped exit, paying compliance costs rather than being extracted from directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The bridge framework's founding problem (fragmented, competing governance camps producing contradictory legislative guidance) was real and is corroborated by outside legislative-staff testimony. The classification as tangled_rope rather than pure rope or pure snare reflects that the coordination function has not fully atrophied — cross-cutting technical work (e.g., model evaluation infrastructure serving both bias auditing and catastrophic-risk assessment) genuinely benefits from integration. But the framework's active maintenance requires enforcement (funding criteria, editorial gatekeeping) that concentrates gains in a narrow broker population disproportionate to the coordination problem's actual size, which is the extraction signature layered on top of the genuine coordination core. Declaring this tangled_rope rather than rope prevents mislabeling a partially-captured framework as pure coordination; declaring it tangled_rope rather than snare prevents erasing the real cross-cutting technical value the bridge framework does produce.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bridge_reading_kernel_position,
    'Is the bridge/unified-framework reading a genuinely superior synthesis of the existential and near-term concerns, or a third distinct political-institutional claim that primarily benefits a narrow class of cross-field brokers rather than resolving the underlying resource-allocation dispute between the two camps?',
    'Track whether funding and policy outcomes attributable to bridge-framework advocacy demonstrably improve outcomes for BOTH present-harm remediation and existential-risk technical research relative to counterfactual dedicated-track funding, over a multi-year window. If broker-mediated funding underperforms dedicated tracks on both dimensions simultaneously, the bridge reading functions primarily as institutional capture rather than synthesis.',
    'If the bridge reading is genuine synthesis, its tangled_rope classification understates its coordination value and it may deserve reclassification toward rope. If it is primarily broker capture, the classification may need to move toward snare, with bridging institutions as the sole clear beneficiary and both camps as victims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bridge_reading_kernel_position, conceptual, 'Whether unification is genuine synthesis or a captured intermediary claim — the core committer-structure ambiguity of this kernel reading.').

omega_variable(
    sibling_reading_resource_delta,
    'What would the resource and attention allocation look like under the existential_risk_reading or near_term_harms_reading instead, and does the bridge reading''s resource flow toward broker institutions represent a net loss to both siblings'' preferred allocations?',
    'Comparative analysis of grant allocation data across jurisdictions/funders that have adopted bridge-style unified criteria versus those that maintain separate dedicated tracks for existential risk and near-term harms.',
    'If dedicated-track jurisdictions show better outcomes on their respective dimension without materially worse cross-cutting failures, this weakens the coordination-function claim underlying the tangled_rope classification and strengthens a reading of the bridge framework as primarily rent-seeking by broker institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_resource_delta, empirical, 'Comparative resource-allocation test between this reading and its two siblings.').

omega_variable(
    broker_fragility_ambiguity,
    'Given that the bridge framework''s coordination benefit depends on a small number of broker actors (the 5% of papers/institutions carrying 85% of cross-field links) rather than distributed collaboration, what happens to both present-harm and existential-risk resource flows if key broker actors exit, burn out, or are captured by one side?',
    'Track broker population turnover and whether integration funding criteria persist or collapse following departure of key brokering individuals/institutions.',
    'High fragility (framework collapses without specific brokers) supports reading the bridge framework as a personalistic capture structure rather than a durable coordination mechanism, pushing classification toward snare or piton depending on whether performative integration language persists after functional brokering capacity is lost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broker_fragility_ambiguity, empirical, 'Structural fragility of the bridge coordination function given its dependence on a narrow broker population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_governance_priority__bridge_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_governance_priority__bridge_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_governance_priority__bridge_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_governance_priority__bridge_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__bridge_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(ai_r_tr_t24, ai_risk_governance_priority__bridge_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_governance_priority__bridge_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_governance_priority__bridge_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_governance_priority__bridge_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_governance_priority__bridge_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__bridge_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(ai_r_be_t24, ai_risk_governance_priority__bridge_reading, base_extractiveness, 24, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_governance_priority__bridge_reading, suppression_requirement, 4, 0.26).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_governance_priority__bridge_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_governance_priority__bridge_reading, suppression_requirement, 12, 0.32).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_governance_priority__bridge_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__bridge_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(ai_r_su_t24, ai_risk_governance_priority__bridge_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__bridge_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the ai_risk_governance_priority kernel. existential_risk_reading treats prevention of catastrophic/existential AI scenarios as the overriding priority (different victim set: primarily future humanity, different beneficiary set: dedicated technical safety institutions). near_term_harms_reading treats present documented harms to marginalized populations as the overriding priority (different victim set: primarily present marginalized groups, different beneficiary set: dedicated harm-remediation advocacy institutions). This bridge_reading claims both are correct simultaneously and requires unified frameworks, producing a third distinct epsilon profile with its own beneficiary class (cross-field brokering institutions) not present in either sibling reading. All three should be read together to see the full kernel contest; none is the 'correct' single measurement of 'AI risk governance priority' — each is a structurally distinct constraint with its own stable epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
