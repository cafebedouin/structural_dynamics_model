% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: Near-Term Harms Reading of AI Risk Governance Priority
 *   domain: AI governance / technology ethics / risk assessment
 *
 * SUMMARY:
 *   This story authors the near-term-harms reading of the contested
 *   AI-risk-governance-priority kernel: the claim that governance attention,
 *   funding, and regulatory infrastructure should be directed at demonstrated
 *   present harms — algorithmic bias, surveillance, labor displacement —
 *   rather than speculative existential risk from future superintelligent
 *   systems. Structurally, this reading identifies frontier AI labs and
 *   existential-risk research institutes as beneficiaries of the RIVAL
 *   framing insofar as x-risk discourse diverts scrutiny and regulatory
 *   bandwidth away from currently deployed systems generating measurable harm
 *   to Global South populations, marginalized algorithmic subjects, and
 *   displaced workers. The ε authored here (0.68) is high because it measures
 *   the standing arrangement under contest AS THIS READING SEES IT: a
 *   governance landscape where present, empirically documented harms remain
 *   under-addressed while attention concentrates on long-horizon scenarios
 *   that primarily benefit the institutions positioned to research and
 *   mitigate them. This is deliberately NOT the ε of the sibling readings —
 *   the existential_risk_reading and bridge_reading are separate constraint
 *   stories with their own ε values, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - global_south_populations: primary target (powerless/trapped) — bears algorithmic harms with no meaningful exit
 *   - marginalized_algorithmic_subjects: primary target (powerless/constrained) — misclassified by deployed systems
 *   - automation_displaced_workers: primary target (powerless/constrained) — loses livelihood to current automation
 *   - frontier_ai_labs: primary beneficiary of rival framing (institutional/arbitrage) — deploys harmful systems while shaping x-risk discourse
 *   - x_risk_research_institutes: secondary beneficiary of rival framing (organized/mobile) — competes for attention this reading would redirect
 *   - fairness_auditors_and_regulators: agenda-setter for this reading (organized/constrained) — pushes present-harm mitigation
 *   - technology_policy_bodies: analytical observer (institutional/analytical) — adjudicates resource allocation between framings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.68).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.52).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "Near-Term Harms Reading of AI Risk Governance Priority").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "AI governance / technology ethics / risk assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, 'e0e104fe-c87f-43eb-a58f-4d3eb5cb4a49').
narrative_ontology:cs_kernel_codification('e0e104fe-c87f-43eb-a58f-4d3eb5cb4a49', distributed).
narrative_ontology:cs_authority_grounding('e0e104fe-c87f-43eb-a58f-4d3eb5cb4a49', distributed).
narrative_ontology:cs_reading_relation('e0e104fe-c87f-43eb-a58f-4d3eb5cb4a49', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0e104fe-c87f-43eb-a58f-4d3eb5cb4a49', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('e0e104fe-c87f-43eb-a58f-4d3eb5cb4a49', foundational, demonstrated_harm_precedes_speculative_harm_in_moral_priority).
narrative_ontology:cs_axiom_status(demonstrated_harm_precedes_speculative_harm_in_moral_priority, holdable).
narrative_ontology:cs_axiom_grounding('e0e104fe-c87f-43eb-a58f-4d3eb5cb4a49', demonstrated_harm_precedes_speculative_harm_in_moral_priority, deontological).
narrative_ontology:cs_axiom('e0e104fe-c87f-43eb-a58f-4d3eb5cb4a49', secondary, x_risk_framing_functions_as_regulatory_attention_diversion).
narrative_ontology:cs_axiom_status(x_risk_framing_functions_as_regulatory_attention_diversion, holdable).
narrative_ontology:cs_axiom_grounding('e0e104fe-c87f-43eb-a58f-4d3eb5cb4a49', x_risk_framing_functions_as_regulatory_attention_diversion, empirically_contingent).
narrative_ontology:cs_reference_frame('e0e104fe-c87f-43eb-a58f-4d3eb5cb4a49', harm_reduction_first_governance_norm).
narrative_ontology:cs_drift_state('e0e104fe-c87f-43eb-a58f-4d3eb5cb4a49', post_generative_ai_deployment_surge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e0e104fe-c87f-43eb-a58f-4d3eb5cb4a49', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, x_risk_research_institutes).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, marginalized_algorithmic_subjects).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, automation_displaced_workers).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__near_term_harms_reading, distributive_justice_priority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to AI-driven credit scoring, content moderation, and welfare-allocation systems built and deployed with minimal local input. Bears the compounding harms of biased models trained on non-representative data, with essentially no capacity to opt out of digital infrastructure increasingly gatekept by these systems.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_populations, payer,
    powerless, immediate, trapped, global).

% Includes racialized, disabled, and low-income populations who are disproportionately misclassified by facial recognition, hiring algorithms, and predictive policing tools currently in active deployment. Can sometimes contest individual decisions through litigation or advocacy but cannot exit the systems themselves.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, marginalized_algorithmic_subjects, payer,
    powerless, immediate, constrained, national).

% Lose income and occupational identity as AI systems automate tasks across logistics, customer service, and clerical work. Retraining programs are underfunded relative to the pace of displacement; exit means downward occupational mobility, not genuine alternative employment.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, automation_displaced_workers, payer,
    powerless, biographical, constrained, national).

% Civil society organizations, algorithmic auditors, and regulatory bodies that push for bias audits, transparency mandates, and labor protections targeting deployed systems. Their agenda-setting power is real but bounded by funding, access to model internals, and the political salience competing frames command.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, fairness_auditors_and_regulators, agenda_setter,
    organized, biographical, constrained, national).

% Develop and deploy the very systems generating present harms while participating prominently in existential-risk discourse. Benefit when regulatory and public attention is captured by speculative superintelligence scenarios rather than audits of currently deployed products; can relocate operations or reframe compliance obligations across jurisdictions.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, frontier_ai_labs, beneficiary,
    institutional, generational, arbitrage, global).

% Compete for philanthropic and governmental funding and policy attention under the existential-risk frame. Under a near-term-harms-prioritized regime, their research agenda is treated as a lower-priority use of scarce regulatory and funding bandwidth, though they retain resources and mobility that displaced workers and Global South populations lack.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, x_risk_research_institutes, beneficiary,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, x_risk_research_institutes, excluded).

% Legislatures, standards bodies, and multilateral institutions weighing how to allocate governance attention and resources between present-harm mitigation and long-horizon risk research. They receive competing testimony from all other seats and set the frameworks that operationalize whichever priority reading gains traction.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, technology_policy_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__near_term_harms_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates governance attention, audit capacity, and regulatory resources on measurable, currently-occurring algorithmic harms — bias in deployed systems, surveillance infrastructure, labor displacement — rather than distributing that attention across speculative future scenarios.
% TRANSFER_FUNCTION: Moves regulatory bandwidth, funding, and legislative priority away from long-horizon existential-risk research programs and toward fairness audits, anti-discrimination enforcement, transparency mandates, and labor-transition programs targeting harms already occurring to identifiable populations.
% ABSENT_VOICES: The populations bearing present algorithmic harms — Global South communities subject to opaque scoring systems, displaced workers, algorithmically misclassified individuals — are rarely present in the elite policy fora where the x-risk/near-term priority contest is actually adjudicated; they are represented, imperfectly, by advocacy organizations and academic researchers rather than speaking directly.
% DISAPPEARANCE_RATIONALE: If this prioritization claim disappeared and governance attention defaulted fully to existential-risk framing, fairness audit funding, algorithmic accountability litigation, and labor-transition programs tied to present AI deployment would lose their primary justificatory anchor, and enforcement resources would visibly shift toward long-horizon safety research at frontier labs — a real reallocation of budgets and legislative calendars, not a cosmetic change.
% FOUNDING_PROBLEM: Documented, ongoing harms from deployed AI systems — discriminatory lending and hiring algorithms, surveillance tools used against marginalized communities, and labor displacement from automation — were going unaddressed while public and philanthropic attention concentrated on speculative future risks from advanced AI systems that do not yet exist.
% FOUNDING_PROBLEM_CORROBORATION: Independent empirical audits (e.g. academic algorithmic-bias studies, investigative journalism documenting predictive-policing and welfare-algorithm harms) and international-labor-organization displacement data corroborate the founding problem from outside both the near-term-harms advocacy coalition and the frontier AI labs; these sources are not beneficiaries of either governance framing.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the measured interval (0.45 to 0.68) reflecting this reading's claim that as x-risk discourse has gained prominence in policy and philanthropic circles, the opportunity cost borne by present-harm populations has grown — funding and legislative attention that could address deployed-system harms is measurably diverted. Theater ratio is moderate and also rising (0.30 to 0.44): fairness-audit and AI-ethics compliance activity has expanded, but a growing share is documented as symbolic (voluntary audits without enforcement teeth, ethics boards without veto power) rather than substantively remedial. Suppression (0.52) reflects real but partial structural barriers — displaced workers and algorithmically misclassified individuals face genuine but not fully totalizing constraints on redress (litigation exists, though it is slow and resource-intensive). Accessibility collapse is moderate (0.40): alternative governance arrangements (binding present-harm regulation) are conceivable and partially implemented in some jurisdictions, so alternatives have not fully collapsed. Resistance is substantial (0.60): advocacy coalitions, algorithmic justice organizations, and some regulators actively contest the resource-allocation status quo.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (fairness auditors and regulators), the arrangement is a coordination effort under construction, contested but making real gains. From the payer seats (Global South populations, marginalized subjects, displaced workers), the same governance landscape looks like an extraction dynamic in which resources continually route past their documented injuries toward long-horizon research infrastructure that primarily benefits institutional actors. From the frontier lab beneficiary seat, the current allocation appears efficient prioritization of scarce technical expertise. The engine computes these divergent per-seat readings from the structural power/exit data; this story does not average or reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South populations, marginalized algorithmic subjects, and displaced workers are declared victims: they bear the transfer function (diverted regulatory attention and funding) with trapped-to-constrained exit options, pushing their derived directionality toward the full-target end. Frontier AI labs and x-risk research institutes are declared beneficiaries: they retain institutional power and mobile-to-arbitrage exit options, and the diversion of scrutiny toward speculative risk is structurally advantageous to them (less binding regulation on deployed products, more research funding and prestige for long-horizon work), pushing their derived directionality toward the subsidized end. Fairness auditors and regulators occupy the agenda-setter role without capturing extraction themselves — they administer the coordination function this reading defends.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — present, measurable AI harms to identifiable populations — remains live and corroborated by sources outside the advocacy coalition (independent audits, labor-displacement data), which forecloses treating this reading's priority claim as an obsolete mandate merely defended by inertia. The classification prevents mislabeling the near-term-harms coordination function as pure extraction: the fairness-audit and regulatory-enforcement apparatus does solve a genuine, documented problem (Q1 coordination_function), even though it simultaneously enables a resource competition in which x-risk-adjacent institutions experience real opportunity costs. Equally, it prevents mislabeling the arrangement as pure coordination: the tangled_rope classification requires and receives both a genuine coordination function AND an asymmetric extraction structure (present-harm populations pay through continued exposure to inadequately audited systems while resource competition plays out at the institutional level above them).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_zero_sum_assumption,
    'Is regulatory and philanthropic attention to AI risk actually zero-sum between present-harm mitigation and existential-risk research, or can both be scaled up simultaneously without one crowding out the other?',
    'Track funding and legislative-calendar allocations over time across both categories; if aggregate AI governance investment grows faster than either category individually, the zero-sum premise weakens.',
    'If genuinely zero-sum, this reading''s beneficiary/victim structure (x-risk institutes and frontier labs benefiting from diverted attention) holds robustly. If not zero-sum, the extraction claim weakens substantially and the arrangement looks more like the bridge_reading''s non-competing framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_zero_sum_assumption, empirical, 'Whether present-harm and existential-risk governance attention genuinely compete for the same finite resource pool.').

omega_variable(
    frontier_lab_x_risk_sincerity,
    'Do frontier AI labs promote existential-risk discourse strategically to divert scrutiny from present deployment harms, or do they sincerely believe in and prioritize both concerns, with the diversion effect an unintended side effect rather than a deliberate strategy?',
    'Compare internal resource allocation, lobbying records, and public statements across labs; examine whether labs that emphasize x-risk publicly also underinvest in present-harm mitigation internally relative to peers.',
    'If strategic diversion is demonstrated, the beneficiary designation for frontier_ai_labs is strongly warranted and the tangled_rope classification is robust. If sincere dual concern with unintended diversion effects, the beneficiary relationship is weaker and more attenuated — still present but less clearly asymmetric extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(frontier_lab_x_risk_sincerity, conceptual, 'Whether the beneficiary relationship between frontier labs and the diversion-of-scrutiny effect is strategic or incidental.').

omega_variable(
    kernel_framing_disagreement_location,
    'Where exactly does the near_term_harms_reading and the existential_risk_reading disagree — is it about the PROBABILITY of catastrophic AI risk, the TRACTABILITY of addressing it now versus later, or the MORAL WEIGHT assigned to diffuse present harms versus low-probability catastrophic harms?',
    'This is a conceptual/preference disagreement not resolvable by further data alone, but philosophical and empirical literature on population ethics, risk aggregation, and AI capability forecasting can narrow which component of disagreement is doing the most work in any given policy dispute.',
    'If the disagreement is primarily about probability estimates, better forecasting could in principle dissolve much of the kernel contest. If it is primarily about moral weighting (diffuse certain harm vs. low-probability catastrophic harm), the disagreement is likely to persist regardless of empirical resolution, and the three readings will remain permanently coexisting rather than convergent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_disagreement_location, conceptual, 'Locating the structural source of disagreement among the kernel''s sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 4, 0.33).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(ai_r_tr_t24, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(ai_r_be_t24, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 24, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ai_risk_governance_priority__near_term_harms_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, bridge_reading).

% DUAL FORMULATION NOTE:
% This story, existential_risk_reading, and bridge_reading form a three-member constraint family decomposing the natural-language concept 'AI risk governance priority.' Each reading authors a distinct ε, distinct beneficiary/victim structure, and distinct claimed_type from its own structural premises. near_term_harms_reading (this story) authors high ε (0.68) on the standing present-deployment-harm arrangement and low implicit priority on speculative risk; existential_risk_reading would author low ε on present harms and high ε on under-resourced long-horizon safety work; bridge_reading rejects the zero-sum framing underlying both and would author a correspondingly lower overall ε by treating the categories as non-competing. All three link to each other via affects_constraints because they share a contested resource-allocation domain and mutually influence which framing captures governance attention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
