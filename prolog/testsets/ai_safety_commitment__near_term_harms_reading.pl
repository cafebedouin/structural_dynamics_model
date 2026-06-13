% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__near_term_harms_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_safety_commitment__near_term_harms_reading
 *   human_readable: AI Safety: Near-Term Harms Prevention Commitment
 *   domain: technology_governance/risk_assessment
 *
 * SUMMARY:
 *   AI safety—a contested kernel—is here instantiated as the near-term harms
 *   reading: AI safety means preventing documented present-day harms from
 *   deployed systems (bias, discrimination, labor exploitation,
 *   misinformation). This is ONE of three competing readings of what 'AI
 *   safety' commitments entail. The near-term reading focuses on measurable,
 *   empirically-documented harms occurring in deployed systems today; it
 *   excludes existential-risk and long-horizon concerns as outside the safety
 *   mandate. The constraint exhibits tangled-rope structure: companies that
 *   deploy AI systems benefit by having regulatory clarity pinned to
 *   measurable, addressable harms (narrower scope than existential-risk
 *   frameworks, cheaper to mitigate than dual-priority mandates); affected
 *   populations (marginalized communities, gig workers,
 *   misinformation-exposed groups) are nominally included in harm-prevention
 *   protocols but lack power to shape what counts as a harm or how
 *   remediation is defined; the arrangement requires active enforcement via
 *   auditing, compliance certification, and regulatory oversight to suppress
 *   alternatives (existential-risk frameworks, victim-centered safety
 *   standards, extraction-focused critiques).
 *
 * KEY AGENTS:
 *   - ai_deploying_companies: institutional agenda-setter; controls harm definitions, audit procedures, remediation timelines; benefits from narrow scope; high arbitrage exit (can comply minimally or shift operations)
 *   - marginalized_communities: powerless payers; face structural discrimination in deployed systems; trapped exit (cannot opt out of algorithmic governance)
 *   - gig_platform_workers: organized payers; subject to algorithmic management; constrained exit (depend on platforms for income)
 *   - misinformation_exposed_populations: moderate-power payers; experience epistemic and political harm; constrained exit (embedded in digital ecosystems)
 *   - ai_safety_researchers_near_term_focus: moderate-power beneficiaries; gain funding and career pathways from companies' compliance needs; mobile exit
 *   - excluded_future_populations and alignment_research_communities: structurally outside the constraint's temporal and epistemic boundaries; their concerns are excluded by the reading's own framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.68).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.72).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "AI Safety: Near-Term Harms Prevention Commitment").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, 'd3743a20-9a07-4d90-938f-5ccdbdc45600').
narrative_ontology:cs_kernel_codification('d3743a20-9a07-4d90-938f-5ccdbdc45600', distributed).
narrative_ontology:cs_authority_grounding('d3743a20-9a07-4d90-938f-5ccdbdc45600', distributed).
narrative_ontology:cs_reading_relation('d3743a20-9a07-4d90-938f-5ccdbdc45600', ai_safety_commitment__existential_risk_reading, forecloses).
narrative_ontology:cs_reading_relation('d3743a20-9a07-4d90-938f-5ccdbdc45600', ai_safety_commitment__dual_priority_reading, coexists_with).
narrative_ontology:cs_axiom('d3743a20-9a07-4d90-938f-5ccdbdc45600', foundational, documented_harms_are_primary_safety_problem).
narrative_ontology:cs_axiom_status(documented_harms_are_primary_safety_problem, holdable).
narrative_ontology:cs_axiom_grounding('d3743a20-9a07-4d90-938f-5ccdbdc45600', documented_harms_are_primary_safety_problem, empirically_contingent).
narrative_ontology:cs_axiom('d3743a20-9a07-4d90-938f-5ccdbdc45600', foundational, measurable_present_day_harms_define_safety_scope).
narrative_ontology:cs_axiom_status(measurable_present_day_harms_define_safety_scope, holdable).
narrative_ontology:cs_axiom_grounding('d3743a20-9a07-4d90-938f-5ccdbdc45600', measurable_present_day_harms_define_safety_scope, conventional).
narrative_ontology:cs_reference_frame('d3743a20-9a07-4d90-938f-5ccdbdc45600', empirically_documented_ai_system_harms_framework).
narrative_ontology:cs_drift_state('d3743a20-9a07-4d90-938f-5ccdbdc45600', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d3743a20-9a07-4d90-938f-5ccdbdc45600', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, ai_deploying_companies).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_communities).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_platform_workers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, misinformation_exposed_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, gig_platform_workers).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, ai_safety_researchers_near_term_focus).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, empirical_measurability_of_ai_harms).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, regulatory_legitimacy_of_deployed_system_oversight).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy large language models, recommendation systems, and automated decision systems at scale. Frame AI safety as preventing documented harms through internal compliance, auditing, and harm-mitigation protocols. Benefit from regulatory clarity that pins safety to measurable near-term harms (narrower scope than existential risk, cheaper to address than dual-priority frameworks). Control the definition of harm metrics, audit procedures, and remediation timelines. Can shift costs to users or marginalized populations while claiming safety compliance. Can move operations to jurisdictions with weaker oversight if domestic regulations tighten. Can sponsor safety research that affirms the near-term framing and excludes existential-risk or extraction critiques.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_deploying_companies, agenda_setter,
    institutional, biographical, arbitrage, global).

% Face algorithmic discrimination in hiring, credit, housing, and child welfare systems. Disproportionately harmed by bias in training data and deployment without recourse. Experience the constraint as companies' minimal compliance with harm auditing while structural biases persist. Cannot opt out of algorithmic systems; exit options are geographic or economic (both costly and incomplete). Lack institutional resources to demand audits, challenge results, or negotiate remediation. Depend on regulators and advocacy organizations to defend their interests, which creates principal-agent problems.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_communities, payer,
    powerless, immediate, trapped, global).

% Subject to algorithmic management systems that optimize platform revenue, not worker welfare. Deactivation algorithms, wage-setting, and task allocation operate with minimal transparency. Near-term harm framing creates compliance theater around 'bias detection' in deactivation while the extraction mechanism (algorithmic wage suppression, surveillance, rating manipulation) operates untouched. Can organize collectively (Gig Workers Rising, RWDSU organizing drives) but exit requires finding alternative work in constrained labor markets. Benefit from platform access (gig income when employment alternatives are scarce) while paying algorithmic management costs.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, gig_platform_workers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, gig_platform_workers, beneficiary).

% Exposed to AI-amplified misinformation through recommendation systems and language models. Bear epistemic and political harms (radicalization, belief distortion, democratic degradation). The near-term safety framing creates content moderation and fact-checking protocols that appear to address the harm while algorithmic amplification of engagement-maximizing content (including polarizing false claims) continues as the underlying business model. Cannot fully exit digital platforms; alternatives (alternative media, decentralized networks) often lack reach and are harder to access. Can organize to demand accountability (media literacy initiatives, platform boycotts) but lack institutional power.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, misinformation_exposed_populations, payer,
    moderate, immediate, constrained, global).

% Receive funding, institutional recognition, and career pathways for researching documented harms: bias detection, fairness metrics, transparency tooling, auditing frameworks. Benefit from companies' need to demonstrate safety compliance. Can publish findings and shift to other research areas if funding changes. Have institutional incentive to affirm that near-term harms research is the primary safety frontier (justifies their funding). Can switch research focus to existential risk or other domains if institutional incentives shift.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_safety_researchers_near_term_focus, beneficiary,
    moderate, biographical, mobile, global).

% Evaluate AI safety claims and set regulatory boundaries. Operate under pressure from both near-term harm advocates (demanding measurable accountability) and existential-risk advocates (claiming near-term focus is insufficient). Make policy based on attested harms and research evidence; their classification of what 'counts' as AI safety shapes institutional resource allocation. Lack technical expertise to independently verify safety claims and depend on researcher consensus. Face lobbying pressure from both companies and safety advocates.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, regulators_and_governance_bodies, observer,
    institutional, generational, analytical, national).

% Have no voice in present-day AI safety debates. If AI systems evolve toward misalignment or persistent autonomy, future populations bear risks this reading's framework does not address. Excluded by the temporal constraint of the near-term framing itself (cannot organize, testify, or negotiate). Structurally unrepresented in regulatory and company decisions about AI development trajectories.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, excluded_future_populations, excluded,
    powerless, civilizational, trapped, universal).

% Research long-horizon alignment, interpretability, and existential-risk mitigation. Treated as outside the scope of the near-term safety commitment, competing for resources and institutional attention. Their concern that focusing on documented harms leaves speculative but high-impact risks unaddressed is structurally excluded from the constraint's framing. Can argue for alternative safety frameworks (existential-risk reading, dual-priority reading) but face institutional headwind that near-term harms are more urgent and measurable.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, alignment_research_communities, excluded,
    organized, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__near_term_harms_reading, ai_deploying_companies).
narrative_ontology:fixing_cost_class(ai_safety_commitment__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes measurable, auditable standards for AI system harm prevention (bias testing, fairness metrics, transparency requirements, labor protections, content moderation). Solves the collective-action problem: companies deploying at scale need agreed standards for what constitutes acceptable harm risk; regulators need criteria for enforcement; affected populations need recourse mechanisms.
% TRANSFER_FUNCTION: Moves compliance and auditing burdens (and their costs) to companies; moves harm-remediation responsibility partly to companies and partly back to affected populations (whose data is used for bias testing, whose labor trains feedback systems, whose algorithmic exposure continues while 'fixes' are implemented). Resource flows toward near-term safety research and away from long-horizon alignment work.
% ABSENT_VOICES: Future populations and long-horizon AI researchers cannot testify to the constraint's adequacy because the constraint is framed as present-focused. Gig workers and marginalized communities are nominally included as victim seats but structurally lack power to shape the definition of harms or remediation standards—regulatory and company agendas set the terms. Affected communities would demand safety standards centered on their exit options and structural positions, not on companies' capacity to measure and report metrics.
% DISAPPEARANCE_RATIONALE: If the near-term AI safety commitment disappeared, regulatory frameworks around algorithmic transparency and labor protections would dissolve, companies would cease harm auditing, and remediation standards would revert to market-only mechanisms. The organizational and epistemic infrastructure for measuring documented harms would atrophy. Affected populations would lose a (limited) avenue for accountability, though the underlying extraction mechanisms would persist. The constraint's disappearance would not restore the pre-constraint world; it would leave the harms unaddressed and unmeasured, creating political pressure for alternative safety frameworks (existential-risk, victim-centered, or extraction-focused approaches).
% FOUNDING_PROBLEM: Deployed AI systems (recommender algorithms, hiring systems, content moderation, gig-platform management) produce documented, measurable harms: racial bias in hiring, gender bias in credit, algorithmic deactivation of workers, amplification of misinformation. These harms occur today, at scale, without systematic oversight or accountability. The constraint was built to create the measurement and auditing infrastructure to make these harms visible and remediable.
% FOUNDING_PROBLEM_CORROBORATION: Independent researchers at academic institutions (MIT Media Lab, Stanford AI Index, Princeton Civil Rights-AI Lab), non-profit auditing organizations (Algorithm Audit, Partnership on AI), and affected-community organizations (CAIR, National Fair Housing Alliance, National Employment Law Project) have documented specific harms with empirical evidence spanning 2015–2026. Legal discovery from discrimination suits (Texas v. Google on fair lending, Slack's algorithmic bias findings) and labor disputes (Instacart and DoorDash deactivation algorithms) confirm documented harms. Union organizing and community testimony provide corroboration independent of both the near-term safety researchers (who benefit from the problem's existence) and the companies (who have incentive to minimize the problem's scope). This corroboration is geographically distributed, temporally consistent, and sourced from multiple institutional frames (legal, academic, community, regulatory).
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures the degree to which the constraint transfers value from victims to beneficiaries. At 0.68 (endpoint), the constraint extracts substantially: companies avoid regulation for long-horizon concerns while minimizing documented-harm remediation; affected populations gain compliance theater but not structural change in algorithmic wage suppression, discrimination, or misinformation amplification. The trajectory (0.54 → 0.68 over 20 periods) shows extraction accumulating as compliance protocols proliferate without addressing root mechanisms. Theater ratio (0.48 at endpoint) indicates that a growing share of enforcement activity is performative: companies conduct audits they designed, measure metrics they selected, and report compliance without independent verification of actual harm reduction. Suppression (0.72) reflects active enforcement: the near-term framing is defended through research incentives, regulatory structures, and institutional positioning that suppress alternative framings (existential risk, extraction critique, victim-centered safety). Accessibility_collapse (0.62) is moderate: alternatives (existential-risk and dual-priority framings) remain live in academic and policy discourse but are structurally marginalized by institutional investment in near-term harm frameworks. Resistance (0.58) is substantial: gig workers organize, affected communities document harms, and alignment researchers argue for broader scope—the constraint's persistence requires active suppression of these challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (ai_deploying_companies), the constraint is genuine coordination: companies face regulatory uncertainty and need clear safety standards; the near-term framing provides that clarity while allowing them to address the most measurable, legally-actionable harms. From the powerless and organized victim seats, the same constraint is extraction: companies are allowed to optimize for profit (engagement, wage minimization, microtargeting) while performing harm mitigation; the framework narrows 'safety' to avoid touching business models. From the observer seat (regulators), the constraint appears as a governance solution: measurable harms can be regulated, audited, and remediated—though they note the framework excludes long-horizon concerns. From the excluded seats (future populations, alignment researchers), the constraint is a legitimacy capture: it claims to solve AI safety while excluding the most speculative but potentially high-impact risks. The engine computes each seat's effective directionality from the structural asymmetries: companies near the beneficiary end (d~0.2), victims near the target end (d~0.8), near-term safety researchers moderate-beneficiary (d~0.35), excluded seats structurally outside the directionality model. These divergences are not errors; they are the measure the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures how much a given agent's structural relationship to the constraint pulls them toward extraction or subsidy. ai_deploying_companies: high arbitrage exit (can comply minimally, shift operations, or lobby for different standards) + beneficiary role + institutional power → d ≈ 0.2 (near-full beneficiary). marginalized_communities: trapped exit (cannot opt out of algorithmic systems) + powerless + victim role → d ≈ 0.85 (near-full target). gig_platform_workers: constrained exit (can change platforms but labor market is tight) + organized power (collective action possible) + dual payer/beneficiary role (harmed by wage algorithms but depend on platform income) → d ≈ 0.65 (substantial target with mixed interests). misinformation_exposed_populations: constrained exit (embedded in digital ecosystems, limited alternatives) + moderate power + victim role → d ≈ 0.72. ai_safety_researchers_near_term_focus: mobile exit (can research other topics, seek other funding) + moderate power + beneficiary role → d ≈ 0.35. No directionality overrides are needed; the derivation from beneficiary/victim roles + power + exit captures the structural asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy—the decay of a constraint's founding justification while its enforcement machinery persists—is visible in this story as rising theater_ratio (0.32 → 0.48) while extraction remains high. The founding problem ('deployed AI systems produce documented harms without accountability') is genuinely live and well-corroborated. But the constraint's response—pinning safety to measured near-term harms—is increasingly decoupled from actual harm reduction for victims. Companies conduct audits, publish fairness metrics, implement content-moderation labels, and report compliance—all of which are recorded in the theater category—while algorithmic wage suppression, discrimination, and engagement-optimization harms persist. The constraint has not mandated removal of harmful systems or realignment of business models; it has mandated disclosure and measurement. This is a coherent policy choice, but it is structurally different from harm prevention. If the time series continues and theater approaches 0.60–0.70 while accessibility_collapse remains high (victims have limited alternatives even as the constraint's preventive function degrades), the constraint becomes a piton: the enforcement machinery (auditing, regulatory approval, research funding) persists because the institutional ecosystem depends on it, but the founding harm-prevention function has atrophied into compliance theater. The mandatrophy resolution: the constraint does prevent some measurable harms (bias in hiring recommendations can be reduced via technical intervention); it does not prevent extraction through design (algorithmic wage suppression) or structural power (gatekeeping). Labeling this as 'safety' is the cover story the constraint enables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_scope_vs_extraction_scope,
    'Does the near-term harm framing capture the full scope of extraction the constraint enables, or does it exclude structural harms (wage suppression, attention manipulation, data exploitation) that are harder to measure or attribute causally?',
    'Comparative analysis of harms documented in peer-reviewed audits versus harms claimed by affected-community organizations and labor advocates. Trace which harms are addressed by compliance protocols and which persist despite certification.',
    'If extraction scope exceeds documented-harm scope, the constraint''s beneficiary (companies) gets regulatory shield against a wider extraction than the framework acknowledges; the effective ε is higher than the metrics reflect. If scopes align, the measured extraction is the real constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_scope_vs_extraction_scope, empirical, 'Whether measurable documented harms fully capture the extraction the constraint enables.').

omega_variable(
    kernel_reading_boundary,
    'Is the near-term harms reading a coherent, internally consistent commitment to preventing documented AI system harms, or is it functioning as a conceptual boundary to exclude existential-risk concerns from the safety mandate?',
    'Genealogical analysis: trace whether near-term harm focus originated as an independent empirical claim (documented harms are the primary safety problem) or as a reactive boundary-drawing against existential-risk framings. Examine whether the boundary is defended on evidential grounds or by assertion.',
    'If the boundary is reactive, the reading is partly defined by what it excludes rather than by its positive claim—the constraint''s legitimacy depends on excluding the sibling reading from consideration. If the boundary is evidential, it stands on independent grounds. This affects how the engine classifies the constraint when existential-risk evidence shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the near-term framing is an independent empirical claim or a reactive boundary-drawing against existential-risk concerns.').

omega_variable(
    compliance_theater_vs_harm_reduction,
    'To what extent do measured compliance actions (bias audits, fairness metrics, content moderation labels) actually reduce harm to affected populations, versus creating the appearance of safety while extraction mechanisms persist?',
    'Longitudinal outcome studies: track whether communities subject to algorithmic systems report reduced discrimination, wage suppression, or misinformation exposure after companies implement near-term safety protocols. Compare outcomes in regulated versus unregulated deployment contexts.',
    'High theater indicates the constraint functions as extraction-cover, not harm-reduction. If theater_ratio rises while extractiveness plateaus or rises, the constraint is becoming performative. This would support mandatrophy detection and piton reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_theater_vs_harm_reduction, empirical, 'Whether near-term safety protocols actually reduce harms to victims or primarily create compliance theater.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) primarily structural—regulatory barriers, technical opacity, resource asymmetry—or has affected-population resistance been partly internalized, with communities accepting limited remediation frameworks as the boundary of what ''counts'' as safety?',
    'Post-constraint-removal trajectory: if the near-term safety framing were abandoned, would suppressed communities rapidly mobilize for broader remediation, or would internalized acceptance of the harm-measurement framework persist? Community interviews and organizing history analysis.',
    'If partly internalized, the effective suppression is higher than the structural measure suggests; communities carry constraint assumptions into subsequent negotiations. This strengthens the snare classification and affects whether removal of the constraint would actually enable victim voice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural versus internalized suppression of victim resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__near_term_harms_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(ai_s_tr_t0, observed).
narrative_ontology:measurement(ai_s_tr_t4, ai_safety_commitment__near_term_harms_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement_basis(ai_s_tr_t4, observed).
narrative_ontology:measurement(ai_s_tr_t8, ai_safety_commitment__near_term_harms_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement_basis(ai_s_tr_t8, observed).
narrative_ontology:measurement(ai_s_tr_t12, ai_safety_commitment__near_term_harms_reading, theater_ratio, 12, 0.45).
narrative_ontology:measurement_basis(ai_s_tr_t12, observed).
narrative_ontology:measurement(ai_s_tr_t16, ai_safety_commitment__near_term_harms_reading, theater_ratio, 16, 0.47).
narrative_ontology:measurement_basis(ai_s_tr_t16, observed).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__near_term_harms_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(ai_s_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(ai_s_be_t0, observed).
narrative_ontology:measurement(ai_s_be_t4, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 4, 0.59).
narrative_ontology:measurement_basis(ai_s_be_t4, observed).
narrative_ontology:measurement(ai_s_be_t8, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement_basis(ai_s_be_t8, observed).
narrative_ontology:measurement(ai_s_be_t12, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(ai_s_be_t12, observed).
narrative_ontology:measurement(ai_s_be_t16, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement_basis(ai_s_be_t16, observed).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(ai_s_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(ai_s_su_t0, observed).
narrative_ontology:measurement(ai_s_su_t4, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement_basis(ai_s_su_t4, observed).
narrative_ontology:measurement(ai_s_su_t8, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement_basis(ai_s_su_t8, observed).
narrative_ontology:measurement(ai_s_su_t12, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement_basis(ai_s_su_t12, observed).
narrative_ontology:measurement(ai_s_su_t16, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(ai_s_su_t16, observed).
narrative_ontology:measurement(ai_s_su_t20, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(ai_s_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__dual_priority_reading).

% DUAL FORMULATION NOTE:
% The ai_safety_commitment kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading of what 'AI safety' means. This story (near_term_harms_reading) focuses on preventing documented present-day harms; it forecloses existential-risk framings from the safety mandate while coexisting with dual-priority approaches in institutional discourse. The sibling stories differ in victim sets (present-day marginalized communities vs. future populations vs. both), beneficiaries (tech companies vs. alignment research vs. coordination), and ε values (0.68 vs. higher on long-horizon risk vs. intermediate on dual priorities). Each story has its own metrics, stakeholders, and mandatrophy trajectory. The network edges encode the relationships: this reading influences the dual-priority reading (establishes what near-term commitment would require) and forecloses the existential-risk reading from the same institutional mandate (you cannot simultaneously commit to near-term harms as the safety mandate and existential risk as the safety mandate—they have different victim sets, different empirical grounding, and different remediation timelines).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
