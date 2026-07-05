% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Near-Term Harms Priority Reading of AI Risk Governance
 *   domain: technology/policy/social
 *
 * SUMMARY:
 *   This story instantiates the near-term-harms reading of the contested AI
 *   risk governance priority kernel: the claim that governance resources
 *   should center on demonstrated present injuries (algorithmic bias,
 *   misinformation, labor displacement, surveillance) affecting marginalized
 *   populations, rather than on speculative existential risk from future
 *   superintelligent systems. As a reading, it is analyzed on its own
 *   structural terms — victim set, beneficiary set, and resource flow are
 *   specific to this framing and not averaged with the sibling readings
 *   (existential_risk_reading, bridge_reading), which are separate
 *   constraints in the family.
 *
 * KEY AGENTS:
 *   - technology_companies: primary structural beneficiary (institutional/arbitrage) — capture attention-diversion benefit and administer much of the resulting audit apparatus
 *   - fairness_audit_industry: secondary beneficiary (organized/mobile) — revenue depends on the framework's persistence
 *   - global_south_populations: primary named victim (powerless/trapped) — harms invoked to justify the framing but underserved by its remedies
 *   - algorithmically_discriminated_groups: named victim (powerless/constrained) — bear ongoing harm during protracted audit-and-disclosure cycles
 *   - displaced_workers: named victim (powerless/trapped) — rhetorically prioritized but under-resourced relative to audit infrastructure
 *   - ai_safety_research_community: excluded from near-term-scoped bodies despite the resource competition this reading creates
 *   - policy_analysts: analytical observer tracing resource flows against stated priorities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.62).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.48).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "Near-Term Harms Priority Reading of AI Risk Governance").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "technology/policy/social").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, '51af7336-9645-45f0-8350-64ffeb97818d').
narrative_ontology:cs_kernel_codification('51af7336-9645-45f0-8350-64ffeb97818d', distributed).
narrative_ontology:cs_authority_grounding('51af7336-9645-45f0-8350-64ffeb97818d', distributed).
narrative_ontology:cs_reading_relation('51af7336-9645-45f0-8350-64ffeb97818d', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('51af7336-9645-45f0-8350-64ffeb97818d', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('51af7336-9645-45f0-8350-64ffeb97818d', foundational, only_demonstrated_harm_warrants_binding_governance).
narrative_ontology:cs_axiom_status(only_demonstrated_harm_warrants_binding_governance, holdable).
narrative_ontology:cs_axiom_grounding('51af7336-9645-45f0-8350-64ffeb97818d', only_demonstrated_harm_warrants_binding_governance, empirically_contingent).
narrative_ontology:cs_axiom('51af7336-9645-45f0-8350-64ffeb97818d', secondary, speculative_risk_governance_diverts_resources_from_present_victims).
narrative_ontology:cs_axiom_status(speculative_risk_governance_diverts_resources_from_present_victims, holdable).
narrative_ontology:cs_axiom_grounding('51af7336-9645-45f0-8350-64ffeb97818d', speculative_risk_governance_diverts_resources_from_present_victims, instrumental).
narrative_ontology:cs_reference_frame('51af7336-9645-45f0-8350-64ffeb97818d', harm_reduction_first_governance).
narrative_ontology:cs_drift_state('51af7336-9645-45f0-8350-64ffeb97818d', contemporary_generative_ai_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('51af7336-9645-45f0-8350-64ffeb97818d', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, fairness_audit_industry).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, algorithmically_discriminated_groups).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, displaced_workers).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__near_term_harms_reading, harm_must_be_demonstrated_not_speculated).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy the systems whose present-day harms (biased hiring tools, content moderation failures, surveillance products) are the object of the near-term framing, while simultaneously funding and staffing much of the fairness-audit and responsible-AI apparatus that governs them. Because the near-term reading routes attention and compliance obligation toward auditable present-day metrics rather than toward frontier capability restriction, it leaves the companies' most consequential future development largely outside the governance conversation. They can absorb compliance costs, pivot audit teams, or relocate development to lighter-touch jurisdictions.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, technology_companies, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, technology_companies, agenda_setter).

% A growing ecosystem of auditors, bias-testing vendors, and compliance consultancies whose revenue depends on the near-term harms framework remaining the dominant regulatory paradigm. They benefit from every new bias audit mandate and fairness certification requirement, and have professional and financial reasons to keep governance attention anchored to measurable present deployment harms.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, fairness_audit_industry, beneficiary,
    organized, biographical, mobile, global).

% Subject to AI systems developed and deployed with minimal local input — content moderation systems that fail their languages, credit-scoring and welfare-allocation algorithms trained on foreign data, surveillance exports from wealthier states. The near-term framing names their harms explicitly but the resulting resources (audit frameworks, fairness certifications) are typically built and administered from outside their countries, with limited capacity for them to enforce remedies locally.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_populations, payer,
    powerless, biographical, trapped, global).

% Individuals denied loans, jobs, housing, or parole favorably due to biased scoring and predictive systems. The near-term reading directs regulatory energy toward documenting and slightly mitigating these harms through audits and disclosure requirements, but rarely toward banning the systems outright or giving affected individuals binding remedies; they bear the ongoing cost of systems that remain in deployment while audits proceed.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, algorithmically_discriminated_groups, payer,
    powerless, immediate, constrained, national).

% Workers whose jobs are automated by AI-driven systems, a harm the near-term reading explicitly names as a governance priority. In practice, resources flow disproportionately to bias-audit and misinformation infrastructure rather than to labor-transition funding, retraining programs, or restrictions on displacement-driving deployment, leaving this named harm comparatively under-addressed relative to its rhetorical priority.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, displaced_workers, payer,
    powerless, biographical, trapped, national).

% Researchers who argue that governance capacity is finite and that near-term-exclusive framing systematically starves long-horizon capability-risk work of funding, personnel, and regulatory hooks. Their concerns are frequently characterized within near-term-reading discourse as distraction from real, measurable harms, and they have limited standing within regulatory bodies whose mandates are scoped to present deployment harms.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_safety_research_community, excluded,
    organized, civilizational, constrained, global).

% Study which harms get funded, which populations get named as beneficiaries of governance attention, and whether the near-term framing's resource allocation matches its stated victim set. They can trace funding flows and audit outcomes but do not control policy.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates governance attention and resources on harms that are already measurable and demonstrable — biased algorithms, misinformation systems, surveillance deployment, labor displacement — allowing regulators, auditors, and affected communities to coordinate around concrete, evidenced injuries rather than speculative future scenarios.
% TRANSFER_FUNCTION: Moves regulatory attention, compliance budgets, and remedial resources toward present-harm mitigation infrastructure (audits, fairness certifications, disclosure regimes) and, correspondingly, away from frontier-capability restriction and long-horizon safety research funding; the named beneficiaries of this attention shift (technology companies avoiding capability-scoped regulation, the audit industry capturing compliance spend) are structurally distinct from the named victims whose harms justify the framing.
% ABSENT_VOICES: Communities in the Global South most affected by exported surveillance and poorly localized systems have little seat at the standard-setting tables that define what counts as an adequate fairness audit. Long-horizon safety researchers are also structurally absent from near-term-scoped regulatory bodies, and displaced workers rarely have organized representation in AI policy processes despite being explicitly named as a priority population.
% DISAPPEARANCE_RATIONALE: Technology companies and the audit industry would argue the world rearranges substantially — compliance obligations, disclosure mandates, and audit markets would collapse or restructure. Advocates for algorithmically harmed populations would argue enforcement is already so weak that formal disappearance would change comparatively little in practice, since remedies are rarely binding. The dispute is real and unresolved.
% FOUNDING_PROBLEM: AI systems were being deployed at scale with documented discriminatory, exploitative, and destabilizing effects on real people — biased hiring and lending algorithms, content moderation failures amplifying harm in under-resourced languages, surveillance tools exported to repressive contexts, and labor displacement without transition support — while governance discourse risked being captured entirely by speculative long-horizon existential-risk framing that offered no near-term remedy.
% FOUNDING_PROBLEM_CORROBORATION: Independent journalism, civil-society audits (e.g., algorithmic justice advocacy organizations), and academic labor-economics research document ongoing algorithmic discrimination and displacement harms from outside the technology-company and audit-industry beneficiary set, corroborating that the founding problem remains live. However, the same outside sources frequently note that the resulting governance response is thin relative to the scale of documented harm, particularly for labor displacement and Global South populations.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.62) reflects that resources nominally directed at named harms flow disproportionately toward audit and compliance infrastructure that benefits technology companies and the audit industry, while the underlying deployment harms to Global South populations, discriminated groups, and displaced workers persist largely unremedied. Suppression (0.48) is moderate: there is no formal barrier to raising these harms, but the framing itself channels dissent into procedural audit demands rather than deployment restriction, softly suppressing more structural remedies. Theater ratio (0.40, rising over the interval) captures the growth of audit and certification activity that is increasingly performative relative to binding enforcement. Accessibility collapse (0.35) is moderate-low because alternative framings (bridge, existential-risk) remain live in the discourse and are not foreclosed by this reading's institutional dominance. Resistance (0.55) reflects active pushback from both long-horizon safety researchers (who see resource capture) and grassroots harm-affected communities (who see remedy capture).
 *
 * PERSPECTIVAL GAP:
 *   From the technology company and audit industry seats, this reading looks like responsible, evidence-based governance correctly prioritizing measurable harm. From the payer seats — Global South populations, discriminated individuals, displaced workers — the same structure looks like a framework that names their injury as its justification while directing the resulting resources elsewhere. The engine computes these as different seat-level classifications from the same structural data; neither seat is in error, and the divergence is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Technology companies sit near the beneficiary end: the near-term framing, by scoping governance attention to auditable present deployment metrics, structurally diverts regulatory energy away from capability-level restrictions that would bind their frontier development, while letting them absorb or monetize compliance. The fairness audit industry is a secondary beneficiary whose commercial interest is served by the framework's persistence regardless of remedy efficacy. The three named victim groups are structurally targets: they are invoked as the moral justification for the framework, yet the resource flow the framework generates (audit markets, disclosure regimes) does not correspond proportionally to remedy delivered to them — especially displaced workers, whose harm is prominently named but poorly resourced relative to bias-audit infrastructure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unaddressed present-day algorithmic harm) remains live and independently corroborated, which argues against pure mandatrophy. However, the mismatch between founding_problem_status (live) and the thinness of delivered remedy relative to rhetorical priority — especially for labor displacement and Global South populations — is exactly the signal the tangled_rope classification is meant to catch: a real coordination function (naming and measuring present harms) co-existing with asymmetric extraction (resource capture by the audit industry and attention-diversion benefit to technology companies) that requires active enforcement (audit mandates, disclosure requirements) to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attention_diversion_causal_link,
    'Does prioritizing near-term harms governance actually cause reduced regulatory attention to frontier capability risk, or do the two compete for genuinely separate institutional capacity such that neither framing displaces the other?',
    'Comparative analysis of regulatory body mandates and funding allocations across jurisdictions with different declared priorities; track whether capability-restriction proposals lose ground specifically when near-term harm frameworks gain institutional traction, versus independent variation.',
    'If the diversion link is real, technology companies are a stronger structural beneficiary of this reading than the metrics currently reflect, raising ε; if the domains are institutionally separable, the beneficiary classification for technology companies weakens substantially and the constraint moves closer to a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_diversion_causal_link, empirical, 'Whether near-term harm prioritization structurally displaces existential-risk governance capacity or merely coexists with it.').

omega_variable(
    remedy_delivery_vs_rhetoric_gap,
    'Is the gap between rhetorical priority (displaced workers, Global South populations explicitly named) and actual resource delivery (concentrated in audit/compliance infrastructure) a temporary implementation lag in a maturing governance field, or a structural feature of who controls the framework''s institutional apparatus?',
    'Longitudinal tracking of funding allocation and binding-remedy rates across the named victim categories over a multi-year window; a persistent or widening gap would indicate structural capture rather than lag.',
    'A structural gap supports the tangled_rope classification and a rising extractiveness trajectory; a closing gap over time would support reclassifying toward a genuine rope as the coordination function matures and remedy delivery catches up to naming.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_delivery_vs_rhetoric_gap, empirical, 'Whether under-delivery to named victim groups is transitional or structural.').

omega_variable(
    framing_choice_as_kernel_contest,
    'Is the choice to prioritize near-term harms over existential risk (or to treat them as entangled, per the bridge reading) a genuine empirical disagreement about tractability and evidence, or a proxy contest over which institutional actors control governance resources and legitimacy?',
    'Trace whether advocates'' revealed institutional positions (funding sources, employer, career incentives) predict their reading preference better than their stated epistemic reasoning does.',
    'If reading choice is substantially predicted by institutional self-interest rather than by evidence about tractability, all three kernel readings should be evaluated with heightened suspicion of motivated framing, including this one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_choice_as_kernel_contest, conceptual, 'Whether the kernel contest between readings is primarily epistemic or primarily institutional/political.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(ai_r_tr_t24, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(ai_r_be_t24, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 24, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ai_risk_governance_priority__near_term_harms_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'AI risk governance priority,' per the ε-invariance principle: the near-term-harms, existential-risk, and bridge readings of the same kernel produce materially different beneficiary/victim structures and different ε values and must not be collapsed into a single constraint. This reading shows a moderate-to-high, rising ε concentrated on present deployment harms; the existential-risk reading would show a different beneficiary structure (long-horizon safety institutions, potentially also technology companies via regulatory-capture-of-the-narrative dynamics) and a different, more speculative ε profile; the bridge reading would show a hybrid structure attempting to avoid the resource-competition dynamic named in the omega above. All three are linked via affects_constraints because institutional resource allocation in one reading structurally competes with resource allocation available to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
