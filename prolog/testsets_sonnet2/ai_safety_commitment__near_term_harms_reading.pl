% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: ai_safety_commitment__near_term_harms_reading
 *   human_readable: AI Safety as Present-Day Harm Mitigation (Bias, Labor, Misinformation)
 *   domain: technology/labor/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the near-term-harms reading of the contested 'AI
 *   safety' kernel: the claim that safety means preventing documented,
 *   measurable, present-tense harms — algorithmic discrimination,
 *   exploitative labor in the AI supply chain, and misinformation
 *   amplification — from systems already deployed at scale. Under this
 *   reading, the coordination function (shared technical vocabulary for bias
 *   testing and incident reporting) is real but has been substantially
 *   captured: large developers use the broader 'AI safety' umbrella, and the
 *   philanthropic/research attention it commands, to redirect scrutiny toward
 *   speculative long-horizon alignment work while documented present-day
 *   harms continue with comparatively little binding remediation. The victims
 *   are structurally powerless (gig workers, screened applicants,
 *   discriminated communities); the beneficiaries are the developers who
 *   avoid near-term regulatory bite and the existential-risk research
 *   institutes who gain funding and legitimacy without needing to demonstrate
 *   present-day accountability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.68).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.58).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "AI Safety as Present-Day Harm Mitigation (Bias, Labor, Misinformation)").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "technology/labor/civil_rights").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, 'a7961bf0-902d-47e8-be70-bd2b210b842a').
narrative_ontology:cs_kernel_codification('a7961bf0-902d-47e8-be70-bd2b210b842a', distributed).
narrative_ontology:cs_authority_grounding('a7961bf0-902d-47e8-be70-bd2b210b842a', distributed).
narrative_ontology:cs_reading_relation('a7961bf0-902d-47e8-be70-bd2b210b842a', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7961bf0-902d-47e8-be70-bd2b210b842a', ai_safety_commitment__dual_priority_reading, influences).
narrative_ontology:cs_axiom('a7961bf0-902d-47e8-be70-bd2b210b842a', foundational, documented_present_harm_has_priority_claim).
narrative_ontology:cs_axiom_status(documented_present_harm_has_priority_claim, holdable).
narrative_ontology:cs_axiom_grounding('a7961bf0-902d-47e8-be70-bd2b210b842a', documented_present_harm_has_priority_claim, empirically_contingent).
narrative_ontology:cs_axiom('a7961bf0-902d-47e8-be70-bd2b210b842a', foundational, speculative_future_harm_cannot_justify_deferring_remediable_present_harm).
narrative_ontology:cs_axiom_status(speculative_future_harm_cannot_justify_deferring_remediable_present_harm, holdable).
narrative_ontology:cs_axiom_grounding('a7961bf0-902d-47e8-be70-bd2b210b842a', speculative_future_harm_cannot_justify_deferring_remediable_present_harm, deontological).
narrative_ontology:cs_reference_frame('a7961bf0-902d-47e8-be70-bd2b210b842a', harm_reduction_accountability_framework).
narrative_ontology:cs_drift_state('a7961bf0-902d-47e8-be70-bd2b210b842a', contemporary_generative_ai_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a7961bf0-902d-47e8-be70-bd2b210b842a', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, large_ai_developers).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, existential_risk_research_institutes).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_content_moderators).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, algorithmically_screened_job_applicants).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_communities_facing_discriminatory_scoring).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, data_annotation_workers_global_south).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the terms of what counts as 'AI safety' in public discourse, funding, and self-regulatory frameworks. Deploy systems that generate documented discriminatory and exploitative outcomes now, while directing safety budgets and PR toward speculative long-horizon alignment work that requires no near-term operational change. Can relocate labor pipelines and reclassify workers across jurisdictions to avoid regulatory exposure.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, large_ai_developers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, large_ai_developers, beneficiary).

% Receive substantial philanthropic and corporate funding to study speculative long-horizon alignment failure modes. Benefit from a discourse environment where 'AI safety' is defined broadly enough to include their work as central, drawing attention and resources away from near-term harm auditing without needing to demonstrate any present accountability.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, existential_risk_research_institutes, beneficiary,
    organized, civilizational, mobile, global).

% Review the most traumatic outputs of deployed systems for subsistence wages, often through opaque subcontracting chains that shield the platform from labor law. Exit means losing income with few comparable alternatives; the psychological cost of the work is treated as a private burden rather than a safety metric.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, gig_content_moderators, payer,
    powerless, immediate, trapped, global).

% Are filtered, scored, or rejected by opaque hiring and lending algorithms that reproduce and sometimes amplify historical discrimination. Rarely learn a model was involved in the decision, let alone can contest it; alternative application channels are increasingly scarce as automated screening becomes the default.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, algorithmically_screened_job_applicants, payer,
    powerless, biographical, trapped, national).

% Are disproportionately misclassified by predictive policing, credit, healthcare, and welfare-eligibility systems trained on biased historical data. Bear compounding harm across institutions with no unified recourse; the harms are documented in peer-reviewed audits yet rarely trigger deployment changes.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_communities_facing_discriminatory_scoring, payer,
    powerless, generational, trapped, national).

% Label and filter training data, including graphic and abusive content, for wages far below the value captured downstream. Work through layered contractors that obscure the client relationship, making it difficult to organize or to attribute harm to a specific deploying company.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, data_annotation_workers_global_south, payer,
    powerless, immediate, trapped, global).

% Attempt to mandate transparency, bias audits, and labor protections but are frequently out-resourced, out-lobbied, or handed voluntary self-assessment frameworks in place of binding rules. Often lack technical access to model internals or training data provenance needed to verify claims.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, regulators_and_auditors, excluded,
    moderate, biographical, constrained, national).

% Track how safety funding, legislative attention, and corporate messaging allocate across near-term harm categories versus speculative long-horizon risk, documenting the divergence between where documented harm occurs and where resources concentrate.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__near_term_harms_reading, large_ai_developers).
narrative_ontology:fixing_cost_class(ai_safety_commitment__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely coordinates a shared vocabulary and set of technical practices (bias testing, red-teaming, incident reporting) that some developers use to reduce discriminatory outcomes and labor abuses in deployed systems, and gives regulators and civil society a common frame to demand accountability.
% TRANSFER_FUNCTION: Moves attention, funding, and regulatory capital away from auditing, labor standards enforcement, and remediation for documented present-day harms, and toward speculative long-horizon research and voluntary self-governance frameworks that companies control; the underlying cost of discriminatory and exploitative deployment is transferred onto the powerless populations who bear it, largely uncompensated.
% ABSENT_VOICES: Gig moderators, data annotation workers, and communities subject to discriminatory algorithmic scoring are rarely present in the standards bodies, model cards, or safety-framework drafting processes that define what counts as an AI safety commitment; they would object that 'safety' should be measured by audited harm reduction in deployed systems, not by internal red-teaming disclosures or long-horizon research budgets.
% DISAPPEARANCE_RATIONALE: If the near-term-harms framing of AI safety vanished, developers argue existing internal review processes and market pressure would continue producing similar outcomes (world_unchanged position); labor advocates, discrimination litigants, and independent auditors argue the framing is the primary lever currently forcing any disclosure or bias testing at all, and its disappearance would remove what little accountability infrastructure exists (world_rearranges position) — the parties dispute which is true, and no neutral resolution mechanism currently adjudicates it.
% FOUNDING_PROBLEM: Deployed machine learning systems were producing measurable, documented harm — discriminatory hiring and lending outcomes, exploitative content moderation and data labeling supply chains, and large-scale misinformation amplification — well before any speculative alignment failure had occurred, and no existing regulatory or auditing infrastructure was tracking or correcting it.
% FOUNDING_PROBLEM_CORROBORATION: Independent academic audits (e.g., peer-reviewed algorithmic bias studies), investigative journalism on outsourced content-moderation and data-annotation labor conditions, and government agency findings (equal-opportunity and labor regulators in multiple jurisdictions) corroborate that documented near-term harms are ongoing and under-addressed — this attestation comes from outside the developers and the existential-risk research institutes who benefit from the current resource allocation.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, contested).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__near_term_harms_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68) and its rising trajectory reflect an accumulating gap between the scale of documented, audited harm and the resources or binding rules directed at fixing it — the harm compounds as more high-stakes automated decisions are deployed while accountability infrastructure lags. Theater ratio (0.52, rising past the 0.5 Goodhart-drift threshold) captures the growing prevalence of voluntary self-assessment, model cards, and safety pledges that substitute for audited, enforceable harm reduction. Suppression (0.58) is structural: opaque subcontracting chains, non-disclosure of algorithmic decision-making to affected individuals, and lobbying against binding audit requirements actively foreclose the exits and remedies that would otherwise be available. Accessibility collapse (0.45) is moderate rather than severe — some jurisdictions have won transparency mandates and litigation footholds, so alternatives have not fully collapsed. Resistance (0.62) is substantial: labor organizing among content moderators and annotation workers, discrimination litigation, and journalistic investigation are active countervailing forces.
 *
 * DIRECTIONALITY LOGIC:
 *   Large AI developers sit near the full-beneficiary end: institutional power, arbitrage-grade exit (they can relocate labor pipelines and reframe compliance across jurisdictions), and they administer the very framework that defines what safety work counts. Existential-risk research institutes are secondary beneficiaries — organized, mobile, civilizational time horizon — who gain resources and legitimacy from the broad 'safety' framing without bearing present-day accountability. The four victim groups are all powerless with trapped exit options and immediate-to-generational time horizons; the engine's directionality derivation should push their effective extraction toward the full-target end given this combination of powerlessness and lack of exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (documented present-day algorithmic harm) is live, not dead — this blocks a piton reading. But the tangled_rope classification is what prevents mislabeling this as pure extraction: there is a genuine coordination function (shared bias-testing vocabulary, incident-reporting norms, some real audits) operating alongside the asymmetric extraction. Collapsing this into a pure snare would erase the real, if partial, victories that near-term-harms advocacy has won (disclosure mandates, some litigation successes); calling it a pure rope would launder the ongoing, well-documented harm. Tangled rope holds both facts open at once.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_competition_vs_complementarity,
    'Is funding/attention for existential-risk alignment research structurally zero-sum with near-term harm auditing, or do they draw from largely separate funding pools such that one does not actually crowd out the other?',
    'Track philanthropic and corporate safety budgets over time, decomposed by category (long-horizon alignment research vs. near-term audit/compliance/labor remediation) and by funding source, to test whether increases in one category correlate with decreases in the other or come from distinct pools.',
    'If the pools are genuinely separate, the extraction claim weakens substantially — the near-term-harms reading''s core transfer_function claim (attention/funding diverted away from present-day harm) would not hold, and this constraint would look more like a rope with a parallel, non-competing research program. If the pools are shared or substitute for each other, the tangled_rope reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_competition_vs_complementarity, empirical, 'Whether long-horizon and near-term AI safety funding compete for the same resources.').

omega_variable(
    which_reading_is_the_real_kernel,
    'Is there a single underlying ''AI safety'' commitment that these three readings interpret differently, or are ''near-term harms,'' ''existential risk,'' and ''dual priority'' simply three different, unrelated policy agendas that share a label by rhetorical accident?',
    'Trace the genealogy of the term ''AI safety'' across research communities, funding announcements, and policy documents to determine whether a single commitment splintered into these readings or whether they arose independently and were later forced under one label.',
    'If the readings share a genuine common kernel, contest between them is a legitimate site of reading-dependent classification (as modeled here) and each reading''s ε remains valid on its own terms. If the label is a rhetorical accident with no shared kernel, the committer-frame structure itself (kernel + readings) may be the wrong model, and these should be treated as three unrelated constraints rather than three readings of one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_real_kernel, conceptual, 'Whether the three AI-safety readings share a genuine common kernel or only a label.').

omega_variable(
    self_regulation_durability,
    'Will current voluntary self-assessment and disclosure frameworks (model cards, voluntary bias audits, safety pledges) converge toward binding, externally verifiable standards, or will they remain permanently voluntary and developer-controlled?',
    'Observe whether binding legislative or regulatory mandates (not voluntary frameworks) requiring third-party audits and enforceable labor standards are adopted in major jurisdictions within the next several years, and whether enforcement actions follow.',
    'Convergence toward binding external standards would indicate the coordination function is strengthening relative to the extraction and could shift the classification toward rope over time; continued voluntary self-governance would confirm the tangled_rope reading and likely support a rising theater_ratio trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_regulation_durability, empirical, 'Whether AI safety self-governance will harden into binding external standards or remain voluntary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__near_term_harms_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ai_s_tr_t4, ai_safety_commitment__near_term_harms_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(ai_s_tr_t8, ai_safety_commitment__near_term_harms_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(ai_s_tr_t12, ai_safety_commitment__near_term_harms_reading, theater_ratio, 12, 0.44).
narrative_ontology:measurement(ai_s_tr_t16, ai_safety_commitment__near_term_harms_reading, theater_ratio, 16, 0.47).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__near_term_harms_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(ai_s_tr_t24, ai_safety_commitment__near_term_harms_reading, theater_ratio, 24, 0.52).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_s_be_t4, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(ai_s_be_t8, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(ai_s_be_t12, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(ai_s_be_t16, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ai_s_be_t24, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ai_s_su_t4, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(ai_s_su_t8, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(ai_s_su_t12, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(ai_s_su_t16, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(ai_s_su_t20, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(ai_s_su_t24, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__dual_priority_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the ai_safety_commitment kernel, each authored as a separate ε-invariant constraint per the decomposition principle. near_term_harms_reading (this file) authors a high ε (0.68) reflecting substantial, documented, ongoing extraction from powerless populations via labor exploitation and algorithmic discrimination, with the coordination function partially captured by resource diversion toward speculative research. existential_risk_reading is expected to author a very different beneficiary/victim structure (the research institutes as the coordinated, non-extractive party, with no comparable present-day victim set) and a different ε. dual_priority_reading is expected to sit structurally between the two, denying the zero-sum transfer claim this reading makes. All three should be linked bidirectionally via affects_constraints, since resource allocation to one reading's priorities structurally affects the others' operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
