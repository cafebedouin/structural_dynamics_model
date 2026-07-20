% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__existential_risk_reading, []).

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
 *   constraint_id: ai_safety_commitment__existential_risk_reading
 *   human_readable: AI Safety as Existential Risk Prevention Commitment
 *   domain: technological/governance
 *
 * SUMMARY:
 *   This constraint instantiates the existential risk reading of the
 *   contested AI safety commitment kernel: the institutionalized definitional
 *   frame that AI safety means preventing extinction-level outcomes from
 *   misaligned superintelligent systems. It is one of three structurally
 *   distinct readings (alongside near-term harms and dual-priority readings)
 *   that decompose the colloquial label 'AI safety' into separate constraints
 *   with different Îµ values, beneficiary structures, and classifications.
 *   The kernel is distributed across papers, funding announcements, and
 *   institutional strategies rather than fixed in a single canonical text.
 *   KEY AGENTS (by structural relationship): x_risk_research_institutions
 *   (agenda_setter/institutional/constrained) â define and administer the
 *   frame; frontier_ai_labs
 *   (beneficiary/agenda_setter/institutional/arbitrage) â capture
 *   legitimacy and resources; longtermist_funders
 *   (beneficiary/powerful/mobile) â validate and finance the frame;
 *   near_term_harm_communities (payer/powerless/trapped) â bear costs of
 *   exclusion; algorithmic_accountability_researchers
 *   (payer/moderate/constrained) â marginalized expertise;
 *   global_south_civil_society (excluded/powerless/trapped) â absent
 *   voices; ai_governance_policymakers (observer/institutional/constrained)
 *   â contested adoption.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: Primary agenda_setter (institutional/civilizational/constrained) â controls funding and epistemic norms
 *   - frontier_ai_labs: Primary beneficiary with secondary agenda-setting role (institutional/generational/arbitrage) â scales capabilities while claiming safety
 *   - longtermist_funders: Secondary beneficiary (powerful/civilizational/mobile) â capital validation
 *   - near_term_harm_communities: Primary target (powerless/immediate/trapped) â bears present-harm costs of definitional exclusion
 *   - algorithmic_accountability_researchers: Secondary target (moderate/biographical/constrained) â expertise excluded from safety funding
 *   - global_south_civil_society: Excluded voice (powerless/immediate/trapped) â absent from governance
 *   - ai_governance_policymakers: Analytical observer (institutional/generational/constrained) â contested policy adoption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, 0.71).
domain_priors:suppression_score(ai_safety_commitment__existential_risk_reading, 0.64).
domain_priors:theater_ratio(ai_safety_commitment__existential_risk_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, resistance, 0.56).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__existential_risk_reading, "AI Safety as Existential Risk Prevention Commitment").
narrative_ontology:topic_domain(ai_safety_commitment__existential_risk_reading, "technological/governance").

domain_priors:requires_active_enforcement(ai_safety_commitment__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__existential_risk_reading, 'c61724c5-bb94-47b0-887f-fecdb46cfa55').
narrative_ontology:cs_kernel_codification('c61724c5-bb94-47b0-887f-fecdb46cfa55', distributed).
narrative_ontology:cs_authority_grounding('c61724c5-bb94-47b0-887f-fecdb46cfa55', expertise).
narrative_ontology:cs_interpretation_layer_present('c61724c5-bb94-47b0-887f-fecdb46cfa55').
narrative_ontology:cs_reading_relation('c61724c5-bb94-47b0-887f-fecdb46cfa55', ai_safety_commitment__near_term_harms_reading, influences).
narrative_ontology:cs_reading_relation('c61724c5-bb94-47b0-887f-fecdb46cfa55', ai_safety_commitment__dual_priority_reading, coexists_with).
narrative_ontology:cs_axiom('c61724c5-bb94-47b0-887f-fecdb46cfa55', foundational, extinction_preemption_supersedes_all).
narrative_ontology:cs_axiom_status(extinction_preemption_supersedes_all, holdable).
narrative_ontology:cs_axiom_grounding('c61724c5-bb94-47b0-887f-fecdb46cfa55', extinction_preemption_supersedes_all, empirically_contingent).
narrative_ontology:cs_axiom('c61724c5-bb94-47b0-887f-fecdb46cfa55', foundational, superintelligence_forecast_actionable).
narrative_ontology:cs_axiom_status(superintelligence_forecast_actionable, holdable).
narrative_ontology:cs_axiom_grounding('c61724c5-bb94-47b0-887f-fecdb46cfa55', superintelligence_forecast_actionable, empirically_contingent).
narrative_ontology:cs_reference_frame('c61724c5-bb94-47b0-887f-fecdb46cfa55', longtermist_existential_security).
narrative_ontology:cs_drift_state('c61724c5-bb94-47b0-887f-fecdb46cfa55', post_frontier_scaling_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c61724c5-bb94-47b0-887f-fecdb46cfa55', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__existential_risk_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, near_term_harm_communities).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, algorithmic_accountability_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, global_south_civil_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the global AI safety research agenda, prioritize technical alignment and interpretability work, control grantmaking and conference programs, and establish the epistemic norms that determine what counts as legitimate safety research. Their institutional identity is fused with the existential risk frame.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, civilizational, constrained, global).

% Conduct and fund technical safety research framed around existential risk while simultaneously scaling frontier model capabilities. Use the safety commitment to legitimize continued development and resist binding external regulation. Can pivot messaging if the frame becomes liability.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, frontier_ai_labs, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, frontier_ai_labs, agenda_setter).

% Direct large-scale philanthropic capital toward existential risk research and longtermist policy, prioritizing speculative future outcomes over present welfare. Their grant criteria validate and propagate the exclusive existential risk definition of safety.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, longtermist_funders, beneficiary,
    powerful, civilizational, mobile, global).

% Bear documented algorithmic harms including discriminatory scoring, exploitative labor conditions in data supply chains, and misinformation impacts. These harms are structurally excluded from the AI safety frame, leaving them without access to the funding and policy attention diverted to existential risk.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, near_term_harm_communities, payer,
    powerless, immediate, trapped, global).

% Research present-day algorithmic harms, bias, and labor exploitation but are systematically excluded from AI safety funding streams, top-tier safety conferences, and elite governance forums. Their work is categorized as fairness or ethics rather than safety, limiting career paths and institutional support.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, algorithmic_accountability_researchers, payer,
    moderate, biographical, constrained, national).

% Experience the most severe present-day impacts of AI deployment but are absent from existential risk discourse that centers wealthy nations and hypothetical future populations. Their policy priorities and lived experiences are not represented in safety governance architectures.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, global_south_civil_society, excluded,
    powerless, immediate, trapped, global).

% Develop AI governance frameworks under pressure from competing definitions of safety. Some adopt the existential risk framing pushed by expert institutions; others seek to broaden the agenda to include near-term harms but face resistance from dominant funding and discourse networks.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, ai_governance_policymakers, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__existential_risk_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_safety_commitment__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global research attention, philanthropic funding, and governance efforts around the prevention of catastrophic misalignment in hypothetical future superintelligent systems, addressing a theorized collective-action problem where unilateral competitive development could impose extinction risk on all of humanity.
% TRANSFER_FUNCTION: Moves financial capital, research labor, and definitional authority over the term AI safety from present-day algorithmic accountability and near-term harm mitigation toward speculative technical alignment research, interpretability, and longtermist governance structures.
% ABSENT_VOICES: Communities experiencing present-day algorithmic harms including data workers, subjects of automated decision systems, and Global South populations are structurally excluded from the safety conversation. Algorithmic accountability researchers who dispute the threat model or resource allocation are marginalized in funding and high-status discourse despite holding relevant expertise.
% DISAPPEARANCE_RATIONALE: If the exclusive existential risk framing disappeared overnight, AI safety funding would redistribute toward present harms, research agendas and conferences would diversify, governance forums would reconstitute around different threat models, and the institutional power of frontier labs to justify unchecked scaling through safety branding would weaken substantially.
% FOUNDING_PROBLEM: The theoretical prospect that artificial general intelligence or superintelligent systems could cause human extinction, with competitive pressures among developers potentially racing toward unsafe systems absent coordinated technical or governance solutions.
% FOUNDING_PROBLEM_CORROBORATION: The x-risk research community and frontier AI lab leadership attest the problem is urgent and live. Critics from algorithmic justice, social science, independent AI ethics bodies, and some Global South policy scholars attest the problem is either overstated, poorly specified, or less urgent than documented present harms; systematic reviews of AI forecasting track records and independent sociological analyses of field formation support the contested reading.
narrative_ontology:disappearance_verdict(ai_safety_commitment__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__existential_risk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__existential_risk_reading, 0.71, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.71) is high because the constraint channels massive resources toward speculative technical interventions with unproven efficacy while systematically deprioritizing documented present harms. Suppression (0.64) reflects active enforcement of the definitional boundary through funding gatekeeping, conference curation, and hiring norms rather than legal coercion. Theater ratio (0.46) captures the growing performative component: frontier labs increasingly adopt safety signaling to legitimize capability scaling without accepting binding constraints. Accessibility collapse (0.60) indicates that once inside the x-risk institutional ecosystem, alternative approaches become nearly invisible in high-status discourse despite their visibility outside it. Resistance (0.56) reflects substantial pushback from algorithmic justice movements, accountability researchers, and some policymakers. Temporal measurements trace the frame's institutional capture from 2015-2025: extractiveness and theater rose monotonically as the frame captured more funding and commercial labs adopted it, while suppression requirement rose as the boundary was actively defended against near-term challengers.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter and beneficiary seats (x-risk institutions, frontier labs, longtermist funders) experience the constraint as genuine coordination around an existential threat requiring technical and governance solutions. The payer and excluded seats (near-term communities, accountability researchers, Global South civil society) experience the same constraint as an extractive definitional lockout that diverts resources and legitimacy away from their lived harms. The engine computes this divergence from the structural data: beneficiaries with arbitrage or constrained exit sit near the low-d end, while victims with trapped or constrained exit sit near the high-d end. The claimed type (tangled_rope) does not adjudicate this gap; it names the structural condition that both experiences are partially true.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations combined with exit options. The three beneficiary agents (x_risk_research_institutions, frontier_ai_labs, longtermist_funders) collect resources, legitimacy, or definitional authority from the constraint and have constrained-to-arbitrage exit, placing them at the low-d beneficiary end. The three victim/excluded agents (near_term_harm_communities, algorithmic_accountability_researchers, global_south_civil_society) bear the costs of resource diversion and epistemic marginalization, with trapped or constrained exit placing them at the high-d target end. Frontier_ai_labs have arbitrage-grade exit (can pivot narratives), which further dampens their effective extraction; near_term_harm_communities are identity_locked through the immediacy of their harm experience with no exit, amplifying their effective extraction. No directionality overrides are needed because the structural derivation accurately captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists mandatrophy mislabeling because it genuinely coordinates a real collective-action problem (preventing misaligned superintelligence) while simultaneously extracting from identifiable victims (present-harm communities and excluded researchers). A pure snare reading would ignore the coordination function; a pure rope reading would ignore the asymmetric extraction. The tangled_rope classification is warranted by the simultaneous presence of beneficiaries (proving coordination), victims (proving asymmetric extraction), and active enforcement (proving the definitional boundary must be maintained against competing readings). The founding problem status is contested, not dead, because the underlying extinction risk claim remains live in discourse even as its resource allocation is challenged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extinction_risk_empirical_status,
    'Is the predicted extinction risk from future superintelligent systems empirically grounded at a level that warrants the current resource concentration, or is the probability estimate too speculative to justify the extraction from present harms?',
    'Track record analysis of AI forecasting, independent empirical assessment of alignment progress relative to capability growth, and historical comparison of existential risk predictions.',
    'If the risk is substantially overstated, the constraint is predominantly extractive and the coordination function is weaker than claimed; if well-grounded, the extraction may be the necessary price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extinction_risk_empirical_status, empirical, 'Empirical grounding of the core extinction prediction').

omega_variable(
    safety_washing_vs_genuine_coordination,
    'Does the existential risk frame enable frontier labs to perform safety theater while scaling capabilities, or does it impose genuine binding constraints on development?',
    'Audit lab conduct against stated safety commitments: measure capability scaling versus safety investment ratios, and track whether governance proposals advocated by labs would actually restrict their own development trajectories.',
    'If safety theater dominates, theater_ratio rises and the constraint approaches snare; if binding constraints are real, the coordination function is stronger than the metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_washing_vs_genuine_coordination, empirical, 'Whether safety commitments bind labs or serve as cover').

omega_variable(
    kernel_naturalness_ambiguity,
    'Is the existential risk reading a discovered natural priority for civilization, or a constructed definitional constraint that emerged from specific institutional and philanthropic investments?',
    'Historical sociology of the AI safety field: trace funding flows, institutional founding dates, and discourse evolution to determine whether the reading emerged organically from technical necessity or was constructed by identifiable actors and capital sources.',
    'If constructed, the constraint triggers false-summit detection dynamics and reclassifies toward the extractive end of the tangled rope spectrum; if natural, it would approach mountain immunity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_naturalness_ambiguity, conceptual, 'Whether the x-risk priority is natural or constructed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__existential_risk_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__existential_risk_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_s_tr_t2, ai_safety_commitment__existential_risk_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement(ai_s_tr_t4, ai_safety_commitment__existential_risk_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(ai_s_tr_t6, ai_safety_commitment__existential_risk_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement(ai_s_tr_t8, ai_safety_commitment__existential_risk_reading, theater_ratio, 8, 0.41).
narrative_ontology:measurement(ai_s_tr_t10, ai_safety_commitment__existential_risk_reading, theater_ratio, 10, 0.46).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__existential_risk_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_s_be_t2, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(ai_s_be_t4, ai_safety_commitment__existential_risk_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(ai_s_be_t6, ai_safety_commitment__existential_risk_reading, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(ai_s_be_t8, ai_safety_commitment__existential_risk_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(ai_s_be_t10, ai_safety_commitment__existential_risk_reading, base_extractiveness, 10, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__existential_risk_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ai_s_su_t2, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2, 0.46).
narrative_ontology:measurement(ai_s_su_t4, ai_safety_commitment__existential_risk_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(ai_s_su_t6, ai_safety_commitment__existential_risk_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(ai_s_su_t8, ai_safety_commitment__existential_risk_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(ai_s_su_t10, ai_safety_commitment__existential_risk_reading, suppression_requirement, 10, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__existential_risk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__existential_risk_reading, 0.08).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__dual_priority_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_safety_commitment kernel, which decomposes into three structurally distinct claims: the existential risk reading (extinction prevention as safety), the near-term harms reading (present-day harms as safety), and the dual-priority reading (both as non-competing). Each reading has a different epsilon, beneficiary/victim structure, and classification. The upstream existential risk reading structurally influences both siblings by capturing funding and definitional authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
