% ============================================================================
% CONSTRAINT STORY: employment_boundary__hybrid_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__hybrid_security_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: employment_boundary__hybrid_security_reading
 *   human_readable: Platform Worker Hybrid Security Status (Third Category Reading)
 *   domain: labor/social_policy
 *
 * SUMMARY:
 *   This constraint embodies ONE READING of the contested employment boundary
 *   kernel: the view that platform workers occupy a third category requiring
 *   tailored protections distinct from both employment and independent
 *   contracting. This reading institutionalizes a hybrid status that extends
 *   basic medical and injury insurance (91.5% and 86.2% coverage
 *   respectively) while explicitly excluding career development, retirement
 *   security, and job security guarantees. The constraint is claimed as
 *   tangled_rope because it genuinely solves coordination (algorithmic task
 *   matching at scale) while simultaneously extracting from workers through
 *   precarity institutionalization. The other readings of this kernel
 *   (formalist_employment_reading and substantive_employment_reading) are
 *   separate constraints with different ε values and different stakeholder
 *   structures — this story instantiates only the hybrid reading's structural
 *   logic.
 *
 * KEY AGENTS:
 *   - platform_operators: institutional power, arbitrage exit — set the classification framework, operate the insurance systems, benefit from reduced labor cost relative to full employment while appearing protective
 *   - platform_workers_receiving_protections: moderate power, constrained exit — access basic protections but excluded from career and security benefits; sit at a boundary between benefit and precarity
 *   - platform_workers_unprotected_categories: powerless, trapped exit — fall outside even the hybrid protections; subsidize platform economics while bearing full individual risk
 *   - traditional_employment_workers: organized power, constrained exit — absorb structural cost as labor market normalizes around precarity; union leverage declines as work becomes platform-mediated
 *   - regulatory_agencies: institutional power, analytical exit — adjudicate the category, enforce minimum protections, navigate pressure from platforms and labor advocates
 *   - labor_advocates: organized power, constrained exit — excluded from governance; see hybrid status as institutional capture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.58).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.52).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Platform Worker Hybrid Security Status (Third Category Reading)").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, 'd05826b9-3368-4241-8f7a-d37276c0ae44').
narrative_ontology:cs_kernel_codification('d05826b9-3368-4241-8f7a-d37276c0ae44', distributed).
narrative_ontology:cs_authority_grounding('d05826b9-3368-4241-8f7a-d37276c0ae44', extraction).
narrative_ontology:cs_interpretation_layer_present('d05826b9-3368-4241-8f7a-d37276c0ae44').
narrative_ontology:cs_reading_relation('d05826b9-3368-4241-8f7a-d37276c0ae44', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('d05826b9-3368-4241-8f7a-d37276c0ae44', employment_boundary__substantive_employment_reading, coexists_with).
narrative_ontology:cs_axiom('d05826b9-3368-4241-8f7a-d37276c0ae44', foundational, platform_work_requires_third_category).
narrative_ontology:cs_axiom_status(platform_work_requires_third_category, holdable).
narrative_ontology:cs_axiom_grounding('d05826b9-3368-4241-8f7a-d37276c0ae44', platform_work_requires_third_category, empirically_contingent).
narrative_ontology:cs_axiom('d05826b9-3368-4241-8f7a-d37276c0ae44', foundational, basic_protections_preserve_coordination).
narrative_ontology:cs_axiom_status(basic_protections_preserve_coordination, holdable).
narrative_ontology:cs_axiom_grounding('d05826b9-3368-4241-8f7a-d37276c0ae44', basic_protections_preserve_coordination, instrumental).
narrative_ontology:cs_reference_frame('d05826b9-3368-4241-8f7a-d37276c0ae44', algorithmic_task_matching_labor).
narrative_ontology:cs_drift_state('d05826b9-3368-4241-8f7a-d37276c0ae44', contemporary_regulatory_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d05826b9-3368-4241-8f7a-d37276c0ae44', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_workers_receiving_protections).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers_unprotected_categories).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, traditional_employment_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers_receiving_protections).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set classification frameworks that define platform workers as a distinct hybrid category. Operate mandatory injury insurance and medical coverage systems while resisting full employment classification and associated benefit obligations. Control the algorithmic assignment and rating systems that determine work availability. Benefit from the partial classification because it reduces total labor cost while satisfying regulatory pressure for basic protections.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive medical coverage (91.5% access) and injury insurance (86.2% access) through platform-provided mechanisms. Remain excluded from retirement plan enrollment, career progression structures, paid leave, and job security guarantees that full employees receive. Face algorithmic deactivation without recourse. Benefit from protection floor that independent contractors lack; bear costs of precarity that employees avoid.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers_receiving_protections, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, platform_workers_receiving_protections, payer).

% Fall outside even the hybrid protections: gig workers in lower-tier platforms, part-time threshold workers, international workers, workers in jurisdictions without enforcement. Receive neither the basic protections nor the employment guarantees. Absorb risk entirely individually while subsidizing platform economics through below-market rate structures justified by 'flexibility' framing.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers_unprotected_categories, payer,
    powerless, biographical, trapped, global).

% Maintain full employment status with comprehensive benefits but face labor market contraction as platform work absorbs employment volume. Tax contributions fund social safety net that platforms externalize costs into. Union organizing becomes harder as work becomes platform-mediated. Bear structural cost as labor normalization shifts away from traditional employment.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, traditional_employment_workers, payer,
    organized, generational, constrained, national).

% Establish the hybrid category through legislation, regulation, and enforcement of minimum protections. Adjudicate which platforms and worker classes qualify for hybrid status. Navigate political pressure from platforms (against expansion) and labor advocates (against contraction). Frame hybrid status as innovation protecting vulnerable workers while managing employment classification pressure.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, regulatory_agencies, observer).

% Argue for full employment classification and comprehensive union-negotiated benefits rather than platform-designed hybrid status. See the third category as institutional capture that locks workers out of power-backed protections while appearing to protect them. Would fundamentally reclassify the relationship if admitted to governance.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_advocates, excluded,
    organized, generational, constrained, national).

% Manage costs of platform worker precarity through unemployment insurance, healthcare supplements, disability claims, and emergency assistance programs. Observe growth in means-tested benefit enrollment among workers classified as independent/hybrid. Track labor market fragmentation as employment normalcy erodes.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, social_safety_net_administrators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__hybrid_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves platform scalability and algorithmic labor matching: enables rapid task-to-worker assignment without the coordination overhead of full employment administration (hiring, firing, training, management layers). Provides workers with partial insurance against injury and medical need without requiring employer contributions to retirement, which would reduce scale-adjusted margins.
% TRANSFER_FUNCTION: Moves: (1) labor surplus from platform workers to platform operators (below-market rate structures justified by flexibility); (2) insurance and medical cost absorption downward from full-employment norms to partial-platform-provided coverage; (3) retirement and career-progression risk from platforms to workers; (4) tax burden upward to public safety net for benefit supplements and unemployment insurance gaps.
% ABSENT_VOICES: Full-time union-organized labor (excluded from platform economy governance), workers in lower-tier platforms receiving no protections (invisible to regulation), workers in non-compliant jurisdictions (outside regulatory scope), future workers whose career pathways are foreclosed by normalization of platform precarity.
% DISAPPEARANCE_RATIONALE: If the hybrid third-category framework disappeared, platform operators would either absorb workers into full employment (shifting labor cost upward and reducing platform scalability, restructuring the business model) or reclassify as pure independent contractors (removing all protections, creating pressure for public-system safety net expansion). Labor markets would either normalize toward employment or bifurcate more sharply toward independent/unprotected status. The regulatory category sustains the current economic model; removing it forces organizational choice.
% FOUNDING_PROBLEM: Platform work emerged as a labor form that was genuinely novel: algorithmic task matching, on-demand availability, no fixed workplace or team. Pure independent contractor status exposed workers to total risk; full employment classification was structurally awkward for work that is episodic and demand-driven. The founding problem is: how to protect episodic workers without destroying the coordination advantages of platform matching.
% FOUNDING_PROBLEM_CORROBORATION: Platforms attest the founding problem is live and justify hybrid status as the solution. Labor economists and public health researchers document the protection gap: workers in hybrid status show better health and income stability than pure contractors but persistent deficits in career development, retirement readiness, and job security relative to full employees. Regulatory agencies cite the founding problem as the reason the category exists. Labor advocates contest whether the founding problem could be solved through full employment with flexible scheduling, rather than through a precarity-entrenching category. Independent research (Kellogg et al., Rahman) from outside the platform sector supports the contested reading.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__hybrid_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__hybrid_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.48–0.58 across the interval, reflecting the constraint's core dynamic: genuine coordination function (platform matching, on-demand flexibility) paired with asymmetric extraction (workers bear precarity cost, platforms avoid full-employment expense). The trajectory shows modest growth (0.48→0.58) as platforms expand worker populations into lower-tier structures with thinner protections, raising effective extraction even as headline medical coverage holds steady. Theater_ratio climbs to 0.49 by period 15, then holds, indicating growing performative framing ('worker-friendly protections') relative to functional change. Suppression_requirement (structural barriers preventing exit: algorithmic deactivation, limited alternative platforms, credential lock-in) remains stable at 0.46–0.53, reflecting the constraint's persistent coercive infrastructure. The measurement grid is shared across all three metrics — every metric is authored at every time point so temporal analysis has complete data.
 *
 * PERSPECTIVAL GAP:
 *   Platform operators perceive this as coordination achievement (genuine solution to algorithmic labor matching) justified by market realities; workers in the protected cohort perceive it as partial protection riding on precarity; workers outside the protections perceive it as exclusion; traditional workers perceive it as labor market erosion; labor advocates perceive it as regulatory capture preventing full-employment classification. The engine computes these divergences from power, exit options, and beneficiary/victim positioning — the narrative gap IS the constraint's core asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are beneficiaries (set the framework, control the insurance pools, reduce total labor cost) with directionality near 0.0 (subsidy). Workers receiving protections sit near 0.5 (symmetric: real benefits, real costs). Workers unprotected sit near 1.0 (full targets). Traditional workers sit near 0.8 (bear structural cost through labor market fragmentation). The constraint's persistence depends on regulatory enforcement (active exclusion of full-employment reclassification) and suppression (algorithmic deactivation preventing exit), so d values are modulated upward for targets relative to what independent exit options alone would suggest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to protect episodic workers without destroying platform matching) is CONTESTED, not dead. Platforms attest it remains live; labor advocates argue it could be solved through full employment with flexible scheduling (implying the hybrid status choice is not structurally necessary). This contest prevents mandatrophy certification — the constraint is not an atrophied function maintained theatrically, but an active choice between different solutions to a real problem. The theater_ratio growth (0.38→0.49) reflects rising performative framing, but the function persists. Classification remains tangled_rope, not piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    protection_sufficiency_boundary,
    'Is the medical and injury insurance coverage (91.5% and 86.2% respectively) sufficient to constitute meaningful protection, or does the exclusion of retirement, career development, and job security render the protections largely performative?',
    'Longitudinal tracking of worker financial security, retirement readiness, income volatility, and comparison to full employees and pure contractors over 10+ year cohorts. Analysis of whether hybrid-status workers converge toward employee or contractor outcomes.',
    'If protections prove insufficient (workers converge to contractor outcomes despite coverage), the theater_ratio rises sharply and classification shifts toward snare. If sufficient (workers achieve meaningful stability gains), the reading''s viability as a genuine compromise strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_sufficiency_boundary, empirical, 'Whether the scope of hybrid protections achieves material worker security or functions as cover story for precarity.').

omega_variable(
    coordination_independence_of_precarity,
    'Could the algorithmic coordination function (task matching, on-demand availability) be delivered through full-employment status with flexible scheduling, or is precarity structurally required for the platform model to operate economically?',
    'Natural experiment from jurisdictions that mandate employment classification for platform workers and observe whether platforms maintain service quality and scale, or whether the business model requires precarity.',
    'If coordination survives under employment classification, the extraction component is disaggregated from coordination and the reading collapses toward snare (pure extraction). If coordination materially degrades, the reading''s tangled_rope framing is vindicated (true coordination/extraction hybrid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_independence_of_precarity, conceptual, 'Whether platform coordination fundamentally requires precarious labor status or whether precarity is economically chosen.').

omega_variable(
    regulatory_capture_vs_worker_voice_exclusion,
    'Is the hybrid category sustained primarily by platform political power over regulators (regulatory capture), or by deliberate regulatory choice to exclude labor voice from governance (worker voice exclusion)?',
    'Institutional analysis of regulatory decision-making, legislative testimony, regulatory comment periods, and whether platforms or workers had greater access to standard-setting. Review whether organized labor was seated in the category''s design.',
    'Capture-dominant narrative suggests the constraint is more snare-like (extracted through institutional power). Voice-exclusion narrative suggests the constraint is genuinely tangled (regulators made a deliberate choice, excluding one party intentionally). This distinction affects how mandatrophy resolution would proceed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_vs_worker_voice_exclusion, empirical, 'Whether the institutional structure reflects capture or deliberate voice exclusion.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the institutionalization of the hybrid third category logically or institutionally foreclose the substantive_employment_reading (economic dependence → employment), or do they remain live alternative readings under pressure?',
    'Review whether jurisdiction-level adoption of hybrid category prevents or merely delays substantive employment reclassification. Examine whether hybrid status persists when political power shifts or whether it collapses under labor pressure.',
    'If foreclosure is real and structural, the reading_relations entry should be forecloses rather than coexists_with. If reading remains contestable, coexists_with is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether this reading''s institutional success logically rules out competing readings or merely represents one live option.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__hybrid_security_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(empl_tr_t0, observed).
narrative_ontology:measurement(empl_tr_t3, employment_boundary__hybrid_security_reading, theater_ratio, 3, 0.41).
narrative_ontology:measurement_basis(empl_tr_t3, observed).
narrative_ontology:measurement(empl_tr_t6, employment_boundary__hybrid_security_reading, theater_ratio, 6, 0.44).
narrative_ontology:measurement_basis(empl_tr_t6, observed).
narrative_ontology:measurement(empl_tr_t10, employment_boundary__hybrid_security_reading, theater_ratio, 10, 0.47).
narrative_ontology:measurement_basis(empl_tr_t10, observed).
narrative_ontology:measurement(empl_tr_t15, employment_boundary__hybrid_security_reading, theater_ratio, 15, 0.49).
narrative_ontology:measurement_basis(empl_tr_t15, observed).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__hybrid_security_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(empl_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__hybrid_security_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(empl_be_t0, observed).
narrative_ontology:measurement(empl_be_t3, employment_boundary__hybrid_security_reading, base_extractiveness, 3, 0.51).
narrative_ontology:measurement_basis(empl_be_t3, observed).
narrative_ontology:measurement(empl_be_t6, employment_boundary__hybrid_security_reading, base_extractiveness, 6, 0.54).
narrative_ontology:measurement_basis(empl_be_t6, observed).
narrative_ontology:measurement(empl_be_t10, employment_boundary__hybrid_security_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(empl_be_t10, observed).
narrative_ontology:measurement(empl_be_t15, employment_boundary__hybrid_security_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(empl_be_t15, observed).
narrative_ontology:measurement(empl_be_t20, employment_boundary__hybrid_security_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(empl_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__hybrid_security_reading, suppression_requirement, 0, 0.46).
narrative_ontology:measurement_basis(empl_su_t0, observed).
narrative_ontology:measurement(empl_su_t3, employment_boundary__hybrid_security_reading, suppression_requirement, 3, 0.48).
narrative_ontology:measurement_basis(empl_su_t3, observed).
narrative_ontology:measurement(empl_su_t6, employment_boundary__hybrid_security_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement_basis(empl_su_t6, observed).
narrative_ontology:measurement(empl_su_t10, employment_boundary__hybrid_security_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(empl_su_t10, observed).
narrative_ontology:measurement(empl_su_t15, employment_boundary__hybrid_security_reading, suppression_requirement, 15, 0.53).
narrative_ontology:measurement_basis(empl_su_t15, observed).
narrative_ontology:measurement(empl_su_t20, employment_boundary__hybrid_security_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(empl_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(employment_boundary__hybrid_security_reading, 0.18).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, employment_boundary__substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, platform_labor_supply_elasticity).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, social_safety_net_supplementation).

% DUAL FORMULATION NOTE:
% This constraint is part of the employment_boundary kernel family with two sibling readings. The formalist_employment_reading treats platform workers as independent contractors (low ε, no extraction, mountain-like). The substantive_employment_reading treats them as employees under algorithmic control (high ε, snare-like). This hybrid_security_reading occupies the middle: moderate ε, tangled coordination/extraction. Each reading has its own ε-invariant constraint story because the observables (what counts as 'employment') yield materially different extraction profiles. The family is linked through network.affects_constraints so contamination analysis can track how one reading's adoption affects the others' institutional pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__hybrid_security_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
