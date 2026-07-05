% ============================================================================
% CONSTRAINT STORY: employment_boundary__hybrid_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: employment_boundary__hybrid_security_reading
 *   human_readable: Platform Worker Third-Category Hybrid Status Framework
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   Following years of litigation over whether app-based gig and delivery
 *   workers were employees or independent contractors, several jurisdictions
 *   adopted a statutory third category — 'platform worker' or 'dependent
 *   contractor' — granting a fixed bundle of protections (medical coverage at
 *   91.5% of claims, workplace injury insurance at 86.2%) while explicitly
 *   foreclosing full employment status (no unemployment insurance, no
 *   retirement contributions, no collective bargaining rights, no
 *   wage-and-hour floor). This story is the hybrid_security_reading of the
 *   contested employment_boundary kernel: it treats the third category as a
 *   genuine, if imperfect, coordination solution — distinct from the
 *   formalist_employment_reading (which would deny platform workers are in
 *   any employment relationship at all) and the
 *   substantive_employment_reading (which would treat algorithmic control and
 *   economic dependence as dispositive and classify them as full employees).
 *   Rising theater_ratio and base_extractiveness over the interval reflect
 *   the pattern observed in early-adopting jurisdictions: the initial
 *   coverage percentages were negotiated generously to secure political
 *   buy-in, but enforcement of the injury-insurance and medical-coverage
 *   guarantees has softened as platforms found compliance workarounds
 *   (classifying more hours as 'offline' time, restructuring shift
 *   boundaries) while the statutory foreclosure of the employment question
 *   has remained fully intact and increasingly cited in unrelated litigation.
 *
 * KEY AGENTS:
 *   - platform_workers: primary target (powerless/constrained) — receives partial protections but bears the cost of foreclosed full-employment claims
 *   - platform_operators: primary beneficiary (institutional/arbitrage) — fixed, lower-cost obligation replaces open-ended litigation exposure
 *   - policy_intermediary_agencies: secondary beneficiary (institutional/analytical) — institutional mandate and credit for resolving the dispute
 *   - labor_unions_and_worker_advocates: excluded voice (organized/constrained) — argued for substantive employment, lost the drafting fight
 *   - courts_and_labor_regulators: analytical observer (institutional/analytical) — enforces the compromise as written
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.52).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.48).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Platform Worker Third-Category Hybrid Status Framework").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, '32234a67-faed-45c3-8c13-33ccae8b66cc').
narrative_ontology:cs_kernel_codification('32234a67-faed-45c3-8c13-33ccae8b66cc', distributed).
narrative_ontology:cs_authority_grounding('32234a67-faed-45c3-8c13-33ccae8b66cc', distributed).
narrative_ontology:cs_reading_relation('32234a67-faed-45c3-8c13-33ccae8b66cc', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('32234a67-faed-45c3-8c13-33ccae8b66cc', employment_boundary__substantive_employment_reading, influences).
narrative_ontology:cs_axiom('32234a67-faed-45c3-8c13-33ccae8b66cc', foundational, platform_work_is_structurally_distinct_category).
narrative_ontology:cs_axiom_status(platform_work_is_structurally_distinct_category, holdable).
narrative_ontology:cs_axiom_grounding('32234a67-faed-45c3-8c13-33ccae8b66cc', platform_work_is_structurally_distinct_category, conventional).
narrative_ontology:cs_axiom('32234a67-faed-45c3-8c13-33ccae8b66cc', foundational, tailored_protection_without_full_employment_cost_is_just).
narrative_ontology:cs_axiom_status(tailored_protection_without_full_employment_cost_is_just, holdable).
narrative_ontology:cs_axiom_grounding('32234a67-faed-45c3-8c13-33ccae8b66cc', tailored_protection_without_full_employment_cost_is_just, instrumental).
narrative_ontology:cs_reference_frame('32234a67-faed-45c3-8c13-33ccae8b66cc', negotiated_tripartite_compromise).
narrative_ontology:cs_drift_state('32234a67-faed-45c3-8c13-33ccae8b66cc', post_enactment_five_year_mark, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('32234a67-faed-45c3-8c13-33ccae8b66cc', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, policy_intermediary_agencies).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_workers).
narrative_ontology:constraint_vindicates(employment_boundary__hybrid_security_reading, third_category_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive medical injury coverage (91.5%) and workplace injury insurance (86.2%) under the new hybrid designation, which is real and better than nothing. But they get no minimum wage floor tied to hours worked, no unemployment insurance, no retirement contribution, and no seniority-based career ladder — the hybrid category was built to give them just enough protection to answer the political pressure for full employment status without granting the cost structure of employment. Algorithmic deactivation remains largely unreviewable. They cannot bargain collectively as employees can, and the new category forecloses the litigation path that would have forced a substantive-employment finding.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, platform_workers, beneficiary).

% Lobbied for and helped draft the hybrid category as the alternative to being reclassified as employers. They fund the injury insurance pool and medical coverage — a real cost — but this is materially cheaper than payroll tax, unemployment insurance contributions, retirement matching, and overtime liability that full employment status would require. The hybrid designation is a durable legal shield: it forecloses future employee-misclassification suits by statute, converting an open legal question into a settled compromise that favors their cost structure.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, platform_operators, beneficiary).

% The agencies that designed and administer the third category gain a durable regulatory mandate, a new inspection and certification apparatus, and credit for 'solving' the platform labor question through a negotiated middle path. Their institutional survival is now tied to defending the hybrid category as the correct answer rather than a transitional compromise, which gives them an interest in the category's permanence independent of whether it serves workers well.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, policy_intermediary_agencies, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, policy_intermediary_agencies, observer).

% Compete for labor and capital against platform firms who now carry a lighter, statutorily fixed cost structure. They have no seat in the hybrid-category negotiations despite bearing a competitive disadvantage, and no formal channel to argue that the third category creates unfair labor-cost arbitrage across their industry.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, traditional_employers_in_competing_sectors, excluded,
    powerful, biographical, constrained, national).

% Advocated for full employment reclassification and lost that fight to the hybrid compromise. They continue to argue the third category is a permanent holding pattern that gives platforms the appearance of having addressed worker concerns while leaving the core economic dependence unaddressed. Their objections are on the public record but were not incorporated into the final category design.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_unions_and_worker_advocates, excluded,
    organized, generational, constrained, national).

% Adjudicate disputes under the new hybrid framework and monitor compliance with its specific protections (medical, injury insurance). They can observe whether the category functions as advertised or drifts toward theater, but their statutory mandate is now to enforce the compromise as written, not to revisit whether the underlying employment question was correctly resolved.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, courts_and_labor_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__hybrid_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a legally cognizable middle category so that platform workers are not left with zero protections under a pure independent-contractor reading, while giving platforms and regulators a stable, litigable status that ends years of case-by-case misclassification suits.
% TRANSFER_FUNCTION: Moves a bounded package of injury and medical coverage from platforms to workers, while foreclosing the larger transfer (unemployment insurance, retirement contributions, wage floors, collective bargaining rights) that a substantive-employment finding would have moved from platforms to workers.
% ABSENT_VOICES: Labor unions and worker advocates who pushed for full employment status were present in the debate but not in the final drafting; traditional employers bearing comparative labor-cost disadvantage were not consulted at all. Both would object that the hybrid category converts an open question into a settled compromise favorable to platforms.
% DISAPPEARANCE_RATIONALE: Platforms and policy intermediaries would say the world rearranges badly — years of litigation uncertainty would return and workers would lose the medical/injury coverage they currently have. Worker advocates would say the world barely changes for workers' substantive economic position, since the hybrid category was never delivering wage floors or retirement security in the first place, and its disappearance would simply reopen the door to the substantive-employment finding they wanted.
% FOUNDING_PROBLEM: Case-by-case litigation over whether platform workers were employees or contractors was producing inconsistent rulings, leaving many workers with zero protections and leaving platforms with unpredictable, jurisdiction-by-jurisdiction liability exposure.
% FOUNDING_PROBLEM_CORROBORATION: Policy intermediary agencies and platform operators attest the problem is solved: workers now have guaranteed baseline coverage and platforms have legal certainty. Independent labor economists and worker advocacy organizations outside the drafting process attest the founding problem — economic dependence without commensurate protection — remains live, and that the hybrid category primarily solved platforms' litigation-uncertainty problem rather than workers' income-security problem.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, contested).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__hybrid_security_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate 0.52 (rather than snare-level) because the hybrid category delivers real, verifiable transfers to workers — 91.5% medical and 86.2% injury coverage are not nominal. But it is well above rope-level because the category's design deliberately forecloses the larger transfer (wage floor, unemployment insurance, retirement, bargaining rights) that a substantive-employment finding would produce, and does so by statute rather than negotiation, which is why requires_active_enforcement is true and why the victim/beneficiary sets both have entries. Suppression (0.48) reflects that workers cannot easily litigate their way out of the category once it is codified — the very stability that makes it attractive to platforms and regulators forecloses the substantive_employment_reading as a live legal option for this population. Theater ratio rises over the interval (0.25 → 0.44) because compliance with the specific numeric guarantees has softened even as the political narrative of 'we solved platform labor' has hardened — a classic Goodhart pattern where the category's existence substitutes for its function.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (platform_operators, policy_intermediary_agencies), this reads as a rope or scaffold — genuine coordination that solved a real legal-uncertainty problem for everyone. From the payer seat (platform_workers), the same structure reads as extraction dressed as protection: real but partial benefits used to foreclose a larger, more substantive claim. The engine computing these as different seat-level types is the correct behavior, not an error — that divergence is exactly what identifies the hybrid category as tangled_rope rather than a clean rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform_workers are the structural payer: they lack the retirement, wage-floor, and bargaining protections a full-employment finding would deliver, and they cannot exit the category individually (constrained, not mobile — leaving platform work forfeits even the partial protections). Platform_operators are the structural beneficiary: the fixed-cost, statute-shielded bundle is materially cheaper than employer-of-record obligations, and they have arbitrage-grade exit (can restructure operations across jurisdictions with different hybrid rules). Policy_intermediary_agencies benefit institutionally — the category is their signature achievement and their continued relevance depends on defending it, giving them an incentive structure aligned with platforms' interest in permanence rather than with workers' interest in revisiting the boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — litigation uncertainty over classification — has been substantially resolved for platforms and regulators (founding_problem_status: contested, but corroborated as largely dead from the platform/agency side). For workers, the deeper founding problem — economic dependence without commensurate income security — remains live. This is the mandatrophy signature: an arrangement whose administering parties experience it as solved while the population it was nominally built to protect experiences the underlying problem as ongoing. The disappearance_verdict of 'contested' rather than a clean 'world_rearranges' captures that the two sides would not even agree on what disappearing would mean.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    third_category_genuine_vs_pretextual,
    'Is the hybrid category a genuinely novel, appropriately tailored legal form responding to real structural differences between platform work and traditional employment, or is it a pretextual compromise that primarily serves to foreclose the substantive-employment claim without addressing the underlying economic dependence?',
    'Longitudinal comparison of worker income security and retirement outcomes under the hybrid category versus outcomes in jurisdictions that adopted the substantive_employment_reading; convergence toward similar total compensation packages over time would support the genuine-novelty reading, persistent divergence would support the pretextual reading.',
    'If genuine, the tangled_rope classification may be too harsh and this reading should trend toward scaffold (a legitimate transitional form awaiting a sunset or maturation clause) or even rope. If pretextual, the classification should trend toward snare as theater_ratio continues rising and the statutory foreclosure hardens into permanent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_category_genuine_vs_pretextual, empirical, 'Whether the third category is structurally novel or a foreclosure device.').

omega_variable(
    kernel_reading_selection_pressure,
    'Which of the three employment_boundary kernel readings will become the dominant legal framework as more jurisdictions legislate, and does the hybrid_security_reading''s institutionalization make the substantive_employment_reading progressively harder to establish elsewhere (a foreclosure-by-precedent effect) even though it does not logically foreclose it within a single framework?',
    'Track citation patterns: do courts in substantive_employment_reading jurisdictions cite hybrid-category statutes as evidence that a middle path is ''the'' solution, narrowing the substantive reading''s persuasive force?',
    'If the hybrid reading is functioning as a de facto influence mechanism against the substantive reading across jurisdictions, that supports classifying the reading_relations edge to substantive_employment_reading as influences (structural downstream pressure) rather than mere coexistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether hybrid adoption exerts cross-jurisdictional pressure against the substantive reading.').

omega_variable(
    coverage_percentage_durability,
    'Are the 91.5% medical and 86.2% injury coverage figures durable statutory floors, or administrative snapshots likely to erode as platforms find compliance workarounds (as the rising theater_ratio measurements suggest)?',
    'Compare coverage percentages authored here against actual claims-paid data 5-10 years post-enactment across multiple adopting jurisdictions.',
    'Durable floors support the tangled_rope classification holding steady; eroding floors would push the classification toward snare as the coordination function (real coverage) degrades while the extraction function (foreclosure of full employment) remains fixed by statute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coverage_percentage_durability, empirical, 'Whether the headline coverage statistics will hold up under real-world enforcement over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__hybrid_security_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(empl_tr_t4, employment_boundary__hybrid_security_reading, theater_ratio, 4, 0.29).
narrative_ontology:measurement(empl_tr_t8, employment_boundary__hybrid_security_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__hybrid_security_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(empl_tr_t16, employment_boundary__hybrid_security_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__hybrid_security_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(empl_tr_t24, employment_boundary__hybrid_security_reading, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__hybrid_security_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(empl_be_t4, employment_boundary__hybrid_security_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(empl_be_t8, employment_boundary__hybrid_security_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(empl_be_t12, employment_boundary__hybrid_security_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(empl_be_t16, employment_boundary__hybrid_security_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(empl_be_t20, employment_boundary__hybrid_security_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(empl_be_t24, employment_boundary__hybrid_security_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__hybrid_security_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(empl_su_t4, employment_boundary__hybrid_security_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(empl_su_t8, employment_boundary__hybrid_security_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(empl_su_t12, employment_boundary__hybrid_security_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(empl_su_t16, employment_boundary__hybrid_security_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(empl_su_t20, employment_boundary__hybrid_security_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(empl_su_t24, employment_boundary__hybrid_security_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(employment_boundary__hybrid_security_reading, 0.15).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, substantive_employment_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'the platform worker employment classification debate' per the ε-invariance principle. formalist_employment_reading treats platform workers as outside any employment relationship (near-zero ε, low extraction, minimal victim set). substantive_employment_reading treats them as full employees under economic-dependence and algorithmic-control tests (higher ε, larger transfer, platforms as clearer beneficiary of misclassification). This hybrid_security_reading occupies the middle: moderate ε (0.52), a partial victim set, and a real but bounded coordination function. All three should be read as distinct constraints with distinct stakeholders and distinct classifications, not as one constraint viewed through different observational lenses — the underlying legal facts (algorithmic control, contract form, economic dependence) are fixed; what differs across the three stories is which normative framework is applied to those fixed facts, which is exactly the kernel/reading structure this family instantiates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
