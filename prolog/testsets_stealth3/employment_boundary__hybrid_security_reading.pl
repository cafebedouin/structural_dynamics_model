% ============================================================================
% CONSTRAINT STORY: employment_boundary__hybrid_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Third-Category Platform Work Settlement (Hybrid Security Reading)
 *   domain: labor economics/platform economy/social policy
 *
 * SUMMARY:
 *   Across jurisdictions that adopted a third category for platform work,
 *   workers receive mandated medical coverage (reaching 91.5% of platform
 *   workers in adopting jurisdictions) and occupational injury insurance
 *   (86.2%), while remaining outside the employment relationship — no career
 *   development pathway, no employment-track retirement accrual, no income
 *   floor between engagements. Platforms fund the partial schemes and escape
 *   full employer obligation; legislatures defend the boundary against
 *   reclassification pressure from both directions. The claim/metric split is
 *   deliberate: the constraint is CLAIMED as tangled_rope because it
 *   structurally possesses both a genuine coordination function and
 *   asymmetric extraction, while the metrics are authored as independent
 *   descriptive judgments — the engine computes per-seat classifications from
 *   the structural data, and divergence between claim and computation is the
 *   datum, not an error. This story is one reading of the employment_boundary
 *   kernel (see kernel_context); the sibling readings are separate files
 *   linked through network.affects_constraints, per the epsilon-invariance
 *   decomposition rule.
 *
 * KEY AGENTS:
 *   - platform_workers: Primary target (moderate/constrained) — bears the institutionalized gaps in career development, retirement security, and income stability despite receiving medical and injury coverage
 *   - platform_operators: Primary beneficiary and co-agenda-setter (institutional/arbitrage) — collects obligation relief and regulatory certainty; funded the category's creation and defends its boundary
 *   - legislatures_regulators: Agenda setter (institutional/constrained) — codifies eligibility and contribution rates, defends the category against court and directive-level challenge
 *   - consumers_of_platform_services: Incidental beneficiary and diffuse payer (moderate/mobile) — enjoys low prices and availability; carries backstop costs through public systems
 *   - future_retired_platform_workers: Absent voice (powerless/trapped) — the cohort bearing the deferred retirement deficit, present in no hearing
 *   - courts_classification_adjudicators: Analytical observer (institutional/analytical) — adjudicates which reading binds, case by case, reshaping the precedent landscape
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.58).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.62).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Third-Category Platform Work Settlement (Hybrid Security Reading)").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor economics/platform economy/social policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, '5b2387da-c118-4660-abc3-e3ba71979439').
narrative_ontology:cs_kernel_codification('5b2387da-c118-4660-abc3-e3ba71979439', formalized).
narrative_ontology:cs_authority_grounding('5b2387da-c118-4660-abc3-e3ba71979439', distributed).
narrative_ontology:cs_reading_relation('5b2387da-c118-4660-abc3-e3ba71979439', employment_boundary__formalist_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('5b2387da-c118-4660-abc3-e3ba71979439', employment_boundary__substantive_employment_reading, forecloses).
narrative_ontology:cs_axiom('5b2387da-c118-4660-abc3-e3ba71979439', foundational, protection_entitlements_attach_to_work_performed).
narrative_ontology:cs_axiom_status(protection_entitlements_attach_to_work_performed, holdable).
narrative_ontology:cs_axiom_grounding('5b2387da-c118-4660-abc3-e3ba71979439', protection_entitlements_attach_to_work_performed, deontological).
narrative_ontology:cs_axiom('5b2387da-c118-4660-abc3-e3ba71979439', secondary, per_engagement_obligation_design_preserves_flexibility).
narrative_ontology:cs_axiom_status(per_engagement_obligation_design_preserves_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('5b2387da-c118-4660-abc3-e3ba71979439', per_engagement_obligation_design_preserves_flexibility, instrumental).
narrative_ontology:cs_reference_frame('5b2387da-c118-4660-abc3-e3ba71979439', bespoke_protection_pluralism).
narrative_ontology:cs_drift_state('5b2387da-c118-4660-abc3-e3ba71979439', contemporary_directive_transposition_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5b2387da-c118-4660-abc3-e3ba71979439', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, consumers_of_platform_services).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, consumers_of_platform_services).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform on-demand tasks dispatched by apps — driving, delivery, micro-tasking — under algorithmic assignment and rating systems. Where the third category exists they receive mandated medical coverage and occupational injury insurance through the category's schemes. They do not accrue seniority, training pathways, paid leave, or contribution-based retirement credits comparable to employed peers, and their earnings swing with dispatch volumes they do not control. Moving to a rival platform keeps them inside the same category; leaving the sector means surrendering accumulated ratings and accepting lower expected earnings against local wage floors.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers, payer,
    moderate, biographical, constrained, global).

% Operate dispatch marketplaces and classify their workforces under the third category where it exists, funding the mandated injury-insurance and medical contributions while avoiding scheduling, minimum-hours, severance, and full payroll obligations that employee status would carry. They financed the campaigns that created the category in several jurisdictions, including a nine-figure ballot-initiative effort in California, and maintain standing litigation and legislative programs against reclassification. They can shift operations between markets, rebalance fleets, or accelerate automation investments.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, platform_operators, agenda_setter).

% Enact and administer the category: define eligibility, set contribution rates for injury and medical schemes, and defend the boundary against court challenges and supranational harmonization. They face simultaneous lobbying from platforms seeking narrower obligation and unions seeking full employment status, and in at least one jurisdiction the settlement was locked through a voter initiative later amended specifically to limit ordinary legislative repeal.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, legislatures_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Purchase rides, deliveries, and services at prices shaped by the category's lighter obligation load. They receive fast, inexpensive, always-available service and carry a diffuse indirect share of the arrangement's costs through taxes that fund the safety-net programs absorbing income volatility and thin retirement accrual among platform workers. Switching away from platform services is easy at the margin.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, consumers_of_platform_services, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, consumers_of_platform_services, payer).

% The cohort made up of today's platform workers at retirement age, twenty to forty years forward. Their pension adequacy depends on contribution histories the current category does not build. They attend no hearings, cast no votes, and enter the policy record only through actuarial projections prepared by others.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, future_retired_platform_workers, excluded,
    powerless, generational, trapped, global).

% Hear classification suits brought by workers, platforms, and public prosecutors. They apply multifactor tests — control, economic dependence, integration into the business — case by case, and their rulings pull particular jurisdictions toward one pole or the other, setting the precedent landscape the third category must survive in.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, courts_classification_adjudicators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__hybrid_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The third category solves a genuine matching problem: algorithmically dispatched work lacks the fixed schedules, workplaces, and bilateral supervision that anchor employment law, while lacking the autonomy and capital that justify independent contracting. A bespoke category lets legislators extend portable medical coverage and occupational injury insurance to workers whom neither existing category reaches, without imposing scheduling or minimum-hours obligations that conflict with on-demand flexibility.
% TRANSFER_FUNCTION: Moves mandated injury-insurance premiums and medical-scheme contributions from platforms toward platform workers, while moving the residual security burden — career progression, retirement accrual, income floors between engagements — from platforms onto workers themselves and, downstream, onto public safety nets. It also moves regulatory certainty to platforms: the category shields them from reclassification liability that employee status would trigger.
% ABSENT_VOICES: Workers who would claim full employee status, and the unions organizing them, testify in hearings but are structurally overridden by the category's design. The sharpest absent voice is the future-retiree cohort — today's platform workers at sixty-five, discovering that contribution-based pensions built on intermittent earnings fall short — who cannot be present in any proceeding because they do not yet exist as a recognized constituency.
% DISAPPEARANCE_RATIONALE: If the third category vanished overnight, every adopting jurisdiction would snap back toward the binary: formalist jurisdictions would strip the mandated medical and injury schemes, dropping platform-worker coverage toward contractor levels, while substantively-leaning jurisdictions would impose full employment packages, repricing platform services and contracting flexibility. Millions of work relationships, platform cost structures, and safety-net exposures would reorganize within years; the parties dispute only which direction the collapse runs.
% FOUNDING_PROBLEM: Gig work grew inside legal categories built for factories and consultancies. Workers fell through both openings: treated as independent contractors they carried injury and illness risk uninsured, yet the algorithmic control they worked under disqualified them from the entrepreneurial autonomy that justifies contractor treatment. The founding problem was the mismatch between algorithmically managed work and legacy employment categories.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: ILO platform-work reports documenting coverage gaps, occupational-injury epidemiology showing elevated incident rates among gig workers relative to employed comparators, and court findings of dense algorithmic control (the UK Supreme Court's Uber determination, the Spanish Supreme Court's Glovo rider ruling). Platform-operator attestation that a problem exists is discounted as self-interested; the external sources establish both the original mismatch and its persistence.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__hybrid_security_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction 0.58 is moderate by the reading's own construction: the category delivers real coverage — mandated medical schemes reach 91.5% of platform workers and injury insurance 86.2% where adopted — which pulls epsilon below what a formalist world would show for the same workers, but it simultaneously institutionalizes the career and retirement deficits that constitute the operators' cost advantage, pulling epsilon well above any pure-coordination baseline. Suppression 0.62 is authored as a raw structural property — the foreclosure of reclassification routes through preemption clauses, initiative locks, and standing litigation defense — and is left unscaled; only extractiveness is scaled by directionality and scope in the engine's computation. Theater 0.40: the schemes pay out, but a growing share of category activity is defending the protective label ('benefits without bureaucracy') rather than extending coverage, and that branding performs the legitimacy work that shields the lighter obligation load. Accessibility collapse 0.55: recognizing the hybrid as institutionalized precarity does not close alternatives — substantive reclassification stays live in courts and directive negotiations — but within a jurisdiction that has adopted and locked the category, worker-side exits narrow sharply. Resistance 0.62 reflects sustained strikes, classification suits, and union campaigns rather than token dissent. The three tracked series run on ONE shared time grid (t = 0,6,12,18,24,30) with every metric authored at every point, per the alignment rule; suppression_requirement is included deliberately because this story's narrative specifically traces enforcement-capacity change — preemption statutes, initiative amendments, escalating litigation defense — not merely shifting extraction. The interval maps to 1996-2026: T0 web-era labor intermediation, T12 smartphone dispatch platforms, T18 gig expansion and first classification suits, T24 initiative entrenchment and pandemic exposure of coverage gaps, T30 directive-transposition era; the t=30 points are tagged projected.
 *
 * PERSPECTIVAL GAP:
 *   From the operator seat the category is a modernization it paid to build: coverage where none previously existed, flexibility preserved, obligations calibrated to work actually performed. From the worker seat the same statute is half-security: injury covered today, old age uncovered, advancement structurally unavailable. From the legislature seat it is the only compromise that survived contact with both lobbies and the electorate. Seat divergence should compute sharply between the operator (beneficiary, arbitrage exit, global scope) and the worker (declared victim, constrained exit): identical statute, opposing classifications. Coalition dynamics matter for the resistance figure: the 2019-2024 strike wave demonstrated the worker class's latent collective leverage, and each coordinated action moved contribution rates and deactivation protections — evidence that resistance here is organized capability, not noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map directly onto the derivation: platform_workers appear in victims[] with constrained exit, placing them near the full-target end (high d, amplified by the category's identity-binding effect on career expectations); platform_operators appear in beneficiaries[] with arbitrage-grade exit across markets, placing them near the beneficiary end (low d); consumers carry a dual beneficiary/payer position the derivation should land near symmetric — genuine service benefit, diffuse tax-borne backstop cost; legislatures and courts carry no beneficiary or victim declaration and revert toward neutral mediation. One override is authored: the powerless atom is raised to d = 0.80 because the future-retiree cohort is a TEMPORAL target — its extraction is deferred beyond the current-period snapshot the beneficiary/victim declarations encode, and structural derivation from present-day data cannot see it. Without the override these agents inherit a neutral fallback despite sitting structurally near the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the category as pure rope would erase the asymmetry: the workers' retirement deficit is not coordination overhead but a transferred cost, priced into the operators' obligation relief. Reading it as pure snare would erase the delivered schemes, which no snare maintains — medical and injury coverage are real transfers with real payout records. Tangled_rope holds both truths: genuine coordination (bespoke protection reaching work forms the binary cannot classify) coexisting with asymmetric extraction (obligation relief for operators purchased with workers' deferred security). The founding problem is live and externally corroborated, so no mandatrophy is declared; and unlike a scaffold, the category is defended as a permanent settlement rather than a transition — no sunset clause is authored, which is precisely what converts its embedded deficits from transitional costs into structural ones.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_employment_boundary,
    'This story instantiates the hybrid_security_reading of the employment_boundary kernel; how would instantiating the formalist_employment_reading or substantive_employment_reading instead change the victim set, epsilon, and classification?',
    'Compile the two sibling stories against the same referent — the standing platform-work arrangement — and compare victim sets, epsilon, and computed types across the family; the kernel contest resolves only across files, never within one.',
    'Under the formalist sibling the victim set widens (even the medical and injury coverage vanishes) and epsilon rises toward the snare range; under the substantive sibling the victim set narrows to residual gaps and epsilon falls toward the rope range. This file''s moderate epsilon is meaningful only relative to its own reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_employment_boundary, conceptual, 'Committer-frame omega: one reading of a contested kernel; sibling readings instantiate different constraints with different epsilon over the same referent.').

omega_variable(
    coverage_delivery_vs_enrollment_theater,
    'Do the headline coverage rates (medical 91.5%, injury 86.2%) reflect effective protection — claims filed, paid, and care actually received — or nominal enrollment behind access friction?',
    'Claims-adjudication data from the mandated schemes: filing rates against injury incidence, payout ratios, denial and appeal outcomes, and independent worker-reported access audits.',
    'If delivery is largely theatrical, theater_ratio is understated and the classification drifts toward piton (protective performance over an atrophied function); if delivery is real, the authored moderate theater_ratio stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coverage_delivery_vs_enrollment_theater, empirical, 'Whether scheme enrollment converts to delivered protection or functions as coverage theater.').

omega_variable(
    retirement_deficit_magnitude,
    'How large is the retirement-security deficit the category institutionalizes, relative to an employment-track contribution history for the same work?',
    'Actuarial projection of pension accrual on representative intermittent platform earnings versus a matched employment trajectory, computed per adopting jurisdiction.',
    'A small deficit supports reading the arrangement closer to rope (authored epsilon overstated); a large deficit pushes epsilon upward and strengthens the asymmetric-extraction leg of the tangled_rope structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retirement_deficit_magnitude, empirical, 'Size of the deferred retirement gap embedded in the hybrid category.').

omega_variable(
    flexibility_preference_authenticity,
    'Is workers'' revealed preference for schedule flexibility — the category''s central legitimacy ground — an autonomous preference or an adaptation to constrained outside options?',
    'Longitudinal panel data tracking worker transitions when conventional employment at comparable pay becomes locally available; divergence between stated and revealed flexibility valuations under changed constraints.',
    'If flexibility valuation proves adaptive, the consent basis for excluding platform workers from employment protections weakens and the constraint''s effective suppression exceeds the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_preference_authenticity, preference, 'Whether the flexibility warrant reflects authentic preference or constraint-shaped adaptation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__hybrid_security_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(empl_tr_t6, employment_boundary__hybrid_security_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__hybrid_security_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(empl_tr_t18, employment_boundary__hybrid_security_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(empl_tr_t24, employment_boundary__hybrid_security_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(empl_tr_t30, employment_boundary__hybrid_security_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__hybrid_security_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(empl_be_t6, employment_boundary__hybrid_security_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(empl_be_t12, employment_boundary__hybrid_security_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(empl_be_t18, employment_boundary__hybrid_security_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(empl_be_t24, employment_boundary__hybrid_security_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(empl_be_t30, employment_boundary__hybrid_security_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__hybrid_security_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(empl_su_t6, employment_boundary__hybrid_security_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(empl_su_t12, employment_boundary__hybrid_security_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(empl_su_t18, employment_boundary__hybrid_security_reading, suppression_requirement, 18, 0.59).
narrative_ontology:measurement(empl_su_t24, employment_boundary__hybrid_security_reading, suppression_requirement, 24, 0.61).
narrative_ontology:measurement(empl_su_t30, employment_boundary__hybrid_security_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, substantive_employment_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'what do platform workers deserve?' decomposes into three epsilon-distinct constraint stories over the shared employment_boundary kernel: the formalist reading (contract form controls; no platform-specific obligations), this hybrid reading (bespoke category, partial obligations), and the substantive reading (employee status regardless of form). The hybrid is structurally downstream of the formalist incumbent — it exists as a patch on the binary that reading defends — and structurally pressured by the substantive reading, since every reclassification win shrinks the category's jurisdiction. Each family member links the other two; epsilon differs across the family because each reading assesses the same referent under different lights, and no single story hedges across readings (DP-001).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__hybrid_security_reading, powerless, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
