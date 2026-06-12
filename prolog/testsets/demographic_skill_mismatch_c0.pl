% ============================================================================
% CONSTRAINT STORY: demographic_skill_mismatch_c0
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_demographic_skill_mismatch_c0, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: demographic_skill_mismatch_c0
 *   human_readable: Demographic Skill Mismatch in Blue-Collar Labor Markets
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   The demographic skill mismatch in blue-collar labor markets describes a
 *   structural tension between an aging manual labor workforce (over 50% aged
 *   40+) and systematic avoidance of trades by workers born after 1990. This
 *   creates sustained upward wage pressure for blue-collar work. The
 *   constraint is CLAIMED as Mountain because the demographic transition
 *   appears to be an irreversible structural feature of economic development
 *   — declining birth rates, rising educational attainment, and
 *   intergenerational mobility away from manual work. The METRICS are
 *   authored to reflect low but rising extraction (0.18 at interval end) as
 *   intermediaries and institutions capture rents from the mismatch, and very
 *   low suppression (0.22) and resistance (0.08) because the constraint
 *   operates primarily through cultural preference rather than coercion. The
 *   claim/metric independence is deliberate: the engine will measure whether
 *   a claimed natural law with identifiable beneficiaries computes as
 *   Mountain or triggers false summit detection. KEY AGENTS (by structural
 *   relationship): - Incumbent blue-collar workers: Primary beneficiaries
 *   (moderate/constrained) — capture wage premium from scarcity - Labor
 *   intermediaries: Secondary beneficiaries (organized/mobile) — extract
 *   margin from controlled access - Vocational training institutions:
 *   Institutional beneficiaries (institutional/constrained) — receive funding
 *   to address framed skills gap - Employers requiring manual labor: Primary
 *   payers (powerful/constrained) — absorb rising labor costs - End
 *   consumers: Diffuse payers (powerless/trapped) — pay higher prices for
 *   manual services - Younger potential entrants: Culturally excluded
 *   (moderate/mobile) — avoid trades despite wage signals - Labor economists:
 *   Analytical observers (analytical/analytical) — investigate natural vs
 *   constructed dynamics
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(demographic_skill_mismatch_c0, 0.18).
domain_priors:suppression_score(demographic_skill_mismatch_c0, 0.22).
domain_priors:theater_ratio(demographic_skill_mismatch_c0, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(demographic_skill_mismatch_c0, extractiveness, 0.18).
narrative_ontology:constraint_metric(demographic_skill_mismatch_c0, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(demographic_skill_mismatch_c0, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(demographic_skill_mismatch_c0, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(demographic_skill_mismatch_c0, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(demographic_skill_mismatch_c0, mountain).
narrative_ontology:human_readable(demographic_skill_mismatch_c0, "Demographic Skill Mismatch in Blue-Collar Labor Markets").
narrative_ontology:topic_domain(demographic_skill_mismatch_c0, "labor_economics/platform_economy/social_policy").

domain_priors:emerges_naturally(demographic_skill_mismatch_c0).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(demographic_skill_mismatch_c0, incumbent_blue_collar_workers).
narrative_ontology:constraint_beneficiary(demographic_skill_mismatch_c0, labor_intermediaries).
narrative_ontology:constraint_beneficiary(demographic_skill_mismatch_c0, vocational_training_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(demographic_skill_mismatch_c0, employers_requiring_manual_labor).
narrative_ontology:constraint_victim(demographic_skill_mismatch_c0, end_consumers).
narrative_ontology:constraint_vindicates(demographic_skill_mismatch_c0, educational_attainment_mobility_doctrine).
narrative_ontology:constraint_vindicates(demographic_skill_mismatch_c0, demographic_transition_inevitability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers over 40 in manual trades experiencing unprecedented wage growth and bargaining power as their cohort shrinks and demand remains stable. They did not create the demographic transition but capture its wage premium. Their exit options are constrained by age and skill specificity, but within the trades they now command premium compensation.
narrative_ontology:constraint_stakeholder(demographic_skill_mismatch_c0, incumbent_blue_collar_workers, beneficiary,
    moderate, biographical, constrained, national).

% Workers born after 1990 who systematically avoid manual trades despite rising wages, pursuing college degrees and white-collar work instead. They are excluded not by formal barriers but by cultural narratives about status, educational achievement, and career trajectories that make blue-collar work psychologically inaccessible even when economically rational.
narrative_ontology:constraint_stakeholder(demographic_skill_mismatch_c0, younger_potential_entrants, excluded,
    moderate, biographical, mobile, national).

% Staffing agencies, trade unions, and contractor networks that capture margin from the wage premium by controlling access to scarce skilled labor. They benefit from the mismatch persisting because it sustains their intermediation rents, though they did not cause the underlying demographic shift.
narrative_ontology:constraint_stakeholder(demographic_skill_mismatch_c0, labor_intermediaries, beneficiary,
    organized, biographical, mobile, regional).

% Community colleges and trade schools that receive increased funding and enrollment as policymakers attempt to address the shortage. They benefit from the mismatch being framed as a skills gap requiring institutional intervention rather than a cultural preference requiring no solution.
narrative_ontology:constraint_stakeholder(demographic_skill_mismatch_c0, vocational_training_institutions, beneficiary,
    institutional, generational, constrained, national).

% Construction firms, manufacturers, logistics operators facing rising labor costs and project delays due to worker scarcity. They can automate some functions but many tasks remain stubbornly manual. They absorb the wage premium, delay projects, or pass costs to customers; they cannot conjure workers from the missing cohort.
narrative_ontology:constraint_stakeholder(demographic_skill_mismatch_c0, employers_requiring_manual_labor, payer,
    powerful, biographical, constrained, national).

% Households and businesses paying higher prices for construction, repair, and manual services as labor costs rise. They experience the constraint as unavoidable cost inflation in essential services with no alternative providers offering lower rates.
narrative_ontology:constraint_stakeholder(demographic_skill_mismatch_c0, end_consumers, payer,
    powerless, immediate, trapped, local).

% Researchers analyzing whether the mismatch is a natural demographic transition, a constructed outcome of educational policy and status signaling, or a hybrid where natural trends are amplified by institutional choices that benefit specific actors.
narrative_ontology:constraint_stakeholder(demographic_skill_mismatch_c0, labor_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None claimed — the constraint is presented as a demographic fact, not a coordinated arrangement. The labor market clears at higher wages; no coordination mechanism is required.
% TRANSFER_FUNCTION: Transfers purchasing power from employers and end consumers to incumbent blue-collar workers through wage premiums, and to labor intermediaries and training institutions through increased demand for their services.
% ABSENT_VOICES: Younger potential entrants are structurally present but culturally excluded — their voice in labor market discourse is drowned out by educational attainment narratives that treat college as the default path. Alternative framings that would normalize blue-collar work as a rational choice given current wage structures are systematically marginalized in guidance counseling, media representation, and family expectations.
% DISAPPEARANCE_RATIONALE: If the cultural stigma against manual work vanished overnight, younger workers would rationally enter trades given current wage premiums, the shortage would resolve within a training cycle, and wage premiums would compress — the labor market would rearrange. But if the demographic transition is truly structural (declining birth rates, rising educational attainment as irreversible development), the mismatch persists regardless of cultural framing — the world stays unchanged. The parties dispute which.
% FOUNDING_PROBLEM: Post-industrial economies experienced rising educational attainment and declining birth rates, creating a cohort gap where fewer young workers were available for manual trades while older workers aged toward retirement.
% FOUNDING_PROBLEM_CORROBORATION: Demographic data on birth rates and educational attainment is uncontested and comes from census bureaus and education ministries (outside any benefiting party). The contested element is whether the AVOIDANCE of manual work by educated cohorts is a natural preference or a constructed outcome of status signaling and institutional channeling. Sociologists and cultural historians outside the labor market attest that occupational prestige is socially constructed and historically variable, not a natural fact.
narrative_ontology:disappearance_verdict(demographic_skill_mismatch_c0, contested).
narrative_ontology:founding_problem_status(demographic_skill_mismatch_c0, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(demographic_skill_mismatch_c0, '046e0a40c34cddf4fff29b8c15f632dbdef31b7a',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-12',
    'cohort_zero_regen', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'temperature=0.2').
narrative_ontology:story_seed(demographic_skill_mismatch_c0, 'demographic_skill_mismatch', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(demographic_skill_mismatch_c0_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(demographic_skill_mismatch_c0, ExtMetricName, E),
    domain_priors:suppression_score(demographic_skill_mismatch_c0, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(demographic_skill_mismatch_c0),
    narrative_ontology:constraint_metric(demographic_skill_mismatch_c0, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(demographic_skill_mismatch_c0, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(demographic_skill_mismatch_c0_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low but rising (0.08 → 0.18) because the demographic transition itself is arguably natural, but identifiable actors increasingly capture rents from its persistence: labor intermediaries control access to scarce workers, training institutions receive funding to solve a problem that may be cultural rather than educational, and incumbent workers benefit from wage premiums that exceed productivity gains. Theater ratio is very low (0.12 at interval end) because most activity is functional — actual training, actual wage negotiation — but a small and growing share is performative policy responses that do not address the cultural stigma driving avoidance. Suppression is low (0.22) because younger workers are not coerced away from trades; they choose college paths in response to status signals and family expectations. Resistance is very low (0.08) because the constraint is widely accepted as demographic reality. Accessibility collapse is very high (0.88) because once the demographic facts are understood, alternative labor supply sources (immigration, automation, cultural shift) appear difficult and slow. The measurement series shows gradual extraction accumulation as institutional actors learn to monetize the mismatch.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent worker seat, the constraint is pure demographic luck — a natural scarcity they benefit from but did not engineer. From the employer seat, it is an unavoidable cost increase driven by forces beyond any actor's control. From the analytical seat, the constraint is a hybrid: the demographic transition is real, but the AVOIDANCE of trades by younger workers is amplified by constructed status hierarchies, educational tracking, and cultural narratives that benefit specific institutional actors. The engine computes this divergence from the structural data; the beneficiary declarations trigger false summit evaluation even though the claim is Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent blue-collar workers are beneficiaries (d near 0.2) — they did not create the shortage but capture its wage premium; their constrained exit options keep them in trades where they now have pricing power. Labor intermediaries and training institutions are also beneficiaries (d near 0.15-0.25) — they extract margin and funding from the mismatch without causing it. Employers and end consumers are payers (d near 0.7-0.85) — they bear rising costs with constrained or trapped exit options. Younger potential entrants are excluded rather than targeted — their exclusion is cultural, not extractive, so they do not map cleanly to the beneficiary/victim axis (d near 0.5, symmetric). The analytical seat sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a mandatrophy in the classical sense — it was not built to solve a problem that has since disappeared. Instead, it is a candidate false summit: a demographic reality that appears natural but has identifiable beneficiaries who gain from its persistence and from the framing that treats it as unsolvable. The omega variables document the irreducible uncertainty: is the avoidance of manual work by educated cohorts a natural preference (supporting Mountain classification) or a constructed outcome of status signaling and institutional channeling (supporting Tangled Rope or Snare classification where cultural gatekeepers extract by maintaining stigma)?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_avoidance,
    'Is the systematic avoidance of manual trades by younger workers a natural preference arising from educational attainment, or a constructed outcome of status signaling and institutional channeling that benefits specific actors?',
    'Cross-national comparison of countries with similar educational attainment but different cultural prestige for trades (e.g., Germany, Switzerland vs. US, UK). If avoidance varies independently of education levels, it is constructed. Longitudinal analysis of occupational prestige shifts in response to wage changes would also resolve: if younger workers enter trades when wages rise sufficiently, preference is economic; if they avoid trades despite wage premiums, preference is cultural/constructed.',
    'If avoidance is natural, the constraint is a genuine Mountain — a demographic fact no actor can reverse. If avoidance is constructed and maintained by actors who benefit from the mismatch (training institutions framing it as a skills gap, intermediaries controlling access, cultural gatekeepers maintaining stigma), the constraint is a false summit — a Tangled Rope or Snare where extraction rides on a real demographic trend but is amplified by institutional choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_avoidance, empirical, 'Whether occupational avoidance is natural preference or constructed stigma').

omega_variable(
    beneficiary_amplification,
    'Do the identifiable beneficiaries (intermediaries, training institutions) actively maintain the cultural stigma against manual work, or do they passively benefit from an independent demographic trend?',
    'Analysis of messaging and policy advocacy by training institutions and labor intermediaries: do they promote cultural normalization of trades, or do they frame the problem as a skills gap requiring institutional intervention (which sustains their funding and intermediation rents)? Examination of guidance counseling practices and media representation of trades would reveal whether gatekeepers actively steer younger workers away from manual work.',
    'If beneficiaries actively maintain stigma or frame the problem in ways that sustain their rents, the constraint is extractive and constructed. If they passively benefit from an independent cultural shift, the constraint is closer to a natural demographic reality with incidental beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_amplification, empirical, 'Whether beneficiaries amplify the constraint or merely benefit from it').

omega_variable(
    immigration_suppression,
    'Is the labor shortage sustained in part by immigration restrictions that prevent foreign workers from filling blue-collar roles, and do domestic beneficiaries lobby for those restrictions?',
    'Analysis of immigration policy advocacy by trade unions, labor intermediaries, and incumbent workers. Comparison of labor shortages in jurisdictions with open vs. restricted immigration for manual trades. If beneficiaries actively lobby against immigration pathways that would resolve the shortage, the constraint is partly constructed.',
    'If immigration restrictions are actively maintained by beneficiaries to preserve wage premiums, the constraint is a Snare or Tangled Rope — a natural demographic trend weaponized through policy to extract rents. If immigration policy is independent of beneficiary advocacy, the constraint is closer to a genuine Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immigration_suppression, empirical, 'Whether immigration restrictions are maintained to preserve beneficiary rents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(demographic_skill_mismatch_c0, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(demo_tr_t0, demographic_skill_mismatch_c0, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(demo_tr_t0, observed).
narrative_ontology:measurement(demo_tr_t5, demographic_skill_mismatch_c0, theater_ratio, 5, 0.06).
narrative_ontology:measurement_basis(demo_tr_t5, observed).
narrative_ontology:measurement(demo_tr_t10, demographic_skill_mismatch_c0, theater_ratio, 10, 0.08).
narrative_ontology:measurement_basis(demo_tr_t10, observed).
narrative_ontology:measurement(demo_tr_t15, demographic_skill_mismatch_c0, theater_ratio, 15, 0.09).
narrative_ontology:measurement_basis(demo_tr_t15, observed).
narrative_ontology:measurement(demo_tr_t20, demographic_skill_mismatch_c0, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(demo_tr_t20, observed).
narrative_ontology:measurement(demo_tr_t25, demographic_skill_mismatch_c0, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(demo_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(demo_be_t0, demographic_skill_mismatch_c0, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(demo_be_t0, observed).
narrative_ontology:measurement(demo_be_t5, demographic_skill_mismatch_c0, base_extractiveness, 5, 0.11).
narrative_ontology:measurement_basis(demo_be_t5, observed).
narrative_ontology:measurement(demo_be_t10, demographic_skill_mismatch_c0, base_extractiveness, 10, 0.14).
narrative_ontology:measurement_basis(demo_be_t10, observed).
narrative_ontology:measurement(demo_be_t15, demographic_skill_mismatch_c0, base_extractiveness, 15, 0.16).
narrative_ontology:measurement_basis(demo_be_t15, observed).
narrative_ontology:measurement(demo_be_t20, demographic_skill_mismatch_c0, base_extractiveness, 20, 0.17).
narrative_ontology:measurement_basis(demo_be_t20, observed).
narrative_ontology:measurement(demo_be_t25, demographic_skill_mismatch_c0, base_extractiveness, 25, 0.18).
narrative_ontology:measurement_basis(demo_be_t25, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(demographic_skill_mismatch_c0, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(demographic_skill_mismatch_c0, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is a candidate for decomposition if analysis reveals distinct mechanisms: (1) the demographic transition itself (declining birth rates, aging workforce) as a genuine Mountain, and (2) the cultural stigma against manual work as a constructed Snare or Tangled Rope maintained by institutional actors. Current formulation treats them as a single hybrid constraint pending empirical resolution of the omega variables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
