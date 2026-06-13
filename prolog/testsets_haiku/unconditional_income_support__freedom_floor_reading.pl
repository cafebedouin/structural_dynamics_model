% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Autonomy-Enabling Floor
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the 'freedom floor' reading of the
 *   unconditional income support kernel: UIS is a mechanism for removing
 *   coercion from labor markets by guaranteeing subsistence independent of
 *   employment. The reading presents UIS as enabling voluntary participation
 *   in labor and care work by decoupling survival from wage dependency. It
 *   contrasts structurally with the dependency-trap reading (UIS as
 *   incentive-distorting subsidy that rewards idleness) and the
 *   universality-paradox reading (UIS as politically ambiguous structure that
 *   masks incompatible implementation paths). This JSON models ONLY the
 *   freedom-floor reading: its beneficiaries are those constrained by labor
 *   market coercion; its claimed type is rope (coordination enabling
 *   voluntary participation); its epsilon is moderate because empirical data
 *   (Alaska, Kenya, Finland pilots) shows minimal labor supply substitution.
 *   The constraint is CLAIMED as rope by its advocates; measured metrics
 *   describe modest extractiveness and low suppression consistent with the
 *   coordination framing. The sibling readings are OTHER constraints with
 *   DIFFERENT epsilon values, beneficiary structures, and types; they are not
 *   alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - Precarious workers: labor-coerced population experiencing constant wage depression and exploitation. Exit from coercion is the direct benefit.
 *   - Caregivers (parents, elder-care providers, community organizers): identity-locked population performing essential unpaid work. Income floor formalizes care as legitimate economic contribution.
 *   - Artists: constrained-exit population producing cultural goods markets underprice. Income floor enables artistic labor without wage-labor cross-subsidy.
 *   - Abuse survivors: trapped population economically coerced into remaining in abusive relationships. Income floor enables exit and recovery.
 *   - Employers dependent on coercion: powerful population relying on labor market desperation to suppress wages. They pay through reduced control over labor supply.
 *   - Tax base (employed, asset-holding): organized population funding the floor through progressive taxation. Cost is distributed rather than concentrated.
 *   - Welfare bureaucracy: excluded from the freedom reading but structurally affected (administrative functions absorbed). Their exclusion is structural, not incidental.
 *   - Political theorists and empiricists: analytical observers testing whether labor supply elasticity supports or falsifies the autonomy mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.18).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.12).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support as Autonomy-Enabling Floor").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, '7ec8a2bd-87f2-457c-9c8d-db1c77c508d2').
narrative_ontology:cs_kernel_codification('7ec8a2bd-87f2-457c-9c8d-db1c77c508d2', distributed).
narrative_ontology:cs_authority_grounding('7ec8a2bd-87f2-457c-9c8d-db1c77c508d2', distributed).
narrative_ontology:cs_reading_relation('7ec8a2bd-87f2-457c-9c8d-db1c77c508d2', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ec8a2bd-87f2-457c-9c8d-db1c77c508d2', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('7ec8a2bd-87f2-457c-9c8d-db1c77c508d2', foundational, labor_market_coercion_is_real).
narrative_ontology:cs_axiom_status(labor_market_coercion_is_real, holdable).
narrative_ontology:cs_axiom_grounding('7ec8a2bd-87f2-457c-9c8d-db1c77c508d2', labor_market_coercion_is_real, empirically_contingent).
narrative_ontology:cs_axiom('7ec8a2bd-87f2-457c-9c8d-db1c77c508d2', foundational, unconditional_transfer_removes_coercion).
narrative_ontology:cs_axiom_status(unconditional_transfer_removes_coercion, holdable).
narrative_ontology:cs_axiom_grounding('7ec8a2bd-87f2-457c-9c8d-db1c77c508d2', unconditional_transfer_removes_coercion, empirically_contingent).
narrative_ontology:cs_axiom('7ec8a2bd-87f2-457c-9c8d-db1c77c508d2', secondary, voluntary_participation_improves_social_coordination).
narrative_ontology:cs_axiom_status(voluntary_participation_improves_social_coordination, holdable).
narrative_ontology:cs_axiom_grounding('7ec8a2bd-87f2-457c-9c8d-db1c77c508d2', voluntary_participation_improves_social_coordination, instrumental).
narrative_ontology:cs_created_at('7ec8a2bd-87f2-457c-9c8d-db1c77c508d2', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, artists).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, market_shock_vulnerable).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).
:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.18 at interval end) because the freedom reading frames UIS as removing coercion rather than imposing it. The beneficiaries list is large and specific (precarious workers, caregivers, abuse survivors, artists) — these are real populations whose constraints the measure directly addresses. The victims list is EMPTY because the reading explicitly claims a Pareto improvement: no one is made worse off; the cost is distributed taxation absorbed by those with sufficient income to bear it. Suppression is LOW (0.12) because enforcement is voluntary (participants opt in; no one is forced onto the income floor) and resistance is expected to come from those whose labor market power depends on coercion (employers in coerced sectors, those defending means-testing bureaucracy). Theater ratio is very low (0.08) because the mechanism is straightforward: unconditional transfer removes coercion, enabling voluntary participation. The slight rise in suppression and extractiveness over the interval reflects increasing political pressure from coercion-dependent employers and bureaucratic interests defending current targeting, not a change in the constraint's function. All measurements are authored on a shared time grid: every metric has a value at every time point (0, 5, 10, 20, 30, 40), satisfying the alignment rule.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting institutional seat (a UIS-implementing government) would experience this constraint as coordination: it solves a genuine labor market failure (coercion compressing wages) and enables voluntary participation, making the arrangement self-sustaining once established. The coercion-dependent employer seat experiences it as extraction: their labor cost structure rises, their workforce negotiating power falls, and the constraint actively prevents them from using starvation as leverage. The precarious worker seat experiences it as liberation: survival is decoupled from wage dependency, enabling genuine choice. The welfare bureaucracy seat (excluded from this reading but affected) would experience it as institutional displacement. These divergent experiences all follow from the same constraint because the directionality differs by seat — the freedom reading does not erase the distributional conflict; it names the beneficiaries and specifies the mechanism. The engine computes each seat's type from the directionality and structural data; the freedom reading's claim (rope) should compute as rope for beneficiary seats and as tangled rope or snare for coercion-dependent seats. That divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   The freedom-floor reading generates a clean directionality map: precarious workers, caregivers, artists, and abuse survivors are BENEFICIARIES (d near 0.0) — the constraint subsidizes their position by removing coercion. Employers dependent on labor market coercion and the tax base funding the transfer are PAYERS (d near 1.0 for coercion-dependent employers; d near 0.5 for distributed tax base). Labor market participants generally are near-symmetric: they fund through tax but benefit from improved wages and working conditions as coerced labor supply tightens. The welfare bureaucracy is structurally EXCLUDED — they have no authored stakeholder role because the freedom reading does not address the distributional question of whether they are harmed or redeployed. Each power atom operates at its home directionality: powerless precarious workers get maximum subsidy (d→0); powerful coercion-dependent employers get maximum extraction (d→1); organized employed population gets symmetric treatment (d≈0.5). No overrides are needed because the structural data (beneficiary/victim declarations + exit options + power atoms) derives the correct directionality through standard chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (labor market coercion preventing voluntary participation) maps cleanly to the mechanism (removing coercion by decoupling survival from wage dependency). The problem is LIVE in all labor markets where worker desperation exceeds alternative options. The constraint's function persists as long as coercion persists — there is no mandate-function decay unless labor market conditions fundamentally shift (universal voluntary employment, no scarcity). Mandatrophy does NOT apply to this reading because the founding problem has not been solved; the constraint remains functionally justified by the problem's persistence. The freedom reading explicitly avoids the mandatrophy trap by refusing to claim that UIS is a temporary measure or that the founding problem is nearly solved. Where mandatrophy arises is in the dependency-trap reading, which must claim that UIS solves the problem of idleness or undeserving poverty — those are philosophically prior claims that the evidence directly challenges, creating the mandatrophy dynamic. The freedom reading's empirical grounding (labor supply elasticity data from Alaska, Kenya, Finland) protects it from mandatrophy by tying the mechanism to measurable, observable outcomes rather than normative judgments about deservingness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_elasticity_counterfactual,
    'Does unconditional income support actually remove coercion from labor markets, or does it merely substitute government income for market coercion while preserving work-for-survival pressure?',
    'Comparative analysis of labor market participation, wage composition, and work-choice satisfaction across UIS and non-UIS populations with equivalent income levels. Alaska Permanent Fund, Kenya GiveDirectly trials, Finland 2017-2018 pilot provide preliminary evidence; a multi-cohort longitudinal study with matched controls would resolve.',
    'If labor supply is inelastic (workers maintain participation and wage composition despite UIS), the freedom reading holds: coercion is removed and voluntary participation is enabled. If labor supply exhibits substantial substitution (significant exit from degrading work, concentrated in specific sectors), the freedom reading is partially confirmed but must specify which sectors face coercion-driven participation. If labor supply collapse occurs in specific regions or demographic groups, the dependency-trap reading gains empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_elasticity_counterfactual, empirical, 'Whether UIS removes labor market coercion or merely shifts its form.').

omega_variable(
    coercion_dependent_employer_adaptation,
    'How do employers dependent on labor market desperation adapt when the income floor rises? Do they improve wages and conditions (supporting the autonomy reading), exit the market, automate, or capture policy implementation to undermine the floor?',
    'Historical analysis of wage and condition changes in previously coercion-dependent sectors (agriculture, domestic service, garment manufacturing) in jurisdictions implementing UIS. Employer lobbying intensity and policy capture attempts. Substitution patterns (automation, offshoring, service reduction).',
    'If adaptation is wage/condition improvement with sector persistence, the freedom reading is supported — markets adjust via price, not coercion. If adaptation is capture and floor-undermining, the snare reading''s warning about enforcement escalation is confirmed. If adaptation is exit/automation, the universality-paradox reading gains support: the floor is real but employment structure collapses, forcing different UIS levels and creating de facto targeting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_dependent_employer_adaptation, empirical, 'Whether coercion-dependent employers can adapt or whether they trigger political opposition that undermines the floor.').

omega_variable(
    care_work_formalization_reversibility,
    'Does declaring care work legitimate via income floor stabilize care participation, or does it create a separate (lower-status, lower-paid) tier of ''care work'' distinct from employment?',
    'Comparative analysis of care-work participation, wage premium/discount relative to market wages, and social status across jurisdictions with different UIS framing (care-as-legitimate vs. care-as-welfare). Caregiver self-identification and care-work sustainability over 10+ year intervals.',
    'If care work stabilizes at parity with market work in status and income, the freedom reading''s formalization claim holds. If care work becomes a separate, lower-status category despite UIS, the universality-paradox reading is partially confirmed: the floor is real but reproduces gendered hierarchies. If care participation declines despite UIS, the dependency-trap reading gains support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(care_work_formalization_reversibility, conceptual, 'Whether unconditional income support genuinely legitimizes care work or reproduces care-as-welfare stigma under different framing.').

omega_variable(
    coercion_versus_dependency_distinction,
    'Is the freedom reading''s distinction between removing coercion (autonomy-enabling) and creating dependency (welfare-expanding-dependency) a genuine empirical dividing line, or a philosophical assertion that collapses under implementation?',
    'Longitudinal measurement of: (1) subjective autonomy (workers'' self-reported ability to refuse degrading work); (2) objective exit (actual sector/employment change); (3) dependency indicators (reliance on transfer as primary income source, permanence of transfer receipt). Test whether autonomy increases precede or follow exit behavior; whether permanent transfer receipt indicates dependency or choice.',
    'If autonomy increases accompany exit behavior and transfers enable exit to preferred work (lower-wage but less degrading), the freedom reading holds. If transfers become permanent and serve as income floor rather than autonomy-enabling supplement, the dependency-trap reading''s concern about long-term effects is validated. If the distinction collapses (autonomy increases for some while dependency deepens for others, segmented by prior income), the universality-paradox reading''s warning about incompatible implementation paths is supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_versus_dependency_distinction, conceptual, 'Whether autonomy and dependency are meaningfully distinct empirical phenomena or philosophical reframings of the same material condition.').

omega_variable(
    reading_contention_axis,
    'Which empirical outcomes would falsify this reading (freedom-floor) and support one of its siblings?',
    'Pre-registered hypothesis testing against pilot outcomes and quasi-experimental data. Specify the effect sizes, sector participation patterns, wage trajectories, and subjective autonomy measures that would trigger reading revision.',
    'If labor supply substantially contracts in coercion-dependent sectors and dependence on UIS becomes permanent for large populations, the dependency-trap reading becomes more empirically grounded. If regional UIS implementation produces divergent outcomes (universality fails; different effective floors emerge by jurisdiction), the universality-paradox reading is confirmed. If coercion-dependent employers successfully lobby to lower the floor or implement means-testing, the snare reading becomes plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contention_axis, empirical, 'Specification of conditions under which this reading would be superseded by one of its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__freedom_floor_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__freedom_floor_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__freedom_floor_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(unco_tr_t30, unconditional_income_support__freedom_floor_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(unco_tr_t40, unconditional_income_support__freedom_floor_reading, theater_ratio, 40, 0.08).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__freedom_floor_reading, base_extractiveness, 5, 0.16).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__freedom_floor_reading, base_extractiveness, 10, 0.17).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__freedom_floor_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(unco_be_t30, unconditional_income_support__freedom_floor_reading, base_extractiveness, 30, 0.19).
narrative_ontology:measurement(unco_be_t40, unconditional_income_support__freedom_floor_reading, base_extractiveness, 40, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__freedom_floor_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__freedom_floor_reading, suppression_requirement, 5, 0.09).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__freedom_floor_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__freedom_floor_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement(unco_su_t30, unconditional_income_support__freedom_floor_reading, suppression_requirement, 30, 0.13).
narrative_ontology:measurement(unco_su_t40, unconditional_income_support__freedom_floor_reading, suppression_requirement, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__universality_paradox_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, labor_market_coercion_wage_suppression).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, care_work_undercompensation).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, welfare_bureaucracy_targeting_extraction).

% DUAL FORMULATION NOTE:
% The 'unconditional_income_support' kernel admits three structurally distinct readings: freedom_floor_reading (this constraint, Rope), dependency_trap_reading (Snare or Tangled Rope), and universality_paradox_reading (Tangled Rope). The freedom reading models UIS as enabling voluntary labor participation by removing coercion; epsilon is moderate (~0.18) because empirical data shows minimal labor-supply substitution and the mechanism directly addresses a genuine coordination problem (coercion-driven wage suppression). The dependency-trap reading models UIS as incentive-distorting subsidy rewarding idleness; epsilon would be higher (~0.55+) because it claims the mechanism extracts from active workers to subsidize non-participation. The universality-paradox reading models UIS as politically ambiguous commitment masking incompatible fiscal and distributive outcomes; epsilon would reflect the hidden extraction from workers whose jobs vanish as employers automate or exit coercion-dependent sectors. Each reading has different beneficiary/victim structures, directionality maps, and stakeholder configurations. They are NOT alternative measurements of the same constraint; they are three separate constraints instantiated from one contested kernel. The freedom reading influences both siblings: if empirical labor-supply data supports autonomy-enabling effects, it creates pressure to accept the freedom reading's framing and makes the dependency-trap reading's claims about idleness less empirically grounded. Conversely, if sector-specific employment collapse occurs (universality-paradox scenario), both the freedom and trap readings must account for uneven effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
