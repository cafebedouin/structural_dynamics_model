% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__integration_primary, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Free Movement as Constitutive of Single Market (Integration-Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint instantiates the integration-primary reading of the
 *   contested kernel federation_membership_treaty. It treats free movement of
 *   workers as constitutive of the single market, rendering national
 *   restrictions presumptively illegitimate unless narrowly justified. The
 *   arrangement coordinates a continental labor market but asymmetrically
 *   extracts from static workforces and local welfare systems while
 *   suppressing national regulatory autonomy. The claim is tangled_rope
 *   because the coordination function (single-market integration) is genuine,
 *   yet the same structure enforces asymmetric costs on immobile labor and
 *   national administrations.
 *
 * KEY AGENTS:
 *   - eu_institutions (agenda_setter / institutional / arbitrage) â enforce free movement and adjudicate national restrictions
 *   - mobile_workers (beneficiary / moderate / mobile) â gain cross-border rights and income opportunities
 *   - cross_border_employers (beneficiary / organized / arbitrage) â capture labor-supply expansion and wage differentials
 *   - static_workforce (payer / powerless / trapped) â bear wage and competition pressure without mobility offset
 *   - local_welfare_administrations (payer / moderate / constrained) â administer equal benefits under fiscal strain
 *   - national_governments (payer / institutional / constrained) â lose regulatory autonomy over labor and welfare
 *   - sovereignty_advocates (excluded / organized / constrained) â excluded from treaty-level framing
 *   - federal_economists (observer / analytical / analytical) â measure aggregate efficiency without bearing costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.74).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.8).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.74).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Free Movement as Constitutive of Single Market (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, 'c0d85ff2-3342-440c-a887-564442125bba').
narrative_ontology:cs_kernel_codification('c0d85ff2-3342-440c-a887-564442125bba', formalized).
narrative_ontology:cs_authority_grounding('c0d85ff2-3342-440c-a887-564442125bba', lineage).
narrative_ontology:cs_interpretation_layer_present('c0d85ff2-3342-440c-a887-564442125bba').
narrative_ontology:cs_reading_relation('c0d85ff2-3342-440c-a887-564442125bba', federation_membership_treaty__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('c0d85ff2-3342-440c-a887-564442125bba', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('c0d85ff2-3342-440c-a887-564442125bba', foundational, free_movement_constitutive_of_union).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_union, holdable).
narrative_ontology:cs_axiom_grounding('c0d85ff2-3342-440c-a887-564442125bba', free_movement_constitutive_of_union, conventional).
narrative_ontology:cs_axiom('c0d85ff2-3342-440c-a887-564442125bba', foundational, supremacy_of_mobility_rights).
narrative_ontology:cs_axiom_status(supremacy_of_mobility_rights, holdable).
narrative_ontology:cs_axiom_grounding('c0d85ff2-3342-440c-a887-564442125bba', supremacy_of_mobility_rights, conventional).
narrative_ontology:cs_reference_frame('c0d85ff2-3342-440c-a887-564442125bba', supranational_market_integration).
narrative_ontology:cs_drift_state('c0d85ff2-3342-440c-a887-564442125bba', post_enlargement_austerity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c0d85ff2-3342-440c-a887-564442125bba', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, cross_border_employers).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, static_workforce).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, local_welfare_administrations).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, national_governments).
narrative_ontology:constraint_vindicates(federation_membership_treaty__integration_primary, supremacy_of_union_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiate infringement proceedings against member states that restrict free movement, propose legislation to remove barriers, and set the enforcement agenda for market integration across the union.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, eu_institutions, agenda_setter,
    institutional, generational, arbitrage, continental).

% Exercise treaty rights to seek employment and reside across member states, gaining income arbitrage and expanded opportunity; their mobility is protected by supranational law against national restriction.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Recruit labor from across the union, benefiting from wage differentials and enlarged applicant pools; their competitive position improves relative to firms restricted to local labor markets.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, cross_border_employers, beneficiary,
    organized, biographical, arbitrage, continental).

% Compete for jobs and wages in local labor markets that are opened to mobile workers without equivalent exit options; face downward wage pressure and reduced bargaining power without receiving mobility offset.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, static_workforce, payer,
    powerless, biographical, trapped, national).

% Administer social assistance and housing programs that must be provided to mobile EU citizens on equal terms; face fiscal strain and political pressure without authority to restrict eligibility to nationals.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, local_welfare_administrations, payer,
    moderate, biographical, constrained, national).

% Retain formal responsibility for labor market and welfare policy but must justify any restriction on free movement as narrowly tailored; face infringement proceedings and political costs when attempting to protect domestic constituencies.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_governments, payer,
    institutional, generational, constrained, national).

% Argue for ultimate national authority over labor market entry and welfare eligibility; structurally excluded from EU treaty interpretation and Commission agenda-setting, their policy preferences are treated as presumptively illegitimate.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, sovereignty_advocates, excluded,
    organized, generational, constrained, national).

% Analyze aggregate efficiency gains from labor-market integration, productivity effects, and fiscal impacts; do not bear costs or collect benefits directly from the constraint.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, federal_economists, observer,
    analytical, civilizational, analytical, continental).

narrative_ontology:fixing_cost_class(federation_membership_treaty__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a unified labor market across member states, eliminating barriers to cross-border mobility and enabling firms to recruit across the union under a single legal framework.
% TRANSFER_FUNCTION: Transfers regulatory authority over labor market access and welfare eligibility from national governments to supranational institutions; transfers competitive pressure from mobile workers to static local workforces and welfare administrations.
% ABSENT_VOICES: Sovereignty advocates and subnational communities experiencing rapid demographic change are largely excluded from treaty-level framing; national parliaments that would restrict entry are constrained by supremacy doctrine.
% DISAPPEARANCE_RATIONALE: If free movement of workers and the presumption against national restrictions vanished overnight, member states would re-erect border controls, labor markets would segment along national lines, wage arbitrage would collapse, and the single market would fragment into discrete national economies.
% FOUNDING_PROBLEM: Post-war economic fragmentation and protectionist labor markets in Europe prevented efficient allocation of labor, hindered competitiveness, and were associated with national rivalries.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and federalist historians attest the problem is live; nationalist parties and some labor economists argue the problem has mutated and the arrangement now generates costs (wage compression, welfare strain) that exceed the original ailment; independent economic historians note the efficiency gains but corroborate the distributional asymmetry from outside the beneficiary set.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__integration_primary, 0.74, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.25 to 0.74 over the interval as jurisprudence hardens, mobility volumes increase, and the scope of protected cross-border activity expands. Suppression is high (0.80â0.82 at end) because national restrictions are systematically challenged through infringement procedures and ECJ rulings. Theater ratio is moderate (0.36): much enforcement is genuine market-building, but a growing share performs federal integration for its own sake, especially as resistance rises and justifications become more ritualized. Accessibility collapse is high (0.75) because, once the single market is institutionalized, national protectionism ceases to be a live alternative within the legal framework. Resistance is moderate (0.55) because member states and affected populations push back politically, even as they are legally constrained.
 *
 * PERSPECTIVAL GAP:
 *   From the mobile worker or cross-border employer seat, the constraint computes as coordination (rope or mild tangled_rope). From the static workforce or local welfare administration seat, the same structure computes as extraction (snare-leaning tangled_rope). The engine produces this divergence automatically from identical structural data because exit options and power differ: mobile workers have continental arbitrage, while static workforces are trapped in local labor markets. National governments experience yet another patternâinstitutional power but constrained exitâproducing a distinct per-seat classification.
 *
 * DIRECTIONALITY LOGIC:
 *   EU institutions and mobile workers sit near the beneficiary end of directionality: the constraint subsidizes their mobility and expands their authority. Cross-border employers also benefit from enlarged labor supply. Static workforces and local welfare administrations sit near the full-target end: they bear the competitive and fiscal costs without offsetting mobility rights. National governments occupy a middle-high d because they lose sovereignty regardless of any aggregate efficiency gains. The derivation chain maps beneficiary/victim declarations directly to these positions; no overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (it solves a real coordination problem: post-war fragmentation and inefficient labor allocation) and prevents mislabeling it as pure coordination (it actively suppresses national alternatives and asymmetrically burdens static populations). The rising theater_ratio over the measurement interval signals that the coordination story is partially atrophying into performance, but the core extractive asymmetry remains structurally tied to the genuine coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_reading_stability,
    'Does the integration-primary reading structurally foreclose the sovereignty-primary reading, or do they merely coexist across different political coalitions?',
    'Analysis of whether any single legal framework can simultaneously hold free movement as constitutive and member-state consent as the ultimate authority; treaty exit clauses and constitutional crises provide natural experiments.',
    'If foreclosed, the integration reading is a commitment system with logical dominance; if coexisting, the constraint remains politically contested and may drift with electoral cycles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_reading_stability, conceptual, 'Committer stability of integration-primary reading against sovereignty sibling').

omega_variable(
    welfare_strain_vs_labor_supply,
    'To what extent does free movement extract from welfare systems through benefit tourism versus genuinely supplying labor to shortage sectors?',
    'Cross-national administrative data on in-work benefits claims by EU migrants versus their tax contributions and sectoral employment patterns.',
    'If extraction from welfare is high relative to labor contribution, the victim set expands and the coordination story weakens; if low, the extraction is primarily borne by static wages rather than fiscal systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_strain_vs_labor_supply, empirical, 'Empirical ambiguity over whether welfare systems or labor markets are the primary victim').

omega_variable(
    enforcement_sustainability,
    'Can the high suppression of national restrictions be sustained indefinitely without treaty revision or member-state exit?',
    'Track infringement case volumes, ECJ ruling compliance rates, and member-state political realignments over time.',
    'If suppression requires escalating enforcement despite rising resistance, the constraint may drift toward snare or piton; if enforcement stabilizes with compliance, it remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_sustainability, empirical, 'Sustainability of enforcement against rising national resistance').

omega_variable(
    cs_framing_under_determination,
    'Does the commitment system frame the kernel as a formal treaty text interpreted by courts, or as an evolving practice of market integration that has outgrown its textual origins?',
    'Compare ECJ rulings that cite treaty text versus those that infer principles from integration practice; examine whether the text or the practice is doing the legitimating work.',
    'If practice rather than text is the true kernel, the authority_grounding shifts from lineage to practice, potentially changing the drift_state direction and the classification of subsequent jurisprudence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Alternative framing of the kernel as text versus practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__integration_primary, theater_ratio, 10, 0.22).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__integration_primary, theater_ratio, 20, 0.24).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__integration_primary, theater_ratio, 30, 0.28).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__integration_primary, theater_ratio, 40, 0.31).
narrative_ontology:measurement(fede_tr_t50, federation_membership_treaty__integration_primary, theater_ratio, 50, 0.34).
narrative_ontology:measurement(fede_tr_t60, federation_membership_treaty__integration_primary, theater_ratio, 60, 0.36).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__integration_primary, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__integration_primary, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__integration_primary, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__integration_primary, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(fede_be_t50, federation_membership_treaty__integration_primary, base_extractiveness, 50, 0.69).
narrative_ontology:measurement(fede_be_t60, federation_membership_treaty__integration_primary, base_extractiveness, 60, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__integration_primary, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__integration_primary, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__integration_primary, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__integration_primary, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(fede_su_t50, federation_membership_treaty__integration_primary, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(fede_su_t60, federation_membership_treaty__integration_primary, suppression_requirement, 60, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
