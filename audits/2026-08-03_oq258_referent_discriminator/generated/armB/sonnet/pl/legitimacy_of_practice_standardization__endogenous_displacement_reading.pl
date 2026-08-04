% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Endogenous Displacement Reading of Practice Legitimacy (Voluntary Adoption / Cultural Evolution)
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This story instantiates the endogenous-displacement reading of a
 *   contested kernel about when practice change (e.g., calendar reform, dress
 *   convention shift) is legitimate. Under this reading, legitimacy derives
 *   from voluntary adoption driven by perceived utility and organic cultural
 *   evolution — the change diffuses from early-adopter elites through
 *   commercial intermediaries into urban populations, then unevenly into
 *   rural areas, without state compulsion. This is structurally distinct from
 *   a reading where a state decrees the change (exogenous_override_reading)
 *   and from a reading where public and private domains are legitimately
 *   partitioned between competing authorities
 *   (dual_practice_equilibrium_reading). Each reading is a separate
 *   constraint with its own ε; this file addresses only the
 *   voluntary-diffusion claim.
 *
 * KEY AGENTS:
 *   - early_adopter_elites: primary beneficiary and informal agenda-setter (powerful/arbitrage) — sets the model, moves freely between registers
 *   - commercial_intermediaries: beneficiary (organized/mobile) — profits from supplying the new practice's material apparatus
 *   - urban_populations_seeking_status_mobility: beneficiary/payer (moderate/mobile) — adopts for access, bears acquisition cost
 *   - rural_holdouts_facing_social_penalty: payer (powerless/constrained) — structurally lags rather than resists
 *   - traditional_practice_specialists: payer (moderate/constrained) — expertise depreciates gradually
 *   - regional_diffusion_researchers: analytical observer — documents the gradualism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.28).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.22).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Endogenous Displacement Reading of Practice Legitimacy (Voluntary Adoption / Cultural Evolution)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/modernization_studies/institutional_change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'c1e445b7-5026-455a-89b4-fde2cd607bca').
narrative_ontology:cs_kernel_codification('c1e445b7-5026-455a-89b4-fde2cd607bca', distributed).
narrative_ontology:cs_authority_grounding('c1e445b7-5026-455a-89b4-fde2cd607bca', practice).
narrative_ontology:cs_interpretation_layer_present('c1e445b7-5026-455a-89b4-fde2cd607bca').
narrative_ontology:cs_reading_relation('c1e445b7-5026-455a-89b4-fde2cd607bca', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1e445b7-5026-455a-89b4-fde2cd607bca', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('c1e445b7-5026-455a-89b4-fde2cd607bca', foundational, voluntary_uptake_confers_legitimacy).
narrative_ontology:cs_axiom_status(voluntary_uptake_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c1e445b7-5026-455a-89b4-fde2cd607bca', voluntary_uptake_confers_legitimacy, conventional).
narrative_ontology:cs_axiom('c1e445b7-5026-455a-89b4-fde2cd607bca', secondary, gradual_diffusion_indicates_genuine_utility).
narrative_ontology:cs_axiom_status(gradual_diffusion_indicates_genuine_utility, holdable).
narrative_ontology:cs_axiom_grounding('c1e445b7-5026-455a-89b4-fde2cd607bca', gradual_diffusion_indicates_genuine_utility, empirically_contingent).
narrative_ontology:cs_reference_frame('c1e445b7-5026-455a-89b4-fde2cd607bca', organic_cultural_evolution_baseline).
narrative_ontology:cs_drift_state('c1e445b7-5026-455a-89b4-fde2cd607bca', post_modernization_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c1e445b7-5026-455a-89b4-fde2cd607bca', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopter_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, commercial_intermediaries).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_populations_seeking_status_mobility).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rural_holdouts_facing_social_penalty).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_practice_specialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_populations_seeking_status_mobility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt the new calendar or dress convention first, gaining prestige association with the perceived-modern practice and using it to signal status and international legibility. They can move freely between old and new practice registers depending on audience, and their early adoption sets the model others measure themselves against.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopter_elites, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopter_elites, agenda_setter).

% Merchants, tailors, printers, and calendar-makers who profit from supplying the material apparatus of the new practice (new garments, printed calendars, adapted contracts). Their exit options are wide because demand for both old and new goods persists during the transition, letting them serve both markets.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, commercial_intermediaries, beneficiary,
    organized, biographical, mobile, national).

% City residents who adopt the practice to signal alignment with modernizing elites and access urban social and economic networks. They bear the cost of acquiring new goods and skills but gain access to opportunities gated by demonstrated adoption.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_populations_seeking_status_mobility, beneficiary,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_populations_seeking_status_mobility, payer).

% Populations in regions where old-practice infrastructure (local calendars, traditional dress economies) remains embedded who face increasing social and administrative friction as the new practice becomes normative elsewhere. They did not resist the change through organized opposition; rather, the diffusion simply outpaced their local practice ecology, leaving them structurally lagging rather than actively suppressed.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, rural_holdouts_facing_social_penalty, payer,
    powerless, biographical, constrained, regional).

% Ritual calendar-keepers, traditional tailors, and practice-transmission specialists whose expertise loses market value as voluntary adoption of the new practice spreads. Some retrain into the new practice's parallel roles; others see demand for their specialization shrink gradually rather than through prohibition.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_practice_specialists, payer,
    moderate, biographical, constrained, regional).

% Historians and sociologists tracking adoption curves, regional variation, and elite-to-mass diffusion patterns. They document the gradualism and voluntariness of the shift, distinguishing it from state-decreed changes elsewhere.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, regional_diffusion_researchers, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a population to converge on a single shared practice (calendar, dress, measurement convention) that reduces transaction friction with modernizing trade partners and signals cultural alignment with perceived utility gains, without requiring central compulsion.
% TRANSFER_FUNCTION: Moves status, market access, and administrative legibility toward early adopters and commercial intermediaries who supply the new practice's material apparatus, and away from holders of location-specific or specialist knowledge tied to the old practice, whose expertise depreciates as adoption spreads.
% ABSENT_VOICES: Traditional practice specialists and rural populations whose local economies are built around the old practice rarely appear in the diffusion narrative except as a lagging residual category; their perspective on what is lost (transmitted knowledge, local economic ecosystems) is largely absent from accounts that frame the change as pure cultural evolution.
% DISAPPEARANCE_RATIONALE: If voluntary-adoption dynamics vanished, elites and commercial intermediaries who benefited from first-mover status would need another sorting mechanism, and the diffusion pattern (elite-to-mass, regionally uneven) would not have occurred as it did; but proponents of the endogenous reading argue the underlying practices (calendars, dress) are inherently mutable and would have converged eventually through some mechanism regardless — hence the contest between 'this specific transition depended on organic diffusion' and 'the world would look similar under any transition mechanism.'
% FOUNDING_PROBLEM: Fragmented, locally-varying practices (calendars, dress conventions, measurement systems) created friction in trade, administration, and cross-regional communication as populations increasingly interacted with modernizing economic and diplomatic networks.
% FOUNDING_PROBLEM_CORROBORATION: Regional diffusion researchers (analytical observers outside the beneficiary set) attest that adoption curves and regional variation data support a genuine coordination problem being solved gradually; however, some economic historians studying rural regions argue the 'friction reduction' framing was largely retrospective elite narrative, and that much of the practical benefit accrued disproportionately to those already positioned to benefit from urban/international integration — making the founding-problem's universality contested rather than settled.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28 at interval end) because the mechanism genuinely is voluntary — no coercive apparatus compels rural holdouts or specialists to adopt; their disadvantage arises from being structurally outpaced by diffusion dynamics rather than targeted extraction. Suppression is correspondingly low (0.22): there is no active suppression of the old practice, only the passive erosion of its economic niche as demand shifts. Theater ratio stays low throughout (0.15 at end) because the coordination function (reduced friction with modernizing trade partners) is real and substantially unperformed — the practice change actually does what it claims to do for those who adopt it. Accessibility collapse is moderate (0.45): once adoption reaches critical mass, the old practice becomes progressively harder to operate within (calendars stop being printed, tailoring skills atrophy), but this is a gradual market effect, not an imposed collapse.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopter elites and commercial intermediaries sit near the full-beneficiary end: they gain status, market access, or profit and can move between practice registers at will (arbitrage/mobile exit). Rural holdouts and traditional specialists sit toward the target end: they bear the cost of depreciating local practice infrastructure, and their exit options are constrained by geography and specialized human capital that does not transfer easily. Urban populations are more symmetric — they invest to gain access but genuinely benefit from the access gained.
 *
 * MANDATROPHY ANALYSIS:
 *   The endogenous-displacement reading avoids the mandatrophy trap in the opposite direction from a coercive-mandate story: because adoption is voluntary and gradual, there is no expired mandate to unmask — the risk here is the inverse, over-crediting the coordination story and undercounting the diffuse cost to those whose local economies and expertise are stranded by a change they did not choose but also were not compelled into. The founding_problem_status is marked contested precisely because the corroboration question — whether the friction-reduction benefit was general or disproportionately elite-captured — remains open.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_adoption_vs_structural_pressure,
    'Is the diffusion pattern genuinely voluntary (driven by perceived utility), or does apparent voluntariness mask structural pressure — economic and status incentives so strong that ''choice'' is nominal for populations connected to modernizing trade networks?',
    'Compare adoption timing and rate against exposure to trade/administrative pressure: if adoption correlates tightly with proximity to state or commercial modernization infrastructure rather than with independent evaluation of utility, the voluntary framing is weaker than claimed.',
    'If adoption tracks structural pressure rather than independent utility assessment, this reading collapses toward the exogenous_override_reading or reveals a hybrid mechanism, which would raise the effective extractiveness and could shift classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_adoption_vs_structural_pressure, conceptual, 'Whether apparent voluntariness in diffusion is genuine or structurally induced.').

omega_variable(
    reading_boundary_calendar_dress_cases,
    'Across historical cases of calendar and dress reform, which specific instances best fit the endogenous_displacement_reading as opposed to the exogenous_override_reading or dual_practice_equilibrium_reading, and how clean is the boundary?',
    'Case-by-case historical coding: adoption curve shape (gradual vs. abrupt), presence/absence of state decree, and whether domains split between public/private authority. Cases with abrupt state-decreed timelines belong to the sibling reading, not this one.',
    'Misclassifying an exogenous case as endogenous would understate the coercive component and inappropriately extend this reading''s low-extraction profile to cases that structurally belong elsewhere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_calendar_dress_cases, empirical, 'Boundary precision between this reading and its siblings across historical cases.').

omega_variable(
    rural_lag_passive_or_penalized,
    'Is the disadvantage borne by rural holdouts and traditional specialists purely a passive byproduct of diffusion timing, or does it involve active social/administrative penalty (e.g., contracts, taxes, or legal instruments becoming unusable in the old practice) that would constitute suppression rather than mere lag?',
    'Examine whether administrative and legal instruments were redesigned to require the new practice during the transition period, which would indicate suppression layered onto ostensibly voluntary diffusion.',
    'If active penalty mechanisms are found, suppression and extractiveness values authored here understate the true structure, and the constraint would sit closer to tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rural_lag_passive_or_penalized, empirical, 'Whether rural/specialist disadvantage is passive lag or active penalty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 8, 0.16).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 16, 0.2).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 24, 0.24).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 32, 0.26).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 40, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(legitimacy_of_practice_standardization__endogenous_displacement_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.08).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legitimacy_of_practice_standardization kernel. The exogenous_override_reading locates legitimacy in state decree; the dual_practice_equilibrium_reading locates legitimacy in domain-partitioned authority (state governs public/administrative practice, tradition governs private/ritual practice). This reading locates legitimacy in voluntary, utility-driven diffusion. Each reading has a distinct ε, distinct beneficiary/victim structure, and distinct enforcement profile — they are linked here for contamination/coupling analysis, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
