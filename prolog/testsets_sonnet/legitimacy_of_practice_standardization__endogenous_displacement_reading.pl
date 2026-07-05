% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   This story instantiates the endogenous_displacement reading of the
 *   practice-legitimacy kernel: a calendar or dress convention changes
 *   because it is voluntarily taken up by people who find it useful, starting
 *   among commercially exposed elites and diffusing outward through
 *   demonstration, imitation, and gradually shifting perceived utility,
 *   without a central decree compelling the switch. Under this reading, the
 *   process looks like a rope — a coordination convention that participants
 *   opt into because it lowers transaction costs with people they already
 *   deal with. The metrics are kept low-extraction and low-suppression
 *   because, on THIS reading, no actor is coerced and no formal sanction
 *   attaches to holding out; the friction holdouts experience is coordination
 *   cost, not punishment. This is one of three readings of the same kernel:
 *   the exogenous_override_reading treats state decree as the legitimating
 *   mechanism instead of voluntary uptake, and the
 *   dual_practice_equilibrium_reading treats legitimacy as domain-partitioned
 *   between state and traditional authority rather than settled by either
 *   mechanism alone. Each reading is authored as its own constraint with its
 *   own epsilon; this file does not average across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.22).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Endogenous Displacement Reading of Practice Legitimacy (Voluntary Adoption / Cultural Evolution)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/modernization_studies/institutional_change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '359a4986-e1af-4385-b5f6-f9e3d8189b11').
narrative_ontology:cs_kernel_codification('359a4986-e1af-4385-b5f6-f9e3d8189b11', distributed).
narrative_ontology:cs_authority_grounding('359a4986-e1af-4385-b5f6-f9e3d8189b11', practice).
narrative_ontology:cs_interpretation_layer_present('359a4986-e1af-4385-b5f6-f9e3d8189b11').
narrative_ontology:cs_reading_relation('359a4986-e1af-4385-b5f6-f9e3d8189b11', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('359a4986-e1af-4385-b5f6-f9e3d8189b11', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('359a4986-e1af-4385-b5f6-f9e3d8189b11', foundational, utility_perception_alone_legitimates_change).
narrative_ontology:cs_axiom_status(utility_perception_alone_legitimates_change, holdable).
narrative_ontology:cs_axiom_grounding('359a4986-e1af-4385-b5f6-f9e3d8189b11', utility_perception_alone_legitimates_change, conventional).
narrative_ontology:cs_axiom('359a4986-e1af-4385-b5f6-f9e3d8189b11', secondary, gradual_diffusion_curve_is_evidence_of_voluntariness).
narrative_ontology:cs_axiom_status(gradual_diffusion_curve_is_evidence_of_voluntariness, holdable).
narrative_ontology:cs_axiom_grounding('359a4986-e1af-4385-b5f6-f9e3d8189b11', gradual_diffusion_curve_is_evidence_of_voluntariness, empirically_contingent).
narrative_ontology:cs_reference_frame('359a4986-e1af-4385-b5f6-f9e3d8189b11', customary_practice_as_self_legitimating).
narrative_ontology:cs_drift_state('359a4986-e1af-4385-b5f6-f9e3d8189b11', post_state_modernization_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('359a4986-e1af-4385-b5f6-f9e3d8189b11', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopter_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, commercially_integrated_urban_populations).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, practice_innovators).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, voluntary_diffusion_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_evolution_as_source_of_norms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Merchants, professionals, and urban notables who adopt new calendar reckoning or dress conventions first because doing so signals modernity, eases dealings with foreign trading partners, or confers status inside emerging commercial networks. They set the diffusion in motion by example rather than decree, and profit from being seen as ahead of the curve.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopter_elites, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopter_elites, agenda_setter).

% Urban residents whose daily transactions increasingly run on the new practice because trading partners, calendars of commerce, and social circles have already shifted. Adoption reduces friction in their existing dealings; abstaining costs them coordination with people they already deal with, but nothing outside compels them.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, commercially_integrated_urban_populations, beneficiary,
    moderate, biographical, mobile, regional).

% Tailors, printers, calendar-makers, and cultural intermediaries who develop and market the new practice's material infrastructure (new garment styles, new almanacs). They gain from expanding demand as adoption spreads, but their livelihood does not depend on anyone being compelled to switch.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, practice_innovators, beneficiary,
    moderate, generational, mobile, regional).

% Populations distant from commercial and elite circuits who adopt the new practice slowly if at all, continuing older reckoning and dress for decades. They are not targeted or extracted from; the old practice simply persists locally because the incentive gradient that drove urban adoption is weaker where it does not touch their daily transactions.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, rural_and_peripheral_communities, observer,
    powerless, generational, constrained, regional).

% Individuals and communities who continue the older practice by preference, viewing the new one as foreign or status-driven rather than genuinely superior. Under this reading their continued adherence is treated as a legitimate coexisting choice rather than resistance requiring correction; they are not consulted by the diffusion process but are not coerced by it either.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditionalist_holdouts, excluded,
    moderate, biographical, mobile, regional).

% Scholars who trace adoption curves, regional variation, and elite-to-mass diffusion patterns to assess whether a given practice change was genuinely voluntary or masked by informal social pressure. They compile the comparative record this reading depends on.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, historians_of_diffusion, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces transaction friction among people who already interact commercially or socially by converging on a shared calendar, dress code, or convention, allowing the convention to spread through demonstrated utility rather than command.
% TRANSFER_FUNCTION: Moves prestige and coordination advantage toward early adopters and toward those already embedded in the networks where the new practice is useful; imposes mild coordination cost on holdouts who wish to continue transacting with adopters, but transfers no formal sanction or resource extraction.
% ABSENT_VOICES: Rural and peripheral communities are rarely surveyed on whether the new practice serves them; their slower adoption is read by this account as low incentive rather than exclusion, though an exogenous-override reading would characterize the same gap differently.
% DISAPPEARANCE_RATIONALE: If voluntary-adoption legitimacy were withdrawn as a frame, most instances of gradual practice change (dress reform, calendar shift, currency convention) would continue to exist as a historical fact, but their legitimacy would be reinterpreted — likely folded into the exogenous_override or dual_practice_equilibrium readings of the same events, so the world of practice does not rearrange but the classification of its legitimacy does.
% FOUNDING_PROBLEM: Communities needed a way to explain and legitimate practice changes that were observably not commanded by a central authority but nonetheless swept through a population — voluntary status competition, commercial convenience, and cultural prestige needed a legitimating account distinct from decree.
% FOUNDING_PROBLEM_CORROBORATION: Comparative historians of diffusion (e.g., studies of 19th–20th century dress and calendar reform) attest that adoption curves and regional variation patterns are genuinely observable and predate any centralized enforcement in many documented cases. State-modernization historians dispute the framing, arguing that apparently voluntary diffusion is frequently downstream of prior state signaling, prestige economies engineered by elites close to state power, or anticipatory compliance — corroboration for the fully voluntary account is contested even among scholars outside the adopting elites themselves.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.22, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is low and rises only mildly over the interval (0.10 to 0.22) because voluntary diffusion by construction has no rent extraction mechanism — the closest thing to a cost is the coordination penalty borne by holdouts who wish to keep transacting with adopters, and that penalty grows slowly as adoption spreads and the old practice becomes rarer. Suppression is low and flat because nothing in this reading's structural account authorizes coercion; resistance is present but moderate (0.25) because some traditionalist friction persists as a genuine minority preference rather than active suppression requiring machinery to overcome. Theater ratio stays low because there is little performative apparatus in a voluntary-adoption account — no enforcement theater to perform.
 *
 * PERSPECTIVAL GAP:
 *   From the early-adopter seat, the practice change looks like organic coordination they helped originate; from the traditionalist-holdout seat, it can look like slow social pressure dressed as inevitability — but under THIS reading that pressure is modeled as ordinary coordination cost, not extraction, which is exactly the structural commitment an exogenous_override or dual_practice_equilibrium reading of the same historical episode would contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopter elites and commercially integrated populations sit near the beneficiary end: they gain coordination advantage and status from adoption, and their exit options remain mobile throughout. Practice innovators benefit as demand for the new practice's material infrastructure grows. Rural and peripheral communities and traditionalist holdouts are not targets in this reading's structure — they simply experience a weaker incentive gradient or a genuine preference for the older practice, and nothing in the endogenous account transfers cost onto them by design. This is the central structural claim that distinguishes this reading from its siblings: the absence of a victim class is not an oversight but the reading's defining premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legitimating practice change that visibly was not commanded — remains contested rather than resolved: historians disagree about whether apparently voluntary diffusion genuinely lacks a coercive substrate (elite prestige engineering, anticipatory compliance with expected state preference) or whether the voluntary account is itself a retrospective legitimating myth. This story does not resolve that; it holds the endogenous account as one live, structurally coherent reading among three.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_engineered_diffusion,
    'Is the observed adoption curve genuinely driven by decentralized perceived utility, or is it downstream of elite prestige engineering and anticipatory compliance with an expected future state mandate?',
    'Comparative historical analysis of adoption timing relative to any contemporaneous state signaling, and interviews or documentary evidence from early adopters about their stated motivations versus anticipated regulatory change.',
    'If diffusion tracks anticipated state action rather than autonomous utility perception, this reading collapses toward the exogenous_override_reading and the low extractiveness/suppression scores authored here would be a mischaracterization of what is actually decree-shadowed compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_engineered_diffusion, empirical, 'Whether the voluntary-adoption account is structurally distinct from anticipatory state-decree compliance.').

omega_variable(
    holdout_coercion_threshold,
    'At what point does declining commercial or social viability for holdouts become functionally indistinguishable from coercion, even absent formal sanction?',
    'Track whether traditionalist holdouts face escalating exclusion from markets, employment, or social recognition over the interval, and whether that exclusion is organized or purely emergent from aggregate individual choices.',
    'If holdout exclusion becomes severe and organized, the tangled_rope or snare characterization would be more accurate than rope, even without formal state involvement — informal social enforcement can substitute for decree.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holdout_coercion_threshold, conceptual, 'Whether informal social/market exclusion of holdouts constitutes suppression under this reading''s own terms.').

omega_variable(
    kernel_framing_choice_signal,
    'What in the historical record justifies choosing the endogenous_displacement framing over the exogenous_override or dual_practice_equilibrium framings for a given episode of practice change?',
    'Examine the sequencing of events: does adoption precede any state decree (favoring this reading), coincide with or follow decree (favoring exogenous_override), or split cleanly by domain of use (favoring dual_practice_equilibrium)? Different historical episodes may genuinely fit different readings.',
    'Choosing this reading for an episode that actually fits exogenous_override or dual_practice_equilibrium would misattribute legitimacy to voluntary process when the true mechanism was decree or domain-partitioned coexistence, changing the classification from rope toward tangled_rope or a partitioned structure entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_signal, conceptual, 'Under-determination between the three kernel readings and what historical signals justify selecting this one.').


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
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 16, 0.13).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 8, 0.13).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 16, 0.17).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 24, 0.19).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 32, 0.21).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 40, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(legitimacy_of_practice_standardization__endogenous_displacement_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.08).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% Sibling reading within the legitimacy_of_practice_standardization kernel. exogenous_override_reading locates the same class of practice-change episodes' legitimacy in state decree rather than voluntary uptake and would show a higher extractiveness/suppression profile with named beneficiaries in state administrative apparatus and victims among populations bearing compliance costs. dual_practice_equilibrium_reading partitions legitimacy by domain (public/state vs. private/traditional) rather than resolving it uniformly, and would show a hybrid structure with distinct beneficiary/victim sets per domain. All three are linked here rather than merged, per the epsilon-invariance principle: they have different epsilon values, different victim structures (this reading has none; exogenous_override_reading does), and different type classifications for what may be the same underlying historical episode viewed through different structural commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
