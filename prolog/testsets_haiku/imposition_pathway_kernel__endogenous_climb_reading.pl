% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__endogenous_climb_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: imposition_pathway_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Fringe-to-Center Commitment Climb Pathway
 *   domain: historical/institutional/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the mechanism by which apparent state
 *   impositions of new commitments (calendar systems, dress codes,
 *   organizational forms) are actually compressed climbs: fringe actors in
 *   trade, military, and intellectual contact zones voluntarily adopt new
 *   systems, discover their efficacy, and establish them as de facto
 *   standards before state decree makes them mandatory. The Meiji calendar
 *   reform (1872) is the canonical example: Gregorian calendar adoption in
 *   treaty ports preceded the decree by decades; the state decree accelerated
 *   and universalized an already-normalized system. This reading contests the
 *   sibling reading (exogenous_override_reading) which asserts state capacity
 *   can impose commitment change de novo, without fringe adoption. This
 *   reading's core empirical claim is that fringe adoption is ALWAYS present,
 *   though often invisible in state records; the state's enforcement is
 *   ratification of existing climbs, not imposition on resistant populations.
 *
 * KEY AGENTS:
 *   - modernizing_fringe_adopters — early merchants, military personnel, intellectuals in treaty ports who voluntarily experiment with new systems
 *   - merchant_cosmopolitan_class — traders across jurisdictions who benefit from standardization and establish de facto standards in contact zones
 *   - early_state_coordinators — state actors (military modernizers, administrative reformers) who observe fringe success and adopt proven systems
 *   - traditional_system_defenders — communities whose legitimacy derives from old systems, bearing conversion costs after consensus shifts
 *   - state_enforcement_apparatus — military/administrative bodies whose enforcement is light because fringe adoption has normalized the new system
 *   - international_contact_zones — treaty ports and merchant networks where heterogeneity creates demand for standards; the discovery nursery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__endogenous_climb_reading, 0.31).
domain_priors:suppression_score(imposition_pathway_kernel__endogenous_climb_reading, 0.18).
domain_priors:theater_ratio(imposition_pathway_kernel__endogenous_climb_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__endogenous_climb_reading, rope).
narrative_ontology:human_readable(imposition_pathway_kernel__endogenous_climb_reading, "Endogenous Fringe-to-Center Commitment Climb Pathway").
narrative_ontology:topic_domain(imposition_pathway_kernel__endogenous_climb_reading, "historical/institutional/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__endogenous_climb_reading, 'd7a14cf6-7228-4369-94e5-304e90d569c9').
narrative_ontology:cs_kernel_codification('d7a14cf6-7228-4369-94e5-304e90d569c9', distributed).
narrative_ontology:cs_authority_grounding('d7a14cf6-7228-4369-94e5-304e90d569c9', practice).
narrative_ontology:cs_reading_relation('d7a14cf6-7228-4369-94e5-304e90d569c9', imposition_pathway_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7a14cf6-7228-4369-94e5-304e90d569c9', imposition_pathway_kernel__hybrid_cascade_reading, coexists_with).
narrative_ontology:cs_axiom('d7a14cf6-7228-4369-94e5-304e90d569c9', foundational, fringe_adoption_precedes_state_codification).
narrative_ontology:cs_axiom_status(fringe_adoption_precedes_state_codification, holdable).
narrative_ontology:cs_axiom_grounding('d7a14cf6-7228-4369-94e5-304e90d569c9', fringe_adoption_precedes_state_codification, empirically_contingent).
narrative_ontology:cs_axiom('d7a14cf6-7228-4369-94e5-304e90d569c9', foundational, state_decree_accelerates_proven_systems).
narrative_ontology:cs_axiom_status(state_decree_accelerates_proven_systems, holdable).
narrative_ontology:cs_axiom_grounding('d7a14cf6-7228-4369-94e5-304e90d569c9', state_decree_accelerates_proven_systems, empirically_contingent).
narrative_ontology:cs_reference_frame('d7a14cf6-7228-4369-94e5-304e90d569c9', organic_fringe_discovery_process).
narrative_ontology:cs_drift_state('d7a14cf6-7228-4369-94e5-304e90d569c9', state_decree_acceleration_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d7a14cf6-7228-4369-94e5-304e90d569c9', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, modernizing_fringe_adopters).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, merchant_cosmopolitan_class).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, early_state_coordinators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, international_contact_zones).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, traditional_system_defenders).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__endogenous_climb_reading, commitment_displacement_is_endogenous_process).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__endogenous_climb_reading, state_decree_accelerates_existing_climb).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__endogenous_climb_reading, fringe_adoption_precedes_apparent_imposition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Early adopters in treaty ports and contact zones: merchants, military personnel, intellectuals who voluntarily adopt new calendar systems, dress codes, organizational forms because they perceive coordination or status benefits. They are the discovery layer for what works; their choices are not coerced but incentivized by market access, peer standing, or military effectiveness. They gain directly from early adoption — first-mover advantage in trade, status elevation in modernizing hierarchies.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, modernizing_fringe_adopters, beneficiary,
    moderate, biographical, mobile, regional).

% Traders across jurisdictions who operate in multiple calendar/measurement systems simultaneously and benefit from standardization that reduces transaction friction. Their voluntary adoption of shared calendars and standards in treaty ports creates a de facto coordination substrate that makes later state adoption cheaper. They have the highest exit options — they can exit to any jurisdiction where their preferred system operates — which paradoxically makes their voluntary adoption the strongest signal of coordination value.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, merchant_cosmopolitan_class, beneficiary,
    powerful, generational, arbitrage, continental).

% State actors (military modernizers, administrative reformers) who observe fringe adoption succeeding in adjacent domains and adopt new calendars/standards themselves as administrative tools. They set the formal decree that appears to impose the new system, but the decree ratifies and accelerates an already-existing climb. Their enforcement is enforcement of the new state consensus, not imposition on a resistant population. They benefit from the coordination gains the fringe layer discovered.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, early_state_coordinators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__endogenous_climb_reading, early_state_coordinators, beneficiary).

% Communities and institutions whose legitimacy derives from the old calendar/measurement system (religious calendars, traditional ceremonies, hereditary administrative practices). When the climb reaches critical mass and the state decree accelerates the transition, they bear the cost of system conversion. Their resistance is real but comes late — the fringe adoption layer has already normalized the new system sufficiently that state decree meets less resistance than it would have if imposed de novo. They cannot exit (territorial, identity-locked) but could have opted in earlier at lower cost.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, traditional_system_defenders, payer,
    organized, biographical, constrained, national).

% Military and administrative bodies that execute the formal decree. They apply enforcement when the decree arrives, but because fringe adoption has normalized the new system, enforcement is light — ratifying consensus rather than crushing resistance. Their enforcement role appears to impose commitment change top-down, but is actually accelerating a climb already in motion. Their power is structural (they can suppress resistance) but only necessary at the margins where the climb has not yet reached.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, trapped, national).

% Treaty ports, merchant networks, military academies where actors from multiple jurisdictions interact and adopt shared systems. These are the structural nurseries of fringe adoption. They benefit because the heterogeneity there creates demand for standards; the standards that emerge in contact zones later climb into the state system when state actors observe and adopt them. International contact zones are where the reading's mechanism is most visible.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, international_contact_zones, beneficiary,
    powerful, generational, arbitrage, global).

% Scholars and analysts who reconstruct commitment displacement processes. This reading asserts they MUST find fringe adoption layers (even if compressed or invisible in state records) preceding every apparent imposition. If they find evidence of true top-down imposition without fringe adoption, the reading is falsified. Their role is to measure whether the reading's empirical claim holds.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, institutional_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__endogenous_climb_reading, early_state_coordinators).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The fringe adoption layer solves the discovery problem: which new commitments (calendar systems, dress codes, organizational forms) actually work across heterogeneous contexts? Private merchants and early military adopters experiment with new systems in low-stakes environments (trade, voluntary codes); the ones that survive this filter provide proven templates for state adoption. The state then accelerates the climb by making the proven system mandatory, reducing holdout costs. Coordination is achieved through a discovery-ratification pipeline, not top-down mandate.
% TRANSFER_FUNCTION: Moves the cost of commitment switching from individual agents (who bore discovery risk, learning costs, early coordination failure in fringe adoption) to the state (which bears enforcement costs for universal adoption). Also transfers legitimacy: from fringe-level voluntary adopters to state-level decree-making, which allows traditional-system defenders to blame state coercion rather than peer pressure or market incentives.
% ABSENT_VOICES: Actors in pre-contact, non-fringe regions who are unaware of the climb until the decree arrives have no voice in the fringe adoption process. Their retrospective experience of imposition is structurally real but temporally posterior to the fringe layer's voluntary adoption decisions.
% DISAPPEARANCE_RATIONALE: If the fringe adoption mechanism disappeared (i.e., if state actors could not observe and learn from merchant/military/intellectual fringe adoption), states would have to impose calendar and organizational commitments without proven efficacy, facing higher resistance and higher enforcement costs. The pathway exists because it reduces the cost of commitment displacement by allowing states to adopt only proven systems. Without it, commitment change would require either pure imposition (much more costly) or much longer organic diffusion from heterogeneous experiments. The mechanism is essential to how state-level modernization occurs.
% FOUNDING_PROBLEM: How can states adopt new organizational, temporal, and social commitments without massive resistance and enforcement cost? How can a state know which new commitments are worth adopting across an entire population? The fringe adoption layer solves both: fringe actors discover which commitments work, and state actors can observe the proof and adopt, using state authority to accelerate a change that fringe actors have already shown to be workable.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Meiji Restoration (e.g., Marius Jansen, Andrew Gordon) document pre-decree fringe adoption of calendar, dress, and military codes in treaty ports and military academies; the state decree in 1872 for the Gregorian calendar followed merchant and military adoption, not preceded it. David Landes on timekeeping standardization, Janet Hunter on merchant capitalism in Japan, and comparative historians of Qing administrative reform (e.g., Benjamin Elman on the adoption of Jesuit-influenced astronomical systems) all trace fringe adoption preceding state codification. Corroboration comes from outside the state apparatus that issued the decree.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__endogenous_climb_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__endogenous_climb_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW and RISING over the interval (0.08 → 0.31) because the mechanism itself is not inherently extractive — fringe adoption is voluntary, and state decree accelerates a proven system. However, extractiveness rises as the state apparatus captures and mandates the system, transforming it from voluntary coordination to enforced uniformity. At t=0 (pure fringe, pre-decree), extraction is near zero — the pathway is genuine coordination. By t=32-40 (decree enforced, universal adoption), extraction rises to 0.31 because traditional-system defenders now bear conversion costs involuntarily. Theater ratio is low (0.22 at interval end) because the enforcement mechanism is light — state apparatus ratifies consensus rather than crushing resistance, so performative activity is minimal. Suppression is very low (0.18) because alternatives have already been tested and rejected by fringe actors, so the state's suppression is enforcing the outcome of a discovery process, not suppressing real resistance to untested systems. Accessibility collapse is low-to-moderate (0.41) because fringe-level actors had genuine alternatives during the climb; traditional-system defenders face collapsed alternatives only after consensus shifts. Resistance is moderate (0.38) — real from tradition defenders, but muted because the climb has normalized the new system before state decree arrives.
 *
 * PERSPECTIVAL GAP:
 *   Fringe adopters and state coordinators experience this constraint as low-extractiveness coordination: discovering what works and then scaling it. Traditional-system defenders experience it as extraction: they had no say in the fringe discovery process, see the state decree as imposition, and bear conversion costs. The engine should compute high d (near target) for traditional defenders and low d (near beneficiary) for fringe adopters, reflecting this asymmetry. The state enforcement apparatus occupies a middle position: they are the execution seat, but their enforcement is light because they are ratifying consensus, not imposing on resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Modernizing fringe adopters (d near 0.2): voluntary adopters, benefit from early discovery and status elevation, high exit options (can migrate to contact zones or exit the system), derive directionality from beneficiary role with high mobility. Merchant cosmopolitan class (d near 0.1): highest exit options (can operate anywhere), arbitrage-grade mobility, pure beneficiary from standardization. Early state coordinators (d near 0.25): institutional power but constrained by need to observe fringe success before acting; they are beneficiaries but dependent on fringe layer discovery. Traditional system defenders (d near 0.75): constrained exit (identity-locked, territorial), payer role, discover conversion cost only after consensus shifts; high directionality toward target because they bear cost without having chosen participation in the climb. State enforcement apparatus (d near 0.5): symmetric position — they execute the decree but have strong incentive for the system to work (their institutional effectiveness depends on it). No overrides needed; structural data drives appropriate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT exhibit mandatrophy because the founding problem remains live: states continue to face the challenge of adopting new commitments efficiently, and the fringe pathway continues to operate. The constraint's function (enabling low-cost commitment adoption) is still operant. However, a related mandatrophy trap exists at the state level: once a state makes a decree, the appearance of imposition can obscure the fringe layer that enabled it, leading future analysts and policymakers to believe states CAN impose commitments de novo. This false belief (exogenous_override_reading) could lead to wasteful top-down imposition attempts without discovering fringe efficacy first. The mechanism itself is not degraded, but its visibility is — the theater ratio rises modestly (0.22) because state actors have incentive to take credit for the commitment shift, obscuring the fringe discovery that made it possible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_visibility_problem,
    'How do we detect fringe adoption in historical record when state authorities suppress or erase pre-decree evidence? Is the absence of documented fringe adoption proof that no climb occurred, or proof only that fringe adoption was invisible to state recordkeepers?',
    'Microhistorical reconstruction using merchant archives, military academy records, treaty port documents, and private correspondence. The more detailed the non-state sources, the more fringe adoption becomes visible. Alternatively, comparative analysis: do jurisdictions with detailed fringe-layer records always show pre-decree adoption, while jurisdictions with state-only records show none? If so, the pattern is recording bias, not absence of climb.',
    'If fringe adoption is structurally invisible in state records, the reading becomes unfalsifiable — any apparent imposition can be explained as compression of invisible climb. This undermines the reading''s empirical grounding. If fringe-layer sources reliably show pre-decree adoption, the reading is strongly supported.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fringe_visibility_problem, empirical, 'Whether fringe adoption is always present or sometimes truly absent, masked by historical record gaps.').

omega_variable(
    exogenous_vs_endogenous_boundary,
    'What distinguishes a true endogenous climb (fringe discovers and voluntarily adopts, state ratifies) from a state-engineered pseudo-fringe (state mandates military/bureaucratic adoption, which then appears as fringe-level precedent for universal decree)?',
    'Temporal and institutional analysis: did fringe adoption precede state decree by sufficient margin that state actors COULD NOT have engineered the fringe? Did fringe adoption occur in jurisdictions (merchant networks, foreign enclaves) outside direct state control? Did state actors explicitly cite fringe precedent as justification for decree?',
    'If the boundary is blurry or if states routinely engineer pseudo-fringe, the distinction between endogenous and hybrid collapse, and the reading converges to hybrid_cascade_reading. If the boundary is clear (fringe in uncontrolled jurisdictions, long temporal lead, explicit state citation), the reading is distinct and defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_vs_endogenous_boundary, empirical, 'Whether endogenous climb is empirically distinguishable from state-engineered pseudo-fringe.').

omega_variable(
    reading_scope_ambiguity,
    'Does this reading apply to ALL commitment displacement, or only to certain classes (e.g., practical/technical commitments like calendars and dress, but not ideological or religious commitments)?',
    'Test the reading across diverse commitment domains: do religious conversions, ideological systems, and political hierarchies show fringe adoption layers before state codification? If they do, the reading is universal; if only technical commitments show the pattern, the reading must be scoped narrowly.',
    'If scoped narrowly, the reading is a claim about institutional/technical systems, not a general theory of commitment displacement. If universal, it is a stronger structural claim about how ALL commitments propagate. Scope affects which sibling reading it coexists with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_scope_ambiguity, empirical, 'Whether the endogenous climb mechanism applies to all commitment types or only technical/institutional commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(impo_tr_t0, observed).
narrative_ontology:measurement(impo_tr_t8, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 8, 0.08).
narrative_ontology:measurement_basis(impo_tr_t8, observed).
narrative_ontology:measurement(impo_tr_t16, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement_basis(impo_tr_t16, observed).
narrative_ontology:measurement(impo_tr_t24, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement_basis(impo_tr_t24, observed).
narrative_ontology:measurement(impo_tr_t32, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 32, 0.22).
narrative_ontology:measurement_basis(impo_tr_t32, observed).
narrative_ontology:measurement(impo_tr_t40, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(impo_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(impo_be_t0, observed).
narrative_ontology:measurement(impo_be_t8, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement_basis(impo_be_t8, observed).
narrative_ontology:measurement(impo_be_t16, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 16, 0.22).
narrative_ontology:measurement_basis(impo_be_t16, observed).
narrative_ontology:measurement(impo_be_t24, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 24, 0.28).
narrative_ontology:measurement_basis(impo_be_t24, observed).
narrative_ontology:measurement(impo_be_t32, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 32, 0.31).
narrative_ontology:measurement_basis(impo_be_t32, observed).
narrative_ontology:measurement(impo_be_t40, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(impo_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement_basis(impo_su_t0, observed).
narrative_ontology:measurement(impo_su_t8, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 8, 0.05).
narrative_ontology:measurement_basis(impo_su_t8, observed).
narrative_ontology:measurement(impo_su_t16, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 16, 0.09).
narrative_ontology:measurement_basis(impo_su_t16, observed).
narrative_ontology:measurement(impo_su_t24, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 24, 0.14).
narrative_ontology:measurement_basis(impo_su_t24, observed).
narrative_ontology:measurement(impo_su_t32, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 32, 0.17).
narrative_ontology:measurement_basis(impo_su_t32, observed).
narrative_ontology:measurement(impo_su_t40, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement_basis(impo_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__endogenous_climb_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__endogenous_climb_reading, 0.08).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel imposition_pathway_kernel. The kernel asks: how do states adopt new commitments? This reading (endogenous_climb) asserts: through fringe adoption that precedes apparent imposition. The sibling exogenous_override_reading asserts states can impose de novo. The sibling hybrid_cascade_reading asserts states create artificial fringe. The three readings coexist as live scholarly positions; none logically forecloses the others (all are defensible interpretations of Meiji data and comparable cases), but they make different empirical claims about the universal prevalence of the climb mechanism. The constraint family models this indeterminacy as three separate constraints with distinct ε values and evidence bases. The endogenous_climb reading has lower extractiveness (climb is voluntary → state accelerates) than exogenous_override (imposition without precedent → higher resistance) or hybrid_cascade (state creates fringe → ambiguous agency).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
