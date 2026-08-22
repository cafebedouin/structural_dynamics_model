% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__lapsed_alternatives_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Layout Persistence — Lapsed Alternatives Reading
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   The QWERTY key arrangement, fixed in the 1870s typewriter era and shipped
 *   worldwide on Remington machines, remains the default input layout on
 *   virtually every keyboard manufactured today despite repeated ergonomic
 *   alternatives — Dvorak (1936), Colemak (2006) — that remain freely
 *   available and never reach critical mass. This story instantiates the
 *   lapsed_alternatives_reading: the arrangement persists because a single
 *   shared layout solves a real coordination problem — skill portability,
 *   hardware interchangeability, curricular economy, hiring legibility — and
 *   alternatives lapse because each would-be adopter bears the full private
 *   switching cost up front while the benefit depends on a collective
 *   adoption that never arrives. On this reading no party administers,
 *   enforces, or profits from the arrangement; the only charge it levies is
 *   the private cost of leaving it. Claimed type and metrics are authored
 *   independently: the claim is rope; the metrics describe modest, symmetric,
 *   self-borne costs that decline over the interval as software remapping
 *   removed the hardware barrier to experimentation. KEY AGENTS (by
 *   structural relationship): - individual_typists: mass adherents
 *   (powerless/constrained) — receive skill portability; bear switching costs
 *   only if they personally deviate - keyboard_manufacturers: equipment-side
 *   adherents (organized/constrained) — tooling and inventory follow the
 *   installed base - typing_education_providers: instruction-side adherents
 *   (organized/constrained) — teach the layout the labor market expects -
 *   os_platform_vendors: default reproducers (institutional/mobile) — inherit
 *   and re-ship the default; hold technical exit they have no reason to use -
 *   alternative_layout_communities: would-be coordinators outside the
 *   conversation (moderate/constrained) — bear full switching cost, cannot
 *   assemble critical mass - human_factors_researchers: analytical observer —
 *   sees the full structure: live coordination function, lapsed alternatives,
 *   no enforcing party
 *
 * KEY AGENTS:
 *   - individual_typists: mass adherents (powerless/constrained) — receive skill portability; bear switching costs only if they personally deviate
 *   - keyboard_manufacturers: equipment-side adherents (organized/constrained) — tooling and inventory follow the installed base
 *   - typing_education_providers: instruction-side adherents (organized/constrained) — teach the layout the labor market expects
 *   - os_platform_vendors: default reproducers (institutional/mobile) — inherit and re-ship the default; hold technical exit they have no reason to use
 *   - alternative_layout_communities: would-be coordinators outside the conversation (moderate/constrained) — bear full switching cost, cannot assemble critical mass
 *   - human_factors_researchers: analytical observer — sees the full structure: live coordination function, lapsed alternatives, no enforcing party
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.16).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.05).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Layout Persistence — Lapsed Alternatives Reading").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology_history/industrial_standards/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, '49cdce3e-8a63-4e55-9566-5bf1ce34fb04').
narrative_ontology:cs_kernel_codification('49cdce3e-8a63-4e55-9566-5bf1ce34fb04', implicit).
narrative_ontology:cs_authority_grounding('49cdce3e-8a63-4e55-9566-5bf1ce34fb04', practice).
narrative_ontology:cs_interpretation_layer_present('49cdce3e-8a63-4e55-9566-5bf1ce34fb04').
narrative_ontology:cs_reading_relation('49cdce3e-8a63-4e55-9566-5bf1ce34fb04', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('49cdce3e-8a63-4e55-9566-5bf1ce34fb04', foundational, adoption_dynamics_select_standards).
narrative_ontology:cs_axiom_status(adoption_dynamics_select_standards, holdable).
narrative_ontology:cs_axiom_grounding('49cdce3e-8a63-4e55-9566-5bf1ce34fb04', adoption_dynamics_select_standards, empirically_contingent).
narrative_ontology:cs_axiom('49cdce3e-8a63-4e55-9566-5bf1ce34fb04', foundational, deviation_costs_are_unclaimed).
narrative_ontology:cs_axiom_status(deviation_costs_are_unclaimed, holdable).
narrative_ontology:cs_axiom_grounding('49cdce3e-8a63-4e55-9566-5bf1ce34fb04', deviation_costs_are_unclaimed, empirically_contingent).
narrative_ontology:cs_reference_frame('49cdce3e-8a63-4e55-9566-5bf1ce34fb04', decentralized_coordination_equilibrium).
narrative_ontology:cs_drift_state('49cdce3e-8a63-4e55-9566-5bf1ce34fb04', contemporary_soft_input_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('49cdce3e-8a63-4e55-9566-5bf1ce34fb04', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, individual_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, typing_education_providers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, os_platform_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Types on whatever layout the surrounding equipment already uses; the value of their typing skill comes from its portability across jobs, machines, and countries, which only a universally shared layout provides. Leaving personally means weeks of slowed output and a private mismatch with every shared keyboard they touch; staying costs them nothing beyond the layout's disputed ergonomics.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, individual_typists, beneficiary,
    powerless, biographical, constrained, global).

% Tooling, inventory, and product lines are built to the dominant layout because demand concentrates there; producing alternative-layout hardware means carrying slow-moving stock against uncertain orders. They neither set nor defend the default; they follow the installed base.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, beneficiary,
    organized, generational, constrained, global).

% Schools, vocational courses, and online tutors teach the layout employers expect, because graduates need portable, recognizable skills. Curriculum follows labor-market demand rather than ergonomic research; switching what they teach would strand students with a minority skill.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, typing_education_providers, beneficiary,
    organized, biographical, constrained, national).

% Ship the inherited layout as the factory default in firmware, operating systems, and mobile keyboards, and maintain remapping support for alternatives. Changing the default would break user expectation worldwide for no competitive gain, so the default reproduces itself through them; they retain easy technical exit — remapping interfaces exist — but no commercial reason to use it.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, os_platform_vendors, beneficiary,
    institutional, generational, mobile, global).

% Designers and user communities behind alternative layouts publish ergonomic arguments, free remapping tools, and training material, and have done so for decades. Each convert bears the full private switching cost immediately while the benefit they seek — a larger coordinated user base — accrues only if adoption reaches critical mass, which it never has. They stand outside the rooms where defaults, curricula, and product lines are set; their objection is heard and answered with the coordination argument, not suppressed.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_communities, excluded,
    moderate, biographical, constrained, global).

% Study typing ergonomics, retraining costs, and standard-selection dynamics across decades; they see the whole structure — the live coordination function, the lapsed alternatives, the absence of any enforcing party — and publish assessments that neither side of practice much acts on.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, human_factors_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__lapsed_alternatives_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence__lapsed_alternatives_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains one shared key layout so that typing skill, hardware, instructional material, and hiring expectations remain mutually compatible across users, employers, and equipment makers — a compatibility problem solved once by common adoption instead of negotiated per workplace.
% TRANSFER_FUNCTION: Transfers nothing between parties: no payment, work, attention, or status moves through the layout itself. The only cost in the system is the private switching cost a deviator bears when adopting an alternative, and no seat collects it.
% ABSENT_VOICES: Alternative-layout designers and their user communities would object that superior layouts are locked out of default status; they are outside the conversation in the specific sense that no forum exists where default-setting happens for them to attend — defaults reproduce through procurement, habit, and curriculum rather than through any decision meeting. Ergonomic-injury clinicians also hold a standing interest that reaches the conversation only episodically.
% DISAPPEARANCE_RATIONALE: Typing skill, hardware inventory, school curricula, hiring tests, and software defaults all presuppose the shared layout. Overnight dissolution would force simultaneous re-coordination — every employer, school, and manufacturer choosing and teaching a replacement — with a multi-year transition of incompatible skills and equipment before a successor convention stabilized.
% FOUNDING_PROBLEM: The layout was standardized in the 1870s typewriter era, when type bars swinging to a common platen could collide and jam; the arrangement of keys was fixed to pace operator input against the machine's mechanical recovery.
% FOUNDING_PROBLEM_CORROBORATION: Mechanical-engineering assessments of modern keyboards confirm no type-bar collision constraint exists — the founding mechanism has no contemporary referent; typewriter historiography documents the jamming-era origin independently of any party that benefits from the layout today. No beneficiary-dependent source is needed for the dead-status finding, and none contradicts it.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__lapsed_alternatives_reading_tests).
:- end_tests(qwerty_persistence__lapsed_alternatives_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.16 at interval end) because the arrangement's entire charge is the private switching cost of deviation — retraining hours, a transient productivity dip, mismatch with shared equipment — and no seat collects it; the series declines from 0.32 to 0.16 as operating-system remapping removed the hardware cost of experimenting, making the persistence of the standard under falling exit costs the constraint's central datum. Suppression is authored near zero (0.05) and is a raw structural property, unscaled by power or scope: no enforcement machinery exists or ever existed; deviation is lawful, unsanctioned, and technically trivial, and the only pressure a deviator feels is the network externality that IS the coordination function. Theater is low throughout (0.08-0.15): typing pedagogy retains some legacy drill ritual and standards documentation describes rather than sustains the layout; the small mid-interval bumps track advocacy-interest cycles (postwar efficiency studies, the internet-era alternative-layout revival) rather than any oscillating extraction mechanism. Accessibility_collapse (0.35) records partial practical collapse only: alternatives remain fully accessible technically — free remapping, purchasable hardware — but sit below critical mass, so the practical alternative set is thin without being closed. Resistance (0.08) reflects marginal, dissipating advocacy rather than organized opposition. Fixing — coordinating a global transition to any alternative — is prohibitive relative to its disputed benefit, and no seat exists with both the mandate and the margin to attempt it; combined with diffuse gains this is the expensive-to-change face of a healthy convention, not an atrophied one, because the coordination function remains live. Both tracked series run on one shared seven-point grid spanning 1936-2026 so every metric is authored at every examined time point; suppression_requirement is deliberately untracked because the enforcement picture is statically empty and is carried by the scalar instead.
 *
 * PERSPECTIVAL GAP:
 *   From inside a workplace the layout is experienced as a fact of nature — mountain-like phenomenology on a structurally conventional base — because every neighboring keyboard confirms it. From the alternative-layout advocate's seat the same structure is experienced as a wall: decades of argument produce no movement. Under this reading neither experience indicates an extractor: the first is the phenomenology of a successful convention, the second is the arithmetic of critical mass failing to clear. The engine computes per-seat classifications from power, exit, and role; the divergence between the effortless-compatibility seat and the frustrated-advocate seat is real and requires no beneficiary-victim asymmetry to generate.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated party derives a low d from the beneficiary declarations: typists receive portability worth more than their probabilistic share of switching-cost exposure; manufacturers and educators ride demand concentrations they did not create; platform vendors inherit a default whose reproduction costs them nothing and whose alteration would cost them users. Unseated deviators — the individuals who actually leave — bear d near 1.0, but their costs are self-borne and uncollected, which is precisely what keeps epsilon at the switching-cost floor rather than converting it into anyone's revenue. No directionality overrides are needed: the derivation from beneficiary structure plus exit options reproduces the intended relationships, and the excluded seat (alternative_layout_communities) is commentary-grade by design and drives no correction. Receipt surface: no seat receives the arrangement's charge — the switching costs of deviators accrue to no one — so gain_flow is authored as the affirmative diffuse claim after checking every named seat; manufacturers' and vendors' ordinary commerce in standard-compatible goods is coordination surplus, not extraction receipts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — pacing operator input against type-bar jamming — is dead: no modern keyboard contains a type bar, a fact attested by mechanical engineering rather than by any beneficiary. The arrangement nonetheless persists by serving a successor function the founders did not intend: inter-user coordination. The R5 mismatch (status=dead x verdict=world_rearranges) raises a zombie flag, and the cross-check is what clears it: theater is low, gain_flow is diffuse with no capturing seat, and the successor function is demonstrably load-bearing — any multinational onboarding pipeline, temp-labor pool, or hardware supply chain exhibits it. Reading the dead founding mandate alone would misclassify a live rope as a piton; reading the live coordination function alone would miss the soft-input erosion recorded in drift_state, whereby voice input and predictive text are gradually decoupling text production from key layout. Mandatrophy here took the form of functional migration, not capture: the mandate died, a new one grew over it, and no one administered either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_qwerty_persistence,
    'This constraint is one reading (lapsed_alternatives_reading) of the kernel qwerty_persistence; the sibling reading (incumbent_preservation_reading) holds that the same persistence is produced by incumbents actively defending capital investments. Which causal structure actually sustains the standing arrangement?',
    'Comparative institutional record: search for documented episodes of incumbents blocking alternative-layout adoption (contractual exclusivity, predatory pricing, standards-body capture) versus natural experiments in which alternatives faced no identifiable opposition and still failed to reach critical mass.',
    'If the sibling reading is correct, epsilon rises sharply (rent-protection extraction), a victim set appears (alternative-layout producers and the consumers paying an inefficiency premium), and the classification shifts toward tangled_rope or snare; if this reading is correct, the rope classification stands with epsilon set by switching costs alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_qwerty_persistence, empirical, 'Which reading of the qwerty_persistence kernel explains the standing arrangement.').

omega_variable(
    individual_switching_cost_magnitude,
    'How large is the actual private cost of individual deviation from the standard — retraining hours, temporary productivity loss, shared-equipment mismatch — relative to the coordination value received?',
    'Controlled retraining studies with productivity telemetry and longitudinal tracking of individual converts'' output curves; reconcile the conflicting historical estimates (vendor-era retraining claims versus retrospective economic critiques).',
    'Sets epsilon directly: trivial costs push epsilon toward the coordination floor and the arrangement toward pure convention; large costs raise epsilon while keeping it symmetric and uncollected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_switching_cost_magnitude, empirical, 'Magnitude of the private switching cost that constitutes this reading''s entire epsilon.').

omega_variable(
    critical_mass_threshold_counterfactual,
    'Did alternatives lapse because adoption could not reach critical mass, or because latent demand for them was weaker than advocates claim?',
    'Track insulated adoption pockets (firms, rehabilitation clinics, programming communities that coordinated internally on alternative layouts): sustained pockets indicate viability at small scale and equilibrium selection; universal decay even in insulated settings indicates hidden costs the coordination account misses.',
    'Sustained pockets strengthen the rope reading (alternatives lapse for coordination-arithmetic reasons); universal decay implies unmodeled private costs, raising effective epsilon and complicating the symmetry assumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_threshold_counterfactual, empirical, 'Whether non-adoption is an equilibrium-selection outcome or preference revelation.').

omega_variable(
    cost_symmetry_across_seats,
    'Are coordination costs truly borne symmetrically, or do some seats — hunt-and-peck typists, multilingual users, assistive-device users, high-volume data-entry workers — bear disproportionate burdens that would constitute a diffuse victim set?',
    'Distributional analysis of who retrains, who purchases specialty hardware, and whose error rates or strain injuries track the layout; occupational-health data stratified by typing population.',
    'If burdens are systematically asymmetric, a victim set emerges and the classification drifts from rope toward tangled_rope; confirmed symmetry locks the rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_symmetry_across_seats, empirical, 'Symmetry of the cost burden underlying the empty-victim declaration.').

omega_variable(
    explanatory_exclusivity_of_readings,
    'Are the two readings of the kernel rival explanations (at most one true) or complementary layers (coordination value sets the baseline persistence; incumbent defense explains specific blocked transitions)?',
    'Episode-level attribution: classify each historical transition attempt (Dvorak commercialization, government trials, internet-era revivals) as coordination-limited, opposition-limited, or both, and test whether the residual after removing coordination limits correlates with documented incumbent action.',
    'If complementary, this story''s epsilon covers only the coordination layer and the sibling story covers the residual; each classification stays clean but joint attribution of the arrangement''s persistence becomes additive rather than adjudicative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(explanatory_exclusivity_of_readings, conceptual, 'Whether the kernel''s readings are rivals or layers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(qwer_tr_t0, observed).
narrative_ontology:measurement(qwer_tr_t15, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(qwer_tr_t15, observed).
narrative_ontology:measurement(qwer_tr_t30, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement_basis(qwer_tr_t30, observed).
narrative_ontology:measurement(qwer_tr_t45, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 45, 0.1).
narrative_ontology:measurement_basis(qwer_tr_t45, observed).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement_basis(qwer_tr_t60, observed).
narrative_ontology:measurement(qwer_tr_t75, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 75, 0.14).
narrative_ontology:measurement_basis(qwer_tr_t75, observed).
narrative_ontology:measurement(qwer_tr_t90, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 90, 0.15).
narrative_ontology:measurement_basis(qwer_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(qwer_be_t0, observed).
narrative_ontology:measurement(qwer_be_t15, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 15, 0.3).
narrative_ontology:measurement_basis(qwer_be_t15, observed).
narrative_ontology:measurement(qwer_be_t30, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement_basis(qwer_be_t30, observed).
narrative_ontology:measurement(qwer_be_t45, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 45, 0.26).
narrative_ontology:measurement_basis(qwer_be_t45, observed).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 60, 0.22).
narrative_ontology:measurement_basis(qwer_be_t60, observed).
narrative_ontology:measurement(qwer_be_t75, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 75, 0.18).
narrative_ontology:measurement_basis(qwer_be_t75, observed).
narrative_ontology:measurement(qwer_be_t90, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 90, 0.16).
narrative_ontology:measurement_basis(qwer_be_t90, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence__lapsed_alternatives_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% The colloquial claim 'QWERTY persists because of path dependence' conflates two structurally distinct explanations of the same standing arrangement. This story instantiates the lapsed_alternatives_reading: persistence via coordination value, alternatives lapsing below critical mass, epsilon equal to private switching costs only, no beneficiary concentration, no victims. The sibling story (incumbent_preservation_reading) instantiates the incumbent-defense account: persistence via active preservation of capital investments, with correspondingly higher reading-indexed epsilon and a non-empty victim set. Both stories take the SAME referent (the standing arrangement — QWERTY's persistence) and differ only in the reading-indexed assessment of it; they are linked here as a constraint family and must not be merged into one story, since their epsilon values and victim structures are mutually inconsistent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
