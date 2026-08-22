% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__degrowth_reading, []).

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
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Growth-Bounded Mitigation Arrangement (Degrowth Reading)
 *   domain: climate policy / political economy / intergenerational ethics
 *
 * SUMMARY:
 *   This story instantiates the degrowth reading of the
 *   climate_harm_prevention kernel. The standing arrangement under contest —
 *   the mitigation-within-growth-framework consensus that has structured
 *   international climate negotiations since the UNFCCC's founding in 1992 —
 *   is assessed here by the degrowth reading's own lights: as a coordination
 *   mechanism (a genuine shared carbon budget problem exists) captured by an
 *   asymmetric extraction structure (Global North consumption preserved,
 *   Global South and future generations bear the physical costs of
 *   insufficient contraction). The ε authored here is for THIS standing
 *   arrangement, not for the degrowth reading's own endorsed alternative
 *   (planned contraction), which would show near-zero extraction if it were
 *   ever the referent — per the ε-referent rule for kernel readings.
 *
 * KEY AGENTS:
 *   - global_north_consuming_classes: primary beneficiary of continued growth-compatible consumption
 *   - fossil_and_growth_dependent_capital: agenda-setter defining what counts as legitimate climate policy
 *   - global_south_frontline_states: primary target bearing physical climate costs
 *   - future_generations: primary target bearing deferred, compounding costs
 *   - global_south_subsistence_populations: primary target with no exit from local climate exposure
 *   - degrowth_policy_advocates: excluded voice arguing the growth boundary itself is the constraint
 *   - climate_science_assessment_bodies: analytical observer documenting the physical budget gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.81).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.72).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Growth-Bounded Mitigation Arrangement (Degrowth Reading)").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate policy / political economy / intergenerational ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, '16745265-2715-4543-8686-516100038071').
narrative_ontology:cs_kernel_codification('16745265-2715-4543-8686-516100038071', distributed).
narrative_ontology:cs_authority_grounding('16745265-2715-4543-8686-516100038071', distributed).
narrative_ontology:cs_reading_relation('16745265-2715-4543-8686-516100038071', climate_harm_prevention__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('16745265-2715-4543-8686-516100038071', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_axiom('16745265-2715-4543-8686-516100038071', foundational, growth_is_not_a_valid_constraint_boundary).
narrative_ontology:cs_axiom_status(growth_is_not_a_valid_constraint_boundary, holdable).
narrative_ontology:cs_axiom_grounding('16745265-2715-4543-8686-516100038071', growth_is_not_a_valid_constraint_boundary, empirically_contingent).
narrative_ontology:cs_axiom('16745265-2715-4543-8686-516100038071', foundational, present_global_north_consumption_may_be_legitimately_contracted_for_distant_harm_prevention).
narrative_ontology:cs_axiom_status(present_global_north_consumption_may_be_legitimately_contracted_for_distant_harm_prevention, holdable).
narrative_ontology:cs_axiom_grounding('16745265-2715-4543-8686-516100038071', present_global_north_consumption_may_be_legitimately_contracted_for_distant_harm_prevention, deontological).
narrative_ontology:cs_reference_frame('16745265-2715-4543-8686-516100038071', unbounded_growth_compatible_mitigation_consensus).
narrative_ontology:cs_drift_state('16745265-2715-4543-8686-516100038071', post_paris_agreement_implementation_gap, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('16745265-2715-4543-8686-516100038071', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_north_consuming_classes).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, fossil_and_growth_dependent_capital).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_south_frontline_states).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_south_subsistence_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains current consumption levels, mobility, and material standard of living under the growth-framework arrangement. Bears none of the deferred climate costs directly and experiences the mitigation-within-growth story as normal, uncontested life; has strong political voice to resist any contraction proposal at the ballot box.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_consuming_classes, beneficiary,
    organized, biographical, mobile, national).

% Sets the terms of what counts as a 'legitimate' climate response by funding technology-transition narratives, lobbying against contraction policy, and financing political campaigns. Captures continued profit from a growth-bounded framework; can relocate capital and production across jurisdictions to avoid any national contraction regime that does get enacted.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, fossil_and_growth_dependent_capital, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, fossil_and_growth_dependent_capital, beneficiary).

% Absorbs the physical consequences of continued Global North emissions — sea level rise, drought, monsoon disruption — that a growth-bounded mitigation pathway does not adequately prevent. Has no plausible relocation of national territory and minimal leverage over Global North domestic growth politics; participates in UNFCCC processes but cannot compel contraction.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_frontline_states, payer,
    powerless, generational, trapped, global).

% Inherits whatever emissions budget is left after the growth-framework's mitigation pace is exhausted; cannot participate in present political decisions about growth or contraction and bears cumulative risk of tipping-point exceedance that current policy discounts heavily.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Rural and coastal communities dependent on climate-stable agriculture and fisheries; displacement and crop failure follow directly from warming the growth-bounded mitigation pathway permits. Migration is heavily restricted by destination-country border regimes, foreclosing exit even as local conditions worsen.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_subsistence_populations, payer,
    powerless, biographical, trapped, regional).

% Academics, movement organizations, and some subnational governments arguing planned contraction is the only physically consistent pathway; systematically excluded from mainstream climate-policy negotiating tables, which are structured around growth-compatible technological transition and treat contraction as politically inadmissible a priori.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, degrowth_policy_advocates, excluded,
    moderate, generational, constrained, national).

% Produce carbon budget and warming trajectory assessments (e.g. IPCC) that document the physical gap between growth-compatible mitigation pledges and required emissions pathways, without themselves adjudicating which policy response — mitigation, degrowth, or adaptation — is legitimate.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, climate_science_assessment_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_harm_prevention__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under the degrowth reading, the legitimate coordination function is remaining within a finite, physically-bounded global carbon budget shared across generations and nations — a genuine collective-action problem requiring someone's consumption to fall for the budget to hold.
% TRANSFER_FUNCTION: The growth-framework arrangement this reading contests moves climate-stable conditions and remaining emissions budget from the Global South and future generations to the Global North's present consumption; the degrowth reading itself proposes reversing that transfer by moving consumption capacity from Global North present populations toward emissions-budget preservation for the excluded parties.
% ABSENT_VOICES: Degrowth policy advocates and Global South negotiating blocs raise planned contraction in international forums but are structurally excluded from the frame that treats growth as a fixed constraint; their objection — that mitigation-within-growth is not physically achievable at the pledged pace — is heard but not adopted as the operative premise of negotiated agreements.
% DISAPPEARANCE_RATIONALE: If the growth-bounded mitigation arrangement disappeared and were replaced by planned contraction, Global North consumption patterns and capital accumulation would rearrange substantially; whether the world overall 'unchanged' or 'rearranges' is itself the site of dispute between readings — the degrowth reading holds the current arrangement's disappearance would rearrange emissions trajectories in a way that world_rearranges captures, while beneficiaries of the current arrangement contest that framing entirely.
% FOUNDING_PROBLEM: The felt problem the growth-framework mitigation arrangement was built to solve: how to reduce emissions without requiring any redistribution of consumption or contraction of Global North economies, preserving the political viability of climate action within existing growth-oriented electoral and market institutions.
% FOUNDING_PROBLEM_CORROBORATION: Climate science assessment bodies attest, from outside any beneficiary group, that the pace of technological emissions reduction pledged under growth-compatible frameworks is inconsistent with declared carbon budgets (IPCC AR6 and related assessments); degrowth advocates and Global South negotiators corroborate that the founding problem — reconciling growth with the physical carbon budget — remains unsolved rather than resolved by technological transition to date. Fossil and growth-dependent capital and Global North consuming classes, the beneficiaries, maintain the founding problem is being adequately addressed.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, contested).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__degrowth_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 by 2024) because the growth-framework arrangement systematically defers physical costs onto parties who cannot object within the negotiating structure — the transfer from Global South/future generations to Global North present consumption is direct and measurable in cumulative emissions accounting. Suppression (0.72) reflects the active political and institutional work required to keep contraction off the table: growth is treated as an unquestioned constraint boundary in every major negotiating text, which is itself a suppressive move against the degrowth position rather than a neutral finding. Theater ratio rises over the interval (0.30 to 0.58) because an increasing share of climate diplomacy activity — pledges, net-zero targets, voluntary commitments — substitutes for the physical contraction the degrowth reading holds is required, consistent with Goodhart-style metric substitution. Accessibility collapse is moderate (0.40) rather than high because degrowth remains a live, articulated alternative in academic and some political discourse — it has not been fully suppressed from view, only excluded from operative negotiating frames. Resistance is high (0.78): degrowth advocates, Global South blocs, and youth/future-oriented movements actively contest the growth-bounded frame, which is inconsistent with a settled mountain and consistent with a constructed, contested arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of global_north_consuming_classes and fossil_and_growth_dependent_capital, the growth-bounded mitigation arrangement looks like reasonable, incremental coordination — a rope solving a genuine emissions problem without unnecessary disruption. From the seat of global_south_frontline_states and future_generations, the identical structure computes as extraction: their carbon budget share and climate-stable future are being consumed by parties who face no proportionate cost. The engine should register this as tangled_rope precisely because both readings are structurally present in the same arrangement — genuine coordination function AND asymmetric extraction — not because either seat's perception is simply mistaken.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North consuming classes and growth-dependent capital sit near the beneficiary end of directionality: they retain consumption and profit while the arrangement's costs are displaced. Global South frontline states, subsistence populations, and future generations sit near the full-target end: trapped exit options (no relocation of national territory, no participation by the unborn, restricted migration), civilizational or generational time horizons that compound the harm, and no leverage to alter the growth boundary treated as fixed. Fossil capital's arbitrage-grade exit (relocating capital and production across jurisdictions) further damps its effective extraction relative to its structural benefit — it can escape even the partial accountability mechanisms that do exist.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling emissions reduction with a physically bounded carbon budget — remains live by the assessment of climate science bodies external to every beneficiary group, yet the growth-framework mitigation arrangement increasingly substitutes pledge theater for the contraction the degrowth reading holds physically necessary. This is not simple mandatrophy (mandate outliving function) but contested mandatrophy: the beneficiaries maintain the founding problem is being addressed through technological transition, while the excluded degrowth position and the payer seats hold the arrangement has drifted from live coordination toward extraction dressed as coordination. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (a shared carbon budget problem is real) while registering the asymmetric extraction the degrowth reading identifies — avoiding both the false-summit error (treating growth-boundedness as a neutral mountain) and the over-correction of labeling all climate diplomacy pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_boundary_naturalness,
    'Is treating economic growth as a fixed, non-negotiable constraint boundary a structural necessity of modern political economy, or a constructed convention that benefits identifiable parties (growth-dependent capital, incumbent consuming classes) who would lose from its removal?',
    'Comparative institutional analysis of jurisdictions or historical periods that have implemented planned contraction or steady-state policies without systemic collapse (e.g. wartime rationing economies, post-growth municipal experiments) to test whether growth-boundedness is physically required or politically defended.',
    'If growth-boundedness is constructed rather than physically required, the mitigation_priority sibling reading''s core premise (growth-compatible technological transition suffices) loses its physical grounding and the degrowth reading''s classification as coordination-plus-extraction strengthens toward the extraction pole. If growth-boundedness is physically required for social stability, this reading''s rejection of the growth boundary as a valid constraint weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(growth_boundary_naturalness, conceptual, 'Whether the growth constraint boundary rejected by this reading is physically necessary or a constructed, interest-serving convention.').

omega_variable(
    kernel_disagreement_locus,
    'Where exactly do the three sibling readings of climate_harm_prevention (degrowth, mitigation_priority, adaptation_priority) disagree — is it about physical feasibility (can emissions fall fast enough within growth?), about the moral weighting of present versus future/distant harm, or about the legitimacy of coercive redistribution as a policy instrument?',
    'Structural decomposition of each reading''s foundational axioms (see cs_structure.axioms across the three files) cross-referenced against empirical emissions-pathway modeling to separate the physically contested claims from the normatively contested claims.',
    'If the disagreement is purely physical (feasibility of mitigation-within-growth), it is empirically resolvable and the kernel could in principle converge on one reading as evidence accumulates. If the disagreement is normative (whose harm counts, who may be coerced), the readings coexist indefinitely as genuinely different value commitments, which is the structural reason all three are authored as separate coexisting constraints rather than competing hypotheses about one fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_locus, conceptual, 'Whether the sibling readings of the climate_harm_prevention kernel disagree on physical feasibility, moral weighting, or policy legitimacy — and thus whether the kernel is empirically or normatively contested.').

omega_variable(
    degrowth_own_extraction_risk,
    'Could a planned-contraction policy itself become extractive — imposing disproportionate costs on Global North working classes or precarious populations rather than growth-dependent capital — if implemented without careful distributional design?',
    'Distributional modeling of proposed degrowth policy instruments (e.g. carbon rationing, wealth caps, working-time reduction) to identify who within the Global North would bear contraction costs under realistic implementation.',
    'If the endorsed alternative (planned contraction) would itself concentrate costs on a subset of Global North beneficiaries here, the degrowth reading''s own prescriptive claim would require its own separate constraint story with a non-zero ε for that alternative arrangement — consistent with the rule that ε is authored for the standing arrangement, not the endorsed alternative, but flagging that the alternative is not automatically extraction-free once implemented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degrowth_own_extraction_risk, empirical, 'Whether the degrowth reading''s own endorsed policy alternative would introduce new extractive asymmetries within the Global North.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_harm_prevention__degrowth_reading, theater_ratio, 1992, 0.3).
narrative_ontology:measurement_basis(clim_tr_t1992, observed).
narrative_ontology:measurement(clim_tr_t1997, climate_harm_prevention__degrowth_reading, theater_ratio, 1997, 0.35).
narrative_ontology:measurement_basis(clim_tr_t1997, observed).
narrative_ontology:measurement(clim_tr_t2005, climate_harm_prevention__degrowth_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement_basis(clim_tr_t2005, observed).
narrative_ontology:measurement(clim_tr_t2012, climate_harm_prevention__degrowth_reading, theater_ratio, 2012, 0.48).
narrative_ontology:measurement_basis(clim_tr_t2012, observed).
narrative_ontology:measurement(clim_tr_t2018, climate_harm_prevention__degrowth_reading, theater_ratio, 2018, 0.53).
narrative_ontology:measurement_basis(clim_tr_t2018, observed).
narrative_ontology:measurement(clim_tr_t2024, climate_harm_prevention__degrowth_reading, theater_ratio, 2024, 0.58).
narrative_ontology:measurement_basis(clim_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_harm_prevention__degrowth_reading, base_extractiveness, 1992, 0.55).
narrative_ontology:measurement_basis(clim_be_t1992, observed).
narrative_ontology:measurement(clim_be_t1997, climate_harm_prevention__degrowth_reading, base_extractiveness, 1997, 0.6).
narrative_ontology:measurement_basis(clim_be_t1997, observed).
narrative_ontology:measurement(clim_be_t2005, climate_harm_prevention__degrowth_reading, base_extractiveness, 2005, 0.66).
narrative_ontology:measurement_basis(clim_be_t2005, observed).
narrative_ontology:measurement(clim_be_t2012, climate_harm_prevention__degrowth_reading, base_extractiveness, 2012, 0.71).
narrative_ontology:measurement_basis(clim_be_t2012, observed).
narrative_ontology:measurement(clim_be_t2018, climate_harm_prevention__degrowth_reading, base_extractiveness, 2018, 0.77).
narrative_ontology:measurement_basis(clim_be_t2018, observed).
narrative_ontology:measurement(clim_be_t2024, climate_harm_prevention__degrowth_reading, base_extractiveness, 2024, 0.81).
narrative_ontology:measurement_basis(clim_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_harm_prevention__degrowth_reading, suppression_requirement, 1992, 0.45).
narrative_ontology:measurement_basis(clim_su_t1992, observed).
narrative_ontology:measurement(clim_su_t1997, climate_harm_prevention__degrowth_reading, suppression_requirement, 1997, 0.52).
narrative_ontology:measurement_basis(clim_su_t1997, observed).
narrative_ontology:measurement(clim_su_t2005, climate_harm_prevention__degrowth_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement_basis(clim_su_t2005, observed).
narrative_ontology:measurement(clim_su_t2012, climate_harm_prevention__degrowth_reading, suppression_requirement, 2012, 0.63).
narrative_ontology:measurement_basis(clim_su_t2012, observed).
narrative_ontology:measurement(clim_su_t2018, climate_harm_prevention__degrowth_reading, suppression_requirement, 2018, 0.68).
narrative_ontology:measurement_basis(clim_su_t2018, observed).
narrative_ontology:measurement(clim_su_t2024, climate_harm_prevention__degrowth_reading, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(clim_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__degrowth_reading, 0.1).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__adaptation_priority).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the climate_harm_prevention kernel, decomposed per the ε-invariance principle because the natural-language label 'legitimate climate response' covers three structurally distinct claims with different beneficiary/victim structures and different physical premises about growth compatibility. degrowth_reading authors ε=0.81 for the growth-bounded mitigation arrangement as this reading sees it (high extraction: Global South and future generations bear deferred costs while Global North consumption is preserved). mitigation_priority and adaptation_priority are separate files with their own ε, stakeholders, and classification, sharing the same underlying carbon-budget coordination problem but differing on whether growth is a fixed boundary and on the acceptable warming trajectory. All three link to each other via affects_constraints because a shift in one reading's political viability (e.g. degrowth gaining traction) structurally pressures the resource and legitimacy conditions available to the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
