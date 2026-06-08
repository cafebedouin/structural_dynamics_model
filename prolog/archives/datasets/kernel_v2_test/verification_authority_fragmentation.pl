% ============================================================================
% CONSTRAINT STORY: verification_authority_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_verification_authority_fragmentation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: verification_authority_fragmentation
 *   human_readable: Verification Authority Fragmentation in Russian Military Reporting
 *   domain: military_operations_analysis/information_warfare/institutional_dysfunction
 *
 * SUMMARY:
 *   The fragmentation of verification authority in Russian military
 *   operations reporting creates a three-way epistemic deadlock with no
 *   adjudication mechanism. Russian Ministry of Defense produces optimistic
 *   official assessments; Institute for the Study of War (ISW) synthesizes
 *   open-source intelligence with Western analytical framing; Russian
 *   milbloggers provide ground-truth battlefield reporting that frequently
 *   contradicts MoD claims. Each source has institutional incentives that
 *   diverge from ground truth: MoD preserves regime legitimacy, ISW maintains
 *   analytical franchise, milbloggers capture audience through contrarian
 *   credibility. The constraint exhibits rising extraction and theater over
 *   the 2022-2024 interval as the war's duration exposes the unsustainability
 *   of MoD's optimistic narrative, forcing increased suppression to maintain
 *   the official line while milblogger contradictions proliferate. The lack
 *   of adjudication mechanism is not a bug but a feature: each institutional
 *   actor benefits from sustained ambiguity in different ways, and building
 *   shared verification protocols would require at least one actor to
 *   sacrifice institutional advantage.
 *
 * KEY AGENTS:
 *   - Russian Tactical Commander: Primary victim (powerless/trapped) — must navigate three mutually exclusive truth regimes while under fire; no exit option; career destruction if honest reporting contradicts MoD line
 *   - Operational Ground Truth: Abstract victim (powerless/trapped) — the actual battlefield state has no advocate and no mechanism for adjudication; epistemic commons degraded by institutional extraction
 *   - External Intelligence Analyst: Secondary victim (moderate/constrained) — must synthesize contradictory sources; benefits from triangulation but systematically biased toward institutionally backed sources; moderate extraction
 *   - ISW Analytical Franchise: Primary beneficiary (institutional/arbitrage) — fragmentation creates demand for authoritative synthesis; captures analytical market share; low effective extraction
 *   - Russian MoD Institutional Authority: Mixed actor (institutional/constrained) — benefits from domestic narrative control but constrained by milblogger erosion of credibility; extraction from domestic audience, suppression cost rising
 *   - Milblogger Audience Capture: Beneficiary (organized/mobile) — contrarian credibility captures audience; benefits from MoD's loss of credibility; mobile exit options (can shift platforms, audiences)
 *   - Russian State Media Apparatus: Institutional actor (institutional/arbitrage) — maintains performative verification ritual despite epistemic collapse; piton perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(verification_authority_fragmentation, 0.68).
domain_priors:suppression_score(verification_authority_fragmentation, 0.72).
domain_priors:theater_ratio(verification_authority_fragmentation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(verification_authority_fragmentation, extractiveness, 0.68).
narrative_ontology:constraint_metric(verification_authority_fragmentation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(verification_authority_fragmentation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(verification_authority_fragmentation, tangled_rope).
narrative_ontology:human_readable(verification_authority_fragmentation, "Verification Authority Fragmentation in Russian Military Reporting").
narrative_ontology:topic_domain(verification_authority_fragmentation, "military_operations_analysis/information_warfare/institutional_dysfunction").

domain_priors:requires_active_enforcement(verification_authority_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(verification_authority_fragmentation, russian_mod_institutional_authority).
narrative_ontology:constraint_beneficiary(verification_authority_fragmentation, isw_analytical_franchise).
narrative_ontology:constraint_beneficiary(verification_authority_fragmentation, milblogger_audience_capture).
narrative_ontology:constraint_victim(verification_authority_fragmentation, operational_ground_truth).
narrative_ontology:constraint_victim(verification_authority_fragmentation, russian_tactical_commanders).
narrative_ontology:constraint_victim(verification_authority_fragmentation, external_intelligence_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RUSSIAN TACTICAL COMMANDER (SNARE) — Trapped between contradictory reporting chains with no exit. MoD demands optimistic reports; milbloggers expose failures; actual battlefield conditions determine survival. Cannot reconcile the three without career destruction. Maximum extraction: forced to navigate mutually exclusive truth regimes while under fire.
constraint_indexing:constraint_classification(verification_authority_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: EXTERNAL INTELLIGENCE ANALYST (TANGLED ROPE) — Constrained by need to synthesize contradictory sources but benefits from triangulation opportunities. The fragmentation creates genuine analytical work (coordination function) but also systematic bias toward sources with institutional backing. Moderate extraction: career depends on navigating the contradiction, but some agency exists in source weighting.
constraint_indexing:constraint_classification(verification_authority_fragmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ISW ANALYTICAL FRANCHISE (ROPE) — Primary beneficiary. The fragmentation creates demand for authoritative synthesis. ISW's institutional position allows arbitrage between Russian official sources and milblogger ground truth, capturing analytical market share. Low effective extraction: the constraint subsidizes ISW's epistemic authority.
constraint_indexing:constraint_classification(verification_authority_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RUSSIAN MOD INSTITUTIONAL AUTHORITY (TANGLED ROPE) — Benefits from monopoly on official narrative but constrained by milblogger contradictions eroding credibility. The fragmentation preserves domestic information control (coordination for regime) while leaking operational reality. Mixed: extraction from domestic audience, but suppression cost rising as milbloggers gain traction.
constraint_indexing:constraint_classification(verification_authority_fragmentation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RUSSIAN CIVIL SOCIETY HYPOTHETICAL (SCAFFOLD) — If organized civil society could form, the fragmentation would be temporary: distributed verification via milbloggers is building alternative epistemic infrastructure that could sunset MoD monopoly. But this perspective is counterfactual — the suppression prevents the coalition from forming. Scaffold classification reflects the structural potential, not the realized state.
constraint_indexing:constraint_classification(verification_authority_fragmentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: RUSSIAN STATE MEDIA APPARATUS (PITON) — The official verification ritual (MoD briefings, state TV war coverage) is largely performative. Domestic audience increasingly aware that milbloggers provide ground truth. The apparatus persists through institutional inertia and coercive enforcement, not functional credibility. High theater ratio: the verification performance continues despite epistemic collapse.
constraint_indexing:constraint_classification(verification_authority_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The fragmentation serves dual function: genuine coordination problem (multiple observers produce different ground truth) and extraction mechanism (institutional actors benefit from ambiguity). The lack of adjudication mechanism is not accidental — it preserves plausible deniability for MoD, analytical franchise for ISW, and audience capture for milbloggers. Structural asymmetry: those with exit options benefit; those without bear costs.
constraint_indexing:constraint_classification(verification_authority_fragmentation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(verification_authority_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(verification_authority_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(verification_authority_fragmentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(verification_authority_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(verification_authority_fragmentation, TR),
    TR >= 0.70.

:- end_tests(verification_authority_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The fragmentation extracts substantially from those without exit options (tactical commanders forced to navigate contradictory reporting chains, external analysts dependent on synthesis, operational ground truth with no adjudication mechanism) while benefiting institutional actors with arbitrage capacity (ISW analytical franchise, milblogger audience capture, MoD domestic narrative control). The extraction has increased over the interval as war duration makes MoD optimism unsustainable, forcing commanders into sharper contradictions. Suppression (0.72): High. Maintaining the fragmentation requires active suppression: MoD must prevent honest tactical reporting from surfacing, state media must suppress milblogger contradictions in domestic coverage, and the lack of adjudication mechanism must be actively maintained against attempts to build shared verification protocols. Suppression has increased as milblogger credibility has grown and MoD's narrative has diverged further from battlefield reality. Theater ratio (0.58): Moderate-high. MoD briefings and state media war coverage are substantially performative — domestic audience increasingly aware that milbloggers provide ground truth, but the ritual continues through institutional inertia and coercive enforcement. Theater has increased as the gap between official narrative and milblogger reporting has widened, making the performance more obvious.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how institutional position determines whether fragmentation appears as coordination problem or extraction mechanism. ISW sees coordination (Rope) — they are solving the legitimate problem of synthesizing contradictory sources and capturing analytical market share fairly. Russian MoD sees mixed coordination and extraction (Tangled Rope) — the fragmentation preserves domestic information control but at rising suppression cost as milbloggers erode credibility. Tactical commanders see pure extraction (Snare) — trapped between mutually exclusive truth regimes with no exit and no adjudication. External analysts see mixed coordination and extraction (Tangled Rope) — genuine analytical work required but systematic bias toward institutionally backed sources. State media sees degraded ritual (Piton) — the verification performance continues despite epistemic collapse. The analytical observer sees the fragmentation as structurally dual: genuine coordination problem (multiple observers, no shared protocol) AND extraction mechanism (institutional actors benefit from sustained ambiguity). The perspectival gap is not resolvable by better measurement — it reflects real differences in structural position relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the extraction flow. Beneficiaries (ISW analytical franchise, milblogger audience capture, MoD domestic narrative control) experience low or negative effective extraction — the fragmentation subsidizes their institutional authority or audience capture. Victims (tactical commanders, operational ground truth, external intelligence consumers) experience high effective extraction — they bear the costs of navigating contradictory sources with no adjudication mechanism. The Russian MoD is a mixed case: benefits from domestic narrative control (beneficiary) but constrained by rising suppression costs as milblogger contradictions proliferate (partial victim). The directionality derivation follows from beneficiary/victim declarations plus exit options: trapped agents with victim status get high d (high experienced extraction); arbitrage agents with beneficiary status get low d (low or negative experienced extraction); constrained agents with mixed status get moderate d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that fragmentation serves dual function: it is BOTH a genuine coordination problem (multiple verification authorities with no shared protocol) AND an extraction mechanism (institutional actors benefit from sustained ambiguity). The coordination function is real — external analysts genuinely need to synthesize contradictory sources, and building shared verification protocols would reduce analytical overhead. The extraction function is also real — ISW captures analytical franchise, MoD preserves domestic narrative control, milbloggers capture audience through contrarian credibility, and all three benefit from the absence of adjudication. The tangled_rope classification at the analytical level reflects this duality: the constraint cannot be reduced to pure coordination (rope) because identifiable victims exist and active enforcement is required to maintain the fragmentation; it cannot be reduced to pure extraction (snare) because genuine coordination work is required to navigate the contradictory sources. The mandatrophy is resolved by recognizing that the same structural arrangement serves both functions simultaneously, and the perspectival gap between beneficiaries (who see coordination) and victims (who see extraction) is the measurement the framework exists to take.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adjudication_mechanism_absence,
    'Is the absence of adjudication mechanism a coordination failure (no actor has incentive to build it) or an extraction feature (multiple actors benefit from sustained ambiguity)?',
    'Historical analysis of attempts to build cross-source verification protocols; identification of which actors blocked or undermined such attempts; correlation between fragmentation persistence and institutional funding/authority gains',
    'If coordination failure: constraint is rope from more perspectives, and technical solution (shared verification protocol) is feasible. If extraction feature: constraint is snare/tangled_rope from more perspectives, and technical solution will be actively suppressed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adjudication_mechanism_absence, empirical, 'Whether adjudication absence is coordination failure or extraction feature').

omega_variable(
    milblogger_independence,
    'Are Russian milbloggers genuinely independent verification sources, or are they partially controlled opposition serving regime information management strategy?',
    'Network analysis of milblogger funding sources, editorial red lines, and correlation between milblogger criticism timing and MoD strategic messaging; comparison of milblogger vs external OSINT on same events',
    'If genuinely independent: fragmentation represents real epistemic pluralism and potential sunset of MoD monopoly. If controlled opposition: fragmentation is theater — apparent contradiction masks coordinated information operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(milblogger_independence, empirical, 'Whether milbloggers are independent or controlled opposition').

omega_variable(
    territorial_control_operationalization,
    'What constitutes ''territorial control'' for verification purposes — physical occupation, administrative function, contested fire control, or claimed sovereignty?',
    'Comparison of territorial control definitions across sources; identification of systematic divergence patterns; correlation between definition choice and institutional incentives',
    'If definitions are incommensurable: fragmentation is partly conceptual (different sources measure different things). If definitions converge but claims diverge: fragmentation is purely empirical/political.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(territorial_control_operationalization, conceptual, 'Operationalization ambiguity in territorial control measurement').

omega_variable(
    isw_epistemic_capture,
    'Does ISW''s institutional position as ''authoritative synthesis'' create incentive to perpetuate rather than resolve source fragmentation?',
    'Analysis of ISW funding model, citation patterns in policy documents, and correlation between fragmentation severity and ISW institutional growth; comparison of ISW assessments in high-fragmentation vs low-fragmentation theaters',
    'If ISW benefits from fragmentation: the analytical franchise is extraction mechanism, not coordination service. If ISW would benefit from resolution: the franchise is genuine coordination attempting to solve collective action problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(isw_epistemic_capture, empirical, 'Whether ISW has institutional incentive to perpetuate fragmentation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(verification_authority_fragmentation, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_initial, verification_authority_fragmentation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(verif_auth_frag_tr_t3, verification_authority_fragmentation, theater_ratio, 3, 0.42).
narrative_ontology:measurement(verif_auth_frag_tr_t6, verification_authority_fragmentation, theater_ratio, 6, 0.51).
narrative_ontology:measurement(verif_auth_frag_tr_t9, verification_authority_fragmentation, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(verif_auth_frag_be_t0, verification_authority_fragmentation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(verif_auth_frag_be_t3, verification_authority_fragmentation, base_extractiveness, 3, 0.56).
narrative_ontology:measurement(verif_auth_frag_be_t6, verification_authority_fragmentation, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(verif_auth_frag_be_t9, verification_authority_fragmentation, base_extractiveness, 9, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(verif_auth_frag_su_t0, verification_authority_fragmentation, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(verif_auth_frag_su_t3, verification_authority_fragmentation, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(verif_auth_frag_su_t6, verification_authority_fragmentation, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(verif_auth_frag_su_t9, verification_authority_fragmentation, suppression_requirement, 9, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(verification_authority_fragmentation, information_standard).
narrative_ontology:affects_constraint(verification_authority_fragmentation, russian_domestic_information_control).
narrative_ontology:affects_constraint(verification_authority_fragmentation, western_intelligence_assessment_reliability).
narrative_ontology:affects_constraint(verification_authority_fragmentation, tactical_command_reporting_chains).

% DUAL FORMULATION NOTE:
% Verification authority fragmentation is upstream of specific battlefield assessment constraints (territorial control claims, casualty figures, equipment losses) but represents a distinct structural constraint. The downstream constraints have their own extractiveness values reflecting the empirical status of specific claims; the fragmentation constraint has its own extractiveness reflecting the institutional incentives that prevent adjudication mechanism formation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(verification_authority_fragmentation, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
