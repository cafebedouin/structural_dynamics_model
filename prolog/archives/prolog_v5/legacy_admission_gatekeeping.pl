% ============================================================================
% CONSTRAINT STORY: legacy_admission_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legacy_admission_gatekeeping, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legacy_admission_gatekeeping
 *   human_readable: Legacy Admission Gatekeeping in Elite Universities
 *   domain: education/institutional_access
 *
 * SUMMARY:
 *   Legacy admission preferences in elite universities create a
 *   multi-perspective structural constraint that bundles wealth-based
 *   gatekeeping, donation incentives, identity maintenance, and
 *   merit-ideology performance. The constraint exhibits genuine coordination
 *   properties (maintaining institutional relationships with alumni and
 *   donors) alongside pure extraction (excluding qualified applicants from
 *   status-bearing credentials). The increasing theater ratio (0.48 → 0.64
 *   over the interval) reflects growing visibility of the contradiction
 *   between meritocratic rhetoric and legacy-based practice, forcing
 *   universities to expand performative diversity narratives while
 *   maintaining extraction mechanisms. The extractiveness trajectory (0.28 →
 *   0.52) shows acceleration as demographic diversity pressures increase and
 *   legal challenges mount, forcing institutions to articulate
 *   ever-more-elaborate justifications for preference mechanisms. This
 *   constraint is a canonical tangled-rope exemplar: it solves a genuine
 *   coordination problem (sustaining endowments, maintaining institutional
 *   identity continuity) while simultaneously extracting status-based
 *   exclusion from non-legacy applicants.
 *
 * KEY AGENTS:
 *   - Non-Legacy Applicants: Primary victims (powerless/trapped) — face systematic exclusion from status credentials with no functionally equivalent alternative
 *   - Legacy Families: Primary beneficiaries (institutional/arbitrage) — capture institutional status, networking access, and admission probability lift without explicit selection pressure
 *   - University Endowment/Alumni Relations: Secondary beneficiary (institutional/arbitrage) — maintains donor pipeline through preference signaling and historical relationship preservation
 *   - Aspiring Middle-Class Students: Mixed-position agent (moderate/constrained) — benefit from credential legitimacy but face resource and information barriers to access
 *   - Diversity Coalition: Organized challengers (organized/mobile) — civil rights groups, policy advocates, and researchers pushing sunset mechanisms through legal/normative pressure
 *   - Merit Ideology: Institutional narrative (institutional/arbitrage) — degraded performance of meritocratic claim; persists through inertia despite contradicting observed practice
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent wealth-gatekeeping as immutable property of positional goods
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legacy_admission_gatekeeping, 0.52).
domain_priors:suppression_score(legacy_admission_gatekeeping, 0.58).
domain_priors:theater_ratio(legacy_admission_gatekeeping, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legacy_admission_gatekeeping, extractiveness, 0.52).
narrative_ontology:constraint_metric(legacy_admission_gatekeeping, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(legacy_admission_gatekeeping, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legacy_admission_gatekeeping, tangled_rope).
narrative_ontology:human_readable(legacy_admission_gatekeeping, "Legacy Admission Gatekeeping in Elite Universities").
narrative_ontology:topic_domain(legacy_admission_gatekeeping, "education/institutional_access").

domain_priors:requires_active_enforcement(legacy_admission_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legacy_admission_gatekeeping, legacy_families).
narrative_ontology:constraint_beneficiary(legacy_admission_gatekeeping, university_endowment_maintenance).
narrative_ontology:constraint_victim(legacy_admission_gatekeeping, non_legacy_applicants).
narrative_ontology:constraint_victim(legacy_admission_gatekeeping, field_institutional_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-LEGACY APPLICANT (SNARE) — Faces systematic exclusion from elite institutional pathways with no alternative of comparable status. Career trajectories and economic outcomes are demonstrably constrained by admission gatekeeping. Cannot exit the constraint — the status differential between legacy-admitted and non-admitted groups is baked into hiring, networking, and credential recognition. Trapped with maximal experienced extraction.
constraint_indexing:constraint_classification(legacy_admission_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ASPIRING MIDDLE-CLASS STUDENT (TANGLED ROPE) — Constrained by high application costs, test preparation burdens, and limited information about institutional preferences. Also benefits from the legitimacy of elite university credentials — the system coordinates signals of competence alongside extracting exclusion. Can theoretically exit via other universities but faces prestige penalty. Mixed coordination-extraction experience.
constraint_indexing:constraint_classification(legacy_admission_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNIVERSITY ENDOWMENT/ALUMNI RELATIONS (ROPE) — Experiences legacy preference as coordination mechanism: maintains donor relationships, ensures fundraising continuity, and aligns institutional identity with historical community. Net beneficiary with full arbitrage options (can choose which applicants to admit without constraint). The constraint solves the genuine problem of sustaining institutional funding and alumni engagement.
constraint_indexing:constraint_classification(legacy_admission_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DIVERSITY COALITION (SCAFFOLD) — Organized actors (civil rights groups, policy advocates, holistic admissions researchers) see legacy preferences as a temporary coordination mechanism being displaced by alternative legitimacy claims (merit-based holistic review, socioeconomic diversity, geographic diversity). The sunset is structural: as demographic pressures and legal challenges mount, legacy preferences face institutional and legal sunset. Mobile exit options: selective schools without legacy preferences are rising in status. Theater remains high because the system must maintain legitimacy narratives (diversity statements, merit claims) while preserving legacy extraction.
constraint_indexing:constraint_classification(legacy_admission_gatekeeping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: MERIT IDEOLOGY (PITON) — The explicit institutional narrative is meritocratic: elite universities claim to select by ability. Legacy preferences are rarely defended on philosophical grounds; instead, they persist through institutional inertia. The performative layer (admissions committees conducting 'holistic review,' claiming to seek 'diverse classes') masks the actual extraction mechanism (wealth-based gatekeeping). Theater ratio reflects the gap between stated meritocratic mission and actual legacy-weighted practice. The ideology is degraded — no one defends it, but it persists.
constraint_indexing:constraint_classification(legacy_admission_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MARKET EQUILIBRIUM VIEW (MOUNTAIN) — From a civilizational/global perspective on market equilibrium, elite gatekeeping is an immutable property of status systems: any positional good (admission to a scarce, high-status institution) will naturally generate gatekeeping mechanisms, and legacy preferences are one such mechanism. This perspective naturalizes institutional gatekeeping as a structural law of scarce resource allocation. However, the structural data contradicts this — legacy preferences are not inevitable but historically contingent (rare in US before 1920s, used explicitly to exclude Jewish applicants, maintained through deliberate institutional choice). The false summit detection reveals naturalization of a contingent rent-seeking arrangement.
constraint_indexing:constraint_classification(legacy_admission_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legacy_admission_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legacy_admission_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legacy_admission_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legacy_admission_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legacy_admission_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(legacy_admission_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts meaningful status advantage for legacy applicants (demonstrably higher admission rates, network effects that persist through career), but the extraction is not as severe as pure monopolistic gatekeeping would be. Non-legacy students can still achieve elite credentials at other institutions, and the status differential, while real, does not prevent alternative success pathways. The extraction reflects wealth-based privilege amplification rather than total exclusion. The trajectory from 0.28 to 0.52 shows acceleration as the mechanism becomes more overt and challenged. Suppression (0.58): Moderate-high. Non-legacy applicants face multiple barriers: lack of information about preference weights, test preparation resource gaps, application cost burden, and internalized narratives of institutional unwelcome. However, suppression is not total — the constraint operates through preference weightings rather than explicit bars, and organized applicants can still access information and resources. Theater ratio (0.64): Moderately high. Universities explicitly claim meritocratic selection while systematically weighting non-merit factors (legacy status). The theater has increased over the interval as visibility of the contradiction has grown — universities now issue diversity statements, conduct 'holistic review' narratives, and frame legacy preference as 'institutional mission' (code for endowment protection) rather than defending it on any philosophical ground. The trajectory (0.48 → 0.64) reflects increasing performative justification burden.
 *
 * PERSPECTIVAL GAP:
 *   The snare perspective (non-legacy powerless applicant) perceives pure extraction because they are structurally trapped: admission to an elite institution is a scarce positional good, legacy preference directly reduces their probability of access, and no functionally equivalent alternative exists. Their experience of the constraint is coercive — the extraction is unavoidable and the exit option (attend a lower-status university) carries permanent earnings and network penalties. The rope perspective (university endowment) perceives pure coordination because the constraint solves a genuine institutional problem: maintaining donor relationships and funding continuity. From their vantage point, legacy preference is a legitimate coordination mechanism that aligns institutional incentives with community sustainability. The tangled rope perspective (moderate aspiring student) experiences both: the constraint coordinates educational signals (elite credentials have genuine labor market value) while simultaneously extracting status through gatekeeping. They can theoretically exit (attend other universities) but face prestige penalty. The scaffold perspective (diversity coalition) sees the extraction as temporally bounded — legal challenges (Students for Fair Admissions), normative shifts toward socioeconomic diversity, and rising prestige of merit-based alternatives are creating structural sunset for legacy preferences. The piton perspective (merit ideology) reveals that the institution no longer genuinely defends legacy preferences on philosophical grounds; instead, it maintains them through institutional inertia while increasingly resorting to performative diversity rhetoric. The false summit perspective (analytical observer naturalizing gatekeeping as immutable) is revealed as a misreading: legacy preferences are not an inherent property of positional goods but a historically specific institutional choice with alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary directionality flow runs from non-legacy applicants (victims with trapped exit → high d → high χ) toward legacy families and university endowment (beneficiaries with arbitrage exit → low d → negative χ). The beneficiary group gains admission probability lift, network effects, and identity continuity; the victim group loses status credentials and career-outcome opportunities. The aspiring middle-class student sits at the hinge: they benefit from credential legitimacy (institutional coordination) but bear extraction costs through information gaps and resource barriers. Directionality overrides are not needed here — the straightforward beneficiary/victim + exit options framework produces correct d values. The institutional/arbitrage beneficiary derives d ≈ 0.10 (nearly full beneficiary); the powerless/trapped victim derives d ≈ 0.95 (nearly full target). The divergence is stark, which explains why different indices classify the constraint so differently.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint avoids mandatrophy (false coordination/extraction confusion) by clearly satisfying all tangled-rope gates: (1) beneficiaries declared (legacy_families, university_endowment_maintenance) — genuine coordination function exists: maintaining donor relationships and institutional identity continuity; (2) victims declared (non_legacy_applicants, field_institutional_diversity) — asymmetric extraction occurs: status gatekeeping disproportionately targets non-legacy applicants; (3) requires_active_enforcement = true — the preference mechanism requires deliberate institutional choice and policy maintenance; (4) 0.40 ≤ χ ≤ 0.90 for at least one perspective — the moderate constrained student and organized coalition perspectives produce χ in this range. The analytical observer's false summit (mountain perspective) is correctly identified as a false summit: the constraint is not a law of nature but a contingent institutional arrangement that could be (and is being) displaced by alternative legitimacy criteria. The mandatrophy is resolved by acknowledging that legacy preferences solve a real coordination problem (donor relationships, institutional continuity) while simultaneously extracting status through gatekeeping, and that both mechanisms are present, neither is reducible to the other, and different perspectives legitimately perceive different aspect ratios of the hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_donation_elasticity,
    'Would legacy preference elimination significantly reduce university fundraising, or is the causal relationship between legacy preference and donations spurious?',
    'Quasi-experimental analysis of universities that have eliminated legacy preferences (Bowdoin, Amherst, etc.) and subsequent donation trends; comparison to peer institutions with unchanged legacy policies; donor survey data on decision-making',
    'If high elasticity (>30% donation decline): legacy preference is legitimately necessary for endowment maintenance (scaffolding logic). If low elasticity (<10%): legacy preference is rent-seeking with false justification (pure extraction). If medium (10-30%): tangled rope is correct classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_donation_elasticity, empirical, 'Whether fundraising requires legacy preference or if donation drops are overstated').

omega_variable(
    alternative_status_coordination,
    'Are non-legacy students able to achieve equivalent career and networking outcomes through alternative elite institutions or merit-based cohorts?',
    'Longitudinal earnings data for non-legacy graduates of top-20 universities vs legacy-admitted peers; network analysis of hiring patterns and alumni activation; prestige-adjusted career trajectory comparison',
    'If high equivalence: legacy gatekeeping is coordinate selection (Rope, theater obscures low extraction). If low equivalence: gatekeeping creates durable disadvantage (Snare, extraction is real). If partial: tangled rope classification sustained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_status_coordination, empirical, 'Whether alternative pathways provide equivalent status and outcomes').

omega_variable(
    identity_lock_magnitude,
    'Do legacy applicants experience identity fusion with their inherited institutional affiliation sufficient to make exit from legacy preference unthinkable even when elimination would increase diversity without reducing overall institutional function?',
    'Qualitative analysis of legacy donor rhetoric and alumni identity statements; behavioral data on whether legacy applicants from marginalized groups suppress diversity advocacy; survey data on identity-based resistance to preference elimination',
    'If identity lock is high: legacy beneficiaries experience the constraint as natural/inevitable (piton theater rationale confirmed). If low: beneficiaries could exit but choose not to (pure rational extraction). If identity lock is asymmetric (strong for some legacy groups, weak for others): reveals which communities have most fused identity with institutional gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_magnitude, empirical, 'Degree of identity fusion between legacy beneficiaries and institutional gatekeeping').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression of non-legacy applicants primarily structural (lack of information, test prep resources, application cost barriers) or internalized (applicants from non-legacy backgrounds internalize inferiority narratives and self-select out)?',
    'Comparison of application rates before/after public information campaigns about legacy preference impact; analysis of applicant pool composition in states with vs without affirmative action; post-admission student persistence and academic performance data',
    'If structural only: removing gatekeeping mechanisms directly increases access (scaffold sunset is real). If primarily internalized: removal of formal gatekeeping may be insufficient without additional narrative interventions. If mixed: both structural and internalized mechanisms require simultaneous intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural barriers or internalized inferiority narratives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legacy_admission_gatekeeping, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legacy_tr_t0, legacy_admission_gatekeeping, theater_ratio, 0, 0.48).
narrative_ontology:measurement(legacy_tr_t5, legacy_admission_gatekeeping, theater_ratio, 5, 0.58).
narrative_ontology:measurement(legacy_tr_t10, legacy_admission_gatekeeping, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(legacy_be_t0, legacy_admission_gatekeeping, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(legacy_be_t5, legacy_admission_gatekeeping, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(legacy_be_t10, legacy_admission_gatekeeping, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legacy_admission_gatekeeping, identity_coordination).
narrative_ontology:boltzmann_floor_override(legacy_admission_gatekeeping, 0.12).
narrative_ontology:affects_constraint(legacy_admission_gatekeeping, socioeconomic_diversity_pressure).
narrative_ontology:affects_constraint(legacy_admission_gatekeeping, affirmative_action_legal_challenges).
narrative_ontology:affects_constraint(legacy_admission_gatekeeping, wealth_credential_feedback_loop).

% DUAL FORMULATION NOTE:
% Legacy admission gatekeeping is upstream of broader status-reproduction mechanisms but represents a distinct structural constraint. Related constraints in the family include socioeconomic diversity pressures (agents organized to displace legacy preference), affirmative action legal challenges (institutional sunset mechanisms), and wealth-credential feedback loops (how status concentration through admission gatekeeping amplifies economic inequality). This story focuses on the gatekeeping mechanism itself; related stories address its downstream effects and institutional challengers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
