% ============================================================================
% CONSTRAINT STORY: criminal_conviction_reversal_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_criminal_conviction_reversal_friction, []).

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
 *   constraint_id: criminal_conviction_reversal_friction
 *   human_readable: Criminal Conviction Reversal Friction
 *   domain: criminal_justice/appellate_process
 *
 * SUMMARY:
 *   Criminal conviction reversal friction represents a structural constraint
 *   in appellate systems where institutional, procedural, and resource
 *   barriers systematically suppress the correction of wrongful convictions.
 *   The constraint operates between trapped victims (wrongly convicted
 *   persons) who bear the full cost of systemic error and institutional
 *   beneficiaries (prosecutorial agencies, conviction preservation
 *   incentives) who benefit from conviction finality and resource scarcity in
 *   appeal systems. The extractiveness of 0.58 reflects that the system
 *   extracts freedom, time, and life opportunity from those it has wrongly
 *   convicted while maintaining appearance of due process through theatrical
 *   procedural review. Suppression of 0.72 captures the multiple barriers to
 *   reversal: strict evidentiary standards (newly discovered evidence rules),
 *   procedural finality doctrines, resource asymmetry between prosecution and
 *   defense, institutional resistance to reopening cases, and appellate
 *   docket pressure that creates disincentives for reversal. Theater ratio of
 *   0.68 reflects that appellate review maintains performative correctness
 *   (written opinions, standards of review, evidentiary hearings) while
 *   functionally preserving convictions through procedural friction rather
 *   than evidence-based reversal.
 *
 * KEY AGENTS:
 *   - Wrongly Convicted Persons: Primary victims (powerless/trapped) — bear full extraction of freedom and life years; lack resources and legal standing for reversal
 *   - Prosecutorial Agencies: Primary beneficiaries (institutional/arbitrage) — benefit from conviction preservation, resource scarcity, and performance metrics tied to conviction rates
 *   - Innocence Verification System: Secondary victim (moderate/constrained) — operates under severe resource constraints, procedural barriers, and institutional resistance; moral pressure prevents exit despite constrained options
 *   - Appellate Courts: Institutional actor (organized/constrained) — constrained by docket pressure and legitimacy concerns; benefit from finality while having genuine correctness interest
 *   - Legal Formalism and Procedural Theater: Institutional mechanism (institutional/arbitrage) — maintains appearance of due process while functionally preserving convictions through rigid evidentiary standards
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing finality doctrine as immutable legal requirement rather than contingent policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(criminal_conviction_reversal_friction, 0.58).
domain_priors:suppression_score(criminal_conviction_reversal_friction, 0.72).
domain_priors:theater_ratio(criminal_conviction_reversal_friction, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(criminal_conviction_reversal_friction, extractiveness, 0.58).
narrative_ontology:constraint_metric(criminal_conviction_reversal_friction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(criminal_conviction_reversal_friction, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(criminal_conviction_reversal_friction, snare).
narrative_ontology:human_readable(criminal_conviction_reversal_friction, "Criminal Conviction Reversal Friction").
narrative_ontology:topic_domain(criminal_conviction_reversal_friction, "criminal_justice/appellate_process").

domain_priors:requires_active_enforcement(criminal_conviction_reversal_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(criminal_conviction_reversal_friction, prosecutorial_agencies).
narrative_ontology:constraint_beneficiary(criminal_conviction_reversal_friction, conviction_preservation_incentives).
narrative_ontology:constraint_victim(criminal_conviction_reversal_friction, wrongly_convicted_persons).
narrative_ontology:constraint_victim(criminal_conviction_reversal_friction, innocence_verification_system).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WRONGLY CONVICTED PERSON (SNARE) — Trapped by procedural barriers, evidentiary standards, and institutional inertia. Lacks resources for prolonged appeals, faces stigma and employment barriers. The system extracts their labor (incarceration), freedom, and life years with minimal mechanism for correction. Zero meaningful exit options.
constraint_indexing:constraint_classification(criminal_conviction_reversal_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INNOCENCE VERIFICATION SYSTEM (SNARE) — Operates under severe resource constraints, procedural friction, and institutional resistance. Can theoretically exit (decline to pursue cases) but faces moral/ethical suppression. Experiences the constraint as high coercion with minimal coordination benefit. Must fight entrenched prosecutorial incentives to function.
constraint_indexing:constraint_classification(criminal_conviction_reversal_friction, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROSECUTORIAL AGENCIES (ROPE) — Benefit from conviction preservation through resource allocation, performance metrics (conviction rates), and institutional continuity. Experience the constraint as coordination: maintaining conviction finality enables prosecutorial confidence and case closure. Net beneficiary with high exit optionality.
constraint_indexing:constraint_classification(criminal_conviction_reversal_friction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: APPELLATE COURTS (TANGLED ROPE) — Constrained by docket pressure, precedent requirements, and institutional legitimacy concerns. Benefit from finality (reduces appeals volume) but also have genuine interest in correctness. Mixed coordination (efficient case resolution) and extraction (suppression of reversal pathways) with significant enforcement requirements.
constraint_indexing:constraint_classification(criminal_conviction_reversal_friction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROCEDURAL FORMALISM (PITON) — The appellate process maintains high theater through rigid evidentiary standards (newly discovered evidence rules, harmless error doctrine, Brady materiality thresholds) that appear neutral but functionally preserve convictions. These procedures persist through institutional inertia despite empirical evidence of wrongful convictions. Formalism is performative — the ritual of review appears thorough while systematically disfavoring reversal.
constraint_indexing:constraint_classification(criminal_conviction_reversal_friction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, conviction finality appears as an immutable requirement of legal systems: every system must eventually close cases and prevent infinite appeals. This perspective naturalizes what is actually a policy choice about error tolerance. The engine's false summit detector will identify this as naturalization — finality is not a law of nature but a contingent institutional arrangement trading error correction against system stability.
constraint_indexing:constraint_classification(criminal_conviction_reversal_friction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(criminal_conviction_reversal_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(criminal_conviction_reversal_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(criminal_conviction_reversal_friction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(criminal_conviction_reversal_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(criminal_conviction_reversal_friction, TR),
    TR >= 0.70.

:- end_tests(criminal_conviction_reversal_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from wrongly convicted persons (freedom, time, opportunity, dignity) while providing little coordination benefit to victims. The extracted value accrues to prosecutorial agencies (conviction rate metrics, resource allocation, institutional continuity) and legal formalism (legitimacy through appearance of review). The metric reflects that extraction is substantial but not absolute — some reversals occur, and DNA evidence has forced reform in high-profile cases. The trajectory from 0.35 to 0.58 shows that extractive friction has increased as institutional resistance to reversal has hardened and evidentiary standards have become more restrictive. Suppression (0.72): High. Multiple barriers suppress reversal pathways: (1) Procedural barriers (harmless error doctrine, newly discovered evidence rules that require showings of materiality and impact); (2) Resource asymmetry (prosecution has vastly more investigative resources than defense appeals); (3) Institutional inertia (courts reluctant to revisit settled convictions due to legitimacy concerns and docket pressure); (4) Brady violation suppression (materiality standard requires showing that withheld evidence would have changed outcome, a high bar); (5) Temporal barriers (statute of limitations on motions, timeliness requirements that trap claims). Suppression is not total — some mechanisms exist (DNA testing statutes, actual innocence claims) — but barriers are substantial and require wealthy/privileged access (innocence organizations, pro bono counsel). Theater ratio (0.68): Moderate-high. Appellate process maintains significant performative content: written opinions, standards of review, evidentiary hearings, reasoned analysis all create appearance of thorough correctness verification. Yet the functional outcome is conviction preservation — theater disguises systematic friction. The trajectory from 0.52 to 0.68 shows theater increasing as procedures become more elaborate (harmless error frameworks, Brady materiality standards) while reversals become less frequent.
 *
 * PERSPECTIVAL GAP:
 *   The wrongly convicted person perceives pure extraction (Snare) — the system takes their freedom with high procedural friction and low correction rate. The innocence verification system perceives extraction with moral pressure (Snare with constrained exit) — they have the option to stop pursuing cases but face ethical suppression. Prosecutorial agencies perceive coordination (Rope) — conviction finality enables confidence and closure; they benefit from the constraint. Appellate courts perceive mixed coordination and extraction (Tangled Rope) — they benefit from docket management (finality) but have genuine correctness interest; their suppression is constrained by legitimacy concerns. The legal system's formalism perceives its own degradation (Piton) — procedures persist through institutional inertia despite showing limited reversal function. The civilizational observer perceives natural law (Mountain) — finality appears immutable — but this is a false summit that naturalizes what is actually a contingent institutional arrangement balancing speed against accuracy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position: wrongly convicted persons are victims with trapped exit (d ≈ 0.95, f(d) ≈ 1.42) — maximum experienced extraction. Innocent verification systems are also victims but with constrained exit (d ≈ 0.75, f(d) ≈ 1.10) — high but not maximum extraction, with some residual agency. Prosecutorial agencies are beneficiaries with arbitrage exit (d ≈ 0.10, f(d) ≈ -0.02) — they experience the constraint as coordination or even subsidy. The scope modifier (national scale, σ=1.0) does not amplify or dampen chi for this constraint — the verification difficulty and institutional complexity are at national level. The directionality pipeline produces asymmetric experienced extractiveness: maximum for trapped victims, near-zero or negative for beneficiaries with arbitrage options, moderate for constrained secondary actors.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE DIAGNOSIS: The constraint clearly exhibits snare properties — it extracts from victims with high suppression and minimal coordination benefit. The snare identification is not threatened by the tangled_rope or rope perspectives from other actors, because those represent the beneficiaries' and intermediate actors' genuine experience, not mislabeling of the constraint's true nature. The mandatrophy is resolved by noting that the snare exists within a larger institutional ecology (appellate courts, legal formalism) that contains tangled_rope and piton elements. The wrongly convicted person's snare is real. The prosecutor's rope experience is also real. Both are accurate readings from their respective positions. The theater ratio increase (0.52 to 0.68) supports the snare diagnosis — as procedures have become more elaborate without increasing reversals, theater has risen, concealing what is functionally a pure-extraction mechanism under appearance of thorough review. The false summit at the civilizational perspective reveals that naturalization of finality is the primary mechanism by which the snare persists: treating finality as a law of nature prevents questioning the institutional arrangements that enforce it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_standard_function,
    'Do strict evidentiary standards (newly discovered evidence, harmless error doctrine, Brady materiality) function as legitimate gatekeeping mechanisms or as systemic suppression of reversal pathways?',
    'Empirical analysis of reversal rates for jurisdictions with strict vs permissive standards; longitudinal tracking of DNA exonerations and their relationship to evidentiary bar height; comparison of error detection rates across appeals frameworks',
    'If legitimate gatekeeping: suppression metric should be lower (0.50-0.60). If systemic suppression: suppression metric justified at current level (0.72+). Determines whether extractiveness is 0.58 (moderate, mixed coordination) or should be 0.68+ (high, pure extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_standard_function, empirical, 'Whether procedural standards function as legitimate gatekeeping or systemic reversal suppression').

omega_variable(
    prosecutorial_incentive_structure,
    'Are prosecutorial agencies actively suppressing reversal mechanisms (organized opposition to innocence projects, resource hoarding, Brady violations) or passively benefiting from structural friction?',
    'Documentary analysis of prosecutorial responses to exoneration cases; comparison of Brady violation rates and patterns across jurisdictions; tracking of prosecutorial resource allocation to oppose reversals',
    'If active suppression: snare classification is solid (high intentional extraction). If passive benefit from friction: constraint may be better classified as rope with extractive byproduct; suppression metric could be 0.55-0.65 instead of 0.72.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prosecutorial_incentive_structure, empirical, 'Whether prosecutorial agencies actively suppress reversals or passively benefit from structural friction').

omega_variable(
    resource_constraint_materiality,
    'Would innocence verification systems achieve significantly different reversal rates if given equivalent resources to prosecutorial agencies, or does institutional resistance prevent resource solutions?',
    'Comparative analysis of reversal rates in resource-rich vs resource-poor innocence organizations; post-funding analysis of organizations receiving major grant infusions; structural interviews with innocence practitioners on resource-to-reversal elasticity',
    'If resource-elastic: constraint could transition to rope/tangled_rope with targeted funding (policy-solvable). If resource-inelastic: extraction is structural, not contingent on funding, supporting snare classification and justifying high suppression metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_constraint_materiality, empirical, 'Whether reversal barriers are resource constraints or structural institutional resistance').

omega_variable(
    collective_action_possibility,
    'Can wrongly convicted persons and innocence advocates organize into a coalition that shifts the institutional balance toward reversal, or is trapped status immutable by structural barriers?',
    'Historical analysis of innocence movements in states with coordinator power; case studies of jurisdictions where organized pressure shifted reversal standards; identification of critical mass thresholds where political coalition capacity emerges',
    'If coalition-capable: powerless agent classification could upgrade to organized at longer time horizons (generational); classification might shift from snare toward tangled_rope. If coalition-blocked: snare classification persists; trapped status is confirmed as immutable structural condition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_action_possibility, conceptual, 'Whether trapped victims can achieve coalition power to shift institutional balance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(criminal_conviction_reversal_friction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccrf_tr_t0, criminal_conviction_reversal_friction, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ccrf_tr_t10, criminal_conviction_reversal_friction, theater_ratio, 10, 0.6).
narrative_ontology:measurement(ccrf_tr_t20, criminal_conviction_reversal_friction, theater_ratio, 20, 0.68).
narrative_ontology:measurement(ccrf_tr_t5, criminal_conviction_reversal_friction, theater_ratio, 5, 0.56).

% Extraction over time
narrative_ontology:measurement(ccrf_be_t0, criminal_conviction_reversal_friction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ccrf_be_t10, criminal_conviction_reversal_friction, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ccrf_be_t20, criminal_conviction_reversal_friction, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ccrf_be_t5, criminal_conviction_reversal_friction, base_extractiveness, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(criminal_conviction_reversal_friction, enforcement_mechanism).
narrative_ontology:affects_constraint(criminal_conviction_reversal_friction, prosecutorial_resource_asymmetry).
narrative_ontology:affects_constraint(criminal_conviction_reversal_friction, appellate_docket_pressure).
narrative_ontology:affects_constraint(criminal_conviction_reversal_friction, brady_violation_materiality_standard).

% DUAL FORMULATION NOTE:
% Criminal conviction reversal friction is downstream of specific procedural mechanisms (Brady rules, harmless error doctrine, newly discovered evidence standards) and upstream of systemic wrongful conviction persistence. The constraint represents the aggregate effect of multiple institutional arrangements that individually appear neutral but collectively suppress reversal pathways. Separate constraint stories may be written for specific procedural mechanisms (e.g., harmless_error_doctrine, brady_materiality_standard) with their own ε values; this story captures the aggregate structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(criminal_conviction_reversal_friction, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
