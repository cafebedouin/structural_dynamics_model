% ============================================================================
% CONSTRAINT STORY: social_credit_architecture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_credit_architecture, []).

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
 *   constraint_id: social_credit_architecture
 *   human_readable: Social Credit Architecture
 *   domain: social/political/technological
 *
 * SUMMARY:
 *   China's social credit system operationalizes 'trustworthiness' by
 *   decoding behavioral data—payment history, tax compliance, traffic
 *   violations, online speech—into a unified score that triggers cascading
 *   consequences: travel bans, job loss, school exclusion, lending
 *   restrictions, and public shaming. The system represents a nation-scale
 *   attempt to externalize trust assessment from embedded relationships and
 *   reputation to algorithmic scoring. It is a canonical snare constraint:
 *   high suppression (0.75) through opaque algorithms and ambiguous
 *   behavioral standards; high extractiveness (0.68) through political
 *   weaponization and intergenerational harm; and strategic theater (0.65) in
 *   claims of fairness and appeals mechanisms that are performative rather
 *   than functionally effective. The constraint exhibits all major DR types
 *   from different positions: ordinary citizens see pure extraction (snare);
 *   marginalized populations see systematic overexecution (snare with higher
 *   penalty rates); state administrators see coordination (rope); enterprises
 *   see mixed coordination and coercion (tangled rope); international
 *   observers see institutional degradation (piton); Western analysts risk
 *   naturalizing it as inevitable (false summit).
 *
 * KEY AGENTS:
 *   - Individual Subjects: Primary victims (powerless/trapped) — citizens cannot exit system, face opaque penalties, have no meaningful appeal
 *   - Marginalized Populations: Primary victims (powerless/trapped) — rural residents, ethnic minorities, low-income populations face systematic over-surveillance and higher thresholds; extraction is intergenerational
 *   - Political Dissidents: Secondary victims (moderate/constrained) — activists targeted with low scores as political punishment; exit requires emigration
 *   - State Administrative Apparatus: Primary beneficiary (institutional/arbitrage) — central authority, law enforcement, and regulators gain unified behavioral data and enforcement efficiency
 *   - Private Enterprise & Fintech: Secondary beneficiary/victim (organized/constrained) — benefit from credit risk automation but coerced into enforcement participation by political pressure
 *   - International Human Rights Observers: External analytical position (institutional/arbitrage) — see system as degraded institution; theater masks extraction
 *   - Western Analysts: Risk position (analytical/analytical) — risk naturalizing system as inevitable outcome of scale rather than specific policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_credit_architecture, 0.68).
domain_priors:suppression_score(social_credit_architecture, 0.75).
domain_priors:theater_ratio(social_credit_architecture, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_credit_architecture, extractiveness, 0.68).
narrative_ontology:constraint_metric(social_credit_architecture, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(social_credit_architecture, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_credit_architecture, snare).
narrative_ontology:human_readable(social_credit_architecture, "Social Credit Architecture").
narrative_ontology:topic_domain(social_credit_architecture, "social/political/technological").

domain_priors:requires_active_enforcement(social_credit_architecture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_credit_architecture, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(social_credit_architecture, enforcement_agencies).
narrative_ontology:constraint_victim(social_credit_architecture, individual_subjects).
narrative_ontology:constraint_victim(social_credit_architecture, marginalized_populations).
narrative_ontology:constraint_victim(social_credit_architecture, political_dissidents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SCORED SUBJECT (SNARE) — Individual citizens cannot exit the system, have no meaningful appeal mechanism, and face cascading penalties (travel bans, job loss, school exclusion, lending restrictions) for algorithmic infractions. The scoring mechanism is opaque; behavioral standards are ambiguous and politically fluid. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.95.
constraint_indexing:constraint_classification(social_credit_architecture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED POPULATIONS (SNARE) — Rural residents, ethnic minorities, and low-income populations face systematic over-surveillance and higher penalty thresholds. The system compounds existing inequalities. No exit from nation-state territory. Extraction is maximal and intergenerational. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.97.
constraint_indexing:constraint_classification(social_credit_architecture, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: POLITICAL DISSIDENTS (SNARE) — Activists and opposition figures face targeted low scores as political punishment, with no recourse. Exit requires emigration (accessible to few). The system weaponizes behavioral data for political control. d≈0.90, f(d)≈1.35, σ=1.0 → χ≈0.92.
constraint_indexing:constraint_classification(social_credit_architecture, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE ADMINISTRATIVE APPARATUS (ROPE) — Central authority, law enforcement, and regulatory agencies benefit from unified behavioral data, streamlined enforcement, and reduced administrative friction. System solves real coordination problem: how to scale trust assessment across 1.4B+ people. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.07. Net beneficiary; experiences system as coordination.
constraint_indexing:constraint_classification(social_credit_architecture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PRIVATE ENTERPRISE & FINTECH (TANGLED ROPE) — Commercial banks, e-commerce platforms, and fintech firms benefit from credit risk automation and reduced loan approval friction (coordination). But they are also constrained by political pressure to participate in enforcement (victim of regulatory extraction). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.51. Mixed experience: coordination benefit offset by extraction pressure.
constraint_indexing:constraint_classification(social_credit_architecture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL HUMAN RIGHTS OBSERVERS (PITON) — From outside the system, observers see a degraded institution: social credit claims to be objective behavioral assessment (theater=0.65) but functions primarily as political control (reality). The performative element (fairness, appeals, transparency) exists but is theatrically maintained rather than functionally effective. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.97. High theater masks extraction.
constraint_indexing:constraint_classification(social_credit_architecture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW — Risk that Western analysts naturalize social credit as an inevitable outcome of data collection at scale, treating it as a 'law of digital governance.' The structural data contradicts this: high suppression (0.75), low appeal mechanisms, and political weaponization are not inherent to trust assessment but to specific policy choices. This mountain view is a false summit — the engine will detect it as such.
constraint_indexing:constraint_classification(social_credit_architecture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_credit_architecture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_credit_architecture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_credit_architecture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_credit_architecture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_credit_architecture, TR),
    TR >= 0.70.

:- end_tests(social_credit_architecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The system extracts behavioral compliance through threat of penalties. Citizens internalize surveillance (panopticon effect), altering behavior to maintain scores. The extraction is not absolute (0.95) because scoring is not literally incompatible with survival—penalties are severe but not immediate death or enslavement. But cascading penalties (travel ban → loss of job → loss of housing) create severe material extraction. The trajectory shows growth from 0.35 at system inception (initial enthusiasm, limited rollout) to 0.68 (mature enforcement, weaponization normalized). Suppression (0.75): High. Citizens have no meaningful exit from the system (territorial exit requires emigration, which is restricted). Appeal mechanisms exist but are theatrical—reversal rates are negligible (<5% in available data). Behavioral standards are ambiguous and politically fluid, making avoidance impossible. Theater ratio (0.65): Moderate-high. The system maintains performative fairness: published appeals procedures, claims of objectivity, periodic 'amnesty' campaigns. But the substance is pure political control—scoring criteria shift with regime priorities, marginalized populations face higher thresholds, dissidents are targeted. Theater has increased over interval as legitimacy challenges mounted, requiring performative defense. Mandatrophy resolved at ε=0.68 > 0.70 via analysis: the system is unambiguously snare (not coordination hidden as extraction), confirmed by victim status, lack of beneficiary burden-sharing, and political weaponization.
 *
 * PERSPECTIVAL GAP:
 *   Massive perspectival divergence. State administrative apparatus (institutional/arbitrage) sees a Rope—a genuine coordination solution to the problem of assessing trustworthiness at national scale. They experience low effective extraction (negative χ) because they are the beneficiary and the system solves a real problem they face. Individual subjects (powerless/trapped) see a Snare—pure extraction with no coordination benefit, no exit, and opaque punishment criteria. Political dissidents (moderate/constrained) see an even sharper snare—the system is weaponized against them specifically. Marginalized populations see snare with intergenerational harm (generational time horizon, trapped exit, national scope). Enterprises see tangled rope—they benefit from credit automation (rope component) but are coerced into enforcement (extraction component). International observers see Piton—institutional degradation where theater (fairness claims, appeals) masks the extractive function. Western analysts risk seeing Mountain—naturalizing the system as an inevitable outcome of data collection and AI at scale. This perspectival gap is not a measurement error; it reflects the genuine structural reality: the state apparatus and the subject experience fundamentally different constraints, and no single type captures both. The mandatrophy is resolved by recognizing that snare is the accurate base classification (victim identification, extraction without reciprocal burden-sharing), and the state's rope experience is an artifact of their beneficiary status.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual subjects: Victims + trapped → d≈0.92, f(d)≈1.40. Near-maximal extraction. They bear all costs (penalties, surveillance, behavior modification) with no exit. Marginalized populations: Victims + trapped + intergenerational → d≈0.95, f(d)≈1.42. Maximal extraction. Over-surveilled, higher penalty thresholds, no exit, effects persist across generations. Political dissidents: Victims + constrained (emigration possible but expensive) → d≈0.90, f(d)≈1.35. Near-maximal extraction. System is weaponized against them; exit requires sacrifice. State administrative apparatus: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.10. Negative effective extraction (net beneficiary). They can choose enforcement levels and appeal standards; they designed the system. Private enterprise: Mixed beneficiary/victim (benefit from credit automation) + constrained (regulatory pressure to participate) → d≈0.55, f(d)≈0.75. Moderate extraction. They experience both coordination and coercion. International observers: Piton perspective, institutional actor viewing from outside. Theater maintenance keeps apparent d lower than reality. Analytical observer: Risk of false summit (mountain perspective) naturalizes the system as inevitable, but structural data contradicts this.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED AT ε=0.68: The constraint is unambiguously Snare, not misclassified as Rope or Scaffold. Resolution evidence: (1) Victim identification is clear and structural—individuals are penalized for behavioral data; state is beneficiary. (2) No reciprocal burden-sharing—beneficiary (state) bears none of the extraction costs; victims bear all. (3) Suppression is structural (no meaningful exit, opaque standards, political fluidity). (4) Beneficiary benefits from system's existence independent of whether victims cooperate—state extracts value from control regardless of subject compliance. (5) Theater ratio (0.65) confirms institutional degradation—fairness claims mask extraction rather than constituting genuine coordination. Mandatrophy risk: Western analysts naturalizing the system as an inevitable outcome of 'AI at scale' or 'data collection efficiency,' treating it as a law of governance rather than a contingent political choice. The false summit (mountain perspective) claims that any national-scale trust system must be extractive to some degree. But comparative analysis shows that trust systems with transparency, appeals, and political constraints (e.g., credit scoring in regulated markets) operate at much lower suppression and extractiveness. The Chinese system's suppression (0.75) is not inherent to scoring but to specific design choices: opacity, political fluidity, cascade effects, and coercive enterprise participation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_opacity_vs_fairness,
    'Is the opacity of scoring algorithms a necessary feature of system security or a deliberate choice to prevent contestation?',
    'Comparison of systems with published vs proprietary algorithms; analysis of appeals rates and reversal outcomes in transparent vs opaque jurisdictions',
    'If necessary: extraction is moderately justified (security externality). If deliberate: extraction mechanism is pure political control (snare confirmed at higher confidence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_opacity_vs_fairness, empirical, 'Whether algorithmic opacity is structural necessity or political choice').

omega_variable(
    behavioral_standard_drift,
    'Do scoring criteria remain stable across political cycles or shift to target opposition movements?',
    'Longitudinal analysis of penalty categories and thresholds; correlation between political regime changes and scoring standard revisions; comparison of penalty rates for identical behaviors across time periods',
    'If stable: system is extractive but not politically weaponized (pure snare for ordinary citizens, tangled rope for dissidents). If drift is systematic: system is political control apparatus (snare for all; mountain view is false summit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_standard_drift, empirical, 'Whether scoring criteria drift to target political opposition').

omega_variable(
    marginalization_feedback_loop,
    'Does the system compound existing inequalities through biased training data and higher surveillance of marginalized populations?',
    'Statistical analysis of penalty rates by demographic; comparison of algorithmic audit results for identical behavior profiles across population groups; longitudinal income and mobility outcomes for high-score vs low-score populations',
    'If system is agnostic: extraction rates are uniform (snare for all citizens equally). If biased: marginalized populations face higher extraction (snare rate asymmetrically high; intergenerational victims perspective confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marginalization_feedback_loop, empirical, 'Whether system compounds existing inequalities through biased surveillance').

omega_variable(
    enterprise_coercion_mechanism,
    'Are private enterprises genuinely voluntary participants in enforcement, or are they coerced through regulatory pressure and data access conditions?',
    'Documentation of regulatory directives; comparison of enterprise behavior with/without government incentives; interviews with fintech CEOs on freedom to opt out',
    'If voluntary: enterprises are beneficiaries (rope perspective accurate). If coerced: enterprises are also victims (tangled rope perspective accurate; extraction affects private sector).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enterprise_coercion_mechanism, empirical, 'Whether private enterprises are coerced into enforcement participation').

omega_variable(
    exit_option_for_high_scorers,
    'Can high-score individuals actually exit constraining situations (e.g., dissidents emigrate, reformers change careers), or does the system''s global reach eliminate practical exit?',
    'Tracking emigration rates of dissidents and low-score populations; analysis of whether high scores provide meaningful new opportunities; comparison with control jurisdictions',
    'If exit is real: some victims are constrained rather than trapped (moderate classification possible for some cohorts). If exit is illusory: all are trapped (powerless/snare universal for subjects).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_for_high_scorers, empirical, 'Whether high-score individuals can exit constraining situations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_credit_architecture, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sca_tr_t0, social_credit_architecture, theater_ratio, 0, 0.4).
narrative_ontology:measurement(sca_tr_t7, social_credit_architecture, theater_ratio, 7, 0.53).
narrative_ontology:measurement(sca_tr_t15, social_credit_architecture, theater_ratio, 15, 0.65).
narrative_ontology:measurement(sca_tr_t3, social_credit_architecture, theater_ratio, 3, 0.47).

% Extraction over time
narrative_ontology:measurement(sca_be_t0, social_credit_architecture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sca_be_t7, social_credit_architecture, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(sca_be_t15, social_credit_architecture, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(sca_be_t3, social_credit_architecture, base_extractiveness, 3, 0.43).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_credit_architecture, enforcement_mechanism).
narrative_ontology:affects_constraint(social_credit_architecture, digital_surveillance_panopticon).
narrative_ontology:affects_constraint(social_credit_architecture, regulatory_capture_in_fintech).
narrative_ontology:affects_constraint(social_credit_architecture, speech_suppression_via_economic_penalty).

% DUAL FORMULATION NOTE:
% Social credit architecture is downstream of several structural constraints. Digital surveillance (ε≈0.42) provides data input; regulatory capture of fintech (ε≈0.55) enables enterprise enforcement; speech suppression mechanisms (ε≈0.60) use scoring as implementation. Social credit itself (ε=0.68) represents the unified extraction layer across all three inputs. Each upstream constraint has different empirical status; the downstream constraint (social credit) integrates their effects into a single snare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_credit_architecture, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
