% ============================================================================
% CONSTRAINT STORY: us_legal_standard_reasonable_doubt
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_legal_standard_reasonable_doubt, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_legal_standard_reasonable_doubt
 *   human_readable: The US Legal Standard of "Guilt Beyond a Reasonable Doubt" in Political Prosecutions
 *   domain: political/legal
 *
 * SUMMARY:
 *   This constraint models the legal principle of "guilty beyond a reasonable
 *   doubt" as applied to a high-profile political figure (Donald Trump)
 *   following the events of January 6th, 2021. The principle aims to protect
 *   the innocent, but politically charged contexts can erode this safeguard
 *   and enable selective or malicious prosecutions. It is critical to examine
 *   the multiple perspectives influenced by this legal standard, including
 *   the accused, the jury, the legal system itself, the prosecuting
 *   attorneys, and outside observers.
 *
 * KEY AGENTS:
 *   - Accused Political Figures: Primary target (powerless/trapped) – bears the brunt of the prosecution.
 *   - The US Legal System: Primary beneficiary (institutional/arbitrage) – benefits from upholding the standard.
 *   - The Jury: Secondary actor (moderate/constrained) – bound by evidence and instructions, but capable of bias.
 *   - The Prosecuting Attorneys: Secondary actor (powerful/mobile) – motivated by both justice and political ambition.
 *   - Public Trust in Justice: Victim (powerless/trapped) - suffers when the standard is perceived to be unfairly applied.
 *   - Political Stability: Beneficiary (institutional/arbitrage) - benefits from fair application of legal standards.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_legal_standard_reasonable_doubt, 0.55).
domain_priors:suppression_score(us_legal_standard_reasonable_doubt, 0.45).
domain_priors:theater_ratio(us_legal_standard_reasonable_doubt, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_legal_standard_reasonable_doubt, extractiveness, 0.55).
narrative_ontology:constraint_metric(us_legal_standard_reasonable_doubt, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(us_legal_standard_reasonable_doubt, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_legal_standard_reasonable_doubt, tangled_rope).
narrative_ontology:human_readable(us_legal_standard_reasonable_doubt, "The US Legal Standard of \"Guilt Beyond a Reasonable Doubt\" in Political Prosecutions").
narrative_ontology:topic_domain(us_legal_standard_reasonable_doubt, "political/legal").

domain_priors:requires_active_enforcement(us_legal_standard_reasonable_doubt).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_legal_standard_reasonable_doubt, us_legal_system).
narrative_ontology:constraint_beneficiary(us_legal_standard_reasonable_doubt, political_stability).
narrative_ontology:constraint_victim(us_legal_standard_reasonable_doubt, accused_political_figures).
narrative_ontology:constraint_victim(us_legal_standard_reasonable_doubt, public_trust_in_justice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The accused political figure, lacking the resources to effectively combat the prosecution, may be trapped by the legal system, regardless of actual guilt or innocence. The standard of 'beyond a reasonable doubt' offers little comfort when facing politically motivated charges. Faces potentially total extraction of liberty and reputation.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The jury is constrained by the evidence presented, legal instructions, and social pressures. They benefit from a (presumably) fair process, but also bear the burden of potentially misjudging the evidence, resulting in unjust outcomes. The requirement to reach a verdict beyond a reasonable doubt adds pressure and complexity. Benefit from the presumption of fairness in legal system, but risk extraction of their time and mental energy.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The legal system benefits from upholding the standard of 'beyond a reasonable doubt' to maintain legitimacy and public trust. It coordinates judicial processes. High-profile prosecutions are arbitrage opportunities to demonstrate fairness.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The prosecuting attorneys benefit from securing convictions, boosting their careers and furthering political agendas. However, they are also constrained by the need to adhere to legal ethics and standards, preventing them from pursuing purely malicious or unfounded prosecutions. Extraction arises through the political ramifications of the case that impact the accused; the system's coordination aims for justice (rope).
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% From an analytical, global perspective, the 'beyond a reasonable doubt' standard is a mixed bag. It aims to protect the innocent but can be manipulated in politically charged cases, leading to selective enforcement and erosion of public trust. The standard serves as a legal construct that is at once a tool to safeguard against wrongful conviction and an exploitable political weapon.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_legal_standard_reasonable_doubt_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_legal_standard_reasonable_doubt, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_legal_standard_reasonable_doubt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-High. The prosecution, even if unsuccessful, extracts significant resources and reputation from the accused. Suppression (0.45): Moderate. The presumption of innocence is weakened by intense media coverage and public opinion. Theater ratio (0.30): Moderate. While the trial adheres to formal legal procedures, the underlying political motivations can introduce theatrical elements, such as grandstanding or appealing to public sentiment.
 *
 * PERSPECTIVAL GAP:
 *   The accused political figure perceives the system as a snare, given the potential for biased application of the law and overwhelming resources of the state. The legal system, meanwhile, views itself as a rope, fulfilling its duty to impartially adjudicate accusations. The jury experiences mixed extraction and coordination as they are bound by legal constraints but are also influenced by personal biases and the pressure to reach a verdict. The prosecuting attorneys see a tangled rope, seeking justice but also potentially motivated by political gain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the structural position of each agent. The accused political figure has limited exit options and is the target of extraction. The US legal system benefits from the coordination function of justice. Juries are constrained by evidence but benefit from participating in the legal process. Prosecutors seek justice while facing extraction in the form of potential public backlash and career consequences if the prosecution is deemed unfair or politically motivated. Observers provide an analytical perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_political_motivation,
    'To what extent is the prosecution motivated by legitimate legal concerns versus political considerations?',
    'Independent investigations into the prosecutor''s decision-making process, analysis of public statements and political affiliations.',
    'High political motivation: system becomes snare for accused. Low political motivation: system functions as intended, rope for society.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_political_motivation, empirical, 'The degree to which the prosecution is influenced by political factors.').

omega_variable(
    evidentiary_threshold_interpretation,
    'How is the ''reasonable doubt'' standard interpreted and applied in the specific context of political prosecutions?',
    'Legal scholarship analyzing relevant case law, surveys of legal professionals, analysis of jury instructions.',
    'Strict interpretation: greater protection for the accused. Lax interpretation: increased risk of wrongful conviction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evidentiary_threshold_interpretation, conceptual, 'Interpretation of ''reasonable doubt'' in political cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_legal_standard_reasonable_doubt, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_l_tr_t0, us_legal_standard_reasonable_doubt, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_l_tr_t5, us_legal_standard_reasonable_doubt, theater_ratio, 5, 0.3).
narrative_ontology:measurement(us_l_tr_t10, us_legal_standard_reasonable_doubt, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(us_l_be_t0, us_legal_standard_reasonable_doubt, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(us_l_be_t5, us_legal_standard_reasonable_doubt, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(us_l_be_t10, us_legal_standard_reasonable_doubt, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_legal_standard_reasonable_doubt, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
