% ============================================================================
% CONSTRAINT STORY: sotu_2004_bush_patriot_act_renewal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2004_bush_patriot_act_renewal, []).

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
 *   constraint_id: sotu_2004_bush_patriot_act_renewal
 *   human_readable: Patriot Act Renewal: Counterterrorism Authority Centralization (2004)
 *   domain: governance/executive_power/civil_liberties
 *
 * SUMMARY:
 *   The USA PATRIOT Act (2001) and its 2004 reauthorization created a
 *   structural constraint that centralizes federal counterterrorism authority
 *   while systematically reducing procedural protections for those targeted
 *   by surveillance, detention, and asset seizure. The constraint exhibits
 *   the core DR signature of tangled_rope: a genuine coordination function
 *   (integrating fragmented intelligence across FBI, CIA, NSA) embedded
 *   within a structure that systematically extracts from civil liberties and
 *   procedural protections. The 2004 renewal moment crystallized this:
 *   Congress debated reauthorization against the backdrop of post-9/11
 *   security narratives while civil liberties organizations documented
 *   expansion of surveillance and detention practices. The constraint's
 *   theater_ratio has risen from 0.52 to 0.71 over the interval, indicating
 *   increasing performative content: judicial review persists but state
 *   secrets doctrine prevents meaningful scrutiny; congressional oversight
 *   hearings occur but intelligence classification prevents substantive
 *   accountability. Base extractiveness has risen from 0.42 to 0.61,
 *   reflecting both the institutional expansion of law enforcement
 *   authorities and documented civil liberties harms accumulating over time.
 *   The structure is defended through competing naturalizations: security
 *   professionals naturalize it as necessary institutional response to
 *   terrorism (mountain view); civil libertarians naturalize it as
 *   unjustified executive power grab (snare view). The analytical framework
 *   reveals both positions contain truth and incompleteness. The Act does
 *   solve a real coordination problem (fragmented terrorism intelligence); it
 *   simultaneously extracts from constitutional protections in a manner
 *   difficult to reverse even after threat perception normalizes.
 *
 * KEY AGENTS:
 *   - Federal law enforcement agencies (FBI, NSA, CIA): Institutional beneficiaries (arbitrage exit) — gain expanded investigative authority, information-sharing capacity, asset seizure power; shape policy through administrative channels
 *   - Executive branch counterterrorism authority: Institutional beneficiary (arbitrage exit) — concentrates decision-making power in presidential office, bypasses congressional constraints, justifies emergency authority through security rhetoric
 *   - Civil liberties advocates and ACLU: Organized opposition (constrained exit) — possess legal and narrative resources but face state secrets doctrine barriers and judicial deference to executive; constrained by security doctrine and resource asymmetry
 *   - Suspected individuals and communities under surveillance: Powerless victims (trapped exit) — subjected to detention, asset seizure, surveillance without meaningful due process; no organizational capacity to exit or challenge
 *   - Fourth Amendment protections and due process norms: Abstract victim (powerless/trapped) — suspended, eroded, difficult to restore through litigation or legislative action; attacked from position of structural authority asymmetry
 *   - Congress: Powerful but constrained (constrained exit) — benefits from delegating security decisions to executive (political cover); bears partial responsibility for civil liberties damage; constrained by post-9/11 political pressure and intelligence asymmetry
 *   - Judiciary: Institutional actor with degraded function (piton perspective) — maintains review ritual through state secrets doctrine and national security deference; unable to access evidence or meaningfully question executive threat assessment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2004_bush_patriot_act_renewal, 0.58).
domain_priors:suppression_score(sotu_2004_bush_patriot_act_renewal, 0.72).
domain_priors:theater_ratio(sotu_2004_bush_patriot_act_renewal, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2004_bush_patriot_act_renewal, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_2004_bush_patriot_act_renewal, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sotu_2004_bush_patriot_act_renewal, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2004_bush_patriot_act_renewal, tangled_rope).
narrative_ontology:human_readable(sotu_2004_bush_patriot_act_renewal, "Patriot Act Renewal: Counterterrorism Authority Centralization (2004)").
narrative_ontology:topic_domain(sotu_2004_bush_patriot_act_renewal, "governance/executive_power/civil_liberties").

domain_priors:requires_active_enforcement(sotu_2004_bush_patriot_act_renewal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2004_bush_patriot_act_renewal, federal_law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(sotu_2004_bush_patriot_act_renewal, executive_branch_counterterrorism_authority).
narrative_ontology:constraint_victim(sotu_2004_bush_patriot_act_renewal, constitutional_fourth_amendment_protections).
narrative_ontology:constraint_victim(sotu_2004_bush_patriot_act_renewal, civil_liberties_advocates).
narrative_ontology:constraint_victim(sotu_2004_bush_patriot_act_renewal, privacy_interests_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUSPECTED INDIVIDUAL (SNARE) — Subject to asset seizure, information sharing across agencies, and surveillance without traditional Fourth Amendment protections. No meaningful exit: targeted individuals cannot opt out of federal investigative authority. Biological timescale reveals immutable extraction — constitutional protections are suspended, recovery mechanisms are minimal.
constraint_indexing:constraint_classification(sotu_2004_bush_patriot_act_renewal, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL LIBERTIES COMMONS (SNARE) — The procedural protections (warrants, due process, Fourth Amendment limits) constitute a public good that cannot be reclaimed once suspended. The commons is trapped by structural authority asymmetry: executive enforcement power > individual legal redress capacity. No coalition mechanism exists to reclaim suspended protections at biographical scale.
constraint_indexing:constraint_classification(sotu_2004_bush_patriot_act_renewal, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESS (TANGLED ROPE) — Powerful actor constrained by post-9/11 political pressure and security rhetoric. Congress benefits from delegating counterterrorism decisions to executive (reduced political exposure) while bearing partial responsibility for civil liberties damage. The constraint coordinates information-sharing between branches AND concentrates executive power. Constrained by electoral politics and intelligence asymmetry (executive knows threats better).
constraint_indexing:constraint_classification(sotu_2004_bush_patriot_act_renewal, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL LAW ENFORCEMENT (ROPE) — Primary beneficiary perceives the constraint as enabling coordination (information-sharing between FBI, CIA, NSA) that solves a genuine counterterrorism problem: fragmented intelligence prevented pre-9/11 threat detection. Immediate timescale captures operational benefits. Exit is frictionless (agencies shape policy through administrative channels). Extraction flows toward this agent.
constraint_indexing:constraint_classification(sotu_2004_bush_patriot_act_renewal, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL RIGHTS ORGANIZATIONS (TANGLED ROPE) — Organized agents (ACLU, etc.) constrained by resource asymmetry and legal doctrine (state secrets privilege, judicial deference to executive in security matters). Benefit minimally from transparency gains achieved through litigation; bear disproportionate costs of coordinating legal response. Generational timescale shows institutional capture (courts defer, precedent locks in executive power).
constraint_indexing:constraint_classification(sotu_2004_bush_patriot_act_renewal, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: JUDICIAL REVIEW (PITON) — Review mechanism persists as institutional ritual with diminished function. State secrets doctrine, deference to executive on national security, and classified evidence procedures render courts unable to meaningfully review detention, surveillance, or asset seizure decisions. Theater ratio high: judges go through review process but cannot access evidence or question executive threat assessment. Maintenance through institutional inertia rather than verification.
constraint_indexing:constraint_classification(sotu_2004_bush_patriot_act_renewal, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, some executive concentration of counterterrorism authority appears necessary (speed, secrecy, coordination requirements outpace due-process mechanisms). This perspective naturalizes the structure as inherent to security governance. However, structural data contradicts mountain classification — beneficiaries exist, suppression is high, extraction is documented. Engine flags this as false summit: security necessity naturalization covers contingent institutional choices.
constraint_indexing:constraint_classification(sotu_2004_bush_patriot_act_renewal, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2004_bush_patriot_act_renewal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2004_bush_patriot_act_renewal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2004_bush_patriot_act_renewal, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2004_bush_patriot_act_renewal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2004_bush_patriot_act_renewal, TR),
    TR >= 0.70.

:- end_tests(sotu_2004_bush_patriot_act_renewal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately-high. The Patriot Act creates measurable extraction mechanisms: asset seizure without conviction, surveillance without warrant, detention without traditional due process. However, extraction is not maximal (0.70+) because the underlying coordination problem (fragmented intelligence) is real and the procedural modifications serve operational efficiency that some argue is necessary. The rise from 0.42 (2001 initial) to 0.58 (2004 renewal) to 0.61 (2007 institutionalization) reflects that institutionalization expands both the coordination function AND the extraction mechanism as practices calcify. Suppression (0.72): High. Multiple barriers prevent resistance: (1) post-9/11 security rhetoric makes opposition appear unpatriotic, (2) surveillance by definition prevents public awareness of targeting, (3) state secrets doctrine removes judicial redress, (4) civil liberties organizations are resource-constrained relative to law enforcement budgets. These mechanisms combine to achieve high suppression despite not being formally coercive. Theater ratio (0.68): Moderately-high. Congressional oversight hearings on Patriot Act reauthorization maintained democratic theater: debates occurred, votes were cast, competing narratives were documented. However, the actual substance of oversight was constrained by classification: Congress could not access evidence of how authorities were used, could not interrogate specific operational decisions, could not verify executive threat assessments. Judicial review likewise persists as theater: courts conduct habeas corpus review but state secrets doctrine prevents substantive assessment of detention justification. The theater maintains legitimacy appearance while preventing meaningful constraint on executive action.
 *
 * PERSPECTIVAL GAP:
 *   Perspectival gap is maximum and structurally unresolvable within the constraint system. Law enforcement sees rope (coordination); civil liberties see snare (extraction). These are not alternative measurements of the same phenomenon — they are different agents perceiving different flows through the same structure. From law enforcement perspective, the coordination benefit is visible (fragmented intelligence is integrated, threat detection improves). From civil liberties perspective, the extraction cost is visible (constitutional protections suspended, surveillance expands, due process erodes). Both perceptions are accurate within their respective structural positions. The constraint system does NOT resolve this gap — it documents it. The analytical observer's task is to recognize that both perspectives are grounded in real structural asymmetries and that the political decision to renew the Act represents a choice to weight law enforcement coordination benefits more heavily than civil liberties extraction costs. This is a value choice, not a factual disagreement about whether the constraint is rope or snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this constraint flows from power asymmetry and exit differentiation. Federal law enforcement enters as institutional power with arbitrage exit: they can influence policy through administrative channels, judicial interpretation, and executive preference. Their d is low (0.15-0.25), yielding negative f(d) → extraction flows toward them, not from them. Civil liberties advocates enter as organized power with constrained exit: they can litigate and legislate but face structural barriers (state secrets, judicial deference, security doctrine). Their d is moderate (0.45-0.55), yielding moderate f(d) → they experience moderate extraction. Suspected individuals enter as powerless with trapped exit: they have no exit capacity, no negotiating position, no appeal to professional standing. Their d is high (0.85-0.95), yielding high f(d) → maximum experienced extraction. Directionality is not arbitrary: it flows from who holds institutional authority (low d), who can exit without cost (low d), who cannot exit (high d). Congress holds power but is constrained by political pressure — d moderate (0.50-0.60). The judiciary holds institutional authority but is constrained by security doctrine and state secrets privilege — d moderate (0.40-0.50). These structural relationships determine who experiences extraction and who benefits from coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: This constraint exhibits the classical mandatrophy between coordination (genuine intelligence integration) and extraction (civil liberties erosion). The classification as tangled_rope reflects both functions coexisting, but the tension is not resolved — it is managed through power asymmetry. Law enforcement benefits from the coordination; civil liberties absorb the extraction. Congress manages the tension through periodic reauthorization with nominal 'oversight' constraints that state secrets doctrine prevents from functioning meaningfully. The judicial system manages the tension through maintaining review theater (habeas corpus, FISA courts) while denying substantive review authority (state secrets privilege). The RESOLVE path would require either: (a) accepting that civil liberties extraction is necessary cost of counterterrorism coordination (snare classification), or (b) demonstrating that the coordination could be achieved through warrant requirement reform without suspending Fourth Amendment scope (rope classification). Neither resolution occurs. Instead, the constraint persists in tangled_rope tension through institutional management of the disagreement. The 2004 renewal represents a choice NOT to resolve the mandatrophy but rather to recommit to the coordination-extraction balance favoring law enforcement. The theater ratio rise (0.52 → 0.71) indicates that political theater (reauthorization debate, oversight hearings) increasingly substitutes for substantive resolution. This is a Piton-ization signal: the constraint's legitimacy increasingly depends on performative democracy rather than functional oversight.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_threat_prevention_efficacy,
    'Did Patriot Act information-sharing and asset seizure powers measurably improve counterterrorism outcomes relative to pre-Act baseline, or did the efficiency gains come from law enforcement behavior change independent of new authorities?',
    'Comparative analysis of prevented attack statistics (controlled for attack difficulty, bomber sophistication); attribution of prevention credit to information-sharing vs. other factors (increased vigilance, improved technical surveillance, international cooperation); controlled historical counterfactual.',
    'If Act powers directly prevented attacks: genuine coordination function exists, tangled_rope classification strengthens. If no measurable incremental efficacy: snare classification dominates, extraction mechanism persists without functional justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(actual_threat_prevention_efficacy, empirical, 'Whether Patriot Act powers measurably improved counterterrorism outcomes').

omega_variable(
    civil_liberties_harm_reversibility,
    'Can constitutional protections suspended under the Patriot Act be restored through legislative repeal, or has the institutional infrastructure (surveillance systems, precedent, inter-agency dependencies) created path dependence that makes restoration structurally difficult even after formal repeal?',
    'Analysis of reauthorization debates (did legislators debate substantive restoration or merely renewal?); examination of whether post-repeal statutory frameworks maintain surveillance architectures; tracking of litigation seeking restoration of Fourth Amendment scope.',
    'If reversible: constraint is temporary institutional choice (scaffold). If path-dependent: constraint locks in extraction even after nominal repeal (snare with institutional inertia masking as policy choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_liberties_harm_reversibility, empirical, 'Whether civil liberties harm is reversible after Patriot Act repeal').

omega_variable(
    state_secrets_doctrine_boundary,
    'Does state secrets privilege prevent meaningful judicial review of Patriot Act executions, or do procedural workarounds (congressional oversight, inspector general reports, declassification) provide adequate check on executive abuse?',
    'Counting of cases in which state secrets doctrine prevented substantive review; analysis of congressional oversight hearing outputs and whether they produced enforceable accountability; comparison of declassified post-action review findings with executive claims made at time of action.',
    'If secrets doctrine is absolute barrier: judicial check fails, snare classification accurate for civil liberties. If procedural checks work: tangled_rope classification holds (extraction exists but constrained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_secrets_doctrine_boundary, empirical, 'Whether state secrets doctrine prevents meaningful Patriot Act judicial review').

omega_variable(
    post_911_threat_perception_contingency,
    'Did the specific post-9/11 threat environment justify emergency powers that would be unjustifiable under normal security levels, or did threat perception remain elevated post-2004 through institutional incentives independent of actual threat level?',
    'Comparative threat level analysis (attack frequency, plot sophistication, incident severity pre-9/11 vs 2004-2007); tracking of DHS and FBI threat assessments over time; analysis of whether threat narratives reflected classified intelligence or public threat data.',
    'If threat was genuinely elevated: scaffold logic applies (temporary response to emergency). If threat perception was artificially maintained: snare logic applies (extraction sustained through manufactured emergency).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_911_threat_perception_contingency, empirical, 'Whether post-9/11 threat elevation justified emergency authority scope').

omega_variable(
    information_sharing_coordination_necessity,
    'Could the legitimate counterterrorism coordination problem (intelligence fragmentation) have been solved through warrant requirement reform alone, without suspending Fourth Amendment scope, or was blanket authority genuinely necessary for operational speed?',
    'Simulation of warrant process timeline vs actual operational timeline for relevant cases; analysis of whether specific prevented attacks required speed that warrant process could not accommodate; comparison with international intelligence coordination (FVEY partners) that operates under warrant-like constraints.',
    'If warrant reform sufficient: snare classification dominates (suppression and extraction not necessary for coordination). If blanket authority necessary: tangled_rope holds (genuine coordination function plus extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_sharing_coordination_necessity, empirical, 'Whether information-sharing required Fourth Amendment suspension or warrant reform would suffice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2004_bush_patriot_act_renewal, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(patriot_theater_2001_initial, sotu_2004_bush_patriot_act_renewal, theater_ratio, 0, 0.52).
narrative_ontology:measurement(patriot_theater_2004_renewal, sotu_2004_bush_patriot_act_renewal, theater_ratio, 3, 0.68).
narrative_ontology:measurement(patriot_theater_2007_institutionalization, sotu_2004_bush_patriot_act_renewal, theater_ratio, 6, 0.71).

% Extraction over time
narrative_ontology:measurement(patriot_ext_2001_initial, sotu_2004_bush_patriot_act_renewal, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(patriot_ext_2004_renewal, sotu_2004_bush_patriot_act_renewal, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(patriot_ext_2007_institutionalization, sotu_2004_bush_patriot_act_renewal, base_extractiveness, 6, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2004_bush_patriot_act_renewal, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_2004_bush_patriot_act_renewal, fourth_amendment_scope_compression).
narrative_ontology:affects_constraint(sotu_2004_bush_patriot_act_renewal, state_secrets_doctrine_judicial_review).
narrative_ontology:affects_constraint(sotu_2004_bush_patriot_act_renewal, administrative_surveillance_architecture).

% DUAL FORMULATION NOTE:
% The Patriot Act renewal operates as a constraint family with three interdependent structures: (1) coordination of intelligence across federal agencies (genuine rope function), (2) extraction of civil liberties and procedural protections (snare function), (3) state secrets doctrine preventing meaningful judicial review (piton function). These are structurally distinct constraints with different ε values linked by institutional causality. Upstream constraint (intelligence fragmentation problem) gives rise to Patriot Act response; Patriot Act response creates downstream extraction (civil liberties erosion) and creates institutional dependencies that sustain itself through judicial deference (piton).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_2004_bush_patriot_act_renewal, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
