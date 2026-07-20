% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta Feudal Obsolescence Reading
 *   domain: constitutional history / legal philosophy / political theory
 *
 * SUMMARY:
 *   This constraint story instantiates the feudal_obsolescence_reading of the
 *   magna_carta_constraint_authority kernel. The kernel is the stabilized
 *   commitment that Magna Carta constitutes a constraint on sovereign power;
 *   this reading treats the charter as a historically situated feudal compact
 *   whose authority expired with the feudal order, leaving no binding force
 *   over modern executive, legislative, or judicial institutions. The
 *   constraint's operation is the interpretive and doctrinal practice that
 *   dismisses Magna Carta-based arguments in constitutional litigation and
 *   political discourse. It is claimed as a mountain of historical fact but
 *   functions as a snare: the 'historical accuracy' narrative covers the
 *   extraction of executive discretion from a potential source of higher-law
 *   restraint. The sibling readingsâliving_constitutionalism (perpetual
 *   binding precedent) and parliamentary_sovereignty (absorbed and repealable
 *   statute)âare structurally distinct constraints with different Îµ
 *   profiles and are modeled separately.
 *
 * KEY AGENTS:
 *   - executive_government: Primary beneficiary (institutional/arbitrage) â collects unconstrained discretion by treating medieval charter restraints as historically inert.
 *   - constitutional_restraint_advocates: Primary target (moderate/constrained) â bears the cost of losing a higher-law restraint mechanism against modern executive overreach.
 *   - popular_constitutionalists: Secondary target (powerless/constrained) â marginalized when invoking the charter in political or legal challenges to state action.
 *   - senior_judiciary: Agenda-setter (institutional/constrained) â administers the doctrinal dismissal of Magna Carta claims while preserving judicial legitimacy through positivist neutrality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.74).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, snare).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta Feudal Obsolescence Reading").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional history / legal philosophy / political theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__feudal_obsolescence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, '0e79ea32-67c4-4a42-a051-3129a2688971').
narrative_ontology:cs_kernel_codification('0e79ea32-67c4-4a42-a051-3129a2688971', fixed_text).
narrative_ontology:cs_authority_grounding('0e79ea32-67c4-4a42-a051-3129a2688971', lineage).
narrative_ontology:cs_interpretation_layer_present('0e79ea32-67c4-4a42-a051-3129a2688971').
narrative_ontology:cs_reading_relation('0e79ea32-67c4-4a42-a051-3129a2688971', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('0e79ea32-67c4-4a42-a051-3129a2688971', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('0e79ea32-67c4-4a42-a051-3129a2688971', foundational, charter_authority_terminates_with_feudal_order).
narrative_ontology:cs_axiom_status(charter_authority_terminates_with_feudal_order, holdable).
narrative_ontology:cs_axiom_grounding('0e79ea32-67c4-4a42-a051-3129a2688971', charter_authority_terminates_with_feudal_order, conventional).
narrative_ontology:cs_axiom('0e79ea32-67c4-4a42-a051-3129a2688971', foundational, positivist_sovereignty_rejects_higher_medieval_law).
narrative_ontology:cs_axiom_status(positivist_sovereignty_rejects_higher_medieval_law, holdable).
narrative_ontology:cs_axiom_grounding('0e79ea32-67c4-4a42-a051-3129a2688971', positivist_sovereignty_rejects_higher_medieval_law, conventional).
narrative_ontology:cs_reference_frame('0e79ea32-67c4-4a42-a051-3129a2688971', medieval_origins_terminus).
narrative_ontology:cs_drift_state('0e79ea32-67c4-4a42-a051-3129a2688971', contemporary_executive_sovereignty, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0e79ea32-67c4-4a42-a051-3129a2688971', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_government).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, constitutional_restraint_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the doctrinal dismissal of Magna Carta because it removes a potential external restraint on prerogative and statutory power; legal advisors routinely cite the charter's feudal origins to resist judicial review claims and preserve broad discretion over security, taxation, and administration.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_government, beneficiary,
    institutional, generational, arbitrage, national).

% Argue that Magna Carta's due process and limitation clauses retain normative force against modern overreach; their arguments are systematically rejected by courts as historically misplaced, forcing reliance on weaker statutory or common-law grounds and bearing the cost of a lost higher-law mechanism.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, constitutional_restraint_advocates, payer,
    moderate, generational, constrained, national).

% Citizen movements and litigants invoking Magna Carta against state overreach; treated as historically naive by courts and legal elites, their participatory channel is foreclosed and their political identity is marginalized within positivist constitutional discourse.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalists, payer,
    powerless, biographical, constrained, national).

% Administers the doctrinal boundary between historical curiosity and binding law; explicitly treats Magna Carta as feudal context in judgments, thereby enforcing its non-binding status while preserving judicial legitimacy through claims of positivist neutrality and comity with the political branches.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, senior_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_government).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__feudal_obsolescence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves interpretive conflict about the historical scope of medieval charters by locating Magna Carta in 13th-century feudal grievances, preventing anachronistic legal arguments from destabilizing modern statutory and executive sovereignty.
% TRANSFER_FUNCTION: Moves authority to dismiss constitutional restraint claims from historical juridical limits to modern executive and parliamentary sovereignty; transfers the cost of lost restraint to litigants and popular constitutional movements.
% ABSENT_VOICES: Medievalists who treat Magna Carta as a perpetually renewed compact, comparative constitutionalists who see its principles as binding higher-law elements, and popular litigants invoking clause 39 against arbitrary detention are absent from dominant British constitutional doctrine; their exclusion is enforced by positivist legal education and judicial appointment.
% DISAPPEARANCE_RATIONALE: If the feudal-obsolescence reading vanished and Magna Carta were treated as a binding constraint on modern sovereignty, executive and parliamentary discretion would face new juridical limits; constitutional litigation would reorganize around charter provisions, and the current positivist settlement would destabilize.
% FOUNDING_PROBLEM: To prevent anachronistic medieval feudal instruments from overriding modern parliamentary statute and executive discretion.
% FOUNDING_PROBLEM_CORROBORATION: Executive legal advisors and parliamentary sovereignty theorists attest the problem is live; comparative constitutionalists and popular constitutionalists outside the beneficiary set attest the problem is fabricated to dissolve higher-law restraint, corroborated by jurisdictions where medieval charters remain constitutionally operative.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is moderate-to-high because the reading systematically transfers the cost of lost constitutional restraint to litigants and movements while the executive captures discretion. Suppression (0.74) is high because the constraint persists only by actively excluding alternative readings in legal education, judicial appointment, and doctrinal gatekeeping; alternatives are not intellectually refuted but institutionally disallowed. Theater ratio (0.55) reflects that a growing share of the 'feudal context' discourse is performative: it recites historical detail to display scholarly neutrality while the political function is to nullify restraint claims. Accessibility collapse (0.60) captures that once the obsolescence frame is accepted, alternative constitutional arguments appear intellectually naive rather than legally plausible. Resistance (0.42) is moderate: popular constitutionalists and some jurists resist, but they lack institutional leverage against the positivist mainstream.
 *
 * PERSPECTIVAL GAP:
 *   The executive and judiciary experience this constraint as benign historical correctionâremoving anachronistic legal clutterâwhile constitutional restraint advocates and popular constitutionalists experience it as an active barrier to invoking historical guarantees against modern overreach. The engine computes this divergence from the structural data: the beneficiary seats have low directionality (subsidized by the constraint), while the victim seats have high directionality (extracted from).
 *
 * DIRECTIONALITY LOGIC:
 *   Executive_government is the structural beneficiary: the constraint subsidizes its discretion by removing a potential external check (d near 0.0). Constitutional_restraint_advocates and popular_constitutionalists are structural victims: the constraint extracts from them by foreclosing a legal and political channel (d near 1.0). Senior_judiciary sits near symmetric or slightly subsidized: it does not collect the extraction directly, but benefits from doctrinal stability and comity with the political branches (d ~0.35).
 *
 * MANDATROPHY ANALYSIS:
 *   The reading prevents mislabeling by distinguishing the genuine historical coordination function (accurately placing Magna Carta in context) from the extraction function (using that context to negate all modern juridical force). The coordination story is not falseâMagna Carta is indeed feudalâbut the inference from feudal origin to non-bindingness is a non-sequitur that serves extraction. If the coordination were pure, the constraint would be a rope (historical consensus with no victims); the presence of identifiable victims (restraint advocates) and a concentrated beneficiary (executive discretion) makes it a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the feudal_obsolescence_reading of the magna_carta_constraint_authority kernel; would adopting the living_constitutionalism_reading or parliamentary_sovereignty_reading change the beneficiary/victim structure or the extractiveness profile?',
    'Cross-reading comparison of the same institutional seats under sibling constraints.',
    'If sibling readings flip the victim/beneficiary sets, the kernel''s classification is reading-dependent and no single reading captures the constraint''s full political economy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural uncertainty from kernel reading multiplicity').

omega_variable(
    historical_accuracy_vs_doctrinal_convenience,
    'Does the scholarly consensus on Magna Carta''s feudal specificity genuinely entail its lack of modern binding authority, or is the inference from historical context to legal non-bindingness a non-sequitur serving executive convenience?',
    'Comparative legal analysis of jurisdictions where medieval charters are treated as perpetually binding constitutional instruments.',
    'If historical accuracy does not entail non-bindingness, the coordination story is cover and the constraint is more extractive than its scholarly defense admits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_accuracy_vs_doctrinal_convenience, conceptual, 'Whether the non-bindingness inference is historically valid or doctrinally convenient').

omega_variable(
    suppression_of_alternative_readings,
    'Are alternative readings of Magna Carta (as binding higher law) suppressed by the legal education and judicial appointment systems, or do they simply fail on intellectual merits?',
    'Citation analysis of judicial opinions and law school curricula; tracking of career consequences for scholars advocating binding Magna Carta interpretations.',
    'If suppression is institutional rather than intellectual, the constraint''s persistence depends on coercion and exits are narrower than they appear.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternative_readings, empirical, 'Institutional suppression of competing constitutional readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(magn_tr_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(magn_tr_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(magn_tr_t60, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 60, 0.47).
narrative_ontology:measurement(magn_tr_t80, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 80, 0.52).
narrative_ontology:measurement(magn_tr_t100, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(magn_be_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(magn_be_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(magn_be_t60, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(magn_be_t80, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 80, 0.67).
narrative_ontology:measurement(magn_be_t100, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(magn_su_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(magn_su_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(magn_su_t60, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(magn_su_t80, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 80, 0.73).
narrative_ontology:measurement(magn_su_t100, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 100, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, identity_coordination).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the magna_carta_constraint_authority kernel. The kernel decomposes into at least three structurally distinct constraints because the Îµ values and beneficiary/victim structures differ across readings: feudal_obsolescence treats the charter as inert (high extractiveness for executive discretion), living_constitutionalism treats it as perpetually binding restraint (low extractiveness for rulers, potential extraction for litigants), and parliamentary_sovereignty treats it as absorbed and repealable statute (moderate extraction contingent on parliamentary majority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
