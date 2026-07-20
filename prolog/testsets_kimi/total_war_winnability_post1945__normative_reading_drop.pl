% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Normative Illegitimacy of Total War Post-1945 (Normative Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint is the normative_reading_drop of the contested kernel
 *   total_war_winnability_post1945. It holds that total war remains
 *   physically possible but was rendered normatively illegitimate by Article
 *   2(4) of the UN Charter and the development of international humanitarian
 *   law. The constraint functions as a coordination mechanism among states to
 *   avoid civilizational destruction, with global civilian populations as the
 *   primary beneficiaries and revisionist powers bearing the cost of foregone
 *   strategic optionality. The claim is Rope-class: it solves a genuine
 *   collective-action problem through treaty-based coordination with minimal
 *   coercive overhead, even though revisionist powers experience it as a
 *   binding limitation.
 *
 * KEY AGENTS:
 *   - Global civilian populations (beneficiary/organized): Protected by the normative prohibition; benefit from reduced exposure to unlimited war.
 *   - Revisionist powers constrained by norms (payer/powerful): Bear the opportunity cost of foregone total-war strategies; constrained by legal and reputational costs.
 *   - Status quo powers (agenda_setter/powerful): Founders and maintainers of the UN Charter system; benefit from stability and institutional legitimacy.
 *   - International legal institutions (observer/institutional): Interpret and reinforce the norm without capturing its gains.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.18).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.24).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.18).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.24).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Normative Illegitimacy of Total War Post-1945 (Normative Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/strategic_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, 'abcce6ee-ec2c-4202-9db5-8c9c48574f1c').
narrative_ontology:cs_kernel_codification('abcce6ee-ec2c-4202-9db5-8c9c48574f1c', formalized).
narrative_ontology:cs_authority_grounding('abcce6ee-ec2c-4202-9db5-8c9c48574f1c', lineage).
narrative_ontology:cs_interpretation_layer_present('abcce6ee-ec2c-4202-9db5-8c9c48574f1c').
narrative_ontology:cs_reading_relation('abcce6ee-ec2c-4202-9db5-8c9c48574f1c', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('abcce6ee-ec2c-4202-9db5-8c9c48574f1c', total_war_winnability_post1945__strategic_culture_drift, influences).
narrative_ontology:cs_axiom('abcce6ee-ec2c-4202-9db5-8c9c48574f1c', foundational, aggressive_war_charter_prohibited).
narrative_ontology:cs_axiom_status(aggressive_war_charter_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('abcce6ee-ec2c-4202-9db5-8c9c48574f1c', aggressive_war_charter_prohibited, conventional).
narrative_ontology:cs_axiom('abcce6ee-ec2c-4202-9db5-8c9c48574f1c', foundational, total_war_civilian_harm_morally_illegitimate).
narrative_ontology:cs_axiom_status(total_war_civilian_harm_morally_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('abcce6ee-ec2c-4202-9db5-8c9c48574f1c', total_war_civilian_harm_morally_illegitimate, deontological).
narrative_ontology:cs_reference_frame('abcce6ee-ec2c-4202-9db5-8c9c48574f1c', postwar_pacific_settlement_order).
narrative_ontology:cs_drift_state('abcce6ee-ec2c-4202-9db5-8c9c48574f1c', contemporary_multipolar_challenges, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('abcce6ee-ec2c-4202-9db5-8c9c48574f1c', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers_constrained_by_norms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Diffuse global population whose physical security and economic stability depend on the normative prohibition of total war; they benefit from the legal restraints on targeting and territorial aggression but do not directly administer the constraint.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    organized, generational, constrained, global).

% States seeking to revise the territorial or institutional status quo through unlimited military means; they bear the opportunity cost of foregone total-war strategies and face institutional opprobrium, sanctions, and legal jeopardy when they challenge the prohibition.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers_constrained_by_norms, payer,
    powerful, generational, constrained, global).

% Great powers that founded and sustain the UN Charter system; they set the interpretive agenda for Article 2(4), benefit from systemic stability, and bear the primary burden of defending the norm through diplomacy, sanctions, and institutional leadership.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, status_quo_powers, agenda_setter,
    powerful, generational, constrained, global).

% International Court of Justice, International Criminal Court, and ad hoc tribunals that adjudicate breaches of the prohibition on aggressive war and humanitarian law; they interpret and reinforce the normative framework without capturing its gains.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_legal_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__normative_reading_drop, diffuse).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__normative_reading_drop, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents total war and protects civilian populations through a shared legal norm that solves the collective-action problem of escalation and retaliation among states.
% TRANSFER_FUNCTION: Moves the legitimate use of total-war violence away from revisionist states and toward the protected status of global civilian populations; status quo powers gain systemic stability in exchange for institutional maintenance.
% ABSENT_VOICES: Non-state armed actors operating outside the UN Charter framework; civilians in non-member states or unrecognized territories; future generations inheriting the norm without voice in its original construction.
% DISAPPEARANCE_RATIONALE: If the normative prohibition vanished overnight, the restraints on total war would collapse; states could legally pursue unlimited objectives against civilian infrastructure, and the post-1945 international order would unravel into unrestrained strategic competition.
% FOUNDING_PROBLEM: Pre-1945 total warfare produced catastrophic civilian casualties, economic devastation, and undermined the stability of the international state system; the World Wars demonstrated that unrestrained war threatened civilization itself.
% FOUNDING_PROBLEM_CORROBORATION: Historical record of World War I and World War II civilian death tolls; strategic studies literature documenting the civilizational cost of unrestrained war; corroborated by neutral international historians, the International Committee of the Red Cross, and third-party academic institutions outside the status-quo power bloc.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__normative_reading_drop, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end) because the constraint coordinates a genuine collective-action problem â preventing total war â and the 'cost' to revisionist powers is primarily opportunity cost rather than a concentrated rent transfer. Suppression is low (0.24) because the norm persists through institutionalized coordination, reputational costs, and voluntary adherence rather than active coercion; alternatives (limited war, diplomacy) are not suppressed. Theater ratio is low (0.15) because the legal framework remains substantively functional, though recent geopolitical challenges have increased performative defense of the norm. Accessibility collapse is moderately high (0.70) because once the norm is internalized, total war becomes unthinkable as a legitimate policy option for most states. Resistance is moderate (0.30) because revisionist powers periodically challenge the norm, but the coordination remains broadly stable. Measurements share a single time grid to prevent misaligned drift signals.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of global civilian populations, the constraint is an unambiguous benefit (directionality near the beneficiary pole). From the seat of revisionist powers, the constraint reads as an externally imposed limitation on sovereign military prerogatives (directionality near the target pole). Status quo powers sit closer to the beneficiary end because they gain systemic stability and institutional legitimacy, though they bear the costs of norm enforcement. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (global_civilian_populations) and agenda_setters (status_quo_powers) have low directionality because the constraint subsidizes their security and institutional interests. The payer (revisionist_powers_constrained_by_norms) has high directionality because the constraint directly extracts strategic optionality. Exit options reinforce this: status quo powers can reform or reinterpret the system, while revisionist powers face high exit costs (sanctions, ostracism, loss of diplomatic recognition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â catastrophic total war producing civilizational-scale destruction â remains live, as evidenced by existing nuclear arsenals and renewed great-power rivalry. The arrangement has not outlived its function, and no party administers it purely for inertia or theater. Were the problem ever solved (e.g., through universal disarmament or permanent great-power reconciliation), the norm might degrade toward a piton, but currently it remains a functional rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the normative_reading_drop of kernel total_war_winnability_post1945. Sibling readings include structural_contraction_reading (physical impossibility via nuclear deterrence) and strategic_culture_drift (ideational abandonment in elite discourse). Does adopting this normative reading foreclose the structural reading, or can they coexist within a single analytical framework?',
    'Examine whether nuclear deterrence and UN Charter norms are treated as analytically separable mechanisms or as a merged explanation in strategic studies literature and state doctrine.',
    'If merged, the rope classification is contaminated by mountain elements (physical impossibility), potentially reclassifying this seat; if separable, the normative reading stands as a distinct rope constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Position of this reading within the contested kernel and relation to sibling readings').

omega_variable(
    revisionist_beneficiary_ambiguity,
    'Do revisionist powers actually benefit from the total-war prohibition via the general stability it affords all states, or are they net payers due to foregone strategic options and compliance costs?',
    'Game-theoretic and historical analysis of whether rising or status-quo-challenging states prefer a constrained or unconstrained conflict environment; examine revealed preference through treaty adherence and violation patterns.',
    'If revisionist powers are net beneficiaries, the victim declaration is structurally false and the rope is purer; if net payers, the constraint may function as a tangled_rope rather than a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revisionist_beneficiary_ambiguity, empirical, 'Whether revisionist powers are victims or latent beneficiaries of the norm').

omega_variable(
    hegemonic_enforcement_vs_voluntary_coordination,
    'Is compliance with Article 2(4) driven by voluntary coordination among states recognizing mutual benefit, or by hegemonic enforcement of the post-war order by status quo powers?',
    'Historical analysis of sanctioning patterns, collective security coalitions, and the distribution of enforcement costs relative to norm compliance across different state types.',
    'If hegemonic, directionality for revisionist powers shifts toward target and effective extraction rises; if voluntary, the low-extraction rope profile holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hegemonic_enforcement_vs_voluntary_coordination, empirical, 'Whether coordination is voluntary or hegemonically enforced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(normative_reading_drop_tr_t0, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0, 0.1).
narrative_ontology:measurement(normative_reading_drop_tr_t20, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 20, 0.08).
narrative_ontology:measurement(normative_reading_drop_tr_t40, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 40, 0.06).
narrative_ontology:measurement(normative_reading_drop_tr_t60, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 60, 0.12).
narrative_ontology:measurement(normative_reading_drop_tr_t80, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 80, 0.15).

% Extraction over time
narrative_ontology:measurement(normative_reading_drop_be_t0, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(normative_reading_drop_be_t20, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(normative_reading_drop_be_t40, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(normative_reading_drop_be_t60, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 60, 0.14).
narrative_ontology:measurement(normative_reading_drop_be_t80, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 80, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(normative_reading_drop_su_t0, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(normative_reading_drop_su_t20, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(normative_reading_drop_su_t40, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(normative_reading_drop_su_t60, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 60, 0.2).
narrative_ontology:measurement(normative_reading_drop_su_t80, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 80, 0.24).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is one member of the total_war_winnability_post1945 constraint family. It is analytically separable from its siblings by its distinct epsilon (low extraction, coordination function) and its distinct beneficiary/victim structure. The normative reading treats the kernel as a formalized legal commitment; the structural contraction reading treats it as a physical limit; the strategic culture drift reading treats it as an ideational piton.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
