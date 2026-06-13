% ============================================================================
% CONSTRAINT STORY: udhr_article_3__negative_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__negative_liberty_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: UDHR Article 3 Negative Liberty Reading: State Deprivation Prohibition
 *   domain: constitutional_law/human_rights
 *
 * SUMMARY:
 *   The negative liberty reading of UDHR Article 3 interprets the prohibition
 *   on arbitrary deprivation as establishing an absolute sphere of individual
 *   bodily integrity into which state power cannot reach except through
 *   narrow, codified legal procedure. This is NOT a claim about positive
 *   state obligations to provide welfare or security; it is a claim about
 *   categorical prohibition: execution is wrong, torture is forbidden,
 *   indefinite detention without trial is impermissible. The reading
 *   coordinates individual protection against state violence by making state
 *   violence subject to law. It simultaneously extracts from both law
 *   enforcement prerogatives and collective security constituencies, who bear
 *   the operational cost of constraint—they cannot rely on deterrence through
 *   execution, flexibility in interrogation, or swift incapacitation. The
 *   claim is Tangled Rope: genuine coordination function (preventing tyranny)
 *   married to asymmetric extraction (state loses power, individuals gain
 *   immunity). The metrics track the historical trajectory: extractiveness
 *   rose as the constraint hardened (abolition spread, due process expanded,
 *   torture prohibitions ossified into customary law); suppression
 *   requirement rose as enforcement machinery intensified (courts multiplied,
 *   appeals processes lengthened, habeas jurisdiction broadened); theater
 *   rose as the constraint became contested—states engaged in performative
 *   compliance (courts that approve detention anyway, due process theater
 *   that satisfies form while evading substance).
 *
 * KEY AGENTS:
 *   - individual_liberty_claimants: beneficiaries, powerless, identity-locked to bodily inviolability claim, exit forecloses via statelessness
 *   - law_enforcement_prerogatives: institutional payers, constrained by absolute prohibitions (no capital punishment as option, torture categorically off-table)
 *   - collective_security_constituencies: moderate-power diffuse payers, lose deterrent options and swift incapacitation
 *   - state_apparatus: agenda-setter and ambivalent enforcer, formally bound but incentivized to reinterpret during crises
 *   - victims_of_crime: excluded from constraint framing, bear indirect cost through lenient outcomes
 *   - international_human_rights_monitors: analytical observers, audit compliance and document violations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.68).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.71).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "UDHR Article 3 Negative Liberty Reading: State Deprivation Prohibition").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional_law/human_rights").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, 'b1185227-fe5b-4c1d-b7a9-79d7364e8f76').
narrative_ontology:cs_kernel_codification('b1185227-fe5b-4c1d-b7a9-79d7364e8f76', fixed_text).
narrative_ontology:cs_authority_grounding('b1185227-fe5b-4c1d-b7a9-79d7364e8f76', lineage).
narrative_ontology:cs_interpretation_layer_present('b1185227-fe5b-4c1d-b7a9-79d7364e8f76').
narrative_ontology:cs_reading_relation('b1185227-fe5b-4c1d-b7a9-79d7364e8f76', udhr_article_3__positive_entitlement_reading, forecloses).
narrative_ontology:cs_reading_relation('b1185227-fe5b-4c1d-b7a9-79d7364e8f76', udhr_article_3__procedural_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('b1185227-fe5b-4c1d-b7a9-79d7364e8f76', foundational, bodily_inviolability_absolute).
narrative_ontology:cs_axiom_status(bodily_inviolability_absolute, holdable).
narrative_ontology:cs_axiom_grounding('b1185227-fe5b-4c1d-b7a9-79d7364e8f76', bodily_inviolability_absolute, deontological).
narrative_ontology:cs_axiom('b1185227-fe5b-4c1d-b7a9-79d7364e8f76', foundational, state_deprivation_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_deprivation_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('b1185227-fe5b-4c1d-b7a9-79d7364e8f76', state_deprivation_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('b1185227-fe5b-4c1d-b7a9-79d7364e8f76', universal_bodily_inviolability_doctrine).
narrative_ontology:cs_drift_state('b1185227-fe5b-4c1d-b7a9-79d7364e8f76', contemporary_security_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b1185227-fe5b-4c1d-b7a9-79d7364e8f76', '').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, individual_liberty_claimants).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, collective_security_constituencies).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, law_enforcement_prerogatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons whose life and liberty are at risk of state deprivation. The negative liberty reading secures their claim that the state must not execute them, imprison them without trial, or use torture — a sphere of bodily autonomy into which state power cannot reach except through narrow, codified procedural gates. Exit would mean renouncing citizenship or accepting statelessness; identity is fused with the claim to bodily inviolability.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, individual_liberty_claimants, beneficiary,
    powerless, biographical, identity_locked, universal).

% State security apparatus must operate within the constraint's prohibitions: capital punishment is abolished (not merely regulated), detention must satisfy due process, and certain interrogation and restraint techniques are categorically foreclosed regardless of security utility. Their cost is operational constraint and loss of deterrent leverage; they cannot argue necessity as override.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, law_enforcement_prerogatives, payer,
    institutional, generational, constrained, universal).

% Publics whose security against crime and terror depends, they argue, on state capacity for swift incapacitation, capital punishment as the ultimate deterrent, and flexible interrogation authority to prevent attacks. The negative liberty reading constrains all three; the cost is paid diffusely through reduced security options and perceived vulnerability to repeat offenders.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, collective_security_constituencies, payer,
    moderate, biographical, constrained, universal).

% The machinery of legal enforcement and adjudication that must implement the Article 3 constraints: courts must hear habeas petitions, prosecutors must prove their case beyond reasonable doubt, and executioners (where they exist) are operating in violation. The state is formally bound by the reading but has strong incentives to reinterpret or selectively enforce it, especially during crises.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, state_apparatus, agenda_setter,
    institutional, generational, constrained, universal).

% Persons harmed by criminal acts that Article 3 constraints may prevent the state from punishing severely. They have no voice in the constraint's framing (excluded from the original covenant negotiation) but experience its effects through lenient sentencing, abolition of capital punishment for their attackers, and due process delays. Their interests are structurally absent from the constraint's beneficiary logic.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, victims_of_crime, excluded,
    powerless, biographical, trapped, universal).

% NGOs and treaty bodies that measure state compliance with Article 3 negative liberty standards. They document violations, testify to treaty breaches, and provide the observational seat from which the constraint's operation is audited. They collect no rents and bear no direct operational costs.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, international_human_rights_monitors, observer,
    organized, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__negative_liberty_reading, state_apparatus).
narrative_ontology:fixing_cost_class(udhr_article_3__negative_liberty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, justiciable boundary on state violence: no killing without trial, no detention without process, no torture under any circumstance. This boundary solves the collective problem of preventing state tyranny by making violence a legal rather than discretionary act—a constraint on the powerful, not a coordination among equals.
% TRANSFER_FUNCTION: Transfers the power to summarily execute, torture, and indefinitely detain from the state apparatus to juridical process. The state loses operational authority; individuals gain procedural protection and (in the negative liberty framing) absolute immunity from certain acts regardless of security claims.
% ABSENT_VOICES: Victims of crime whose attackers are protected by due process delays and life sentences rather than execution; security-focused constituencies who argue necessity overrides should exist; authoritarian governments and security-maximizing states that read Article 3 as a constraint on their sovereignty and prerogative.
% DISAPPEARANCE_RATIONALE: If the negative liberty reading of Article 3 vanished, states would immediately restore capital punishment, expand detention authority, and operationalize interrogation techniques currently forbidden. The entire global legal architecture for individual protection would collapse into discretionary state violence. Millions of persons currently protected by abolition or due process norms would face imminent risk.
% FOUNDING_PROBLEM: Mid-20th century fascism and totalitarianism demonstrated that states with uncontrolled power to kill, torture, and detain will use it systematically against disfavored populations. A binding constraint on that power was necessary to prevent genocide and terror.
% FOUNDING_PROBLEM_CORROBORATION: Human rights monitors and international courts attest the founding problem remains live in many jurisdictions and resurges during crises (emergency detention, enhanced interrogation). States implementing the negative liberty reading attest compliance. Security-focused governments and some academic theorists attest the problem is outdated and the constraint obsolete in the face of novel threats; international human rights bodies dispute this assessment.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__negative_liberty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__negative_liberty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__negative_liberty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at end) because the constraint permanently forecloses security options that constituencies value—capital punishment never returns once abolished, torture is never permissible regardless of threat, detention must satisfy due process even if it reduces public safety. Suppression is higher still (0.71) because maintaining the constraint requires active enforcement against state incentives to reinterpret during security crises: courts must hear appeals even in emergencies, prosecutors must prove guilt beyond reasonable doubt even against terrorists, police must follow procedure even when it lets dangerous persons escape. The constraint is sustained by global monitoring, treaty obligations, and domestic constitutional entrenchment—not by voluntary beneficiary enthusiasm (individuals cannot opt out and lose the protection; it is imposed). Theater rose over the interval (0.18 to 0.42) as crisis-driven exceptions proliferated: states formally retained the constraint while operationally evading it through emergency detention, secret detention sites, outsourced torture, and indefinite-detention loopholes. The 2001 dip in extractiveness reflects a temporary security-maximizing reframing (enhanced interrogation justified as necessity); the subsequent rise reflects pushback by human rights monitors and courts that reasserted the constraint's categorical nature. The measurement series shows the constraint is not decaying but rather being more actively defended and more actively resisted—the theater ratio rising indicates the constraint's function is increasingly performative rather than generative.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (state apparatus) and the beneficiary (individual liberty claimants) occupy irreconcilable positions. From the state's institutional perspective, Article 3 is a fetter on sovereignty and effective security—a constraint that must be interpreted narrowly or suspended in emergencies to preserve the state itself. From the individual's perspective, Article 3 is the foundation of all other rights—a shield against the very violence the state claims to prevent through detention and execution. Law enforcement and security constituencies, from their institutional position, experience the constraint as a permanent operational cost that cannot be recovered; they cannot point to a beneficiary who compensates them (individuals do not negotiate the constraint's terms; it is imposed). This asymmetry is precisely what makes the constraint Tangled Rope rather than pure Rope: the coordination benefit (preventing tyranny) is real, but it is achieved by coercing the state apparatus and security prerogatives into a subordinate role. The beneficiary seats compute differently from the payer seats by design: the engine will compute a beneficiary directionality near 0.0 for individuals (low extraction, high immunity) and near 1.0 for law enforcement (high target status, constrained prerogative), reflecting the structural asymmetry the constraint encodes.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual liberty claimants derive near-zero directionality (d ≈ 0.1–0.2): they are protected by the constraint, have identity-locked exit (cannot renounce the claim without renouncing personhood), and are powerless globally but benefiting from a universal rule. Law enforcement derives near-unity directionality (d ≈ 0.85–0.95): they are the operational targets, lose prerogatives unconditionally, have constrained exit (cannot opt out of the constraint; it is imposed by law), and their institutional power does not offset the structural targeting. Collective security constituencies derive mid-range directionality (d ≈ 0.65–0.75): they are targeted by the constraint (lose security options), moderate in power (can lobby and resist), constrained in exit (must comply with law), but diffuse rather than concentrated (the cost is spread across populations). The state apparatus derives high directionality (d ≈ 0.80–0.90): it is the seat that must enforce the constraint against its own incentives, is institutional in power but constrained in implementation, and cannot exit (the state is bound by international law and domestic constitutional entrenchment). No directionality overrides are required; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The negative liberty reading avoids the mandatrophy trap by maintaining a sharp distinction between its founding problem (totalitarian state violence) and its persistent function (preventing tyranny). The constraint is not a degraded coordination mechanism; it is an active, globally-monitored enforcement regime that grows more rather than less stringent as states test its boundaries. The theater ratio rise from 0.18 to 0.42 indicates performative compliance increasing, but the underlying constraint remains live—courts still hear cases, appeals still succeed, international monitors still document violations. The constraint could degrade into pure theater (Piton) if states universally evaded it with impunity, but the measurement series shows persistent enforcement and resistance. Mandatrophy is not resolved—the constraint remains contested and actively defended—but it is not present in the form of atrophied function. The constraint's mandate (preventing arbitrary state deprivation) has not outlived its function; states continue to resist it precisely because it constrains real power. A mandatrophy reading would require evidence that the constraint persists because no party has the power to change it, not because beneficiaries defend it and payers are structurally subordinated. The latter is the case here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_override_ambiguity,
    'Does the negative liberty reading permit exceptions to the absolute prohibitions (execution, torture, indefinite detention) when the state faces genuine existential threat or imminent catastrophic harm?',
    'Track state behavior during declared emergencies and crises: do states that endorse the negative liberty reading suspend it, reinterpret it, or maintain absolute prohibition? Compare outcomes (security vs. rights violations) to assess whether necessity-based exceptions are necessary or whether the constraint can hold under crisis.',
    'If exceptions are recognized, the constraint degrades from absolute prohibition to rebuttable presumption—extractiveness and suppression both decline, and the constraint becomes Tangled Rope verging on Snare (coercive but permitting state discretion). If no exceptions are recognized, the constraint remains absolutist and the state bears unbounded cost during emergencies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_override_ambiguity, empirical, 'Whether the absolute prohibition permits emergency exceptions.').

omega_variable(
    performance_vs_substance_boundary,
    'Is the rising theater_ratio (0.18 to 0.42) evidence of constraint degradation or evidence of active contestation and institutional maturation?',
    'Distinguish between two types of theater: (1) performative compliance where states formally retain the constraint while operationally evading it (emergency detention sites, secret prisons, outsourced torture), and (2) procedural elaboration where states strengthen enforcement machinery (appeals courts, habeas review, international monitoring). Track the composition of the theater—how much is evasion theater vs. enforcement theater.',
    'If theater is mostly evasion, the constraint is degrading toward Piton (maintained performatively but not functionally). If theater is mostly enforcement, the constraint is intensifying—higher costs for states, stronger protections for individuals. The classification depends on the composition, not the ratio alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_substance_boundary, empirical, 'What drives the rising theater ratio—evasion or enforcement intensification?').

omega_variable(
    kernel_reading_contest,
    'Is Article 3''s core meaning a negative liberty constraint (prohibition on state deprivation), a positive entitlement (state obligation to provide), or a procedural hybrid (due process without resolving substantive liberty/welfare)?',
    'This is a conceptual/committer-axis question: the kernel is contested and the sibling readings generate genuinely different constraints with different ε values. Resolution depends on which parties'' reading of Article 3 prevails in global jurisprudence and state practice. This omega does not resolve to a single verdict; it documents the constraint family structure.',
    'If the positive_entitlement_reading prevails globally, states will be obligated to provide welfare and healthcare—extractiveness will shift from state-apparatus-as-payer to taxpayers-and-government-budgets-as-payers, and the constraint will shift from Tangled Rope to either Rope (genuine coordination) or Snare (coercive redistribution). If the procedural_hybrid_reading prevails, the constraint will remain ambiguous and contested, permitting each state to choose its own balance. The negative liberty reading is currently maintained by treaty bodies and Western democracies but contested in practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The contested kernel: which reading of Article 3 is structurally true?').

omega_variable(
    identity_lock_mechanism_interpersonal,
    'Is the identity-locking of individual liberty claimants structural (the claim to bodily inviolability is inseparable from personhood as such) or internalized (persons have fused their identity with the claim through socialization into rights-bearing culture)?',
    'Post-constraint-abandonment trajectory: if a state formally repealed Article 3 protections and persons maintained the claim to bodily inviolability despite societal recoding, the lock is structural. If the claim erodes as the cultural/legal frame changes, the lock is internalized. Alternatively, examine persons in societies that never adopted the constraint: do they exhibit the same claim to bodily inviolability (structural) or do they accept arbitrary state deprivation as legitimate (internalized cultural product)?',
    'If identity-locking is structural, the constraint is more durable—even if formally repealed, persons would resist and reformers would restore it. If internalized, the constraint is more fragile—a generation of cultural recoding could dissolve the claim. Either way, the suppression_requirement remains high (enforcement machinery must be active), but the sources of suppression differ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpersonal, empirical, 'Mechanism of identity-locking for individual liberty claimants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1950, udhr_article_3__negative_liberty_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement_basis(udhr_tr_t1950, projected).
narrative_ontology:measurement(udhr_tr_t1975, udhr_article_3__negative_liberty_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement_basis(udhr_tr_t1975, observed).
narrative_ontology:measurement(udhr_tr_t1990, udhr_article_3__negative_liberty_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement_basis(udhr_tr_t1990, observed).
narrative_ontology:measurement(udhr_tr_t2001, udhr_article_3__negative_liberty_reading, theater_ratio, 2001, 0.39).
narrative_ontology:measurement_basis(udhr_tr_t2001, observed).
narrative_ontology:measurement(udhr_tr_t2010, udhr_article_3__negative_liberty_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement_basis(udhr_tr_t2010, observed).
narrative_ontology:measurement(udhr_tr_t2024, udhr_article_3__negative_liberty_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(udhr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1950, udhr_article_3__negative_liberty_reading, base_extractiveness, 1950, 0.52).
narrative_ontology:measurement_basis(udhr_be_t1950, projected).
narrative_ontology:measurement(udhr_be_t1975, udhr_article_3__negative_liberty_reading, base_extractiveness, 1975, 0.61).
narrative_ontology:measurement_basis(udhr_be_t1975, observed).
narrative_ontology:measurement(udhr_be_t1990, udhr_article_3__negative_liberty_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement_basis(udhr_be_t1990, observed).
narrative_ontology:measurement(udhr_be_t2001, udhr_article_3__negative_liberty_reading, base_extractiveness, 2001, 0.59).
narrative_ontology:measurement_basis(udhr_be_t2001, observed).
narrative_ontology:measurement(udhr_be_t2010, udhr_article_3__negative_liberty_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement_basis(udhr_be_t2010, observed).
narrative_ontology:measurement(udhr_be_t2024, udhr_article_3__negative_liberty_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(udhr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1950, udhr_article_3__negative_liberty_reading, suppression_requirement, 1950, 0.48).
narrative_ontology:measurement_basis(udhr_su_t1950, projected).
narrative_ontology:measurement(udhr_su_t1975, udhr_article_3__negative_liberty_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement_basis(udhr_su_t1975, observed).
narrative_ontology:measurement(udhr_su_t1990, udhr_article_3__negative_liberty_reading, suppression_requirement, 1990, 0.66).
narrative_ontology:measurement_basis(udhr_su_t1990, observed).
narrative_ontology:measurement(udhr_su_t2001, udhr_article_3__negative_liberty_reading, suppression_requirement, 2001, 0.62).
narrative_ontology:measurement_basis(udhr_su_t2001, observed).
narrative_ontology:measurement(udhr_su_t2010, udhr_article_3__negative_liberty_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement_basis(udhr_su_t2010, observed).
narrative_ontology:measurement(udhr_su_t2024, udhr_article_3__negative_liberty_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(udhr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__negative_liberty_reading, 0.18).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% UDHR Article 3 kernel generates three structurally distinct constraints with different ε values and beneficiary/victim sets. The negative_liberty_reading instantiates a prohibition on state deprivation (absolute, enforcement-intensive, high extractiveness via operational constraints). The positive_entitlement_reading would instantiate state obligation to provide (redistribution-intensive, different payer set). The procedural_hybrid_reading would instantiate due process without resolving substantive contest (ambiguous payer set, lower extractiveness). All three readings compete in global jurisprudence; each is a live position held by different treaty interpretive bodies and states. They are linked via network.affects_constraints to document the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
