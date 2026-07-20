% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Feudal Oath under Ecclesiastical Mediation and Sacramental Limit
 *   domain: medieval_political_economy/legal_history
 *
 * SUMMARY:
 *   This constraint is the ecclesiastical_mediation_reading of the contested
 *   kernel feudal_oath_reciprocity. The kernel asks what structural work the
 *   feudal oath performs. This reading holds that the oath is bound by
 *   Christian charity and sacramental obligation, which limits secular
 *   extraction and vests interpretive authority in the ecclesiastical
 *   hierarchy. Sibling readings include lord_extraction_reading (maximal
 *   extraction bounded by capacity) and vassal_coordination_reading (fixed
 *   reciprocal obligations enforced by charter text).
 *
 * KEY AGENTS:
 *   - ecclesiastical_hierarchy: Primary agenda-setter (institutional/arbitrage) â administers sacramental framework and gains interpretive authority
 *   - secular_lords: Primary payer (powerful/constrained) â bear the cost of limited extraction under theological constraints
 *   - vassal_peasantry: Secondary beneficiary (powerless/trapped) â receive limited protection from unchecked extraction but remain bound to the manorial economy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.58).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.62).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath under Ecclesiastical Mediation and Sacramental Limit").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval_political_economy/legal_history").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'd4bc8c94-259c-42ff-9ba9-1e05bd6332b1').
narrative_ontology:cs_kernel_codification('d4bc8c94-259c-42ff-9ba9-1e05bd6332b1', formalized).
narrative_ontology:cs_authority_grounding('d4bc8c94-259c-42ff-9ba9-1e05bd6332b1', lineage).
narrative_ontology:cs_interpretation_layer_present('d4bc8c94-259c-42ff-9ba9-1e05bd6332b1').
narrative_ontology:cs_reading_relation('d4bc8c94-259c-42ff-9ba9-1e05bd6332b1', feudal_oath_reciprocity__lord_extraction_reading, forecloses).
narrative_ontology:cs_reading_relation('d4bc8c94-259c-42ff-9ba9-1e05bd6332b1', feudal_oath_reciprocity__vassal_coordination_reading, influences).
narrative_ontology:cs_axiom('d4bc8c94-259c-42ff-9ba9-1e05bd6332b1', foundational, sacramental_oath_subjects_secular_power).
narrative_ontology:cs_axiom_status(sacramental_oath_subjects_secular_power, holdable).
narrative_ontology:cs_axiom_grounding('d4bc8c94-259c-42ff-9ba9-1e05bd6332b1', sacramental_oath_subjects_secular_power, theological).
narrative_ontology:cs_axiom('d4bc8c94-259c-42ff-9ba9-1e05bd6332b1', foundational, christian_charity_as_extractive_limit).
narrative_ontology:cs_axiom_status(christian_charity_as_extractive_limit, holdable).
narrative_ontology:cs_axiom_grounding('d4bc8c94-259c-42ff-9ba9-1e05bd6332b1', christian_charity_as_extractive_limit, deontological).
narrative_ontology:cs_reference_frame('d4bc8c94-259c-42ff-9ba9-1e05bd6332b1', sacramental_reciprocity_framework).
narrative_ontology:cs_drift_state('d4bc8c94-259c-42ff-9ba9-1e05bd6332b1', rise_of_secular_jurisdiction, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d4bc8c94-259c-42ff-9ba9-1e05bd6332b1', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_peasantry).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, sacramental_oath_doctrine).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, christian_charity_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the sacramental framework of the feudal oath, claims exclusive interpretive authority over its obligations, and enforces compliance through excommunication, interdict, and the threat of soul jeopardy. Gains jurisdictional authority, tithe flows, and the power to adjudicate elite disputes in ecclesiastical courts.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy, beneficiary).

% Swear sacramental oaths to vassals and receive them in return, but are subject to ecclesiastical judgment on whether their extraction violates Christian charity. Face excommunication and loss of sacred legitimacy if they exceed theological limits. Their military and economic power is partially offset by the church's moral monopoly.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords, payer,
    powerful, biographical, constrained, regional).

% Bound to the land and to the lord by oath, but ostensibly protected from unlimited predation by the church's doctrine of charity and sacramental reciprocity. Their protection is real but attenuated by their inability to invoke church courts directly or exit the manorial economy.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_peasantry, beneficiary,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__ecclesiastical_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sacralizes the lord-vassal bond to stabilize post-Carolingian political order where secular state enforcement is absent, creating transcendent reciprocal obligations backed by spiritual sanctions rather than centralized coercion.
% TRANSFER_FUNCTION: Moves interpretive authority over feudal obligations from secular customary law to the ecclesiastical hierarchy, and moves surplus extraction potential away from secular lords toward ecclesiastical legitimacy and limited vassal protection.
% ABSENT_VOICES: Secular jurists advocating Roman or pure customary contract law, heretical movements rejecting sacramental mediation of political bonds, and vassals seeking direct appeal to monarchical rather than ecclesiastical authority are structurally excluded from oath interpretation.
% DISAPPEARANCE_RATIONALE: If the sacramental oath and its ecclesiastical enforcement vanished overnight, feudal relations would lose their primary non-coercive stabilizer; lords would face unchecked reciprocal violence or require costly secular enforcement institutions that did not yet exist, and the church's mediating role would collapse.
% FOUNDING_PROBLEM: How to stabilize armed elite relationships and limit predatory extraction in the absence of a centralized secular state or reliable contract enforcement after the Carolingian collapse.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical chroniclers and canon lawyers attest to the problem of elite violence and the church's peacemaking role. Modern legal historians debate whether church mediation was necessary or whether customary law and comital courts could have stabilized order independently. Secular chroniclers note the violence problem but attribute stabilization to multiple factors, not uniquely ecclesiastical mediation.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint genuinely redistributes power to the church and limits lordly extraction, but it does not reduce lords to pure victims â they retain military and economic dominance. Suppression (0.62) reflects active ecclesiastical enforcement through excommunication, interdict, and pulpit narrative. Theater ratio (0.45) captures the elaborate ritual of oath-taking and the performative dimension of charity that partially substitutes for material limits. Accessibility collapse (0.68) is high because once the sacramental framework is accepted, secular-contract alternatives collapse within the relevant moral economy; resistance (0.45) reflects chronic lordly pushback, including appeals to custom and royal protection.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical seat, the constraint is sacred coordination that pacifies elite violence and protects the weak; from the lordly seat, it is an illegitimate external limit on proprietary power; from the vassal seat, it is partial protection wrapped in paternalistic dependency. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesiastical hierarchy is the structural beneficiary: it gains interpretive authority and the capacity to adjudicate feudal disputes in its own courts. Its directionality sits near the beneficiary pole. Secular lords are the structural victims: the constraint explicitly limits their extractive capacity under threat of spiritual sanction. Their constrained exit and victim status place them near the target pole. Vassal peasantry are diffuse beneficiaries of the limitation, but their trapped status means the coordination benefit reaches them attenuated; they sit nearer the symmetric-to-beneficiary range.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents two errors: (1) reading the church's role as pure coordination (rope) would ignore the asymmetric extraction of interpretive authority and the victimization of lords; (2) reading it as pure snare would ignore the genuine stabilization function the sacramental oath provided in a state-poor environment. The moderate theater ratio acknowledges ritual performance without reducing the constraint to mere theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecclesiastical_extraction_vs_coordination,
    'Does the church''s interpretive authority over the feudal oath function as genuine coordination or as asymmetric extraction of jurisdictional power?',
    'Comparative analysis of jurisdictions where church courts did versus did not adjudicate feudal disputes, measuring whether outcomes were more reciprocal or more favorable to ecclesiastical interests.',
    'If the church extracted more than it coordinated, the effective extractiveness is higher than moderate and the constraint leans toward snare; if coordination dominated, it approaches rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_extraction_vs_coordination, empirical, 'Church authority as coordination or extraction').

omega_variable(
    sacramental_limit_efficacy,
    'Did sacramental oath obligations and the threat of excommunication actually constrain secular lordly extraction, or were they routinely ignored when materially inconvenient?',
    'Manorial record analysis correlating excommunication events with subsequent extraction rates, and comparison of extraction levels across regions with stronger versus weaker ecclesiastical presence.',
    'If the limits were theatrical, theater_ratio should be higher and the victim status of lords questionable; if real, the extraction limit is structurally operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_limit_efficacy, empirical, 'Whether sacramental limits were enforceable or theatrical').

omega_variable(
    kernel_reading_exclusivity,
    'Does the ecclesiastical mediation reading of the feudal oath logically foreclose the lord extraction reading, or do they merely coexist across different partisan frameworks?',
    'Historical analysis of whether any single actor or legal framework simultaneously maintained both sacramental charity limits and maximal extraction authorization without internal contradiction.',
    'If foreclosed, the kernel is a true logical disjunction; if coexistent, the kernel is a strategic ambiguity exploited differently by different parties.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_exclusivity, conceptual, 'Logical relationship between sibling kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feudal_oath_ecc_med_tr_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(feudal_oath_ecc_med_tr_t10, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(feudal_oath_ecc_med_tr_t20, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(feudal_oath_ecc_med_tr_t30, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(feudal_oath_ecc_med_tr_t40, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(feudal_oath_ecc_med_tr_t50, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(feudal_oath_ecc_med_be_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(feudal_oath_ecc_med_be_t10, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(feudal_oath_ecc_med_be_t20, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(feudal_oath_ecc_med_be_t30, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(feudal_oath_ecc_med_be_t40, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(feudal_oath_ecc_med_be_t50, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(feudal_oath_ecc_med_su_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(feudal_oath_ecc_med_su_t10, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(feudal_oath_ecc_med_su_t20, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(feudal_oath_ecc_med_su_t30, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(feudal_oath_ecc_med_su_t40, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement(feudal_oath_ecc_med_su_t50, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, identity_coordination).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is the ecclesiastical_mediation_reading of the feudal_oath_reciprocity kernel. The kernel decomposes into structurally distinct constraints per the epsilon-invariance principle: the lord_extraction_reading exhibits high extractiveness with lords as beneficiaries, while the vassal_coordination_reading exhibits lower extractiveness with reciprocal charter enforcement. This reading centers ecclesiastical interpretive authority and theological limits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
