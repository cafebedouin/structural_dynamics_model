% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__crown_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Remonstrance Authority (Crown Reading)
 *   domain: constitutional/political-economy
 *
 * SUMMARY:
 *   This constraint story instantiates the crown_reading of the
 *   remonstrance_authority kernel: the procedural right of provincial estates
 *   and magistrates to remonstrate against royal edicts is read as an
 *   illegitimate minoritarian veto that extracts fiscal and legislative
 *   capacity from the Crown and protects particularist privileges. The
 *   sibling magistrate_reading reverses the beneficiary-victim structure,
 *   treating the Crown as beneficiary of limited authority and the
 *   magistrates as guardians of ancient liberties. The high extraction and
 *   victimization of the Crown are the structural delta of this reading.
 *
 * KEY AGENTS:
 *   - crown: Primary target (institutional/constrained) â bears extraction as lost fiscal authority and blocked chain of command.
 *   - provincial_estates: Primary agenda-setter and beneficiary (organized/constrained) â administers the remonstrance veto and derives constitutional leverage.
 *   - privilege_orders: Secondary beneficiary (organized/constrained) â collects economic rents from shielded immunities.
 *   - absolutist_jurists: Analytical observer (analytical/analytical) â provides doctrinal coherence for the Crown reading from outside the beneficiary coalition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.84).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.76).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Remonstrance Authority (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional/political-economy").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, 'ecdfffb4-d09b-4672-87c3-52f791677df2').
narrative_ontology:cs_kernel_codification('ecdfffb4-d09b-4672-87c3-52f791677df2', fixed_text).
narrative_ontology:cs_authority_grounding('ecdfffb4-d09b-4672-87c3-52f791677df2', lineage).
narrative_ontology:cs_interpretation_layer_present('ecdfffb4-d09b-4672-87c3-52f791677df2').
narrative_ontology:cs_reading_relation('ecdfffb4-d09b-4672-87c3-52f791677df2', remonstrance_authority__magistrate_reading, forecloses).
narrative_ontology:cs_axiom('ecdfffb4-d09b-4672-87c3-52f791677df2', foundational, particularist_veto_usurps_sovereignty).
narrative_ontology:cs_axiom_status(particularist_veto_usurps_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('ecdfffb4-d09b-4672-87c3-52f791677df2', particularist_veto_usurps_sovereignty, conventional).
narrative_ontology:cs_axiom('ecdfffb4-d09b-4672-87c3-52f791677df2', foundational, undivided_fiscal_authority_necessary_for_state).
narrative_ontology:cs_axiom_status(undivided_fiscal_authority_necessary_for_state, holdable).
narrative_ontology:cs_axiom_grounding('ecdfffb4-d09b-4672-87c3-52f791677df2', undivided_fiscal_authority_necessary_for_state, instrumental).
narrative_ontology:cs_reference_frame('ecdfffb4-d09b-4672-87c3-52f791677df2', undivided_royal_prerogative).
narrative_ontology:cs_drift_state('ecdfffb4-d09b-4672-87c3-52f791677df2', remonstrance_consolidation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ecdfffb4-d09b-4672-87c3-52f791677df2', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, provincial_estates).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, privilege_orders).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, crown).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds formal sovereignty over legislation and taxation; every remonstrance blocks registration of edicts and forces costly concessions or coercive escalation; bears the extraction as lost revenue, delayed reform, and compromised chain of command; exit from the constraint requires constitutional rupture or civil war.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown, payer,
    institutional, generational, constrained, national).

% Provincial and corporate assemblies that register royal edicts and formalize remonstrances; they derive political existence and bargaining leverage from the veto power; they enforce the constraint by refusing registration and mobilizing legal precedent; exit would mean surrendering constitutional status to simple royal subjects.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, provincial_estates, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__crown_reading, provincial_estates, beneficiary).

% Noble, clerical, and municipal corporations whose tax immunities and local customs are shielded by the remonstrance veto; they receive the direct economic benefit of non-compliance with uniform law; they support the constraint financially and politically but do not administer its procedure.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, privilege_orders, beneficiary,
    organized, biographical, constrained, regional).

% Legal theorists and royal counselors who argue that remonstrance is usurpation of sovereignty; they observe the structural asymmetry from outside the beneficiary coalition and provide the Crown reading with doctrinal coherence.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, absolutist_jurists, observer,
    analytical, generational, analytical, national).

narrative_ontology:fixing_cost_class(remonstrance_authority__crown_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a procedural mechanism for subordinate corporate bodies to register formal opposition to royal edicts before enforcement, ostensibly ensuring deliberative synchronization between central legislation and local consent.
% TRANSFER_FUNCTION: Moves fiscal and legislative authority from the Crown to provincial estates and particularist corporations by granting a veto over registration and implementation of edicts.
% ABSENT_VOICES: Royal bureaucratic intendants and taxpayers outside privileged orders are structurally excluded; they would argue for uniform law and efficient fiscal extraction but lack standing in the remonstrance procedure.
% DISAPPEARANCE_RATIONALE: If the remonstrance authority vanished, royal edicts would proceed directly to implementation without procedural veto; provincial tax immunities and corporate privileges would lose their constitutional shield; the fiscal-military state would reorganize around unitary authority.
% FOUNDING_PROBLEM: Unchecked royal innovation in taxation and law threatened local customary rights and corporate charters; absence of mechanism to compel deliberation before enforcement.
% FOUNDING_PROBLEM_CORROBORATION: Royalist and absolutist jurists outside the beneficiary estates attest that the founding problem of arbitrary royal overreach was resolved by the consolidation of sovereign legitimacy and that the remonstrance now functions as atrophied obstruction; no corroboration from non-beneficiary sources supports the continued live status of the problem.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__crown_reading, 0.84, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__crown_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__crown_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__crown_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.84) because the veto systematically blocks central fiscal authority and preserves non-uniform privilege against general law. Suppression is high (0.76) because the constraint's persistence depends on suppressing royal absolutism as an alternative institutional form and on delegitimizing unitary fiscal command. Theater is moderate (0.42) because remonstrance is performed with elaborate legal formalism and constitutional rhetoric that obscures its distributive function. Resistance is high (0.72) because the Crown continuously contests the veto through litigation, propaganda, and coercive escalation. The measurement series run on a single shared time grid and show extraction and enforcement hardening as fiscal-military demands intensify.
 *
 * PERSPECTIVAL GAP:
 *   The Crown seat experiences the constraint as pure obstruction and extraction (high d, high effective Ï), while the provincial estates and privilege orders experience it as legitimate constitutional protection (low d, subsidy-like structural position). The absolutist jurist seat sees the full asymmetry. The engine computes this divergence from the structural data; the authored snare claim does not adjudicate the magistrate reading's competing framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown is declared victim because the constraint extracts fiscal and legislative capacity from itâevery remonstrance transfers authority from the center to provincial bodies. Provincial estates are declared agenda-setter/beneficiary because they administer the veto and derive political existence from it. Privilege orders are declared beneficiaries because they capture the economic rent of non-compliance. The structural derivation assigns the Crown a high directionality and the estate beneficiaries low directionality; no override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as snare rather than tangled_rope prevents mislabeling an atrophied coordination mechanism as still functional. The R5 genealogy shows the founding problem (arbitrary royal overreach) is dead, corroborated by non-beneficiary absolutist jurists, while the disappearance verdict is world_rearranges. The mismatch (dead founding problem + rearranging disappearance) flags the constraint as a zombie mechanism whose persistence serves extraction rather than coordination. Were the coordination function genuine and live, the reading would compute toward tangled_rope; the mandate's obsolescence pushes the classification toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remonstrance_kernel_reading_location,
    'Is the remonstrance authority a crown-reading snare (illegitimate particularist veto extracting from the Crown) or a magistrate-reading rope/scaffold (legitimate constitutional safeguard preserving ancient liberties)?',
    'Historical-structural analysis of whose privileges are actually protected, whose authority is blocked, and whether the procedure coordinates genuine consent or merely extracts rents.',
    'Resolving this omega would invert the beneficiary-victim structure and reclassify the constraint from snare to coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remonstrance_kernel_reading_location, conceptual, 'Kernel reading ambiguity between crown and magistrate framings of the same procedural authority.').

omega_variable(
    sovereignty_foreclosure_ambiguity,
    'Does the absolutist premise of the crown reading logically foreclose the magistrate reading''s ancient-liberties framing, or can both coexist as live constitutional theories within a single framework?',
    'Analysis of whether any single legal framework has ever simultaneously held both that remonstrance is illegitimate usurpation and that it is a fundamental constitutional mechanism.',
    'If foreclosed, the two readings instantiate mutually exclusive constraints; if coexisting, they are competing framings whose divergence is perspectival rather than logical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_foreclosure_ambiguity, conceptual, 'Whether the crown reading logically forecloses the magistrate reading.').

omega_variable(
    fiscal_authority_extraction_verification,
    'Does the remonstrance right actually block general-welfare fiscal policy, or does it only block particular impositions while leaving sufficient central authority intact?',
    'Comparative fiscal history comparing revenue extraction and policy implementation in jurisdictions with and without remonstrance rights, controlling for war, geography, and administrative capacity.',
    'If remonstrance only blocks particularist-targeted impositions, the Crown''s victim status and the high epsilon are weakened; if it systematically blocks general fiscal capacity, the extraction reading is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_authority_extraction_verification, empirical, 'Empirical test of whether remonstrance structurally blocks general fiscal authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remonstrance_crown_tr_t0, remonstrance_authority__crown_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(remonstrance_crown_tr_t8, remonstrance_authority__crown_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(remonstrance_crown_tr_t16, remonstrance_authority__crown_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(remonstrance_crown_tr_t24, remonstrance_authority__crown_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(remonstrance_crown_tr_t32, remonstrance_authority__crown_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(remonstrance_crown_tr_t40, remonstrance_authority__crown_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(remonstrance_crown_be_t0, remonstrance_authority__crown_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(remonstrance_crown_be_t8, remonstrance_authority__crown_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(remonstrance_crown_be_t16, remonstrance_authority__crown_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(remonstrance_crown_be_t24, remonstrance_authority__crown_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(remonstrance_crown_be_t32, remonstrance_authority__crown_reading, base_extractiveness, 32, 0.8).
narrative_ontology:measurement(remonstrance_crown_be_t40, remonstrance_authority__crown_reading, base_extractiveness, 40, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(remonstrance_crown_su_t0, remonstrance_authority__crown_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(remonstrance_crown_su_t8, remonstrance_authority__crown_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(remonstrance_crown_su_t16, remonstrance_authority__crown_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(remonstrance_crown_su_t24, remonstrance_authority__crown_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(remonstrance_crown_su_t32, remonstrance_authority__crown_reading, suppression_requirement, 32, 0.73).
narrative_ontology:measurement(remonstrance_crown_su_t40, remonstrance_authority__crown_reading, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, remonstrance_authority__magistrate_reading).

% DUAL FORMULATION NOTE:
% The remonstrance authority kernel decomposes into two structurally distinct constraints: the crown_reading (high extraction from Crown, magistrates as beneficiaries) and the magistrate_reading (coordination function preserving liberties, Crown as agenda-setter/beneficiary). The two readings invert the directionality structure, beneficiary/victim sets, and claimed type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
