% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto: Endogenous Reinterpretation via Prophetic Revelation
 *   domain: religious_institutional_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the
 *   endogenous_reinterpretation_reading of the marriage_commitment_reversal
 *   kernel: the 1890 Manifesto and its associated revelatory claims, in which
 *   Wilford Woodruff reported a divine vision reversing the practice of
 *   plural marriage. The reading treats the reversal as an internally
 *   generated doctrinal revision that preserves prophetic authority, as
 *   distinct from an exogenous_override_reading (federal threat as sole
 *   cause) and a practice_doctrine_gap reading (doctrine preserved while
 *   practice suspended). The constraint is the institutionalized pattern of
 *   legitimacy-preserving, endogenous reinterpretation through prophetic
 *   revelation.
 *
 * KEY AGENTS:
 *   - Prophetic leadership: agenda-setter and primary beneficiaryâholds interpretive keys and collects institutional legitimacy.
 *   - General membership: beneficiaryâgains institutional survival and legal compliance at the cost of epistemic flexibility.
 *   - Plural marriage practitioners: payerâbears the costs of dissolved unions and doctrinal abandonment.
 *   - Suppressed dissenters: payerâexcommunicated or marginalized for holding the prior revelation to be eternal.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.55).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.72).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "1890 Manifesto: Endogenous Reinterpretation via Prophetic Revelation").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious_institutional_history/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, '3d906a71-2faf-4087-8773-ec2d72079338').
narrative_ontology:cs_kernel_codification('3d906a71-2faf-4087-8773-ec2d72079338', fixed_text).
narrative_ontology:cs_authority_grounding('3d906a71-2faf-4087-8773-ec2d72079338', lineage).
narrative_ontology:cs_interpretation_layer_present('3d906a71-2faf-4087-8773-ec2d72079338').
narrative_ontology:cs_reading_relation('3d906a71-2faf-4087-8773-ec2d72079338', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d906a71-2faf-4087-8773-ec2d72079338', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('3d906a71-2faf-4087-8773-ec2d72079338', foundational, prophet_can_reverse_binding_commandments).
narrative_ontology:cs_axiom_status(prophet_can_reverse_binding_commandments, holdable).
narrative_ontology:cs_axiom_grounding('3d906a71-2faf-4087-8773-ec2d72079338', prophet_can_reverse_binding_commandments, theological).
narrative_ontology:cs_axiom('3d906a71-2faf-4087-8773-ec2d72079338', foundational, new_revelation_supersedes_prior_practice).
narrative_ontology:cs_axiom_status(new_revelation_supersedes_prior_practice, holdable).
narrative_ontology:cs_axiom_grounding('3d906a71-2faf-4087-8773-ec2d72079338', new_revelation_supersedes_prior_practice, theological).
narrative_ontology:cs_reference_frame('3d906a71-2faf-4087-8773-ec2d72079338', living_prophetic_authority).
narrative_ontology:cs_drift_state('3d906a71-2faf-4087-8773-ec2d72079338', post_manifesto_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3d906a71-2faf-4087-8773-ec2d72079338', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, plural_marriage_practitioners).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppressed_dissenters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the sole institutional right to receive and declare revelation binding on the entire church. In 1890, Woodruff issued the Manifesto claiming divine instruction to cease plural marriages. They maintain that the prophet's authority to reverse prior practices is inherent to the office, and they derive continued legitimacy from the community's acceptance of this revelatory chain.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_leadership, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_leadership, beneficiary).

% Comprises the body of adherents who look to prophetic leadership for doctrinal and practical guidance. They accept the Manifesto as divinely inspired, which allows them to remain in compliance with federal law without abandoning the church. Their religious identity is tied to the institution's survival and the prophet's infallibility claims.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, general_membership, beneficiary,
    organized, biographical, identity_locked, global).

% Entered plural unions in obedience to prior revelations and church instruction. After the Manifesto, they were required to abandon further cohabitation or face church discipline. Many families were disrupted, and they bore the stigma of a practice that the church simultaneously disavowed and historically required.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, plural_marriage_practitioners, payer,
    moderate, biographical, trapped, regional).

% Continued to regard plural marriage as an essential, unchangeable doctrine after the Manifesto. They were excommunicated, driven into hiding, or forced to separate from the main body. Their objections to the reversal were systematically excluded from official church councils and historical records.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppressed_dissenters, payer,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective survival of the church under existential federal threat by providing a single authoritative mechanism to reverse practice without requiring doctrinal repudiation or institutional schism.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy to the prophetic office to supersede prior commandments; transfers the concrete costs of marital dissolution, excommunication, and epistemic dissonance onto practitioners and dissenters.
% ABSENT_VOICES: Plural wives and children of dissolved unions had no representation in the revelatory council; suppressed dissenters who held the prior revelation to be eternal were excluded from official deliberations; federal authorities who demanded explicit legal compliance were kept at arm's length by the mystified revelation narrative.
% DISAPPEARANCE_RATIONALE: If the endogenous revelatory framing vanished, the church would have had to either openly capitulate to federal powerâseverely damaging prophetic legitimacyâor defy the government and face destruction. The specific arrangement of legitimacy-preserving reversal is what prevented mass schism in 1890.
% FOUNDING_PROBLEM: Federal anti-polygamy enforcement in the 1880s threatened the church with property seizure, corporate dissolution, and imprisonment of leaders, while the church was theologically committed to plural marriage as a divine principle.
% FOUNDING_PROBLEM_CORROBORATION: Federal congressional records, the Edmunds-Tucker Act, and Supreme Court rulings (Reynolds v. United States, Late Corporation v. United States) corroborate the external pressure from outside the church. Post-Manifesto private correspondence among apostles (e.g., Woodruff, Cannon) corroborates the survival motive from inside the benefiting party, but independent historical scholarship (Quinn, Hardy) attests the problem's resolution by 1904.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the reversal solved a genuine existential coordination problem but concentrated severe costs on specific adherents. Suppression is high (0.72) because maintaining the revelatory narrative required active disciplining of dissenters, erasure of underground practice, and homogenization of public memory. Theater ratio is moderate-high (0.45) because the performance of unanimity and revelatory certainty was substantialâespecially in the decade after 1890âbut did not fully displace a real institutional survival function. Accessibility collapse (0.6) reflects that alternatives such as schism, open strategic capitulation, or doctrinal repudiation were rendered nearly inaccessible by identity-locked membership and institutional loyalty. Resistance (0.4) captures the fragmented but real opposition from hardliners and practitioners that was systematically suppressed rather than accommodated. Measurements are aligned on a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the prophetic leadership's seat, the arrangement is legitimate revelation that saved the church and preserved divine authority; from the practitioner and dissenter seats, it is an abandonment of sacred covenants that extracted marital stability, social standing, and theological coherence from those who had sacrificed for the prior commandment. The engine computes this divergence from the structural role and exit dataâno reconciliation is authored.
 *
 * DIRECTIONALITY LOGIC:
 *   Prophetic leadership sits near full beneficiary (low d): they gain legitimacy, authority, and institutional continuity. General membership sits mid-range: they benefit from survival but pay diffuse cognitive costs. Plural marriage practitioners and suppressed dissenters sit near full target (high d): they bear the direct costs of reversal, exclusion, and family dissolution. No directionality overrides are required because the structural derivation from beneficiary/victim declarations and exit options captures the relationship accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâexistential federal threatâwas resolved by the early twentieth century. The constraint persists, however, as a reusable template for subsequent legitimacy-preserving reversals (most notably the 1978 priesthood extension). It has not atrophied into a piton because the coordination functionâadaptive revelation that prevents schism during doctrinal changeâremains actively invoked. The theater ratio is elevated but not dominant; the mechanism is still functionally deployed rather than merely performed. Thus, while the original mandate is dead, the constraint transformed into a generalized institutional capacity rather than decaying into inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_epistemic_status,
    'Was Woodruff''s September 23 vision a supernatural communication, a sincerely believed but internally generated psychological resolution, or a deliberate strategic construction?',
    'Historiographical analysis of Woodruff''s diaries, contemporary eyewitness accounts, and psychological profiling of decision-making under extreme institutional threat.',
    'If the vision was strategic fabrication, effective extraction rises sharply because the cost of the reversal was imposed on membership through deception; if sincerely believed, the coordination function is stronger and the constraint''s legitimacy claim is more defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_epistemic_status, empirical, 'Epistemic nature of the founding revelatory experience').

omega_variable(
    kernel_reading_cohabitation,
    'Can a single adherent simultaneously hold the endogenous reinterpretation reading and the exogenous override reading without logical contradiction?',
    'Analysis of official church curriculum, correlated instructional materials, and survey of folk theological positions held by contemporary believers.',
    'If the readings are mutually exclusive within a single framework, the kernel is structurally fractured and the constraint''s long-term stability depends on suppressing one reading; if co-holdable, the constraint operates through distributed ambiguity that lowers effective resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_cohabitation, conceptual, 'Logical coexistence of endogenous and exogenous readings within a single believer').

omega_variable(
    doctrine_practice_logical_status,
    'Is the suspension of plural marriage practice logically compatible with the eternal status of Section 132, or does it constitute a tacit repudiation of the prior axiom?',
    'Formal analysis of Mormon theological semantics distinguishing eternal principle from temporal practice, including official apologetics and critical theological scholarship.',
    'If the suspension is logically incompatible with the eternal claim, the extraction from theological consistency is higher than the base metric suggests; if compatible, the endogenous reading is structurally coherent and the victim set may be smaller.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_logical_status, conceptual, 'Logical compatibility of practice suspension with eternal doctrinal status').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(marr_tr_t4, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(marr_tr_t8, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 8, 0.45).
narrative_ontology:measurement(marr_tr_t12, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(marr_tr_t16, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(marr_be_t4, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(marr_be_t8, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(marr_be_t12, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(marr_be_t16, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marr_su_t4, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(marr_su_t8, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(marr_su_t12, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(marr_su_t16, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_commitment_reversal kernel. The kernel decomposes into three structurally distinct constraints because the same historical event (the 1890 Manifesto) supports three incompatible empirical and normative claims about causation, legitimacy, and doctrinal status. Each reading carries a different epsilon, different beneficiary/victim structure, and different classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
