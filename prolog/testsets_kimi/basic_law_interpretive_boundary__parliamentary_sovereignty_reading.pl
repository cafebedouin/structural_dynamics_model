% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Knesset Parliamentary Sovereignty over Basic Laws
 *   domain: constitutional_law/comparative_constitutionalism
 *
 * SUMMARY:
 *   The Knesset as elected sovereign claims ultimate authority to interpret
 *   and amend Basic Laws via simple majority, including the power to override
 *   judicial review. This constraint instantiates the parliamentary
 *   sovereignty reading of the basic_law_interpretive_boundary kernel. It
 *   treats the Knesset majority as the final check in the constitutional
 *   order, rendering the Supreme Court advisory on Basic Law questions and
 *   leaving individual rights dependent on legislative grace. The constraint
 *   is contested: the Court and civil society read it as dismantling
 *   constitutional checks, while the coalition reads it as restoring
 *   democratic supremacy.
 *
 * KEY AGENTS:
 *   - knesset_majoritarian_coalition: Primary beneficiary and agenda-setter (institutional/arbitrage) â captures unconstrained legislative and constituent power
 *   - supreme_court: Primary target (institutional/constrained) â loses binding constitutional interpretive authority
 *   - rights_bearing_minorities: Secondary target (powerless/trapped) â lose judicial forum to challenge majoritarian legislation
 *   - civil_society_legal_advocates: Excluded voice (organized/constrained) â structurally excluded from effective constitutional advocacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.55).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.72).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Knesset Parliamentary Sovereignty over Basic Laws").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional_law/comparative_constitutionalism").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '2f821686-62b8-4faa-ab19-7f76725cf51c').
narrative_ontology:cs_kernel_codification('2f821686-62b8-4faa-ab19-7f76725cf51c', formalized).
narrative_ontology:cs_authority_grounding('2f821686-62b8-4faa-ab19-7f76725cf51c', lineage).
narrative_ontology:cs_reading_relation('2f821686-62b8-4faa-ab19-7f76725cf51c', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('2f821686-62b8-4faa-ab19-7f76725cf51c', basic_law_interpretive_boundary__balanced_contestation_reading, forecloses).
narrative_ontology:cs_axiom('2f821686-62b8-4faa-ab19-7f76725cf51c', foundational, knesset_constituent_authority).
narrative_ontology:cs_axiom_status(knesset_constituent_authority, holdable).
narrative_ontology:cs_axiom_grounding('2f821686-62b8-4faa-ab19-7f76725cf51c', knesset_constituent_authority, conventional).
narrative_ontology:cs_axiom('2f821686-62b8-4faa-ab19-7f76725cf51c', foundational, judicial_review_advisory_only).
narrative_ontology:cs_axiom_status(judicial_review_advisory_only, holdable).
narrative_ontology:cs_axiom_grounding('2f821686-62b8-4faa-ab19-7f76725cf51c', judicial_review_advisory_only, conventional).
narrative_ontology:cs_reference_frame('2f821686-62b8-4faa-ab19-7f76725cf51c', parliamentary_sovereignty_framework).
narrative_ontology:cs_drift_state('2f821686-62b8-4faa-ab19-7f76725cf51c', contemporary_constitutional_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2f821686-62b8-4faa-ab19-7f76725cf51c', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majoritarian_coalition).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, rights_bearing_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the Knesset majority and government coalition, enabling passage and amendment of Basic Laws by simple majority. Can enact override clauses to negate judicial review and claims ultimate constituent authority over constitutional interpretation. Exit is unrestricted because the coalition can alter the legal framework it inhabits.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majoritarian_coalition, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majoritarian_coalition, beneficiary).

% Sits as the highest judicial instance but is rendered advisory on Basic Law questions by the Knesset's asserted override authority. Can hear petitions and issue rulings, yet the coalition may legislate around or through them. Exit is constrained because the court lacks enforcement mechanisms against a sovereign legislature asserting supremacy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court, payer,
    institutional, generational, constrained, national).

% Includes ethnic, religious, or political minorities whose fundamental rights protections depend on judicial review of legislation. With judicial override available, their ability to block discriminatory laws through the courts collapses. Exit is trapped because emigration is costly and domestic political channels are majoritarian.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, rights_bearing_minorities, payer,
    powerless, biographical, trapped, national).

% Comprises human rights organizations and public-interest lawyers who previously channeled constitutional challenges through the Supreme Court. Their advocacy is now structurally excluded from authoritative effect when the Knesset can override adverse rulings. Exit is constrained to protest and international forums.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, civil_society_legal_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majoritarian_coalition).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves inter-branch constitutional uncertainty by assigning final interpretive and amendment authority to the elected legislature, preventing deadlock between courts and parliament over Basic Law meaning.
% TRANSFER_FUNCTION: Moves constitutional interpretive authority and final amendment power from the judiciary and dispersed rights-bearing populations to the Knesset majority coalition.
% ABSENT_VOICES: Judicial dissenters and minority rights communities who would argue for enforceable constitutional limits on majority power are formally present in court but structurally excluded from effective constitutional review once judicial override is permitted.
% DISAPPEARANCE_RATIONALE: If the Knesset's claimed absolute sovereignty disappeared, the Supreme Court's rulings on Basic Laws would become binding, individual rights petitioners would regain standing to block legislation, and the constitutional order would shift from majoritarian supremacy to separated powers with effective judicial review.
% FOUNDING_PROBLEM: The need to establish a clear hierarchy in constitutional interpretation between unelected judges and elected representatives in a system with no single entrenched constitutional document, resolving who has final say over Basic Law meaning.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars outside Israel note that parliamentary sovereignty is a legitimate resolution in some democracies, but the Israeli court and civil society organizations attest the 'problem' was manufactured to consolidate coalition power rather than remedy a genuine institutional deadlock.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects the transfer of constitutional authority from the judiciary and rights-holders to the legislative majority. Suppression (0.72) is high because the constraint's operation requires actively suppressing judicial review as a binding check, substituting advisory opinion for enforceable ruling. Theater ratio (0.45) captures the performative invocation of 'the people' to legitimate coalition-specific agenda items. Accessibility collapse (0.78) is high because once parliamentary sovereignty is accepted, there is no domestic legal alternative to majoritarian will; resistance (0.82) reflects sustained institutional and social pushback from the Court, the bar, and minority communities. Temporal measurements show rising extraction and suppression from t=0 to t=20 as the reading has been increasingly asserted and contested.
 *
 * PERSPECTIVAL GAP:
 *   From the Knesset coalition seat, the constraint appears as a rope â democratic coordination that resolves who has final say and prevents unelected judges from vetoing the popular will. From the Supreme Court and rights-bearing minority seats, the identical structure computes as extractive: judicial independence and individual rights protections are stripped away and transferred to a transient legislative majority. The engine derives this divergence from the structural asymmetry in beneficiary and victim declarations and exit options, not from narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The Knesset majoritarian coalition is the declared beneficiary and agenda-setter; its directionality sits near the full-beneficiary end (d approximately 0.1), scaling its effective extraction downward into net subsidy â it gains unconstrained authority. The Supreme Court and rights-bearing minorities are declared victims with severely constrained or trapped exit options; their directionality sits near the full-target end (d approximately 0.9), amplifying effective extraction. Civil society legal advocates are excluded, with directionality reflecting their structural irrelevance to the constraint's operation (d approximately 0.8). No override is needed because the structural derivation matches the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists mandatrophy mislabeling because its coordination function is structurally genuine: assigning final constitutional authority to an elected body does solve inter-branch uncertainty and deadlock. However, the Tangled Rope classification is gated by the simultaneous presence of beneficiaries and victims and active enforcement. The Knesset coalition benefits from unconstrained power; the Court and minorities pay through lost protections; and the arrangement requires active legislative enforcement (override laws, procedural changes) to maintain. The founding problem status is contested â whether deadlock ever existed â which feeds the zombie hypothesis without altering the base classification. If the coordination story were cover (no genuine deadlock, pure consolidation), the metrics would trend toward snare; the authored metrics retain a coordination residue that keeps it in tangled territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Does the Basic Law interpretive boundary admit parliamentary sovereignty as the only coherent reading, or do sibling readings (judicial supremacy, balanced contestation) represent structurally defensible alternatives?',
    'Comparative constitutional analysis of how other democracies resolve inter-branch authority, plus historical tracing of Israeli constitutional practice to see which reading better fits institutional evolution.',
    'If sibling readings are structurally coherent, this constraint''s claimed exclusivity is undermined and its extractiveness increases by denying alternative frameworks legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Structural status of sibling readings within the same kernel').

omega_variable(
    majoritarian_coordination_or_extraction,
    'Does assigning ultimate constitutional authority to a simple legislative majority solve a genuine coordination problem (inter-branch deadlock) or primarily extract protective checks from minorities and the judiciary?',
    'Empirical analysis of legislative behavior: whether override powers are used for constitutional clarification or for coalition-specific agenda passage against protected rights.',
    'If used primarily for rights-restricting coalition agenda, the coordination story is cover and the constraint trends toward snare; if used sparingly for genuine deadlock, it remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_coordination_or_extraction, empirical, 'Coordination function versus extraction function of parliamentary sovereignty').

omega_variable(
    international_obligations_residual_constraint,
    'To what extent do international treaty obligations actually constrain Knesset sovereignty when the Knesset retains ultimate interpretive authority over treaty incorporation?',
    'Case study analysis of Knesset treatment of international law in domestic legislation post-override.',
    'If international obligations are routinely overridden or interpreted away, the near-zero epsilon claim fails and the constraint is more extractive than presented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_obligations_residual_constraint, empirical, 'Residual binding force of international law on asserted parliamentary sovereignty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the basic_law_interpretive_boundary kernel. It instantiates parliamentary sovereignty; sibling readings instantiate judicial supremacy and balanced contestation. The epsilon values differ because this reading treats Basic Laws as ordinary legislation subject to simple majority override, while judicial supremacy treats them as higher-order law immune to legislative override, and balanced contestation treats them as partially entrenched. These are structurally distinct constraints, not the same constraint viewed differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
