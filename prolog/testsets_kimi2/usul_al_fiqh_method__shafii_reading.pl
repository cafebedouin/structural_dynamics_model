% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__shafii_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__shafii_reading
 *   human_readable: Shafi'i Reading of Usul al-Fiqh: Hadith-Prerequisite Legal Method
 *   domain: legal/religious/comparative
 *
 * SUMMARY:
 *   This constraint instantiates the Shafi'i reading of the usul al-fiqh
 *   kernel: a systematized legal methodology in which authenticated hadith
 *   serves as a prerequisite to legal derivation, qiyas is permitted only in
 *   the absence of authenticated text, and ijma is restricted to the
 *   consensus of the Prophet's Companions. The arrangement coordinates legal
 *   derivation by fixing source hierarchy, but simultaneously extracts
 *   authority from rationalist jurists and transfers gatekeeping power to
 *   hadith transmission specialists. As a kernel reading, it is one of four
 *   structurally distinct classical legal methodologies; this file isolates
 *   the Shafi'i instantiation and its specific beneficiary/victim structure.
 *
 * KEY AGENTS:
 *   - hadith_transmission_specialists (institutional/arbitrage): Primary agenda-setters and beneficiaries who control the authentication prerequisite and enforce textual gatekeeping.
 *   - rationalist_jurists (organized/constrained): Primary payers whose analogical and reasoned methods are subordinated to hadith authentication; authority constrained by textual gatekeeping.
 *   - textualist_jurists (organized/mobile): Beneficiaries who gain methodological alignment from a hierarchy that prioritizes Qur'an and authenticated hadith over unaided reason.
 *   - comparative_legal_theorists (analytical/analytical): External observers who analyze the source hierarchy without participating in its operation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.62).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.58).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i Reading of Usul al-Fiqh: Hadith-Prerequisite Legal Method").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "legal/religious/comparative").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, '80316f2e-c34e-474f-9b53-26ee3150a856').
narrative_ontology:cs_kernel_codification('80316f2e-c34e-474f-9b53-26ee3150a856', fixed_text).
narrative_ontology:cs_authority_grounding('80316f2e-c34e-474f-9b53-26ee3150a856', lineage).
narrative_ontology:cs_interpretation_layer_present('80316f2e-c34e-474f-9b53-26ee3150a856').
narrative_ontology:cs_reading_relation('80316f2e-c34e-474f-9b53-26ee3150a856', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('80316f2e-c34e-474f-9b53-26ee3150a856', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('80316f2e-c34e-474f-9b53-26ee3150a856', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('80316f2e-c34e-474f-9b53-26ee3150a856', foundational, authenticated_hadith_prerequisite).
narrative_ontology:cs_axiom_status(authenticated_hadith_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('80316f2e-c34e-474f-9b53-26ee3150a856', authenticated_hadith_prerequisite, conventional).
narrative_ontology:cs_axiom('80316f2e-c34e-474f-9b53-26ee3150a856', foundational, companions_consensus_bound).
narrative_ontology:cs_axiom_status(companions_consensus_bound, holdable).
narrative_ontology:cs_axiom_grounding('80316f2e-c34e-474f-9b53-26ee3150a856', companions_consensus_bound, conventional).
narrative_ontology:cs_reference_frame('80316f2e-c34e-474f-9b53-26ee3150a856', prophetic_textual_supremacy).
narrative_ontology:cs_drift_state('80316f2e-c34e-474f-9b53-26ee3150a856', post_formative_school_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('80316f2e-c34e-474f-9b53-26ee3150a856', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, textualist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, rationalist_jurists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the authentication of prophetic reports through isnad criticism and biographical evaluation. Their certification is a prerequisite before any hadith may serve as evidence in legal derivation. They teach, transmit, and enforce methodological standards that determine which texts enter the legal corpus, and their scholarly networks span multiple legal jurisdictions.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, beneficiary).

% Derive law through analogical reasoning, juristic preference, and reasoned opinion. Under this framework their methods are subordinated to hadith authentication; they may not rely on qiyas or ra'y when an authenticated hadith addresses the question. Their authority is constrained by the textual gatekeeping of the hadith specialists, and exit to other methodological frameworks requires abandoning the Shafi'i system.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, rationalist_jurists, payer,
    organized, biographical, constrained, global).

% Derive law primarily from Qur'an and authenticated hadith. They benefit from a source hierarchy that prioritizes textual evidence over unaided reason, aligning their methodology with the authentication standards enforced by the hadith scholars. Their authority increases relative to rationalist jurists within this framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, textualist_jurists, beneficiary,
    organized, generational, mobile, global).

% Study the Shafi'i usul system from an external analytical position, comparing its source hierarchy with those of other legal traditions and schools. They document how authentication prerequisites redistribute authority among scholar classes without participating in the constraint's operation as beneficiaries or payers.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, comparative_legal_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a hierarchical, systematized method for deriving legal rulings from revealed sources, resolving the problem of ad hoc or regionally inconsistent legal reasoning by fixing the order of Qur'an, authenticated hadith, consensus of the Companions, and analogy.
% TRANSFER_FUNCTION: Transfers gatekeeping authority over legal evidence from general jurists to hadith authentication specialists; moves rationalist jurists from equal participants in source derivation to subordinate users of authenticated texts.
% ABSENT_VOICES: Hanafi jurists who hold expansive qiyas and istihsan as independently valid sources; Maliki jurists who accord independent weight to Medinan practice and unrestricted public interest; they are excluded from the Shafi'i methodological framework though they remain live positions in the broader legal field.
% DISAPPEARANCE_RATIONALE: If the hadith-authentication prerequisite and the usul hierarchy vanished, rationalist jurists would regain ungated authority to use qiyas and ra'y, hadith specialists would lose their prerequisite gatekeeping role, and the systematic order of legal sources would collapse into competing regional methods.
% FOUNDING_PROBLEM: Early Islamic jurisprudence lacked a unified method for resolving conflicts between Qur'an, prophetic reports, consensus, and regional reasoning; legal derivation was fragmented across regional schools with inconsistent source hierarchies, producing uncertainty and contradictory rulings.
% FOUNDING_PROBLEM_CORROBORATION: Comparative legal historians and non-Shafi'i jurists attest that source inconsistency was a genuine problem, but dispute whether the Shafi'i solutionâsubordinating all sources to hadith authenticationâwas necessary; Hanafi and Maliki traditions from outside the benefiting parties preserve alternative resolutions that were live at the founding.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__shafii_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the constraint transfers gatekeeping authority to a specialized class and subordinates rationalist legal methods. Suppression (0.58) is moderate-high: the framework does not eliminate rationalist methods entirely but structurally subordinates them, requiring active enforcement of the authentication prerequisite. Theater ratio (0.38) reflects moderate performative maintenance of authentication rituals and isnad evaluation that partly serve scholarly status preservation alongside genuine epistemic work. Accessibility collapse (0.68) is high because once the usul framework is accepted, alternative source hierarchies become methodologically illegitimate within the system. Resistance (0.42) is moderate: rationalist jurists resist the subordination, but the framework's systematization provides real coordination value that dampens outright rejection.
 *
 * PERSPECTIVAL GAP:
 *   The hadith specialist seat experiences the constraint as genuine coordination that protects the legal tradition from arbitrary reasoning, with low effective extraction. The rationalist jurist seat experiences the same structure as active extraction that bars unauthenticated rational methods and confines their authority to textual lacunae. The engine computes this divergence from the structural data: same constraint, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith transmission specialists are structurally situated at the beneficiary end: they control the authentication gate, have high mobility between scholarly networks, and collect methodological authority. Rationalist jurists are structurally situated at the target end: they bear the cost of subordination, have constrained exit within the Shafi'i framework, and lose source authority. Textualist jurists share the beneficiary side with hadith specialists. Comparative theorists occupy an analytical seat with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by preserving a genuine coordination function: early Islamic law did face a real problem of source inconsistency, and systematization solved it. A snare reading would ignore this and treat the constraint as pure extraction; a rope reading would ignore the asymmetric gatekeeping benefit to hadith specialists and the subordination of rationalist jurists. The tangled-rope classification captures both the real coordination and the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authentication_epistemic_or_social,
    'Is the hadith-authentication prerequisite driven primarily by epistemic reliability concerns, or by the socio-professional interest of a specialized scholar class in maintaining gatekeeping authority?',
    'Comparative historical analysis of authentication standards across schools: if authentication criteria tighten in proportion to hadith scholars'' institutional power and relax when that power weakens, the social hypothesis is supported; if they track independently verifiable reliability indicators, the epistemic hypothesis is supported.',
    'If primarily social, the constraint''s extractiveness is higher than its coordination function suggests, pushing classification toward snare; if primarily epistemic, the coordination function dominates and the tangled-rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentication_epistemic_or_social, empirical, 'Whether authentication gates serve knowledge or professional gatekeeping.').

omega_variable(
    kernel_reading_sibling_displacement,
    'Does the Shafi''i reading foreclose the Hanafi rationalist reading within a single legal framework, or do the readings merely coexist as distinct school commitments?',
    'Analysis of whether a single jurist can simultaneously hold that hadith authentication is an absolute prerequisite and that qiyas with istihsan may override or bypass authenticated textual evidence.',
    'If foreclosed, the kernel generates logically incompatible constraints and the engine should register a forecloses relation; if coexisting, the relation remains coexists_with and the kernel is a distributed authority system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_displacement, conceptual, 'Structural relationship between Shafi''i and Hanafi readings of the usul kernel.').

omega_variable(
    ijma_companions_restriction,
    'Is the restriction of ijma to Companions'' consensus a historically recoverable feature of early practice, or a retroactive methodological construct that limits later jurists'' legislative authority?',
    'Historical jurisprudential archaeology: examination of early adjudication records for evidence that non-Companion consensus was treated as binding before the usul systematization.',
    'If retroactive construct, the restriction is an extraction mechanism limiting subsequent jurists'' authority; if historically grounded, it is a coordination feature preserving textual fidelity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ijma_companions_restriction, empirical, 'Historical versus constructed status of Companion-only ijma.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__shafii_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__shafii_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__shafii_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(usul_tr_t60, usul_al_fiqh_method__shafii_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement(usul_tr_t80, usul_al_fiqh_method__shafii_reading, theater_ratio, 80, 0.36).
narrative_ontology:measurement(usul_tr_t100, usul_al_fiqh_method__shafii_reading, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__shafii_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__shafii_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__shafii_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(usul_be_t60, usul_al_fiqh_method__shafii_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(usul_be_t80, usul_al_fiqh_method__shafii_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(usul_be_t100, usul_al_fiqh_method__shafii_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__shafii_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__shafii_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__shafii_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(usul_su_t60, usul_al_fiqh_method__shafii_reading, suppression_requirement, 60, 0.56).
narrative_ontology:measurement(usul_su_t80, usul_al_fiqh_method__shafii_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement(usul_su_t100, usul_al_fiqh_method__shafii_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, identity_coordination).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The usul_al_fiqh_method kernel decomposes into four structurally distinct readings (Shafi'i, Hanafi, Maliki, Hanbali) because each assigns different epsilon values to the hierarchy of legal sources and distributes authority differently among scholar classes. This story isolates the Shafi'i reading; siblings are linked as parallel constraints within the same family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
