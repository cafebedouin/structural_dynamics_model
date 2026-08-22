% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: Federal Override of Plural Marriage Doctrine (Exogenous Coercion Reading)
 *   domain: religious/political/institutional
 *
 * SUMMARY:
 *   The Manifesto of 1890 declared the suspension of plural marriage doctrine
 *   among the LDS Church in response to federal anti-polygamy legislation,
 *   court seizures of church property, and threat of institutional
 *   dissolution. This reading interprets the Manifesto as federal coercion
 *   forcing institutional capitulation: doctrine remains theologically true,
 *   but practice is suspended under duress. The LDS membership becomes the
 *   victim, bearing the doctrinal contradiction between the claim that plural
 *   marriage is eternally binding and its sudden institutional suspension.
 *   The federal government is the structural beneficiary, achieving
 *   suppression of religious practice through pressure on institutional
 *   leadership without constitutional amendment. Theater ratio rises over the
 *   interval as the Manifesto becomes increasingly framed as prophetic
 *   revelation rather than coerced capitulation—the institutional account of
 *   why practice was suspended transforms, while the underlying extraction
 *   (compliance extracted through legal and economic pressure) remains.
 *
 * KEY AGENTS:
 *   - Federal government: institutional beneficiary, extracting compliance and institutional subordination through legal jurisdiction
 *   - LDS polygamist practitioners: moderate-power payers with identity-locked exit, facing imprisonment or doctrinal abandonment
 *   - LDS membership collectively: organized payer bearing doctrinal contradiction and institutional legitimacy crisis
 *   - Polygamist wives and children: powerless victims facing legal exposure and kinship rupture
 *   - LDS prophetic authority: institutional agenda-setter holding constrained exit, preserving institutional survival at cost of visible doctrine-practice gap
 *   - Resistance cells: excluded voice, explicitly rejected as apostate for maintaining the doctrine the Manifesto suspends
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.79).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.87).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "Federal Override of Plural Marriage Doctrine (Exogenous Coercion Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious/political/institutional").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, 'b3dc35ca-ad09-41b2-8d4a-c9f746be32a2').
narrative_ontology:cs_kernel_codification('b3dc35ca-ad09-41b2-8d4a-c9f746be32a2', fixed_text).
narrative_ontology:cs_authority_grounding('b3dc35ca-ad09-41b2-8d4a-c9f746be32a2', extraction).
narrative_ontology:cs_interpretation_layer_present('b3dc35ca-ad09-41b2-8d4a-c9f746be32a2').
narrative_ontology:cs_reading_relation('b3dc35ca-ad09-41b2-8d4a-c9f746be32a2', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('b3dc35ca-ad09-41b2-8d4a-c9f746be32a2', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('b3dc35ca-ad09-41b2-8d4a-c9f746be32a2', foundational, doctrine_truth_independent_of_practice).
narrative_ontology:cs_axiom_status(doctrine_truth_independent_of_practice, holdable).
narrative_ontology:cs_axiom_grounding('b3dc35ca-ad09-41b2-8d4a-c9f746be32a2', doctrine_truth_independent_of_practice, deontological).
narrative_ontology:cs_axiom('b3dc35ca-ad09-41b2-8d4a-c9f746be32a2', foundational, federal_authority_overrides_religious_institution).
narrative_ontology:cs_axiom_status(federal_authority_overrides_religious_institution, holdable).
narrative_ontology:cs_axiom_grounding('b3dc35ca-ad09-41b2-8d4a-c9f746be32a2', federal_authority_overrides_religious_institution, empirically_contingent).
narrative_ontology:cs_reference_frame('b3dc35ca-ad09-41b2-8d4a-c9f746be32a2', doctrine_integrity_uncompromised_by_suspension).
narrative_ontology:cs_drift_state('b3dc35ca-ad09-41b2-8d4a-c9f746be32a2', post_manifesto_institutional_theology, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b3dc35ca-ad09-41b2-8d4a-c9f746be32a2', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_polygamist_practitioners).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership_collectively).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, anti_polygamy_coalition).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, polygamist_wives_and_children).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_prophetic_authority).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__exogenous_override_reading, federal_jurisdiction_over_religious_practice).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__exogenous_override_reading, state_monopoly_on_marriage_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses the Manifesto as evidence of institutional capitulation, achieving prohibition of plural marriage without constitutional amendment. Sets the terms: apostasy or compliance. Collects political legitimacy and administrative jurisdiction over religious practice by forcing visible doctrinal reversal.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, agenda_setter,
    institutional, civilizational, analytical, national).

% Were practicing plural marriage as doctrine-mandated spiritual commitment. Face imprisonment, property seizure, and excommunication if they continue. Doctrinal obligation now collides with legal mandate; the only visible exit is abandonment of the practice they held as eternally binding. The identity-lock is doctrinal: leaving the church means severing eternal kinship bonds and rejecting salvation claims.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_polygamist_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Bear the doctrinal abandonment collectively: the claim that plural marriage is eternally true is now publicly revoked by the prophetic authority that proclaimed it. Members experience the gap between the doctrine they were taught—and taught as binding—and its sudden suspension. Leaving requires severing institutional and relational identity; staying requires internalizing the contradiction between past and present teaching.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership_collectively, payer,
    organized, generational, constrained, national).

% Face legal exposure if their relationship continues, loss of legal status and inheritance rights, and internal community rupture. Children born to plural marriages have uncertain legal standing. Escape requires geographic relocation and severing all kinship and institutional ties. The trap is legal, economic, and relational.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, polygamist_wives_and_children, payer,
    powerless, biographical, trapped, national).

% Continue plural marriage underground, explicitly rejecting the Manifesto as coerced capitulation rather than revelation. They maintain the doctrine and practice the constraint was designed to suppress. Their voice is structurally excluded from the institutional settlement that frames the Manifesto as prophetic truth.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, polygamist_resistance_cells, excluded,
    moderate, biographical, trapped, regional).

% Issues the Manifesto under federal pressure, formally suspending the doctrine while maintaining it as spiritual truth 'for this time.' Preserves the church's institutional existence and legal standing, but at the cost of visible contradiction between doctrine and practice. The prophet's authority is sustained, but its truth-claim becomes temporally modulated: doctrine true eternally, practice suspended exogenously.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_prophetic_authority, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, lds_prophetic_authority, payer).

% A theological proposition (not an agent): the claim that plural marriage is eternally doctrinally binding. This reading treats the Manifesto as coercive suspension of practice, not reinterpretation of doctrine. The proposition remains, but its institutional validity is suspended.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, mainstream_mormon_theology, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_legitimacy__exogenous_override_reading, mainstream_mormon_theology).

% Achieves the goal of prohibiting plural marriage through federal authority rather than constitutional amendment. The Manifesto is treated as evidence that coercive state power works: religious institutional practice can be suspended through legal and economic pressure, regardless of doctrine.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, anti_polygamy_coalition, beneficiary,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% TRANSFER_FUNCTION: Moves institutional compliance (visible practice cessation) from the LDS institution to federal authority; moves administrative jurisdiction over marriage from religious to civil domain; moves legitimacy of the constraint from institutional doctrine to federal law; costs (legal exposure, kinship rupture, doctrinal contradiction) are borne by practitioners and general membership.
% ABSENT_VOICES: Underground polygamist practitioners and resistance cells are structurally excluded from the institutional settlement. Their voice—that the Manifesto is coerced, not revelatory—is administratively invisible and labeled apostate. Wives and children in plural marriages are excluded from legal recognition and protection.
% DISAPPEARANCE_RATIONALE: If the federal override and Manifesto disappeared overnight, plural marriage doctrine would resume institutional legitimacy, practitioners would resume practice, and the federal government would lose the most significant leverage it achieved over a major religious institution without constitutional amendment. The doctrinal landscape and institutional structure would reorganize fundamentally.
% FOUNDING_PROBLEM: Federal authority sought to suppress plural marriage doctrine without constitutional amendment or direct institutional takeover. The founding problem is regulatory: how to achieve suppression of a religious practice when the Establishment Clause limits federal power to regulate religion directly.
% FOUNDING_PROBLEM_CORROBORATION: Federal courts (Reynolds v. United States, Late Corporation of the Church of Jesus Christ v. United States) explicitly addressed the founding problem: religious liberty does not extend to practice deemed harmful to public policy. Historians outside the LDS institution (academic historians of religious persecution and state regulation) corroborate that the founding problem motivated federal escalation and remains relevant to church-state relationships today.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 0.79, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.79 at interval end) and rising because the constraint's persistence requires continuous suppression of the underlying practice—federal law, property seizure, and the threat of institutional extinction extract institutional compliance. Suppression is higher still (0.87) because the constraint depends entirely on external coercive force: without federal pressure, plural marriage would resume. Theater ratio is moderate-to-high (0.58) and rising because the Manifesto is increasingly reframed as genuine revelation (prophetic authority) rather than capitulation (federal pressure)—the functional account of the constraint's persistence increasingly hides the coercive mechanism. Resistance decays substantially over the interval (from 0.72 individual at t0 to 0.38 at t50): active resistance becomes costlier and more isolated as institutional teaching reframes the Manifesto as divine will, and resistance cells are driven further underground. Accessibility_collapse rises because individual practitioners face binary choice (abandon practice or face legal/social exclusion), organizational alternatives (founding a separate church) become administratively suppressed, and structural alternatives (federal jurisdiction over marriage) are foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   The federal government seat computes the constraint as necessary (jurisdictional authority over marriage law, suppression of a practice it reads as harmful). The LDS institutional seat computes survival through constrained compliance (preserve the church at cost of doctrine-practice gap). The practitioner seats compute extraction (practice forbidden by external force, doctrine abandoned by institutional authority they trusted). The resistance seat computes betrayal (doctrine declared eternally true, then suspended). The engine derives these divergent types from the structural data: federal authority frame + institutional constraint + practitioner victimhood + resistance exclusion produces fundamentally different constraint-type computations per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government: d near 0.0 (beneficiary, collects administrative jurisdiction and compliance, exit is arbitrage—can apply pressure elsewhere or scale it). LDS leadership: d near 0.5 (balanced cost-benefit: gains institutional survival, pays doctrinal contradiction and internal legitimacy crisis—constrained exit makes it hard to exit entirely). Polygamist practitioners: d near 1.0 (full target: bear legal exposure, doctrinal abandonment, kinship rupture; identity-locked exit means leaving practice requires severing eternal kinship claims and institutional identity). Resistance cells: d near 1.0 (explicitly rejected, labeled apostate, excluded from institutional voice—trapped exit, no arbitrage). Wives and children: d = 1.0 (powerless, trapped, exposed to legal and economic ruin).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (federal jurisdiction over religious practice remains contested and relevant). Disappearance verdict is world_rearranges (if the federal override disappeared, plural marriage doctrine would reassert full institutional legitimacy and practice would resume). The constraint avoids mandatrophy classification because the federal extraction mechanism remains active: suppression requirement rises to 0.87, theater ratio rises (indicating growing performative maintenance), and resistance decays (indicating successful suppression, not mandate obsolescence). Mandatrophy would appear only if resistance remained high while theater ratio dominated (performing compliance that no one enforces)—this constraint shows enforcement intensification, not atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_practice_boundary,
    'Is the doctrine-practice distinction coherent in this reading, or does suspending practice under duress amount to changing doctrine?',
    'Theological interpretation from LDS tradition: does Mormon theology permit doctrine to be true but institutionally suspended indefinitely? Compare to other religious traditions (Islamic dispensations, Jewish halachic rulings, Christian catacomb-era practice) for consistency.',
    'If suspension amounts to doctrinal change, the reading collapses toward the endogenous reinterpretation reading, and extractiveness may be lower (the membership is not bearing abandoned doctrine, but reinterpreted doctrine). If the distinction holds, extractiveness remains high and the reading is structurally stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_boundary, conceptual, 'Whether suspending practice under duress leaves doctrine intact or effectively changes it.').

omega_variable(
    coercion_mechanism_attribution,
    'How much of the Manifesto''s adoption was direct federal coercion (legal threat, property seizure, imprisonment) versus institutional choice to preserve the church by compliance?',
    'Historical analysis of federal pressure timeline, institutional testimony from decision-makers, property/legal outcomes. Distinguish federal legal mechanisms (Reynolds decision, enforcement against living polygamists, property seizure) from institutional strategic response.',
    'If coercion was overwhelming and direct, this reading''s snare classification is secure and suppression remains high. If institutional choice predominated (leadership calculated that compliance was preferable to extinction), the constraint may trend toward tangled_rope or scaffold (institutional coordination with federal authority to end plural marriage practice). The measurement series shows rising theater ratio, which is consistent with coercion interpretation (performance hides the mechanism), but could also indicate institutional legitimacy-building around a choice they made.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_mechanism_attribution, empirical, 'Whether the Manifesto was primarily coercive or primarily strategic institutional choice.').

omega_variable(
    internalized_suppression_trajectory,
    'How much of the measured suppression (0.87) is structural (legal barriers, institutional exclusion, property seizure) versus internalized (members believe the doctrine was genuinely revised, not coerced)?',
    'Post-Manifesto institutional rhetoric analysis: does LDS teaching frame the Manifesto as divine revelation (internalization pathway) or coerced suspension (structural-only suppression)? Comparative study of institutional members who encounter pre-Manifesto teachings: do they experience cognitive dissonance indicating internalized suppression has failed?',
    'If suppression is primarily internalized (members accept the Manifesto as revelation despite contradictory prior teaching), the constraint''s effective suppression may be lower than measured (the lock is internal rather than external). If primarily structural, the constraint requires continuous external enforcement. This affects the trajectory of the constraint if federal pressure were withdrawn: would suppression persist (internalized) or collapse (structural only)?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_trajectory, empirical, 'Whether suppression is structural (legal/institutional barriers) or internalized (members believe the doctrine changed).').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Does the distinction between doctrine and practice, as stated in the Manifesto itself, correspond to this reading''s frame, or is the doctrine-practice distinction itself constructed by this reading to support the coercion interpretation?',
    'Textual analysis of the Manifesto and contemporaneous LDS leadership statements: do they explicitly separate doctrine from practice, or does the reading impose that separation retroactively? Compare the Manifesto''s own language to this reading''s interpretive claim.',
    'If the Manifesto itself sustains the doctrine-practice distinction, this reading has textual grounding and the coercion interpretation is defensible from the kernel. If the reading imposes the distinction, it is a more aggressive reinterpretation—valid as a committer frame, but less grounded in the kernel''s own language. This affects whether the reading should be classified as hermeneutical or revisionist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether the doctrine-practice distinction is native to the kernel or imposed by this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(marr_tr_t5, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t35, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 35, 0.56).
narrative_ontology:measurement_basis(marr_tr_t35, observed).
narrative_ontology:measurement(marr_tr_t50, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement_basis(marr_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(marr_be_t5, observed).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t35, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement_basis(marr_be_t35, observed).
narrative_ontology:measurement(marr_be_t50, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 50, 0.79).
narrative_ontology:measurement_basis(marr_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 5, 0.74).
narrative_ontology:measurement_basis(marr_su_t5, observed).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 20, 0.84).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t35, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 35, 0.86).
narrative_ontology:measurement_basis(marr_su_t35, observed).
narrative_ontology:measurement(marr_su_t50, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 50, 0.87).
narrative_ontology:measurement_basis(marr_su_t50, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(marr_grid_01, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(marr_grid_02, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(class), 50, 0.81).
narrative_ontology:measurement(marr_grid_03, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(individual), 0, 0.55).
narrative_ontology:measurement(marr_grid_04, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(individual), 50, 0.78).
narrative_ontology:measurement(marr_grid_05, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(organizational), 0, 0.62).
narrative_ontology:measurement(marr_grid_06, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(organizational), 50, 0.84).
narrative_ontology:measurement(marr_grid_07, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(structural), 0, 0.48).
narrative_ontology:measurement(marr_grid_08, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(structural), 50, 0.72).
narrative_ontology:measurement(marr_grid_09, marriage_commitment_legitimacy__exogenous_override_reading, resistance(class), 0, 0.74).
narrative_ontology:measurement(marr_grid_10, marriage_commitment_legitimacy__exogenous_override_reading, resistance(class), 50, 0.42).
narrative_ontology:measurement(marr_grid_11, marriage_commitment_legitimacy__exogenous_override_reading, resistance(individual), 0, 0.72).
narrative_ontology:measurement(marr_grid_12, marriage_commitment_legitimacy__exogenous_override_reading, resistance(individual), 50, 0.38).
narrative_ontology:measurement(marr_grid_13, marriage_commitment_legitimacy__exogenous_override_reading, resistance(organizational), 0, 0.65).
narrative_ontology:measurement(marr_grid_14, marriage_commitment_legitimacy__exogenous_override_reading, resistance(organizational), 50, 0.28).
narrative_ontology:measurement(marr_grid_15, marriage_commitment_legitimacy__exogenous_override_reading, resistance(structural), 0, 0.48).
narrative_ontology:measurement(marr_grid_16, marriage_commitment_legitimacy__exogenous_override_reading, resistance(structural), 50, 0.18).
narrative_ontology:measurement(marr_grid_17, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(class), 0, 0.45).
narrative_ontology:measurement(marr_grid_18, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(class), 50, 0.76).
narrative_ontology:measurement(marr_grid_19, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(individual), 0, 0.52).
narrative_ontology:measurement(marr_grid_20, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(individual), 50, 0.89).
narrative_ontology:measurement(marr_grid_21, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(organizational), 0, 0.68).
narrative_ontology:measurement(marr_grid_22, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(organizational), 50, 0.92).
narrative_ontology:measurement(marr_grid_23, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(structural), 0, 0.38).
narrative_ontology:measurement(marr_grid_24, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(structural), 50, 0.64).
narrative_ontology:measurement(marr_grid_25, marriage_commitment_legitimacy__exogenous_override_reading, suppression(class), 0, 0.62).
narrative_ontology:measurement(marr_grid_26, marriage_commitment_legitimacy__exogenous_override_reading, suppression(class), 50, 0.84).
narrative_ontology:measurement(marr_grid_27, marriage_commitment_legitimacy__exogenous_override_reading, suppression(individual), 0, 0.58).
narrative_ontology:measurement(marr_grid_28, marriage_commitment_legitimacy__exogenous_override_reading, suppression(individual), 50, 0.85).
narrative_ontology:measurement(marr_grid_29, marriage_commitment_legitimacy__exogenous_override_reading, suppression(organizational), 0, 0.72).
narrative_ontology:measurement(marr_grid_30, marriage_commitment_legitimacy__exogenous_override_reading, suppression(organizational), 50, 0.91).
narrative_ontology:measurement(marr_grid_31, marriage_commitment_legitimacy__exogenous_override_reading, suppression(structural), 0, 0.51).
narrative_ontology:measurement(marr_grid_32, marriage_commitment_legitimacy__exogenous_override_reading, suppression(structural), 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__exogenous_override_reading, 0.18).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The marriage_commitment_legitimacy kernel decomposes into three structurally distinct readings, each producing different ε-invariant constraint stories. The exogenous_override_reading (this story) treats federal coercion as the primary mechanism and doctrine-practice distinction as genuine; extractiveness is high (0.79). The endogenous_reinterpretation_reading (sibling) treats the Manifesto as genuine revelation and doctrine as reinterpreted; extractiveness is lower. The hybrid_pragmatic_reading (sibling) treats the Manifesto as strategic adaptation that preserves core theology through scope ambiguity; extractiveness intermediate. These are not different observations of the same constraint—they are different constraints instantiated from the same kernel by different reading frames. Each story has its own beneficiary/victim structure, its own omegas addressing reading-specific uncertainties, and its own network edges to siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
