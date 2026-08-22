% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: Marriage Commitment Legitimacy (Hybrid Pragmatic Reading)
 *   domain: religious/institutional/political theology
 *
 * SUMMARY:
 *   This constraint describes one reading of how a religious institution
 *   legitimized a reversal of core doctrinal teaching on marriage in response
 *   to federal legal change. The hybrid_pragmatic_reading interprets the
 *   Manifesto (the institution's formal pronouncement) as strategic
 *   institutional adaptation: the leadership deploys prophetic authority—a
 *   legitimate theological category—to simultaneously comply with federal law
 *   AND preserve a credible claim to doctrinal consistency. This is neither
 *   pure prophecy (endogenous_reinterpretation_reading) nor pure coercion
 *   (exogenous_override_reading), but a third pathway where institutional
 *   leadership benefits from the ambiguity itself. Rank-and-file members and
 *   doctrinal traditionalists bear the cost of interpretive uncertainty: they
 *   cannot definitively determine whether their institution has evolved
 *   through revelation or capitulated under pressure. The Manifesto's scope
 *   ambiguity—deliberately leaving the doctrinal status unresolved—is what
 *   makes this arrangement work; it permits compliance while deferring the
 *   question of whether doctrine itself has changed.
 *
 * KEY AGENTS:
 *   - institutional_leadership: agenda-setter, collects legitimacy and legal standing through strategic framing
 *   - rank_and_file_membership: identity-locked payers, bear interpretive uncertainty and legitimacy ambiguity
 *   - federal_legal_authority: beneficiary through achieved compliance without confrontation
 *   - doctrinal_traditionalists: moderate-power payers, excluded from decision-making, constrained exit
 *   - theological_scholars: analytical observers documenting scope ambiguity and constituency interpretation divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.58).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.61).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "Marriage Commitment Legitimacy (Hybrid Pragmatic Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious/institutional/political theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '436a40c3-fb93-464c-82d4-b0abfd9e9120').
narrative_ontology:cs_kernel_codification('436a40c3-fb93-464c-82d4-b0abfd9e9120', formalized).
narrative_ontology:cs_authority_grounding('436a40c3-fb93-464c-82d4-b0abfd9e9120', extraction).
narrative_ontology:cs_interpretation_layer_present('436a40c3-fb93-464c-82d4-b0abfd9e9120').
narrative_ontology:cs_reading_relation('436a40c3-fb93-464c-82d4-b0abfd9e9120', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('436a40c3-fb93-464c-82d4-b0abfd9e9120', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('436a40c3-fb93-464c-82d4-b0abfd9e9120', foundational, prophetic_authority_preserves_institutional_autonomy).
narrative_ontology:cs_axiom_status(prophetic_authority_preserves_institutional_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('436a40c3-fb93-464c-82d4-b0abfd9e9120', prophetic_authority_preserves_institutional_autonomy, conventional).
narrative_ontology:cs_axiom('436a40c3-fb93-464c-82d4-b0abfd9e9120', foundational, federal_compliance_compatible_with_doctrinal_claim).
narrative_ontology:cs_axiom_status(federal_compliance_compatible_with_doctrinal_claim, holdable).
narrative_ontology:cs_axiom_grounding('436a40c3-fb93-464c-82d4-b0abfd9e9120', federal_compliance_compatible_with_doctrinal_claim, instrumental).
narrative_ontology:cs_reference_frame('436a40c3-fb93-464c-82d4-b0abfd9e9120', unchanging_marriage_doctrine_prophetic_authority).
narrative_ontology:cs_drift_state('436a40c3-fb93-464c-82d4-b0abfd9e9120', post_federal_compliance_mandate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('436a40c3-fb93-464c-82d4-b0abfd9e9120', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_membership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_legal_authority).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, reform_advocates).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_traditionalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manages the institution's survival under federal legal pressure while attempting to preserve doctrinal coherence. Issues the Manifesto as a strategic instrument that permits federal compliance (marriage legitimacy reversal) while maintaining theological claim to prophetic authority. Collects the benefit of institutional continuity, legal standing, and continued control over doctrine. Faces constraints from both federal law and internal doctrinal constituencies.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Bears the cost of interpretive uncertainty: the Manifesto's dual framing (pragmatic accommodation + prophetic authority) leaves members unable to determine whether the institution's core theological commitments remain intact or have been fundamentally revised. Must navigate the ambiguity between the official narrative (prophetic revelation) and the evident external pressure (federal coercion). Exit means severing religious identity and community belonging; cost is high. No voice in the decision that creates the ambiguity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_membership, payer,
    powerless, biographical, identity_locked, national).

% Secures institutional compliance with marriage-equality law without explicit confrontation or coercive litigation to completion. The Manifesto's framing as prophetic rather than coerced provides political cover: the institution appears to have freely revised its doctrine, reducing friction with internal religious constituencies while compliance is achieved. Gains compliance without having to publicly name the pressure that induced it.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_legal_authority, beneficiary,
    institutional, generational, analytical, national).

% Witness the institution's core teaching on marriage reversed under the cover of prophecy, which they interpret as either betrayal of doctrine or false prophecy. They are not part of the institutional leadership's decision-making; their objections are managed through internal church discipline rather than incorporated into the framing. Exit means schism from the institutional community. Active resistance is constrained by hierarchical structure and community dependence.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_traditionalists, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_traditionalists, excluded).

% See the Manifesto as evidence the institution is evolving toward justice; they read it as genuine prophetic adaptation rather than coercion. They benefit from the change and interpret the prophetic framing as legitimate spiritual growth, reinforcing their own advocacy. Mobile exit options but choose to remain and celebrate the change.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, reform_advocates, beneficiary,
    moderate, biographical, mobile, national).

% Analyzes the Manifesto's internal coherence, the historical relationship between doctrine and practice, and the mechanics of prophetic authorization claims. They document the scope ambiguity and track how different constituencies interpret the same text as either prophetic or coerced. Provide external corroboration (or lack thereof) for institutional claims about the Manifesto's authenticity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theological_scholars, observer,
    powerful, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains institutional legitimacy and legal standing under conflicting pressures: the institution must comply with federal law while preserving a credible claim to autonomous theological authority. The Manifesto coordinates these otherwise irreconcilable demands by deploying prophetic authority—a legitimate theological category—to simultaneously comply with federal law AND preserve a credible claim to doctrinal consistency. This is the genuine coordination achieved.
% TRANSFER_FUNCTION: Moves interpretive authority from democratic doctrinal process (where the reversal would require explicit acknowledgment of external pressure) to prophetic pronouncement (where authority is concentrated in institutional leadership and presented as divinely mandated). The cost of this transfer—interpretive uncertainty and legitimacy ambiguity—is borne by rank-and-file members and doctrinal traditionalists. The benefit (institutional continuity and legal standing) accrues to institutional leadership and legal authorities.
% ABSENT_VOICES: Rank-and-file members who would object to the ambiguity are managed through church discipline rather than included in the framing conversation. Doctrinal traditionalists who see false prophecy are explicitly excluded from the decision-making structure. External legal authorities do not participate in the theological justification, only observe and benefit from the compliance outcome.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its framing vanished, the institution would face explicit conflict between federal law and doctrinal commitment, likely resulting in schism, legal exposure, or explicit reversal of doctrine under acknowledged external pressure. The scope ambiguity that the Manifesto creates is what prevents this reorganization from occurring; its removal forces a choice that the ambiguity currently defers. The institution would either split into traditionalist and reformist factions, face federal enforcement action, or undertake transparent doctrinal revision.
% FOUNDING_PROBLEM: An institution with unchanging doctrinal claims about marriage encounters federal legal change requiring marriage-law compliance. The institution must preserve both its theological coherence claim AND its legal standing simultaneously—a coordination problem without a solution that satisfies both constraints transparently.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians document the federal pressure as a historical fact; theological scholars internal to the tradition attest the doctrinal commitment preceded the legal change; external observers (historians, policy analysts, religious studies scholars) confirm the institutional conflict and the absence of any prior doctrinal development toward the reversal. The founding problem is not in dispute; the characterization of how the Manifesto resolves it is contested across the three readings.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because institutional leadership genuinely solves a coordination problem (federal compliance + doctrinal coherence claim cannot be achieved transparently), but the solution extracts interpretive authority from rank-and-file democratic process. Suppression is slightly lower (0.61) because the ambiguity is tolerated rather than violently enforced; members can hold competing interpretations within the same institution. Theater is elevated (0.52) because the prophetic framing is the mechanism that makes the arrangement work—without the prophetic claim, the reversal appears as pure capitulation. The measurement series shows extraction rising through the first 20 time points as the institution invests in reinforcing the prophetic framing, then plateauing as the narrative equilibrium stabilizes. Theater rises with extraction, indicating that as extractiveness settles, the prophetic performance becomes more central to institutional maintenance. Accessibility of alternatives (0.48) is moderate because members can exit, but exit costs are high (identity loss); resistance (0.67) is substantial because doctrinal traditionalists actively contest the reading, even if they lack institutional power to change the outcome.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership seat, the Manifesto is a success: it preserves both legal standing and theological claim, coordinates otherwise irreconcilable pressures, and demonstrates prophetic authority. From the rank-and-file seat, the same Manifesto is deeply ambiguous: it neither definitively endorses prophetic evolution nor acknowledges external constraint, leaving members unable to ground their own faith commitments in a stable reading of institutional doctrine. From the legal authority seat, it is a diplomatic success: compliance is achieved without explicit coercion, reducing political friction. The engine should compute these as different types across seats: institutional leadership likely computes as rope (coordination genuinely achieved); rank-and-file as snare or tangled_rope (extraction via interpretive authority, suppressed through identity-lock); legal authority as rope (coordination solved). The structural data—beneficiary/victim declarations, power asymmetry, exit heterogeneity—drives this divergence; I author it directly.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership is the structural beneficiary: they set the agenda, frame the interpretation, collect the benefit of institutional continuity and legal standing, and preserve authority over doctrine. Directionality for this seat is low (near 0.0), indicating net benefit. Rank-and-file membership are the structural victims: they bear the cost of interpretive uncertainty, cannot exit without identity loss (identity_locked), and have no voice in the decision that creates the ambiguity. Directionality for this seat is high (near 1.0), indicating net extraction. Federal legal authorities sit as secondary beneficiaries: they achieve compliance without confrontation, which reduces political cost. Reform advocates benefit incidentally from the change and are mobile, so they compute near 0.2 (minor extraction). Doctrinal traditionalists are harmed by the reversal and lack exit (constrained), computing near 0.9 (high extraction). The asymmetry between leadership and membership is the structural fact that makes this tangled_rope: genuine coordination (federal compliance + institutional legitimacy) coupled with asymmetric extraction (rank-and-file bears the cost of ambiguity).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal compliance + doctrinal coherence) is live and urgent. The Manifesto's function as a coordination mechanism is real: it does permit both compliance and authority claim simultaneously. However, the mechanism works BY EXTRACTING from rank-and-file interpretive agency. A pure rope would solve the coordination problem transparently; a snare would enforce compliance without a coordination story. This constraint is tangled_rope because it has BOTH: genuine coordination (the prophetic framing does solve the dual-pressure problem) AND asymmetric extraction (the framing concentrates interpretive authority in leadership and leaves members with unresolved doctrine). The mandatrophy test: if the founding problem disappeared (e.g., federal law changed again), the constraint would likely persist but shift to pure snare (extractive authority without coordination justification) because the prophetic framing would lose its functional grounding. This signals that the coordination function and the extraction mechanism are currently coupled; they would decouple if the exogenous pressure lifted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophecy_authenticity_ambiguity,
    'Does the prophetic framing of the Manifesto represent genuine spiritual authority, or is it a post-hoc legitimation cover for institutional adaptation to external pressure?',
    'Historical analysis of the Manifesto''s genesis: testimony from institutional leadership, theological consistency analysis with prior doctrine, and comparison with other institutions'' responses to similar federal pressure. Doctrinal traditionalist testimony about whether the framing aligns with the institution''s own prophetic criteria.',
    'If the framing is authentic (by the institution''s own theological standards), the constraint may reclassify toward rope with reduced extractiveness; if the framing is post-hoc, extractiveness remains high and the constraint may reclassify toward snare. This directly affects the rank-and-file seat''s classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prophecy_authenticity_ambiguity, conceptual, 'Whether prophetic authority deployed to justify the reversal is authentic by the institution''s own criteria or instrumental cover for external capitulation.').

omega_variable(
    identity_lock_dissolution_trajectory,
    'If a rank-and-file member attempts exit, does the identity-lock persist after severing institutional ties, or does it dissolve once the member is outside the institution?',
    'Qualitative research with members who have left the institution: do they report continued theological commitments to the institution''s doctrines, or does doctrinal identity reconstruct outside the institutional frame?',
    'If identity-lock dissolves post-exit, the effective suppression at the time of exit decision is structural only; if it persists, the suppression is internalized and follows the exiting member. This affects whether the measured suppression (0.61) accurately captures the extractive force, or whether effective suppression is higher for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_dissolution_trajectory, empirical, 'Whether identity-lock to theological doctrine persists after institutional exit or dissolves once the member reconstructs identity outside the institution.').

omega_variable(
    scope_ambiguity_intentionality,
    'Is the Manifesto''s scope ambiguity (federal compliance + doctrinal claim both preserved, neither resolved) intentional institutional strategy, or an artifact of genuine doctrinal confusion?',
    'Analysis of institutional leadership''s internal communications, theological education materials, and response to clarification requests from members. Comparison with other institutions'' handling of similar pressures: do they also leave scope ambiguous, or do they resolve doctrinal status explicitly?',
    'If ambiguity is intentional, the institutional leadership''s benefit is deliberately captured—extractiveness may rise and classification may shift toward snare. If ambiguity is genuine confusion, the constraint may reclassify toward rope (failed coordination) or even toward piton (the institution cannot resolve the problem and is performing resolution). The theater_ratio interpretation hinges on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_ambiguity_intentionality, conceptual, 'Whether the Manifesto''s scope ambiguity is deliberate institutional strategy to preserve authority and compliance simultaneously, or an artifact of genuine doctrinal incoherence.').

omega_variable(
    kernel_reading_coexistence_constraint,
    'Can both the endogenous_reinterpretation_reading (pure prophecy) and the hybrid_pragmatic_reading (strategic adaptation) coexist within a single institutional framework without logical contradiction?',
    'Theological analysis: do the axioms of authentic prophecy in this tradition require that external pressure be absent? If yes, the readings foreclose each other; if no, they coexist. Institutional practice: does the institution itself acknowledge both readings simultaneously, or does it enforce one as orthodox?',
    'If readings foreclose, the constraint''s classification on the endogenous seat flips toward snare (false prophecy) and institutional leadership''s extraction becomes visible. If readings coexist, the institutional framework itself enables the ambiguity and both readings can remain live. This affects the network structure linking the kernel''s three readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_constraint, conceptual, 'Whether this reading and the endogenous-prophecy reading can coexist in the same institutional framework or whether they logically foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(marr_tr_t25, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 25, 0.53).
narrative_ontology:measurement(marr_tr_t30, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(marr_tr_t35, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 35, 0.52).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(marr_be_t25, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 25, 0.59).
narrative_ontology:measurement(marr_be_t30, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(marr_be_t35, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 35, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 15, 0.59).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(marr_su_t25, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(marr_su_t30, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(marr_su_t35, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 35, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel marriage_commitment_legitimacy. Three structurally distinct constraint stories instantiate the three sibling readings: endogenous_reinterpretation_reading (pure prophecy, mountain-class, ε≈0.15), exogenous_override_reading (pure coercion, snare-class, ε≈0.75), and hybrid_pragmatic_reading (strategic adaptation, tangled_rope-class, ε≈0.58). The three readings are NOT observations of the same constraint from different angles; they instantiate different ε values because they differ on the referent (what part of the institution's behavior is being evaluated and who benefits). Each reading compiles to a separate constraint_story file; the network edges link them as a kernel family. The decomposition follows the ε-invariance principle: when the same natural-language concept (the Manifesto's meaning and function) can be read to yield substantially different structural assessments of extraction, the readings are distinct constraints, not perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, powerless, 0.88).
constraint_indexing:directionality_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
