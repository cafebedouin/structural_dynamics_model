% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Marriage Commitment Reversal: Practice-Doctrine Gap (1890â1910)
 *   domain: religious_institutional_history
 *
 * SUMMARY:
 *   Between 1890 and 1910, the Church of Jesus Christ of Latter-day Saints
 *   publicly suspended plural marriage while preserving Doctrine and
 *   Covenants Section 132 â the revelation authorizing it â as canonical
 *   scripture. The 1890 Manifesto announced compliance with federal law, yet
 *   over 200 plural marriages were subsequently performed in Mexico, Canada,
 *   and other jurisdictions under tacit or explicit leadership authorization.
 *   The Second Manifesto in 1904 attempted to close the gap but left the
 *   doctrinal text untouched. General membership experienced bewilderment and
 *   betrayal as public rhetoric and private practice diverged;
 *   fundamentalists experienced the gap as apostasy and eventually schismed.
 *   This constraint is the practice_doctrine_gap reading of the
 *   marriage_commitment_reversal kernel, distinct from readings attributing
 *   the reversal to pure external coercion or to genuine internal revelation.
 *
 * KEY AGENTS:
 *   - Church hierarchy (agenda_setter/beneficiary): Manages the dual-track system, preserves institutional survival and flexibility.
 *   - General membership (payer): Bears cognitive and spiritual costs of unresolved ambiguity; exit blocked by identity lock.
 *   - Fundamentalist dissidents (payer): Insists on literal continuity, pays costs of schism and ostracism.
 *   - Federal authorities (observer): Accepts theatrical compliance, enabling the ambiguity to persist.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.82).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.78).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Marriage Commitment Reversal: Practice-Doctrine Gap (1890â1910)").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious_institutional_history").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '9fa3bd59-6ea8-4950-b6a2-a5343fd55842').
narrative_ontology:cs_kernel_codification('9fa3bd59-6ea8-4950-b6a2-a5343fd55842', fixed_text).
narrative_ontology:cs_authority_grounding('9fa3bd59-6ea8-4950-b6a2-a5343fd55842', lineage).
narrative_ontology:cs_interpretation_layer_present('9fa3bd59-6ea8-4950-b6a2-a5343fd55842').
narrative_ontology:cs_reading_relation('9fa3bd59-6ea8-4950-b6a2-a5343fd55842', marriage_commitment_reversal__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('9fa3bd59-6ea8-4950-b6a2-a5343fd55842', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('9fa3bd59-6ea8-4950-b6a2-a5343fd55842', foundational, section_132_doctrinal_permanence).
narrative_ontology:cs_axiom_status(section_132_doctrinal_permanence, holdable).
narrative_ontology:cs_axiom_grounding('9fa3bd59-6ea8-4950-b6a2-a5343fd55842', section_132_doctrinal_permanence, theological).
narrative_ontology:cs_axiom('9fa3bd59-6ea8-4950-b6a2-a5343fd55842', secondary, public_compliance_without_doctrinal_abrogation).
narrative_ontology:cs_axiom_status(public_compliance_without_doctrinal_abrogation, holdable).
narrative_ontology:cs_axiom_grounding('9fa3bd59-6ea8-4950-b6a2-a5343fd55842', public_compliance_without_doctrinal_abrogation, conventional).
narrative_ontology:cs_reference_frame('9fa3bd59-6ea8-4950-b6a2-a5343fd55842', section_132_everlasting_covenant).
narrative_ontology:cs_drift_state('9fa3bd59-6ea8-4950-b6a2-a5343fd55842', post_manifesto_public_compliance, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9fa3bd59-6ea8-4950-b6a2-a5343fd55842', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, church_hierarchy).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_dissidents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the doctrinal-practice boundary after the 1890 Manifesto. Publicly announces cessation of plural marriage while privately authorizing continued performance in peripheral jurisdictions and preserving Section 132 as canonical scripture. Controls the interpretive apparatus that defines what counts as compliance. Benefits from institutional survival, statehood, and retention of doctrinal flexibility.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, church_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, church_hierarchy, beneficiary).

% Receives contradictory signals: public statements declare plural marriage ended, yet doctrinal texts remain unrepealed and peripheral practice continues. Bears confusion, betrayal, and cognitive load of sustaining an unresolved ambiguity. Exit is constrained by deep family and community identity fusion; leaving constitutes social and spiritual rupture.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    organized, biographical, identity_locked, national).

% Insists on the literal validity of Section 132 and the prophetic necessity of plural marriage. Experiences the gap as apostasy rather than ambiguity. Eventually forced into schism and peripheral communities (Mexico, Canada, Utah fringe) after 1904 Second Manifesto. Pays costs of excommunication, legal jeopardy, and social ostracism.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_dissidents, payer,
    moderate, biographical, constrained, national).

% Legislates and prosecutes against plural marriage (Edmunds-Tucker Act, Reynolds). Accepts public compliance as sufficient for statehood and asset return. Does not systematically verify private practice after 1890, treating performative compliance as legal resolution. External to the doctrinal system but structurally enabling the ambiguity by rewarding theatrical cessation.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__practice_doctrine_gap, church_hierarchy).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__practice_doctrine_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates institutional survival under existential federal threat by decoupling public practice from doctrinal permanence, allowing the organization to comply with federal demands for statehood and asset security while retaining scriptural claims to prophetic continuity.
% TRANSFER_FUNCTION: Moves institutional loyalty, compliance, and cognitive burden from general membership to the hierarchy; extracts clarity and trust from members in exchange for institutional flexibility and survival capacity concentrated in the leadership.
% ABSENT_VOICES: Peripheral practitioners in Mexico and Canada who were authorized to continue plural marriage after 1890 but later abandoned or disavowed; rank-and-file members who sought clear doctrinal repudiation rather than strategic ambiguity; federal authorities who demanded actual cessation rather than performative compliance.
% DISAPPEARANCE_RATIONALE: If the doctrinal-practice ambiguity vanished overnight â either by honest repeal of Section 132 or by public resumption of the practice â the institutional structure would rearrange: honest repeal would trigger fundamentalist schism and loss of prophetic authority claims; honest resumption would renew federal seizure and prosecution. The dual-track system was load-bearing.
% FOUNDING_PROBLEM: Federal seizure of church assets, disincorporation, and imprisonment of leadership under the Edmunds-Tucker Act and subsequent federal enforcement; the need to secure institutional existence and Utah statehood.
% FOUNDING_PROBLEM_CORROBORATION: Federal statutes and Supreme Court rulings (Reynolds, Late Corporation of the Church) attest the external threat from outside the benefiting party. Independent historians (Van Wagoner, Quinn, Hardy) corroborate the existential risk and the subsequent continuation of plural marriages post-1890, attesting the gap between founding problem and continued arrangement.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at peak) because institutional survival is purchased with membership clarity and trust; the hierarchy gains generational flexibility while members absorb the contradiction. Suppression is high (0.78) because the dual-track system requires active information control â disciplining public discussion, obscuring peripheral authorizations, and stigmatizing dissent. Theater ratio is significant (0.55â0.65) because public proclamations of cessation increasingly diverged from private authorization, especially 1894â1904. Accessibility collapse is high (0.75) because religious identity fusion makes exit nearly unthinkable for most members. Resistance is moderate (0.45) â fundamentalists resist doctrinally, some members drift away, but the majority comply under ambiguity.
 *
 * PERSPECTIVAL GAP:
 *   From the hierarchy's seat, the arrangement is necessary institutional survival under existential threat â a tangled coordination that keeps the organization intact. From the general membership seat, the same structure operates as extraction of clarity and trust, producing bewilderment and identity strain. From the fundamentalist seat, it operates as bad-faith apostasy. The engine computes these divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The church hierarchy is the structural beneficiary (low d): it collects institutional survival, statehood, and doctrinal optionality. General membership and fundamentalist dissidents are targets (high d): they bear the cognitive, spiritual, and social costs of the ambiguity. Federal authorities sit at analytical distance (d near 0.5 or analytical default) â they do not pay the constraint's costs but structurally enable it by accepting surface compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â federal destruction of the institution â was live in 1890 but substantially dead by 1904. The arrangement persisted beyond its original mandate, yet it did not atrophy into pure piton because it was actively managing an ongoing schism threat and doctrinal contradiction. The persistence is better modeled as tangled_rope: the coordination function (holding the institution together) and the extraction function (member confusion, dual-track legitimation) are inseparable and both require active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_reading_location,
    'This constraint is the practice_doctrine_gap reading of the marriage_commitment_reversal kernel. How does its structural classification change if the exogenous_override_reading or endogenous_reinterpretation_reading is adopted instead?',
    'Cross-reading comparison within the kernel family: evaluate whether the causal mechanism (external coercion vs internal revelation) alters the beneficiary/victim structure or the necessity of active enforcement.',
    'If the reversal was genuinely endogenous revelation, the coordination function strengthens and extraction may lower; if purely exogenous coercion, the tangled_rope extraction intensifies as the gap becomes institutionalized bad faith.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_location, conceptual, 'Committer-frame location of this reading within the contested kernel').

omega_variable(
    ambiguity_intentionality,
    'Was the doctrine-practice gap a deliberate leadership strategy (dual-track legitimation) or an emergent institutional compromise under contradictory pressures?',
    'Archival discovery of internal First Presidency and Quorum of the Twelve deliberations, correspondence, and authorization records from 1890â1904.',
    'If deliberate, the constraint trends toward snare (coordination story as cover); if emergent, it remains tangled_rope (genuine survival coordination with asymmetric extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ambiguity_intentionality, empirical, 'Whether the ambiguity was strategic or emergent').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (excommunication, surveillance, social expulsion) or internalized (members rationalizing the gap as sacred mystery or testing faith)?',
    'Post-exit narrative analysis: if suppression persists after structural barriers are removed (e.g., after relocation outside Mormon communities), the mechanism is partially internalized.',
    'Internalized suppression raises effective extraction beyond the structural measure because members carry the constraint with them after physical exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    section_132_operative_status,
    'Does preserving Section 132 in scripture constitute genuine doctrinal permanence, or is it theatrical preservation masking effective abrogation?',
    'Analysis of subsequent official curriculum, correlated discourse, and ritual practice to determine whether the revelation retains operative normative force or is treated as historically suspended.',
    'If theatrical preservation dominates, theater_ratio rises and the constraint edges toward piton/snare territory as the doctrinal claim becomes performative rather than substantive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section_132_operative_status, conceptual, 'Whether canonical preservation is substantive or theatrical').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(marr_tr_t4, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 4, 0.45).
narrative_ontology:measurement(marr_tr_t8, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 8, 0.55).
narrative_ontology:measurement(marr_tr_t12, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 12, 0.65).
narrative_ontology:measurement(marr_tr_t16, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 16, 0.6).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 20, 0.5).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(marr_be_t4, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 4, 0.68).
narrative_ontology:measurement(marr_be_t8, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 8, 0.75).
narrative_ontology:measurement(marr_be_t12, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 12, 0.82).
narrative_ontology:measurement(marr_be_t16, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 16, 0.8).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 20, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(marr_su_t4, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 4, 0.75).
narrative_ontology:measurement(marr_su_t8, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 8, 0.8).
narrative_ontology:measurement(marr_su_t12, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 12, 0.85).
narrative_ontology:measurement(marr_su_t16, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 16, 0.82).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
