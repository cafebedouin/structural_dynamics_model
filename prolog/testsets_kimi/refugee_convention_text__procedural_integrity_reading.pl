% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__procedural_integrity_reading, []).

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
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: Refugee Convention Procedural Integrity Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint instantiates the procedural-integrity reading of the 1951
 *   Refugee Convention kernel. It treats the Convention as mandating
 *   non-negotiable fair individualized assessment, permitting flexible
 *   protection thresholds but forbidding elimination of substantive review or
 *   its procedural scaffolding. The reading coordinates state behavior around
 *   a common procedural floor while asymmetrically extracting sovereignty
 *   costs from states and human costs from asylum seekers denied access
 *   through offshore processing and externalization. It is one of three
 *   structurally distinct readings of the same kernel; the expansive
 *   humanitarian reading and restrictive sovereignty reading instantiate
 *   different constraints with different stakeholder directionalities.
 *
 * KEY AGENTS:
 *   - states_parties: Primary agenda-setter and payer (institutional/constrained) â administers the system and bears sovereignty/administrative costs.
 *   - asylum_seekers: Primary beneficiary (powerless/trapped) â receives procedural protection.
 *   - offshore_detained_asylum_seekers: Primary victim (powerless/trapped) â bears cost of procedural evasion.
 *   - unhcr: Secondary beneficiary (institutional/constrained) â derives mandate from the framework.
 *   - human_rights_courts: Analytical observer (institutional/analytical) â reviews compliance.
 *   - sovereigntist_advocates: Excluded voice (organized/constrained) â objects but is outside the interpretive framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.52).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.48).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "Refugee Convention Procedural Integrity Reading").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, 'c1a38148-d435-497f-a574-21aa3857101f').
narrative_ontology:cs_kernel_codification('c1a38148-d435-497f-a574-21aa3857101f', fixed_text).
narrative_ontology:cs_authority_grounding('c1a38148-d435-497f-a574-21aa3857101f', lineage).
narrative_ontology:cs_interpretation_layer_present('c1a38148-d435-497f-a574-21aa3857101f').
narrative_ontology:cs_reading_relation('c1a38148-d435-497f-a574-21aa3857101f', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1a38148-d435-497f-a574-21aa3857101f', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_axiom('c1a38148-d435-497f-a574-21aa3857101f', foundational, fair_individualized_assessment_obligation).
narrative_ontology:cs_axiom_status(fair_individualized_assessment_obligation, holdable).
narrative_ontology:cs_axiom_grounding('c1a38148-d435-497f-a574-21aa3857101f', fair_individualized_assessment_obligation, conventional).
narrative_ontology:cs_axiom('c1a38148-d435-497f-a574-21aa3857101f', foundational, substantive_review_non_eliminable).
narrative_ontology:cs_axiom_status(substantive_review_non_eliminable, holdable).
narrative_ontology:cs_axiom_grounding('c1a38148-d435-497f-a574-21aa3857101f', substantive_review_non_eliminable, conventional).
narrative_ontology:cs_reference_frame('c1a38148-d435-497f-a574-21aa3857101f', procedural_individualized_review_framework).
narrative_ontology:cs_drift_state('c1a38148-d435-497f-a574-21aa3857101f', contemporary_externalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c1a38148-d435-497f-a574-21aa3857101f', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, asylum_seekers).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, unhcr).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, offshore_detained_asylum_seekers).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, states_parties).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, refugee_status_determination_proceduralism).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, non_refoulement_as_procedural_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer refugee status determination systems and are bound by the Convention to provide fair individualized assessment. They retain flexibility to narrow protection thresholds but cannot eliminate substantive review or outsource determination entirely without procedural guarantees. Bear administrative costs, legal exposure, and sovereignty constraints over border control.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, states_parties, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, states_parties, payer).

% Receive a procedurally anchored right to individualized review of their protection claims. The non-negotiable process integrity requirement affords them formal access to status determination and appeal, even where the substantive protection threshold is contested or narrow.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers, beneficiary,
    powerless, biographical, trapped, national).

% Subjected to offshore processing and detention arrangements that deny or delay individualized procedural guarantees. They bear the human cost of state evasion strategies: prolonged detention, legal limbo, and exposure to refoulement while nominally inside a protection framework.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, offshore_detained_asylum_seekers, payer,
    powerless, immediate, trapped, national).

% Derives institutional mandate, operational budget, and normative authority from the Convention's procedural framework. Monitors state compliance, advises on procedural standards, and intervenes in litigation to defend process integrity.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, unhcr, beneficiary,
    institutional, generational, constrained, global).

% Review state compliance with procedural integrity requirements through binding and non-binding judgments. Interpret the Convention's text to maintain the non-negotiability of individualized assessment and effective remedy.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, human_rights_courts, observer,
    institutional, generational, analytical, continental).

% Advocate for summary executive discretion and restrictive border closure without individualized judicial review. They are structurally marginalized in the treaty-based interpretive conversation but exercise political pressure on states to circumvent procedural obligations.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, sovereigntist_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international recognition of refugee status through a shared procedural minimum, preventing a race-to-the-bottom in border control and establishing cross-border standards for individualized assessment and appeal.
% TRANSFER_FUNCTION: Moves the obligation to conduct fair individualized assessment from unchecked executive discretion to asylum seekers as procedural rights-claimants and to courts as reviewing authorities; moves administrative burdens and sovereignty constraints to states; moves liberty and safety costs to asylum seekers subjected to offshore processing without guarantees.
% ABSENT_VOICES: Persons fleeing generalized violence, climate displacement, and economic deprivation who fall outside the individualized persecution framework; state executives seeking to externalize or eliminate refugee review entirely; populist movements advocating summary border control without judicial review.
% DISAPPEARANCE_RATIONALE: If the procedural integrity requirement vanished overnight, states would revert to summary removals and bilateral externalization agreements; the international refugee protection architecture would fragment into unilateral discretion, and asylum seekers would lose the primary legal mechanism preventing arbitrary refoulement.
% FOUNDING_PROBLEM: Post-World War II statelessness and arbitrary rejection of refugees by states closing borders without individual review; need to prevent recurrence of systematic pushbacks and collective expulsions.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and human rights courts attest that arbitrary rejection remains live and procedural review essential. Restrictionist governments and some migration scholars attest the founding crisis has evolved into mixed migration flows that the procedural model poorly addresses; independent migration researchers corroborate that border closures continue but dispute whether the Convention's procedural model remains the appropriate solution.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__procedural_integrity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__procedural_integrity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate but significant: the constraint genuinely coordinates international protection but extracts sovereignty from states and liberty from offshore-detained asylum seekers. Suppression (0.48) reflects the soft-law enforcement infrastructure (courts, UNHCR monitoring) that suppresses unrestricted state discretion without hard sanctions. Theater ratio (0.40) captures the growing gap between procedural form and substantive protection, as states construct elaborate review faÃ§ades while externalizing determination. Accessibility collapse (0.65) is moderately high because the Convention dominates normative space, making non-procedural alternatives seem illegitimate even when functional. Resistance (0.55) reflects sustained state circumvention through offshore processing and expedited removal.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (states_parties) experiences the constraint as a sovereignty cost and administrative burden; the engine should compute a target-leaning directionality. The beneficiary seats (asylum_seekers, UNHCR) experience it as protective coordination; the engine should compute beneficiary-leaning directionality. The payer seat (offshore_detained_asylum_seekers) experiences it as extraction dressed in procedural language; the engine should compute full-target directionality. The divergence arises because the same procedural framework that protects some asylum seekers actively harms others through evasion.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (asylum_seekers, UNHCR) feed low directionality. Victim declarations (offshore_detained_asylum_seekers, states_parties) feed high directionality. The structural asymmetry is that the constraint extracts from both ends: sovereignty from states and liberty from excluded asylum seekers, while concentrating coordination benefits on those who successfully access procedure and on the institutional apparatus. States' constrained exit (denunciation is costly and rare) amplifies their effective extraction despite their institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy mislabeling by insisting on its founding problem's continued relevance (arbitrary rejection) while acknowledging contested status. The R5 genealogy interview (founding_problem_status contested) prevents automatic piton classification. The temporal measurements show rising theater_ratio and extractiveness, signaling Goodhart drift rather than atrophy. If the procedural machinery were clearly obsolete, the metrics would support piton detection; instead, the drift is toward extraction-accumulation, consistent with tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_form_vs_substance_gap,
    'Does the procedural integrity reading describe the Convention''s actual operation, or does it function as a legitimating frame for increasingly restrictive state practices that perform procedure while denying protection?',
    'Comparative case-law and empirical outcome analysis measuring the gap between procedural guarantees articulated in judicial decisions and protection outcomes experienced by asylum seekers in major destination states.',
    'If the gap is severe, the constraint''s extraction exceeds its coordination function, pushing the effective classification toward snare. If modest, the reading remains a tangled rope with genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_form_vs_substance_gap, empirical, 'Whether procedural integrity is operational reality or legitimating rhetoric.').

omega_variable(
    authority_grounding_ambiguity,
    'Does the authority of this reading derive from the fixed 1951 text (lineage), from subsequent state practice and opinio juris (practice), or from the institutional self-interest of UNHCR and courts (extraction)?',
    'Genealogical analysis of key procedural-integrity judgments tracing whether they ground themselves in textual originalism, evolving practice, or institutional necessity.',
    'If authority is primarily institutional-extractive, part of the coordination story is cover for institutional self-preservation, raising effective extraction. If lineage-based, the constraint''s stability is anchored in consent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_ambiguity, conceptual, 'Ambiguity in the grounding of interpretive authority for the procedural reading.').

omega_variable(
    victim_set_composition,
    'Is the primary extraction borne by states (sovereignty and administrative costs) or by asylum seekers denied procedural access (liberty and safety costs through offshore processing)?',
    'Disaggregation of extraction flows: measuring state budgetary and sovereignty costs against asylum seeker detention, refoulement, and legal-limbo metrics.',
    'If state costs dominate, the asymmetry is state-as-payer versus individual-as-beneficiary, a classic tangled rope. If asylum seeker costs dominate, the constraint may function as a procedural snare that manufactures exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_composition, empirical, 'Uncertainty about which seat bears the primary extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rc_procint_tr_t0, refugee_convention_text__procedural_integrity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(rc_procint_tr_t3, refugee_convention_text__procedural_integrity_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement(rc_procint_tr_t6, refugee_convention_text__procedural_integrity_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(rc_procint_tr_t9, refugee_convention_text__procedural_integrity_reading, theater_ratio, 9, 0.35).
narrative_ontology:measurement(rc_procint_tr_t12, refugee_convention_text__procedural_integrity_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(rc_procint_tr_t14, refugee_convention_text__procedural_integrity_reading, theater_ratio, 14, 0.4).

% Extraction over time
narrative_ontology:measurement(rc_procint_be_t0, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(rc_procint_be_t3, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(rc_procint_be_t6, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(rc_procint_be_t9, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 9, 0.5).
narrative_ontology:measurement(rc_procint_be_t12, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(rc_procint_be_t14, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 14, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(rc_procint_su_t0, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(rc_procint_su_t3, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 3, 0.4).
narrative_ontology:measurement(rc_procint_su_t6, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(rc_procint_su_t9, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 9, 0.5).
narrative_ontology:measurement(rc_procint_su_t12, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(rc_procint_su_t14, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__expansive_humanitarian_reading).

% DUAL FORMULATION NOTE:
% The refugee_convention_text kernel conflates three structurally distinct claims: an expansive humanitarian mandate, a procedural integrity safeguard, and a restrictive sovereignty floor. Each reading has a distinct epsilon, beneficiary/victim structure, and stakeholder directionality profile. They are modeled as separate constraints linked by network edges rather than as one constraint with parameter-dependent classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
