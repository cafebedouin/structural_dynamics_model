% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__constitutional_floor_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: fisa_702_statutory_text__constitutional_floor_reading
 *   human_readable: Fourth Amendment Warrant Requirement for 702 Queries of U.S. Person Communications
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the constitutional_floor_reading of
 *   the fisa_702_statutory_text kernel. It asserts that the Fourth Amendment
 *   imposes a probable cause warrant requirement for any government search of
 *   U.S. person communications content, including queries of the 702
 *   database, regardless of the foreign intelligence purpose of the original
 *   collection. The reading reframes 702 from a foreign intelligence statute
 *   to a criminal procedure question: every query of a U.S. person's
 *   communications is a Fourth Amendment search requiring individualized
 *   judicial approval. The claimed_type is mountain (constitutional
 *   requirement emerging naturally from the Fourth Amendment), but base
 *   extractiveness ε=0.25 reflects compliance costs imposed on the executive
 *   branch's preferences for speed and secrecy — a divergence the engine will
 *   measure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.35).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, mountain).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Fourth Amendment Warrant Requirement for 702 Queries of U.S. Person Communications").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).
domain_priors:emerges_naturally(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, 'ccde9e45-991d-4824-87a8-48e38b7ff631').
narrative_ontology:cs_kernel_codification('ccde9e45-991d-4824-87a8-48e38b7ff631', fixed_text).
narrative_ontology:cs_authority_grounding('ccde9e45-991d-4824-87a8-48e38b7ff631', lineage).
narrative_ontology:cs_interpretation_layer_present('ccde9e45-991d-4824-87a8-48e38b7ff631').
narrative_ontology:cs_reading_relation('ccde9e45-991d-4824-87a8-48e38b7ff631', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('ccde9e45-991d-4824-87a8-48e38b7ff631', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_axiom('ccde9e45-991d-4824-87a8-48e38b7ff631', foundational, fourth_amendment_applies_to_database_queries).
narrative_ontology:cs_axiom_status(fourth_amendment_applies_to_database_queries, holdable).
narrative_ontology:cs_axiom_grounding('ccde9e45-991d-4824-87a8-48e38b7ff631', fourth_amendment_applies_to_database_queries, deontological).
narrative_ontology:cs_axiom('ccde9e45-991d-4824-87a8-48e38b7ff631', foundational, probable_cause_warrant_required_pre_query).
narrative_ontology:cs_axiom_status(probable_cause_warrant_required_pre_query, holdable).
narrative_ontology:cs_axiom_grounding('ccde9e45-991d-4824-87a8-48e38b7ff631', probable_cause_warrant_required_pre_query, deontological).
narrative_ontology:cs_reference_frame('ccde9e45-991d-4824-87a8-48e38b7ff631', classical_fourth_amendment_warrant_requirement).
narrative_ontology:cs_drift_state('ccde9e45-991d-4824-87a8-48e38b7ff631', post_carpenter_digital_privacy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ccde9e45-991d-4824-87a8-48e38b7ff631', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, us_persons).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, privacy_advocates_civil_liberties_orgs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_branch).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_warrant_requirement).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, constitutional_floor_doctrine).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, carpenter_digital_privacy_extension).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their communications content is protected from warrantless government searches under this reading; they bear no compliance costs but gain constitutional privacy protection. Exit from the constraint's protection would mean leaving U.S. jurisdiction or accepting surveillance — neither is a meaningful exit for most.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, us_persons, beneficiary,
    organized, biographical, trapped, national).

% Must obtain probable cause warrants for 702 queries of U.S. person communications; bears operational delays, resource costs for FISA Court proceedings, and loss of speed/secrecy in intelligence gathering. Cannot exit the constraint without constitutional amendment or Supreme Court reversal.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Adjudicates individualized probable cause warrant applications for 702 queries; bears increased caseload and responsibility for constitutional compliance. The court's legitimacy depends on faithful application of the warrant requirement this reading establishes.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, agenda_setter,
    institutional, generational, analytical, national).

% Enacted FISA 702 statute; oversees implementation but cannot override constitutional floor. Legislative fixes must operate within the warrant requirement this reading establishes.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, congress, observer,
    institutional, generational, analytical, national).

% Non-U.S. persons abroad targeted under 702; their communications with U.S. persons are incidentally collected and would require warrants for query under this reading. They have no standing in U.S. constitutional framework and no voice in the adjudication.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, foreign_intelligence_targets, excluded,
    powerless, immediate, trapped, global).

% Advocate for this reading as the correct constitutional interpretation; benefit from the constraint's enforcement as it aligns with their mission. Can shift advocacy focus but remain committed to this constitutional frame.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, privacy_advocates_civil_liberties_orgs, beneficiary,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundary between legitimate foreign intelligence gathering and unconstitutional domestic surveillance by requiring judicial pre-approval for any search of U.S. person communications content.
% TRANSFER_FUNCTION: Moves the burden of justification from post-hoc rationalization to pre-search probable cause showing, transferring decision authority from executive branch to Article III court for each query of U.S. person communications.
% ABSENT_VOICES: Foreign intelligence targets (non-U.S. persons abroad) whose communications with U.S. persons would be subject to warrant requirements under this reading; they have no constitutional standing and no voice in U.S. courts. Also, future U.S. persons whose communications patterns haven't yet been established.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, 702 queries of U.S. person communications would proceed without warrants, fundamentally altering the privacy-surveillance balance. The executive branch would revert to warrantless querying of the 702 database for U.S. person communications; FISA Court role would shrink to collection approval only; U.S. person privacy protections would depend solely on statutory minimization procedures.
% FOUNDING_PROBLEM: The founding problem was preventing general warrants and unreasonable searches of private communications, extended to modern digital communications collected incidentally under foreign intelligence authorities.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court in Carpenter v. United States (2018) affirmed Fourth Amendment protection for digital communications data; FISA Court opinions (e.g., 2018 FISC memorandum opinion) have acknowledged constitutional questions about 702 queries; legal scholars outside the intelligence community (e.g., PCLOB reports) corroborate the constitutional floor reading.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fisa_702_statutory_text__constitutional_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__constitutional_floor_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, ExtMetricName, E),
    domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fisa_702_statutory_text__constitutional_floor_reading),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.25) measures the constraint's cost to executive operational preferences, not extraction from citizens. Suppression (0.35) reflects the constraint's blocking of warrantless query alternatives. Theater ratio (0.12) is low — the warrant process is functional, not performative. Accessibility collapse (0.88) is high: once the constitutional floor is recognized, statutory alternatives cannot lower it. Resistance (0.68) is substantial: the executive branch has consistently resisted extending warrant requirements to 702 queries. The measurement grid shows rising extractiveness and suppression from 2008 enactment through Carpenter (2018) to present, tracking judicial and public pressure.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (executive branch) experiences this as substantial extraction (operational friction, secrecy loss) while beneficiary seats (U.S. persons, privacy advocates) experience it as coordination (privacy protection). The engine computes this divergence from the structural data — the claimed mountain type reflects the beneficiary/analytical perspective, while the metrics reflect the executive's lived experience.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. persons are structural beneficiaries (d near 0.0) — the constraint subsidizes their privacy. Executive branch is the primary target (d near 1.0) — bears compliance costs. FISA Court sits near symmetric (d≈0.5) — administers the constraint but gains institutional legitimacy. Congress is analytical observer. Foreign targets are excluded — no standing. Privacy advocates are beneficiaries but with mobile exit (can shift focus).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing general warrants) remains live in the digital age — Carpenter confirms it. But the executive branch argues the foreign intelligence exception resolves it. The constraint is not mandatrophic: its function (constitutional floor) is actively contested, not atrophied. The theater ratio stays low because the warrant process does real work, though the executive treats it as friction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the fisa_702_statutory_text kernel, or does it represent the only valid interpretation?',
    'Supreme Court ruling on whether 702 queries of U.S. person communications constitute Fourth Amendment searches requiring warrants. If Court adopts this reading, sibling readings are foreclosed; if Court adopts sibling reading, this reading becomes a dissenting position.',
    'If this reading is foreclosed, the constitutional floor collapses to statutory minimization; if upheld, sibling readings become non-operative for U.S. person queries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the constitutional_floor_reading is a live constitutional interpretation or a policy preference masquerading as constitutional law.').

omega_variable(
    structural_delta_ambiguity,
    'Does the ''search'' classification for 702 queries depend on the collection''s legitimacy, or is it independent as this reading asserts?',
    'Judicial resolution of the ''query-as-search'' question in pending/future litigation (e.g., Mohamud, Hasbajrami, or new challenges).',
    'If queries are searches independent of collection legitimacy, this reading''s structural delta holds; if queries inherit collection''s legitimacy, the incidental_collection_reading''s framework prevails.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_delta_ambiguity, empirical, 'Whether the Fourth Amendment search inquiry separates collection from query.').

omega_variable(
    extraction_referent_ambiguity,
    'Is ε=0.25 (executive compliance costs) the correct referent, or should extraction be measured from U.S. person privacy loss under sibling readings?',
    'Comparative analysis: measure ε for each reading from its own structural perspective. This reading''s ε is executive compliance cost; sibling readings'' ε would be U.S. person privacy extraction.',
    'Different referents produce different ε values for the same kernel — confirming they are distinct constraints per ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_referent_ambiguity, conceptual, 'ε-invariance test: each reading of the kernel has its own ε referent; comparing across readings requires separate stories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_702_const_floor_tr_t0, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(fisa_702_const_floor_tr_t5, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 5, 0.08).
narrative_ontology:measurement(fisa_702_const_floor_tr_t10, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(fisa_702_const_floor_tr_t13, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 13, 0.11).
narrative_ontology:measurement(fisa_702_const_floor_tr_t16, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 16, 0.12).

% Extraction over time
narrative_ontology:measurement(fisa_702_const_floor_be_t0, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(fisa_702_const_floor_be_t5, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(fisa_702_const_floor_be_t10, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(fisa_702_const_floor_be_t13, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 13, 0.24).
narrative_ontology:measurement(fisa_702_const_floor_be_t16, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 16, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fisa_702_const_floor_su_t0, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(fisa_702_const_floor_su_t5, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(fisa_702_const_floor_su_t10, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(fisa_702_const_floor_su_t13, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 13, 0.34).
narrative_ontology:measurement(fisa_702_const_floor_su_t16, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 16, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__constitutional_floor_reading, 0.1).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).

% DUAL FORMULATION NOTE:
% This constraint and its siblings form the fisa_702_statutory_text constraint family. Each reading instantiates a different constraint with different ε referents: this reading (ε=0.25, executive compliance), incidental_collection_reading (ε≈0.60, U.S. person privacy extraction), foreign_target_strict_reading (ε≈0.15, collection targeting compliance). Linked via affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fisa_702_statutory_text__constitutional_floor_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
