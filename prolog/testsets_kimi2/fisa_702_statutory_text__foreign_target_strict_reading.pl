% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__foreign_target_strict_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__foreign_target_strict_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: fisa_702_statutory_text__foreign_target_strict_reading
 *   human_readable: FISA Section 702 Foreign-Target Strict Reading
 *   domain: constitutional/law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint story captures the foreign_target_strict_reading of the
 *   fisa_702_statutory_text kernel. It treats the statutory foreign-targeting
 *   language as a genuine structural limit: collection is authorized only
 *   where the target is a non-U.S. person located abroad, and incidentally
 *   collected U.S. person data must be minimized through deletion or true
 *   inaccessibility rather than merely access-restricted or logged. Under
 *   this reading, the FBI is categorically prohibited from querying
 *   702-acquired data for ordinary domestic crimes. The constraint
 *   coordinates foreign intelligence collection while protecting U.S. person
 *   privacy, extracting primarily from foreign targets. It is contested by
 *   the incidental_collection_reading (which permits retention and
 *   warrantless query of U.S. person data under foreign-intelligence
 *   justification) and the constitutional_floor_reading (which imposes an
 *   independent Fourth Amendment warrant requirement regardless of statutory
 *   parsing).
 *
 * KEY AGENTS:
 *   - us_persons: Primary beneficiary (organized/constrained) â retain statutory privacy protection against mass surveillance.
 *   - foreign_persons_abroad: Primary target (powerless/trapped) â communications collected without individualized warrant or recourse.
 *   - intelligence_community: Agenda setter (institutional/constrained) â authorized to collect but must comply with targeting and minimization procedures.
 *   - fbi: Payer (institutional/constrained) â prohibited from domestic criminal queries under the strict reading, losing an investigative tool.
 *   - fisa_court: Observer (institutional/analytical) â adjudicates compliance with the statutory text.
 *   - civil_liberties_advocates: Observer (organized/analytical) â advocate strict textual adherence and deletion-based minimization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.15).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.5).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA Section 702 Foreign-Target Strict Reading").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional/law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, '0b4cfeb1-a7d1-4da5-8b30-af33e6e44b17').
narrative_ontology:cs_kernel_codification('0b4cfeb1-a7d1-4da5-8b30-af33e6e44b17', formalized).
narrative_ontology:cs_authority_grounding('0b4cfeb1-a7d1-4da5-8b30-af33e6e44b17', lineage).
narrative_ontology:cs_interpretation_layer_present('0b4cfeb1-a7d1-4da5-8b30-af33e6e44b17').
narrative_ontology:cs_reading_relation('0b4cfeb1-a7d1-4da5-8b30-af33e6e44b17', fisa_702_statutory_text__incidental_collection_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b4cfeb1-a7d1-4da5-8b30-af33e6e44b17', fisa_702_statutory_text__constitutional_floor_reading, influences).
narrative_ontology:cs_axiom('0b4cfeb1-a7d1-4da5-8b30-af33e6e44b17', foundational, statutory_foreign_target_limitation).
narrative_ontology:cs_axiom_status(statutory_foreign_target_limitation, holdable).
narrative_ontology:cs_axiom_grounding('0b4cfeb1-a7d1-4da5-8b30-af33e6e44b17', statutory_foreign_target_limitation, conventional).
narrative_ontology:cs_axiom('0b4cfeb1-a7d1-4da5-8b30-af33e6e44b17', foundational, minimization_as_deletion_requirement).
narrative_ontology:cs_axiom_status(minimization_as_deletion_requirement, holdable).
narrative_ontology:cs_axiom_grounding('0b4cfeb1-a7d1-4da5-8b30-af33e6e44b17', minimization_as_deletion_requirement, conventional).
narrative_ontology:cs_reference_frame('0b4cfeb1-a7d1-4da5-8b30-af33e6e44b17', foreign_target_limitation_framework).
narrative_ontology:cs_drift_state('0b4cfeb1-a7d1-4da5-8b30-af33e6e44b17', contemporary_surveillance_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0b4cfeb1-a7d1-4da5-8b30-af33e6e44b17', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_persons).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, foreign_persons_abroad).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, fbi).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a statutory wall that excludes them from mass surveillance authorization unless an individualized warrant is obtained. Their communications are legally shielded by the foreign-target limitation and minimization requirements, though they cannot individually opt out of the statutory scheme.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_persons, beneficiary,
    organized, generational, constrained, national).

% Are the statutorily authorized targets of 702 collection. Their communications are collected without individualized warrant or probable cause, and they have no standing or recourse in U.S. courts to challenge that collection.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, foreign_persons_abroad, payer,
    powerless, immediate, trapped, global).

% Designs targeting procedures, conducts foreign intelligence collection, and certifies compliance to the FISA Court. Gains a legal authorization framework for overseas surveillance but must absorb compliance costs, targeting restrictions, and audit burdens imposed by the strict statutory text.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_community, agenda_setter,
    institutional, generational, constrained, global).

% Under the strict reading, is categorically prohibited from querying 702-acquired databases for ordinary domestic criminal investigations. Bears the cost of losing an intelligence source that looser interpretations would permit, and must maintain procedural firewalls to prevent domestic-query violations.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fbi, payer,
    institutional, biographical, constrained, national).

% Reviews targeting and minimization procedures for compliance with the statutory foreign-target requirement. Does not conduct collection but adjudicates whether executive procedures satisfy the statutory text, with authority to approve or disapprove annual certifications.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fisa_court, observer,
    institutional, generational, analytical, national).

% Advocate for strict adherence to the foreign-target limitation and deletion-based minimization. File amicus briefs, public comments, and litigation asserting that the statute must be read to categorically prohibit domestic queries and require true destruction of incidentally collected U.S. person data.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_community).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__foreign_target_strict_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a statutory framework for signals intelligence collection against foreign targets located abroad, replacing an unchecked executive surveillance regime with a congressionally authorized scheme subject to judicial oversight and procedural compliance.
% TRANSFER_FUNCTION: Transfers foreign intelligence value from foreign persons abroad to the U.S. intelligence community; transfers privacy protection to U.S. persons by legally excluding them from bulk collection authority absent individualized warrant.
% ABSENT_VOICES: Foreign persons abroad whose communications are collected have no standing to challenge 702 collection in U.S. courts and are structurally excluded from statutory minimization debates; their interests are represented only indirectly by civil liberties organizations.
% DISAPPEARANCE_RATIONALE: If the statutory foreign-target limitation vanished overnight, the intelligence community would lack clear authorization for overseas electronic surveillance, U.S. persons would lose a statutory privacy barrier against mass collection, and the domestic-intelligence boundary would collapse as agencies operated without a legislative warrant floor.
% FOUNDING_PROBLEM: Unchecked executive-branch surveillance of domestic communications during the mid-20th century, including warrantless wiretaps of U.S. persons and political targets, created a legitimacy crisis for intelligence agencies and exposed citizens to unchecked government intrusion.
% FOUNDING_PROBLEM_CORROBORATION: The Church Committee historical record and subsequent declassified documents corroborate the founding problem from outside the intelligence community. The IC asserts the problem is solved by existing compliance regimes; this self-assessment is contested by civil liberties organizations and some FISC opinions.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).
:- end_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.15 because the strict reading narrows authorized collection to foreign targets abroad and excludes U.S. persons from the victim set, yielding a low base extraction relative to broader surveillance regimes. Suppression is moderate (0.50): the statutory prohibition on domestic use is legally enforced but faces persistent institutional pressure from law enforcement and intelligence agencies. Theater ratio is low-moderate (0.25): the statutory text and FISC oversight retain substantive protective function, but compliance documentation and minimization procedures have drifted toward performative access-logging rather than true deletion. Accessibility collapse is moderate (0.60): legal alternatives to the statutory framework are foreclosed by enactment, though extralegal alternatives remain physically possible. Resistance is moderate (0.45): the IC and FBI actively seek broader query authority, while civil liberties groups resist loosening. The metrics are intentionally independent of the claimed tangled_rope type: a constraint can be structurally hybrid (coordination plus asymmetric extraction) while operating at low extractiveness because its victim set is narrowly drawn.
 *
 * PERSPECTIVAL GAP:
 *   The intelligence community seat experiences the constraint as a compliance burden that limits targeting flexibility and requires resource-intensive minimization procedures. The U.S. person seat experiences it as a privacy protection. The foreign target seat experiences it as surveillance exposure with no recourse. These divergences are structurally inherent: the same legal text simultaneously authorizes foreign-intelligence collection and limits domestic application. The engine computes per-seat directionality from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. persons are declared beneficiaries (low d, structural subsidy via privacy protection). Foreign persons abroad are declared victims/payers (high d, extraction via authorized surveillance without consent or recourse). The FBI is a payer under the strict reading because it bears the cost of a categorical prohibition on domestic criminal queries. The intelligence community is agenda-setter; its directionality is moderated by its dual position as both the recipient of legal authorization and the target of compliance constraints. No directionality overrides are required because the structural derivation chain (beneficiary/victim declarations plus exit options) accurately captures these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The strict reading prevents mandatrophy mislabeling by preserving both the coordination function (foreign intelligence authorization with judicial oversight) and the asymmetric extraction (foreign-target surveillance). If the constraint were coded as a pure rope, the extraction from foreign targets would be analytically invisible. If coded as a snare, the genuine protective coordination for U.S. persons would be lost. The active enforcement requirement (FISC review, targeting certifications) confirms the constraint is not self-executing natural law. The temporal drift data show theater_ratio rising slowly, which the engine can evaluate for piton conversion risk, but the core coordination function remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fisa_702_reading_location,
    'How does the foreign_target_strict_reading structurally differ from its sibling readings of the same statutory kernel, and what would change if a sibling reading were adopted?',
    'Authoritative judicial adoption of one reading in FISC or Supreme Court opinions, or legislative amendment that clarifies minimization and query standards.',
    'Adoption of incidental_collection_reading would expand the victim set to include U.S. persons and raise base extractiveness; adoption of constitutional_floor_reading would impose a warrant requirement independent of statutory text, potentially overriding the statutory debate entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fisa_702_reading_location, conceptual, 'Kernel reading location and sibling structural delta for FISA 702').

omega_variable(
    minimization_enforcement_gap,
    'Does statutory minimization actually enforce deletion or functional inaccessibility of U.S. person data, or has practice drifted toward access-logging and audit-trail theater?',
    'FISC compliance audits, declassified targeting procedures, whistleblower disclosures, and technical reviews of agency data-retention systems.',
    'If practice has drifted to access restriction, the constraint''s actual operation converges with incidental_collection_reading, raising effective extraction and theater_ratio; if deletion is genuine, the strict reading remains structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_enforcement_gap, empirical, 'Gap between statutory minimization-as-deletion and actual agency practice').

omega_variable(
    foreign_target_misidentification,
    'Are foreign-target determinations sufficiently accurate that the strict reading''s exclusion of U.S. persons holds in practice, or does high misidentification rate sweep U.S. persons into collection?',
    'Independent audit of targeting selectors, post-collection review of upstream collection sets, and FISC reporting on compliance incidents.',
    'High misidentification would mean U.S. persons are effectively in the victim set despite the strict textual limitation, raising the constraint''s effective extractiveness from the rights-holder population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_target_misidentification, empirical, 'Accuracy of foreign-target determinations and U.S. person contamination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fisa_tr_t4, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(fisa_tr_t8, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(fisa_tr_t12, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(fisa_tr_t16, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 16, 0.25).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(fisa_be_t4, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 4, 0.11).
narrative_ontology:measurement(fisa_be_t8, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 8, 0.12).
narrative_ontology:measurement(fisa_be_t12, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 12, 0.13).
narrative_ontology:measurement(fisa_be_t16, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 16, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fisa_702_statutory_text__foreign_target_strict_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the fisa_702_statutory_text kernel, decomposed per the epsilon-invariance principle because the sibling readings assign different epsilon values, victim sets, and enforcement architectures to the same statutory text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
