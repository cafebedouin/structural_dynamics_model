% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__originalist_limitation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__originalist_limitation_reading, []).

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
 *   constraint_id: magna_carta_clause_39__originalist_limitation_reading
 *   human_readable: Magna Carta Clause 39 (Originalist Limitation Reading)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates an 'originalist limitation reading' of
 *   Magna Carta Clause 39, which asserts that the clause's scope is strictly
 *   bounded by the specific royal abuses documented in the 1215 context. It
 *   is a reading that limits the clause's application to the feudal
 *   grievances of the time, rather than interpreting it as a source of
 *   universal or evolving rights. The constraint is claimed as a Tangled Rope
 *   because it coordinates the King's power for the benefit of the barons
 *   while extracting from the King's arbitrary prerogative, requiring active
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.35).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.2).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Magna Carta Clause 39 (Originalist Limitation Reading)").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, 'f2459274-b7e1-4660-92e4-d31c6628e80f').
narrative_ontology:cs_kernel_codification('f2459274-b7e1-4660-92e4-d31c6628e80f', fixed_text).
narrative_ontology:cs_authority_grounding('f2459274-b7e1-4660-92e4-d31c6628e80f', lineage).
narrative_ontology:cs_interpretation_layer_present('f2459274-b7e1-4660-92e4-d31c6628e80f').
narrative_ontology:cs_reading_relation('f2459274-b7e1-4660-92e4-d31c6628e80f', magna_carta_clause_39__liberal_due_process_reading, forecloses).
narrative_ontology:cs_reading_relation('f2459274-b7e1-4660-92e4-d31c6628e80f', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_axiom('f2459274-b7e1-4660-92e4-d31c6628e80f', foundational, clause_39_is_historically_contingent).
narrative_ontology:cs_axiom_status(clause_39_is_historically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('f2459274-b7e1-4660-92e4-d31c6628e80f', clause_39_is_historically_contingent, empirically_contingent).
narrative_ontology:cs_axiom('f2459274-b7e1-4660-92e4-d31c6628e80f', foundational, rights_are_specific_not_universal).
narrative_ontology:cs_axiom_status(rights_are_specific_not_universal, holdable).
narrative_ontology:cs_axiom_grounding('f2459274-b7e1-4660-92e4-d31c6628e80f', rights_are_specific_not_universal, conventional).
narrative_ontology:cs_reference_frame('f2459274-b7e1-4660-92e4-d31c6628e80f', feudal_legal_order_1215).
narrative_ontology:cs_drift_state('f2459274-b7e1-4660-92e4-d31c6628e80f', contemporary_legal_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f2459274-b7e1-4660-92e4-d31c6628e80f', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, barons_of_1215).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, king_john).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The original beneficiaries of Clause 39, who sought protection from King John's arbitrary rule. They gained specific procedural rights and limits on royal power, ensuring their feudal privileges were respected. Their 'exit' from the constraint (i.e., not having it) would mean continued vulnerability to royal abuses.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, barons_of_1215, beneficiary,
    organized, immediate, constrained, national).

% The primary target of the constraint, whose arbitrary power was limited by Clause 39. He bore the cost of having his prerogative curtailed, being forced to adhere to established legal procedures and customs. His 'exit' would have been to ignore the charter, risking further rebellion.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, king_john, payer,
    institutional, biographical, constrained, national).

% Modern legal scholars and judges who advocate for this specific, historically bounded interpretation of Clause 39. They benefit from the intellectual coherence and perceived stability this reading provides to constitutional law, and their professional identity is often tied to this interpretive methodology.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, originalist_jurists, agenda_setter,
    powerful, generational, identity_locked, national).

% Scholars who study the historical context and original intent of Magna Carta. They provide the empirical grounding for this reading, documenting the specific grievances and feudal legal order of 1215. Their role is to analyze, not to enforce or directly benefit from the constraint's operation.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% Under this originalist reading, modern citizens are largely excluded from directly benefiting from Clause 39, as its protections are deemed specific to the 1215 feudal context. They would have no direct recourse to it for contemporary grievances, as the clause is not interpreted as establishing universal rights.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, modern_citizens, excluded,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the King's exercise of power within established feudal customs and specific legal limits, preventing arbitrary actions against the barons and ensuring a degree of predictable governance.
% TRANSFER_FUNCTION: Transfers the right to arbitrary action from the King to the collective body of barons, ensuring due process according to feudal law and custom, thereby limiting royal prerogative.
% ABSENT_VOICES: Modern citizens and liberal jurists, who would argue for an evolving interpretation of Clause 39 that establishes universal individual rights against arbitrary state power, rather than a historically limited one.
% DISAPPEARANCE_RATIONALE: If this originalist interpretation of Clause 39 vanished, the understanding of English constitutional history and the foundational limits on executive power would shift significantly. Legal arguments relying on this specific historical boundedness would lose their force, leading to a reorganization of constitutional discourse.
% FOUNDING_PROBLEM: King John's arbitrary and abusive exercise of royal prerogative, including unjust imprisonment, seizure of property, and denial of traditional feudal rights, which threatened the stability of the feudal order and the barons' customary liberties.
% FOUNDING_PROBLEM_CORROBORATION: Historical documents, contemporary chronicles, and scholarly consensus among legal historians corroborate the specific abuses of King John and the feudal context of 1215. While the principle of limiting executive power remains, King John's specific abuses are no longer a live problem; independent historical analysis supports the shifted-function reading.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_clause_39__originalist_limitation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__originalist_limitation_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__originalist_limitation_reading_tests).
:- end_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.35) because it limits specific abuses of King John, not all royal power, and is not seen as broadly oppressive. Suppression is low (0.20) as the constraint's purpose is to limit royal power, not to suppress subjects, though it requires enforcement to hold. Theater ratio is low (0.10) as this reading treats Magna Carta as a serious, foundational legal text, not a performative one. The metrics are held constant over the interval to reflect the stability of this specific historical interpretation, which anchors itself to the past.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Barons, the constraint was a necessary coordination mechanism to secure their liberties. From King John's perspective, it was an imposition on his royal prerogative. Originalist jurists view it as a faithful interpretation of historical fact, while liberal jurists would see it as unduly restrictive. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Barons of 1215 are the beneficiaries, gaining protection and specific rights. King John is the payer/victim, as his arbitrary power is curtailed. Originalist jurists act as agenda-setters, promoting and applying this reading. Modern citizens are excluded, as this reading does not extend Clause 39's protections to them. This structural asymmetry, combined with active enforcement, supports the Tangled Rope classification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_clause_39_ambiguity,
    'Is Clause 39''s scope truly limited to 1215 grievances, or does its language, even in its original context, allow for broader application or evolution?',
    'Further historical and linguistic analysis of contemporary legal documents and interpretations from the period immediately following Magna Carta''s promulgation.',
    'If broader application is found, the ''originalist limitation reading'' would be weakened, potentially shifting the constraint''s classification towards a more expansive (and potentially more extractive or coordinative) type, or requiring a re-evaluation of its extractiveness against a wider set of royal actions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_clause_39_ambiguity, empirical, 'Ambiguity regarding the precise historical scope of Clause 39.').

omega_variable(
    interpretive_choice_vs_historical_fact,
    'Is the ''originalist limitation reading'' a ''natural'' and unavoidable interpretation of historical fact, or a constructed interpretive choice that serves specific legal and political agendas in contemporary discourse?',
    'Analysis of the historical development of originalist methodologies and their application to Magna Carta, examining whether the methodology itself is a neutral tool or a means to achieve particular legal outcomes.',
    'If it is primarily a constructed choice, the constraint''s perceived legitimacy and ''naturalness'' would decrease, potentially increasing its effective extractiveness for those whose rights are denied by this limited interpretation, and shifting its classification towards a more ''constructed'' type like Snare or Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_choice_vs_historical_fact, conceptual, 'Whether originalist interpretation is a neutral historical reading or a contemporary interpretive choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 1215, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1415, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1415, 0.1).
narrative_ontology:measurement(magn_tr_t1615, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1615, 0.1).
narrative_ontology:measurement(magn_tr_t1815, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1815, 0.1).
narrative_ontology:measurement(magn_tr_t2023, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1215, 0.35).
narrative_ontology:measurement(magn_be_t1415, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1415, 0.35).
narrative_ontology:measurement(magn_be_t1615, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1615, 0.35).
narrative_ontology:measurement(magn_be_t1815, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1815, 0.35).
narrative_ontology:measurement(magn_be_t2023, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 2023, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1215, 0.2).
narrative_ontology:measurement(magn_su_t1415, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1415, 0.2).
narrative_ontology:measurement(magn_su_t1615, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1615, 0.2).
narrative_ontology:measurement(magn_su_t1815, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1815, 0.2).
narrative_ontology:measurement(magn_su_t2023, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 2023, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__originalist_limitation_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Magna Carta Clause 39 kernel, each representing a distinct structural claim about its nature and scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
