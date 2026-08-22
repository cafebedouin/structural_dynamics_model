% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of
 *   constitutional interpretive authority, where courts hold final say over
 *   the meaning of the constitution and can nullify legislative acts. This
 *   reading is often justified as a necessary safeguard for fundamental
 *   rights and constitutional order. The metrics reflect a system where
 *   judicial power has steadily increased, leading to higher extraction from
 *   the legislative branch and the electorate, and requiring active
 *   enforcement to maintain this hierarchical relationship. The claimed type
 *   is 'tangled_rope' because it provides a coordination function (finality
 *   in constitutional disputes) but also involves significant asymmetric
 *   extraction.
 *
 * KEY AGENTS:
 *   - judiciary: Primary beneficiary and agenda-setter (institutional/identity_locked)
 *   - legislature: Primary target (institutional/constrained)
 *   - electorate: Secondary target (organized/constrained)
 *   - rights_advocacy_groups: Secondary beneficiary (organized/mobile)
 *   - executive_branch: Observer/enforcer (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.7).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, 'f637e142-7264-4982-b1a1-f85bbfa13023').
narrative_ontology:cs_kernel_codification('f637e142-7264-4982-b1a1-f85bbfa13023', fixed_text).
narrative_ontology:cs_authority_grounding('f637e142-7264-4982-b1a1-f85bbfa13023', lineage).
narrative_ontology:cs_interpretation_layer_present('f637e142-7264-4982-b1a1-f85bbfa13023').
narrative_ontology:cs_reading_relation('f637e142-7264-4982-b1a1-f85bbfa13023', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('f637e142-7264-4982-b1a1-f85bbfa13023', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('f637e142-7264-4982-b1a1-f85bbfa13023', foundational, judicial_review_is_inherent_constitutional_power).
narrative_ontology:cs_axiom_status(judicial_review_is_inherent_constitutional_power, holdable).
narrative_ontology:cs_axiom_grounding('f637e142-7264-4982-b1a1-f85bbfa13023', judicial_review_is_inherent_constitutional_power, conventional).
narrative_ontology:cs_axiom('f637e142-7264-4982-b1a1-f85bbfa13023', foundational, judiciary_is_final_arbiter_of_constitutional_meaning).
narrative_ontology:cs_axiom_status(judiciary_is_final_arbiter_of_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('f637e142-7264-4982-b1a1-f85bbfa13023', judiciary_is_final_arbiter_of_constitutional_meaning, deontological).
narrative_ontology:cs_reference_frame('f637e142-7264-4982-b1a1-f85bbfa13023', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('f637e142-7264-4982-b1a1-f85bbfa13023', contemporary_judicial_activism_debate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f637e142-7264-4982-b1a1-f85bbfa13023', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_advocacy_groups).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, electorate).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, judicial_review_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims and exercises final authority in interpreting the constitution, including the power to nullify legislative acts. Benefits from enhanced institutional prestige and control over legal outcomes. Its identity is fused with this guardianship role.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Has its legislative acts subjected to judicial review and potential nullification. Bears the cost of having its democratic will overridden by unelected judges. Its options are to amend the constitution, pass new legislation, or accept judicial rulings.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Experiences the democratic process as potentially subordinate to judicial pronouncements. Benefits from rights protection but pays through reduced direct control over policy via elected representatives. Exit options are limited to electoral change or constitutional amendment.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, electorate, payer,
    organized, biographical, constrained, national).

% Benefit from a powerful judiciary that can enforce constitutional rights against legislative majorities. They use judicial channels to advance their agendas, often bypassing the legislative process. Their influence is amplified by judicial supremacy.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, rights_advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Must enforce judicial rulings, even when they conflict with its policy preferences or legislative agenda. Operates within the framework set by judicial interpretations, but can influence judicial appointments and public discourse.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch, observer,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a final, authoritative arbiter for constitutional disputes, ensuring a consistent interpretation of fundamental law and protecting individual rights from majoritarian overreach.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over the constitution from the democratically elected legislature to the unelected judiciary, along with the power to nullify legislative acts.
% ABSENT_VOICES: Proponents of parliamentary supremacy or popular sovereignty, who would argue that the legislature, as the most representative branch, should have the final say on constitutional meaning. Their voices are often marginalized in systems where judicial review is entrenched.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished, the balance of power would fundamentally shift. The legislature would gain unchecked power to interpret the constitution, potentially leading to rapid policy changes and a re-evaluation of fundamental rights. The legal system would lose its ultimate arbiter, leading to increased inter-branch conflict over constitutional meaning.
% FOUNDING_PROBLEM: To prevent legislative tyranny and protect fundamental rights by establishing an independent body with the authority to ensure all laws conform to the supreme law of the constitution.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil liberties organizations, and a significant portion of the public attest that the problem of potential legislative overreach and rights violations remains live, justifying judicial review. Critics from political science and some legal traditions contest the necessity of judicial supremacy for this purpose, arguing for alternative mechanisms of constitutional enforcement.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the judiciary, an unelected body, effectively controls a significant portion of policy-making by setting constitutional boundaries, often against the expressed will of the legislature. Suppression is also high as the legislature's ability to enact its agenda is constrained by the threat of judicial review, and there are few direct mechanisms for the electorate to override judicial decisions. The theater ratio is low, indicating that the judicial function is genuinely active, though its scope has expanded. The increasing extractiveness and suppression over time reflect the historical trend of judicial power expansion in many constitutional democracies.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this is a necessary 'rope' for constitutional order and rights protection. From the legislature's and electorate's perspective, it can feel like a 'snare' or 'tangled_rope' where their democratic mandate is curtailed. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is a clear beneficiary (d=0.0-0.1) as it gains institutional power and prestige. Rights advocacy groups also benefit (d=0.1-0.2) as they have a powerful avenue to advance their goals. The legislature and electorate are targets (d=0.7-0.9) as their power is curtailed. The executive branch is more symmetric (d=0.5) as it must enforce judicial decisions but also has its own constitutional role.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protecting rights, ensuring constitutional order) is still live, but its operational form (judicial supremacy) is contested. The high extractiveness and suppression suggest that while a coordination function exists, it is intertwined with significant power asymmetry. This prevents mislabeling it as a pure rope, which would ignore the costs borne by the legislature and electorate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_legitimacy_source,
    'Is the judiciary''s claim to final interpretive authority grounded in a genuine constitutional mandate, or is it a self-asserted power that has become entrenched through practice?',
    'Historical analysis of constitutional founding documents and early judicial practice, compared with contemporary legal theory and public acceptance.',
    'If self-asserted, the constraint''s legitimacy is weaker, potentially reclassifying it closer to a snare from the perspective of the legislature and electorate. If mandated, its coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_legitimacy_source, conceptual, 'The source of judicial interpretive authority.').

omega_variable(
    rights_protection_efficacy,
    'Does judicial supremacy genuinely lead to better protection of fundamental rights compared to alternative models of constitutional interpretation?',
    'Comparative empirical studies of rights outcomes in systems with and without judicial supremacy, controlling for other political and social factors.',
    'If rights protection is not demonstrably superior, the primary justification for the extraction from the legislature and electorate is weakened, pushing the classification closer to a snare. If superior, it reinforces the coordination aspect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rights_protection_efficacy, empirical, 'Empirical efficacy of judicial supremacy in rights protection.').

omega_variable(
    democratic_deficit_tolerance,
    'To what extent is a ''democratic deficit'' (unelected judges overriding elected representatives) an acceptable cost for constitutional stability and rights protection?',
    'This is a normative question, resolvable only through societal value judgments and political philosophy, not empirical data.',
    'Societies with a higher tolerance for democratic deficit would view the constraint as more of a rope; those with lower tolerance would view it as more of a snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_deficit_tolerance, preference, 'Societal tolerance for judicial override of democratic will.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1950, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(cons_tr_t1970, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(cons_tr_t1990, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(cons_tr_t2010, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(cons_tr_t2024, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t1950, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(cons_be_t1970, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(cons_be_t1990, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(cons_be_t2010, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(cons_be_t2024, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1950, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(cons_su_t1970, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(cons_su_t1990, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(cons_su_t2010, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(cons_su_t2024, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, legislative_process_rules).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, civil_liberties_enforcement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
