% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_constitutional_floor, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Fourth Amendment Constitutional Floor: Warrant Requirement for 702 Communications Queries
 *   domain: constitutional_law/national_security/surveillance
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the contested FISA
 *   Section 702 statutory kernel. The reading asserts that regardless of
 *   statutory foreign intelligence authorization, the Fourth Amendment
 *   constitutionally requires probable cause warrant for government search of
 *   U.S. person communications content. Under this reading, 702 database
 *   queries accessing such content constitute Fourth Amendment searches,
 *   triggering warrant requirement pre-query independent of whether the
 *   collection itself was lawfully targeted at a foreign person. The
 *   structural reframing is decisive: the statute is read as a foreign
 *   intelligence statute, but the constitutional floor elevates it to a
 *   criminal procedure question where warrant protection is non-negotiable.
 *   This reading coexists with two siblings: the
 *   incidental_collection_reading (which permits warrantless query of
 *   incidentally collected U.S. person communications for foreign
 *   intelligence purposes) and the foreign_target_strict_reading (which
 *   constrains collection to lawful foreign targeting but permits
 *   domestic-purposes query access). Each reading produces a different
 *   constraint with a different beneficiary/victim structure, different ε,
 *   and different institutional implications.
 *
 * KEY AGENTS:
 *   - U.S. persons with Fourth Amendment standing: holders of the constitutional right to content protection
 *   - FISA Court: institutional gatekeeper if this reading governs; conducts individualized probable cause review
 *   - Executive foreign intelligence agencies (FBI, CIA, NSA): current operators of 702; face operational restrictions under this reading
 *   - Congress (FISA statutory authority): excluded from the conversation; statutory authority does not override constitutional floor
 *   - Supreme Court Fourth Amendment jurisprudence: observer/authority grounding; Carpenter and Katz precedent cited by proponents
 *   - Civil liberties advocacy: organized beneficiary and observer; presses the constitutional floor interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.18).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, mountain).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Fourth Amendment Constitutional Floor: Warrant Requirement for 702 Communications Queries").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional_law/national_security/surveillance").

domain_priors:emerges_naturally(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, '070bbd0a-bea4-4f5d-abba-65d98c71c4fe').
narrative_ontology:cs_kernel_codification('070bbd0a-bea4-4f5d-abba-65d98c71c4fe', fixed_text).
narrative_ontology:cs_authority_grounding('070bbd0a-bea4-4f5d-abba-65d98c71c4fe', lineage).
narrative_ontology:cs_interpretation_layer_present('070bbd0a-bea4-4f5d-abba-65d98c71c4fe').
narrative_ontology:cs_reading_relation('070bbd0a-bea4-4f5d-abba-65d98c71c4fe', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('070bbd0a-bea4-4f5d-abba-65d98c71c4fe', fisa_702_statutory_text__foreign_target_strict_reading, influences).
narrative_ontology:cs_axiom('070bbd0a-bea4-4f5d-abba-65d98c71c4fe', foundational, fourth_amendment_content_protection_categorical).
narrative_ontology:cs_axiom_status(fourth_amendment_content_protection_categorical, holdable).
narrative_ontology:cs_axiom_grounding('070bbd0a-bea4-4f5d-abba-65d98c71c4fe', fourth_amendment_content_protection_categorical, deontological).
narrative_ontology:cs_axiom('070bbd0a-bea4-4f5d-abba-65d98c71c4fe', foundational, warrant_requirement_non_negotiable_for_u_s_persons).
narrative_ontology:cs_axiom_status(warrant_requirement_non_negotiable_for_u_s_persons, holdable).
narrative_ontology:cs_axiom_grounding('070bbd0a-bea4-4f5d-abba-65d98c71c4fe', warrant_requirement_non_negotiable_for_u_s_persons, deontological).
narrative_ontology:cs_reference_frame('070bbd0a-bea4-4f5d-abba-65d98c71c4fe', fourth_amendment_original_protective_intent).
narrative_ontology:cs_drift_state('070bbd0a-bea4-4f5d-abba-65d98c71c4fe', post_snowden_702_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('070bbd0a-bea4-4f5d-abba-65d98c71c4fe', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, u_s_persons_with_fourth_amendment_standing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_advocacy).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_foreign_intelligence_agencies).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_categorical_protection_doctrine).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, warrant_requirement_content_access_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% U.S. citizens and permanent residents whose electronic communications are subject to foreign intelligence collection. Under this reading, they possess Fourth Amendment standing to challenge warrantless 702 queries into their communications. Their communications content is constitutionally protected; access requires individualized probable cause and judicial warrant.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, u_s_persons_with_fourth_amendment_standing, beneficiary,
    moderate, generational, trapped, national).

% Under this reading, the FISA Court's mandate expands from approving collection targeting of foreign persons to conducting individualized probable cause review of any 702 query touching U.S. person communications. The court must apply Fourth Amendment standards pre-query, not post-collection. This reading imposes additional administrative burden and constitutional gatekeeping authority.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, agenda_setter,
    institutional, generational, analytical, national).

% FBI, CIA, NSA, and other foreign intelligence agencies that rely on Section 702 collection. This reading restricts operational speed and scope: they cannot query 702 databases without FISA Court individualized probable cause findings. Their current practice of querying for foreign intelligence purposes without per-query warrant authority is reclassified as unconstitutional search.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_foreign_intelligence_agencies, payer,
    institutional, biographical, constrained, global).

% Congress enacted FISA Section 702 to permit collection targeting foreign persons outside U.S. territory without individual warrants. Under this reading, statutory authority does not override Fourth Amendment constraints; Congress would need to amend the statute to align with constitutional floor or accept that 702 cannot constitutionally accommodate warrantless U.S. person query access.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, congress_fisa_statutory_authority, excluded,
    institutional, generational, analytical, national).

% Judicial authority grounding Fourth Amendment doctrine. This reading asserts that Supreme Court precedent (Katz, Carpenter, et al.) establishes that accessing communications content is a Fourth Amendment search requiring warrant protection. The constraint is a constitutional floor, not a policy choice.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, supreme_court_fourth_amendment_jurisprudence, observer,
    institutional, civilizational, analytical, universal).

% Civil liberties organizations assert this reading reflects settled Fourth Amendment law and press litigation, legislative testimony, and public advocacy for warrant-requirement enforcement. They benefit from the constraint's vindication; they also serve as analytical observers documenting the gap between statutory practice and constitutional requirement.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_advocacy, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_advocacy, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading does not describe a coordination arrangement in the Deferential Realism sense. It asserts a constitutional boundary: the Fourth Amendment operates as a structural legal principle that constrains the authority structure itself. The coordination is vertical (constitutional constraint on executive action), not horizontal (among parties solving a collective problem).
% TRANSFER_FUNCTION: No extraction is transferred under this reading — it is precisely the negation of transfer. The constraint asserts that U.S. person communications content cannot be accessed by government search without warrant, regardless of foreign intelligence justification. The prohibited transfer is executive access to protected content without individualized probable cause.
% ABSENT_VOICES: Foreign intelligence targets (non-U.S. persons abroad) are not present in this conversation; their collection regime is lawful under this reading provided the targeting is lawfully authorized. Domestic law enforcement agencies arguing for warrantless content access to investigate U.S. persons would object to the scope of the warrant requirement but are not specifically excluded — they would articulate opposition to the constitutional floor itself.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared — if the Fourth Amendment warrant requirement ceased to apply to 702 queries — the world does not rearrange; the Fourth Amendment itself did not disappear. This reading is not a contingent arrangement but an assertion of pre-existing constitutional law. The disappearance would represent constitutional violation, not institutional reorganization.
% FOUNDING_PROBLEM: The founding problem is the structural constitutional question: whether the Fourth Amendment applies to digital-era government searches of communications content, and whether statutory foreign intelligence collection authority can override that constitutional protection for U.S. persons.
% FOUNDING_PROBLEM_CORROBORATION: The executive branch (ODNI, DOJ, FISA Court current practice) contests that the Fourth Amendment warrant requirement applies pre-query to 702 operations on foreign intelligence ground. Civil liberties organizations, technology researchers, some former FISA Court judges, and U.S. Senator Ron Wyden (from outside the executive beneficiary set) assert the constitutional floor is clear. The constitutional text (Fourth Amendment) and Supreme Court precedent (Carpenter v. United States on digital searches) are cited by both; the disagreement is whether statutory authorization overrides constitutional floor — a hermeneutic contest, not an empirical one.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, world_unchanged).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__constitutional_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__constitutional_floor_reading, 0.25, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is measured at 0.25 — substantially lower than either sibling reading. This reflects the constitutional reading's logic: the constraint asserts a floor, not an extraction mechanism. The 0.25 value represents the compliance cost imposed on the executive (operational slowdown, heightened scrutiny, reduced speed/secrecy) relative to what the executive would prefer (unrestricted 702 query access). This is not rent extraction from a target population but rather a constitutional cost imposed on institutional actors who would prefer faster/less-constrained intelligence operations. Suppression is low (0.18) because the constraint relies on legal doctrine (Fourth Amendment text, Supreme Court precedent), not on coercive enforcement of an unpopular arrangement; the mechanism is judicial (warrant requirement) not police-power suppression. Theater ratio is minimal (0.12) because the constraint is framed as a constitutional principle, not as performative compliance — the warrant requirement is meant to be substantive, not theatrical (though critics argue that FISA Court approval rates undermine the substantive check). Accessibility collapse is moderate (0.35) because alternatives do exist from a Fourth Amendment standpoint: agencies could conduct intelligence collection without querying U.S. person content, could seek traditional warrants, or could comply with the constitutional floor. The constraint does not collapse alternatives completely; it constrains the permitted action set. Resistance is high (0.72) because the executive, congressional authorizers, and FISA Court current practice all resist this reading; the constraint asserts what constitutional law requires against active institutional resistance. Measurements show flat trajectory across the interval: the constitutional principle does not drift over time; extractiveness, suppression, and theater ratios remain stable because the constraint's referent (the standing arrangement under contest) is the constitutional floor itself, not a contingent institutional practice.
 *
 * PERSPECTIVAL GAP:
 *   From the executive agencies' seat, this reading is a constraint that narrows their operational freedom: they experience it as imposing warrant-requirement overhead on every 702 query touching U.S. person communications. From the U.S. persons' seat, it is a constitutional protection — a floor, not an extraction or overhead. From the FISA Court's seat, it is an expansion of institutional authority: warrant review becomes substantive constitutional gatekeeping rather than targeted-collection approval. These perspectival gaps are structural, not mere opinion differences — the engine computes per-seat classification from power (institutional vs. powerless), exit (trapped vs. mobile), and the base metrics. Each seat should derive a different type from the same structural data; that divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for executive intelligence agencies runs toward target status (d ≈ 0.75): they bear the constraint's costs (operational slowdown, warrant requirement compliance, judicial scrutiny). They cannot exit; they are trapped by both the constitutional requirement and their institutional mandate to operate within law. U.S. persons beneficiaries sit at d ≈ 0.25 (toward beneficiary end): the constraint subsidizes their protection; they bear minimal cost from its operation. FISA Court sits near symmetric (d ≈ 0.5): it administers the warrant-requirement gate; its workload increases but its institutional authority is vindicated. No override needed; the structural derivation captures the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is claimed as mountain — constitutional law that emerges naturally from the Fourth Amendment text and does not depend on ongoing institutional maintenance. However, beneficiaries are declared (U.S. persons with Fourth Amendment standing) and vindicated propositions are named (fourth_amendment_categorical_protection_doctrine, warrant_requirement_content_access_principle). The false summit detection signature will fire: identifiable beneficiaries (holders of Fourth Amendment protection) exist on a claimed mountain. The tension is genuine: is the Fourth Amendment a natural law (mountain) or a constructed legal doctrine that benefits those it protects and whose preservation depends on continued judicial enforcement (false summit/tangled rope)? The omega variables route this ambiguity. The mandatrophy analysis resolves in favor of mountain-with-false-summit-candidate: the constraint is constitutional law, which is law (constructed), but law grounded in a foundational text (Bill of Rights) that creates a durable floor independent of any party's preference. The vindicated propositions (fourth_amendment_categorical_protection_doctrine) are not actors collecting rents; they are principles. The beneficiaries (U.S. persons) do not collect anything; they are protected. The doctrine persists because courts enforce it as law, not because beneficiaries extract value from its operation. FSM classification is resolved by reading: if the Fourth Amendment is constructed doctrine that benefits those it protects (one reading), FSM fires and the constraint reclassifies to tangled_rope. If it is constitutional law that operates as a floor regardless of preferences (another reading), it remains mountain. This reading instantiates the latter; the sibling readings would instantiate the former, producing different constraint stories.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_floor_vs_statutory_override,
    'Does Fourth Amendment protection constitute a non-waivable constitutional floor on all government searches, or can statutory authorization override it when the statute serves a compelling interest (foreign intelligence) outside ordinary criminal investigation?',
    'Supreme Court ruling on Section 702 constitutionality, or constitutional amendment reframing the scope of Fourth Amendment. The constitutional text and Carpenter precedent are available now; the resolution depends on judicial interpretation of their scope.',
    'If Fourth Amendment is non-waivable floor: this reading governs, ε ≈ 0.25, mountain/true constitutional law. If statutory override is permitted: incidental_collection_reading or foreign_target_strict_reading governs, ε shifts upward, constraint becomes tangled_rope or rope (statutory coordination of collection and query authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_floor_vs_statutory_override, conceptual, 'Whether Fourth Amendment operates as an absolute floor or as a principle subject to statutory override under compelling circumstances.').

omega_variable(
    false_summit_mountain_ambiguity,
    'Is the Fourth Amendment a natural constitutional law (mountain) or a constructed legal doctrine whose preservation depends on institutional enforcement and benefits identifiable holders of rights (false summit)?',
    'Philosophical/conceptual: if constitutional law is human-constructed doctrine grounded in institutional enforcement, the Fourth Amendment is not a mountain despite its grounding in text; it is tangled_rope (coordination of protection rights with enforcement costs). If constitutional law operates as a floor independent of enforcement preference, it is mountain.',
    'False summit classification would reclassify the constraint to tangled_rope, shifting beneficiary focus to institutional actors (courts enforcing the doctrine) and introducing victim set (executive agencies bearing operational costs). Mountain classification keeps the constraint as a constitutional principle with minimal extraction logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_mountain_ambiguity, conceptual, 'Whether constitutional law doctrine is natural law or constructed institutional practice.').

omega_variable(
    fisa_court_warrant_review_feasibility,
    'Is individualized probable cause review feasible for every 702 query, or does the scale of 702 collection (millions of queries annually) render the warrant requirement administratively impossible?',
    'FISA Court workload analysis, congressional testimony on query volume, feasibility studies from civil liberties organizations and executive agencies.',
    'If feasible: the constraint can be operationalized; warrant requirement stands. If infeasible: the constraint becomes performative (theater_ratio increases); institutional resistance may lead to statutory amendment or judicial narrowing of the warrant scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fisa_court_warrant_review_feasibility, empirical, 'Whether the constitutional floor is administratively implementable given collection scale.').

omega_variable(
    reading_vs_sibling_empirical_disagreement,
    'What is the empirical ground for privileging this (constitutional_floor) reading over the siblings? The contest is partly legal (hermeneutic) but partly empirical: do 702 queries produce foreign intelligence of higher value at significantly lower constitutional cost than traditional warrant-required search?',
    'Classified FISA Court data on query productivity, intelligence value assessment, and comparison to warrant-required collection efficacy.',
    'If warrant requirement substantially degrades foreign intelligence effectiveness: executive resistance hardens, statutory amendment becomes more likely. If 702 query access produces marginal intelligence value above warrant-required collection: the constitutional floor reading gains operational plausibility; FSM and mandatrophy classifications shift toward acceptance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_sibling_empirical_disagreement, empirical, 'Comparative intelligence value of 702 query access versus warrant-required collection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_702_const_floor_tr_t0, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(fisa_702_const_floor_tr_t0, observed).
narrative_ontology:measurement(fisa_702_const_floor_tr_t4, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 4, 0.11).
narrative_ontology:measurement_basis(fisa_702_const_floor_tr_t4, observed).
narrative_ontology:measurement(fisa_702_const_floor_tr_t8, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement_basis(fisa_702_const_floor_tr_t8, observed).
narrative_ontology:measurement(fisa_702_const_floor_tr_t12, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement_basis(fisa_702_const_floor_tr_t12, observed).
narrative_ontology:measurement(fisa_702_const_floor_tr_t16, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement_basis(fisa_702_const_floor_tr_t16, observed).
narrative_ontology:measurement(fisa_702_const_floor_tr_t20, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(fisa_702_const_floor_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(fisa_702_const_floor_be_t0, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(fisa_702_const_floor_be_t0, observed).
narrative_ontology:measurement(fisa_702_const_floor_be_t4, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 4, 0.24).
narrative_ontology:measurement_basis(fisa_702_const_floor_be_t4, observed).
narrative_ontology:measurement(fisa_702_const_floor_be_t8, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 8, 0.26).
narrative_ontology:measurement_basis(fisa_702_const_floor_be_t8, observed).
narrative_ontology:measurement(fisa_702_const_floor_be_t12, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement_basis(fisa_702_const_floor_be_t12, observed).
narrative_ontology:measurement(fisa_702_const_floor_be_t16, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement_basis(fisa_702_const_floor_be_t16, observed).
narrative_ontology:measurement(fisa_702_const_floor_be_t20, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement_basis(fisa_702_const_floor_be_t20, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fisa_702_statutory_text__constitutional_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__constitutional_floor_reading, 0.08).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of FISA Section 702 statutory authority. The kernel is the statute itself (fixed text); the readings decompose into separate constraints with different ε values, beneficiary/victim structures, and institutional implications. Constitutional_floor_reading (this constraint) asserts Fourth Amendment warrant requirement pre-query (ε ≈ 0.25, mountain); foreign_target_strict_reading constrains collection to lawful targeting but permits U.S. person incidental query (ε ≈ 0.45, rope); incidental_collection_reading permits warrantless query of incidentally collected U.S. person communications for foreign intelligence purposes (ε ≈ 0.65, tangled_rope). Each reading produces a different constraint family member with different per-seat classifications. The network edges establish family kinship: changes in constitutional floor enforcement create structural pressure on foreign targeting constraints and incidental collection authorization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
