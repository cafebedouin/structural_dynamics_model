% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__living_constitutionalism_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta — Living Constitutionalism Reading (Inherited Due Process Constraint)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint story captures the living constitutionalism reading of
 *   Magna Carta: the 1215 charter and its reissues establish an inherited due
 *   process and lawful restraint that binds all subsequent rulers through
 *   juridical precedent and evolutionary interpretation. The constraint is
 *   not the parchment but the living doctrinal structure that treats Magna
 *   Carta as a continuing constitutional authority — a coordination mechanism
 *   that channels sovereign power through lawful procedure. Royal prerogative
 *   and unbounded executive discretion are the victims (their arbitrary
 *   exercise is constrained); subjects/citizens, the judiciary, and the
 *   parliamentary tradition are beneficiaries (they gain a due process shield
 *   and a stable framework for legitimate authority). The constraint claims
 *   rope type: genuine coordination around inherited restraint with
 *   low-to-moderate extractiveness. The low extraction reflects that the
 *   constraint primarily coordinates — it provides a stable interpretive
 *   framework for lawful authority — while the moderate theater reflects
 *   ceremonial invocations that exceed operational necessity.
 *
 * KEY AGENTS:
 *   - royal_prerogative: Primary victim (institutional/historical) — the unbounded executive claim constrained by due process
 *   - executive_discretion: Secondary victim (institutional/historical) — administrative latitude that must now proceed through lawful procedure
 *   - subjects_citizens: Primary beneficiary (powerless/moderate/organized) — gain due process shield against arbitrary power
 *   - judiciary: Beneficiary and agenda_setter (institutional) — interprets and applies the constraint, gains institutional authority
 *   - parliament_legacy: Beneficiary (institutional) — inherits constraint authority as legislative supremacy grounded in charter tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.18).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.12).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta — Living Constitutionalism Reading (Inherited Due Process Constraint)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional_history/legal_philosophy/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, 'b3bb8486-5e46-4435-bd4f-6243f96997b8').
narrative_ontology:cs_kernel_codification('b3bb8486-5e46-4435-bd4f-6243f96997b8', fixed_text).
narrative_ontology:cs_authority_grounding('b3bb8486-5e46-4435-bd4f-6243f96997b8', lineage).
narrative_ontology:cs_interpretation_layer_present('b3bb8486-5e46-4435-bd4f-6243f96997b8').
narrative_ontology:cs_reading_relation('b3bb8486-5e46-4435-bd4f-6243f96997b8', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3bb8486-5e46-4435-bd4f-6243f96997b8', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('b3bb8486-5e46-4435-bd4f-6243f96997b8', foundational, charter_binds_successors_through_evolutionary_interpretation).
narrative_ontology:cs_axiom_status(charter_binds_successors_through_evolutionary_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('b3bb8486-5e46-4435-bd4f-6243f96997b8', charter_binds_successors_through_evolutionary_interpretation, conventional).
narrative_ontology:cs_axiom('b3bb8486-5e46-4435-bd4f-6243f96997b8', foundational, due_process_is_living_doctrine_not_historical_artifact).
narrative_ontology:cs_axiom_status(due_process_is_living_doctrine_not_historical_artifact, holdable).
narrative_ontology:cs_axiom_grounding('b3bb8486-5e46-4435-bd4f-6243f96997b8', due_process_is_living_doctrine_not_historical_artifact, conventional).
narrative_ontology:cs_axiom('b3bb8486-5e46-4435-bd4f-6243f96997b8', secondary, judicial_interpretation_authoritatively_extends_charter_principles).
narrative_ontology:cs_axiom_status(judicial_interpretation_authoritatively_extends_charter_principles, holdable).
narrative_ontology:cs_axiom_grounding('b3bb8486-5e46-4435-bd4f-6243f96997b8', judicial_interpretation_authoritatively_extends_charter_principles, conventional).
narrative_ontology:cs_reference_frame('b3bb8486-5e46-4435-bd4f-6243f96997b8', charter_liberties_1215).
narrative_ontology:cs_drift_state('b3bb8486-5e46-4435-bd4f-6243f96997b8', contemporary_judicial_review, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b3bb8486-5e46-4435-bd4f-6243f96997b8', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_citizens).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, parliament_legacy).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, executive_discretion).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, due_process_continuity).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, rule_of_law_above_sovereign).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, evolutionary_interpretation_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The historical claim of unbounded royal/executive authority. The constraint extracts from this claim by requiring all exercises of sovereign power to proceed through lawful procedure. It cannot exit the constraint without ceasing to be 'royal prerogative' — the constraint defines the boundary between lawful authority and arbitrary power. Its structural position is the primary target of the constraint's restraining function.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative, payer,
    institutional, civilizational, trapped, universal).

% The practical latitude of executive administration. The constraint does not eliminate discretion but channels it through legal procedure (judicial review, statutory authority, procedural fairness). Executive discretion can exit by accepting legal bounds — becoming 'lawful executive action' rather than 'prerogative' — but this exit transforms its nature. The constraint extracts the cost of procedural compliance from administrative efficiency.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, executive_discretion, payer,
    institutional, generational, constrained, national).

% The people subject to sovereign power. They gain a due process shield: protection against arbitrary detention, seizure, and judgment. Their exit from the constraint's protection is identity-locked — to reject due process is to reject the status of legal subject/citizen itself. They are the primary beneficiaries of the constraint's coordination function: a stable framework of lawful authority.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_citizens, beneficiary,
    powerless, biographical, identity_locked, universal).

% The courts that interpret and apply Magna Carta's due process guarantees. They gain institutional authority and a stable interpretive framework from the constraint. As agenda_setter, they define the boundary between lawful restraint and unlawful prerogative through case law. They can exit by adopting a different interpretive methodology (e.g., originalism, positivism), but this would transform the constraint's operation — their interpretive role is constitutive of the living constitutionalism reading.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary, agenda_setter).

% The parliamentary tradition that inherits Magna Carta's authority as legislative supremacy grounded in charter continuity. It benefits from the constraint's legitimating force: Parliament's law-making power is the modern form of the charter's restraint on arbitrary rule. It can exit by asserting parliamentary sovereignty over the charter (the parliamentary_sovereignty_reading's position) — a mobile exit that redefines rather than abandons the constitutional structure.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, parliament_legacy, beneficiary,
    institutional, civilizational, mobile, national).

% Academic observers who analyze the constraint's doctrinal evolution, historical legitimacy, and contemporary operation. They neither collect from nor pay into the constraint. Their structural position is analytical: they map the reading's coherence, its relationship to sibling readings, and its empirical fidelity to legal practice.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, inherited framework that channels sovereign power through lawful procedure rather than arbitrary will — solving the coordination problem of legitimate authority by establishing a continuous doctrinal lineage from 1215 to present that all parties can reference and rely upon.
% TRANSFER_FUNCTION: Transfers the capacity for arbitrary executive action from the sovereign/executive to a lawful procedural framework. The 'transfer' is not material resource but authority: the sovereign loses the right to act without legal process; the subject gains the right to demand legal process; the judiciary gains the authority to adjudicate the boundary.
% ABSENT_VOICES: The feudal barons who extracted the original charter — their specific grievances (feudal incidents, wardships, marriage rights) are not the constraint's current coordination function. Colonial subjects and imperial populations — Magna Carta's due process was historically denied to them while being invoked as imperial ideology. Modern administrative state architects — who see the constraint as obsolete formalism impeding efficient governance.
% DISAPPEARANCE_RATIONALE: If the living constitutionalism reading vanished overnight, the doctrinal lineage connecting contemporary due process to 1215 would be severed. Judicial review would lose its deepest historical anchor; executive power would lack its most venerable procedural constraint; the 'ancient constitution' argument in constitutional politics would collapse. The world would rearrange toward either parliamentary sovereignty (statutory rights only) or executive dominance (discretion unchecked by evolutionary interpretation).
% FOUNDING_PROBLEM: Arbitrary royal power in 1215: the king's capacity to detain, seize, and judge without lawful process, treating the realm as private domain rather than public trust.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as live by: (1) judicial review case law continuously applying due process to new executive powers (Entick v Carrington, GCHQ, Miller I/II); (2) constitutional scholars outside the beneficiary set (e.g., positivist critics like Dicey, modern administrative law theorists) who acknowledge the constraint's ongoing operational role while disputing its normative grounding; (3) executive branch itself, which litigates the boundary of prerogative rather than denying the constraint's applicability — confirming the problem (arbitrary power) remains live and the constraint (due process) remains the operative response.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).
:- end_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end) because the constraint primarily coordinates — it establishes a stable framework for legitimate authority rather than extracting resources. The extraction that exists is the compliance cost on executive power: the requirement to proceed through lawful procedure rather than arbitrary will. This cost is structural, not predatory. Suppression is low (0.12) because alternatives (arbitrary rule) are not actively suppressed by force; they are delegitimized by the constraint's normative and institutional weight. Theater ratio (0.22) reflects ceremonial invocations (Magna Carta as symbolic totem) that exceed the constraint's operational necessity — the charter is cited in contexts where its doctrinal relevance is marginal. Accessibility collapse (0.25) is low because alternative constitutional frameworks (parliamentary sovereignty, feudal obsolescence) remain live and contest the reading. Resistance (0.35) is moderate: executive power has historically resisted the constraint (Stuart prerogative claims, modern executive privilege assertions) but the constraint persists through institutionalization.
 *
 * PERSPECTIVAL GAP:
 *   The living constitutionalism reading experiences the constraint as rope (coordination around inherited restraint) from the beneficiary seats (subjects, judiciary, parliamentary tradition). The feudal obsolescence reading would experience it as piton (degraded, theatrical) — a historical artifact with no binding force. The parliamentary sovereignty reading would experience it as scaffold (transitional) — its restraints absorbed into statute, the charter itself superseded. The engine computes these per-seat divergences from the structural data: the same constraint, different structural positions, different types. This story authors only the living constitutionalism reading's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (subjects_citizens, judiciary, parliament_legacy) derive d near 0.0 — the constraint subsidizes their position by providing a due process shield and stable authority framework. Victims (royal_prerogative, executive_discretion) derive d near 1.0 — the constraint extracts from their arbitrary exercise by requiring lawful procedure. The directionality is asymmetric: the constraint coordinates for beneficiaries while restraining victims. This is the structural signature of a rope with a victim set — coordination function genuine, extraction present but low and structurally necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (arbitrary royal power) remains live in mutated form (modern executive discretion, emergency powers, administrative state). The mandate has not atrophied; it has evolved. The living constitutionalism reading treats the constraint as continuously adaptive — its justification is the ongoing need to restrain arbitrary power, not a historical settlement. Mandatrophy is not resolved; the constraint remains functionally justified. The theater ratio reflects ceremonial accretion, not functional decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the living constitutionalism reading a distinct constraint from its sibling readings (feudal obsolescence, parliamentary sovereignty), or a different evaluation of the same constraint?',
    'Compare ε values, beneficiary/victim structures, and claimed types across the three readings. If each reading has a stable ε and distinct structural profile, they are distinct constraints per ε-invariance (DP-001).',
    'If distinct, each reading gets its own constraint_id and classification. If same constraint, they are perspectival variants and the engine''s per-seat computation handles the divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s declared readings instantiate separate ε-invariant constraints.').

omega_variable(
    extraction_referent_ambiguity,
    'Does the extraction referent for this reading capture the historical accumulation of executive power that the constraint restrains, or only the current marginal cost of compliance?',
    'Longitudinal comparison of executive prerogative claims before and after Magna Carta''s invocation in key constitutional moments (1628 Petition of Right, 1689 Bill of Rights, 1765 Entick v Carrington, 1976 Bennett v Horseferry Road).',
    'If extraction is measured against historical executive aggrandizement, ε rises (tangled_rope territory). If measured against current compliance cost, ε stays low (rope). This is the ε-referent discipline for kernel-reading stories (OQ-258).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_referent_ambiguity, empirical, 'Whether the reading''s ε measures restraint of historical accumulation or current marginal cost.').

omega_variable(
    parliamentary_absorption_displacement,
    'Does parliamentary absorption of Magna Carta''s restraints constitute displacement (the constraint survives only as statute) or continuation (Parliament inherits the constraint''s authority)?',
    'Track whether Parliament treats charter provisions as repealable statute or as entrenched constitutional principle in judicial review and legislative practice.',
    'If displacement, the living constitutionalism reading''s victim set shrinks (royal prerogative already displaced) and the constraint becomes primarily coordination. If continuation, executive discretion remains a live victim and the constraint retains restraining force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_absorption_displacement, conceptual, 'Whether parliamentary sovereignty absorbs or inherits the constraint''s authority.').

omega_variable(
    executive_discretion_boundary,
    'Where does legitimate executive discretion end and unlawful prerogative begin under this reading''s evolutionary interpretation?',
    'Case law analysis of prerogative power boundaries from Case of Proclamations (1610) through modern judicial review (GCHQ, Miller I/II).',
    'A narrow boundary increases extraction on executive discretion (higher ε, more snare-like). A wide boundary preserves executive flexibility (lower ε, more rope-like). The boundary is the reading''s core interpretive move.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_discretion_boundary, conceptual, 'The structural location of the extraction/coordination boundary within executive power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement(magn_tr_t1297, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1297, 0.18).
narrative_ontology:measurement(magn_tr_t1628, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1628, 0.25).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1689, 0.22).
narrative_ontology:measurement(magn_tr_t1765, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1765, 0.2).
narrative_ontology:measurement(magn_tr_t1976, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1976, 0.22).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1215, 0.35).
narrative_ontology:measurement(magn_be_t1297, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1297, 0.28).
narrative_ontology:measurement(magn_be_t1628, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1628, 0.22).
narrative_ontology:measurement(magn_be_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1689, 0.18).
narrative_ontology:measurement(magn_be_t1765, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1765, 0.16).
narrative_ontology:measurement(magn_be_t1976, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1976, 0.15).
narrative_ontology:measurement(magn_be_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 2024, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1215, 0.45).
narrative_ontology:measurement(magn_su_t1297, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1297, 0.38).
narrative_ontology:measurement(magn_su_t1628, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1628, 0.28).
narrative_ontology:measurement(magn_su_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1689, 0.15).
narrative_ontology:measurement(magn_su_t1765, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1765, 0.12).
narrative_ontology:measurement(magn_su_t1976, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1976, 0.1).
narrative_ontology:measurement(magn_su_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 2024, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__living_constitutionalism_reading, 0.08).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, petition_of_right_1628).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, bill_of_rights_1689).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, habeas_corpus_tradition).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, judicial_review_doctrine).

% DUAL FORMULATION NOTE:
% The magna_carta_constraint_authority kernel decomposes into three ε-invariant constraint stories: (1) feudal_obsolescence_reading — low extraction, mountain-claimed, treats charter as historical artifact; (2) living_constitutionalism_reading — this story, moderate extraction, rope-claimed, treats charter as living due process constraint; (3) parliamentary_sovereignty_reading — variable extraction, scaffold-claimed, treats charter as transitional statute. The living constitutionalism reading influences the parliamentary sovereignty reading (shared doctrinal lineage, different absorption claims) and coexists with the feudal obsolescence reading (different parties hold each; neither logically forecloses the other in a single framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_constraint_authority__living_constitutionalism_reading, institutional, 0.15).
constraint_indexing:directionality_override(magna_carta_constraint_authority__living_constitutionalism_reading, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
