% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Eternal Marriage Covenant (Temporal Accommodation Reading)
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'temporal accommodation' reading
 *   of the eternal marriage covenant kernel. This reading interprets the 1890
 *   Manifesto as a temporary suspension of the practice of plural marriage
 *   due to federal pressure, without renouncing the underlying eternal
 *   doctrine. The principle remains valid but dormant, awaiting a future time
 *   when political constraints might lift. The constraint functions as a
 *   'piton' because its primary function (avoiding federal persecution) has
 *   largely atrophied, but the doctrine's 'eternal' status is maintained
 *   through theatrical adherence to the suspension, rather than active
 *   renunciation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.35).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.2).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, piton).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Eternal Marriage Covenant (Temporal Accommodation Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology/commitment_system_dynamics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, 'aeec369f-af65-4030-8735-127cda0d56f7').
narrative_ontology:cs_kernel_codification('aeec369f-af65-4030-8735-127cda0d56f7', fixed_text).
narrative_ontology:cs_authority_grounding('aeec369f-af65-4030-8735-127cda0d56f7', lineage).
narrative_ontology:cs_interpretation_layer_present('aeec369f-af65-4030-8735-127cda0d56f7').
narrative_ontology:cs_reading_relation('aeec369f-af65-4030-8735-127cda0d56f7', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('aeec369f-af65-4030-8735-127cda0d56f7', eternal_marriage_covenant__prophetic_override_reading, coexists_with).
narrative_ontology:cs_axiom('aeec369f-af65-4030-8735-127cda0d56f7', foundational, obedience_to_law_of_land_takes_precedence).
narrative_ontology:cs_axiom_status(obedience_to_law_of_land_takes_precedence, holdable).
narrative_ontology:cs_axiom_grounding('aeec369f-af65-4030-8735-127cda0d56f7', obedience_to_law_of_land_takes_precedence, conventional).
narrative_ontology:cs_axiom('aeec369f-af65-4030-8735-127cda0d56f7', foundational, eternal_principles_can_be_temporarily_suspended).
narrative_ontology:cs_axiom_status(eternal_principles_can_be_temporarily_suspended, holdable).
narrative_ontology:cs_axiom_grounding('aeec369f-af65-4030-8735-127cda0d56f7', eternal_principles_can_be_temporarily_suspended, theological).
narrative_ontology:cs_reference_frame('aeec369f-af65-4030-8735-127cda0d56f7', post_manifesto_compliance).
narrative_ontology:cs_drift_state('aeec369f-af65-4030-8735-127cda0d56f7', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('aeec369f-af65-4030-8735-127cda0d56f7', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, church_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, mainstream_members).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, obedience_to_law_of_land).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, divine_law_eternal_nature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the doctrine, publicly upholds the suspension, and teaches that the eternal principle remains valid but dormant. Benefits from maintaining institutional legitimacy and avoiding conflict with secular law.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the church's social acceptance and legal standing, which the accommodation enables. They are not required to practice plural marriage and largely accept the current interpretation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, mainstream_members, beneficiary,
    organized, biographical, mobile, global).

% Adhere to the literal, active practice of plural marriage, viewing the accommodation as a compromise of divine law. They are excommunicated and operate outside the mainstream church, often facing legal and social penalties.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, fundamentalist_groups, excluded,
    powerless, generational, identity_locked, local).

% Enforce anti-polygamy laws. They observe the church's compliance with the law of the land, which the temporal accommodation reading facilitates. Their pressure was the initial cause of the manifesto.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, secular_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the church's relationship with secular law, allowing it to maintain its institutional presence and avoid legal persecution while preserving the theological validity of a core doctrine.
% TRANSFER_FUNCTION: Transfers the burden of legal conflict from the church institution to individual fundamentalist groups, and transfers social legitimacy from the practice of plural marriage to its suspension.
% ABSENT_VOICES: Fundamentalist groups, excommunicated for continuing the practice, are absent from the mainstream discourse. They would argue that the 'eternal principle' demands active obedience, not dormant validity.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the church would either have to actively renounce the doctrine (a major theological shift) or face renewed legal and social conflict over its historical practices, fundamentally altering its relationship with secular society.
% FOUNDING_PROBLEM: The church faced existential legal and social threats from the US federal government due to its practice of plural marriage, including confiscation of property and disenfranchisement of members.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and secular legal scholarship corroborate the severe federal pressure. While the church leadership maintains the 'eternal principle' is still valid, the immediate legal threat that prompted the manifesto is largely resolved for the mainstream church, making the founding problem 'dead' in its original form.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).
:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) as it extracts social and theological consistency from those who might wish to practice plural marriage, but this is diffuse. Suppression is low (0.20) because the church no longer actively suppresses the practice with the same intensity as in the early 20th century; rather, it excommunicates those who continue, effectively externalizing the suppression to secular law. Theater ratio is high (0.60) because the 'suspension' is largely performative; the doctrine is not renounced, and its 'eternal' nature is periodically reaffirmed, maintaining a theological placeholder for a practice that is no longer actively pursued by the mainstream church. The founding problem is 'dead' as the immediate legal threat has passed, but the constraint persists due to institutional inertia and theological commitment to the doctrine's eternal nature.
 *
 * PERSPECTIVAL GAP:
 *   Church leadership views this as a wise, divinely guided accommodation that preserves eternal truth. Fundamentalist groups view it as a betrayal of divine command. Secular authorities see it as a successful enforcement of law. The engine's classification as a piton highlights the gap between the claimed 'eternal' nature and the actual, largely performative, maintenance of the suspension.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership benefits from maintaining institutional legitimacy and avoiding legal conflict (low d). Mainstream members benefit from social acceptance and not being required to practice plural marriage (low d). Fundamentalist groups are excluded and bear the cost of excommunication and legal penalties (high d). Secular authorities are observers whose pressure led to the accommodation, and they benefit from the church's compliance with the law (low d).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to protect the church from federal persecution. While this threat has largely subsided for the mainstream church, the constraint persists due to the theological commitment to the 'eternal' nature of the doctrine. This prevents mislabeling it as a snare (pure extraction) because there isn't a concentrated beneficiary actively profiting from the extraction, but rather a diffuse benefit of institutional stability and theological consistency. It's a piton because the cost of fixing (renouncing the doctrine) is high, but the benefit of maintaining the suspension is diffuse and largely inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''temporal accommodation'' reading of the eternal marriage covenant, or is it better understood as a ''prophetic override'' or ''immutable commandment'' reading?',
    'Analysis of official church statements, historical documents, and theological interpretations regarding the nature of the Manifesto and the status of D&C 132. Specifically, whether the language implies temporary suspension, permanent change, or unyielding command.',
    'If reclassified as ''immutable commandment'', extractiveness and suppression would be higher, and it would likely be a snare. If reclassified as ''prophetic override'', extractiveness might be lower, and it could be a rope or scaffold, depending on the permanence of the override.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''eternal_marriage_covenant'' kernel. This specific reading is ''temporal_accommodation_reading''. Sibling readings include ''immutable_commandment_reading'' and ''prophetic_override_reading''. The disagreement is located in the interpretation of the Manifesto''s theological force and the status of D&C 132.').

omega_variable(
    dormancy_vs_renunciation,
    'Is the ''dormant'' status of the eternal principle a genuine theological position, or a strategic ambiguity to avoid renunciation while complying with secular law?',
    'Examination of internal theological debates, unpublicized doctrinal discussions, and any future shifts in church policy if political conditions were to change. The key is whether the ''dormancy'' is a temporary state or a permanent theological reinterpretation.',
    'If it''s strategic ambiguity, the theater_ratio would be higher, and the constraint would lean more towards a snare, as the ''eternal'' claim would be primarily a cover for institutional self-preservation. If genuine, the piton classification holds, reflecting inertial maintenance of a complex theological position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormancy_vs_renunciation, empirical, 'Ambiguity regarding the theological sincerity of the ''dormant'' status of the eternal principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1890, 0.2).
narrative_ontology:measurement(eter_tr_t1920, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1920, 0.35).
narrative_ontology:measurement(eter_tr_t1950, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1950, 0.45).
narrative_ontology:measurement(eter_tr_t1980, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1980, 0.55).
narrative_ontology:measurement(eter_tr_t2024, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1890, 0.45).
narrative_ontology:measurement(eter_be_t1920, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1920, 0.4).
narrative_ontology:measurement(eter_be_t1950, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1950, 0.38).
narrative_ontology:measurement(eter_be_t1980, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1980, 0.36).
narrative_ontology:measurement(eter_be_t2024, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement(eter_su_t1920, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1920, 0.5).
narrative_ontology:measurement(eter_su_t1950, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(eter_su_t1980, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(eter_su_t2024, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, prophetic_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'eternal_marriage_covenant' kernel. This 'temporal_accommodation_reading' focuses on the suspension of practice without renouncing doctrine. The 'immutable_commandment_reading' asserts the eternal and active requirement of plural marriage, while the 'prophetic_override_reading' emphasizes the living prophet's authority to supersede prior revelation. All three are distinct constraints arising from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
