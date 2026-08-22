% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__antisubordination_reading, []).

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
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection Antisubordination Reading
 *   domain: constitutional_law/civil_rights/education_policy
 *
 * SUMMARY:
 *   The antisubordination reading of the Equal Protection Clause treats the
 *   constitutional prohibition as targeting caste-like hierarchy, not racial
 *   classification per se. It authorizes race-conscious state action that
 *   dismantles subordination (affirmative action, voting rights remedies,
 *   school integration) and forbids action that entrenches hierarchy (Jim
 *   Crow, racial gerrymandering, discriminatory policing). This reading
 *   dominated constitutional law from Brown (1954) through the Bakke/Grutter
 *   era but has been progressively narrowed by the Court's colorblind turn
 *   (Parents Involved, SFFA). The constraint now operates in a contested
 *   doctrinal space where its coordinate authority is shrinking.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.22).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.18).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Antisubordination Reading").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "constitutional_law/civil_rights/education_policy").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, 'ed3f2523-613d-4478-a190-c93f7333625c').
narrative_ontology:cs_kernel_codification('ed3f2523-613d-4478-a190-c93f7333625c', formalized).
narrative_ontology:cs_authority_grounding('ed3f2523-613d-4478-a190-c93f7333625c', lineage).
narrative_ontology:cs_interpretation_layer_present('ed3f2523-613d-4478-a190-c93f7333625c').
narrative_ontology:cs_reading_relation('ed3f2523-613d-4478-a190-c93f7333625c', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('ed3f2523-613d-4478-a190-c93f7333625c', equal_protection_kernel__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('ed3f2523-613d-4478-a190-c93f7333625c', foundational, subordination_not_classification_is_the_harm).
narrative_ontology:cs_axiom_status(subordination_not_classification_is_the_harm, holdable).
narrative_ontology:cs_axiom_grounding('ed3f2523-613d-4478-a190-c93f7333625c', subordination_not_classification_is_the_harm, deontological).
narrative_ontology:cs_axiom('ed3f2523-613d-4478-a190-c93f7333625c', foundational, dominant_groups_lack_standing_against_remedies).
narrative_ontology:cs_axiom_status(dominant_groups_lack_standing_against_remedies, holdable).
narrative_ontology:cs_axiom_grounding('ed3f2523-613d-4478-a190-c93f7333625c', dominant_groups_lack_standing_against_remedies, deontological).
narrative_ontology:cs_axiom('ed3f2523-613d-4478-a190-c93f7333625c', secondary, race_consciousness_permitted_for_hierarchy_dismantling).
narrative_ontology:cs_axiom_status(race_consciousness_permitted_for_hierarchy_dismantling, holdable).
narrative_ontology:cs_axiom_grounding('ed3f2523-613d-4478-a190-c93f7333625c', race_consciousness_permitted_for_hierarchy_dismantling, instrumental).
narrative_ontology:cs_reference_frame('ed3f2523-613d-4478-a190-c93f7333625c', reconstruction_antisubordination_promise).
narrative_ontology:cs_drift_state('ed3f2523-613d-4478-a190-c93f7333625c', post_sffa_2023, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ed3f2523-613d-4478-a190-c93f7333625c', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, historically_subordinated_castes).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, civil_rights_institutions).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, dominant_group_members_seeking_colorblind_remedy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, state_actors_implementing_remedies).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, antisubordination_principle).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, constitutional_permission_for_remedial_race_consciousness).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, hierarchy_dismantling_as_compelling_interest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the cumulative weight of caste-like subordination — material disadvantage, social stigma, political marginalization. The antisubordination reading authorizes state action that targets these specific harms. Exit from the subordinated position is not individually available; identity is fused with the caste position such that 'opting out' is conceptually unavailable without structural transformation. They gain from remedial measures but remain vulnerable to backlash.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_subordinated_castes, beneficiary,
    organized, generational, identity_locked, national).

% Litigate, monitor, and enforce antisubordination doctrine through courts, agencies, and advocacy networks. They shape the doctrinal terrain and benefit institutionally from the reading's vitality (funding, relevance, authority). They can pivot to alternative framings (diversity, colorblindness) if the doctrinal wind shifts — their exit is professional, not existential.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, civil_rights_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__antisubordination_reading, civil_rights_institutions, beneficiary).

% Members of historically dominant groups who claim injury from race-conscious remedial measures (admissions, contracting, voting rights remedies). They experience the constraint as extraction: opportunities denied, classifications imposed. Their exit is mobile — they can relocate, litigate, lobby, or absorb the cost. They are not subordinated; they claim the Constitution protects them from the remedy.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, dominant_group_members_seeking_colorblind_remedy, payer,
    powerful, biographical, mobile, national).

% Universities, employers, legislatures, school districts designing and defending race-conscious programs. They bear compliance costs, litigation risk, and political heat. They are constrained by the doctrinal ceiling the reading sets — they cannot exceed what antisubordination permits, but they also cannot easily abandon remedial commitments without institutional credibility loss.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, state_actors_implementing_remedies, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__antisubordination_reading, state_actors_implementing_remedies, payer).

% Advocates, jurists, and organizations committed to the colorblind reading. They are structurally excluded from the antisubordination framework's internal logic — their core premise (classification per se is the harm) is treated as a category error by this reading. They contest the constraint from outside, through courts, legislation, and public argument.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, colorblind_constitutionalists, excluded,
    organized, generational, constrained, national).

% Scholars, historians, and comparative constitutionalists tracking the doctrine's evolution, its empirical effects on subordination metrics, and its stability across regime changes. They do not collect or pay; they map the constraint's structural dynamics.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, legal_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a multi-institutional project of dismantling caste-like hierarchy: authorizes race-conscious state action where it targets subordination, forbids it where it entrenches hierarchy, and provides a doctrinal touchstone for courts, agencies, and civil society to align remedial efforts without perpetual relitigation of first principles.
% TRANSFER_FUNCTION: Moves remedial authority and resource allocation toward historically subordinated castes (admissions slots, contracting set-asides, voting rights protections, school integration resources) and moves the burden of justification onto state actors who classify by race — they must show the classification dismantles rather than entrenches hierarchy. Dominant-group members bear the competitive cost of remedial measures.
% ABSENT_VOICES: Members of subordinated groups who reject race-conscious remedies as stigmatizing or who favor colorblind universalism — their dissent is marginalized within the antisubordination coalition because the reading treats their position as false consciousness or tactical disagreement. Also absent: future generations who will inherit the doctrinal settlement but have no voice in its crafting.
% DISAPPEARANCE_RATIONALE: If the antisubordination reading vanished overnight, the constitutional permission for race-conscious remedial action would collapse to the colorblind or remedial readings. Race-conscious admissions, voting rights remedies, and disparate-impact enforcement would face immediate constitutional challenge. The institutional architecture of civil rights enforcement would reorganize around colorblind or narrow-remedial frameworks. Subordinated castes would lose their primary constitutional shield against structural hierarchy.
% FOUNDING_PROBLEM: Post-Reconstruction constitutional doctrine failed to prevent the re-entrenchment of racial caste through formally race-neutral means (Jim Crow, redlining, mass incarceration). The antisubordination reading emerged to give the Equal Protection Clause teeth against structural hierarchy, not just facial classifications.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Civil Rights Movement (e.g., Tomiko Brown-Nagin, Risa Goluboff) document the NAACP's strategic shift from formal equality to structural subordination claims. Critical race theorists (Crenshaw, Bell, Delgado) articulate the theoretical foundation. The opposing side (colorblind constitutionalists) disputes that the founding problem was ever 'subordination' rather than 'classification' — they cite the Fourteenth Amendment's text and the anti-classification principle of the Civil Rights Act of 1866. No consensus exists outside the benefiting coalition.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).
:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the constraint primarily enables rather than extracts — it authorizes remedial power. The extraction that exists falls on dominant-group members who lose competitive position under remedial measures, and on state actors who bear compliance costs. Suppression is low (0.18) because the constraint does not coerce compliance through force; it enables authority that others may exercise. Theater is moderate (0.35) because the doctrinal apparatus (strict scrutiny, narrow tailoring, compelling interest) performs more work than the actual remedial outcomes justify — the gap between doctrinal ritual and material dismantling of subordination has widened. Accessibility collapse is low (0.32) because alternative frameworks (colorblind, remedial) remain fully available and actively contested. Resistance is moderate-high (0.55) because the colorblind reading has captured the Supreme Court and is actively dismantling the antisubordination architecture.
 *
 * PERSPECTIVAL GAP:
 *   From the subordinated caste seat, the constraint is a shield (mountain-adjacent: it protects against hierarchy). From the dominant-group payer seat, it is a snare (extracts opportunities via race-conscious classifications). From the civil rights institution seat, it is a rope (coordinates remedial action across institutions). From the state implementer seat, it is a tangled rope (genuine coordination function with asymmetric compliance burdens). The engine computes these per-seat types from the structural data; the claimed_type 'rope' reflects the reading's self-understanding as a coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated castes are identity-locked beneficiaries — they cannot exit the caste position, and the constraint's benefits flow to them structurally. Civil rights institutions are agenda-setters with arbitrage-grade exit — they can pivot framings. Dominant-group members claiming injury are mobile payers — they experience extraction but have exit options. State implementers are constrained agenda-setters/payers — they must operate within the doctrinal ceiling. Colorblind constitutionalists are excluded — their core premise is structurally incompatible with this reading. Observers are analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (caste re-entrenchment through formally neutral means) remains live — subordination metrics (wealth gaps, incarceration disparities, health outcomes) persist. But the reading's capacity to address it has atrophied: strict scrutiny has become 'fatal in fact,' remedial authority has been narrowed to diversity-only, and the Court's colorblind turn treats the reading's core permission as suspect. The constraint persists as doctrinal architecture but its remedial engine is degraded. This is not pure mandatrophy (the problem is not gone) but it approaches piton dynamics — the performance of strict scrutiny theater exceeds the material dismantling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordination_vs_classification_boundary,
    'Can the antisubordination/anti-classification distinction be operationally maintained in doctrine, or does every race-conscious measure inevitably function as both remedy and classification?',
    'Empirical study of remedial programs: do they in practice dismantle subordination metrics, or do they primarily reallocate status within the existing hierarchy? Longitudinal tracking of beneficiaries vs. non-beneficiaries within subordinated castes.',
    'If the boundary collapses, the reading''s claimed coordination function (targeted hierarchy-dismantling) is indistinguishable from the colorblind reading''s feared extraction (racial spoils). The constraint would reclassify toward snare or tangled_rope from the dominant-group seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subordination_vs_classification_boundary, conceptual, 'Whether the core conceptual distinction the reading rests on is doctrinally workable').

omega_variable(
    identity_locked_exit_mechanism,
    'What specific mechanism binds subordinated caste members to the caste position — is it material (wealth, geography), relational (social networks, family), ideological (internalized inferiority), or institutional (legal/political structures)?',
    'Comparative analysis of caste-like systems (race in US, caste in India, Burakumin in Japan) to identify which exit barriers are necessary vs. contingent. Intersectional analysis of within-group mobility.',
    'If identity-lock is primarily material, economic redistribution could loosen it — the constraint''s beneficiary structure would shift. If primarily ideological/institutional, the constraint''s authorization of race-conscious remedies may be necessary but insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanism, empirical, 'Mechanism of identity-locked exit for subordinated caste beneficiaries').

omega_variable(
    kernel_framing_underdetermination,
    'Does the equal_protection_kernel have a single discoverable meaning that the three readings approximate, or is the kernel itself constituted by the contest among readings?',
    'Historical analysis of the Fourteenth Amendment''s drafting and ratification: did the framers have a unified antisubordination/anti-classification/remedial intent, or did they deliberately leave the meaning open for future construction?',
    'If the kernel is constituted by the contest, no reading can claim priority — the constraint family is the real object. If the kernel has a discoverable meaning, the reading that best recovers it has structural authority the others lack.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel itself is a stable referent or a contested construct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_kernel__antisubordination_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(equa_tr_t1964, equal_protection_kernel__antisubordination_reading, theater_ratio, 1964, 0.12).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__antisubordination_reading, theater_ratio, 1978, 0.18).
narrative_ontology:measurement(equa_tr_t1995, equal_protection_kernel__antisubordination_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_kernel__antisubordination_reading, theater_ratio, 2003, 0.28).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_kernel__antisubordination_reading, theater_ratio, 2013, 0.32).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__antisubordination_reading, theater_ratio, 2023, 0.35).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1954, 0.05).
narrative_ontology:measurement(equa_be_t1964, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1964, 0.08).
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1978, 0.15).
narrative_ontology:measurement(equa_be_t1995, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1995, 0.18).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2003, 0.2).
narrative_ontology:measurement(equa_be_t2013, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2013, 0.2).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2023, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1954, 0.05).
narrative_ontology:measurement(equa_su_t1964, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1964, 0.08).
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1978, 0.12).
narrative_ontology:measurement(equa_su_t1995, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1995, 0.15).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2003, 0.16).
narrative_ontology:measurement(equa_su_t2013, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2013, 0.17).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2023, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_kernel__antisubordination_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, affirmative_action_doctrine).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, voting_rights_act_section_2).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, school_desegregation_orders).

% DUAL FORMULATION NOTE:
% Part of the equal_protection_kernel constraint family. This reading (antisubordination) treats hierarchy-dismantling as the constitutional touchstone. The colorblind_reading treats classification-per-se as the touchstone. The remedial_reading treats narrowly-tailored remedy for documented exclusion as the touchstone. The three readings share the same kernel (Equal Protection Clause) but instantiate structurally distinct constraints with different beneficiary/victim sets, different ε values, and different type trajectories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_kernel__antisubordination_reading, organized, 0.15).
constraint_indexing:directionality_override(equal_protection_kernel__antisubordination_reading, powerful, 0.75).
constraint_indexing:directionality_override(equal_protection_kernel__antisubordination_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
