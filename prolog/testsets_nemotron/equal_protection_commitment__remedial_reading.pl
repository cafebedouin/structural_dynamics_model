% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__remedial_reading, []).

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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection Remedial Reading — Race-Conscious Dismantling of Caste
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   The remedial reading of equal protection treats the Constitution as
 *   forbidding the perpetuation of racial caste and authorizing
 *   race-conscious state action to dismantle subordination. It emerged from
 *   the Warren/Burger Court era (Bakke 1978, Fullilove 1980, Metro
 *   Broadcasting 1990) and was progressively narrowed by Croson (1989),
 *   Adarand (1995), Grutter (2003), Fisher (2013), and effectively foreclosed
 *   in education by SFFA v. Harvard (2023). The constraint is a tangled rope:
 *   it coordinates a genuine anti-subordination function (beneficiaries:
 *   subordinated groups, state remedial agencies) while extracting from
 *   historically privileged applicants and suppressing the colorblind
 *   doctrinal alternative. The extraction is substantial (ε ≈ 0.52) because
 *   the beneficiary/victim structure inverts depending on observer position —
 *   the same doctrinal move that coordinates inclusion for some operates as
 *   exclusion for others. This reading does not describe the colorblind or
 *   diversity readings; those are sibling constraints linked via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.52).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.35).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection Remedial Reading — Race-Conscious Dismantling of Caste").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, 'dcbf23dd-40e5-4515-87ba-7f68ecd7e79a').
narrative_ontology:cs_kernel_codification('dcbf23dd-40e5-4515-87ba-7f68ecd7e79a', formalized).
narrative_ontology:cs_authority_grounding('dcbf23dd-40e5-4515-87ba-7f68ecd7e79a', lineage).
narrative_ontology:cs_interpretation_layer_present('dcbf23dd-40e5-4515-87ba-7f68ecd7e79a').
narrative_ontology:cs_reading_relation('dcbf23dd-40e5-4515-87ba-7f68ecd7e79a', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('dcbf23dd-40e5-4515-87ba-7f68ecd7e79a', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('dcbf23dd-40e5-4515-87ba-7f68ecd7e79a', foundational, anti_subordination_as_constitutional_mandate).
narrative_ontology:cs_axiom_status(anti_subordination_as_constitutional_mandate, holdable).
narrative_ontology:cs_axiom_grounding('dcbf23dd-40e5-4515-87ba-7f68ecd7e79a', anti_subordination_as_constitutional_mandate, deontological).
narrative_ontology:cs_axiom('dcbf23dd-40e5-4515-87ba-7f68ecd7e79a', foundational, caste_perpetuation_forbidden).
narrative_ontology:cs_axiom_status(caste_perpetuation_forbidden, holdable).
narrative_ontology:cs_axiom_grounding('dcbf23dd-40e5-4515-87ba-7f68ecd7e79a', caste_perpetuation_forbidden, deontological).
narrative_ontology:cs_reference_frame('dcbf23dd-40e5-4515-87ba-7f68ecd7e79a', reconstruction_anti_caste_promise).
narrative_ontology:cs_drift_state('dcbf23dd-40e5-4515-87ba-7f68ecd7e79a', post_sffa_2023, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dcbf23dd-40e5-4515-87ba-7f68ecd7e79a', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, state_remedial_agencies).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_groups).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, colorblind_doctrine_adherents).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, anti_subordination_principle).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, caste_system_prohibition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer race-conscious admissions, contracting, and voting remedies. Their institutional authority and budget depend on the remedial reading's legitimacy. They bear political costs when remedies are challenged but control the implementation machinery.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, state_remedial_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Gain access to institutions from which caste excluded them. Their self-concept and political mobilization are fused to the remedial framework — exit means abandoning the institutional gains and the collective identity built around them. They experience the constraint as coordination that makes their inclusion possible.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_groups, beneficiary,
    organized, generational, identity_locked, national).

% Denied admission, contracts, or voting influence when race-conscious criteria favor others. They experience the constraint as extraction — a cost imposed by state policy they cannot individually avoid. Their exit options are limited: leave the jurisdiction, accept the disadvantage, or litigate.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_applicants, payer,
    moderate, biographical, constrained, national).

% Advocate for a constitutional rule that forbids all racial classification. They experience the remedial reading as an active suppression of their preferred constitutional order — their framework is excluded from governing doctrine. They cannot exit the conflict; they must fight within the courts or abandon the project.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, colorblind_doctrine_adherents, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__remedial_reading, colorblind_doctrine_adherents, excluded).

% Defend race-conscious measures on diversity grounds (compelling interest in educational diversity), not remedial grounds. They share the remedial reading's practical conclusions in many cases but diverge on the constitutional theory. They observe the remedial/colorblind conflict from a distinct institutional position — university administrators, corporate diversity officers.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, diversity_proponents, observer,
    institutional, generational, analytical, national).

% The institutional actor that authoritatively adjudicates which reading governs. Since 2023 (SFFA v. Harvard), the remedial reading has been substantially foreclosed in education; the Court's composition determines whether the constraint persists as enforceable doctrine or becomes a contested memory.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, supreme_court_majority, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__remedial_reading, supreme_court_majority, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional basis for state actors to dismantle entrenched racial caste through targeted, race-conscious measures — solving the collective-action problem where no individual actor can unilaterally undo systemic subordination without being undercut by others who do not participate.
% TRANSFER_FUNCTION: Moves institutional access, public contracts, and political representation from historically privileged groups (who held them by virtue of caste) to historically subordinated groups (who were excluded by caste), mediated by state remedial machinery.
% ABSENT_VOICES: Individuals who would be subordinated under a caste system but reject race-conscious remediation as stigmatizing or counterproductive — their voices are structurally excluded because the remedial framework treats group membership as the unit of remedy. Also absent: future generations who will inherit the doctrinal settlement but cannot participate in its making.
% DISAPPEARANCE_RATIONALE: If the remedial reading vanished overnight, race-conscious admissions, contracting set-asides, and voting remedies would lose their constitutional footing. State agencies would dismantle programs. Historically subordinated groups would lose institutional access mechanisms. The colorblind reading would become the sole governing doctrine. The institutional and political landscape would reorganize around formal race-neutrality.
% FOUNDING_PROBLEM: Post-Reconstruction constitutional order failed to dismantle racial caste; formal equality (14th Amendment) coexisted with Jim Crow, redlining, and exclusionary systems that reproduced subordination without explicit racial classifications. The remedial reading was built to give the Constitution teeth against caste perpetuation — to make equal protection a sword against structural subordination, not just a shield against formal classification.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the remedial reading's beneficiaries (state agencies, civil rights organizations) as still live — citing persistent wealth gaps, segregation, and voting dilution. It is attested by colorblind adherents as dead — citing formal legal equality achieved and current disparities as non-caste in origin. Diversity proponents attest it as transformed — the problem persists but the remedial frame is no longer the only or best constitutional vehicle. No neutral arbiter corroborates; the status is structurally contested.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__remedial_reading, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_commitment__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.52) because the constraint moves concrete institutional goods (admissions seats, contracts, voting power) from one identifiable group to another via state power. Suppression is moderate (0.35) — the colorblind alternative is not banned from discourse but is excluded from governing doctrine; the constraint persists through active judicial enforcement of strict scrutiny. Theater ratio is low-moderate (0.22) — the anti-subordination function is real and not merely performative, though the narrowing doctrine has increased the performative share (narrow tailoring rituals). Accessibility collapse is moderate (0.48) — alternatives (colorblind formal equality, diversity rationale) remain conceptually available but are structurally disadvantaged in the remedial framework. Resistance is moderate-high (0.58) — sustained intellectual, political, and litigation resistance from colorblind adherents and skeptical justices.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from the state agency/subordinated-group seats, the constraint computes toward rope/scaffold (coordination function dominates). From the privileged-applicant/colorblind-adherent seats, it computes toward snare/tangled_rope (extraction and suppression dominate). This divergence IS the measurement — the remedial reading's structural asymmetry is that it coordinates for some by extracting from others. The claimed_type (tangled_rope) captures the hybrid nature; per-seat types will vary.
 *
 * DIRECTIONALITY LOGIC:
 *   State remedial agencies are the structural agenda-setters and primary beneficiaries (collect institutional authority, budget, mission — d ≈ 0.1). Historically subordinated groups are identity-locked beneficiaries (gain access, fused to the framework — d ≈ 0.15). Historically privileged applicants are constrained payers (bear direct denial costs, limited exit — d ≈ 0.75). Colorblind doctrine adherents are organized payers excluded from governing doctrine (bear ideological/institutional costs, constrained exit — d ≈ 0.8). Diversity proponents are analytical observers (institutional position, shared conclusions but distinct theory — d ≈ 0.5). Supreme Court majority is dual agenda-setter/observer (authoritatively adjudicates, generational horizon — d varies by composition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (caste perpetuation) is contested as live/dead/transformed. If dead, the constraint persists as mandatrophy — a remedial structure whose justification has atrophied but whose enforcement machinery remains. The 2023 foreclosure in education suggests mandatrophy resolution is underway for that domain; contracting and voting remnants may follow different trajectories. The theater_ratio rise (0.12→0.22) tracks the Goodhart drift: narrow-tailoring rituals replace substantive anti-subordination work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_colorblind_incompatibility,
    'Does the remedial reading''s core premise (anti-subordination as constitutional mandate) logically foreclose the colorblind reading''s core premise (formal colorblindness as constitutional mandate) within a single doctrinal framework, or do they merely compete as rival interpretations?',
    'Doctrinal analysis of whether a court could simultaneously hold both as binding law without contradiction — e.g., could anti-subordination be the rule for remedial measures while colorblindness governs non-remedial state action? Historical test: did any Court era sustain both?',
    'If forecloses, the two readings are mutually exclusive regime choices — the kernel admits no stable dual readings. If coexists_with, the kernel structurally supports persistent doctrinal oscillation. The engine''s foreclosure computation depends on this structural fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_colorblind_incompatibility, conceptual, 'Whether remedial and colorblind readings are logically incompatible or practically rival').

omega_variable(
    subordination_measurement_ambiguity,
    'What counts as ''perpetuation of caste'' vs. ''remedying subordination'' — is there an empirically stable boundary, or does the remedial reading''s scope expand/contract with political coalition?',
    'Longitudinal coding of remedial program scope (admissions, contracting, voting, employment) against independent measures of caste persistence (wealth gaps, segregation indices, political representation). If program scope tracks political coalitions rather than caste metrics, the boundary is politically constructed.',
    'If the boundary is politically constructed, extractiveness is higher (the constraint serves coalition interests). If empirically stable, extraction is the price of coordination. Affects ε trajectory interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subordination_measurement_ambiguity, empirical, 'Whether the remedial reading''s scope is empirically anchored or politically elastic').

omega_variable(
    identity_lock_mechanism_subordinated,
    'Is the identity_locked exit of historically subordinated groups driven by material dependence (institutional access), ideological fusion (collective identity constituted through the remedy), or both? If the remedy ended, would the identity frame persist?',
    'Comparative study of groups that lost remedial protections (e.g., post-SFFA in education, post-Proposition 209 in California) — track whether collective mobilization shifts to alternative frames or demobilizes.',
    'If identity fusion dominates, the constraint''s effective extraction on this group is negative (subsidy) even if material benefits decline — the group experiences the constraint as constitutive. If material dependence dominates, extraction rises sharply when benefits are cut.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_subordinated, empirical, 'Mechanism of identity-locked exit for subordinated groups under remedial reading').

omega_variable(
    kernel_reading_framing_ambiguity,
    'Does the equal_protection_commitment kernel admit a single authoritative framing, or are the three declared readings (colorblind, diversity, remedial) genuinely distinct kernels that have been erroneously unified under one label?',
    'Test whether the readings share a common referent (the 14th Amendment text/history) that generates different ε values, or whether they refer to different constitutional objects entirely (e.g., colorblind reads the text; remedial reads the history; diversity reads institutional practice). If different objects, they are separate kernels.',
    'If separate kernels, the network.affects_constraints links are mis-specified — they should be independent constraint families. If one kernel, the reading_relations and drift_state declarations are the correct structural model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'Whether the three readings share a kernel or constitute distinct constraint families').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_remedial_tr_t1978, equal_protection_commitment__remedial_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement(ep_remedial_tr_t1989, equal_protection_commitment__remedial_reading, theater_ratio, 1989, 0.15).
narrative_ontology:measurement(ep_remedial_tr_t1995, equal_protection_commitment__remedial_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(ep_remedial_tr_t2003, equal_protection_commitment__remedial_reading, theater_ratio, 2003, 0.2).
narrative_ontology:measurement(ep_remedial_tr_t2013, equal_protection_commitment__remedial_reading, theater_ratio, 2013, 0.21).
narrative_ontology:measurement(ep_remedial_tr_t2023, equal_protection_commitment__remedial_reading, theater_ratio, 2023, 0.22).

% Extraction over time
narrative_ontology:measurement(ep_remedial_be_t1978, equal_protection_commitment__remedial_reading, base_extractiveness, 1978, 0.28).
narrative_ontology:measurement(ep_remedial_be_t1989, equal_protection_commitment__remedial_reading, base_extractiveness, 1989, 0.35).
narrative_ontology:measurement(ep_remedial_be_t1995, equal_protection_commitment__remedial_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(ep_remedial_be_t2003, equal_protection_commitment__remedial_reading, base_extractiveness, 2003, 0.48).
narrative_ontology:measurement(ep_remedial_be_t2013, equal_protection_commitment__remedial_reading, base_extractiveness, 2013, 0.52).
narrative_ontology:measurement(ep_remedial_be_t2023, equal_protection_commitment__remedial_reading, base_extractiveness, 2023, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ep_remedial_su_t1978, equal_protection_commitment__remedial_reading, suppression_requirement, 1978, 0.18).
narrative_ontology:measurement(ep_remedial_su_t1989, equal_protection_commitment__remedial_reading, suppression_requirement, 1989, 0.25).
narrative_ontology:measurement(ep_remedial_su_t1995, equal_protection_commitment__remedial_reading, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement(ep_remedial_su_t2003, equal_protection_commitment__remedial_reading, suppression_requirement, 2003, 0.33).
narrative_ontology:measurement(ep_remedial_su_t2013, equal_protection_commitment__remedial_reading, suppression_requirement, 2013, 0.35).
narrative_ontology:measurement(ep_remedial_su_t2023, equal_protection_commitment__remedial_reading, suppression_requirement, 2023, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_commitment kernel decomposes into three constraint stories with distinct ε values and beneficiary/victim structures. The remedial reading (this story) centers anti-subordination coordination with high extraction from privileged groups. The colorblind_reading centers formal neutrality with extraction from subordinated groups (denied remediation). The diversity_reading centers institutional diversity coordination with moderate extraction. All three link to each other via affects_constraints — the upstream doctrinal commitments (e.g., Brown v. Board) influence downstream readings, and Court composition shifts propagate across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_commitment__remedial_reading, institutional, 0.1).
constraint_indexing:directionality_override(equal_protection_commitment__remedial_reading, moderate, 0.75).
constraint_indexing:directionality_override(equal_protection_commitment__remedial_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
