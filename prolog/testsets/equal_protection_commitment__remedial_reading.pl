% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection Remedial Reading: State Race-Conscious Dismantling of Caste Subordination
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the REMEDIAL READING of the equal protection
 *   clause — the reading under which equal protection forbids perpetuation of
 *   caste-like subordination and permits (indeed, may require) race-conscious
 *   measures to dismantle it. This is one of three structurally distinct
 *   readings of the same constitutional kernel (equal protection of the
 *   laws). The remedial reading inverts the beneficiary/victim structure
 *   compared to the colorblind reading: under remedial, historically
 *   subordinated groups benefit from targeted programs; under colorblind, the
 *   constraint is inverted and historically privileged groups denied
 *   preferences claim victimhood. The ε-invariance principle requires this to
 *   be authored as a separate constraint from the colorblind and diversity
 *   readings, each with its own beneficiary/victim structure and its own
 *   measured extractiveness, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_groups: Primary beneficiary (organizing principle of the remedial framework); cannot exit racial identity; benefit from targeted programs
 *   - state_remedial_authorities: Agenda-setter and institutional beneficiary (authorized to implement race-conscious programs as constitutional mandate)
 *   - historically_privileged_groups_denied_preferential_access: Direct payer (lose specific opportunities due to remedial allocation)
 *   - colorblind_reading_advocates: Excluded (their core premise is foreclosed by this reading's commitment to subordination remediation)
 *   - courts_and_legislatures: Observer (institutional seat charged with choosing which reading to instantiate)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.52).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.48).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection Remedial Reading: State Race-Conscious Dismantling of Caste Subordination").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional/political").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, 'f5d4e784-2805-4acb-9ce1-6923167f6c64').
narrative_ontology:cs_kernel_codification('f5d4e784-2805-4acb-9ce1-6923167f6c64', formalized).
narrative_ontology:cs_authority_grounding('f5d4e784-2805-4acb-9ce1-6923167f6c64', lineage).
narrative_ontology:cs_interpretation_layer_present('f5d4e784-2805-4acb-9ce1-6923167f6c64').
narrative_ontology:cs_reading_relation('f5d4e784-2805-4acb-9ce1-6923167f6c64', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('f5d4e784-2805-4acb-9ce1-6923167f6c64', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('f5d4e784-2805-4acb-9ce1-6923167f6c64', foundational, equal_protection_requires_subordination_remediation).
narrative_ontology:cs_axiom_status(equal_protection_requires_subordination_remediation, holdable).
narrative_ontology:cs_axiom_grounding('f5d4e784-2805-4acb-9ce1-6923167f6c64', equal_protection_requires_subordination_remediation, deontological).
narrative_ontology:cs_axiom('f5d4e784-2805-4acb-9ce1-6923167f6c64', foundational, state_race_consciousness_justified_for_dismantle).
narrative_ontology:cs_axiom_status(state_race_consciousness_justified_for_dismantle, holdable).
narrative_ontology:cs_axiom_grounding('f5d4e784-2805-4acb-9ce1-6923167f6c64', state_race_consciousness_justified_for_dismantle, deontological).
narrative_ontology:cs_reference_frame('f5d4e784-2805-4acb-9ce1-6923167f6c64', reconstruction_era_subordination_dismantle).
narrative_ontology:cs_drift_state('f5d4e784-2805-4acb-9ce1-6923167f6c64', contemporary_structural_inequality_persistence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f5d4e784-2805-4acb-9ce1-6923167f6c64', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, state_remedial_authorities).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_groups_denied_preferential_access).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, equal_protection_structural_subordination_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, remedial_state_action_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, members of groups subject to systematic legal subordination (slavery, Jim Crow, ongoing segregation and discrimination) are the primary beneficiaries of race-conscious remedial programs. They cannot exit their racial identity; the constraint permits targeted programs that account for and address the structural persistence of subordination. They benefit from admissions preferences, contracting set-asides, and remedial education designed to counteract historical exclusion.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, identity_locked, national).

% Legislatures, courts, and administrative agencies that design and implement race-conscious remedial programs claim authority under this reading to use racial classification as a tool for dismantling caste-like subordination. They benefit institutionally from the legitimacy the remedial reading provides — it authorizes their intervention as constitutional obligation, not mere preference. They can alter or cease remedial programs, but the reading's force is that they should not entirely abandon race-consciousness without demonstrating that subordination is remedied.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, state_remedial_authorities, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__remedial_reading, state_remedial_authorities, beneficiary).

% Individuals from historically non-subordinated racial groups who are denied admission, hiring, or contracting opportunity because of race-conscious remedial programs. Under this reading, they bear the direct cost of the remedial arrangement — they lose opportunities that would otherwise be available. They cannot exit their race; their exclusion from specific benefits is the mechanism by which remedial resources are directed. They contest whether they should bear the burden of addressing historical subordination they did not cause.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_groups_denied_preferential_access, payer,
    powerful, biographical, constrained, national).

% Constitutional scholars, judges, and political actors who hold the colorblind reading of equal protection are structurally excluded from the decision-making process under a remedial-reading regime. They would argue that any state use of racial classification violates equal protection, period — that the Constitution's core commitment is to treat persons as individuals without regard to race. Their exclusion is structural because the remedial reading forecloses their core premise within a single constitutional framework.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, colorblind_reading_advocates, excluded,
    powerful, biographical, mobile, national).

% Constitutional scholars and institutional actors who ground race-conscious classification in educational or institutional diversity interests (rather than remedying past subordination) are partially excluded from legitimacy under the remedial reading. The diversity reading permits race-consciousness but on different doctrinal grounds and with different scope implications. Their voice is not in the room when the remedial reading is dominant; they would argue race-consciousness should extend to contexts where no remedial history exists.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, diversity_reading_advocates, excluded,
    powerful, biographical, mobile, national).

% The institutional actors charged with interpreting and applying the equal protection clause. They observe the contest between readings, interpret precedent, and decide which reading to instantiate in new cases. Their choice to adopt or reject the remedial reading shapes whether race-conscious programs are permissible, and their decisions cascade through institutions.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__remedial_reading, state_remedial_authorities).
narrative_ontology:fixing_cost_class(equal_protection_commitment__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework under which state power can be deployed to dismantle systematic racial subordination through targeted remedial measures — rather than leaving historical exclusion to atrophy unaddressed. The coordination problem is: how can a polity structured on equal protection principles move from a state of entrenched caste-like hierarchy to one of genuine equal status without deliberately accounting for race?
% TRANSFER_FUNCTION: Moves opportunity (admissions, hiring, contracting) from historically privileged groups to historically subordinated groups via race-conscious remedial allocation. The mechanism is preferential treatment for the subordinated group; the cost is borne by members of the privileged group who lose specific opportunities they would otherwise receive.
% ABSENT_VOICES: The colorblind reading advocates are excluded from legitimacy claims under the remedial reading — they would argue the Constitution forbids ANY state racial classification. Non-subordinated individuals who lose specific opportunities due to remedial programs are not organized as a stakeholder voice; they feel the cost individually but do not collectively set terms. Indigenous groups, whose subordination took different legal forms, are often partially absent from remedial programs designed for racial groups constructed under slavery and segregation.
% DISAPPEARANCE_RATIONALE: If the remedial reading disappeared and were replaced by colorblind equal protection, race-conscious remedial programs would lose constitutional foundation, court challenges to existing programs would succeed, state authorities would cease designing targeted interventions, and the perpetuation of caste-like subordination would no longer be seen as a constitutional concern — the subordination would be treated as pre-constitutional fact, not constitutional harm. The entire framework of remedial obligation would vanish.
% FOUNDING_PROBLEM: The Fourteenth Amendment's guarantee of equal protection was written to dismantle slavery and the legal subordination of freed Black Americans. Yet for nearly a century it was read as permitting segregation and explicit racial hierarchy. The remedial reading revives the founding problem: equal protection means what it was written to prevent — it requires active dismantling of caste-like subordination, not merely passive colorblindness toward existing hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Historians and constitutional scholars outside the judiciary — including historians of slavery and segregation (Eric Foner, Dara Strolovitch), remedial-justice advocates, and (conditionally) the NAACP Legal Defense Fund — attest that racial subordination persists in measurable structural form (wealth gaps, educational segregation, criminal justice disparities) and that the founding problem of equal protection (dismantling hierarchy, not merely ceasing to enforce it) remains live. The colorblind reading's advocates contest this, arguing the problem is solved when law stops classifying by race; the remedial reading's advocates argue that colorblindness to ongoing subordination perpetuates hierarchy.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).

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
 *   Extractiveness is moderate-high (0.52 terminal) because the constraint operates as a form of concentrated extraction from privileged groups to subordinated groups — the privileged group bears a measurable cost (lost opportunity), and the subordinated group receives targeted benefit. However, extractiveness is not extreme because: (1) the coordination function (dismantling caste subordination) is genuine and serves a public good, (2) the beneficiary-victim asymmetry is justified by reference to historical wrong and ongoing structural inequality, and (3) alternatives do exist (colorblind equal protection, diversity frameworks). The measurement trajectory shows extractiveness rising steeply from 0.38 (time 0, when remedial reading authority is contested) to 0.50-0.52 (time 30-70, when programs are implemented and challenged). Theater ratio remains low-to-moderate (0.28 terminal), indicating the remedial function is genuine — the programs are not merely performative facades. Suppression requirement rises to 0.48 because implementing remedial programs over colorblind objections requires active enforcement: court decisions striking down colorblind equal protection doctrine, legislative affirmations of remedial authority, and ongoing defense against legal challenges. The constraint persists not because suppression is overwhelming but because the remedial reading commands institutional authority and legitimacy from a coalition of courts, legislatures, and civil-rights advocates.
 *
 * PERSPECTIVAL GAP:
 *   The privileged group's seat experiences this constraint as pure extraction (they lose opportunities, cannot exit their race, and the constraint persists against their legal objections through enforcement machinery); the subordinated group's seat experiences it as coordination (the state acknowledges and addresses structural exclusion from opportunities). This is not mere disagreement about the constraint's effects — it is genuine asymmetry in the constraint's structure. The engine's per-seat computation should reflect this: the privileged-group seat will likely compute snare or tangled-rope-victim, while the subordinated-group seat computes rope-beneficiary. The state authority seat, as agenda-setter, computes the constraint from a position of control and legitimacy — closer to rope or tangled-rope-beneficiary from its position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for historically_subordinated_racial_groups: near 0.0 (full beneficiary) — they benefit from targeted programs, cannot exit race, have identity locked in, and the constraint explicitly permits programs designed for them. Derivation: beneficiary declaration + identity_locked exit + organized power level → low d. Directionality for state_remedial_authorities: near 0.3 (leaning beneficiary) — they are empowered and legitimized by the remedial reading to act, they benefit institutionally from the authority it grants, but they are not pure beneficiaries because they also bear political cost from colorblind advocates and must defend the reading continuously. Derivation: beneficiary + agenda_setter roles + institutional power + mobile exit → moderate-low d (they could theoretically exit by adopting colorblind reading, but the remedial reading's institutional embedding makes that costly). Directionality for historically_privileged_groups_denied_preferential_access: near 0.8 (near full target) — they bear direct cost (lost opportunities), cannot exit race, have constrained exit options (challenge in court, political advocacy, but cannot avoid the constraint's application), and the constraint explicitly allocates away from them. Derivation: victim declaration + identity_locked exit + powerful power (but constrained by constitutional constraint) + national scope → high d. No overrides needed; structural derivation captures directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The remedial reading avoids the mandatrophy trap because the founding problem (dismantling caste-like subordination inherited from slavery and legal segregation) remains live and contested. The constraint is not a zombie — state authorities actively implement remedial programs, courts actively interpret the reading's scope, and the subordinated-group beneficiaries actively advocate for its application. However, the founding problem has a live competitor (the colorblind reading) that denies the mandatrophy: colorblind advocates argue the founding problem is SOLVED (law no longer enforces slavery or Jim Crow, so equal protection is satisfied by colorblindness). The remedial reading's classification as tangled_rope (not piton) depends on this: the constraint persists because it coordinates a genuine function (dismantling subordination) AND extracts asymmetrically (privileged groups bear cost) AND requires active enforcement (defense against colorblind challenges). If the founding problem were universally agreed to be dead, the constraint would degrade into piton — performative invocations of remedial authority with no beneficiary benefiting and no victim opposing, sustained by institutional inertia. That is not yet the case; the constraint remains genuinely contested and genuinely enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_of_subordination,
    'What counts as caste-like subordination requiring remedial race-consciousness? Does it require formal legal hierarchy (slavery, segregation), or does it encompass ongoing structural inequality (wealth gaps, segregated housing, discriminatory policing) even absent explicit legal code?',
    'Statutory or constitutional specification of what conditions trigger remedial authority (e.g., explicit historical subordination vs. measured disparities); court decisions narrowing or expanding the remedial reading''s scope; empirical documentation of whether subordination persists post-remedy.',
    'Narrow specification limits remedial programs to contexts of formal past wrongs (African Americans under slavery/segregation, possibly Native Americans); broad specification extends programs to any group with measurable structural inequality. Narrow cuts extractiveness (fewer programs, less cost to privileged groups); broad raises it (more programs, broader cost incidence).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(specification_of_subordination, conceptual, 'Whether remedial authority applies to formal historical subordination or extends to ongoing structural inequality.').

omega_variable(
    remedy_completeness_threshold,
    'When does subordination cease to require remedial race-consciousness? When legal code is neutral? When measured outcomes equalize? When cultural attitudes shift? When does the remedial reading''s justification end?',
    'Empirical documentation of when gaps narrow, close, or persist despite remedial programs; court decisions declaring subordination remedied and authorizing cessation of race-conscious programs; political agreement on remediation endpoints.',
    'If remedy is deemed complete while structural gaps persist, the remedial reading loses justification and the constraint collapses toward colorblind equal protection. If remedy is deemed incomplete despite formal legal change, remedial programs continue indefinitely, raising extractiveness and perpetuating asymmetry. The disappearance verdict (world_rearranges if remedial reading vanishes) depends on this threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_completeness_threshold, empirical, 'When the founding problem is sufficiently remedied that race-conscious measures are no longer justified.').

omega_variable(
    kernel_reading_contest,
    'Can a single constitutional framework simultaneously hold the remedial reading (race-consciousness is required) and the colorblind reading (race-consciousness is forbidden)? Or does instantiation of one reading logically foreclose the other?',
    'Constitutional jurisprudence settling whether equal protection permits different regimes for remedial vs. forward-looking purposes, or insists on uniform doctrine; legislative action codifying one reading over the other; institutional consensus on the reading''s scope.',
    'If the readings coexist (remedial where justified by past subordination, colorblind elsewhere), the constraint is more tractable but logically unstable. If one reading forecloses the other, the contest outcome determines the constitutional regime entirely — adoption of remedial reading eliminates colorblind legitimacy; adoption of colorblind reading eliminates remedial authority. This omega explains why the remedial and colorblind readings are separate constraints in the corpus, not alternative measurements of the same constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the remedial and colorblind readings are logically compatible within a single constitutional framework, or whether instantiation of one forecloses the other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_commitment__remedial_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(equa_tr_t0, projected).
narrative_ontology:measurement(equa_tr_t10, equal_protection_commitment__remedial_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(equa_tr_t10, observed).
narrative_ontology:measurement(equa_tr_t20, equal_protection_commitment__remedial_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(equa_tr_t20, observed).
narrative_ontology:measurement(equa_tr_t30, equal_protection_commitment__remedial_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(equa_tr_t30, observed).
narrative_ontology:measurement(equa_tr_t40, equal_protection_commitment__remedial_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(equa_tr_t40, observed).
narrative_ontology:measurement(equa_tr_t50, equal_protection_commitment__remedial_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement_basis(equa_tr_t50, observed).
narrative_ontology:measurement(equa_tr_t60, equal_protection_commitment__remedial_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(equa_tr_t60, observed).
narrative_ontology:measurement(equa_tr_t70, equal_protection_commitment__remedial_reading, theater_ratio, 70, 0.28).
narrative_ontology:measurement_basis(equa_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_commitment__remedial_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(equa_be_t0, projected).
narrative_ontology:measurement(equa_be_t10, equal_protection_commitment__remedial_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(equa_be_t10, observed).
narrative_ontology:measurement(equa_be_t20, equal_protection_commitment__remedial_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement_basis(equa_be_t20, observed).
narrative_ontology:measurement(equa_be_t30, equal_protection_commitment__remedial_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement_basis(equa_be_t30, observed).
narrative_ontology:measurement(equa_be_t40, equal_protection_commitment__remedial_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(equa_be_t40, observed).
narrative_ontology:measurement(equa_be_t50, equal_protection_commitment__remedial_reading, base_extractiveness, 50, 0.51).
narrative_ontology:measurement_basis(equa_be_t50, observed).
narrative_ontology:measurement(equa_be_t60, equal_protection_commitment__remedial_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(equa_be_t60, observed).
narrative_ontology:measurement(equa_be_t70, equal_protection_commitment__remedial_reading, base_extractiveness, 70, 0.52).
narrative_ontology:measurement_basis(equa_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_commitment__remedial_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(equa_su_t0, projected).
narrative_ontology:measurement(equa_su_t10, equal_protection_commitment__remedial_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(equa_su_t10, observed).
narrative_ontology:measurement(equa_su_t20, equal_protection_commitment__remedial_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(equa_su_t20, observed).
narrative_ontology:measurement(equa_su_t30, equal_protection_commitment__remedial_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement_basis(equa_su_t30, observed).
narrative_ontology:measurement(equa_su_t40, equal_protection_commitment__remedial_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(equa_su_t40, observed).
narrative_ontology:measurement(equa_su_t50, equal_protection_commitment__remedial_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement_basis(equa_su_t50, observed).
narrative_ontology:measurement(equa_su_t60, equal_protection_commitment__remedial_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement_basis(equa_su_t60, observed).
narrative_ontology:measurement(equa_su_t70, equal_protection_commitment__remedial_reading, suppression_requirement, 70, 0.48).
narrative_ontology:measurement_basis(equa_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__remedial_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the equal_protection_commitment kernel. Each reading has a distinct ε, beneficiary/victim structure, and classification: the remedial_reading instantiated here (tangled_rope, ε ≈ 0.52, subordinated-groups benefit / privileged-groups targeted) forecloses the colorblind_reading (mountain or rope candidate, ε ≈ 0.05-0.15, no racial beneficiaries/victims) and coexists_with the diversity_reading (rope or tangled_rope, ε ≈ 0.30-0.45, educational/institutional beneficiaries). The three readings are not alternative measurements of one constraint; they are distinct constraints sharing a contested kernel. Family links trace through network.affects_constraints; omega variables in each story document the kernel contest and the reading's relationship to siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_commitment__remedial_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
