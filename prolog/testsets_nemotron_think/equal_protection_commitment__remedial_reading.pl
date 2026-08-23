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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection Remedial Reading: Anti-Subordination Mandate with Race-Conscious Remediation
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   The remedial reading of equal protection holds that the 14th Amendment's
 *   core commitment is anti-subordination: it forbids the state from
 *   perpetuating racial caste and affirmatively permits race-conscious
 *   measures to dismantle existing hierarchy. This reading powered the Warren
 *   and Burger Courts' desegregation orders, the affirmative action
 *   jurisprudence from Bakke through Grutter, and voting rights remedies. It
 *   now faces sustained attack from the colorblind reading (culminating in
 *   SFFA v. Harvard/UNC) and has been narrowed by the diversity reading's
 *   weaker doctrinal footing. The constraint is a tangled rope: it performs
 *   genuine coordination (dismantling caste) while extracting from
 *   historically privileged groups (denial of preferential access), and
 *   requires active judicial enforcement to persist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.52).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.45).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection Remedial Reading: Anti-Subordination Mandate with Race-Conscious Remediation").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, 'fd8c749d-860d-4316-ae34-803000d7ecc3').
narrative_ontology:cs_kernel_codification('fd8c749d-860d-4316-ae34-803000d7ecc3', formalized).
narrative_ontology:cs_authority_grounding('fd8c749d-860d-4316-ae34-803000d7ecc3', lineage).
narrative_ontology:cs_interpretation_layer_present('fd8c749d-860d-4316-ae34-803000d7ecc3').
narrative_ontology:cs_reading_relation('fd8c749d-860d-4316-ae34-803000d7ecc3', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('fd8c749d-860d-4316-ae34-803000d7ecc3', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('fd8c749d-860d-4316-ae34-803000d7ecc3', foundational, anti_subordination_principle).
narrative_ontology:cs_axiom_status(anti_subordination_principle, holdable).
narrative_ontology:cs_axiom_grounding('fd8c749d-860d-4316-ae34-803000d7ecc3', anti_subordination_principle, deontological).
narrative_ontology:cs_axiom('fd8c749d-860d-4316-ae34-803000d7ecc3', foundational, remedial_race_consciousness_permitted).
narrative_ontology:cs_axiom_status(remedial_race_consciousness_permitted, holdable).
narrative_ontology:cs_axiom_grounding('fd8c749d-860d-4316-ae34-803000d7ecc3', remedial_race_consciousness_permitted, instrumental).
narrative_ontology:cs_reference_frame('fd8c749d-860d-4316-ae34-803000d7ecc3', anti_subordination_constitutionalism).
narrative_ontology:cs_drift_state('fd8c749d-860d-4316-ae34-803000d7ecc3', contemporary_colorblind_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fd8c749d-860d-4316-ae34-803000d7ecc3', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_groups).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, state_actors_implementing_remedial).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, state_actors_legislatures_executive).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, diversity_advocates).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, anti_subordination_principle).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, constitutional_permission_for_remedial_race_consciousness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups historically subjected to caste-like subordination (Black Americans, Native Americans, Latinx communities, etc.). They benefit from race-conscious remedial programs in education, employment, and contracting that open access previously blocked. Their exit from the constraint's benefits would mean returning to unremedied subordination; they cannot individually exit the structural condition the constraint addresses.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_groups, beneficiary,
    organized, generational, constrained, national).

% Groups (primarily white Americans) who hold accumulated advantage from historical caste hierarchy. When remedial programs allocate seats, contracts, or positions on a race-conscious basis, members of these groups experience denial of preferential access they would otherwise enjoy. They can exit by relocating, using private alternatives, or leveraging political power to roll back the constraint — but the constraint's national scope and constitutional grounding make full exit difficult.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_groups, payer,
    powerful, biographical, mobile, national).

% Federal and state courts that interpret and enforce the remedial reading. They define the scope of permissible race-conscious measures, set evidentiary standards for showing past discrimination, and police the boundary between remedial and preferential use of race. They benefit institutionally from being the authoritative interpreters of the constitutional commitment.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, state_actors_courts, agenda_setter,
    institutional, generational, analytical, national).

% Legislatures and executive agencies that design and implement affirmative action, set-asides, voting rights remedies, and school desegregation orders. They gain political capital and governance tools from the remedial reading's permission structure. They are constrained by judicial review and political backlash; they cannot easily exit the obligation to remediate without violating constitutional duty as this reading defines it.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, state_actors_legislatures_executive, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__remedial_reading, state_actors_legislatures_executive, beneficiary).

% Advocates, scholars, and jurists who hold the colorblind reading (equal protection forbids all racial classification). They are structurally excluded from the remedial framework's internal logic — their objection that race-conscious remediation violates equal protection is treated as outside the constitutional conversation by this reading. They cannot exit the constraint's operation because it governs the legal system they operate within.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, colorblind_constitutionalists, excluded,
    organized, generational, trapped, national).

% Institutions and advocates who rely on the diversity reading (race as one factor for educational diversity). They benefit from the remedial reading's broader permission for race-consciousness but face doctrinal tension: diversity is a weaker justification than remediation. They can pivot to diversity-only arguments if remedial doctrine contracts.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, diversity_advocates, observer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__remedial_reading, diversity_advocates, beneficiary).

% Academic commentators who analyze the constraint from outside. They see the full beneficiary/victim structure and the contest with sibling readings. They neither collect nor pay under the constraint but their analyses shape the legitimacy conditions all seats experience.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, legal_scholars_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dismantles caste-like subordination by authorizing state actors to use race-conscious measures (admissions preferences, contracting set-asides, voting rights remedies, school desegregation) that counteract the self-reinforcing dynamics of historical hierarchy. Solves the collective-action problem where no individual actor can unilaterally undo accumulated disadvantage.
% TRANSFER_FUNCTION: Moves access to elite positions (university seats, government contracts, political representation, quality education) from historically privileged groups who hold them via cumulative advantage to historically subordinated groups who were excluded, using race-conscious allocation as the transfer mechanism.
% ABSENT_VOICES: Future generations of subordinated groups who would bear the cost if remedial permission is withdrawn before subordination is dismantled; individuals within historically privileged groups who lack cumulative advantage (poor whites, recent immigrants) but are grouped with the privileged class by the constraint's broad categorical logic; international human rights bodies that view race-conscious remediation as required by treaty obligations.
% DISAPPEARANCE_RATIONALE: If the remedial reading vanished overnight, race-conscious affirmative action, voting rights remedies, and school desegregation orders would lose their constitutional foundation. Subordinated groups would lose access mechanisms; privileged groups would retain cumulative advantage unchecked; state actors would lose authority to remediate. The racial hierarchy would reorganize toward caste entrenchment.
% FOUNDING_PROBLEM: The post-Reconstruction Constitution failed to prevent the re-establishment of racial caste through Jim Crow, racial terrorism, and structural exclusion. The 14th Amendment's equal protection clause was interpreted to permit 'separate but equal' (Plessy) and to strike down economic regulation (Lochner) while ignoring racial subordination. The founding problem of the remedial reading is: how to make equal protection a guarantee against caste perpetuation rather than a formal symmetry that locks in hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: The remedial reading's founding problem is corroborated by: (1) the historical record of Plessy, the Civil Rights Cases, and the Court's abandonment of Reconstruction; (2) the Kerner Commission report (1968) documenting 'two societies, separate and unequal'; (3) contemporary empirical research on racial wealth gaps, school segregation, and health disparities showing caste-like persistence; (4) international human rights bodies (CERD Committee) concluding the US has not fulfilled its obligation to eliminate racial discrimination. The colorblind reading disputes the founding problem's continued relevance, arguing formal equality has been achieved.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness (0.52) reflects the substantial but bounded transfer: race-conscious measures shift meaningful resources but do not expropriate the privileged class's core holdings. Suppression (0.45) is moderate — courts enforce remedial orders but alternatives (race-neutral policies, private action) remain legally available. Theater ratio (0.25) captures performative compliance (institutions adopting diversity rhetoric without structural change) alongside real remedial function. Accessibility collapse (0.60) is elevated because race-neutral alternatives have repeatedly failed to achieve desegregation, but not total — some jurisdictions achieve integration through socioeconomic proxies. Resistance (0.70) is high, reflecting organized political and legal opposition from colorblind advocates and affected privileged groups.
 *
 * PERSPECTIVAL GAP:
 *   From the subordinated groups' seat, the constraint is a rope (coordination against caste with net benefit). From privileged groups' seat, it is a snare (extraction without their consent). From courts' seat, it is a tangled rope they administer — they coordinate the dismantling while managing the extraction's political fallout. The engine computes this divergence from the structural data: beneficiaries (subordinated groups, implementing state actors) have low directionality; payers (privileged groups) have high directionality; excluded colorblind advocates experience the constraint as illegitimate imposition.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: historically subordinated groups (collect remedial access, constrained exit from subordination) and state actors implementing remedial programs (gain governance tools, constrained by judicial review). Victims: historically privileged groups (bear denial of preferential access, mobile but nationally constrained). The directionality derivation assigns low d to beneficiaries (subsidized by constraint), high d to victims (extracted from), analytical d to observers. Colorblind advocates are excluded — their structural position is not captured by beneficiary/victim binary; they experience the constraint as foreign imposition.
 *
 * MANDATROPHY ANALYSIS:
 *   The remedial reading's founding problem (caste perpetuation) remains live per corroborated evidence, but the constraint's mandate has narrowed: Bakke rejected remedial justification for diversity; Croson/Adarand imposed strict scrutiny; Parents Involved limited K-12 remedies; SFFA effectively ended affirmative action in admissions. The constraint persists in voting rights (Section 2) and some employment/contracting contexts. Mandatrophy is contested — not resolved (caste persists) but the remedial toolkit has atrophied. The constraint is not a piton because active enforcement continues in voting rights and the coordination function remains structurally necessary per the reading's own lights.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the remedial reading a distinct constraint from the colorblind and diversity readings, or a doctrinal variation of a single equal protection constraint?',
    'Apply ε-invariance test: if measuring extractiveness under remedial vs colorblind framing yields different ε values for the same observable arrangement, they are distinct constraints. The remedial reading sees high extraction in caste perpetuation; the colorblind reading sees high extraction in race-conscious remediation. Different referents, different ε — distinct constraints.',
    'If distinct, each reading gets its own constraint story with independent classification. If unified, the corpus must model observable-dependent classification (which the framework forbids). This omega documents the decomposition decision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel decomposes into multiple ε-invariant constraints per DP-001.').

omega_variable(
    structural_delta_vs_siblings,
    'Does the remedial reading''s beneficiary/victim structure (state actors as beneficiaries when implementing; privileged groups as victims) genuinely invert relative to the colorblind reading (where privileged groups are beneficiaries of formal equality; subordinated groups are victims of race-conscious classification)?',
    'Compare directionality derivations across the three constraint stories. If the same agent (e.g., white applicants) has d ≈ 0.2 in colorblind reading (beneficiary of formal equality) but d ≈ 0.8 in remedial reading (target of race-conscious remediation), the inversion is structural, not rhetorical.',
    'Confirms the expected structural delta. If inversion fails, the kernel may not decompose cleanly — the readings might share a beneficiary/victim structure with different justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_vs_siblings, empirical, 'Whether the beneficiary/victim structure genuinely inverts across sibling readings.').

omega_variable(
    caste_perpetuation_measurability,
    'Can the ''caste system'' the remedial reading forbids be measured independently of the reading''s own normative framework, or is caste perpetuation defined circularly by the reading?',
    'Independent empirical indicators: racial wealth gap persistence controlling for income/education; residential segregation indices; school segregation trends; health disparity gradients; political representation gaps. If these track the reading''s predictions without assuming its normative premises, the referent is measurable.',
    'If measurable, the reading''s ε assessment of the standing arrangement (high extraction in caste) is empirically grounded. If circular, the reading''s high ε claim is internally coherent but externally unanchored — an omega for the engine''s natural law detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_perpetuation_measurability, empirical, 'Whether the constraint''s referent (caste perpetuation) admits independent measurement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_remedial_tr_t0, equal_protection_commitment__remedial_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ep_remedial_tr_t10, equal_protection_commitment__remedial_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(ep_remedial_tr_t20, equal_protection_commitment__remedial_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(ep_remedial_tr_t30, equal_protection_commitment__remedial_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(ep_remedial_tr_t40, equal_protection_commitment__remedial_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(ep_remedial_tr_t50, equal_protection_commitment__remedial_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(ep_remedial_tr_t60, equal_protection_commitment__remedial_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(ep_remedial_tr_t70, equal_protection_commitment__remedial_reading, theater_ratio, 70, 0.22).

% Extraction over time
narrative_ontology:measurement(ep_remedial_be_t0, equal_protection_commitment__remedial_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ep_remedial_be_t10, equal_protection_commitment__remedial_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(ep_remedial_be_t20, equal_protection_commitment__remedial_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(ep_remedial_be_t30, equal_protection_commitment__remedial_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(ep_remedial_be_t40, equal_protection_commitment__remedial_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(ep_remedial_be_t50, equal_protection_commitment__remedial_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(ep_remedial_be_t60, equal_protection_commitment__remedial_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(ep_remedial_be_t70, equal_protection_commitment__remedial_reading, base_extractiveness, 70, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(ep_remedial_su_t0, equal_protection_commitment__remedial_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ep_remedial_su_t10, equal_protection_commitment__remedial_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(ep_remedial_su_t20, equal_protection_commitment__remedial_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(ep_remedial_su_t30, equal_protection_commitment__remedial_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(ep_remedial_su_t40, equal_protection_commitment__remedial_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(ep_remedial_su_t50, equal_protection_commitment__remedial_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(ep_remedial_su_t60, equal_protection_commitment__remedial_reading, suppression_requirement, 60, 0.45).
narrative_ontology:measurement(ep_remedial_su_t70, equal_protection_commitment__remedial_reading, suppression_requirement, 70, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__remedial_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint (remedial_reading), colorblind_reading, and diversity_reading form the equal_protection_commitment constraint family. All three decompose the single kernel 'equal protection commitment' per ε-invariance: each reading assigns different ε to the standing arrangement (caste perpetuation vs race-conscious classification vs diversity pursuit). The remedial reading has the highest ε (0.52) because it assesses the caste system as the extractive arrangement; the colorblind reading assigns high ε to race-conscious remediation; the diversity reading assigns moderate ε to diversity-justified classification. Network edges reflect doctrinal influence: remedial reading historically influenced diversity reading (Bakke adopted diversity as fallback when remediation was rejected); colorblind reading now influences both by narrowing permissible race-consciousness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_commitment__remedial_reading, institutional, 0.15).
constraint_indexing:directionality_override(equal_protection_commitment__remedial_reading, powerful, 0.85).
constraint_indexing:directionality_override(equal_protection_commitment__remedial_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
