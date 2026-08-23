% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: Equal Protection Clause — Antisubordination Reading
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the antisubordination reading of the
 *   Equal Protection Clause — the view that the Fourteenth Amendment targets
 *   caste-like subordination of historically oppressed groups, not racial
 *   classification per se. Under this reading, state action that entrenches
 *   racial hierarchy is constitutionally forbidden, while state action that
 *   dismantles it is permitted (and sometimes required). The reading emerged
 *   from Reconstruction's anti-caste purpose, was central to the mid-century
 *   civil rights movement's constitutional vision, and now contests with
 *   colorblind and remedial readings for doctrinal dominance. The constraint
 *   is not the Amendment text itself but this specific interpretive
 *   commitment, which generates a distinct pattern of permissions,
 *   prohibitions, and cost allocations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.42).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.58).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Clause — Antisubordination Reading").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, '234dbb72-2b65-45dc-9c86-f2ed46846fc1').
narrative_ontology:cs_kernel_codification('234dbb72-2b65-45dc-9c86-f2ed46846fc1', formalized).
narrative_ontology:cs_authority_grounding('234dbb72-2b65-45dc-9c86-f2ed46846fc1', lineage).
narrative_ontology:cs_interpretation_layer_present('234dbb72-2b65-45dc-9c86-f2ed46846fc1').
narrative_ontology:cs_reading_relation('234dbb72-2b65-45dc-9c86-f2ed46846fc1', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('234dbb72-2b65-45dc-9c86-f2ed46846fc1', equal_protection_kernel__remedial_reading, influences).
narrative_ontology:cs_axiom('234dbb72-2b65-45dc-9c86-f2ed46846fc1', foundational, anti_subordination_principle).
narrative_ontology:cs_axiom_status(anti_subordination_principle, holdable).
narrative_ontology:cs_axiom_grounding('234dbb72-2b65-45dc-9c86-f2ed46846fc1', anti_subordination_principle, deontological).
narrative_ontology:cs_axiom('234dbb72-2b65-45dc-9c86-f2ed46846fc1', foundational, remedial_measures_permitted).
narrative_ontology:cs_axiom_status(remedial_measures_permitted, holdable).
narrative_ontology:cs_axiom_grounding('234dbb72-2b65-45dc-9c86-f2ed46846fc1', remedial_measures_permitted, instrumental).
narrative_ontology:cs_axiom('234dbb72-2b65-45dc-9c86-f2ed46846fc1', secondary, dominant_groups_no_standing_against_remediation).
narrative_ontology:cs_axiom_status(dominant_groups_no_standing_against_remediation, holdable).
narrative_ontology:cs_axiom_grounding('234dbb72-2b65-45dc-9c86-f2ed46846fc1', dominant_groups_no_standing_against_remediation, deontological).
narrative_ontology:cs_reference_frame('234dbb72-2b65-45dc-9c86-f2ed46846fc1', reconstruction_anti_caste_principle).
narrative_ontology:cs_drift_state('234dbb72-2b65-45dc-9c86-f2ed46846fc1', contemporary_colorblind_ascendancy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('234dbb72-2b65-45dc-9c86-f2ed46846fc1', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, subordinated_castes).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, dominant_groups).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, anti_subordination_principle).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, reconstruction_amendment_purpose).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically oppressed groups (Black Americans, Indigenous peoples, other caste-subordinated populations) who bear the ongoing effects of caste-like subordination. The constraint protects them from state action that entrenches hierarchy and permits race-conscious remedial measures. Their exit from subordination is structurally blocked — identity-locked because caste position is not voluntarily chosen and cannot be exited individually; collective political mobilization is the only exit path, which the constraint both enables and depends on.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, subordinated_castes, beneficiary,
    organized, generational, identity_locked, national).

% Groups positioned at the top of the racial hierarchy (primarily white Americans) who benefit from inherited status advantages and institutional arrangements that reflect historical dominance. Under this reading, they cannot invoke equal protection to block remedial measures or to maintain hierarchy-entrenching state action. They bear the cost of lost legal tools to preserve hierarchical arrangements. Exit is mobile — they can individually disavow racist ideology, but the structural position and its material benefits persist regardless of individual belief.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, dominant_groups, payer,
    powerful, biographical, mobile, national).

% Federal and state legislative/executive branches that enact laws and policies. Under this reading, the state is forbidden from acting to entrench caste hierarchy but permitted (and sometimes obliged) to act to dismantle it. The state administers the constraint through policy choices — its enforcement machinery is the legislative and executive branch themselves, constrained by judicial review.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, state_government, agenda_setter,
    institutional, generational, analytical, national).

% The judiciary, especially the Supreme Court, which authoritatively interprets the Equal Protection Clause. Courts enforce the constraint by striking down hierarchy-entrenching state action and upholding subordination-dismantling measures. They sit in a dual position: agenda_setter when issuing binding precedent, observer when analyzing the constraint's operation from outside the political branches. Their institutional legitimacy depends on maintaining the anti-subordination principle against competing readings.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__antisubordination_reading, federal_courts, observer).

% Advocates and jurists who hold the colorblind reading — that the Constitution categorically forbids all racial classifications. They are structurally excluded from the antisubordination reading's framework because their core premise (all race-consciousness is forbidden) is foreclosed by this reading's core premise (race-consciousness to dismantle subordination is required). They would object to race-conscious remedial measures but have no seat in this constraint's internal logic.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, colorblind_advocates, excluded,
    organized, biographical, mobile, national).

% Advocates and jurists who hold the remedial reading — that race-conscious action is permitted only when narrowly tailored to remedy specific documented historical exclusion or achieve compelling diversity interests. They occupy an adjacent but distinct position: they share the antisubordination reading's permission for some race-conscious measures but reject its broader anti-caste scope. They observe this constraint's operation from a neighboring framework.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, remedial_advocates, observer,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable constitutional principle for distinguishing hierarchy-entrenching state action (forbidden) from hierarchy-dismantling state action (permitted), enabling courts and political branches to adjudicate equal protection claims without collapsing into either colorblind formalism or unbounded racial balancing.
% TRANSFER_FUNCTION: Transfers the legal capacity to use racial classifications from dominant groups (who lose the ability to invoke equal protection against remedial measures or to maintain hierarchy through state action) to subordinated castes (who gain constitutional protection against subordination and access to race-conscious remediation). The transfer is not monetary but doctrinal — it reallocates the 'equal protection' shield.
% ABSENT_VOICES: Colorblind advocates are structurally excluded — their reading is foreclosed by this constraint's core premise. Also absent: those who would define subordination purely economically (class-only frameworks) and those who reject the caste analogy entirely. These voices are not in the conversation because the antisubordination reading defines the constitutional subject as caste-subordinated groups, not economically disadvantaged individuals or abstract colorblind citizens.
% DISAPPEARANCE_RATIONALE: If the antisubordination reading vanished overnight, the constitutional framework would revert to either colorblind formalism (forbidding all race-conscious remediation) or remedial narrowness (permitting race-consciousness only for specific documented harms). Subordinated castes would lose the constitutional basis for broad anti-subordination measures; dominant groups would gain equal protection standing to challenge any race-conscious policy. The entire architecture of civil rights enforcement since the 1960s would restructure.
% FOUNDING_PROBLEM: The Reconstruction Amendments (13th, 14th, 15th) were ratified to dismantle the slave caste system and its badges and incidents. The founding problem was how to give constitutional force to the project of caste abolition — not merely to forbid racial classifications, but to empower the state to dismantle the hierarchy that classifications had created and maintained.
% FOUNDING_PROBLEM_CORROBORATION: The antisubordination reading is corroborated by historians of Reconstruction (Eric Foner, Kate Masur) who document the Amendments' anti-caste purpose; by the NAACP Legal Defense Fund's litigation strategy from the 1930s-1960s which explicitly targeted caste subordination; and by the Warren Court's desegregation jurisprudence (Brown v. Board, etc.) which targeted the stigma of caste. The colorblind reading contests this genealogy, citing the Amendment text's race-neutral language. The remedial reading accepts the anti-caste founding but contests its open-ended scope.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.42) reflects the asymmetric doctrinal cost imposed on dominant groups: they lose the ability to use 'equal protection' as a shield against race-conscious remediation and cannot maintain hierarchy through state action. This is not monetary extraction but a reallocation of constitutional standing. Suppression (0.58) is moderate-high because the constraint requires active judicial enforcement against hierarchy-entrenching laws (which persist through facially neutral mechanisms) and faces sustained political resistance. Theater ratio (0.22) is low — the constraint's enforcement (Brown, voting rights, affirmative action jurisprudence) has substantial functional content, though performative 'colorblind' rhetoric has increased in recent decades. Accessibility collapse (0.48) is moderate — alternative readings (colorblind, remedial) remain live and institutionally powerful, so the constraint does not fully close off competing frameworks. Resistance (0.71) is high — the colorblind reading has captured significant judicial territory, and political resistance to race-conscious remediation is intense.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (subordinated_castes) experiences this constraint as a rope — genuine coordination against caste subordination, with the state as a partner in dismantling hierarchy. The payer seat (dominant_groups) experiences it as a snare — extraction of their constitutional standing and state-power advantages, enforced by courts they do not control. The agenda_setter seats (state_government, federal_courts) experience it as a tangled_rope — they must actively enforce a principle that generates asymmetric costs and faces fierce resistance, while also depending on the constraint for their own anti-caste legitimacy. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the analytical view that the constraint has both a genuine coordination function (anti-subordination principle) and asymmetric extraction (dominant groups bear doctrinal costs).
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinated castes are beneficiaries (d ≈ 0.15): the constraint subsidizes their constitutional position by granting them protection against subordination and access to remedial measures. Their exit is identity_locked — caste position is not individually exitable. Dominant groups are payers (d ≈ 0.85): they bear the doctrinal cost of losing equal protection standing to challenge remediation and losing state power to entrench hierarchy. Their exit is mobile — individuals can disavow the ideology, but the structural position persists. State government and federal courts are agenda_setters (d ≈ 0.5): they administer and interpret the constraint, bearing enforcement costs but also wielding its authority. Colorblind advocates are excluded (d not computed): their reading is foreclosed by this constraint's premises. Remedial advocates are observers (d ≈ 0.5): adjacent but distinct framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (caste abolition) is contested — not dead (caste subordination persists in wealth, incarceration, health, education gaps) but not universally acknowledged as live. The antisubordination reading prevents mislabeling coordination as extraction by insisting that the 'cost' to dominant groups is the loss of an illegitimate advantage (hierarchy maintenance), not a legitimate entitlement. Conversely, it prevents mislabeling extraction as coordination by acknowledging that the constraint's enforcement machinery (judicial review, strict scrutiny for hierarchy-entrenching laws) actively suppresses alternative political arrangements — this is not voluntary coordination. The mandatrophy risk is that if the founding problem is treated as 'solved' (colorblind reading's move), the constraint becomes a piton — performative anti-discrimination rhetoric masking persistent hierarchy. If the founding problem is treated as 'live but narrow' (remedial reading), the constraint becomes a scaffold — transitional remediation with an implicit sunset. The antisubordination reading resists both by keeping the anti-caste principle as a permanent, non-sunsetted coordination target.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the antisubordination reading a distinct constraint with its own ε, or a measurement perspective on the single equal_protection_kernel constraint?',
    'Apply the ε-invariance test: if measuring the constraint via the antisubordination reading yields a different ε than measuring via the colorblind reading, they are distinct constraints. The antisubordination reading permits race-conscious remediation (low extraction for subordinated castes); the colorblind reading forbids it (high extraction for subordinated castes denied remediation). Different ε → distinct constraints.',
    'If distinct constraints, each gets its own story, classification, and stakeholders. If a single constraint with measurement-dependent ε, the framework''s ε-invariance principle is violated. This omega records the committer-frame commitment: this story treats the reading as a distinct ε-invariant constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether kernel readings are distinct constraints or perspectives on one constraint.').

omega_variable(
    foreclosure_vs_coexistence_colorblind,
    'Does the antisubordination reading logically foreclose the colorblind reading within a single constitutional framework, or do they coexist as competing but compatible positions?',
    'Test: can a single court consistently hold both ''race-conscious remediation is permitted to dismantle subordination'' (antisubordination) and ''all racial classifications are forbidden'' (colorblind)? No — the first permits what the second forbids. A framework adopting the antisubordination premise is logically committed to rejecting the colorblind premise. This is foreclosure, not coexistence.',
    'If forecloses, the two readings cannot be simultaneously held by the same authority structure — judicial adoption of one displaces the other. This explains the zero-sum character of Supreme Court Equal Protection jurisprudence. If coexists_with, both could be live options for different cases, which does not match the doctrinal reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence_colorblind, conceptual, 'Structural relationship between antisubordination and colorblind readings.').

omega_variable(
    subordination_measurement,
    'What counts as ''caste-like subordination'' for purposes of this constraint''s victim/beneficiary identification? Is it limited to Black Americans and Indigenous peoples, or does it extend to other groups (Latino, Asian, religious minorities, LGBTQ+)?',
    'Doctrinal analysis of Supreme Court antisubordination jurisprudence (which has centered Black Americans) versus theoretical expansions (critical race theory, intersectionality). Empirical: which groups'' subordination claims have been recognized by courts applying antisubordination logic?',
    'If narrow (Black/Indigenous only), the beneficiary/victim sets are smaller and the constraint''s coordination function is more focused. If broad, the constraint coordinates across multiple subordinated groups but faces greater definitional contestation and potential coalition fragmentation. Affects ε (broader beneficiary set may lower per-capita extraction) and stakeholder composition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_measurement, empirical, 'Scope of the ''subordinated castes'' beneficiary/victim class.').

omega_variable(
    remedial_as_sibling_not_subset,
    'Is the remedial reading a proper subset of the antisubordination reading (all remedial measures are antisubordination measures, but not vice versa), or a structurally distinct sibling with its own ε?',
    'Compare the remedial reading''s ''narrowly tailored to remedy documented exclusion'' standard with the antisubordination reading''s ''dismantle subordination'' standard. The antisubordination standard is broader (reaches systemic/subtle subordination without specific documentary proof of intent). If broader, they are distinct constraints with different ε — the remedial reading extracts less from dominant groups (narrower permission) but also coordinates less (does not reach structural subordination without specific proof).',
    'If distinct siblings, both need separate stories linked by network.affects_constraints. If subset, the remedial reading is a degraded mode of the antisubordination reading (scaffold with implicit sunset). The current story treats them as distinct siblings with influences relation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_as_sibling_not_subset, conceptual, 'Whether remedial reading is a subset or distinct sibling of antisubordination reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 0, 156).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_antisub_tr_t0, equal_protection_kernel__antisubordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ep_antisub_tr_t20, equal_protection_kernel__antisubordination_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(ep_antisub_tr_t50, equal_protection_kernel__antisubordination_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(ep_antisub_tr_t80, equal_protection_kernel__antisubordination_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement(ep_antisub_tr_t100, equal_protection_kernel__antisubordination_reading, theater_ratio, 100, 0.2).
narrative_ontology:measurement(ep_antisub_tr_t130, equal_protection_kernel__antisubordination_reading, theater_ratio, 130, 0.21).
narrative_ontology:measurement(ep_antisub_tr_t156, equal_protection_kernel__antisubordination_reading, theater_ratio, 156, 0.22).

% Extraction over time
narrative_ontology:measurement(ep_antisub_be_t0, equal_protection_kernel__antisubordination_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ep_antisub_be_t20, equal_protection_kernel__antisubordination_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(ep_antisub_be_t50, equal_protection_kernel__antisubordination_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(ep_antisub_be_t80, equal_protection_kernel__antisubordination_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(ep_antisub_be_t100, equal_protection_kernel__antisubordination_reading, base_extractiveness, 100, 0.42).
narrative_ontology:measurement(ep_antisub_be_t130, equal_protection_kernel__antisubordination_reading, base_extractiveness, 130, 0.4).
narrative_ontology:measurement(ep_antisub_be_t156, equal_protection_kernel__antisubordination_reading, base_extractiveness, 156, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ep_antisub_su_t0, equal_protection_kernel__antisubordination_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(ep_antisub_su_t20, equal_protection_kernel__antisubordination_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(ep_antisub_su_t50, equal_protection_kernel__antisubordination_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(ep_antisub_su_t80, equal_protection_kernel__antisubordination_reading, suppression_requirement, 80, 0.48).
narrative_ontology:measurement(ep_antisub_su_t100, equal_protection_kernel__antisubordination_reading, suppression_requirement, 100, 0.52).
narrative_ontology:measurement(ep_antisub_su_t130, equal_protection_kernel__antisubordination_reading, suppression_requirement, 130, 0.58).
narrative_ontology:measurement(ep_antisub_su_t156, equal_protection_kernel__antisubordination_reading, suppression_requirement, 156, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_kernel__antisubordination_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, voting_rights_act_preclearance).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, affirmative_action_strict_scrutiny).

% DUAL FORMULATION NOTE:
% This constraint is the antisubordination_reading of the equal_protection_kernel. It forecloses the colorblind_reading (mutually exclusive core premises) and influences the remedial_reading (broader permission for race-conscious measures creates downstream pressure on the narrower remedial standard). The kernel's formal text ('No State shall... deny equal protection') is the shared kernel; the three readings instantiate three distinct constraints with different ε, beneficiaries, victims, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_kernel__antisubordination_reading, powerful, 0.85).
constraint_indexing:directionality_override(equal_protection_kernel__antisubordination_reading, organized, 0.15).
constraint_indexing:directionality_override(equal_protection_kernel__antisubordination_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
