% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__principled_intervention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__principled_intervention_reading, []).

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
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: Principled State Intervention in Religious Affairs for Social Reform
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint story captures the 'principled intervention' reading of
 *   Indian constitutional secularism — the doctrine that the state may
 *   intervene in religious affairs to advance social reform and protect
 *   weaker sections within communities (Articles 25(2)(b), 17, 44). Unlike
 *   strict neutrality (equal distance) or reformist (affirmative duty to
 *   eliminate oppression), this reading treats intervention as a calibrated,
 *   principled exception to religious autonomy, justified by constitutional
 *   morality. The constraint has drifted from narrow caste/gender reforms
 *   (1950s temple entry, Hindu Code Bills) to broader doctrinal contestation
 *   (Sabarimala, triple talaq, uniform civil code debates), with increasing
 *   extraction from traditional authorities and rising theater as
 *   majoritarian actors appropriate the reform language.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, 0.42).
domain_priors:suppression_score(constitutional_secularism__principled_intervention_reading, 0.38).
domain_priors:theater_ratio(constitutional_secularism__principled_intervention_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "Principled State Intervention in Religious Affairs for Social Reform").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, 'cad2a809-5f40-422a-8fb3-a40550b90e44').
narrative_ontology:cs_kernel_codification('cad2a809-5f40-422a-8fb3-a40550b90e44', formalized).
narrative_ontology:cs_authority_grounding('cad2a809-5f40-422a-8fb3-a40550b90e44', lineage).
narrative_ontology:cs_interpretation_layer_present('cad2a809-5f40-422a-8fb3-a40550b90e44').
narrative_ontology:cs_reading_relation('cad2a809-5f40-422a-8fb3-a40550b90e44', constitutional_secularism__strict_neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('cad2a809-5f40-422a-8fb3-a40550b90e44', constitutional_secularism__reformist_reading, influences).
narrative_ontology:cs_axiom('cad2a809-5f40-422a-8fb3-a40550b90e44', foundational, state_intervention_legitimized_by_reform_objective).
narrative_ontology:cs_axiom_status(state_intervention_legitimized_by_reform_objective, holdable).
narrative_ontology:cs_axiom_grounding('cad2a809-5f40-422a-8fb3-a40550b90e44', state_intervention_legitimized_by_reform_objective, conventional).
narrative_ontology:cs_axiom('cad2a809-5f40-422a-8fb3-a40550b90e44', foundational, constitutional_morality_trumps_religious_autonomy_in_conflict).
narrative_ontology:cs_axiom_status(constitutional_morality_trumps_religious_autonomy_in_conflict, holdable).
narrative_ontology:cs_axiom_grounding('cad2a809-5f40-422a-8fb3-a40550b90e44', constitutional_morality_trumps_religious_autonomy_in_conflict, conventional).
narrative_ontology:cs_axiom('cad2a809-5f40-422a-8fb3-a40550b90e44', secondary, weaker_sections_within_communities_are_identifiable_constitutional_subjects).
narrative_ontology:cs_axiom_status(weaker_sections_within_communities_are_identifiable_constitutional_subjects, holdable).
narrative_ontology:cs_axiom_grounding('cad2a809-5f40-422a-8fb3-a40550b90e44', weaker_sections_within_communities_are_identifiable_constitutional_subjects, conventional).
narrative_ontology:cs_reference_frame('cad2a809-5f40-422a-8fb3-a40550b90e44', constituent_assembly_compromise_1950).
narrative_ontology:cs_drift_state('cad2a809-5f40-422a-8fb3-a40550b90e44', contemporary_majoritarian_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cad2a809-5f40-422a-8fb3-a40550b90e44', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, marginalized_community_members).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, reformist_state_institutions).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, progressive_legal_actors).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, traditional_religious_authorities).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, orthodox_community_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, state_legislature_executive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces legislation intervening in religious practices (temple entry, personal law reform, anti-discrimination measures) citing constitutional mandate for social reform. Gains political capital with progressive constituencies and marginalized groups while expanding state regulatory reach into civil society domain traditionally governed by religious authorities.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, state_legislature_executive, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, state_legislature_executive, beneficiary).

% Dalits, women, LGBTQ+ persons within religious communities who gain legal protection against exclusionary practices (temple entry bans, gender-discriminatory personal laws, caste-based segregation). Their exit from community enforcement is constrained by social, economic, and identity ties; state intervention provides external leverage but does not eliminate community pressure.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, marginalized_community_members, beneficiary,
    powerless, biographical, constrained, local).

% Priestly hierarchies, denominational boards, personal law boards who lose exclusive control over religious doctrine interpretation and practice regulation. Bear compliance costs of state mandates and loss of institutional authority. Their identity is fused with religious office — exit means abandoning vocational and communal identity; resistance is framed as defending religious freedom.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, traditional_religious_authorities, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, traditional_religious_authorities, payer).

% Adherents who experience state intervention as violation of communal self-governance and religious conscience. Bear costs of disrupted traditions, social friction, and perceived illegitimate intrusion. Exit from community is identity-locked — leaving means severing kinship, marriage, and social support networks; dissent within community risks ostracization.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, orthodox_community_members, payer,
    moderate, biographical, identity_locked, local).

% Public interest litigators, human rights NGOs, law reform commissions who use the intervention principle as doctrinal tool to advance equality claims. Gain professional recognition, funding, and institutional influence. Exit is mobile — can shift to other rights frameworks; not identity-locked to this specific constraint.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, progressive_legal_actors, beneficiary,
    organized, biographical, mobile, national).

% Adjudicates the boundary between permissible reform and impermissible interference through essential-practices test and constitutional morality doctrine. Holds structural power to expand or contract the intervention principle. Neither collects rents nor bears extraction — sits as institutional referee whose interpretations reshape the constraint's operational scope.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, judiciary_constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, judiciary_constitutional_courts, observer).

% Political parties and movements that would weaponize the intervention principle to target minority religious practices under guise of reform (e.g., cow protection, anti-conversion laws, uniform civil code as majoritarian project). Currently excluded from formal doctrine but exert pressure through legislative majorities and judicial appointments. Would object to strict neutrality reading that constrains their agenda.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, majoritarian_political_actors, excluded,
    powerful, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional mechanism for the state to override religious autonomy when practices perpetuate caste, gender, or status-based subordination within communities — solving the collective-action problem where internal reform is blocked by power asymmetries.
% TRANSFER_FUNCTION: Transfers interpretive authority over religious practice from traditional religious authorities to state institutions (legislature, judiciary), moving regulatory power and legitimacy from community-based governance to constitutional governance.
% ABSENT_VOICES: Minority religious communities targeted by majoritarian reform agendas (Muslim personal law reform pushed by Hindu nationalist majorities, tribal customary practices reformed by state development agencies) — they are not represented in the principled intervention doctrine's self-understanding but bear its asymmetric application. Also excluded: dissident voices within marginalized groups who prefer internal community reform over state imposition.
% DISAPPEARANCE_RATIONALE: If the principled intervention doctrine vanished, religious communities would regain exclusive authority over internal practices — temple entry bans, gender-discriminatory personal laws, caste exclusion would become legally unchallengeable on constitutional grounds. Marginalized members would lose external legal leverage; state would lose primary doctrinal tool for social reform legislation. The constitutional balance between Articles 25-26 (religious freedom) and Articles 14-15-17 (equality, non-discrimination, untouchability abolition) would collapse into religious autonomy absolutism.
% FOUNDING_PROBLEM: Religious communities historically enforced caste hierarchy, gender subordination, and untouchability through doctrines claiming divine sanction — internal reform was structurally impossible because authorities derived legitimacy from the very practices requiring reform. The state needed constitutional authorization to break this deadlock.
% FOUNDING_PROBLEM_CORROBORATION: Constituent Assembly debates (Ambedkar, Munshi, Alladi Krishnaswami Ayyar) corroborate the founding problem: religious autonomy was explicitly subordinated to social reform in Articles 25(2)(b) and 17. However, traditionalist members (H.V. Kamath, Damodar Swarup Seth) and contemporary religious authorities contest that the problem was ever as described — arguing state intervention was always about majoritarian control, not protection of weaker sections. No neutral arbiter outside the beneficiary/payer structure exists to adjudicate.
narrative_ontology:disappearance_verdict(constitutional_secularism__principled_intervention_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__principled_intervention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__principled_intervention_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(constitutional_secularism__principled_intervention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__principled_intervention_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__principled_intervention_reading_tests).
:- end_tests(constitutional_secularism__principled_intervention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects substantial but bounded transfer of authority from religious to state institutions — higher than a pure coordination mechanism (rope) but lower than pure extraction (snare) because genuine protection of marginalized members occurs. Suppression (0.38) is moderate: religious authorities resist through litigation, political mobilization, and non-compliance, but state enforcement capacity is real. Theater (0.28) has risen as 'reform' rhetoric increasingly covers majoritarian agenda-setting (cow protection, anti-conversion). Accessibility collapse (0.32) is low — alternative arrangements (community mediation, internal reform) persist but are structurally disadvantaged. Resistance (0.55) is significant and sustained across seven decades.
 *
 * PERSPECTIVAL GAP:
 *   From the state/judiciary seat, this is a necessary coordination mechanism solving a genuine collective-action problem (internal reform blocked by power asymmetry). From traditional authority and orthodox member seats, it is experienced as asymmetric extraction — the same intervention principle that opened temples to Dalits now fuels uniform civil code demands targeting Muslim personal law. The engine computes this divergence: identical structural data yields different effective extraction per seat via directionality. The claimed_type (tangled_rope) reflects the author's structural judgment — genuine coordination function exists alongside asymmetric extraction requiring active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   State legislature/executive and judiciary sit at low d (beneficiary/administrator) — they gain authority and legitimacy. Marginalized community members are beneficiaries with constrained exit (d ~0.3) — they gain legal protection but remain embedded in community power structures. Traditional religious authorities and orthodox members are payers with identity-locked exit (d ~0.85) — they lose interpretive monopoly and face compliance costs, with no viable exit from the identity that makes them targets. Progressive legal actors are beneficiaries with mobile exit (d ~0.2) — they gain professional capital without structural dependence. Majoritarian political actors are excluded but structurally positioned to capture the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (caste/gender oppression sanctified by religion) remains partially live but has mutated. Original targets (Hindu caste/gender practices) have seen substantial reform; new targets (minority personal laws) raise majoritarian capture concerns. The constraint has not resolved its mandatrophy — it persists because the reform agenda has expanded, not because the original problem is solved. Theater rise signals displacement of original function by new extraction logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_capture_trajectory,
    'Is the principled intervention doctrine on a trajectory toward majoritarian capture, where ''reform'' becomes a vehicle for targeting minority practices while majority practices are insulated?',
    'Longitudinal analysis of intervention targets: ratio of interventions affecting minority vs. majority religious practices over time; correlation between ruling party ideology and intervention selection.',
    'If capture is confirmed, the constraint reclassifies from tangled_rope toward snare — coordination function becomes cover for asymmetric extraction targeting specific communities. The epsilon would rise substantially for minority community seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_capture_trajectory, empirical, 'Whether the intervention principle''s application has become asymmetrically targeted.').

omega_variable(
    essential_practices_test_coherence,
    'Does the ''essential practices'' test (judicial doctrine determining which religious practices are protected) provide a principled boundary, or does it function as an open-textured tool for judicial policy preference?',
    'Doctrinal analysis of essential-practices jurisprudence: consistency of criteria across cases, predictability of outcomes, correlation with judicial ideology.',
    'If the test is incoherent, the constraint''s suppression and extraction become judge-dependent — effective extraction varies by bench composition, making the constraint structurally unstable and harder to classify. The tangled_rope classification assumes a stable enough boundary for ''active enforcement'' to be meaningful.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(essential_practices_test_coherence, conceptual, 'Whether the doctrinal boundary mechanism is principled or manipulable.').

omega_variable(
    marginalized_voice_authenticity,
    'Do marginalized community members actually experience state intervention as protection, or is their ''beneficiary'' status a construction of progressive legal actors speaking for them?',
    'Empirical studies of affected community attitudes toward specific interventions (Sabarimala women devotees, Muslim women on triple talaq, Dalit temple entry seekers) — distinguishing between elite advocacy claims and grassroots reception.',
    'If beneficiaries experience intervention as further marginalization or cultural alienation, the coordination function is undermined — the constraint extracts from both traditional authorities AND the nominal beneficiaries. This would push classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_voice_authenticity, empirical, 'Whether the purported beneficiaries experience the intervention as beneficial.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the constitutional_secularism kernel admit the principled_intervention_reading as a stable equilibrium, or does its internal logic inevitably collapse toward either strict_neutrality or reformist extremes?',
    'Constitutional theory analysis: whether Articles 25-26 and 14-15-17 can sustain a stable middle position, or whether the tension between religious autonomy and equality guarantees forces resolution to one pole.',
    'If the middle position is structurally unstable, this reading is a transient waystation — its current tangled_rope classification masks an inevitable drift toward either rope (strict neutrality, low extraction) or snare (reformist capture, high extraction). This is a conceptual omega about the kernel''s internal logic, not this reading''s current metrics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the principled intervention reading is a stable doctrinal position or a transitional phase.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__principled_intervention_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1950, constitutional_secularism__principled_intervention_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(cons_tr_t1976, constitutional_secularism__principled_intervention_reading, theater_ratio, 1976, 0.15).
narrative_ontology:measurement(cons_tr_t1985, constitutional_secularism__principled_intervention_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(cons_tr_t1992, constitutional_secularism__principled_intervention_reading, theater_ratio, 1992, 0.22).
narrative_ontology:measurement(cons_tr_t2006, constitutional_secularism__principled_intervention_reading, theater_ratio, 2006, 0.25).
narrative_ontology:measurement(cons_tr_t2018, constitutional_secularism__principled_intervention_reading, theater_ratio, 2018, 0.28).
narrative_ontology:measurement(cons_tr_t2024, constitutional_secularism__principled_intervention_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t1950, constitutional_secularism__principled_intervention_reading, base_extractiveness, 1950, 0.18).
narrative_ontology:measurement(cons_be_t1976, constitutional_secularism__principled_intervention_reading, base_extractiveness, 1976, 0.25).
narrative_ontology:measurement(cons_be_t1985, constitutional_secularism__principled_intervention_reading, base_extractiveness, 1985, 0.31).
narrative_ontology:measurement(cons_be_t1992, constitutional_secularism__principled_intervention_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(cons_be_t2006, constitutional_secularism__principled_intervention_reading, base_extractiveness, 2006, 0.38).
narrative_ontology:measurement(cons_be_t2018, constitutional_secularism__principled_intervention_reading, base_extractiveness, 2018, 0.42).
narrative_ontology:measurement(cons_be_t2024, constitutional_secularism__principled_intervention_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1950, constitutional_secularism__principled_intervention_reading, suppression_requirement, 1950, 0.15).
narrative_ontology:measurement(cons_su_t1976, constitutional_secularism__principled_intervention_reading, suppression_requirement, 1976, 0.22).
narrative_ontology:measurement(cons_su_t1985, constitutional_secularism__principled_intervention_reading, suppression_requirement, 1985, 0.28).
narrative_ontology:measurement(cons_su_t1992, constitutional_secularism__principled_intervention_reading, suppression_requirement, 1992, 0.32).
narrative_ontology:measurement(cons_su_t2006, constitutional_secularism__principled_intervention_reading, suppression_requirement, 2006, 0.35).
narrative_ontology:measurement(cons_su_t2018, constitutional_secularism__principled_intervention_reading, suppression_requirement, 2018, 0.38).
narrative_ontology:measurement(cons_su_t2024, constitutional_secularism__principled_intervention_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__principled_intervention_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__principled_intervention_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__reformist_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, uniform_civil_code_mandate).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, anti_conversion_laws).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, cow_protection_legislation).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, personal_law_reform_triple_talaq).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, sabarimala_temple_entry).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_secularism kernel. The strict_neutrality_reading treats secularism as equal-distance non-interference (lower extraction, rope-like). The reformist_reading treats it as affirmative duty to eliminate oppression (higher extraction, snare-tending). This principled_intervention_reading sits between — genuine coordination function (protecting weaker sections) with asymmetric extraction (traditional authorities lose interpretive monopoly) requiring active enforcement (judicial/legislative machinery). The three readings share the same constitutional text but instantiate different constraints with different epsilon values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_secularism__principled_intervention_reading, institutional, 0.15).
constraint_indexing:directionality_override(constitutional_secularism__principled_intervention_reading, powerless, 0.35).
constraint_indexing:directionality_override(constitutional_secularism__principled_intervention_reading, organized, 0.8).
constraint_indexing:directionality_override(constitutional_secularism__principled_intervention_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
