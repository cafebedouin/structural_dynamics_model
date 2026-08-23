% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: D&C 132 Polygamy as Immutable Divine Law for Exaltation
 *   domain: religious_law/political_theology/commitment_system
 *
 * SUMMARY:
 *   D&C 132 (1843) establishes plural marriage as a 'new and everlasting
 *   covenant' required for the highest degree of exaltation. The immutable
 *   commandment reading holds this revelation is eternally binding,
 *   unchangeable by any mortal authority, and that compliance with federal
 *   anti-polygamy laws constitutes apostasy. Federal pressure (1862–1890)
 *   creates a martyrdom constraint: obey God and face prison/property loss,
 *   or obey man and forfeit exaltation. The 1890 Manifesto suspends practice
 *   but this reading rejects it as coerced and non-binding. The constraint
 *   claims Mountain status (divine law, emerges naturally from God's nature)
 *   but operates with high extraction (total obedience demand), high
 *   suppression (federal + internal enforcement), and identity-locked exit —
 *   a false summit mountain candidate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.75).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.85).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, mountain).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "D&C 132 Polygamy as Immutable Divine Law for Exaltation").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology/commitment_system").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).
domain_priors:emerges_naturally(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, '7c6cd663-97fa-413a-8379-c83b377d9d41').
narrative_ontology:cs_kernel_codification('7c6cd663-97fa-413a-8379-c83b377d9d41', fixed_text).
narrative_ontology:cs_authority_grounding('7c6cd663-97fa-413a-8379-c83b377d9d41', lineage).
narrative_ontology:cs_interpretation_layer_present('7c6cd663-97fa-413a-8379-c83b377d9d41').
narrative_ontology:cs_reading_relation('7c6cd663-97fa-413a-8379-c83b377d9d41', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('7c6cd663-97fa-413a-8379-c83b377d9d41', eternal_marriage_covenant__temporal_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('7c6cd663-97fa-413a-8379-c83b377d9d41', foundational, d_and_c_132_eternally_binding).
narrative_ontology:cs_axiom_status(d_and_c_132_eternally_binding, holdable).
narrative_ontology:cs_axiom_grounding('7c6cd663-97fa-413a-8379-c83b377d9d41', d_and_c_132_eternally_binding, deontological).
narrative_ontology:cs_axiom('7c6cd663-97fa-413a-8379-c83b377d9d41', foundational, polygamy_required_for_exaltation).
narrative_ontology:cs_axiom_status(polygamy_required_for_exaltation, holdable).
narrative_ontology:cs_axiom_grounding('7c6cd663-97fa-413a-8379-c83b377d9d41', polygamy_required_for_exaltation, deontological).
narrative_ontology:cs_reference_frame('7c6cd663-97fa-413a-8379-c83b377d9d41', nabuoo_restoration_peak).
narrative_ontology:cs_drift_state('7c6cd663-97fa-413a-8379-c83b377d9d41', post_manifesto_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('7c6cd663-97fa-413a-8379-c83b377d9d41', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, church_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, polygamous_household_heads).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, women_in_polygamous_marriages).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, rank_and_file_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, dissenting_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, rank_and_file_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, polygamous_household_heads).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, divine_command_theory_of_restoration).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, prophetic_infallibility_on_exaltation_requirements).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, eternal_kinship_sealing_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls interpretation and enforcement of D&C 132; declares the revelation eternally binding; manages the tension between immutable doctrine and federal prosecution; collects institutional legitimacy, tithing loyalty, and theological authority from maintaining the immutable frame.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, church_leadership, agenda_setter,
    institutional, generational, arbitrage, universal).

% Bear the daily costs of plural marriage — resource competition, reproductive burden, social isolation, legal vulnerability — while the doctrine declares their arrangement essential for exaltation; exit means apostasy and loss of eternal family bonds, making theological exit psychologically and socially impossible.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, women_in_polygamous_marriages, payer,
    powerless, biographical, identity_locked, local).

% Gain theological assurance of eternal family sealing and exaltation pathway from the doctrine; pay through tithing, missionary service, and cognitive commitment to a frame that federal law criminalizes; their identity is fused to the church's claims so exit threatens their entire meaning structure.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, rank_and_file_members, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__immutable_commandment_reading, rank_and_file_members, beneficiary).

% Receive status, sexual access, theological assurance, and kinship network expansion from plural marriage; bear federal prosecution risk, financial strain, and internal household conflict; their position is ambiguously empowered — they benefit from the doctrine but are also its primary legal targets.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, polygamous_household_heads, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__immutable_commandment_reading, polygamous_household_heads, payer).

% Apply legal pressure (anti-bigamy acts, disfranchisement, property seizure, imprisonment) to force compliance with monogamous marriage law; structurally excluded from the theological framework but their coercive power shapes the constraint's enforcement dynamics; would object to the immutable claim as sedition but have no voice in the doctrinal conversation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, federal_authorities, excluded,
    powerful, generational, mobile, national).

% Reject the polygamy doctrine or its immutable framing but remain in the community due to family, economic, or identity ties; face excommunication, shunning, and loss of eternal sealing assurances if they speak; their dissent is structurally silenced by the identity-locked exit condition.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, dissenting_members, excluded,
    powerless, biographical, trapped, local).

% Analyze the constraint from historical, theological, legal, and sociological angles; see the full structure of claim, enforcement, extraction, and drift; neither collect nor pay within the system.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, scholarly_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a community's assurance of eternal kinship bonds and exaltation pathway through a fixed theological claim that binds sealing ordinances, temple worship, and prophetic authority into a single immutable package — solving the problem of existential anxiety about salvation and family permanence in a restorationist framework.
% TRANSFER_FUNCTION: Moves obedience, tithing revenue, reproductive labor, missionary service, and cognitive commitment from members (especially women and rank-and-file) to church leadership; moves theological assurance and status to compliant participants; moves legal risk and prosecution costs to polygamous household heads and the institution.
% ABSENT_VOICES: Women in polygamous marriages (their lived experience of the arrangement is subsumed under the theological claim); dissenting members (excommunicated or silenced for questioning); federal authorities (structurally excluded from doctrinal discourse but their coercion shapes the constraint); children of polygamous households (no voice in the arrangement that defines their kinship).
% DISAPPEARANCE_RATIONALE: If the immutable commandment reading vanished overnight, the entire architecture of temple sealing, exaltation theology, prophetic authority claims, and kinship ontology would collapse — the church would lose its distinctive soteriology, the Manifesto's tension would resolve into open doctrinal revision, and the federal legal framework would lose its primary target. The community's identity, institutional legitimacy, and members' existential assurance would all reorganize.
% FOUNDING_PROBLEM: The problem of securing eternal kinship bonds and exaltation assurance in a restorationist framework that claimed to restore all priesthood keys and ordinances — how to guarantee that families are sealed for eternity and that the pathway to godhood is objectively defined and authoritatively administered.
% FOUNDING_PROBLEM_CORROBORATION: Early church documents (Nauvoo period revelations, journals, temple records) attest the founding problem as live for the founding generation. Contemporary church leadership attests it remains live. Critics (former members, historians, federal court records from Reynolds v. United States, late 19th century) attest the problem was structurally solved by the Manifesto's accommodation and is now a cover for institutional power. The corroboration split maps exactly to the kernel's reading division.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, ExtMetricName, E),
    domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(eternal_marriage_covenant__immutable_commandment_reading),
    narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the constraint demands total life reorientation — marriage, reproduction, property, loyalty — with eternal stakes. Suppression (0.85) peaks during federal prosecution (1880s) and remains high due to internal identity-locking. Theater ratio (0.4) reflects the gap between 'eternal immutable law' claim and the Manifesto's de facto suspension; the performance of immutability is maintained while practice shifts. Accessibility collapse (0.9) is near-total: once the frame is accepted, no alternative theology of exaltation exists. Resistance (0.6) captures federal legal resistance and internal dissent, both substantial but contained. The claimed_type 'mountain' with declared beneficiaries triggers FSM evaluation — the engine will test whether the natural-law claim covers extractive operation.
 *
 * PERSPECTIVAL GAP:
 *   From the church leadership seat, the constraint is genuine coordination (Mountain) — they built the theology, maintain the ordinances, and bear the prosecution risk. From the women_in_polygamous_marriages seat, it is a Snare — extraction without consent, exit blocked by identity fusion. From rank_and_file, it is a Tangled Rope — real coordination (eternal family assurance) fused with extraction (total obedience demand). The engine computes these per-seat types from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership (agenda_setter) sits at d≈0.1 — full beneficiary of the immutable frame, collecting institutional authority and loyalty. Women in polygamous marriages (payer, powerless, identity_locked) sit at d≈0.95 — bear the material costs with no theological exit. Rank-and-file members (payer/beneficiary, identity_locked) sit at d≈0.6 — gain assurance but pay through total commitment. Polygamous household heads (beneficiary/payer) sit at d≈0.4 — net benefit but with legal risk. Federal authorities (excluded, mobile) sit outside the directionality derivation but their coercion amplifies suppression. Dissenting members (excluded, trapped) sit at d≈0.9 — bear costs of silence or exile. The derivation chain from beneficiary/victim declarations + power + exit produces this gradient automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (eternal kinship assurance) is contested: the church says it remains live; critics say the Manifesto solved it by making exaltation accessible without polygamy. The constraint persists because the immutable frame prevents acknowledging the solution — declaring the problem dead would collapse the authority structure that depends on prophetic infallibility. This is mandatrophy: the mandate (polygamy as exaltation requirement) has outlived its coordination function (the Manifesto created an alternative pathway) but the constraint remains due to identity-locked members and institutional inertia. The theater ratio rise post-1890 tracks this atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the immutable commandment reading logically foreclose the prophetic override and temporal accommodation readings within a single commitment framework, or do they coexist as live positions held by different factions?',
    'Analyze the logical structure of each reading''s core premise: (1) ''D&C 132 is eternally immutable'' vs (2) ''Living prophet can supersede prior revelation'' vs (3) ''Practice suspended, doctrine intact.'' Test whether a single party could hold (1) and (2) or (1) and (3) without contradiction. Map the actual factional holdings in contemporary Mormonism.',
    'If forecloses, the kernel has a genuine logical fracture — the readings cannot be reconciled and the commitment system is structurally split. If coexists_with, the kernel hosts a managed ambiguity where the authority structure absorbs contradiction through interpretation layer. This determines whether cs_axiom_contradiction computes foreclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Logical relationship between the three kernel readings — foreclosure vs coexistence.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.85) primarily structural (federal prosecution, excommunication, property seizure) or internalized (identity fusion, theological terror of apostasy, belief that exit = damnation)?',
    'Post-exit suppression trajectory study: track suppression levels for members who leave the immutable frame (ex-Mormons, post-Manifesto accommodators, fundamentalist defectors). If suppression persists after structural exit, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This affects the identity_locked exit modulation and the χ computation for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in the martyrdom bind.').

omega_variable(
    beneficiary_structure_ambiguity,
    'Do polygamous household heads genuinely benefit from the immutable frame, or are they net payers who bear disproportionate legal risk while church leadership captures the institutional extraction?',
    'Comparative analysis of household head outcomes vs leadership outcomes across the interval: legal prosecutions, property losses, status gains, theological assurance. Test whether the beneficiary declaration for polygamous_household_heads reflects net benefit or net cost.',
    'If household heads are net payers, the beneficiary list shrinks to church_leadership only, sharpening the FSM signal (single concentrated beneficiary on a claimed mountain). If they are genuine beneficiaries, the extraction is more diffuse and the tangled_rope coordination function (kinship network coordination) gains weight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Whether polygamous household heads are net beneficiaries or net payers under the immutable frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emc_icr_tr_t0, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(emc_icr_tr_t10, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(emc_icr_tr_t30, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(emc_icr_tr_t47, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 47, 0.6).
narrative_ontology:measurement(emc_icr_tr_t61, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 61, 0.55).
narrative_ontology:measurement(emc_icr_tr_t80, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 80, 0.45).
narrative_ontology:measurement(emc_icr_tr_t120, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 120, 0.35).
narrative_ontology:measurement(emc_icr_tr_t180, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 180, 0.4).

% Extraction over time
narrative_ontology:measurement(emc_icr_be_t0, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(emc_icr_be_t10, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(emc_icr_be_t30, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(emc_icr_be_t47, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 47, 0.8).
narrative_ontology:measurement(emc_icr_be_t61, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 61, 0.78).
narrative_ontology:measurement(emc_icr_be_t80, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 80, 0.75).
narrative_ontology:measurement(emc_icr_be_t120, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 120, 0.65).
narrative_ontology:measurement(emc_icr_be_t180, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 180, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(emc_icr_su_t0, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(emc_icr_su_t10, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(emc_icr_su_t30, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(emc_icr_su_t47, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 47, 0.95).
narrative_ontology:measurement(emc_icr_su_t61, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 61, 0.9).
narrative_ontology:measurement(emc_icr_su_t80, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(emc_icr_su_t120, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 120, 0.5).
narrative_ontology:measurement(emc_icr_su_t180, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 180, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__immutable_commandment_reading, 0.08).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% Eternal marriage covenant kernel decomposes into three readings with divergent ε: immutable_commandment (ε=0.75, claimed mountain, FSM candidate), prophetic_override (ε≈0.15, claimed rope/scaffold — continuing revelation absorbs tension), temporal_accommodation (ε≈0.45, claimed tangled_rope — doctrine/practice split). The immutable reading's high ε reflects the martyrdom bind; the override reading's low ε reflects institutional adaptation; the accommodation reading's intermediate ε reflects the unstable doctrine/practice gap. All three share the D&C 132 text as kernel but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__immutable_commandment_reading, institutional, 0.1).
constraint_indexing:directionality_override(eternal_marriage_covenant__immutable_commandment_reading, powerless, 0.95).
constraint_indexing:directionality_override(eternal_marriage_covenant__immutable_commandment_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
