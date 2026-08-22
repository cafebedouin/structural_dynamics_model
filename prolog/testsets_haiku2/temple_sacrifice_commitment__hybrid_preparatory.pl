% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__hybrid_preparatory, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Temple Sacrifice Commitment in Preparatory Suspension
 *   domain: religious/legal/commitment_system
 *
 * SUMMARY:
 *   In Jewish law tradition, the obligation to offer Temple sacrifices is
 *   understood as eternally binding despite the material impossibility of
 *   performance since 70 CE. This constraint story instantiates one reading
 *   of how that commitment persists: the hybrid-preparatory reading frames
 *   study and scholarly maintenance as occupying a suspended middle
 *   state—neither full engagement with performable law nor mere archival
 *   preservation. The commitment is held in abeyance pending messianic
 *   restoration, and study is the mechanism that keeps it in force and ready.
 *   This reading differs from study-as-exercise (which treats study itself as
 *   full occupation of the obligation) and performance-only (which treats
 *   study as mere historical record-keeping of a defunct practice). The
 *   extractiveness is moderate because the constraint extracts intellectual
 *   resources, institutional prestige, and communal funding for an obligation
 *   that is perpetually deferred; uncertainty about restoration and
 *   contestation about whether study adequately preserves the commitment
 *   drive resistance and theater dynamics.
 *
 * KEY AGENTS:
 *   - Halakhic scholars: institutional agenda-setters who define how study maintains the commitment; benefit from scholarly authority and resource flows.
 *   - Communities funding study: moderate-power payers bearing material costs in exchange for the claim that study maintains an eternal obligation.
 *   - Individuals identity-locked to practice: moderate-power payers whose religious identity fuses with participation; exit would require redefining self-concept.
 *   - Messianic tradition keepers: organized beneficiaries who maintain the theological frame that validates the constraint; trapped by the doctrine itself.
 *   - Legal revisionist scholars: analytical observers assessing whether preparatory framing is honest or theater masquerading as restoration-readiness.
 *   - Prayer advocates: excluded voice advocating for authorized alternative (prayer as substitute for study) but unable to reframe institutional consensus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.58).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.42).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.58).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, scaffold).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Temple Sacrifice Commitment in Preparatory Suspension").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious/legal/commitment_system").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:has_sunset_clause(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, '6dd91160-e836-4a9a-b553-91f464fa877e').
narrative_ontology:cs_kernel_codification('6dd91160-e836-4a9a-b553-91f464fa877e', fixed_text).
narrative_ontology:cs_authority_grounding('6dd91160-e836-4a9a-b553-91f464fa877e', lineage).
narrative_ontology:cs_interpretation_layer_present('6dd91160-e836-4a9a-b553-91f464fa877e').
narrative_ontology:cs_reading_relation('6dd91160-e836-4a9a-b553-91f464fa877e', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('6dd91160-e836-4a9a-b553-91f464fa877e', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('6dd91160-e836-4a9a-b553-91f464fa877e', temple_sacrifice_commitment__symbolic_transformation, influences).
narrative_ontology:cs_axiom('6dd91160-e836-4a9a-b553-91f464fa877e', foundational, perpetual_obligation_binding_despite_material_impossibility).
narrative_ontology:cs_axiom_status(perpetual_obligation_binding_despite_material_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('6dd91160-e836-4a9a-b553-91f464fa877e', perpetual_obligation_binding_despite_material_impossibility, deontological).
narrative_ontology:cs_axiom('6dd91160-e836-4a9a-b553-91f464fa877e', foundational, study_maintains_suspended_commitment_preparatory_orientation).
narrative_ontology:cs_axiom_status(study_maintains_suspended_commitment_preparatory_orientation, holdable).
narrative_ontology:cs_axiom_grounding('6dd91160-e836-4a9a-b553-91f464fa877e', study_maintains_suspended_commitment_preparatory_orientation, conventional).
narrative_ontology:cs_reference_frame('6dd91160-e836-4a9a-b553-91f464fa877e', temple_sacrifice_binding_perpetually).
narrative_ontology:cs_drift_state('6dd91160-e836-4a9a-b553-91f464fa877e', post_second_temple_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('6dd91160-e836-4a9a-b553-91f464fa877e', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, halakhic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, religious_tradition_bearers).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, communities_funding_study).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, individuals_identity_locked_to_practice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, individuals_identity_locked_to_practice).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, messianic_tradition_keepers).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, messianic_restoration_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, perpetual_obligation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the corpus of sacrifice law, produce interpretive literature, establish curricula requiring continuous engagement with the law's details. Benefit from institutional status, scholarly authority, and the intellectual resources devoted to their expertise. Set the terms of how the commitment is maintained—study requirements, interpretive schools, centrality in religious education. Their exit would require abandoning scholarly identity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, halakhic_scholars, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, halakhic_scholars, beneficiary).

% Fund the study apparatus—yeshivas, scholars, publication of interpretive materials—on the premise that this maintains the commitment until restoration is possible. Constrained by the belief that abandoning study would constitute breach of perpetual obligation; by the social cost of withdrawing from participation; and by integration into communities where the study framework is normative. The benefit of study (maintaining divine obligation) is deferred and uncertain; the cost is immediate and material.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, communities_funding_study, payer,
    moderate, generational, constrained, regional).

% Organize intellectual and spiritual life around study of sacrifice law, experiencing this as both burden (non-performable law, uncertain future benefit) and identity anchor (occupying role of preservationist, maintaining connection to ancient tradition). Exit would mean redefining their religious identity and relationship to tradition. Some experience the constraint as genuine preparatory obligation; others as institutional inertia that has calcified into theater.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, individuals_identity_locked_to_practice, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, individuals_identity_locked_to_practice, beneficiary).

% Have argued that prayer and liturgical commemoration are authorized substitutes for sacrifice law study, not inferior alternatives. Their reading is structurally excluded from the halakhic consensus that frames study as maintaining the commitment. They would redirect resources toward prayer-focused practice but cannot reframe the institutional requirements without displacing the scholarship establishment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, alternative_prayer_advocates, excluded,
    powerful, generational, constrained, regional).

% Maintain the theological claim that study preserves the commitment for future restoration. Benefit from the apparatus that keeps the commitment credible and salient (without this study and discourse, the commitment would fade). Cannot exit because the doctrine itself is what they are committed to preserving—the belief in eventual restoration grounds their entire framing. Experience the constraint as essential; those who fund it may experience it as extractive.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, messianic_tradition_keepers, beneficiary,
    organized, civilizational, trapped, universal).

% Analyze whether the study commitment is genuinely preparatory (occupying the commitment in suspended state) or whether it has become symbolic theater (vindicating doctrine without material engagement with law's content). Assess whether the apparatus extracts resources for cultural identity maintenance rather than genuine restoration preparation. Their intervention could reframe the commitment or legitimize redirecting resources.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, legal_revisionist_scholars, observer,
    powerful, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__hybrid_preparatory, halakhic_scholars).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__hybrid_preparatory, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuity of Jewish law tradition across a period when its central practice (Temple sacrifice) is materially impossible. Coordinates scholarly and communal resources to preserve legal knowledge, interpretive frameworks, and the conceptual claim that the commitment remains binding despite non-performance.
% TRANSFER_FUNCTION: Transfers intellectual effort, institutional prestige, and financial resources from communities and individuals toward the maintenance of halakhic study and scholarship, justified by the obligation to preserve readiness for messianic restoration. The transfer persists because those funding it believe the commitment is eternally binding; whether the study is preparatory or archival determines whether the transfer is investment or extraction.
% ABSENT_VOICES: Prayer-focused religious movements that view sacrifice study as theologically obsolete; secular Jews who have exited the tradition and do not accept perpetual obligation; Jewish scholars who argue the commitment has been authorized to transform into prayer-based practice. These voices would redirect resources toward prayer, theological innovation, or abandoning the perpetual-obligation doctrine altogether, but are structurally excluded from setting institutional terms.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared—if the study commitment were formally suspended—the immediate rearrangement would be institutional: yeshivas would redirect curricula, scholarly authority structures would shift, resources would flow to alternative religious expressions. The theological rearrangement is contested: some argue the tradition itself would be preserved through prayer; others argue the entire conceptual framework would collapse. The verdict's status depends on which reading of the commitment is true.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE eliminated the material conditions for sacrifice practice. The binding obligation to perform sacrifice could not be abandoned without breach; study emerged as the mechanism to maintain the commitment in suspended state until restoration becomes possible.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (material conditions destroyed, obligation remains binding) is universally acknowledged in the tradition. The contestation is not about the problem but about whether study adequately preserves the commitment or whether the problem has been superseded by authorized transformation. Legal scholars outside the halakhic tradition (academic historians, comparative law scholars) affirm the historical problem and note that institutional solutions vary: some traditions have adopted prayer-substitution officially, others maintain study-as-preparatory, others have reframed the obligation as no longer binding. No external voice affirms that study alone maintains the original commitment indefinitely; external observers tend to classify it either as preparatory exercise (with uncertain restoration) or as symbolic theater (with no real expectation of restoration).
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__hybrid_preparatory, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as Scaffold because it has a declared sunset clause (restoration would end the suspension and enable full performance) and a coordination function (maintaining continuity of law tradition). The metrics describe moderate extractiveness because the constraint extracts from those funding and performing study for a benefit (maintaining the commitment) that is real but deferred and uncertain. Theater rises from 0.35 to 0.48 over the interval because institutional maintenance increasingly focuses on symbolic validation of perpetual obligation rather than substantive preparation for restoration—the content of study becomes more focused on proving the commitment persists than on detailed analysis of how restoration would be implemented. Suppression is lower than in snares (0.42) because the constraint does not require coercive suppression of alternatives—resistance comes from internal theological contestation and resource-scarcity pressure, not from external enforcement. Accessibility_collapse is moderate (0.65) because those embedded in the tradition face real structural barriers to exit (institutional integration, identity fusion, communal cost) but not absolute foreclosure—alternative readings and prayer-focused practices remain theoretically accessible. Resistance is moderate-high (0.55) because alternative readings (study-as-exercise, performance-only, symbolic-transformation, prayer-substitution) actively contest the hybrid-preparatory framing and its claim on resources.
 *
 * PERSPECTIVAL GAP:
 *   The gap between scholar and community seats is the core asymmetry: scholars author the framing of preparatory suspension and benefit from the intellectual resources it generates; communities bear the cost on the premise that this framing is correct. When the theater ratio rises (institutional maintenance decoupling from restoration prep), the community seat experiences extraction while the scholar seat experiences legitimate institutional development. This is not a disagreement about facts but about what the facts mean: does rising theater indicate that scholars are rationalizing extraction, or that the constraint's epistemic status has legitimately shifted as external conditions changed?
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars are beneficiaries with institutional power and arbitrage-grade exit (they can redirect expertise to other domains or other readings); their directionality is low (~0.2), making the constraint appear as coordination from their seat. Communities funding study and individuals identity-locked have moderate power but constrained to identity-locked exit; they sit at higher directionality (~0.65-0.75) because the cost to them is immediate and material while the benefit (maintaining obligation) is deferred and contested. Messianic keepers are organized beneficiaries trapped by the doctrine itself; their directionality is low but their exit is zero (trapped, not identity-locked, because they cannot exit the belief system without existential contradiction). Prayer advocates are excluded; they would sit at the target end of directionality if brought into the structure, because the constraint actively suppresses their alternative reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (material conditions destroyed, obligation remains binding) is dead—Temple restoration is not historically expected in mainstream Jewish tradition, though messianic doctrine keeps it theologically open. The constraint is not in mandatrophy by the strict definition (mandate outlived its function), but it is post-founding in a way that requires active reinterpretation to justify. The hybrid-preparatory reading resolves this by accepting that the commitment is suspended rather than nullified: it is maintained in readiness, not in full occupation, and study is the mechanism of maintenance. This framing prevents mandatrophy by keeping the obligation alive as deferred rather than dead. However, the theater-ratio rise and the strength of alternative readings (study-as-exercise, symbolic-transformation) suggest the constraint is increasingly vulnerable to mandatrophy diagnosis: if external observers assess that preparation is not genuinely happening, the constraint collapses to pure institutional inertia (piton). The R5 mismatch here is: founding problem is dead, but disappearance verdict is contested—the constraint persists not because communities would be harmed by its disappearance (coordination framing) but because those institutionally invested in it resist replacement by prayer or alternative readings (extraction framing). The honest reading is that the constraint has transitioned from genuine suspension-with-readiness toward quasi-maintenance-theater, and the extractiveness rise in the measurements captures that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preparatory_authenticity,
    'Is the study commitment genuinely preparatory (oriented toward restoration possibility) or has it become symbolic theater (maintaining institutional identity without honest restoration orientation)?',
    'Analysis of curricular focus over time: does study emphasize logistical preparation for restoration (temple architecture, sacrifice mechanics, halakhic conditions for re-implementation) or focus increasingly on abstract perpetuation of obligation doctrine? Exit interviews with scholars who leave the apparatus. Comparative analysis: do other traditions'' suspension-commitments show similar theater-ratio trajectories?',
    'If theater rises above 0.65 and stays there while base_extractiveness remains 0.55+, the constraint reclassifies toward piton (institutional inertia theater) from scaffold (genuine temporary suspension). If study content genuinely engages restoration logistics, the hybrid-preparatory framing holds and the constraint remains scaffold despite theater rise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preparatory_authenticity, empirical, 'Whether study apparatus maintains genuine restoration readiness or has become institutional theater.').

omega_variable(
    reading_foreclosure_possibility,
    'Does the hybrid-preparatory reading foreclose the study-as-exercise reading, or can both coexist within a single tradition framework?',
    'Textual and institutional analysis: does the halakhic tradition explicitly reject study-as-exercise on logical grounds (foreclosure) or merely prioritize the preparatory reading as more orthodox, leaving study-as-exercise as a minority live option? Can an individual or community adopt the study-as-exercise reading while remaining in institutional good standing?',
    'If readings genuinely foreclose each other, the constraint family involves real logical contradiction and alternative readings cannot coexist. If they coexist via interpretive pluralism, the family dynamics are political/resource-based rather than logically exclusive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_possibility, conceptual, 'Whether competing readings of how study occupies the commitment are logically exclusive or institutionally coexistent.').

omega_variable(
    identity_lock_internalization,
    'Is suppression of the constraint (the difficulty of exit from study-focused religious identity) structural (institutional barriers, communal cost, financial integration) or internalized (the individual has made the commitment-framework their core identity)?',
    'Cohort analysis of religious exits and identity transitions: do individuals who leave the study apparatus experience suppression as relieved (structural) or as psychologically persistent (internalized identity-fusion)? Do they eventually return at higher rates than the ambient exit rate would predict (internalization indicator)?',
    'If suppression is primarily structural, removing institutional barriers (creating alternative prayer-focused institutions, opening resources for non-study religious engagement) would reduce effective suppression and allow easier exit. If internalized, identity decoupling from the commitment would be required, which is a higher barrier than institutional change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Whether the difficulty of exiting from the commitment-study framework is structural/institutional or internalized through identity fusion.').

omega_variable(
    messianic_doctrine_status,
    'Within the theological tradition, what is the epistemic status of messianic restoration: is it an expectation (something that will happen), a possibility (might happen), or a metaphysical commitment (the tradition''s coherence depends on its logical possibility even if practical expectation is near-zero)?',
    'Comparative theology: how different Jewish movements frame the messianic doctrine and its relationship to the perpetual obligation. Surveys of scholars and community members about their actual restoration expectations vs. their institutional framing.',
    'If restoration is expected in any meaningful timeframe, the study-as-preparatory framing is honest and the constraint remains scaffold. If restoration is only metaphysically required (possibility must exist logically even if practically impossible), the framing is increasingly theater and the constraint approaches piton. If doctrine itself is contested (some hold obligation-perpetuation, others hold transformation-authorized), the founding problem is contested, not dead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_doctrine_status, conceptual, 'What the messianic-restoration doctrine actually commits the tradition to: expectation, possibility, or logical-necessity-without-practical-horizon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temple_sac_hybrid_tr_t0, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(temple_sac_hybrid_tr_t0, observed).
narrative_ontology:measurement(temple_sac_hybrid_tr_t5, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 5, 0.39).
narrative_ontology:measurement_basis(temple_sac_hybrid_tr_t5, observed).
narrative_ontology:measurement(temple_sac_hybrid_tr_t10, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 10, 0.43).
narrative_ontology:measurement_basis(temple_sac_hybrid_tr_t10, observed).
narrative_ontology:measurement(temple_sac_hybrid_tr_t15, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 15, 0.46).
narrative_ontology:measurement_basis(temple_sac_hybrid_tr_t15, observed).
narrative_ontology:measurement(temple_sac_hybrid_tr_t20, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 20, 0.49).
narrative_ontology:measurement_basis(temple_sac_hybrid_tr_t20, observed).
narrative_ontology:measurement(temple_sac_hybrid_tr_t25, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(temple_sac_hybrid_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(temple_sac_hybrid_be_t0, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(temple_sac_hybrid_be_t0, observed).
narrative_ontology:measurement(temple_sac_hybrid_be_t5, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(temple_sac_hybrid_be_t5, observed).
narrative_ontology:measurement(temple_sac_hybrid_be_t10, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(temple_sac_hybrid_be_t10, observed).
narrative_ontology:measurement(temple_sac_hybrid_be_t15, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(temple_sac_hybrid_be_t15, observed).
narrative_ontology:measurement(temple_sac_hybrid_be_t20, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 20, 0.59).
narrative_ontology:measurement_basis(temple_sac_hybrid_be_t20, observed).
narrative_ontology:measurement(temple_sac_hybrid_be_t25, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(temple_sac_hybrid_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(temple_sac_hybrid_su_t0, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(temple_sac_hybrid_su_t0, observed).
narrative_ontology:measurement(temple_sac_hybrid_su_t5, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 5, 0.39).
narrative_ontology:measurement_basis(temple_sac_hybrid_su_t5, observed).
narrative_ontology:measurement(temple_sac_hybrid_su_t10, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 10, 0.41).
narrative_ontology:measurement_basis(temple_sac_hybrid_su_t10, observed).
narrative_ontology:measurement(temple_sac_hybrid_su_t15, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(temple_sac_hybrid_su_t15, observed).
narrative_ontology:measurement(temple_sac_hybrid_su_t20, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 20, 0.43).
narrative_ontology:measurement_basis(temple_sac_hybrid_su_t20, observed).
narrative_ontology:measurement(temple_sac_hybrid_su_t25, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(temple_sac_hybrid_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, attachment_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__hybrid_preparatory, 0.12).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% Part of the temple_sacrifice_commitment family of three constraint stories, each instantiating a different reading of how the binding obligation to sacrifice persists after material performance became impossible. The hybrid-preparatory reading (this story) frames the commitment as held in suspension for restoration; study-as-exercise treats study as full occupation; performance-only treats study as archival record-keeping. These are not the same constraint viewed from different angles—they have different ε values, different victim sets, different institutional beneficiaries, and different foreclosure relationships. The family is linked because each reading contests the others' claim on communal resources and institutional legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_commitment__hybrid_preparatory, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
