% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__hybrid_reading, []).

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
 *   constraint_id: classical_latin_standard__hybrid_reading
 *   human_readable: Classical Latin Standard — Hybrid Reading (Fidelity + Legitimate Post-Classical Development)
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   The hybrid reading of the Classical Latin standard asserts that correct
 *   Latin must simultaneously honor Classical textual norms (primarily
 *   Cicero, Caesar, Augustan literature) and recognize legitimate
 *   post-Classical developments in technical, scientific, and ecclesiastical
 *   domains (medieval Latin vocabulary for theology, law, medicine, botany).
 *   This reading emerged in the Renaissance humanist movement (e.g., Erasmus,
 *   Valla) and was institutionalized in the Tridentine reform, papal curia
 *   style, and early scientific academies. It coordinates a shared Latinity
 *   across domains while suppressing 'barbarisms' — forms deemed corruptions
 *   rather than developments. The constraint is actively enforced through
 *   editorial standards, academic gatekeeping, and institutional style
 *   guides.
 *
 * KEY AGENTS:
 *   - institutional_ecclesiastical_users: Primary beneficiary (institutional/identity_locked) — uses Latin as liturgical/legal/administrative language; benefits from stable normative standard that accommodates domain vocabulary
 *   - technical_scientific_users: Primary beneficiary (organized/constrained) — uses Latin for taxonomy, nomenclature, terminology; benefits from Classical prestige plus domain-specific lexicon
 *   - classical_philologists: Agenda setter (analytical/arbitrage) — defines and polices the Classical norm; derives authority from textual expertise
 *   - neo_latin_innovators: Primary victim (moderate/constrained) — produces new Latin texts; constrained by Classical norm and hybrid boundary; exit to vernacular or reconstruction is costly
 *   - vernacular_latin_users: Victim (powerless/trapped) — uses living Latin varieties; excluded by the hybrid standard's Classical anchor; no institutional recognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, 0.45).
domain_priors:suppression_score(classical_latin_standard__hybrid_reading, 0.4).
domain_priors:theater_ratio(classical_latin_standard__hybrid_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__hybrid_reading, "Classical Latin Standard — Hybrid Reading (Fidelity + Legitimate Post-Classical Development)").
narrative_ontology:topic_domain(classical_latin_standard__hybrid_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__hybrid_reading, '5ea054cc-2aee-4832-9d2a-ce6fdfdb7950').
narrative_ontology:cs_kernel_codification('5ea054cc-2aee-4832-9d2a-ce6fdfdb7950', fixed_text).
narrative_ontology:cs_authority_grounding('5ea054cc-2aee-4832-9d2a-ce6fdfdb7950', lineage).
narrative_ontology:cs_interpretation_layer_present('5ea054cc-2aee-4832-9d2a-ce6fdfdb7950').
narrative_ontology:cs_reading_relation('5ea054cc-2aee-4832-9d2a-ce6fdfdb7950', classical_latin_standard__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5ea054cc-2aee-4832-9d2a-ce6fdfdb7950', classical_latin_standard__reconstruction_reading, coexists_with).
narrative_ontology:cs_axiom('5ea054cc-2aee-4832-9d2a-ce6fdfdb7950', foundational, classical_textual_fidelity_required).
narrative_ontology:cs_axiom_status(classical_textual_fidelity_required, holdable).
narrative_ontology:cs_axiom_grounding('5ea054cc-2aee-4832-9d2a-ce6fdfdb7950', classical_textual_fidelity_required, conventional).
narrative_ontology:cs_axiom('5ea054cc-2aee-4832-9d2a-ce6fdfdb7950', foundational, accredited_postclassical_development_legitimate).
narrative_ontology:cs_axiom_status(accredited_postclassical_development_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('5ea054cc-2aee-4832-9d2a-ce6fdfdb7950', accredited_postclassical_development_legitimate, conventional).
narrative_ontology:cs_reference_frame('5ea054cc-2aee-4832-9d2a-ce6fdfdb7950', renaissance_humanist_restoration).
narrative_ontology:cs_drift_state('5ea054cc-2aee-4832-9d2a-ce6fdfdb7950', modern_institutional_codification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5ea054cc-2aee-4832-9d2a-ce6fdfdb7950', '2026-07-28T14:22:17Z').
narrative_ontology:cs_kernel_id(classical_latin_standard__hybrid_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, institutional_ecclesiastical_users).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, technical_scientific_users).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, classical_philologists).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, neo_latin_innovators).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, vernacular_latin_users).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, classical_normative_authority).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, domain_specific_legitimate_drift).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Catholic Church and related ecclesiastical bodies use Latin as their official liturgical, legal, and administrative language. They benefit from a stable normative standard (Classical) that accommodates their accumulated technical vocabulary (medieval theological, canonical, administrative terms). Exit is identity-locked: Latin is constitutive of their institutional self-concept; switching to vernacular would fracture their claim to continuity with antiquity.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, institutional_ecclesiastical_users, beneficiary,
    institutional, generational, identity_locked, global).

% Scientific academies, taxonomic bodies (ICZN, ICN), and medical nomenclature committees use Latin for international terminology. They benefit from Classical prestige and a shared morphological system, while requiring post-Classical vocabulary for modern concepts. Exit is constrained: vernacular terminologies exist but lack the historical depth and cross-linguistic neutrality that Latin provides.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, technical_scientific_users, beneficiary,
    organized, generational, constrained, global).

% Academic philologists, editors of critical editions, and professors of Latin define and police the Classical norm through textual criticism, commentaries, and pedagogical authority. They derive professional status from expertise in the Classical corpus. Exit is arbitrage-grade: they can move between the hybrid, continuity, and reconstruction readings as scholarly positions without losing their epistemic authority.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, classical_philologists, agenda_setter,
    analytical, biographical, arbitrage, global).

% Writers, translators, and scholars producing new Latin texts (neo-Latin literature, contemporary scientific descriptions, living Latin movement). They must conform to Classical morphology/syntax while their lexical innovations are judged against the opaque 'legitimate development' boundary. Exit is constrained: they can write in vernaculars but lose the international reach and prestige of Latin; they can adopt reconstruction_reading purity but lose the hybrid's accommodation.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, neo_latin_innovators, payer,
    moderate, biographical, constrained, global).

% Communities using living Latin varieties (e.g., ecclesiastical Latin pronunciation traditions, regional neo-Latin dialects, living Latin movement speakers). Their varieties are excluded by the hybrid standard's Classical anchor — no legitimate post-Classical development is recognized for spoken/register variation. Exit is trapped: they lack institutional power to challenge the standard and have no alternative Latin variety with recognition.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, vernacular_latin_users, payer,
    powerless, biographical, trapped, regional).

% Scholars and practitioners (e.g., some medievalists, living Latin communities) who hold that all transmitted Latin is legitimate development. They are excluded from the hybrid standard's authority structure because they reject the Classical-norm anchor. They can move between institutions or form parallel communities (mobile exit).
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, continuity_reading_adherents, excluded,
    organized, generational, mobile, global).

% Purist humanists, some classicists, and Neo-Latin academies that reject all post-Classical forms. They are excluded because they reject the hybrid's accommodation of medieval vocabulary. They can form parallel institutions (mobile exit) but lack the institutional scale of the hybrid reading.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, reconstruction_reading_adherents, excluded,
    organized, generational, mobile, global).

% Historical linguists, sociolinguists, and language policy analysts who study Latin's normative trajectory without participating in its enforcement. They observe the constraint's operation from outside the commitment system.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, linguistic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__hybrid_reading, classical_philologists).
narrative_ontology:fixing_cost_class(classical_latin_standard__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative Latin standard that simultaneously honors Classical textual prestige (enabling cross-epoch communication, shared cultural capital) and accommodates the technical vocabulary required for theology, law, science, and administration to function in Latin — solving the coordination problem of a language that must be both frozen and living.
% TRANSFER_FUNCTION: Moves normative authority from post-Classical innovators and vernacular users to Classical philologists and institutional gatekeepers, who define the boundary of 'legitimate development' and exclude forms deemed barbarous. The transfer is legitimacy and recognition, not money.
% ABSENT_VOICES: Vernacular Latin users (trapped, powerless) and continuity-reading communities (organized but excluded) would object to the Classical anchor and the restrictive boundary. They are absent from the hybrid standard's adjudicating bodies (academies, curial offices, editorial boards).
% DISAPPEARANCE_RATIONALE: If the hybrid standard vanished, ecclesiastical Latin would fracture into competing norms (Tridentine vs. post-Vatican II vs. reconstructionist); scientific nomenclature would lose its morphological stability; neo-Latin production would either collapse into vernacular or splinter into continuity/reconstruction camps. The world rearranges because the constraint coordinates multiple institutional domains.
% FOUNDING_PROBLEM: Renaissance humanism (15th–16th c.) confronted a Latin that had diverged from Classical norms through a millennium of medieval development. The problem: how to restore Classical purity (for cultural legitimacy) without abandoning the technical vocabulary that made Latin usable for theology, law, medicine, and science.
% FOUNDING_PROBLEM_CORROBORATION: Erasmus and Valla (humanist founders) attest the problem as live in their time. Modern philologists (e.g., Waquet 'Latin: The Empire of a Sign') attest the problem persists because the boundary of 'legitimate development' is never settled. The Catholic Church (beneficiary) attests the problem is solved by the hybrid standard. No consensus outside the benefiting parties.
narrative_ontology:disappearance_verdict(classical_latin_standard__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(classical_latin_standard__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__hybrid_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__hybrid_reading_tests).
:- end_tests(classical_latin_standard__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45): the hybrid reading extracts compliance from neo-Latin innovators and vernacular users by delegitimizing their forms, but accommodates enough post-Classical material to serve institutional users. Suppression is moderate (0.40): enforcement operates through editorial control, academic appointment, and institutional style guides — not state coercion. Theater ratio is low-moderate (0.25): the Classical fidelity claim is genuine but the boundary of 'legitimate development' is where performative gatekeeping concentrates. Accessibility collapse (0.35) and resistance (0.55) reflect that alternatives (continuity, reconstruction) remain live and contested. The constraint requires active enforcement (editorial boards, academies, curial offices) and has both beneficiaries (institutional users) and victims (innovators, vernacular users) — satisfying the tangled_rope structural requirements.
 *
 * PERSPECTIVAL GAP:
 *   From the philologist's seat (analytical/arbitrage), the hybrid reading is a principled coordination solving the problem of Latin's dual life as frozen classic and living technical language. From the neo-Latin innovator's seat (moderate/constrained), it is an exclusionary standard that freezes the language at a convenient point for institutional incumbents. From the vernacular user's seat (powerless/trapped), it is a foreign imposition with no coordination value. The engine computes per-seat classification from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional ecclesiastical and technical users are beneficiaries: they receive a stable, prestigious standard that incorporates their domain vocabulary (d ~ 0.2). Classical philologists are agenda_setters who administer the norm and derive professional authority from it (d ~ 0.15). Neo-Latin innovators are payers: they must conform to Classical norms while their innovations are selectively admitted (d ~ 0.7). Vernacular users are payers with no voice: the standard denies their variety any legitimacy (d ~ 0.85). Directionality is derived from these structural positions, not from self-identification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Renaissance: restore Classical purity while keeping Latin usable for theology, law, science) is contested — some argue it is solved by the hybrid standard, others that it persists because the boundary of 'legitimate development' is never settled. The constraint does not have a sunset clause. Mandatrophy is not resolved: the coordination function (shared Latinity across domains) persists but the extraction (delegitimization of innovation and vernacular) continues. The hybrid reading avoids the false summit of 'Classical Latin is a natural law' by explicitly acknowledging post-Classical legitimacy — but the boundary-drawing authority remains a site of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_hybrid_committer_frame,
    'Is the hybrid reading a stable institutionalized position or a transitional compromise between continuity and reconstruction?',
    'Trace the historical trajectory of hybrid norms in ecclesiastical documents, scientific Latin, and philological practice across the 15th–19th centuries; determine whether the accommodation of post-Classical forms was codified or remained contestable.',
    'If transitional, the constraint may be a scaffold with an implicit sunset (the eventual triumph of either continuity or reconstruction); if stable, it is a genuine tangled_rope with durable coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_hybrid_committer_frame, conceptual, 'Whether the hybrid reading represents a stable equilibrium or an unstable compromise between sibling readings.').

omega_variable(
    legitimate_vs_barbarous_boundary,
    'Where is the structural boundary between ''legitimate post-Classical development'' and ''barbarism'' drawn, and who has the authority to draw it?',
    'Analyze the decision procedures in papal curia style guides, scientific academy publications, and critical editions: which forms were admitted, which rejected, and what rationale was given.',
    'If the boundary is arbitrary or capture-prone, suppression is higher and the constraint leans toward snare; if principled and stable, the coordination function is genuine and tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimate_vs_barbarous_boundary, empirical, 'The epistemic and institutional basis for distinguishing legitimate drift from error in the hybrid reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (editorial gatekeeping, academic exclusion) or internalized (philologists self-censoring to conform to Classical norms)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in philological norm enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__hybrid_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(classical_latin_hybrid_tr_t0, classical_latin_standard__hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(classical_latin_hybrid_tr_t30, classical_latin_standard__hybrid_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(classical_latin_hybrid_tr_t60, classical_latin_standard__hybrid_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(classical_latin_hybrid_tr_t90, classical_latin_standard__hybrid_reading, theater_ratio, 90, 0.25).
narrative_ontology:measurement(classical_latin_hybrid_tr_t120, classical_latin_standard__hybrid_reading, theater_ratio, 120, 0.25).
narrative_ontology:measurement(classical_latin_hybrid_tr_t150, classical_latin_standard__hybrid_reading, theater_ratio, 150, 0.25).

% Extraction over time
narrative_ontology:measurement(classical_latin_hybrid_be_t0, classical_latin_standard__hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(classical_latin_hybrid_be_t30, classical_latin_standard__hybrid_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(classical_latin_hybrid_be_t60, classical_latin_standard__hybrid_reading, base_extractiveness, 60, 0.45).
narrative_ontology:measurement(classical_latin_hybrid_be_t90, classical_latin_standard__hybrid_reading, base_extractiveness, 90, 0.44).
narrative_ontology:measurement(classical_latin_hybrid_be_t120, classical_latin_standard__hybrid_reading, base_extractiveness, 120, 0.45).
narrative_ontology:measurement(classical_latin_hybrid_be_t150, classical_latin_standard__hybrid_reading, base_extractiveness, 150, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(classical_latin_hybrid_su_t0, classical_latin_standard__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(classical_latin_hybrid_su_t30, classical_latin_standard__hybrid_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement(classical_latin_hybrid_su_t60, classical_latin_standard__hybrid_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(classical_latin_hybrid_su_t90, classical_latin_standard__hybrid_reading, suppression_requirement, 90, 0.4).
narrative_ontology:measurement(classical_latin_hybrid_su_t120, classical_latin_standard__hybrid_reading, suppression_requirement, 120, 0.4).
narrative_ontology:measurement(classical_latin_hybrid_su_t150, classical_latin_standard__hybrid_reading, suppression_requirement, 150, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__hybrid_reading, 0.08).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__reconstruction_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, ecclesiastical_latin_style_guide).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, scientific_latin_nomenclature_codes).

% DUAL FORMULATION NOTE:
% This constraint is one member of the classical_latin_standard kernel family. The continuity_reading and reconstruction_reading are sibling constraints with different ε, beneficiary/victim structures, and claimed types. All three share the kernel commitment 'Correct Latin is X' but instantiate different constraints. The hybrid reading coordinates identity (Latin as a unified tradition across epochs) while extracting from innovations that fall outside its accredited boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(classical_latin_standard__hybrid_reading, institutional, 0.15).
constraint_indexing:directionality_override(classical_latin_standard__hybrid_reading, analytical, 0.15).
constraint_indexing:directionality_override(classical_latin_standard__hybrid_reading, moderate, 0.7).
constraint_indexing:directionality_override(classical_latin_standard__hybrid_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
