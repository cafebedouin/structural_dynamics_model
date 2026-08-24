% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: classical_latin_standard__hybrid_reading
 *   human_readable: Hybrid Classical Latin Standard (Fidelity + Legitimate Post-Classical Development)
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   The hybrid reading of the Classical Latin standard asserts that
 *   correctness requires fidelity to Classical grammatical and lexical norms
 *   while explicitly legitimizing post-Classical developments in technical
 *   (scientific, medical, legal) and ecclesiastical domains. This reading
 *   emerged from the Carolingian reform (c. 800) which restored Classical
 *   Latin as a written standard, was challenged by the humanist
 *   reconstructionist movement (c. 1400–1600) demanding purer Classical
 *   forms, and stabilized in the early modern period as scientific Latin
 *   codified neologisms on Classical morphology while the Church retained its
 *   theological vocabulary. The constraint operates as a tangled rope: it
 *   coordinates a shared Latin across domains (genuine coordination function)
 *   while extracting legitimacy from excluded vernacular and medieval forms
 *   (asymmetric extraction), enforced through grammars, dictionaries, papal
 *   documents, and nomenclature codes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, 0.45).
domain_priors:suppression_score(classical_latin_standard__hybrid_reading, 0.45).
domain_priors:theater_ratio(classical_latin_standard__hybrid_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__hybrid_reading, "Hybrid Classical Latin Standard (Fidelity + Legitimate Post-Classical Development)").
narrative_ontology:topic_domain(classical_latin_standard__hybrid_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__hybrid_reading, '46aad469-d457-4487-ad06-44002d7af624').
narrative_ontology:cs_kernel_codification('46aad469-d457-4487-ad06-44002d7af624', fixed_text).
narrative_ontology:cs_authority_grounding('46aad469-d457-4487-ad06-44002d7af624', lineage).
narrative_ontology:cs_interpretation_layer_present('46aad469-d457-4487-ad06-44002d7af624').
narrative_ontology:cs_reading_relation('46aad469-d457-4487-ad06-44002d7af624', classical_latin_standard__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('46aad469-d457-4487-ad06-44002d7af624', classical_latin_standard__reconstruction_reading, coexists_with).
narrative_ontology:cs_axiom('46aad469-d457-4487-ad06-44002d7af624', foundational, classical_fidelity_required).
narrative_ontology:cs_axiom_status(classical_fidelity_required, holdable).
narrative_ontology:cs_axiom_grounding('46aad469-d457-4487-ad06-44002d7af624', classical_fidelity_required, deontological).
narrative_ontology:cs_axiom('46aad469-d457-4487-ad06-44002d7af624', foundational, legitimate_domain_specific_drift_recognized).
narrative_ontology:cs_axiom_status(legitimate_domain_specific_drift_recognized, holdable).
narrative_ontology:cs_axiom_grounding('46aad469-d457-4487-ad06-44002d7af624', legitimate_domain_specific_drift_recognized, empirically_contingent).
narrative_ontology:cs_reference_frame('46aad469-d457-4487-ad06-44002d7af624', classical_norm_with_accommodated_drift).
narrative_ontology:cs_drift_state('46aad469-d457-4487-ad06-44002d7af624', contemporary_philological_practice, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('46aad469-d457-4487-ad06-44002d7af624', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__hybrid_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, ecclesiastical_institutions).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, technical_scientific_communities).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, classical_philologists).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, vernacular_latin_practitioners).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, medieval_latin_users).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, non_standard_latin_writers).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, classical_latin_as_normative_anchor).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, domain_specific_latin_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and enforce liturgical, canonical, and documentary Latin standards. The hybrid standard lets them claim Classical authority while retaining theological terminology (e.g., 'trinitas', 'persona', 'sacramentum') that evolved post-Classically. They administer the norm through papal documents, canon law, and liturgical books. Exit would mean abandoning the linguistic identity of the institution.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, ecclesiastical_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Use Latin for international nomenclature (binomial species names, anatomical terms, astronomical designations). The hybrid standard gives them a stable, universally recognized vocabulary root while allowing neologisms formed on Classical morphology (e.g., 'helicobacter', 'quasar'). They cannot easily switch to vernaculars without losing cross-linguistic precision.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, technical_scientific_communities, beneficiary,
    organized, biographical, constrained, global).

% Define, edit, and teach the Classical corpus that anchors the standard. They gain professional authority as gatekeepers of 'correctness' while their own research sometimes documents the very post-Classical developments the hybrid reading legitimizes. Their career mobility lets them exit the Latin specialization, but their professional identity is tied to the Classical anchor.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, classical_philologists, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, classical_philologists, beneficiary).

% Speakers and writers of regional Neo-Latin varieties (e.g., Romanian ecclesiastical usage, regional scientific Latin) whose forms are labeled 'barbarisms' when they deviate from the hybrid norm. They bear the cost of delegitimization — their linguistic heritage is treated as error rather than development. Exit means adopting the institutional standard or abandoning Latin entirely.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, vernacular_latin_practitioners, payer,
    powerless, biographical, trapped, regional).

% Scholars, clerics, and administrators who work with medieval Latin texts and continue medieval usage traditions. The hybrid reading legitimizes only *some* medieval developments (technical/ecclesiastical), rejecting others as barbarisms. They must navigate which of their inherited forms are acceptable. Their exit is constrained by the texts they study and the traditions they maintain.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, medieval_latin_users, payer,
    moderate, biographical, constrained, global).

% Contemporary writers of Latin (hobbyists, educators, neo-Latin authors) who use forms outside the hybrid norm. Their work is corrected or excluded from institutional channels. They have no institutional power and limited exit — either conform or remain marginal.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, non_standard_latin_writers, payer,
    powerless, immediate, trapped, local).

% Advocates for a purified Classical standard (the reconstruction_reading). They argue the hybrid reading corrupts Classical fidelity by admitting any post-Classical forms. They are excluded from the hybrid standard's enforcement apparatus but actively contest it in academic discourse.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, reconstructionist_philologists, excluded,
    moderate, biographical, mobile, global).

% Advocates for the living-transmission standard (the continuity_reading). They argue the hybrid reading arbitrarily freezes some developments while rejecting others, ignoring that all are part of Latin's unbroken history. They are excluded from the hybrid standard's enforcement but maintain a live scholarly position.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, continuity_philologists, excluded,
    moderate, biographical, mobile, global).

% Historical linguists and sociolinguists who study Latin's evolution without normative commitment. They see the hybrid standard as one of several competing normative frameworks imposed on a language that naturally diversified. They neither collect nor pay in the constraint's economy.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, linguistic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared normative anchor for Latin across ecclesiastical, scientific, and scholarly domains: Classical texts supply the grammatical and lexical baseline, while designated post-Classical developments supply necessary technical and theological vocabulary, enabling cross-domain and cross-generational intelligibility.
% TRANSFER_FUNCTION: Transfers legitimacy from excluded vernacular, regional, and medieval Latin forms to the institutional standard-setters (Church, academia, scientific nomenclature bodies). The 'barbarism' label extracts authority from non-conforming usage and concentrates it in the hybrid norm's gatekeepers.
% ABSENT_VOICES: Vernacular Latin speakers (historical and contemporary), regional Neo-Latin writers, and medieval Latin practitioners whose forms are categorized as 'barbarisms' without their participation in the standardization process. Their absence is structural: the hybrid standard's authority derives precisely from excluding their usage from the legitimate register.
% DISAPPEARANCE_RATIONALE: If the hybrid standard vanished overnight, ecclesiastical documents would lose their universal normative reference, scientific nomenclature would fragment into competing national or ad hoc terminologies, and academic Latin would split between reconstructionist purity and continuity drift — the coordination infrastructure for Latin as a transnational learned language would collapse.
% FOUNDING_PROBLEM: The fragmentation of Latin after antiquity into mutually unintelligible vernaculars and specialized registers threatened its function as a universal learned language for theology, law, science, and scholarship across Europe.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary ecclesiastical practice (Vatican Latin documents), international scientific nomenclature codes (ICZN, ICNafp, IAU), and philological consensus outside the direct beneficiaries (e.g., Romance historical linguists, sociolinguists of learned languages) attest that a stabilized cross-domain Latin standard remains necessary for the domains that still use Latin.
narrative_ontology:disappearance_verdict(classical_latin_standard__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness is moderate (0.45) because the standard delegitimizes some forms (barbarisms) while accommodating others — the extraction is partial, not total. Suppression is moderate (0.45) because enforcement is real (nomenclature codes reject non-conforming terms, liturgical books enforce norms) but alternatives persist in marginalized registers. Theater ratio is moderate (0.30): humanist-era performative classicism inflated this, but modern scientific and ecclesiastical usage is functionally grounded. Accessibility collapse is moderate (0.50): non-standard forms remain usable but carry stigma and institutional exclusion. Resistance is moderate (0.40): reconstructionist and continuity scholars actively contest the boundary, but institutional inertia maintains the hybrid norm. The shared time grid (500, 800, 1200, 1500, 1800, 2024) captures the Carolingian standardization, scholastic peak, humanist purge, scientific codification, and contemporary stability.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seats (ecclesiastical institutions, philologists) experience the constraint as coordination they maintain; the payer seats (vernacular, medieval, non-standard users) experience it as extraction they cannot escape. The engine computes this divergence from the structural power/exit asymmetries. The beneficiary seat (technical communities) sits near symmetric — they gain coordination value but pay conformity costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical institutions and classical philologists are agenda_setters with identity_locked or organized power — they set and enforce the norm, deriving authority from it (d near beneficiary end). Technical scientific communities are beneficiaries with constrained exit — they gain a stable international vocabulary but cannot easily opt out (d slightly beneficiary). Vernacular practitioners, medieval users, and non-standard writers are payers with trapped or constrained exit — they bear the delegitimization cost (d near target end). Reconstructionist and continuity philologists are excluded — they contest the norm but hold no enforcement power. Linguistic observers are analytical — they see the full structure without material stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Latin's fragmentation threatening its universal learned-language role) remains live: the domains that still use Latin (ecclesiastical, scientific, scholarly) still need a shared standard. The hybrid reading has not atrophied into a piton because its coordination function is actively used daily in species naming, anatomical terminology, and canon law. However, the extraction of legitimacy from excluded forms persists even where the coordination need is weaker (e.g., neo-Latin literary writing), creating a mandatrophy risk at the margins.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_boundary_ambiguity,
    'What distinguishes a ''legitimate post-Classical development'' from a ''barbarism'' in the hybrid reading, and who decides?',
    'Comparative analysis of accepted vs. rejected forms across ecclesiastical documents, scientific nomenclature codes, and philological reference works; tracing the institutional history of specific boundary decisions.',
    'If the boundary is arbitrary or interest-driven, the hybrid reading''s coordination claim masks extraction; if it tracks functional criteria (precision, stability, cross-domain utility), the extraction is the price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_boundary_ambiguity, conceptual, 'Whether the legitimate/barbarism distinction is principled or interest-serving.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel ''Latin language correctness'' or ''Latin as universal learned language''? The hybrid reading''s structure changes with the framing.',
    'Examine whether the hybrid standard''s enforcement scope matches the domains that need a universal learned language (ecclesiastical, scientific, legal) or extends to domains where Latin is a heritage language (neo-Latin literature, regional usage).',
    'If the kernel is ''universal learned language'', the hybrid reading''s domain restriction is functional coordination; if ''Latin language correctness'', the domain restriction is arbitrary extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s scope is functional (learned language) or ontological (language itself).').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the exclusion of ''barbarisms'' enforced structurally (institutional gatekeeping) or internalized (speakers self-correct to the norm)?',
    'Post-exit trajectory study: if non-standard Latin writers continue producing ''barbarisms'' after leaving institutional channels, suppression is structural; if they self-correct even without enforcement, internalized component exists.',
    'If internalized, effective suppression exceeds the structural measure — the constraint travels with the agent after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for delegitimized forms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__hybrid_reading, 500, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t500, classical_latin_standard__hybrid_reading, theater_ratio, 500, 0.1).
narrative_ontology:measurement(clas_tr_t800, classical_latin_standard__hybrid_reading, theater_ratio, 800, 0.15).
narrative_ontology:measurement(clas_tr_t1200, classical_latin_standard__hybrid_reading, theater_ratio, 1200, 0.2).
narrative_ontology:measurement(clas_tr_t1500, classical_latin_standard__hybrid_reading, theater_ratio, 1500, 0.4).
narrative_ontology:measurement(clas_tr_t1800, classical_latin_standard__hybrid_reading, theater_ratio, 1800, 0.3).
narrative_ontology:measurement(clas_tr_t2024, classical_latin_standard__hybrid_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(clas_be_t500, classical_latin_standard__hybrid_reading, base_extractiveness, 500, 0.2).
narrative_ontology:measurement(clas_be_t800, classical_latin_standard__hybrid_reading, base_extractiveness, 800, 0.35).
narrative_ontology:measurement(clas_be_t1200, classical_latin_standard__hybrid_reading, base_extractiveness, 1200, 0.4).
narrative_ontology:measurement(clas_be_t1500, classical_latin_standard__hybrid_reading, base_extractiveness, 1500, 0.55).
narrative_ontology:measurement(clas_be_t1800, classical_latin_standard__hybrid_reading, base_extractiveness, 1800, 0.45).
narrative_ontology:measurement(clas_be_t2024, classical_latin_standard__hybrid_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t500, classical_latin_standard__hybrid_reading, suppression_requirement, 500, 0.15).
narrative_ontology:measurement(clas_su_t800, classical_latin_standard__hybrid_reading, suppression_requirement, 800, 0.3).
narrative_ontology:measurement(clas_su_t1200, classical_latin_standard__hybrid_reading, suppression_requirement, 1200, 0.35).
narrative_ontology:measurement(clas_su_t1500, classical_latin_standard__hybrid_reading, suppression_requirement, 1500, 0.5).
narrative_ontology:measurement(clas_su_t1800, classical_latin_standard__hybrid_reading, suppression_requirement, 1800, 0.4).
narrative_ontology:measurement(clas_su_t2024, classical_latin_standard__hybrid_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__hybrid_reading, information_standard).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__hybrid_reading, 0.02).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__reconstruction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid_reading in the classical_latin_standard kernel family. It differs from continuity_reading (which treats all natural drift as legitimate, extractiveness near 0) and reconstruction_reading (which treats all post-Classical drift as illegitimate, extractiveness higher due to total purging). The three readings share the kernel 'Correct Latin is X' but instantiate different constraints with different ε, beneficiary/victim structures, and suppression profiles. The hybrid reading's moderate extraction reflects its selective legitimization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
